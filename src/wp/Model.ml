(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Model Registry                                                     --- *)
(* -------------------------------------------------------------------------- *)

type parameter = {
  context : string ;
  param : string ;
  binder : (unit -> unit) -> unit -> unit ;
}

type t = {
  id : string ; (* Identifier Basename for Model (unique) *)
  name : string ; (* Title of the Model (for pretty) *)
  descr : string option ;
  emitter : Emitter.t ;
  mutable params : parameter list ;
}

type model = t
type tuning = (model -> unit)

type registry = 
    name:string -> ?id:string -> ?descr:string -> ?tuning:tuning list -> 
  unit -> model

module MODELS =
struct

  module H = Datatype.String.Map
  let h = ref H.empty
  (* NOT PROJECTIFIED : Models are defined at Plugin load-time, 
     for all projects *)

  let mem id = H.mem id !h
  let add m = h := H.add m.id m !h
  let find id = H.find id !h
  let iter f = H.iter (fun _ m -> f m) !h

end

let find ~id = MODELS.find id
let iter f = MODELS.iter f

let register ~name ?id ?descr ?(tuning=[]) () =
  let id = match id with None -> name | Some id -> id in
  if MODELS.mem id then
    Wp_parameters.fatal "Duplicate model '%s'" id ;
  let emitter = 
    let e_name = "Wp." ^ id in
    let correctness = [ ] in
    let tuning = [ Wp_parameters.Provers.parameter ] in
    Emitter.create e_name [ Emitter.Property_status ] ~correctness ~tuning
  in
  let model = {
    id = id ;
    name = name ;
    descr = descr ;
    emitter = emitter ;
    params = [] ;
  } in
  MODELS.add model ; 
  List.iter (fun p -> p model) tuning ;
  model

let get_id m = m.id
let get_name m = m.name
let get_descr m = m.descr    
(* let get_parameters m = List.map (fun p -> p.param) m.params *)

let set_parameter model context v descr =
  let cname = Context.name context in
  if List.exists (fun p -> p.context = cname) model.params then
    Wp_parameters.fatal "Duplicate parameter '%s' on model '%s'" cname model.id ;
  model.params <- model.params @ 
    [{
       context = cname ;
       param = descr ;
       binder = Context.bind context v ;
     }]

let model = Context.create "Wp.Model"

let rec bind_params f = function
  | [] -> f
  | p::ps -> p.binder (bind_params f ps)

let on_model m f = Context.bind model m (bind_params f m.params) ()

let with_model m f x = 
  let result = ref None in
  on_model m (fun () -> result := Some (f x)) ;
  match !result with
    | None -> assert false
    | Some y -> y

let get_model () = Context.get model
let get_emitter model = model.emitter

let directory () = Wp_parameters.get_output_dir (Context.get model).id

module type Entries =
sig
  type key
  type data
  val name : string
  val compare : key -> key -> int
  val pretty : Format.formatter -> key -> unit
end

module type Registry =
sig
  type key
  type data

  val mem : key -> bool
  val find : key -> data
  val get : key -> data option
  val define : key -> data -> unit
  val update : key -> data -> unit
  val memoize : (key -> data) -> key -> data
  val compile : (key -> data) -> key -> unit
  val callback : (key -> data -> unit) -> unit
  val iter : (key -> data -> unit) -> unit
  val iter_sorted : (key -> data -> unit) -> unit
end

module Index(E : Entries) =
struct

  type key = E.key
  type data = E.data

  module KEY = struct type t = E.key let compare = E.compare end
  module MAP = Map.Make(KEY)
  module SET = Set.Make(KEY)

  let demon = ref []

  type entries = {
    mutable index : E.data MAP.t ;
    mutable lock : SET.t ;
  }

  module ENTRIES : Datatype.S with type t = entries =
    Datatype.Make
      (struct
	 type t = entries
	 include Datatype.Serializable_undefined
	 let reprs = [{index=MAP.empty;lock=SET.empty}]
	 let name = "Wp.Model.Index." ^ E.name
       end)

  module REGISTRY = State_builder.Hashtbl
    (Datatype.String.Hashtbl)
    (ENTRIES)
    (struct
       let name = "Wp.Model." ^ E.name
       let dependencies = [Ast.self]
       let size = 32
     end)
    (* Projectified entry map, indexed by model *)

  let entries () : entries = 
    let mid = (Context.get model).id in
    try REGISTRY.find mid
    with Not_found ->
      let e = { index=MAP.empty ; lock=SET.empty } in
      REGISTRY.add mid e ; e

  let mem k = let e = entries () in MAP.mem k e.index || SET.mem k e.lock

  let find k = let e = entries () in MAP.find k e.index
  let get k = try Some (find k) with Not_found -> None

  let fire k d = 
    List.iter (fun f -> f k d) !demon

  let callback f = demon := !demon @ [f]

  let define k d =
    begin
      let e = entries () in
      if MAP.mem k e.index then
	Wp_parameters.fatal "Duplicate definition (%s:%a)" E.name E.pretty k ;
      if SET.mem k e.lock then
	Wp_parameters.fatal "Locked definition (%s:%a)" E.name E.pretty k ;
      e.index <- MAP.add k d e.index ;
      fire k d ;
    end

  let update k d =
    begin
      let e = entries () in
      e.index <- MAP.add k d e.index ; 
      fire k d ;
    end

  let memoize f k =
    let e = entries () in
    try MAP.find k e.index
    with Not_found ->
      let lock = e.lock in
      e.lock <- SET.add k e.lock ;
      let d = f k in
      e.index <- MAP.add k d e.index ;
      fire k d ;
      e.lock <- lock ; 
      d (* in case of exception, the entry remains intentionally locked *)

  let compile f k =
    ignore (memoize f k)

  let iter f = MAP.iter f (entries()).index

  let iter_sorted f =
    let e = entries () in
    let s = MAP.fold (fun k _ s -> SET.add k s) e.index SET.empty in
    SET.iter (fun k -> f k (MAP.find k e.index)) s

end

module type Key =
sig
  type t
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
end

module type Data =
sig
  type key
  type data
  val name : string
  val compile : key -> data
end

module type Generator =
sig
  type key
  type data
  val get : key -> data
end

module Generator(K : Key)(D : Data with type key = K.t) =
struct

  module G = Index
    (struct
       include K
       include D
     end)

  type key = D.key
  type data = D.data
  let get = G.memoize D.compile

end
