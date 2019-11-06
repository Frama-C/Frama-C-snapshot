(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

type model = {
  id : string ; (* Identifier Basename for Model (unique) *)
  descr : string ; (* Title of the Model (for pretty) *)
  emitter : Emitter.t ;
  hypotheses : hypotheses ;
  tuning : tuning list ;
}

and tuning = unit -> unit
and scope = Global | Kf of Kernel_function.t
and hypotheses = unit -> MemoryContext.clause list
and context = model * scope
and t = context

let nohyp (_kf) = []

module MODEL =
struct
  type t = model
  let id a = a.id
  let descr a = a.descr
  let hash a = Hashtbl.hash a.id
  let equal a b = String.equal a.id b.id
  let compare a b = String.compare a.id b.id
  let repr = {
    id = "?model" ; descr = "?model" ;
    emitter = Emitter.kernel ;
    tuning = [ fun () -> () ] ;
    hypotheses = nohyp ;
  }
end

module MODELS =
struct

  module H = Datatype.String.Map
  let h = ref H.empty
  (* NOT PROJECTIFIED : Models are defined at Plugin load-time,
     for all projects *)

  let mem id = H.mem id !h
  let add m = h := H.add m.id m !h

end

let register ~id ?(descr=id) ?(tuning=[]) ?(hypotheses=nohyp) () =
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
    descr ;
    emitter ;
    tuning ;
    hypotheses ;
  } in
  MODELS.add model ; model

let get_descr m = m.descr
let get_emitter m = m.emitter

module SCOPE =
struct
  type t = scope
  let id = function
    | Global -> "__frama_c_global"
    | Kf f -> Kernel_function.get_name f
  let compare f g =
    match f,g with
    | Global , Global -> 0
    | Global , _ -> (-1)
    | _ , Global -> 1
    | Kf f , Kf g -> Kernel_function.compare f g
  let equal f g = (compare f g = 0)
  let hash = function Global -> 0 | Kf kf -> Kernel_function.hash kf
end

module S =
struct
  type t = context
  let id (model,scope) =
    match scope with
    | Global -> model.id
    | Kf kf -> Printf.sprintf "%s_%s" model.id (Kernel_function.get_name kf)
  let hash (m,s) = match s with
    | Global -> 2 * MODEL.hash m
    | Kf kf -> 3 * MODEL.hash m + 5 * Kernel_function.hash kf
  let equal (m1,s1) (m2,s2) = MODEL.equal m1 m2 && SCOPE.equal s1 s2
  let compare (m1,s1) (m2,s2) =
    let cmp = MODEL.compare m1 m2 in
    if cmp<>0 then cmp else SCOPE.compare s1 s2
end

let context : (string * context) Context.value = Context.create "WpContext"

let configure (model,_) = List.iter (fun f -> f()) model.tuning
let rollback = function None -> () | Some (_,ctxt) -> configure ctxt

let on_context gamma f x =
  let id = S.id gamma in
  let current = Context.push context (id,gamma) in
  try
    Context.configure () ;
    configure gamma ;
    let result = f x in
    Context.pop context current ;
    rollback current ; result
  with err ->
    Context.pop context current ;
    rollback current ; raise err

let is_defined () = Context.defined context
let get_ident () = Context.get context |> fst
let get_context () = Context.get context |> snd
let get_model () = get_context () |> fst
let get_scope () = get_context () |> snd

let compute_hypotheses m f = on_context (m,Kf f) m.hypotheses ()

let directory () = get_model () |> MODEL.id |> Wp_parameters.get_output_dir

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
  module E : Entries
  type key = E.key
  type data = E.data

  val id : basename:string -> key -> string
  val mem : key -> bool
  val find : key -> data
  val get : key -> data option
  val clear : unit -> unit
  val remove : key -> unit
  val define : key -> data -> unit
  val update : key -> data -> unit
  val memoize : (key -> data) -> key -> data
  val compile : (key -> data) -> key -> unit
  val callback : (key -> data -> unit) -> unit
  val iter : (key -> data -> unit) -> unit
  val iter_sorted : (key -> data -> unit) -> unit
end

let types = Hashtbl.create 8
let freetype a =
  try
    let n = Hashtbl.find types a in
    Hashtbl.replace types a (succ n) ;
    Printf.sprintf "%s#%d" a n
  with Not_found ->
    Hashtbl.add types a 1 ; a

module NAMES = FCMap.Make(String)

module Index(E : Entries) =
struct

  module E = E

  type key = E.key
  type data = E.data

  module KEY = struct type t = E.key let compare = E.compare end
  module MAP = FCMap.Make(KEY)
  module SET = FCSet.Make(KEY)

  let demon = ref []

  type entries = {
    mutable index : E.data MAP.t ;
    mutable ident : string MAP.t ;
    mutable names : int NAMES.t ;
    mutable lock : SET.t ;
  }

  let create () = {
    index=MAP.empty;
    ident=MAP.empty;
    names=NAMES.empty;
    lock=SET.empty;
  }

  module ENTRIES : Datatype.S with type t = entries =
    Datatype.Make
      (struct
        type t = entries
        include Datatype.Undefined
        let mem_project = Datatype.never_any_project
        let reprs = [create ()]
        let name = freetype ("Wp.Context.Index." ^ E.name)
      end)

  module REGISTRY = State_builder.Hashtbl
      (Datatype.String.Hashtbl)
      (ENTRIES)
      (struct
        let name = freetype ("Wp.Context." ^ E.name)
        let dependencies = [Ast.self]
        let size = 32
      end)
  (* Projectified entry map, indexed by model *)

  let entries () : entries =
    let cid = get_ident () in
    try REGISTRY.find cid
    with Not_found ->
      let e = create () in REGISTRY.add cid e ; e

  let clear () =
    begin
      let e = entries () in
      e.index <- MAP.empty ;
      e.lock <- SET.empty ;
    end

  let remove k =
    begin
      let e = entries () in
      e.index <- MAP.remove k e.index ;
      e.lock <- SET.remove k e.lock ;
    end

  let mem k = let e = entries () in MAP.mem k e.index || SET.mem k e.lock
  let find k = let e = entries () in MAP.find k e.index

  let get k = try Some (find k) with Not_found -> None

  let id ~basename k =
    begin
      let e = entries () in
      try MAP.find k e.ident with Not_found ->
        let kid,id =
          try
            let kid = succ (NAMES.find basename e.names) in
            kid,Printf.sprintf "%s_%d" basename kid
          with Not_found ->
            0,basename
        in
        e.names <- NAMES.add basename kid e.names ;
        e.ident <- MAP.add k id e.ident ; id
    end

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

module Static(E : Entries) =
struct

  module E = E

  type key = E.key
  type data = E.data

  module KEY = struct type t = E.key let compare = E.compare end
  module MAP = FCMap.Make(KEY)
  module SET = FCSet.Make(KEY)

  let demon = ref []

  type entries = {
    mutable index : E.data MAP.t ;
    mutable ident : string MAP.t ;
    mutable names : int NAMES.t ;
    mutable lock : SET.t ;
  }

  let create () = {
    index=MAP.empty;
    ident=MAP.empty;
    names=NAMES.empty;
    lock=SET.empty;
  }

  module ENTRIES : Datatype.S with type t = entries =
    Datatype.Make
      (struct
        type t = entries
        include Datatype.Undefined
        let reprs = [create ()]
        let name = "Wp.Context.Index." ^ E.name
        let mem_project = Datatype.never_any_project
      end)

  module REGISTRY = State_builder.Ref
      (ENTRIES)
      (struct
        let name = "Wp.Context." ^ E.name
        let dependencies = [Ast.self]
        let default = create
      end)
  (* Projectified entry map *)

  let entries () : entries = REGISTRY.get ()

  let clear () =
    begin
      let e = entries () in
      e.index <- MAP.empty ;
      e.lock <- SET.empty ;
    end

  let remove k =
    begin
      let e = entries () in
      e.index <- MAP.remove k e.index ;
      e.lock <- SET.remove k e.lock ;
    end

  let mem k = let e = entries () in MAP.mem k e.index || SET.mem k e.lock

  let find k = let e = entries () in MAP.find k e.index
  let get k = try Some (find k) with Not_found -> None

  let id ~basename k =
    begin
      let e = entries () in
      try MAP.find k e.ident with Not_found ->
        let kid,id =
          try
            let kid = succ (NAMES.find basename e.names) in
            kid,Printf.sprintf "%s_%d" basename kid
          with Not_found ->
            0,basename
        in
        e.names <- NAMES.add basename kid e.names ;
        e.ident <- MAP.add k id e.ident ; id
    end

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

module type IData =
sig
  type key
  type data
  val name : string
  val basename : key -> string
  val compile : key -> string -> data
end

module type Generator =
sig
  type key
  type data
  val get : key -> data
  val mem : key -> bool
  val clear : unit -> unit
  val remove : key -> unit
end

module StaticGenerator(K : Key)(D : Data with type key = K.t) =
struct

  module G = Static
      (struct
        include K
        include D
      end)

  type key = D.key
  type data = D.data
  let get = G.memoize D.compile
  let mem = G.mem
  let clear = G.clear
  let remove = G.remove
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
  let mem = G.mem
  let clear = G.clear
  let remove = G.remove
end

module GeneratorID(K : Key)(D : IData with type key = K.t) =
struct

  module G = Index
      (struct
        include K
        include D
      end)

  type key = D.key
  type data = D.data
  let get = G.memoize (fun k -> D.compile k (G.id ~basename:(D.basename k) k))
  let mem = G.mem
  let clear = G.clear
  let remove = G.remove
end

module StaticGeneratorID(K : Key)(D : IData with type key = K.t) =
struct

  module G = Static
      (struct
        include K
        include D
      end)

  type key = D.key
  type data = D.data
  let get = G.memoize (fun k -> D.compile k (G.id ~basename:(D.basename k) k))
  let mem = G.mem
  let clear = G.clear
  let remove = G.remove
end
