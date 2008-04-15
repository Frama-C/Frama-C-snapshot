(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: computation.ml,v 1.15 2008/11/18 12:13:41 uid568 Exp $ *)

module type HASHTBL = sig
  include Datatype.HASHTBL
  val clear: 'a t -> unit
  val find: 'a t -> key -> 'a
  val remove: 'a t -> key -> unit
  val mem: 'a t -> key -> bool
end

module type HASHTBL_OUTPUT = sig
  include Project.Computation.OUTPUT
  type key
  type data
  val replace: key -> data -> unit 
  val add: key -> data -> unit
  val clear: unit -> unit
  val length: unit -> int
  val iter: (key -> data -> unit) -> unit
  val fold: (key -> data -> 'a -> 'a) -> 'a -> 'a
  val memo: ?change:(data -> data) -> (key -> data) -> key -> data
    (** @plugin development guide *)
  val find: key -> data
  val find_all: key -> data list
  val unsafe_find: key -> data
  val mem: key -> bool
  val remove: key -> unit
end

module Make_Hashtbl
  (H:HASHTBL)(Data:Project.Datatype.S)(Info:Signature.NAME_SIZE_DPDS) = 
struct

  type key = H.key
  type data = Data.t
      
  let create () = H.create Info.size
    
  let state = ref (create ())
    
  include Project.Computation.Register
  (Datatype.Make_Hashtbl(H)(Data))
  (struct
     type t = data H.t
     let create = create
     let clear = H.clear
     let get () = !state
     let set x = state := x
   end)
  (Info)

  let clear () = H.clear !state
  let length () = H.length !state
  let replace key v = H.replace !state key v
  let add key v = H.add !state key v
  let find key = H.find !state key
  let find_all key = H.find_all !state key
  let unsafe_find key = try find key with Not_found -> assert false
  let mem key = H.mem !state key
  let remove key = H.remove !state key
  let iter f = H.iter f !state
  let fold f acc = H.fold f !state acc

  let memo ?change f key =
    try
      let old = find key in
      Extlib.may_map 
	~dft:old (fun f -> let v = f old in replace key v; v) change
    with Not_found ->
      let data = f key in
      replace key data;
      data

end

module Hashtbl(Key:Hashtbl.HashedType) = Make_Hashtbl(Hashtbl.Make(Key))

module type REF_INPUT = sig
  include Project.Datatype.S
  val default: unit -> t
end

module type REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type data
  val set: data -> unit
  val get: unit -> data
  val clear: unit -> unit
end

module Ref(Data:REF_INPUT)(Info:Signature.NAME_DPDS) = struct

  type data = Data.t

  let create () = ref (Data.default ())
  let state = ref (create ())

  include Project.Computation.Register
  (Datatype.Ref(Data))
  (struct
     type t = data ref
     let create = create
     let clear tbl = tbl := Data.default ()
     let get () = !state
     let set x = state := x
   end)
  (Info)

  let set v = !state := v
  let get () = !(!state)
  let clear () = !state := Data.default ()

end

module type OPTION_REF_OUTPUT = sig
  include REF_OUTPUT
  val memo: ?change:(data -> data) -> (unit -> data) -> data
  val map: (data -> data) -> data option
  val may: (data -> unit) -> unit
end

module OptionRef
  (Data:Project.Datatype.S)(Info:Signature.NAME_DPDS) = struct

  type data = Data.t

  let create () = ref None
  let state = ref (create ())

  include Project.Computation.Register
  (Datatype.OptionRef(Data))
  (struct
     type t = data option ref
     let create = create
     let clear tbl = tbl := None
     let get () = !state
     let set x = state := x
   end)
  (Info)

  let set v = !state := Some v
  let get () = match !(!state) with None -> raise Not_found | Some v -> v
  let clear () = !state := None

  let memo ?change f =
    try
      let old = get () in
      Extlib.may_map 
	~dft:old (fun f -> let v = f old in set v; v) change
    with Not_found ->
      let data = f () in
      set data;
      data

  let map f = Extlib.opt_map f !(!state)
  let may f = Extlib.may f !(!state)

end

module type SET = sig
  type elt
  type t
  val empty: t
  val singleton: elt -> t
  val is_empty: t -> bool
  val add: elt -> t -> t
  val mem: elt -> t -> bool
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (elt -> unit) -> t -> unit
end

module type SET_REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type elt
  val add: elt -> unit
  val mem: elt -> bool
  val is_empty: unit -> bool
  val fold: (elt -> 'a -> 'a) -> 'a -> 'a
  val iter: (elt -> unit) -> unit
end

module Make_SetRef
  (Set:SET)
  (Data:Project.Datatype.S with type t = Set.elt)
  (Info:Signature.NAME_DPDS) = 
struct
  include Ref
    (struct 
       include Datatype.Make_Set(Set)(Data)
       let default () = Set.empty
     end)
    (Info)
  type elt = Set.elt
  let apply f = f (get ())
  let is_empty () = apply Set.is_empty
  let add x = set (apply (Set.add x))
  let mem x = apply (Set.mem x)
  let fold f = apply (Set.fold f)
  let iter f = apply (Set.iter f)
end

module SetRef(Data:Project.Datatype.S) = Make_SetRef(Set.Make(Data))(Data)

(** {3 Queue} *)

module type QUEUE = sig
  type elt
  val add: elt -> unit
  val iter: (elt -> unit) -> unit
  val is_empty: unit -> bool
end

module Queue(Data:Project.Datatype.S)(Info:Signature.NAME_DPDS) = struct

  type elt = Data.t
      
  let state = ref (Queue.create ())
    
  include Project.Computation.Register
  (Datatype.Queue(Data))
  (struct
     type t = elt Queue.t
     let create = Queue.create
     let clear = Queue.clear
     let get () = !state
     let set x = state := x
   end)
  (Info)

  let add x = Queue.add x !state
  let iter f = Queue.iter f !state
  let is_empty () = Queue.is_empty !state

end

(** {3 Project itself} *)

module Project(Info:Signature.NAME_DPDS) = 
  Ref 
    (struct include Datatype.Project let default () = Project.dummy end)
    (Info)

(** {3 Apply Once} *)

let apply_once name dep f =
  let module First = 
    Ref
      (struct include Datatype.Bool let default () = true end)
      (struct let dependencies = dep let name = name end)
  in 
  (fun () ->
     if First.get () then begin
       First.set false;
       try f () with exn -> First.set true; raise exn
     end),
  First.self

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
