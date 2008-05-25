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

(* $Id: datatype.ml,v 1.6 2008/04/10 15:48:06 uid562 Exp $ *)

module Nop = struct
  let before_load () = ()
  let after_load () = ()
end

module Int = Project.Datatype.Persistent(struct type t = int end)
module Bool = Project.Datatype.Persistent(struct type t = bool end)
module String = Project.Datatype.Imperative(String)
module BigInt = Project.Datatype.Persistent(struct type t = Big_int.big_int end)

module type INPUT = sig
  include Project.Datatype.INPUT
  val self : Project.Datatype.t
end

module List(Data:INPUT) = 
  Project.Datatype.Register
    (struct
       type t = Data.t list
       let rehash = List.map Data.rehash
       let copy = List.map Data.copy
       include Nop
       let dependencies = [ Data.self ]
       let name = Project.Datatype.Name.extend "list" Data.name
     end)

module type HASHTBL = sig
  type key
  type 'a t
  val create: int -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val add: 'a t -> key -> 'a -> unit
  val replace: 'a t -> key -> 'a -> unit
  val length: 'a t -> int
end

module Make_Hashtbl(H: HASHTBL)(Data:INPUT) =
  Project.Datatype.Register
    (struct
       type t = Data.t H.t
       let map f tbl =
	 let h = H.create (H.length tbl) in
	 H.iter (fun k v -> H.replace h k (f v)) tbl;
	 h
       let rehash = map Data.rehash
       let copy = map Data.copy
       include Nop
       let dependencies = [ Data.self ] 
       let name = Project.Datatype.Name.extend "hashtbl" Data.name
     end)

module Ref(Data:INPUT) =
  Project.Datatype.Register
    (struct
       type t = Data.t ref
       let rehash x = ref (Data.rehash !x)
       let copy x = ref (Data.copy !x)
       include Nop
       let dependencies = [ Data.self ] 
       let name = Project.Datatype.Name.extend "ref" Data.name
     end)

module Option(Data:INPUT) =
  Project.Datatype.Register(struct
	type t = Data.t option
	let rehash = Extlib.opt_map Data.rehash
	let copy = Extlib.opt_map Data.rehash
	include Nop
	let self = Data.self
	let dependencies = [ Data.self ]
	let name = Project.Datatype.Name.extend "option" Data.name
      end)

module OptionRef(Data:INPUT) = Ref(Option(Data))

module type SET_INPUT = sig
  include INPUT
  val compare: t -> t -> int
end

module type SET = sig
  type elt
  type t
  val empty: t
  val singleton: elt -> t
  val add: elt -> t -> t
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module type MAP = sig
  type key
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

module Make_Map(Map:MAP)(Data:INPUT) = 
  Project.Datatype.Register
    (struct
       type t = Data.t Map.t
       let map f m = Map.fold (fun k d -> Map.add k (f d)) m Map.empty
       let rehash = map Data.rehash
       let copy = map Data.copy
       include Nop
       let dependencies = [ Data.self ]
       let name = Project.Datatype.Name.extend "map" Data.name
     end)

module Make_Set(Set:SET)(Data:INPUT with type t = Set.elt) = 
  Project.Datatype.Register
    (struct
       type t = Set.t
       let map f set = Set.fold (fun d -> Set.add (f d)) set Set.empty
       let rehash = map Data.rehash
       let copy = map Data.copy
       include Nop
       let dependencies = [ Data.self ]
       let name = Project.Datatype.Name.extend "set" Data.name
     end)

module Set(Data:sig include INPUT val compare:t -> t -> int end) = 
  Make_Set(Set.Make(Data))(Data)

module Make_SetRef(Set:SET)(Data:INPUT with type t = Set.elt) = 
  Ref(Make_Set(Set)(Data))

(** {3 Queues} *)

module Queue(Data:INPUT) =
  Project.Datatype.Register
    (struct
       type t = Data.t Queue.t
       let rehash q =
	 let q' = Queue.create () in
	 Queue.iter (fun x -> Queue.add (Data.rehash x) q') q;
	 q'
       let copy _ = assert false (* TODO: deep copy *)
       include Nop
       let name = Project.Datatype.Name.extend "queue" Data.name
       let dependencies = [ Data.self ]
     end)

(** {3 Tuples} *)

module Couple(D1:INPUT)(D2:INPUT) =
  Project.Datatype.Register
    (struct
       type t = D1.t * D2.t
       let rehash (x, y) = D1.rehash x, D2.rehash y
       let copy (x, y) = D1.copy x, D2.copy y
       include Nop
       let name = Project.Datatype.Name.extend2 "couple" D1.name D2.name
       let dependencies = [ D1.self; D2.self ]
     end)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
