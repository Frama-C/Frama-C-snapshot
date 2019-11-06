(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(** Gadt describing the structure of a tree of different data types,
    and providing fast accessors of its nodes.
    The leafs must provide a key from a Key module, see key.mli for details. *)

(** Equality witness between types. *)
type (_,_) eq = Eq : ('a,'a) eq

(** Keys identifying datatypes. *)
module type Key = sig
  type 'a key

  val create_key: string -> 'a key
  val eq_type : 'a key -> 'b key -> ('a, 'b) eq option

  val print: 'a key Pretty_utils.formatter
  val compare: 'a key -> 'b key -> int
  val equal: 'a key -> 'b key -> bool
  val hash : 'a key -> int
  val tag: 'a key -> int
end

module Make (X : sig end) : Key

(** Keys module for the abstract values of Eva. *)
module Key_Value : Key

(** Keys module for the abstract locations of Eva. *)
module Key_Location : Key

(** Keys module for the abstract domains of Eva. *)
module Key_Domain : Key

(** A Key module with its structure type. *)
module type Shape = sig
  include Key
  type 'a data

  (** The gadt, based on keys giving the type of each node.
      Describes the internal structure of a data type.
      Used internally to automatically generate efficient accessors of its nodes. *)
  type 'a structure =
    | Unit : unit structure
    | Leaf : 'a key * 'a data -> 'a structure
    | Node : 'a structure * 'b structure -> ('a * 'b) structure

  val eq_structure: 'a structure -> 'b structure -> ('a, 'b) eq option
end

module Shape (Key: Key) (Data: sig type 'a t end) :
  Shape with type 'a key = 'a Key.key
         and type 'a data = 'a Data.t

(** Internal view of the tree, with the structure. *)
module type Internal = sig
  type t
  type 'a structure
  val structure : t structure
end

(** External view of the tree, with accessors.
    Automatically built by the functor {!Open} from an {!Internal} datatype.
    When a generic datatype is a combination of several other datatypes, these
    functions allow interacting with its subparts. Note that their behavior is
    undefined if the overall datatype contains several times the same datatype. *)
module type External = sig
  type t
  type 'a key
  val mem : 'a key -> bool
  val get : 'a key -> (t -> 'a) option
  val set : 'a key -> 'a -> t -> t
end

(** Opens an internal tree module into an external one. *)
module Open
    (Shape : Shape)
    (Data : Internal with type 'a structure := 'a Shape.structure)
  : External with type t := Data.t
              and type 'a key := 'a Shape.key
