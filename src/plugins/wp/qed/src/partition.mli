(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Union-find based partitions *)

module type Elt =
sig
  type t
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module type Set =
sig
  type t
  type elt
  val singleton : elt -> t
  val iter : (elt -> unit) -> t -> unit
  val union : t -> t -> t
  val inter : t -> t -> t
end

module type Map =
sig
  type 'a t
  type key
  val empty : 'a t
  val is_empty : 'a t -> bool
  val find : key -> 'a t -> 'a
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end


module Make(E : Elt)
    (S : Set with type elt = E.t)
    (M : Map with type key = E.t) :
sig
  type t
  type elt = E.t
  type set = S.t

  val empty : t
  val equal : t -> elt -> elt -> bool
  val merge : t -> elt -> elt -> t
  val merge_list : t -> elt list -> t
  val merge_set : t -> set -> t
  val lookup : t -> elt -> elt
  val members : t -> elt -> set
  val iter : (elt -> set -> unit) -> t -> unit
  val unstable_iter : (elt -> elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val is_empty : t -> bool
end
