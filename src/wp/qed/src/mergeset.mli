(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(** Merging Set Functor *)
(* -------------------------------------------------------------------------- *)

module type Elt =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Make(E : Elt) :
sig

  type elt = E.t

  type t = elt list Intmap.t

  val is_empty : t -> bool
  val empty : t

  (* good sharing *) 
  val add : elt -> t -> t
  val singleton : elt -> t
  val elements : t -> elt list

  (* good sharing *) 
  val remove : elt -> t -> t

  val mem : elt -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val iter_sorted : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_sorted: (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool

  val union : t -> t -> t
  val inter : t -> t -> t
  val subset : t -> t -> bool
  val intersect : t -> t -> bool

end
