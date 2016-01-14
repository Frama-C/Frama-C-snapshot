(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
(** Bound Variables Footprints.

    All provided operation are constant-time bitwise and integer operations.
*)
(* -------------------------------------------------------------------------- *)

type t (** An over-approximation of set of integers *)

val empty : t
val singleton : int -> t

val order : t -> int (** Max stack of binders *)
val bind : t -> t (** Decrease all elements in [s] after removing [0] *)

val union : t -> t -> t

val closed : t -> bool (** All variables are bound *)
val closed_at : int -> t -> bool
(** [closed_at n a] Does not contains variables [k<n] *)

val is_empty : t -> bool
(** No bound variables *)

val contains : int -> t -> bool
(** if [may_constains k s] returns [false] then [k] does not belong to [s] *)

val overlap : int -> int -> t -> bool
(** if [may_overlap k n s] returns [false] then no variable [i] with
    [k<=i<k+n] occurs in [s]. *)

val pretty : Format.formatter -> t -> unit
