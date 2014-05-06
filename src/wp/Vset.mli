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

open Lang.F

(** Logical Sets *)

type set = vset list
and vset =
  | Set of tau * term
  | Singleton of term
  | Range of term option * term option
  | Descr of var list * term * pred

val tau_of_set : tau -> tau

val vars : set -> Vars.t
val occurs : var -> set -> bool

val empty : set
val singleton : term -> set
val range : term option -> term option -> set
val union : set -> set -> set
val inter : term -> term -> term

val member : term -> set -> pred
val in_size : term -> int -> pred
val in_range : term -> term option -> term option -> pred
val sub_range : term -> term -> term option -> term option -> pred
val ordered : limit:bool -> strict:bool -> term option -> term option -> pred
  (** - [limit]: result when either parameter is [None]
      - [strict]: if [true], comparison is [<] instead of [<=] *)

val equal : set -> set -> pred
val subset : set -> set -> pred
val disjoint : set -> set -> pred

val concretize : set -> term

val pp_bound : Format.formatter -> term option -> unit
val bound_shift : term option -> term -> term option
val bound_add : term option -> term option -> term option
val bound_sub : term option -> term option -> term option

(** {3 Maping}
    These operations computes different kinds of [{f x y with x in A, y in B}].
*)

val map : (term -> term) -> set -> set
val map_opp : set -> set

(** {3 Lifting}
    These operations computes different sort of [{f x y with x in A, y in B}].
*)

val lift : (term -> term -> term) -> set -> set -> set
val lift_add : set -> set -> set
val lift_sub : set -> set -> set

val descr : vset -> var list * term * pred
