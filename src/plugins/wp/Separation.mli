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

open Cil_types

(** Elementary regions *)
type region =
  | Var of varinfo (** the variable, [&x] *)
  | Ptr of varinfo (** the cell pointed by [p] *)
  | Arr of varinfo (** the array around [p] *)

(** Prints region in ACSL format *)
val pp_region : Format.formatter -> region -> unit

(** List of regions to be separated.
    The ACSL interpretation of this compact [separation] clause is:
    {v
    //@ requires: \separated(mutex_1, ..., mutex_n, \union(other_1, ..., other_m) ); 
    v}
    Such a specification actually consists of [(n-1)*n/2 + n*m] elementary separation clauses.
*)
type clause = {
  mutex : region list ;
  other : region list ;
}

val is_true : clause -> bool
(** Returns [true] if the clause is degenerated. 
    This occurs when [mutex] is empty, or when [mutex] is a singleton and [other] is empty. *)

val requires : clause list -> clause list
(** Filter out [is_true] clauses *)

(** Prints the separation in ACSL format. *)
val pp_clause : Format.formatter -> clause -> unit
