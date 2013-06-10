(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

open Logic

(* -------------------------------------------------------------------------- *)
(* --- Pattern Matching                                                   --- *)
(* -------------------------------------------------------------------------- *)

type 'f fpattern =
  | Pvar of int
  | Pint of Z.t
  | Pfun of 'f * 'f fpattern list

val size : 'a fpattern -> int (** Number of pattern variables. *)
val size_all : 'a fpattern list -> int (** Number of all patterns variables. *)

module Make(T : Term) :
sig
  open T
  type pattern = Fun.t fpattern

  val pmatch : pattern -> term -> term array
    (** Raise [Not_found] or returns the substitution as an array
	indexed by pattern variable number. *)

  val pmatch_all : pattern list -> term list -> term array
    (** Raise [Not_found] or returns the substitution as an array
	indexed by pattern variable number. *)

  val instance : term array -> pattern -> term
    (** Compute the term matched by the pattern and the substitution. *)

end

