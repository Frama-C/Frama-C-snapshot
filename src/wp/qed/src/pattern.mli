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

open Logic

(* -------------------------------------------------------------------------- *)
(* --- Pattern Matching                                                   --- *)
(* -------------------------------------------------------------------------- *)

type ('z,'f,'e) fpattern =
  | Ptrue
  | Pfalse
  | Pint of 'z
  | Pvar of int
  | Pguard of int * ('e -> bool)
  | Pfun of 'f * ('z,'f,'e) fpattern list

val size : ('z,'f,'e) fpattern -> int (** Number of pattern variables. *)
val size_list : ('z,'f,'e) fpattern list -> int (** Number of all patterns variables. *)

module Make(T : Term) :
sig
  open T
  type pattern = (Z.t,Fun.t,T.t) fpattern

  val pmatch : pattern -> term -> term array
  (** Raise [Not_found] or returns the substitution as an array
      	indexed by pattern variable number. *)

  val pmatch_all : pattern list -> term list -> term array
  (** Raise [Not_found] or returns the substitution as an array
      	indexed by pattern variable number. *)

  val pretty : pattern Plib.printer

end

