(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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

