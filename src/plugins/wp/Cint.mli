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

(* -------------------------------------------------------------------------- *)
(** Integer Arithmetic Model *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Lang
open Lang.F

val of_real : c_int -> unop
val iconvert : c_int -> unop
val irange : c_int -> term -> pred

val to_cint : lfun -> c_int (** Raises [Not_found] if not. *)
val is_cint : lfun -> c_int (** Raises [Not_found] if not. *)

type model = NoRange | Natural | Machine
val configure : model -> unit

val iopp : c_int -> unop
val iadd : c_int -> binop
val isub : c_int -> binop
val imul : c_int -> binop
val idiv : c_int -> binop
val imod : c_int -> binop

val bnot : c_int -> unop
val band : c_int -> binop
val bxor : c_int -> binop
val bor  : c_int -> binop
val blsl : c_int -> binop
val blsr : c_int -> binop

val l_not : unop
val l_and : binop
val l_xor : binop
val l_or  : binop
val l_lsl : binop
val l_lsr : binop

val f_lnot : lfun
val f_land : lfun
val f_lxor : lfun
val f_lor  : lfun
val f_lsl  : lfun
val f_lsr  : lfun
val f_bit  : lfun

(** Simplifiers *)

val is_cint_simplifier: Conditions.simplifier
(** Remove the [is_cint] in formulas that are
    redondant with other conditions.
*)

val is_positive_or_null: term -> bool

