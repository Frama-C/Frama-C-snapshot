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
(** Floatting Arithmetic Model *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Lang
open Lang.F

type model = Real | Float
val configure : model -> unit

val code_lit : float -> term
val acsl_lit : Cil_types.logic_real -> term

val real_of_int : unop
val float_of_int : c_float -> unop
val fconvert : c_float -> unop
val frange : c_float -> term -> pred

val fopp : c_float -> unop
val fadd : c_float -> binop
val fsub : c_float -> binop
val fmul : c_float -> binop
val fdiv : c_float -> binop

val f_iabs : lfun
val f_rabs : lfun
val f_sqrt : lfun
val f_model : lfun
val f_delta : lfun
val f_epsilon : lfun

val flt_rnd : c_float -> lfun
val flt_add : c_float -> lfun
val flt_mul : c_float -> lfun
val flt_div : c_float -> lfun
val flt_sqrt : c_float -> lfun
