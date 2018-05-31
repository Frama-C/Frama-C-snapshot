(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
(** Floating Arithmetic Model *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Lang
open Lang.F

type model = Real | Float
val configure : model -> unit

val code_lit : float -> term
val acsl_lit : Cil_types.logic_real -> term

val float_of_int : c_float -> unop
val float_of_real : c_float -> unop
val real_of_float : c_float -> unop
val range : c_float -> term -> pred

val fopp : c_float -> unop
val fadd : c_float -> binop
val fsub : c_float -> binop
val fmul : c_float -> binop
val fdiv : c_float -> binop

val flt : c_float -> cmp
val fle : c_float -> cmp
val feq : c_float -> cmp
val fneq : c_float -> cmp

val f_model : lfun
val f_delta : lfun
val f_epsilon : lfun

val flt_rnd : c_float -> lfun
val flt_add : c_float -> lfun
val flt_mul : c_float -> lfun
val flt_div : c_float -> lfun
