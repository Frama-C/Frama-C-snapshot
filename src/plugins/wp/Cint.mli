(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
(** Integer Arithmetic Model *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Lang
open Lang.F

val of_real : c_int -> unop
val convert : c_int -> unop (** Independent from model *)

val to_integer : unop
val of_integer : c_int -> unop

val to_cint : lfun -> c_int (** Raises [Not_found] if not. *)
val is_cint : lfun -> c_int (** Raises [Not_found] if not. *)

type model = Natural | Machine
val configure : model -> unit
val current : unit -> model

val range : c_int -> term -> pred (** Dependent on model *)
val downcast : c_int -> unop (** Dependent on model *)

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

val f_bitwised : lfun list (** All except f_bit_positive *)

(** Simplifiers *)

val is_cint_simplifier: simplifier
(** Remove the [is_cint] in formulas that are
    redundant with other conditions. *)

val mask_simplifier: simplifier

val is_positive_or_null: term -> bool
