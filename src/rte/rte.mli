(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

type ('a, 'b) alarm_gen = 
    remove_trivial:bool -> warning:bool -> kernel_function -> kinstr -> 'a -> 'b

val lval_assertion: read_only: Alarms.access_kind -> (lval, unit) alarm_gen
val divmod_assertion: (exp, unit) alarm_gen
val signed_div_assertion: (exp * exp * exp, unit) alarm_gen
val shift_alarm: (exp * int option, unit) alarm_gen
val signed_shift_assertion: (exp * binop * exp * exp, unit) alarm_gen
val mult_sub_add_assertion: (bool * exp * binop * exp * exp, unit) alarm_gen
val uminus_assertion: (exp, unit) alarm_gen
val signed_downcast_assertion: (typ * exp, bool) alarm_gen
val unsigned_downcast_assertion: (typ * exp, bool) alarm_gen
val float_to_int_assertion: (typ * exp, bool) alarm_gen

val generated_annotations: unit -> code_annotation list
val reset_generated_annotations: unit -> unit

val save_alarms: bool ref

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
