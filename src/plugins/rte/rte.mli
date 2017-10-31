(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type 'a alarm_gen =
  remove_trivial:bool ->
  on_alarm:(?status:Property_status.emitted_status ->
            Alarms.alarm ->
            unit) ->
  'a -> unit
(** ['a alarm_gen] is an abstraction over the process of generating a certain
    kind of RTEs over something of type ['a].
    The [on_alarm] argument receives all corresponding alarms, with
    optionally a status indicating that the alarm is red. *)

val lval_assertion: read_only: Alarms.access_kind -> lval alarm_gen
val lval_initialized_assertion: lval alarm_gen
val divmod_assertion: exp alarm_gen
val signed_div_assertion: (exp * exp * exp) alarm_gen
val shift_width_assertion: (exp * int option) alarm_gen
val shift_overflow_assertion: signed:bool -> (exp * binop * exp * exp) alarm_gen
val mult_sub_add_assertion: signed:bool -> (exp * binop * exp * exp) alarm_gen
val uminus_assertion: exp alarm_gen
val signed_downcast_assertion: (typ * exp) alarm_gen
val unsigned_downcast_assertion: (typ * exp) alarm_gen
val float_to_int_assertion: (typ * exp) alarm_gen
val finite_float_assertion: (fkind * exp) alarm_gen
val pointer_call: exp alarm_gen

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
