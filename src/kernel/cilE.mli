(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** CIL Extension for Frama-C.
    @plugin development guide *)

(* ************************************************************************* *)
(* [JS 2011/03/11] All the below stuff manage warnings of the value analysis
   plug-in. Refactoring required. *)
(* ************************************************************************* *)

type syntactic_context =
  | SyNone
  | SyBinOp of Cil_types.binop * Cil_types.exp * Cil_types.exp
  | SyUnOp of  Cil_types.exp
  | SyMem of  Cil_types.lval
  | SyMemLogic of Cil_types.term
  | SySep of Cil_types.exp * Cil_types.exp
      (** assert that two locations must be separated *)

val start_stmt : Cil_types.kinstr -> unit
val end_stmt : unit -> unit
val current_stmt : unit -> Cil_types.kinstr

val set_syntactic_context : syntactic_context -> unit
val get_syntactic_context : unit -> Cil_types.kinstr*syntactic_context

type warn_origin = {
  warn_emitter: Emitter.t;
  warn_deps: State.t list;
}

type alarm_behavior =
  | Aignore
      (** pretend that the problematic values do not happen *)
  | Alog of warn_origin
      (** log the alarm using the global variable that has been set
          with set_syntactic_context, and continue,
          pretending that the problematic values do not happen *)
  | Acall of (unit -> unit)
      (** call function -- in a future version, more information will be
          passed to the function *)

val stop_if_stop_at_first_alarm_mode : unit -> unit

type warn_mode =
    { unspecified: alarm_behavior;
      others: alarm_behavior;
      imprecision_tracing: alarm_behavior }
      (** An argument of type [warn_mode] is required by some of the access
          functions in {!Db.Value}  (the interface to the value analysis). This
          argument tells what should be done with the various messages
          that the value analysis emits during the call.

          Each [warn_mode] field indicates the expected treatment for one
          category of message. These fields are not completely fixed
          yet. However, you can still used functions {!warn_all_mode} and
          {!warn_none_mode} below when you have to provide an argument of type
          [warn_mode]. *)

val warn_all_mode : warn_origin -> warn_mode
  (** Emit all messages, including alarms and informative messages
      regarding the loss of precision. *)

val warn_none_mode : warn_mode
  (** Do not emit any message. *)

val warn_div : warn_mode -> unit
val warn_shift : warn_mode -> int -> unit
val warn_shift_left_positive : warn_mode -> unit
val warn_mem_read : warn_mode -> unit
val warn_mem_write : warn_mode -> unit
val warn_signed_overflow : warn_mode -> Int64.t option -> Int64.t option -> unit

val warn_index : warn_mode -> string -> string -> unit
(** [warn_index w kind index] emite a warning signaling an out of bound
    access of kind [kind] at the index [index].
*)
val warn_pointer_comparison : warn_mode -> unit
val warn_result_nan_infinite : warn_mode -> unit
val warn_uninitialized : warn_mode -> unit
val warn_escapingaddr : warn_mode -> unit
(** warning to be emitted when two incompatible accesses to a location are
    done in unspecified order. Must be called in a [SyNone] or [SySep] context.
*)
val warn_separated : warn_mode -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
