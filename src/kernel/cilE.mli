(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(** Cil Extension for Frama-C.
    @plugin development guide *)

(** Display a localized warning only once per location and per message. *)
val warn_once : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Display a warning only once per message. *)
val log_once : ('a, Format.formatter, unit, unit) format4 -> 'a

(*val compact_body : Cil_types.fundec -> unit*)

(* Restore correct gotos after a statement substitution *)
val update_gotos :
  Cil_types.stmt Cilutil.StmtMap.t -> Cil_types.block -> Cil_types.block

type syntactic_context =
  | SyNone
  | SyBinOp of Cil_types.binop * Cil_types.exp * Cil_types.exp
  | SyUnOp of  Cil_types.exp
  | SyMem of  Cil_types.lval
  | SySep of Cil_types.exp * Cil_types.exp
      (** assert that two locations must be separated *)

val start_stmt : Cil_types.kinstr -> unit
val end_stmt : unit -> unit
val current_stmt : unit -> Cil_types.kinstr

val set_syntactic_context : syntactic_context -> unit
val get_syntactic_context : unit -> Cil_types.kinstr*syntactic_context

type alarm_behavior =
    Aignore  (* pretend that the problematic values do not happen *)
    | Alog (* log the alarm using the global variable that has been set
	     with set_syntactic_context, and continue,
	     pretending that the problematic values do not happen *)
    | Acall of (unit -> unit)
	(* call function -- in a future version, more information will be
	   passed to the function *)

type warn_mode = {unspecified:alarm_behavior;
                  others: alarm_behavior;
                  imprecision_tracing:alarm_behavior}

val warn_all_mode : warn_mode
val warn_none_mode : warn_mode

val warn_div : warn_mode -> unit
val warn_shift : warn_mode -> int -> unit
val warn_mem_read : warn_mode -> unit
val warn_mem_write : warn_mode -> unit
val warn_index : warn_mode -> string -> unit
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
compile-command: "LC_ALL=C make -C ../.."
End:
*)
