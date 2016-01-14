(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** This file will ultimately contain all the results computed by Value
    (which must be moved out of Db.Value), both per stack and globally. *)


open Cil_types

val mark_kf_as_called: kernel_function -> unit
val add_kf_caller: caller:kernel_function * stmt -> kernel_function -> unit

val partition_terminating_instr: stmt -> Db.Value.callstack list * Db.Value.callstack list
(** Returns the list of terminating callstacks and the list of non-terminating callstacks.
    Must be called *only* on statements that are instructions. *)
val is_non_terminating_instr: stmt -> bool
(** Returns [true] iff there exists executions of the statement that does
    not always fail/loop (for function calls). Must be called *only* on
    statements that are instructions. *)

type state_per_stmt = Cvalue.Model.t Cil_datatype.Stmt.Hashtbl.t
val merge_states_in_db:
  state_per_stmt Lazy.t -> Db.Value.callstack -> unit
val merge_after_states_in_db:
  state_per_stmt Lazy.t -> Db.Value.callstack -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
