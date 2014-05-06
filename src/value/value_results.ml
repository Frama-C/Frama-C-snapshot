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

(* This file will ultimately contain all the results computed by Value
   (which must be moved out of Db.Value), both per stack and globally. *)

(* Does the given call statement terminate at least once. Filled only
   for [Instr (Call _)] stmts *)
module Terminating_calls =
  Cil_state_builder.Stmt_hashtbl
    (Datatype.Bool)
    (struct
      let name = "Value_results.Terminating_calls"
      let size = 17
      let dependencies = [ Db.Value.self ]
    end)

let mark_call_terminating stmt =
  Terminating_calls.replace stmt true

let is_non_terminating_call stmt = match stmt.skind with
  | Instr (Call _) -> not (Terminating_calls.mem stmt)
  | _ -> false



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
