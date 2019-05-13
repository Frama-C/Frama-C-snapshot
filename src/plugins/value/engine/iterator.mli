(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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
open Eval

(** Mark the analysis as aborted. It will be stopped at the next safe point *)
val signal_abort: unit -> unit

module Computer
    (* Abstractions with the evaluator. *)
    (Abstract: Abstractions.Eva)
    (* Set of states of abstract domain. *)
    (States : Powerset.S with type state = Abstract.Dom.t)
    (* Transfer functions for statement on the abstract domain. *)
    (Transfer : Transfer_stmt.S with type state = Abstract.Dom.t
                                 and type value = Abstract.Val.t)
    (* Initialization of local variables. *)
    (Init: Initialization.S with type state := Abstract.Dom.t)
    (* Transfer functions for the logic on the abstract domain. *)
    (Logic : Transfer_logic.S with type state = Abstract.Dom.t
                               and type states = States.t)
    (Spec: sig
       val treat_statement_assigns: assigns -> Abstract.Dom.t -> Abstract.Dom.t
     end)
  : sig

    val compute:
      kernel_function -> kinstr -> Abstract.Dom.t ->
      Abstract.Dom.t list or_bottom * Value_types.cacheable

  end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
