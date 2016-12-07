(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
    (* Abstract domain with partitioning. *)
    (Domain: Abstract_domain.External)
    (* Set of states of abstract domain. *)
    (States : Partitioning.StateSet with type state = Domain.t)
    (* Transfer functions for statement on the abstract domain. *)
    (Transfer : Transfer_stmt.S with type state = Domain.t
                                 and type value = Domain.value
                                 and type return = Domain.return)
    (* Transfer functions for the logic on the abstract domain. *)
    (Logic : Transfer_logic.S with type state = Domain.t
                               and type states = States.t)
  : sig

    val compute:
      kernel_function -> kinstr -> Domain.t ->
      (Domain.t, Domain.return, Domain.value) call_result
      * Value_types.cacheable

  end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
