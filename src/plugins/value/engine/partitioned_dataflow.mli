(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
                                 and type summary = Domain.summary)
    (* Transfer functions for the logic on the abstract domain. *)
    (Logic : Transfer_logic.S with type state = Domain.t
                               and type states = States.t)
  : sig

    val compute:
      kernel_function -> kinstr -> Domain.t ->
      (Domain.t, Domain.summary, Domain.value) call_result
      * Value_types.cacheable

  end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
