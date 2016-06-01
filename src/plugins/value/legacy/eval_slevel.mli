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

(** Value analysis of statements and functions bodies with slevel. *)

(** Mark the analysis as aborted. It will be stopped at the next safe point *)
val signal_abort: unit -> unit

module Computer 
  (AnalysisParam : sig
     val kf : Cil_types.kernel_function
     val initial_states : State_set.t
     val active_behaviors : Eval_annots.ActiveBehaviors.t
   end) :
sig
  val compute: State_set.t -> unit

  val results: unit -> Value_types.call_result
  val merge_results : unit -> unit
  val mark_degeneration : unit -> unit
end
