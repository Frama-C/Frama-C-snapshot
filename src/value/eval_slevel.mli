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

(** Mark the analysis as aborted. It will be stopped at the next safe point *)
val signal_abort: unit -> unit

module Computer 
  (AnalysisParam : sig
     val kf : Cil_types.kernel_function
     val initial_states : State_set.t
     val active_behaviors : Eval_annots.ActiveBehaviors.t
   end) :
sig
  type u = { mutable to_propagate : State_set.t; }
  include Dataflow2.ForwardsTransfer with type t = u

  val merge_results : unit -> unit
  val mark_degeneration : unit -> unit
  val results: unit -> Value_types.call_result
end
