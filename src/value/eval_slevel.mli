(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

module Computer 
  (AnalysisParam : sig
     val kf : Cil_types.kernel_function
     val slevel : int
     val initial_states : State_set.t
     val active_behaviors : Eval_annots.ActiveBehaviors.t
     val local_slevel_info : Local_slevel_types.local_slevel_info
   end) :
sig
  type u = { counter_unroll : int; mutable value : State_set.t; }
  include Dataflow.ForwardsTransfer with type t = u

  val merge_results : inform:bool -> unit
  val results: unit -> Value_types.call_result

  (* For local_slevel_compute: to be removed eventually *)
  val clob : Locals_scoping.clobbered_set
  val add_to_worklist : (Cil_datatype.Stmt.Hptset.elt -> unit) ref
  val getStateSet : Cil_types.stmt -> State_set.t
end
