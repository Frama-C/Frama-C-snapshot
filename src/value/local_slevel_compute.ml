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

open Value_util
open Local_slevel_types

let compute_sub_function kf
                         start
                         info
                         initial_states =
  let current_merges = info.merges in
  (* FIXME [SCM] Strict mode only support *)
  assert(Cil_datatype.Stmt.Hptset.cardinal current_merges = 1);
  let module Computer =
    Eval_slevel.Computer
      (struct
        let kf = kf
        let slevel =
            match info.slevel with Some level -> level | None -> get_slevel kf
        let initial_states = initial_states (* for future reference *)
        let active_behaviors =
          Eval_annots.ActiveBehaviors.create (State_set.join initial_states) kf
        let local_slevel_info = info
       end)
  in
  let module Compute = Dataflow.Forwards(Computer) in

  let add_to_worklist stmt = Queue.add stmt Compute.worklist in
  Computer.add_to_worklist := add_to_worklist;

  (* FIX [SCM] Strict mode *)
  let merge = List.hd (Cil_datatype.Stmt.Hptset.elements current_merges) in
  (* Init the dataflow state for the first statement *)
  let dinit = { Computer.counter_unroll = 0; value = initial_states} in
  let dinit = Computer.computeFirstPredecessor start dinit in
  Computer.StmtStartData.add start dinit;
  Compute.compute [start];
  (* Here I will have to collect the results of all merges or returns in non
   * strict mode
   * maybe build a Cil_datatype.Stmt.Map, return this and let ret set
   * stmtstartdata
   * OR this is probably a map or hashtbl already - why double the work*)
  let state = Computer.getStateSet merge in
  Computer.merge_results ~inform:false;
  State_set.join state, Computer.clob.Locals_scoping.clob

let () = Local_slevel.compute_sub_function_ref := compute_sub_function

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
