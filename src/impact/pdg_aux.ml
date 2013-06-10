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

open PdgIndex

(** [find_call_input_nodes pdg_caller s ?z input] find all the nodes of
    [pdg_caller] that define the pdg input [input] above the call statement [s].
    If [input] is an implicit input, its value is refined according to [z]. *)
(*   Copied from pdg/sets.ml, as it is currently not exported *)
let find_call_input_nodes pdg_caller call_stmt ?(z=Locations.Zone.top) in_key =
  match in_key with
  | PdgIndex.Signature.InCtrl
  | PdgIndex.Signature.InNum _ ->
    let idx = PdgTypes.Pdg.get_index pdg_caller in
    let _, call_sgn = FctIndex.find_call idx call_stmt in
    let node = PdgIndex.Signature.find_in_info call_sgn in_key in
    [ node, None ]
  | PdgIndex.Signature.InImpl zone ->
      let zone' = Locations.Zone.narrow zone z in
      let nodes, _undef = 
        !Db.Pdg.find_location_nodes_at_stmt
          pdg_caller call_stmt ~before:true zone'
      in
      nodes

let node_undef_list_to_set =
  List.fold_left (fun set (n, _) -> PdgTypes.NodeSet.add n set)
    PdgTypes.NodeSet.empty


let all_call_input_nodes pdg_caller (kf_callee, pdg_callee) call_stmt =
  let real_inputs =
    let inout =
      !Db.Operational_inputs.get_internal_precise ~stmt:call_stmt kf_callee
    in
    inout.Inout_type.over_inputs_if_termination
  in
  let test_in acc (in_key, in_node) =
    let default ?z () =
      let in_nodes = find_call_input_nodes pdg_caller call_stmt ?z in_key in
      let in_nodes = node_undef_list_to_set in_nodes in
      (in_node, in_nodes) :: acc
    in
    match in_key with
      | Signature.InCtrl | Signature.InNum _ -> default ()
      | Signature.InImpl z ->
          if Locations.Zone.intersects z real_inputs
          then default ~z:real_inputs ()
          else acc
  in
  try
    let sgn = FctIndex.sgn (PdgTypes.Pdg.get_index pdg_callee) in
    PdgIndex.Signature.fold_all_inputs test_in [] sgn
  with PdgTypes.Pdg.Top ->
    Options.warning ~source:(fst (Cil_datatype.Stmt.loc call_stmt)) ~once:true
      "skipping impact within imprecisely analyzed function %a"
      Kernel_function.pretty kf_callee;
    []


let all_call_out_nodes pdg_called pdg_caller call_stmt =
  try
    let _, call_sgn =
      FctIndex.find_call (PdgTypes.Pdg.get_index pdg_caller) call_stmt
    in
    let test_out acc (out_key, call_out_node) =
      let out_nodes, _ = !Db.Pdg.find_output_nodes pdg_called out_key in
      let out_nodes = node_undef_list_to_set out_nodes in
      (call_out_node, out_nodes) :: acc
    in
    PdgIndex.Signature.fold_all_outputs test_out [] call_sgn
  with PdgTypes.Pdg.Top ->
    Options.warning ~source:(fst (Cil_datatype.Stmt.loc call_stmt)) ~once:true
      "cannot propagate impact into imprecisely analyzed caller function %a"
      Kernel_function.pretty (Kernel_function.find_englobing_kf call_stmt);
    []

    
