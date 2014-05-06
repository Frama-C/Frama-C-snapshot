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

(** Provides function to extract information from the PDG. *)

open Cil_types
open PdgIndex

type nodes_and_undef = 
    (PdgTypes.Node.t * Locations.Zone.t option) list * Locations.Zone.t option

let get_init_state pdg =
  try Pdg_state.get_init_state (PdgTypes.Pdg.get_states pdg)
  with Not_found -> assert false

(** @raise Not_found when no last state (strange !) *)
let get_last_state pdg =
  Pdg_state.get_last_state (PdgTypes.Pdg.get_states pdg)

(** @raise Not_found for unreachable stmt *)
let get_stmt_state pdg stmt =
  Pdg_state.get_stmt_state (PdgTypes.Pdg.get_states pdg) stmt

let find_node pdg key = FctIndex.find_info (PdgTypes.Pdg.get_index pdg) key

(** notice that there can be several nodes if the statement is a call.
* For If, Switch, ... the node represent only the condition
* (see find_stmt_nodes below).
*)
let find_simple_stmt_nodes pdg stmt =
  let idx = PdgTypes.Pdg.get_index pdg in
  let key = Key.stmt_key stmt in
  (* The call below can raise Not_found if the statement is unreachable *)
  let nodes = FctIndex.find_all idx key in
    match stmt.skind with
      | Return _ -> (* also add OutRet *)
          (try
            let ret = FctIndex.find_all idx Key.output_key in
              ret @ nodes
          with Not_found -> nodes)
      | _ -> nodes

let rec add_stmt_nodes pdg nodes s =
  let s_nodes =
    try find_simple_stmt_nodes pdg s
    with Not_found -> [] (* Catch the fact that s may correspond to no node,
                            for example if [s] is dead code *)
  in
  let nodes = s_nodes @ nodes in
  let add acc stmt =
    (* Catch the fact that a sub-statement of s may be unreachable *)
    try add_stmt_nodes pdg acc stmt
    with Not_found -> acc
  in
  let add_block_stmts_nodes node_list blk =
    List.fold_left add node_list blk.bstmts
  in
  match s.skind with
  | Switch (_,blk,_,_) | Loop (_, blk, _, _, _) | Block blk ->
      Pdg_parameters.debug ~level:2
        "   select_stmt_computation on composed stmt %d@." s.sid;
      add_block_stmts_nodes nodes blk
  | UnspecifiedSequence seq ->
      Pdg_parameters.debug ~level:2
        "   select_stmt_computation on composed stmt %d@." s.sid;
      add_block_stmts_nodes nodes (Cil.block_from_unspecified_sequence seq)
  | If (_,bthen,belse,_) ->
      let nodes = add_block_stmts_nodes nodes bthen in
      add_block_stmts_nodes nodes belse
  | _ -> nodes

(** notice that there can be several nodes if the statement is a call.
* If the stmt is a composed instruction (block, etc), all the nodes of the
* enclosed statements are considered.
*)
let find_stmt_and_blocks_nodes pdg stmt =
  add_stmt_nodes pdg [] stmt

let find_stmt_node pdg stmt = find_node pdg (Key.stmt_key stmt)

let find_entry_point_node pdg =
  try find_node pdg Key.entry_point
  with Not_found -> assert false

let find_top_input_node pdg = find_node pdg Key.top_input

let find_loc_nodes pdg state loc =
  let nodes, undef = Pdg_state.get_loc_nodes state loc in
  let nodes, undef = match undef with
    | Some undef ->
      let state = get_init_state pdg in
      let init_nodes, init_undef = Pdg_state.get_loc_nodes state undef in
      let init_nodes = match loc with
        | Locations.Zone.Top(_,_) ->
            begin
              try (find_top_input_node pdg, None)::init_nodes
              with Not_found -> init_nodes
            end
        | _ -> init_nodes
      in
      let nodes = List.fold_left (fun acc n -> n::acc) nodes init_nodes in
        nodes, init_undef
    | None -> nodes, undef
  in
  nodes, undef

let find_location_nodes_at_stmt pdg stmt ~before loc =
  let get_nodes state = find_loc_nodes pdg state loc in
  let get_stmt_nodes stmt = get_nodes (get_stmt_state pdg stmt) in
  let get_stmts_nodes stmts =
    let add (acc_nodes, acc_loc) stmt =
      let nodes, undef = get_stmt_nodes stmt in
      let acc_nodes = nodes @ acc_nodes in
      let acc_loc = match acc_loc, undef with
        | _, None -> acc_loc
        | None, _ -> undef
        | Some acc_loc, Some undef -> Some (Locations.Zone.join acc_loc undef)
      in (acc_nodes, acc_loc)
    in List.fold_left add ([], None) stmts
  in
  let nodes, undef_zone =
    if before
    then get_stmt_nodes stmt
    else match stmt.skind, stmt.succs with
      | Return _, [] -> get_nodes (get_last_state pdg)
      | _, [] -> (* no successors but not a return => unreachable *)
          raise Not_found
      | _, succs ->
          get_stmts_nodes succs
  in nodes, undef_zone

let find_location_nodes_at_end pdg loc =
  find_loc_nodes pdg (get_last_state pdg) loc

(* be carreful that begin is different from init because
* init_state only contains implicit inputs
* while begin contains only formal arguments *)
let find_location_nodes_at_begin pdg loc =
  let kf =  PdgTypes.Pdg.get_kf pdg in
  let stmts =
    if !Db.Value.use_spec_instead_of_definition kf then []
    else let f = Kernel_function.get_definition kf in f.sbody.bstmts
  in
  let state = match stmts with
    | [] -> get_last_state pdg
    | stmt :: _ -> get_stmt_state pdg stmt
  in
  find_loc_nodes pdg state loc

let find_label_node pdg label_stmt label =
  find_node pdg (Key.label_key label_stmt label)

let find_decl_var_node pdg v = find_node pdg (Key.decl_var_key v)

let find_output_node pdg = find_node pdg Key.output_key

let find_input_node pdg numin =
  let sgn = FctIndex.sgn (PdgTypes.Pdg.get_index pdg) in
  PdgIndex.Signature.find_input sgn numin

let find_all_input_nodes pdg =
  let sgn = FctIndex.sgn (PdgTypes.Pdg.get_index pdg) in
  let add acc (_in_key, info) = info::acc in
  PdgIndex.Signature.fold_all_inputs add [] sgn

let find_call_input_nodes pdg_caller call_stmt in_key =
  match in_key with
  | PdgIndex.Signature.InCtrl
  | PdgIndex.Signature.InNum _ ->
    let idx = PdgTypes.Pdg.get_index pdg_caller in
    let _, call_sgn = FctIndex.find_call idx call_stmt in
    let node = PdgIndex.Signature.find_in_info call_sgn in_key in
    [ node, None ], None
  | PdgIndex.Signature.InImpl zone ->
    find_location_nodes_at_stmt pdg_caller call_stmt ~before:true zone

let find_call_ctrl_node pdg stmt =
  let key = Key.call_ctrl_key stmt in
  find_node pdg key

let find_call_num_input_node pdg call num_in =
  if num_in = 0 then Pdg_parameters.fatal "0 is not an input number" ;
  let key = Key.call_input_key call num_in in
  find_node pdg key

let find_call_output_node pdg call =
  let key = Key.call_outret_key call in
  find_node pdg key

let find_output_nodes called_pdg out_key =
  match out_key with
  | PdgIndex.Signature.OutRet -> [ find_output_node called_pdg, None ], None
  | PdgIndex.Signature.OutLoc out -> find_location_nodes_at_end called_pdg out

let find_call_stmts kf ~caller =
  match
    List.filter
      (fun (f, _) -> Kernel_function.equal f caller)
      (!Db.Value.callers kf)
  with
  | [] -> []
  | [ _, callsites ] -> assert (callsites <> []); callsites
  | _ -> assert false


(** {2 Build sets of nodes}

    This parts groups the functions that build sets from the pdg.
    Made to answer user questions rather that to build slice marks,
    because efficient marking doesn't need to build this sets.
    However, it might be useful to prove that it is the same... *)

(** add the node in the list if it is not already in. *)
let add_node_in_list node node_list =
  let is_node_in node node_list =
    let is_node n = (PdgTypes.Node.compare node n) = 0 in
    try let _ = List.find is_node node_list in true
    with Not_found -> false
  in
  if is_node_in node node_list
  then node_list, false
  else (node :: node_list), true

(** add the node to the list. It it wasn't already in the list,
* recursively call the same function on the successors or/and predecessors
* according to the flags. *)
let rec add_node_and_custom_dpds get_dpds node_list node =
  let node_list, added = add_node_in_list node node_list in
  if added
  then
    let is_block = match PdgTypes.Node.elem_key node with
      | Key.SigKey (PdgIndex.Signature.In PdgIndex.Signature.InCtrl) -> true
      | Key.Stmt stmt ->
          (match stmt.skind with
               Block _ | UnspecifiedSequence _ -> true
             | _ -> false)
      | _ -> false
    in
    if is_block
    then node_list (* blocks are not relevant to propagate information *)
    else
      List.fold_left
        (add_node_and_custom_dpds get_dpds) node_list (get_dpds node)
  else node_list

let add_nodes_and_custom_dpds get_dpds node_list nodes =
  List.fold_left (add_node_and_custom_dpds get_dpds) node_list nodes

let custom_related_nodes get_dpds nodes =
  add_nodes_and_custom_dpds get_dpds [] nodes

(** we ignore z_part for the moment. TODO ? *)
let filter_nodes l =  List.map (fun (n,_) -> n) l

(** {3 Backward} build sets of the dependencies of given nodes *)

(** gives the list of nodes that the given node depends on,
    without looking at the kind of dependency. *)
let direct_dpds pdg node = 
  filter_nodes (PdgTypes.Pdg.get_all_direct_dpds pdg node)

(** gives the list of nodes that the given node depends on,
    with a given kind of dependency. *)
let direct_x_dpds dpd_type pdg node =
  filter_nodes (PdgTypes.Pdg.get_x_direct_dpds dpd_type pdg node)

let direct_data_dpds = direct_x_dpds PdgTypes.Dpd.Data
let direct_ctrl_dpds = direct_x_dpds PdgTypes.Dpd.Ctrl
let direct_addr_dpds = direct_x_dpds PdgTypes.Dpd.Addr

(** accumulates in [node_list] the results of [add_node_and_dpds_or_codpds]
    for all the [nodes] *)

let find_nodes_all_x_dpds dpd_type pdg nodes =
  let merge_dpds node_list node =
    let node_dpds = direct_x_dpds dpd_type pdg node in
    add_nodes_and_custom_dpds (direct_dpds pdg) node_list node_dpds
  in
  List.fold_left merge_dpds [] nodes

let find_nodes_all_dpds pdg nodes =
  let merge_dpds node_list node =
    let node_dpds = direct_dpds pdg node in
    add_nodes_and_custom_dpds (direct_dpds pdg) node_list node_dpds
  in
  List.fold_left merge_dpds [] nodes

let find_nodes_all_data_dpds = find_nodes_all_x_dpds PdgTypes.Dpd.Data
let find_nodes_all_ctrl_dpds = find_nodes_all_x_dpds PdgTypes.Dpd.Ctrl
let find_nodes_all_addr_dpds = find_nodes_all_x_dpds PdgTypes.Dpd.Addr

(** {3 Forward} build sets of the nodes that depend on given nodes *)

(** @return the list of nodes that directly depend on the given node *)
let direct_uses pdg node = 
  filter_nodes (PdgTypes.Pdg.get_all_direct_codpds pdg node)

let direct_x_uses dpd_type pdg node =
  filter_nodes (PdgTypes.Pdg.get_x_direct_codpds dpd_type pdg node)

let direct_data_uses = direct_x_uses PdgTypes.Dpd.Data
let direct_ctrl_uses = direct_x_uses PdgTypes.Dpd.Ctrl
let direct_addr_uses = direct_x_uses PdgTypes.Dpd.Addr

(** @return a list containing all the nodes that depend on the given nodes. *)
let all_uses pdg nodes =
  let add_codpds node_list node =
    let codpds = PdgTypes.Pdg.get_all_direct_codpds pdg node in
    let codpds = filter_nodes codpds in
    let get n = filter_nodes (PdgTypes.Pdg.get_all_direct_codpds pdg n) in
    add_nodes_and_custom_dpds get node_list codpds
  in
  List.fold_left add_codpds [] nodes

(** {3 Others} *)
(* VP: unused function *)
(*
let node_set_of_list l =
  List.fold_left (fun acc n -> NodeSet.add n acc) NodeSet.empty l
*)

(** @return the call outputs nodes [out] such that
    [find_output_nodes pdg_called out_key]
    intersects [called_selected_nodes]. *)
let find_call_out_nodes_to_select
    pdg_called called_selected_nodes pdg_caller call_stmt
    =
  Pdg_parameters.debug ~level:2
    "[pdg:find_call_out_nodes_to_select] for call sid:%d@."
    call_stmt.sid;
  let _, call_sgn =
    FctIndex.find_call (PdgTypes.Pdg.get_index pdg_caller) call_stmt
  in
  let test_out acc (out_key, call_out_node) =
    let called_out_nodes, _undef = find_output_nodes pdg_called out_key in
    (* undef can be ignored in this case because it is taken into account in
     * the call part. *)
    let intersect =
      List.exists
        (fun (n,_z) -> PdgTypes.NodeSet.mem n called_selected_nodes)
        called_out_nodes
    in
    if intersect then begin
      Pdg_parameters.debug ~level:2
        "\t+ %a@." PdgTypes.Node.pretty call_out_node;
      call_out_node::acc
    end else
      acc
  in
  PdgIndex.Signature.fold_all_outputs test_out [] call_sgn

let find_in_nodes_to_select_for_this_call
    pdg_caller caller_selected_nodes call_stmt pdg_called =
  Pdg_parameters.debug ~level:2
    "[pdg:find_in_nodes_to_select_for_this_call] for call sid:%d@."
    call_stmt.sid;
  let sgn = FctIndex.sgn (PdgTypes.Pdg.get_index pdg_called) in
  let test_in acc (in_key, in_node) =
    let caller_nodes, _undef =
      find_call_input_nodes pdg_caller call_stmt in_key in
    (* undef can be ignored in this case because it is taken into account in
     * the call part. *)
    let intersect =
      List.exists
        (fun (n,_z) -> PdgTypes.NodeSet.mem n caller_selected_nodes)
        caller_nodes
    in
    if intersect then begin
      Pdg_parameters.debug ~level:2 "\t+ %a@." 
        PdgTypes.Node.pretty in_node;
      in_node::acc
    end else
      acc
  in
  PdgIndex.Signature.fold_all_inputs test_in [] sgn

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
