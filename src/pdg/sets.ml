(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** Provides function to extract information from the PDG. *)

open Cil_types

module M = Macros
module P = PdgTypes.Pdg
module PI = PdgTypes.InternalPdg
module D = PdgTypes.Dpd
module N = PdgTypes.Node
module G = PdgTypes.G
module FI = PdgIndex.FctIndex
module K = PdgIndex.Key

type t_node = PdgTypes.Node.t
type t_loc =  Locations.Zone.t
type t_pdg = PdgTypes.Pdg.t
type t_dpds_kind = PdgTypes.Dpd.td

let get_init_state pdg = 
  try State.get_init_state (PI.get_states pdg)
  with Not_found -> assert false
let get_last_state pdg = 
  try State.get_last_state (PI.get_states pdg)
  with Not_found -> raise Db.Pdg.NotFound (* no last state: strange ! *)
let get_stmt_state pdg stmt = 
  try State.get_stmt_state (PI.get_states pdg) stmt
  with Not_found -> raise Db.Pdg.NotFound (* probably an unreachable stmt *)

(** notice that there can be several nodes if the statement is a call *)
let find_stmt_nodes pdg stmt = 
  let key = K.stmt_key stmt in
    FI.find_all  (PI.get_index pdg) key

let find_stmt_node pdg stmt = 
  let key = K.stmt_key stmt in
    FI.find_info  (PI.get_index pdg) key

let find_entry_point_node pdg =
  try
    let key = K.entry_point in
      FI.find_info (PI.get_index pdg) key
  with PdgIndex.NotFound -> assert false

let find_top_input_node pdg =
  let key = K.top_input in
  FI.find_info (PI.get_index pdg) key

let find_nodes_for_stmt_id pdg sid =
  let stmt,_ = Kernel_function.find_from_sid sid in
    find_stmt_nodes pdg stmt

let find_loc_nodes pdg state loc =
  let nodes, undef = State.get_loc_nodes state loc in
  let nodes, undef =
    if not (Locations.Zone.equal undef Locations.Zone.bottom) then
      let state = get_init_state pdg in
      let init_nodes, init_undef = 
        State.get_loc_nodes state undef in
        (init_nodes @ nodes), init_undef
    else 
      nodes, undef
  in
    (*
    if not (Locations.Zone.equal undef Locations.Zone.bottom)
    then
      begin
      Cil.log "[pdg] Warning : try to find @[<h 1>%a@] in function %s \
                     that might be incompletly defined (ignored).\n"
                Locations.Zone.pretty undef 
                (Macros.pdg_name pdg);
      (* raise Not_found *)
      end;
         *)
    nodes, undef

let find_location_nodes_at_stmt pdg stmt ~before loc =
  let get_nodes state = find_loc_nodes pdg state loc in
  let get_stmt_nodes stmt = get_nodes (get_stmt_state pdg stmt) in
  let get_stmts_nodes stmts =
    let add (acc_nodes, acc_loc) stmt =
      let nodes, undef = get_stmt_nodes stmt in
        acc_nodes @ nodes, (Locations.Zone.join acc_loc undef)
    in List.fold_left add ([], Locations.Zone.bottom) stmts
  in
  let nodes, undef_zone =
    if before 
    then get_stmt_nodes stmt
    else match stmt.skind, stmt.succs with 
      | Return _, [] -> get_nodes (get_last_state pdg)
      | _, [] -> (* no successors but not a return => unreachable *)
          raise PdgIndex.NotFound
      | _, succs -> 
          get_stmts_nodes succs
  in nodes, undef_zone

let find_location_nodes_at_end pdg loc = 
  find_loc_nodes pdg (get_last_state pdg) loc

let find_label_node pdg label_stmt label =
  let key = K.label_key label_stmt label in
  FI.find_info (PI.get_index pdg) key

let find_decl_var_node pdg v =
  let key = K.decl_var_key v in
  FI.find_info (PI.get_index pdg) key

let find_output_node pdg =
  let key = K.output_key in
  FI.find_info (PI.get_index pdg) key

let find_input_node pdg numin =
  let sgn = FI.sgn (PI.get_index pdg) in
  PdgIndex.Signature.find_input sgn numin

let find_all_input_nodes pdg =
  let sgn = FI.sgn (PI.get_index pdg) in
  let add acc (_in_key, info) = info::acc in
  PdgIndex.Signature.fold_all_inputs add [] sgn

    (*
let find_all_output_nodes pdg =
  let sgn = FI.sgn (PI.get_index pdg) in
  let add acc (_key, info) = info::acc in
  PdgIndex.Signature.fold_all_outputs add [] sgn
  *)

let find_call_input_nodes pdg_caller call_stmt in_key =
  match in_key with
    | PdgIndex.Signature.InCtrl
    | PdgIndex.Signature.InNum _ ->
        let idx = PI.get_index pdg_caller in
        let _, call_sgn = FI.find_call idx call_stmt in
        let node = PdgIndex.Signature.find_in_info call_sgn in_key in
          [node], Locations.Zone.bottom
    | PdgIndex.Signature.InTop ->
        let idx = PI.get_index pdg_caller in
        let caller_sig = FI.sgn idx in
        let node = PdgIndex.Signature.find_in_top caller_sig in
        let state = get_stmt_state pdg_caller call_stmt in
        let nodes = State.get_all_nodes state in
          node::nodes, Locations.Zone.bottom
    | PdgIndex.Signature.InImpl zone ->
        let nodes, undef =
          find_location_nodes_at_stmt pdg_caller call_stmt ~before:true zone
        in nodes, undef

let find_call_ctrl_node pdg stmt = 
  let key = K.call_ctrl_key stmt in
    FI.find_info (PI.get_index pdg) key

let find_call_topin_node pdg stmt = 
  let key = K.call_topin_key stmt in
    FI.find_info (PI.get_index pdg) key

let find_call_num_input_node pdg call num_in =
  if num_in = 0 then raise (Invalid_argument "[pdg] 0 is not an input number");
  let key = K.call_input_key call num_in in
  FI.find_info (PI.get_index pdg) key

let find_call_output_node pdg call =
  let key = K.call_outret_key call in
  FI.find_info (PI.get_index pdg) key

let find_output_nodes called_pdg out_key =
  match out_key with
  | PdgIndex.Signature.OutRet -> 
      [ find_output_node called_pdg ], Locations.Zone.bottom
  | PdgIndex.Signature.OutLoc out -> find_location_nodes_at_end called_pdg out

let is_call_to_f stmt f_varinfo =
  match stmt.skind with 
  | Instr (Call (_, Lval (Var v, NoOffset), _, _)) -> 
      if v.vid = f_varinfo.vid then true else false
  | _ -> false

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
 * This parts groups the functions that build sets from the pdg.
    Made to answer user questions rather that to build slice marks,
    because efficient marking doesn't need to build this sets.
    However, it might be useful to prove that it is the same...
 *)

(** add the node in the list if it is not already in. *)
let add_node_in_list node node_list =
  let is_node_in node node_list =
    let is_node n = (N.compare node n) = 0 in
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
    let is_block = match N.elem_key node with 
      | K.SigKey (PdgIndex.Signature.In PdgIndex.Signature.InCtrl) -> true
      | K.Stmt stmt -> 
          (match stmt.skind with Block _ -> true | _ -> false)
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

let get_both_dpds pdg n = 
  P.get_all_direct_codpds pdg n @ P.get_all_direct_dpds pdg n

(** {3 Backward} build sets of the dependencies of given nodes *)

(** gives the list of nodes that the given node depends on,
    without looking at the kind of dependency. *)
let direct_dpds pdg node =  P.get_all_direct_dpds pdg node

(** gives the list of nodes that the given node depends on,
    with a given kind of dependency. *)
let direct_x_dpds dpd_type pdg node = P.get_x_direct_dpds dpd_type pdg node 

let direct_data_dpds = direct_x_dpds D.Data
let direct_ctrl_dpds = direct_x_dpds D.Ctrl
let direct_addr_dpds = direct_x_dpds D.Addr


(** accumulates in [node_list] the results of [add_node_and_dpds_or_codpds] 
    for all the [nodes] *)

(** @return a list containing the initial nodes and all their dependencies. *)
let all_rec_dpds pdg nodes = 
  custom_related_nodes (P.get_all_direct_dpds pdg) nodes

let find_nodes_all_x_dpds dpd_type pdg nodes =
  let merge_dpds node_list node =
    let node_dpds = direct_x_dpds dpd_type pdg node in
      add_nodes_and_custom_dpds (P.get_all_direct_dpds pdg) node_list node_dpds
  in 
  List.fold_left merge_dpds [] nodes

(** gives the [dpd_type] dependencies of the node,
    and recursively, all the dependencies of those nodes
    (regardless to their kind) *)
let all_x_dpds dpd_type pdg node = 
  find_nodes_all_x_dpds dpd_type pdg [node]

let find_nodes_all_dpds pdg nodes = 
  let merge_dpds node_list node =
    let node_dpds = direct_dpds pdg node in
      add_nodes_and_custom_dpds (P.get_all_direct_dpds pdg) node_list node_dpds
  in 
  List.fold_left merge_dpds [] nodes

let find_nodes_all_data_dpds = find_nodes_all_x_dpds D.Data
let find_nodes_all_ctrl_dpds = find_nodes_all_x_dpds D.Ctrl
let find_nodes_all_addr_dpds = find_nodes_all_x_dpds D.Addr

(** {3 Forward} build sets of the nodes that depend on given nodes *)

(** @return the list of nodes that directly depend on the given node *)
let direct_uses pdg node = P.get_all_direct_codpds pdg node 

let direct_x_uses dpd_type pdg node = P.get_x_direct_codpds dpd_type pdg node

let direct_data_uses = direct_x_uses D.Data
let direct_ctrl_uses = direct_x_uses D.Ctrl
let direct_addr_uses = direct_x_uses D.Addr

(** @return a list containing all the nodes that depend on the given nodes. *)
let all_uses pdg nodes = 
  let add_codpds node_list node = 
    let codpds = P.get_all_direct_codpds pdg node in
      add_nodes_and_custom_dpds (P.get_all_direct_codpds pdg) node_list codpds
  in 
  List.fold_left add_codpds [] nodes

(** {3 Others} *)

(** build a list of all the nodes that are related 'somehow' to the given
    nodes. It is a kind of transitive closure of [all_uses] U [all_dpds]. *) 
let all_related_nodes pdg nodes = custom_related_nodes (get_both_dpds pdg) nodes

(** @return the call outputs nodes [out] such that 
            [find_output_nodes pdg_called out_key] 
            intersects [called_selected_nodes].
  *)
let find_call_out_nodes_to_select pdg_called called_selected_nodes
                                  pdg_caller call_stmt  =
  if  M.debug2  () then
    Format.printf "[pdg:find_call_out_nodes_to_select] for call sid:%d@\n"
      call_stmt.sid;
  let _, call_sgn = FI.find_call (PI.get_index pdg_caller) call_stmt in
  let called_selected_nodes_set = 
    PdgTypes.NodeSet.add_list called_selected_nodes in
  let test_out acc (out_key, call_out_node) =
    let called_out_nodes, _undef = find_output_nodes pdg_called out_key in
      (* undef can be ignored in this case because it is taken into account in
       * the call part. *)
    let intersect = 
      List.exists (fun n -> PdgTypes.NodeSet.mem n called_selected_nodes_set) 
                  called_out_nodes 
    in
      if intersect then 
        begin
          if  M.debug2  () then
            Format.printf "\t+ n_%a@\n" Macros.pretty_node call_out_node;
          call_out_node::acc 
        end
      else acc
  in let nodes = PdgIndex.Signature.fold_all_outputs test_out [] call_sgn in
    nodes

let find_in_nodes_to_select_for_this_call 
      pdg_caller caller_selected_nodes call_stmt pdg_called =
  if  M.debug2  () then
    Format.printf "[pdg:find_in_nodes_to_select_for_this_call] for call sid:%d@\n"
      call_stmt.sid;
  let sgn = FI.sgn (PI.get_index pdg_called) in
  let caller_selected_nodes_set = 
    PdgTypes.NodeSet.add_list caller_selected_nodes in
  let test_in acc (in_key, in_node) =
    let caller_nodes, _undef = 
      find_call_input_nodes pdg_caller call_stmt in_key in
      (* undef can be ignored in this case because it is taken into account in
       * the call part. *)
    let intersect = 
      List.exists (fun n -> PdgTypes.NodeSet.mem n caller_selected_nodes_set) 
                  caller_nodes 
    in
      if intersect then 
        begin
          if  M.debug2  () then
            Format.printf "\t+ n_%a@\n" Macros.pretty_node in_node;
          in_node::acc 
        end
      else acc
    in PdgIndex.Signature.fold_all_inputs test_in [] sgn
