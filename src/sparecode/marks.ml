(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

let debug1() = Cmdline.Debug.get() >= 1
let debug2() = Cmdline.Debug.get() >= 2

module BoolMark = struct
  type t = M of bool | NotUsed
  type t_call_info = unit

  let bottom = M false
  let top = M true

  let merge b1 b2 = match b1, b2 with
    | M b1, M b2 -> M (b1 || b2)
    | _ -> assert false

  let equal b1 b2 = match b1, b2 with
    | M b1, M b2 -> (b1 = b2)
    | _, _ -> assert false

  let combine old_m new_m =
    let new_m = merge old_m new_m in
    let m_to_prop = if equal old_m new_m then bottom else new_m in
      (new_m, m_to_prop)

  let is_bottom b = (b = bottom)

  let pretty fmt m =
    let txt = match m with
      | M b -> (if b then "true" else "false")
      | NotUsed -> assert false
    in Format.fprintf fmt "%s" txt
end

let call_in_to_check = ref []
let store_call_input_mark call node m =
    call_in_to_check := (call, node, m) :: !call_in_to_check

module Config = struct
  module M = struct
    include BoolMark
  end

    (** we don't propagate input marks to the calls now,
    * because some calls may be useless and we don't want to compute
    * their inputs. We will check calls later on. *)
    let mark_to_prop_to_caller_input call _pdg node m =
      store_call_input_mark call node m; None

    let mark_to_prop_to_called_output _call _pdg _node m = Some m
end

module ProjBoolMarks = Pdg.Register.F_Proj (Config)

type t_proj = ProjBoolMarks.t
type t_fct = ProjBoolMarks.t_fct

let get_marks proj kf =
  ProjBoolMarks.find_marks proj (Kernel_function.get_vi kf)

let mark_visible _fm m = match m with
  | BoolMark.M b -> b
  | BoolMark.NotUsed -> assert false

let call_visible fm call = (* the call is visible if one output is visible *)
  try
    let _, call_sgn = PdgIndex.FctIndex.find_call fm call in
    let test old_v (_, m) = old_v || (mark_visible fm m) in
    let visible = PdgIndex.Signature.fold_all_outputs test false call_sgn in
      visible
  with PdgIndex.NotFound -> false

let key_visible fm key =
  try
    match key with
      | PdgIndex.Key.CallStmt call_id ->
          let call = PdgIndex.Key.call_from_id call_id in
            call_visible fm call
      | _ -> let m = PdgIndex.FctIndex.find_info fm key in
          mark_visible fm m
  with PdgIndex.NotFound -> false

let rec all_keys_visible fm keys = match keys with
  | [] -> true
  | k :: keys -> (key_visible fm k) && (all_keys_visible fm keys)

let select_pdg_elements proj pdg to_select =
  ProjBoolMarks.mark_and_propagate proj pdg to_select

let rec add_pdg_selection to_select pdg sel_mark = match to_select with
  | [] -> [(pdg, [sel_mark])]
  | (p, ln) :: tl ->
      if Db.Pdg.from_same_fun p pdg then (p, sel_mark::ln):: tl
      else (p, ln)::(add_pdg_selection tl pdg sel_mark)

(** [proj] contains some function marks and [!call_in_to_check]
* is a list of call input marks to propagate when the call is visible.
* These marks come from the called function selection,
* but they are not automatically propagated because when a function is visible
* it doesn't mean that all the calls to that function are visible.
*
* So we first split the todo list ([!call_in_to_check]) into the nodes to mark
* which correspond to inputs of visible calls
* and the others that do not yet correspond to visible call
* but we keep them because it can happen later *)
let rec process_call_inputs proj =
  let rec process (to_select, unused) todo = match todo with
    | [] -> (to_select, unused)
    | (call, sel, m) as e :: calls ->
        let _, kf_caller = Kernel_function.find_from_sid call.Cil_types.sid in
        let fm =
          ProjBoolMarks.find_marks proj (Kernel_function.get_vi kf_caller) in
        let fm = match fm with | None -> assert false | Some fm -> fm in
        let res =
          if call_visible fm call then
            let pdg_caller = !Db.Pdg.get kf_caller in
            let to_select = add_pdg_selection to_select pdg_caller (sel, m) in
              (to_select, unused)
          else
              (to_select, e::unused)
        in process res calls
  in
  let to_select, new_list = process ([], []) !call_in_to_check in
    match to_select with
      | [] -> call_in_to_check := []
             (* nothing more to mark : finished ! we can forget [new_list] *)
      | _ ->
          call_in_to_check := new_list;
          List.iter (fun (pdg, sel) -> select_pdg_elements proj pdg sel) to_select;
          process_call_inputs proj


let add_nodes_and_undef_to_select (nodes, undef) to_select =
  let to_select =
    List.fold_left (fun acc n -> PdgMarks.add_node_to_select acc n BoolMark.top)
      to_select nodes
  in
  let to_select = PdgMarks.add_undef_in_to_select to_select undef BoolMark.top
  in to_select

(** used to visit all the annotations of a given function
 * and to find the PDG nodes to select so that the reachable annotations
 * can be visible *)
class annot_visitor pdg = object (self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ())

  val mutable to_select = []
  method get_select = to_select

  method vcode_annot annot =
    let () =
      try
        let stmt = Cilutil.valOf self#current_stmt in
        let before = self#is_annot_before in
          if debug1 () then
            Format.printf "[sparecode] selecting annotation : %a @."
              !Ast_printer.d_code_annotation annot;
        let ctrl_nodes, (data_nodes, data_in) =
          !Db.Pdg.find_code_annot_nodes pdg before stmt annot in
        let sel = (ctrl_nodes @ data_nodes, data_in) in
          to_select <- add_nodes_and_undef_to_select sel to_select
      with PdgIndex.NotFound -> () (* unreachable *)
    in Cil.SkipChildren
end

let select_all_outputs proj kf =
  let pdg = !Db.Pdg.get kf in
  let outputs = !Db.Outputs.get_external kf in
  if debug1 () then
    Format.printf "[sparecode] selecting output zones %a@."
      Locations.Zone.pretty outputs;
  let nodes, undef = !Db.Pdg.find_location_nodes_at_end pdg outputs in
  let nodes =
    try (!Db.Pdg.find_ret_output_node pdg) :: nodes
    with Db.Pdg.NotFound -> nodes
  in
  let to_select = add_nodes_and_undef_to_select (nodes, undef) [] in
    select_pdg_elements proj pdg to_select

let select_annotations proj =
  let visit_fun kf =
    try
      let f = Kernel_function.get_definition kf in
      let pdg = !Db.Pdg.get kf in
      let visit = new annot_visitor pdg in
        ignore (Cil.visitCilFunction (visit:>Cil.cilVisitor) f);
      let to_select = visit#get_select in
        select_pdg_elements proj pdg to_select
    with Kernel_function.No_Definition ->
      () (* nothing to do *)
  in (* TODO : shouldn't we iter only on the already marked functions ? *)
    Globals.Functions.iter visit_fun

let select_usefull_things kf_entry =
  let proj = ProjBoolMarks.empty in
  if debug1 () then
    Format.printf "[sparecode] selecting function %s outputs@."
      (Kernel_function.get_name kf_entry);
  select_all_outputs proj kf_entry;
  select_annotations proj;
  process_call_inputs proj;
  assert (!call_in_to_check = []);
  proj
