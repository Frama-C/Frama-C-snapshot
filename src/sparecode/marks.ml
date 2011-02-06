(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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

let debug n format = Sparecode_params.debug ~level:n format

let fatal text = Sparecode_params.fatal text

module BoolMark = struct
  type prop_mode = Glob | Loc
  type t = bool * prop_mode
  type t_call_info = unit

  let bottom = false,Loc
  let top = true,Glob

  let mk glob = if glob then (true,Glob) else (true, Loc)

  let merge (b1,p1) (b2,p2) =
    let b = b1 || b2 in
    let p = match p1, p2 with
      | Glob, _ | _, Glob -> Glob
      | Loc, Loc -> Loc
    in (b, p)

  let equal (b1,p1:t) (b2,p2) = (b1 = b2) && (p1 = p2)

  let combine old_m new_m =
    let new_m = merge old_m new_m in
    let m_to_prop = if equal old_m new_m then bottom else new_m in
      (new_m, m_to_prop)

  let is_bottom b = (b = bottom)

  let pretty fmt (b,p) =
    Format.fprintf fmt "%s(%s)"
         (if b then "true" else "false")
         (match p with Glob -> "Glob" | Loc -> "Loc")
end

(** when we first compute marks to select outputs,
* we don't immediately propagate input marks to the calls,
* because some calls may be useless and we don't want to compute
* their inputs. We will check calls later on.
* But when we select annotations, we want to preserve all the calls that can
* lead to them : so, we propagate...
* *)
let call_in_to_check = ref []

module Config = struct
  module M = struct
    include BoolMark
  end

    let mark_to_prop_to_caller_input call_opt pdg_caller sel_elem m =
      match m with
        | true, M.Glob -> Some m
        | true, M.Loc ->
            call_in_to_check := 
            (pdg_caller, call_opt, sel_elem, m) :: !call_in_to_check;
            None
        | _ -> fatal "cannot propagate invisible mark@."

    let mark_to_prop_to_called_output _call _pdg _node m =
      match m with
        | true, M.Glob -> Some (true, M.Loc)
        | true, M.Loc -> Some m
        | _ -> fatal "cannot propagate invisible mark@."

end

module ProjBoolMarks = Pdg.Register.F_Proj (Config)

type t_proj = ProjBoolMarks.t
type t_fct = ProjBoolMarks.t_fct

let get_marks proj kf =
  ProjBoolMarks.find_marks proj (Kernel_function.get_vi kf)

let mark_visible _fm (b,_) = b

let rec key_visible fm key =
  try
    match key with
      | PdgIndex.Key.CallStmt call_id ->
          let call = PdgIndex.Key.call_from_id call_id in
            call_visible fm call
      | _ -> let m = PdgIndex.FctIndex.find_info fm key in
          mark_visible fm m
  with PdgIndex.NotFound -> false
and
(** the call is visible if its control node is visible *)
    call_visible fm call =
    let key = PdgIndex.Key.call_ctrl_key call in
      key_visible fm key
    (*
  try
    let _, call_sgn = PdgIndex.FctIndex.find_call fm call in
    let test old_v (_, m) = old_v || (mark_visible fm m) in
    let visible =
    let visible = PdgIndex.Signature.fold_all_outputs test false call_sgn in
      visible
  with PdgIndex.NotFound -> false
      *)


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
    | (pdg_caller, call, sel, m) as e :: calls ->
        let kf_caller = PdgTypes.Pdg.get_kf pdg_caller in
        let fm_caller = get_marks proj kf_caller in
        let visible = match call with
          | Some call ->
              let fm = match fm_caller with 
                | None -> fatal "the caller should have marks@."
                | Some fm -> fm 
              in
                call_visible fm call
          | None -> (* let see if the function is visible or not *)
              assert (PdgTypes.Pdg.is_top pdg_caller);
              match  fm_caller with None -> false | Some _fm -> true 
        in
        let res = if visible then
            let to_select = add_pdg_selection to_select pdg_caller (sel, m) 
            in (to_select, unused)
          else (to_select, e::unused)
        in process res calls
  in
  let to_select, new_list = process ([], []) !call_in_to_check in
    match to_select with
      | [] -> call_in_to_check := []
             (* nothing more to mark : finished ! we can forget [new_list] *)
      | _ ->
          call_in_to_check := new_list;
          List.iter (fun (pdg, sel) -> select_pdg_elements proj pdg sel)
                    to_select;
          process_call_inputs proj


let add_node_to_select glob to_select z_opt node =
  PdgMarks.add_node_to_select to_select (node, z_opt) (BoolMark.mk glob)

let add_nodes_and_undef_to_select 
      glob (ctrl_nodes, decl_nodes, data_info) to_select =
  match data_info with
    | None -> to_select (* don't select anything (computation failed) *)
    | Some (data_nodes, undef) ->
        let to_select = 
          List.fold_left (fun s n -> add_node_to_select glob s None n) 
            to_select ctrl_nodes
        in
        let to_select = 
          List.fold_left (fun s n -> add_node_to_select glob s None n) 
            to_select decl_nodes
        in
        let to_select = 
          List.fold_left (fun s (n,z_opt) -> add_node_to_select glob s z_opt n)
            to_select data_nodes
        in
        let m = (BoolMark.mk glob) in
        let to_select = PdgMarks.add_undef_in_to_select to_select undef m in 
          to_select

(** used to visit all the annotations of a given function
 * and to find the PDG nodes to select so that the reachable annotations
 * can be visible *)
class annot_visitor ~filter pdg = object (self)

  inherit Visitor.generic_frama_c_visitor
    (Project.current ()) (Cil.inplace_visit ())

  val mutable to_select = []
  method get_select = to_select

  method vcode_annot annot =
    let () =
      if filter annot then
      try
        let stmt = Cilutil.valOf self#current_stmt in
        let before = self#is_annot_before in
            debug 1 "selecting annotation : %a @."
              !Ast_printer.d_code_annotation annot;
        let info = !Db.Pdg.find_code_annot_nodes pdg before stmt annot in 
          to_select <- add_nodes_and_undef_to_select true info to_select
      with PdgIndex.NotFound -> () (* unreachable *)
    in Cil.SkipChildren
end

let select_all_outputs proj kf =
  let pdg = !Db.Pdg.get kf in
  let outputs = !Db.Outputs.get_external kf in
    debug 1 "selecting output zones %a@."
      Locations.Zone.pretty outputs;
  try
  let nodes, undef = !Db.Pdg.find_location_nodes_at_end pdg outputs in
  let nodes =
    try ((!Db.Pdg.find_ret_output_node pdg),None) :: nodes
    with Db.Pdg.NotFound -> nodes
  in
  let nodes_and_co = ([], [], Some (nodes, undef)) in
  let to_select = add_nodes_and_undef_to_select false nodes_and_co [] in
    select_pdg_elements proj pdg to_select
  with PdgIndex.NotFound -> (* end is unreachable *)
    ()

let select_annotations ~select_annot ~select_slice_pragma proj =
  let visit_fun kf =
    try
        debug 1 "look for annotations in function %s@."
          (Kernel_function.get_name kf);
      let filter annot = match annot.Cil_types.annot_content with
        | Cil_types.APragma (Cil_types.Slice_pragma _) -> select_slice_pragma
        | _ -> select_annot in
      let f = Kernel_function.get_definition kf in
      let pdg = !Db.Pdg.get kf in
      let visit = new annot_visitor ~filter pdg in
        ignore (Visitor.visitFramacFunction (visit:>Visitor.frama_c_visitor) f);
      let to_select = visit#get_select in
        select_pdg_elements proj pdg to_select
    with Kernel_function.No_Definition ->
      () (* nothing to do *)
  in
    Globals.Functions.iter visit_fun

let select_entry_point proj kf =
  let pdg = !Db.Pdg.get kf in
  let ctrl = !Db.Pdg.find_entry_point_node pdg in
  let to_select = add_node_to_select true [] None ctrl in
    select_pdg_elements proj pdg to_select

let finalize proj =
    debug 1 "finalize (process call inputs) @.";
  process_call_inputs proj;
  assert (!call_in_to_check = [])


let select_usefull_things ~select_annot ~select_slice_pragma kf_entry =
  let proj = ProjBoolMarks.empty in
  assert (!call_in_to_check = []);
    debug 1 "selecting function %s outputs and entry point@."
      (Kernel_function.get_name kf_entry);
  select_entry_point proj kf_entry;
  select_all_outputs proj kf_entry;
  if (select_annot  or select_slice_pragma) then
    select_annotations ~select_annot ~select_slice_pragma proj;
  finalize proj;
  proj
