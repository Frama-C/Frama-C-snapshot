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

  let equal (b1,p1) (b2,p2) = (b1 = b2) && (p1 = p2)

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

    let mark_to_prop_to_caller_input call _pdg node m =
      match m with 
        | true, M.Glob -> Some m
        | true, M.Loc ->
            call_in_to_check := (call, node, m) :: !call_in_to_check;
            None
        | _ -> assert false (* marque invisible ??? *)

    let mark_to_prop_to_called_output _call _pdg _node m = 
      match m with 
        | true, M.Glob -> Some (true, M.Loc)
        | true, M.Loc -> Some m
        | _ -> assert false (* marque invisible ??? *)

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
    | (call, sel, m) as e :: calls ->
        let _, kf_caller = Kernel_function.find_from_sid call.Cil_types.sid in
        let fm = get_marks proj kf_caller in
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
          List.iter (fun (pdg, sel) -> select_pdg_elements proj pdg sel) 
                    to_select;
          process_call_inputs proj


let add_node_to_select glob to_select z_opt node =
  PdgMarks.add_to_select to_select 
           (PdgMarks.mk_select_node ~z_opt node) (BoolMark.mk glob)

let add_nodes_and_undef_to_select glob (ctrl_nodes, (data_nodes, undef)) to_select =
  let to_select =
    List.fold_left 
      (fun s n -> add_node_to_select glob s None n) to_select ctrl_nodes
  in
  let to_select =
    List.fold_left 
      (fun s (n,z_opt) -> add_node_to_select glob s z_opt n) to_select data_nodes
  in
  let to_select = 
    PdgMarks.add_undef_in_to_select to_select undef (BoolMark.mk glob)
  in to_select

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
          if debug1 () then
            Format.printf "[sparecode] selecting annotation : %a @."
              !Ast_printer.d_code_annotation annot;
        let nodes_and_co =
          !Db.Pdg.find_code_annot_nodes pdg before stmt annot in
          to_select <- add_nodes_and_undef_to_select true nodes_and_co to_select
      with PdgIndex.NotFound -> () (* unreachable *)
    in Cil.SkipChildren
end

let select_all_outputs proj kf =
  let pdg = !Db.Pdg.get kf in
  let outputs = !Db.Outputs.get_external kf in
  if debug1 () then
    Format.printf "[sparecode] selecting output zones %a@."
      Locations.Zone.pretty outputs;
  try
  let nodes, undef = !Db.Pdg.find_location_nodes_at_end pdg outputs in
  let nodes =
    try ((!Db.Pdg.find_ret_output_node pdg),None) :: nodes
    with Db.Pdg.NotFound -> nodes
  in
  let nodes_and_co = ([], (nodes, undef)) in
  let to_select = add_nodes_and_undef_to_select false nodes_and_co [] in
    select_pdg_elements proj pdg to_select
  with PdgIndex.NotFound -> (* end is unreachable *)
    ()

let select_annotations ~select_annot ~select_slice_pragma proj =
  let visit_fun kf =
    try
      if debug1 () then
        Format.printf "[sparecode] look for annotations in function %s@."
          (Kernel_function.get_name kf);
      let filter annot = match annot.Cil_types.annot_content with
        | Cil_types.APragma (Cil_types.Slice_pragma _) -> select_slice_pragma
        | _ -> select_annot in
      let f = Kernel_function.get_definition kf in
      let pdg = !Db.Pdg.get kf in
      let visit = new annot_visitor ~filter pdg in
        ignore (Cil.visitCilFunction (visit:>Cil.cilVisitor) f);
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
  if debug1 () then
    Format.printf "[sparecode] finalize (process call inputs) @.";
  process_call_inputs proj;
  assert (!call_in_to_check = [])


let select_usefull_things ~select_annot ~select_slice_pragma kf_entry =
  let proj = ProjBoolMarks.empty in
  assert (!call_in_to_check = []);
  if debug1 () then
    Format.printf "[sparecode] selecting function %s outputs and entry point@."
      (Kernel_function.get_name kf_entry);
  select_entry_point proj kf_entry;
  select_all_outputs proj kf_entry;
  if (select_annot  or select_slice_pragma) then
    select_annotations ~select_annot ~select_slice_pragma proj;
  finalize proj;
  proj
