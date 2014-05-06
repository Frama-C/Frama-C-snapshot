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

(** This module deals with the action management.
  * It consiste of the definitions of the different kind of actions,
  * and the management of the action list.
  *)

(**/**)

type select = SlicingTypes.sl_mark PdgMarks.select
type n_or_d_marks = (SlicingInternals.node_or_dpds * SlicingInternals.pdg_mark) list

(**/**)

(*============================================================================*)
(** {2 Build}  *)

(** {3 How the elements will be selected} *)

(** Build a description to tell that the associated nodes have to be marked
* with the given mark, and than the same one will be propagated through
* their dependencies. (see also {!build_node_and_dpds_selection}) *)
let build_simple_node_selection ?(nd_marks=[]) mark =
  (SlicingInternals.CwNode, mark)::nd_marks

(** Only the control dependencies of the nodes will be marked *)
let build_addr_dpds_selection ?(nd_marks=[]) mark =
  (SlicingInternals.CwAddrDpds, mark)::nd_marks

(** Only the control dependencies of the nodes will be marked *)
let build_data_dpds_selection ?(nd_marks=[]) mark =
  (SlicingInternals.CwDataDpds, mark)::nd_marks

(** Only the control dependencies of the nodes will be marked *)
let build_ctrl_dpds_selection ?(nd_marks=[]) mark =
  (SlicingInternals.CwCtrlDpds, mark)::nd_marks

(** Build a description to tell how the selected PDG nodes and their
* dependencies will have to be marked
* (see {!type:SlicingTypes.Internals.node_or_dpds}).
* This description depend on the mark that has been asked for.
* First of all, whatever the mark is, the node is selected as [spare],
* so that it will be visible, and so will its dependencies. Then,
* if [is_ctrl mark] propagate a m1 control mark through the control dependencies
* and do a similar thing for [addr] and [data] *)
let build_node_and_dpds_selection ?(nd_marks=[]) mark =
  let m_spare = SlicingMarks.mk_user_spare in
  let nd_marks = build_simple_node_selection ~nd_marks:nd_marks m_spare in
  let nd_marks =
    if SlicingMarks.is_ctrl_mark mark
    then
      let m_ctrl = SlicingMarks.mk_user_mark ~ctrl:true ~data:false ~addr:false in
        build_ctrl_dpds_selection ~nd_marks:nd_marks m_ctrl
    else nd_marks
  in
  let nd_marks =
    if SlicingMarks.is_addr_mark mark
    then
      let m_addr = SlicingMarks.mk_user_mark ~ctrl:false ~data:false ~addr:true  in
        build_addr_dpds_selection ~nd_marks:nd_marks m_addr
    else nd_marks
  in
  let nd_marks =
    if SlicingMarks.is_data_mark mark
    then
      let m_data = SlicingMarks.mk_user_mark ~ctrl:false ~data:true ~addr:false  in
        build_data_dpds_selection ~nd_marks:nd_marks m_data
    else nd_marks
  in
    nd_marks

(** {3 Translations to a mapping between marks and program elements} *)

let translate_crit_to_select pdg ?(to_select=[]) list_crit =
  let translate acc (nodes, nd_mark) =
    let add_pdg_mark acc (nd, mark) =
      let add_nodes m acc nodes =
        let add m acc nodepart =
          PdgMarks.add_node_to_select acc nodepart m
        in
          List.fold_left (add m) acc nodes
      in
      let add_node_dpds dpd_mark f_dpds acc (node, _node_z_part) =
        let nodes = f_dpds node in
        add_nodes dpd_mark acc nodes
      in
      let acc = match nd with
        | SlicingInternals.CwNode -> add_nodes mark acc nodes
        | SlicingInternals.CwAddrDpds -> let f = PdgTypes.Pdg.get_x_direct_dpds PdgTypes.Dpd.Addr  pdg in
            List.fold_left (add_node_dpds mark f) acc nodes
        | SlicingInternals.CwCtrlDpds -> let f = PdgTypes.Pdg.get_x_direct_dpds PdgTypes.Dpd.Ctrl pdg in
            List.fold_left (add_node_dpds mark f) acc nodes
        | SlicingInternals.CwDataDpds -> let f = PdgTypes.Pdg.get_x_direct_dpds PdgTypes.Dpd.Data pdg in
            List.fold_left (add_node_dpds mark f) acc nodes
      in acc
    in List.fold_left add_pdg_mark acc nd_mark
  in List.fold_left translate to_select list_crit


(** {3 Function criteria} *)


(** build an action to apply the criteria to the persistent selection of the
* function. It means that it will be applied to all slices. *)
let mk_fct_crit fi crit =
  SlicingInternals.CrFct { SlicingInternals.cf_fct = SlicingInternals.FctSrc fi ; SlicingInternals.cf_info = crit }

let mk_fct_user_crit fi crit = mk_fct_crit fi (SlicingInternals.CcUserMark crit)

let mk_crit_fct_top fi m = mk_fct_user_crit fi (SlicingInternals.CuTop m)
let mk_crit_fct_user_select fi select = mk_fct_user_crit fi (SlicingInternals.CuSelect select)

let mk_crit_prop_persit_marks fi node_marks =
  mk_fct_crit fi (SlicingInternals.CcPropagate node_marks)

(** build an action to apply the criteria to the given slice. *)
let mk_ff_crit ff crit =
  SlicingInternals.CrFct { SlicingInternals.cf_fct = SlicingInternals.FctSliced ff ; SlicingInternals.cf_info = crit }

let mk_ff_user_select ff crit = mk_ff_crit ff (SlicingInternals.CcUserMark (SlicingInternals.CuSelect crit))

let mk_crit_choose_call ff call = mk_ff_crit ff (SlicingInternals.CcChooseCall call)
let mk_crit_change_call ff call f = mk_ff_crit ff (SlicingInternals.CcChangeCall (call, f))

let mk_crit_missing_inputs ff call (input_marks, more_inputs) =
  mk_ff_crit ff (SlicingInternals.CcMissingInputs (call, input_marks, more_inputs))
let mk_crit_missing_outputs ff call (output_marks, more_outputs) =
  mk_ff_crit ff (SlicingInternals.CcMissingOutputs (call, output_marks, more_outputs))
let mk_crit_examines_calls ff call_out_marks =
   mk_ff_crit ff (SlicingInternals.CcExamineCalls call_out_marks)

let mk_appli_select_calls fi = SlicingInternals.CrAppli (SlicingInternals.CaCall fi)

(** {3 Shortcut functions for previous things} *)

let mk_crit_mark_calls fi_caller to_call mark =
  let select = try
    let caller = SlicingMacros.get_fi_kf fi_caller in
    let pdg_caller =  !Db.Pdg.get caller in
    let call_stmts = !Db.Pdg.find_call_stmts ~caller to_call in
    let stmt_mark stmt =
      let stmt_ctrl_node = !Db.Pdg.find_call_ctrl_node pdg_caller stmt in
        (PdgMarks.mk_select_node stmt_ctrl_node, mark)
    in
    let select = List.map stmt_mark call_stmts in
      SlicingInternals.CuSelect select
  with PdgTypes.Pdg.Top -> SlicingInternals.CuTop mark
  in
    mk_fct_user_crit fi_caller select

let mk_crit_add_output_marks ff select =
  (*
  let pdg = SlicingMacros.get_ff_pdg ff in
  let add acc (out, m) =
    let nd_m = build_simple_node_selection m in
    let node = out in
      mk_mark_nodes pdg ~marks:acc [node] nd_m
  in let select = List.fold_left add [] output_marks in
    *)
  mk_ff_user_select ff select

    (*
let mk_crit_add_all_outputs_mark ff mark =
  let pdg = SlicingMacros.get_ff_pdg ff in
  let nodes = !Db.Pdg.find_all_outputs_nodes pdg in
  let nd_m = build_simple_node_selection mark in
  let select = mk_mark_nodes nodes nd_m in
  mk_ff_user_crit ff select
  *)

(*============================================================================*)
(** {2 Print} *)

let print_nd_and_mark f (nd, m) =
  let str = match nd with
  | SlicingInternals.CwNode -> ""
  | SlicingInternals.CwAddrDpds -> "addr->"
  | SlicingInternals.CwDataDpds -> "data->"
  | SlicingInternals.CwCtrlDpds -> "ctrl->"
  in Format.fprintf f "%s%a" str SlicingMarks.pretty_mark m

let rec print_nd_and_mark_list fmt ndm_list =
  match ndm_list with
  | [] -> ()
  | x :: ndm_list ->
      print_nd_and_mark fmt x; print_nd_and_mark_list fmt ndm_list

let print_nodes fmt nodes =
  let print n = Format.fprintf fmt "%a " (!Db.Pdg.pretty_node true) n in
    List.iter print nodes

let print_node_mark fmt n z m =
  Format.fprintf fmt "(%a ,%a)"
    (PdgTypes.Node.pretty_with_part) (n, z) SlicingMarks.pretty_mark m

let print_sel_marks_list fmt to_select =
  let print_sel (s, m) = match s with
    | PdgMarks.SelNode (n, z) -> print_node_mark fmt n z m
    | PdgMarks.SelIn l ->
        Format.fprintf fmt "(UndefIn %a:%a)"
          Locations.Zone.pretty l SlicingMarks.pretty_mark m
  in match to_select with [] -> Format.fprintf fmt "<empty>"
    | _ -> List.iter print_sel to_select

let _print_ndm fmt (nodes, ndm_list) =
  Format.fprintf fmt "(%a,%a)" print_nodes nodes
    print_nd_and_mark_list ndm_list

let print_f_crit fmt f_crit =
  match f_crit with
    | SlicingInternals.CuTop m -> Format.fprintf fmt "top(%a)" SlicingMarks.pretty_mark m
    | SlicingInternals.CuSelect to_select -> print_sel_marks_list fmt to_select

let print_crit fmt crit =
  match crit with
  | SlicingInternals.CrFct fct_crit ->
      let fct = fct_crit.SlicingInternals.cf_fct in
      let name = SlicingMacros.f_name fct in
      Format.fprintf fmt "[%s = " name;
    let _ = match fct_crit.SlicingInternals.cf_info with
      | SlicingInternals.CcUserMark info -> print_f_crit fmt info
      | SlicingInternals.CcMissingInputs (call, _input_marks, more_inputs)
        -> Format.fprintf fmt "missing_inputs for call %d (%s)"
             call.Cil_types.sid
             (if more_inputs then "more_inputs" else "marks only")
      | SlicingInternals.CcMissingOutputs (call, _output_marks, more_outputs)
        -> Format.fprintf fmt "missing_outputs for call %d (%s)"
             call.Cil_types.sid
             (if more_outputs then "more_outputs" else "marks only")
      | SlicingInternals.CcChooseCall call
        -> Format.fprintf fmt "choose_call for call %d" call.Cil_types.sid
      | SlicingInternals.CcChangeCall (call,f)
        -> let fname = match f with
          | SlicingInternals.CallSlice ff -> SlicingMacros.ff_name ff
          | SlicingInternals.CallSrc (Some fi) -> ("(src:"^( SlicingMacros.fi_name fi)^")")
          | SlicingInternals.CallSrc None -> "(src)"
        in Format.fprintf fmt "change_call for call %d -> %s"
             call.Cil_types.sid fname
      | SlicingInternals.CcPropagate nl ->
          Format.fprintf fmt "propagate %a"
            print_sel_marks_list nl
      | SlicingInternals.CcExamineCalls _ -> Format.fprintf fmt "examine_calls"
    in Format.fprintf fmt "]"
  | SlicingInternals.CrAppli (SlicingInternals.CaCall fi) ->
      let name = SlicingMacros.fi_name fi in
      Format.fprintf fmt "[Appli : calls to %s]" name
  | _ ->
    SlicingParameters.not_yet_implemented "Printing this slicing criterion "

let print_list_crit fmt list_crit =
  List.iter (print_crit fmt) list_crit

(*============================================================================*)
