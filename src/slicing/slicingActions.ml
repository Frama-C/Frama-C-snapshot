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

(** This module deals with the action management.
  * It consiste of the definitions of the different kind of actions,
  * and the management of the action list.
  *)

(**/**)

module P = PdgTypes

module T = SlicingInternals
module M = SlicingMacros
module Marks = SlicingMarks

type t_mark = SlicingMarks.t_mark
type t_select = t_mark PdgMarks.t_select
type t_n_or_d_marks = (T.t_node_or_dpds * T.t_pdg_mark) list
type t_fct_info = T.t_fct_info
type t_fct_slice = T.t_fct_slice
type t_call_id = T.t_call_id
type t_fct_crit = T.t_fct_crit
type t_criterion = T.t_criterion

(**/**)

(*============================================================================*)
(** {2 Build}  *)

(** {3 How the elements will be selected} *)

(** Build a description to tell that the associated nodes have to be marked
* with the given mark, and than the same one will be propagated through
* their dependencies. (see also {!build_node_and_dpds_selection}) *)
let build_simple_node_selection ?(nd_marks=[]) mark =
  (T.CwNode, mark)::nd_marks

(** Only the control dependencies of the nodes will be marked *)
let build_addr_dpds_selection ?(nd_marks=[]) mark =
  (T.CwAddrDpds, mark)::nd_marks

(** Only the control dependencies of the nodes will be marked *)
let build_data_dpds_selection ?(nd_marks=[]) mark =
  (T.CwDataDpds, mark)::nd_marks

(** Only the control dependencies of the nodes will be marked *)
let build_ctrl_dpds_selection ?(nd_marks=[]) mark =
  (T.CwCtrlDpds, mark)::nd_marks

(** Build a description to tell how the selected PDG nodes and their
* dependencies will have to be marked
* (see {!type:SlicingTypes.Internals.t_node_or_dpds}).
* This description depend on the mark that has been asked for.
* First of all, whatever the mark is, the node is selected as [spare],
* so that it will be visible, and so will its dependencies. Then,
* if [is_ctrl mark] propagate a m1 control mark through the control dependencies
* and do a similar thing for [addr] and [data] *)
let build_node_and_dpds_selection ?(nd_marks=[]) mark =
  let m_spare = Marks.mk_user_spare in
  let nd_marks = build_simple_node_selection ~nd_marks:nd_marks m_spare in
  let nd_marks =
    if Marks.is_ctrl_mark mark
    then
      let m_ctrl = Marks.mk_user_mark ~ctrl:true ~data:false ~addr:false in
        build_ctrl_dpds_selection ~nd_marks:nd_marks m_ctrl
    else nd_marks
  in
  let nd_marks =
    if Marks.is_addr_mark mark
    then
      let m_addr = Marks.mk_user_mark ~ctrl:false ~data:false ~addr:true  in
        build_addr_dpds_selection ~nd_marks:nd_marks m_addr
    else nd_marks
  in
  let nd_marks =
    if Marks.is_data_mark mark
    then
      let m_data = Marks.mk_user_mark ~ctrl:false ~data:true ~addr:false  in
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
        | T.CwNode -> add_nodes mark acc nodes
        | T.CwAddrDpds -> let f = P.Pdg.get_x_direct_dpds P.Dpd.Addr  pdg in
            List.fold_left (add_node_dpds mark f) acc nodes
        | T.CwCtrlDpds -> let f = P.Pdg.get_x_direct_dpds P.Dpd.Ctrl pdg in
            List.fold_left (add_node_dpds mark f) acc nodes
        | T.CwDataDpds -> let f = P.Pdg.get_x_direct_dpds P.Dpd.Data pdg in
            List.fold_left (add_node_dpds mark f) acc nodes
      in acc
    in List.fold_left add_pdg_mark acc nd_mark
  in List.fold_left translate to_select list_crit


(** {3 Function criteria} *)


(** build an action to apply the criteria to the persistent selection of the
* function. It means that it will be applied to all slices. *)
let mk_fct_crit fi crit =
  T.CrFct { T.cf_fct = T.FctSrc fi ; T.cf_info = crit }

let mk_fct_user_crit fi crit = mk_fct_crit fi (T.CcUserMark crit)

let mk_crit_fct_top fi m = mk_fct_user_crit fi (T.CuTop m)
let mk_crit_fct_user_select fi select = mk_fct_user_crit fi (T.CuSelect select)

let mk_crit_prop_persit_marks fi node_marks =
  mk_fct_crit fi (T.CcPropagate node_marks)

(** build an action to apply the criteria to the given slice. *)
let mk_ff_crit ff crit =
  T.CrFct { T.cf_fct = T.FctSliced ff ; T.cf_info = crit }

let mk_ff_user_select ff crit = mk_ff_crit ff (T.CcUserMark (T.CuSelect crit))

let mk_crit_choose_call ff call = mk_ff_crit ff (T.CcChooseCall call)
let mk_crit_change_call ff call f = mk_ff_crit ff (T.CcChangeCall (call, f))

let mk_crit_missing_inputs ff call (input_marks, more_inputs) =
  mk_ff_crit ff (T.CcMissingInputs (call, input_marks, more_inputs))
let mk_crit_missing_outputs ff call (output_marks, more_outputs) =
  mk_ff_crit ff (T.CcMissingOutputs (call, output_marks, more_outputs))
let mk_crit_examines_calls ff call_out_marks =
   mk_ff_crit ff (T.CcExamineCalls call_out_marks)

let mk_appli_select_calls fi = T.CrAppli (T.CaCall fi)

(** {3 Shortcut functions for previous things} *)

let mk_crit_mark_calls fi_caller to_call mark =
  let select = try
    let caller = M.get_fi_kf fi_caller in
    let pdg_caller =  !Db.Pdg.get caller in
    let call_stmts = !Db.Pdg.find_call_stmts ~caller to_call in
    let stmt_mark stmt =
      let stmt_ctrl_node = !Db.Pdg.find_call_ctrl_node pdg_caller stmt in
        (PdgMarks.mk_select_node stmt_ctrl_node, mark)
    in
    let select = List.map stmt_mark call_stmts in
      T.CuSelect select
  with PdgTypes.Pdg.Top -> T.CuTop mark
  in
    mk_fct_user_crit fi_caller select

let mk_crit_add_output_marks ff select =
  (*
  let pdg = M.get_ff_pdg ff in
  let add acc (out, m) =
    let nd_m = build_simple_node_selection m in
    let node = out in
      mk_mark_nodes pdg ~marks:acc [node] nd_m
  in let select = List.fold_left add [] output_marks in
    *)
  mk_ff_user_select ff select

    (*
let mk_crit_add_all_outputs_mark ff mark =
  let pdg = M.get_ff_pdg ff in
  let nodes = !Db.Pdg.find_all_outputs_nodes pdg in
  let nd_m = build_simple_node_selection mark in
  let select = mk_mark_nodes nodes nd_m in
  mk_ff_user_crit ff select
  *)

(*============================================================================*)
(** {2 Print} *)

let print_nd_and_mark f (nd, m) =
  let str = match nd with
  | T.CwNode -> ""
  | T.CwAddrDpds -> "addr->"
  | T.CwDataDpds -> "data->"
  | T.CwCtrlDpds -> "ctrl->"
  in Format.fprintf f "%s%a" str Marks.pretty_mark m

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
    (PdgTypes.Node.pretty_with_part) (n, z) Marks.pretty_mark m

let print_sel_marks_list fmt to_select =
  let print_sel (s, m) = match s with
    | PdgMarks.SelNode (n, z) -> print_node_mark fmt n z m
    | PdgMarks.SelIn l ->
        Format.fprintf fmt "(UndefIn %a:%a)"
          Locations.Zone.pretty l Marks.pretty_mark m
  in match to_select with [] -> Format.fprintf fmt "<empty>"
    | _ -> List.iter print_sel to_select

let print_ndm fmt (nodes, ndm_list) =
  Format.fprintf fmt "(%a,%a)" print_nodes nodes
    print_nd_and_mark_list ndm_list

let print_f_crit fmt f_crit =
  match f_crit with
    | T.CuTop m -> Format.fprintf fmt "top(%a)" Marks.pretty_mark m
    | T.CuSelect to_select -> print_sel_marks_list fmt to_select

let print_crit fmt crit =
  match crit with
  | T.CrFct fct_crit ->
      let fct = fct_crit.T.cf_fct in
      let name = SlicingMacros.f_name fct in
      Format.fprintf fmt "[%s = " name;
    let _ = match fct_crit.T.cf_info with
      | T.CcUserMark info -> print_f_crit fmt info
      | T.CcMissingInputs (call, _input_marks, more_inputs)
        -> Format.fprintf fmt "missing_inputs for call %d (%s)"
             call.Cil_types.sid
             (if more_inputs then "more_inputs" else "marks only")
      | T.CcMissingOutputs (call, _output_marks, more_outputs)
        -> Format.fprintf fmt "missing_outputs for call %d (%s)"
             call.Cil_types.sid
             (if more_outputs then "more_outputs" else "marks only")
      | T.CcChooseCall call
        -> Format.fprintf fmt "choose_call for call %d" call.Cil_types.sid
      | T.CcChangeCall (call,f)
        -> let fname = match f with
          | T.CallSlice ff -> SlicingMacros.ff_name ff
          | T.CallSrc (Some fi) -> ("(src:"^( SlicingMacros.fi_name fi)^")")
          | T.CallSrc None -> "(src)"
        in Format.fprintf fmt "change_call for call %d -> %s"
             call.Cil_types.sid fname
      | T.CcPropagate nl ->
          Format.fprintf fmt "propagate %a"
            print_sel_marks_list nl
      | T.CcExamineCalls _ -> Format.fprintf fmt "examine_calls"
    in Format.fprintf fmt "]"
  | T.CrAppli (T.CaCall fi) ->
      let name = SlicingMacros.fi_name fi in
      Format.fprintf fmt "[Appli : calls to %s]" name
  | _ ->
      Extlib.not_yet_implemented "Printing this slicing criterion "

let print_list_crit fmt list_crit =
  List.iter (print_crit fmt) list_crit

(*============================================================================*)
