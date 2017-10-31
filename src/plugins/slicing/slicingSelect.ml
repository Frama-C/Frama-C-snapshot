(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Cil_types
open Cil_datatype

(* ---------------------------------------------------------------------- *)
(** {1 For internal use} *)

let check_call stmt is_call =
  let err = match stmt.skind with
    | Instr (Call _ | Local_init(_, ConsInit _,_)) -> not is_call
    | _ -> is_call
  in
    if err then
      let str = if is_call then "not" else "" in
      let msg = "This statement is "^str^" a call" in
        raise (Invalid_argument msg)
    else stmt

let print_select fmt db_select =
  let db_fvar, select = db_select in
    Format.fprintf fmt "In %a : %a"
      Varinfo.pretty db_fvar SlicingActions.print_f_crit select

let get_select_kf (fvar, _select) = Globals.Functions.get fvar

let check_db_select fvar db_select =
  let db_fvar, select = db_select in
  if not (Cil_datatype.Varinfo.equal db_fvar fvar) then
    begin
      SlicingParameters.debug
        "slice name = %s <> select = %a@."
        (fvar.vname) print_select db_select ;
      raise (Invalid_argument
               "This selection doesn't belong to the given function");
    end;
  fvar, select

let empty_db_select kf = (Kernel_function.get_vi kf, SlicingInternals.CuSelect [])
let top_db_select kf m = (Kernel_function.get_vi kf, SlicingInternals.CuTop m)
let check_kf_db_select kf = check_db_select (Kernel_function.get_vi kf)
let _check_fi_db_select fi = check_db_select (SlicingMacros.fi_svar fi)
let check_ff_db_select ff = check_db_select (SlicingMacros.ff_svar ff)

let bottom_msg kf =
  SlicingParameters.feedback
    "bottom PDG for function '%s': ignore selection"
    (Kernel_function.get_name kf)

let basic_add_select kf select nodes ?(undef) nd_marks =
  let fvar, sel = check_kf_db_select kf select in
  match sel with
    | SlicingInternals.CuTop _ -> select
    | SlicingInternals.CuSelect sel ->
        let pdg = !Db.Pdg.get kf in
        let nodes =
          List.map (fun n -> (n, None) (*TODO: add z_part ? *)) nodes in
        (* let nd_marks = SlicingActions.build_node_and_dpds_selection mark in *)
        (* let nd_marks = SlicingActions.build_simple_node_selection mark in *)
        let crit = [(nodes, nd_marks)] in
        let sel = SlicingActions.translate_crit_to_select pdg ~to_select:sel crit  in
        let sel = match undef with None -> sel
          | Some (undef, mark) ->
              PdgMarks.add_undef_in_to_select sel undef mark in
        let sel = SlicingInternals.CuSelect sel in
          (fvar, sel)

let select_pdg_nodes kf ?(select=empty_db_select kf) nodes mark =
  SlicingParameters.debug ~level:1 "[Register.select_pdg_nodes]" ;
  let nd_marks = SlicingActions.build_node_and_dpds_selection mark in
  try basic_add_select kf select nodes nd_marks
  with Db.Pdg.Top | Db.Pdg.Bottom ->
      assert false (* if we have node, we must have a pdg somewhere ! *)

let mk_select pdg sel nodes undef mark =
  let nd_marks = SlicingActions.build_simple_node_selection mark in
  let crit = [(nodes, nd_marks)] in
  let sel = SlicingActions.translate_crit_to_select pdg ~to_select:sel crit in
  let sel = PdgMarks.add_undef_in_to_select sel undef mark in
  let sel = SlicingInternals.CuSelect sel in
    sel

let select_stmt_zone kf ?(select=empty_db_select kf) stmt ~before loc mark =
  SlicingParameters.debug ~level:1 "[Register.select_stmt_zone] %a %s stmt %d (m=%a)"
      Locations.Zone.pretty loc
      (if before then "before" else "after") stmt.sid
       SlicingMarks.pretty_mark mark;
   if not (Db.Value.is_reachable_stmt stmt) then
    begin
      SlicingParameters.feedback
	"@[Nothing to select for @[%a@]@ %s unreachable stmt of %a@]"
        Locations.Zone.pretty loc
        (if before then "before" else "after")
	Kernel_function.pretty kf;
      select
    end
  else
 let fvar, sel = check_kf_db_select kf select in
  match sel with
    | SlicingInternals.CuTop _ -> select
    | SlicingInternals.CuSelect sel ->
        try
          let pdg = !Db.Pdg.get kf in
          let nodes, undef =
            !Db.Pdg.find_location_nodes_at_stmt pdg stmt before loc in
          let sel = mk_select pdg sel nodes undef mark in
            (fvar, sel)
        with
          | Not_found -> (* stmt probably unreachable *)
              SlicingParameters.feedback
                "@[Nothing to select for @[%a@]@ %s required stmt in %a@]"
                Locations.Zone.pretty loc
                (if before then "before" else "after")
		Kernel_function.pretty kf;
              SlicingParameters.debug
                "@[Nothing to select for @[%a@]@ %s stmt %d in %a@]"
                Locations.Zone.pretty loc
                (if before then "before" else "after") stmt.sid  
		Kernel_function.pretty kf;
              select
          | Db.Pdg.Top -> top_db_select kf mark
          | Db.Pdg.Bottom -> bottom_msg kf; select


(** this one is similar to [select_stmt_zone] with the return statement
* when the function is defined, but it can also be used for undefined functions. *)
let select_in_out_zone ~at_end ~use_undef kf select loc mark =
  SlicingParameters.debug
    "[Register.select_in_out_zone] select zone %a (m=%a) at %s of %a"
    Locations.Zone.pretty loc SlicingMarks.pretty_mark mark
    (if at_end then "end" else "begin") Kernel_function.pretty kf;
  let fvar, sel = check_kf_db_select kf select in
  match sel with
    | SlicingInternals.CuTop _ -> select
    | SlicingInternals.CuSelect sel ->
        try
          let pdg = !Db.Pdg.get kf in
          let find =
            if at_end then !Db.Pdg.find_location_nodes_at_end
            else !Db.Pdg.find_location_nodes_at_begin in
          let nodes, undef = find pdg loc in
          let undef = if use_undef then undef else None in
          let sel = mk_select pdg sel nodes undef mark in
            (fvar, sel)
        with
        | Not_found -> (* in or out unreachable ? *)
	    SlicingParameters.feedback
	      "@[Nothing to select for zone %a (m=%a) at %s of %a@]"
	      Locations.Zone.pretty loc SlicingMarks.pretty_mark mark
	      (if at_end then "end" else "begin") Kernel_function.pretty kf;
            select
        | Db.Pdg.Top -> top_db_select kf mark
        | Db.Pdg.Bottom -> bottom_msg kf; select

let select_zone_at_end kf  ?(select=empty_db_select kf) loc mark =
  select_in_out_zone ~at_end:true ~use_undef:true kf select loc mark

let select_modified_output_zone kf  ?(select=empty_db_select kf) loc mark =
  select_in_out_zone ~at_end:true ~use_undef:false kf select loc mark

let select_zone_at_entry kf  ?(select=empty_db_select kf) loc mark =
  select_in_out_zone ~at_end:false ~use_undef:true kf select loc mark

let stmt_nodes_to_select pdg stmt =
  try
    let stmt_nodes = !Db.Pdg.find_stmt_and_blocks_nodes pdg stmt in
    SlicingParameters.debug ~level:2 "[Register.stmt_nodes_to_select] results on stmt %d (%a)" stmt.sid
      (fun fmt l -> List.iter (!Db.Pdg.pretty_node true fmt) l)
      stmt_nodes;
    stmt_nodes
  with Not_found ->
    SlicingParameters.debug ~level:2 "[Register.stmt_nodes_to_select] no results for stmt %d, probably unreachable" stmt.sid;
    []

let select_stmt_computation kf ?(select=empty_db_select kf) stmt mark =
  SlicingParameters.debug ~level:1 "[Register.select_stmt_computation] on stmt %d" stmt.sid;
  if not (Db.Value.is_reachable_stmt stmt) then
    begin
      SlicingParameters.feedback
	"@[Nothing to select for an unreachable stmt of %a@]"
	Kernel_function.pretty kf;
      select
    end
  else
    try
      let pdg = !Db.Pdg.get kf in
      let stmt_nodes = stmt_nodes_to_select pdg stmt in
      let nd_marks = SlicingActions.build_node_and_dpds_selection mark in
        basic_add_select kf select stmt_nodes nd_marks
    with Db.Pdg.Top -> top_db_select kf mark
      | Db.Pdg.Bottom -> bottom_msg kf; select

let select_label kf ?(select=empty_db_select kf) label mark =
  SlicingParameters.debug ~level:1 "[Register.select_label] on label "
    (* Logic_label.pretty label *); 
    try
      let pdg = !Db.Pdg.get kf in
      let nodes =
        let add_label_nodes l acc = match l with
          | StmtLabel stmt ->
            let add acc l =
              try !Db.Pdg.find_label_node pdg !stmt l :: acc
              with Not_found -> acc
            in
            List.fold_left add acc (!stmt).labels
          | FormalLabel _ | BuiltinLabel _ -> acc
        in
        (* Logic_label.Set.fold add_label_nodes labels [] *)
        add_label_nodes label []
      in
      let nd_marks = SlicingActions.build_node_and_dpds_selection mark in
        basic_add_select kf select nodes nd_marks
    with Db.Pdg.Top -> top_db_select kf mark
      | Db.Pdg.Bottom -> bottom_msg kf; select

(** marking a call node means that a [choose_call] will have to decide that to
 * call according to the slicing-level, but anyway, the call will be visible.
 *)
let select_minimal_call kf ?(select=empty_db_select kf) stmt m =
  SlicingParameters.debug ~level:1 "[Register.select_minimal_call]";
    try
      let pdg = !Db.Pdg.get kf in
      let call = check_call stmt true in
      let call_node = !Db.Pdg.find_call_ctrl_node pdg call in
      let nd_marks = SlicingActions.build_simple_node_selection m in
        basic_add_select kf select [call_node] nd_marks
    with Db.Pdg.Top -> top_db_select kf m
      | Db.Pdg.Bottom -> bottom_msg kf; select

let select_stmt_ctrl kf ?(select=empty_db_select kf) stmt =
  SlicingParameters.debug ~level:1 "[Register.select_stmt_ctrl] of sid:%d" stmt.sid;
  let mark = SlicingMarks.mk_user_mark ~ctrl:true ~data:false ~addr:false in
  try
    let pdg = !Db.Pdg.get kf in
    let stmt_nodes = !Db.Pdg.find_simple_stmt_nodes pdg stmt in
    let nd_marks = SlicingActions.build_ctrl_dpds_selection mark in
      basic_add_select kf select stmt_nodes nd_marks
  with Db.Pdg.Top -> top_db_select kf mark
    | Db.Pdg.Bottom -> bottom_msg kf; empty_db_select kf

let select_entry_point kf ?(select=empty_db_select kf) mark =
  SlicingParameters.debug ~level:1 "[Register.select_entry_point] of %a"
      Kernel_function.pretty kf;
  try
    let pdg = !Db.Pdg.get kf in
    let node = !Db.Pdg.find_entry_point_node pdg in
    let nd_marks = SlicingActions.build_simple_node_selection mark in
      basic_add_select kf select [node] nd_marks
  with Db.Pdg.Top -> top_db_select kf mark
    | Db.Pdg.Bottom -> bottom_msg kf; empty_db_select kf

let select_return kf ?(select=empty_db_select kf) mark =
  SlicingParameters.debug ~level:1 "[Register.select_return] of %a"
      Kernel_function.pretty kf;
  try
    let pdg = !Db.Pdg.get kf in
    let node = !Db.Pdg.find_ret_output_node pdg in
    let nd_marks = SlicingActions.build_simple_node_selection mark in
      basic_add_select kf select [node] nd_marks
  with
    | Not_found -> (* unreachable ? *)
        SlicingParameters.feedback
          "@[Nothing to select for return stmt of %a@]"
	  Kernel_function.pretty kf;
        select
    | Db.Pdg.Top -> top_db_select kf mark
    | Db.Pdg.Bottom -> bottom_msg kf; empty_db_select kf

let select_decl_var kf ?(select=empty_db_select kf) vi mark =
  SlicingParameters.debug ~level:1 "[Register.select_decl_var] of %s in %a@."
    vi.Cil_types.vname Kernel_function.pretty kf;
  if vi.Cil_types.vglob (* no slicing request on globals *)
  then select
  else try
    let pdg = !Db.Pdg.get kf in
    let node = !Db.Pdg.find_decl_var_node pdg vi in
    let nd_marks = SlicingActions.build_simple_node_selection mark in
      basic_add_select kf select [node] nd_marks
  with
    | Not_found ->
        SlicingParameters.feedback
          "@[Nothing to select for %s declarationin %a@]"
	  vi.Cil_types.vname Kernel_function.pretty kf;
        select
    | Db.Pdg.Top -> top_db_select kf mark
    | Db.Pdg.Bottom -> bottom_msg kf; empty_db_select kf


let merge_select select1 select2 =
  let select = match select1, select2 with
      | SlicingInternals.CuTop m, _ | _, SlicingInternals.CuTop m -> SlicingInternals.CuTop m
      | SlicingInternals.CuSelect select1, SlicingInternals.CuSelect select2 ->
          (* TODO : we can probably do better...*)
          SlicingInternals.CuSelect (select1 @ select2)
  in select

let merge_db_select db_select1 db_select2 =
  let fvar, select1 = db_select1 in
  let _, select2 = check_db_select fvar db_select2 in
  let select = merge_select select1 select2 in
    (fvar, select)

module Selections = struct

  let add_to_selects db_select set =
    let vf, select = db_select in
    let select =
      try merge_select (Cil_datatype.Varinfo.Map.find vf set) select
      with Not_found -> select
    in
    Cil_datatype.Varinfo.Map.add vf select set

  let iter_selects_internal f set =
    Cil_datatype.Varinfo.Map.iter (fun v sel -> f (v, sel)) set

  let fold_selects_internal f acc selections  =
    let r = ref acc in
    let dof select = r := f !r select in
    iter_selects_internal dof selections; !r
end

let add_crit_ff_change_call ff_caller call f_to_call =
  let crit = SlicingActions.mk_crit_change_call ff_caller call f_to_call in
    SlicingProject.add_filter crit

(** change the call to call the given slice.
 * This is a user request, so it might be the case that
 * the new function doesn't compute enough outputs :
 * in that case, add outputs first.
 *)
let call_ff_in_caller ~caller ~to_call =
  let kf_caller = SlicingMacros.get_ff_kf caller in
  let kf_to_call = SlicingMacros.get_ff_kf to_call in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller  kf_to_call in
  let ff_to_call = SlicingInternals.CallSlice to_call in
  let add_change_call stmt =
    add_crit_ff_change_call caller stmt ff_to_call ;
    match Fct_slice.check_outputs_before_change_call caller
                             stmt to_call with
      | [] -> ()
      | [c] -> SlicingProject.add_filter c
      | _ -> assert false

  in List.iter add_change_call call_stmts

let call_fsrc_in_caller ~caller ~to_call =
  let kf_caller = SlicingMacros.get_ff_kf caller in
  let fi_to_call = SlicingMacros.get_kf_fi to_call in
  let kf_to_call = SlicingMacros.get_fi_kf fi_to_call in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller kf_to_call in
  let add_change_call stmt =
    add_crit_ff_change_call caller stmt (SlicingInternals.CallSrc (Some fi_to_call))
  in List.iter add_change_call call_stmts

let call_min_f_in_caller ~caller ~to_call =
  let kf_caller = SlicingMacros.get_ff_kf caller in
  let pdg = SlicingMacros.get_ff_pdg caller in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller to_call in
  let call_nodes =
    List.map (fun call -> (!Db.Pdg.find_call_ctrl_node pdg call),None)
      call_stmts in
  let m = SlicingMarks.mk_user_spare in
  let nd_marks = SlicingActions.build_simple_node_selection m in
  let select = SlicingActions.translate_crit_to_select pdg [(call_nodes, nd_marks)] in
    SlicingProject.add_fct_ff_filter caller (SlicingInternals.CuSelect select)

let is_already_selected ff db_select =
  let _, select = check_ff_db_select ff db_select in
    match select with
      | SlicingInternals.CuTop _ -> assert false
      | SlicingInternals.CuSelect to_select ->
          (* let pdg = !Db.Pdg.get (Globals.Functions.get fvar) in *)
          let new_marks = Fct_slice.filter_already_in ff to_select in
          let ok = if new_marks = [] then true else false in
            if ok then
              SlicingParameters.debug ~level:1
                "[Api.is_already_selected] %a ?\t--> yes"
                print_select db_select
            else SlicingParameters.debug ~level:1
              "[Api.is_already_selected] %a ?\t--> no (missing %a)"
              print_select db_select
              SlicingActions.print_sel_marks_list new_marks;
            ok

let add_ff_selection ff db_select =
  SlicingParameters.debug ~level:1 "[Api.add_ff_selection] %a to %s"
    print_select db_select (SlicingMacros.ff_name ff);
  let _, select = check_ff_db_select ff db_select in
      SlicingProject.add_fct_ff_filter ff select

(** add a persistent selection to the function.
* This might change its slicing level in order to call slices later on. *)
let add_fi_selection db_select =
  SlicingParameters.debug ~level:1 "[Api.add_fi_selection] %a"
    print_select db_select;
  let kf = get_select_kf db_select in
  let fi = SlicingMacros.get_kf_fi kf in
  let _, select = db_select in
    SlicingProject.add_fct_src_filter fi select;
    match fi.SlicingInternals.fi_level_option with
      |  SlicingInternals.DontSlice |  SlicingInternals.DontSliceButComputeMarks ->
          SlicingMacros.change_fi_slicing_level fi  SlicingInternals.MinNbSlice;
          SlicingParameters.debug ~level:1 "[Register.add_fi_selection] changing %s slicing level to %s@."
              (SlicingMacros.fi_name fi)
              (SlicingMacros.str_level_option  fi.SlicingInternals.fi_level_option)

      |  SlicingInternals.MinNbSlice |  SlicingInternals.MaxNbSlice -> ()

