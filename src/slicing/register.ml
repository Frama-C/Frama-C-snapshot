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

open Cil_types
module FC_file = File
open Cil_datatype

let check_call stmt is_call =
  let err = match stmt.skind with
    | Instr (Call _) -> not is_call
    | _ -> is_call
  in
    if err then
      let str = if is_call then "not" else "" in
      let msg = "This statement is "^str^" a call" in
        raise (Invalid_argument msg)
    else stmt

let _pretty_list pretty fmt l = List.iter (pretty fmt) l

let print_select fmt db_select =
  let db_fvar, select = db_select in
    Format.fprintf fmt "In %a : %a"
      Varinfo.pretty_vname db_fvar SlicingActions.print_f_crit select

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
                  try (!Db.Pdg.find_label_node pdg !stmt l)::acc
                  with Not_found -> acc
                in List.fold_left add acc (!stmt).labels
            | LogicLabel (Some stmt, str) ->
                let add acc l = match l with
                  | Label (sl, _, _) when sl = str ->
                      (try (!Db.Pdg.find_label_node pdg stmt l)::acc
                       with Not_found -> acc)
                  | _ -> acc
                in List.fold_left add acc stmt.labels
            | LogicLabel (None, _) -> acc
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

end

let add_crit_ff_change_call proj ff_caller call f_to_call =
  let crit = SlicingActions.mk_crit_change_call ff_caller call f_to_call in
    SlicingProject.add_filter proj crit

(** change the call to call the given slice.
 * This is a user request, so it might be the case that
 * the new function doesn't compute enough outputs :
 * in that case, add outputs first.
 *)
let call_ff_in_caller proj ~caller ~to_call =
  let kf_caller = SlicingMacros.get_ff_kf caller in
  let kf_to_call = SlicingMacros.get_ff_kf to_call in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller  kf_to_call in
  let ff_to_call = SlicingInternals.CallSlice to_call in
  let add_change_call stmt =
    add_crit_ff_change_call proj caller stmt ff_to_call ;
    match Fct_slice.check_outputs_before_change_call proj caller
                             stmt to_call with
      | [] -> ()
      | [c] -> SlicingProject.add_filter proj c
      | _ -> assert false

  in List.iter add_change_call call_stmts

let call_fsrc_in_caller proj ~caller ~to_call =
  let kf_caller = SlicingMacros.get_ff_kf caller in
  let fi_to_call = SlicingMacros.get_kf_fi proj to_call in
  let kf_to_call = SlicingMacros.get_fi_kf fi_to_call in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller kf_to_call in
  let add_change_call stmt =
    add_crit_ff_change_call proj caller stmt (SlicingInternals.CallSrc (Some fi_to_call))
  in List.iter add_change_call call_stmts

let call_min_f_in_caller proj ~caller ~to_call =
  let kf_caller = SlicingMacros.get_ff_kf caller in
  let pdg = SlicingMacros.get_ff_pdg caller in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller to_call in
  let call_nodes =
    List.map (fun call -> (!Db.Pdg.find_call_ctrl_node pdg call),None)
      call_stmts in
  let m = SlicingMarks.mk_user_spare in
  let nd_marks = SlicingActions.build_simple_node_selection m in
  let select = SlicingActions.translate_crit_to_select pdg [(call_nodes, nd_marks)] in
    SlicingProject.add_fct_ff_filter proj caller (SlicingInternals.CuSelect select)

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
                "[Register.is_already_selected] %a ?\t--> yes"
                !Db.Slicing.Select.pretty db_select
            else SlicingParameters.debug ~level:1
              "[Register.is_already_selected] %a ?\t--> no (missing %a)"
              !Db.Slicing.Select.pretty db_select
              SlicingActions.print_sel_marks_list new_marks;
            ok

let add_ff_selection proj ff db_select =
  SlicingParameters.debug ~level:1 "[Register.add_ff_selection] %a to %s"
      !Db.Slicing.Select.pretty db_select (SlicingMacros.ff_name ff);
  let _, select = check_ff_db_select ff db_select in
      SlicingProject.add_fct_ff_filter proj ff select

(** add a persistent selection to the function.
* This might change its slicing level in order to call slices later on. *)
let add_fi_selection proj db_select =
  SlicingParameters.debug ~level:1 "[Register.add_fi_selection] %a"
                      !Db.Slicing.Select.pretty db_select;
  let kf = get_select_kf db_select in
  let fi = SlicingMacros.get_kf_fi proj kf in
  let _, select = db_select in
    SlicingProject.add_fct_src_filter proj fi select;
    match fi.SlicingInternals.fi_level_option with
      |  SlicingInternals.DontSlice |  SlicingInternals.DontSliceButComputeMarks ->
          SlicingMacros.change_fi_slicing_level fi  SlicingInternals.MinNbSlice;
          SlicingParameters.debug ~level:1 "[Register.add_fi_selection] changing %s slicing level to %s@."
              (SlicingMacros.fi_name fi)
              (SlicingMacros.str_level_option  fi.SlicingInternals.fi_level_option)

      |  SlicingInternals.MinNbSlice |  SlicingInternals.MaxNbSlice -> ()

let get_mark_from_param ff var =
  let kf = SlicingMacros.get_ff_kf ff in
  let param_list = Kernel_function.get_formals kf in
  let rec find n var_list = match var_list with
  | [] -> raise Not_found
  | v :: var_list -> if Cil_datatype.Varinfo.equal v var then n
                     else find (n+1) var_list
  in let n = find 1 param_list in
  Fct_slice.get_param_mark ff n

let get_called_slice ff stmt =
  match stmt.skind with
  | Instr (Call _) -> fst (Fct_slice.get_called_slice ff stmt)
  | _ -> None

let get_called_funcs ff stmt = match stmt.skind with
  | Instr (Call (_,expr_f,_,_)) ->
    if snd (Fct_slice.get_called_slice ff stmt) then
      Kernel_function.Hptset.elements
        (snd (!Db.Value.expr_to_kernel_function
                 (Kstmt stmt)
                 ~with_alarms:CilE.warn_none_mode
                 ~deps:None
                 expr_f))
    else
      []
  | _ -> []


let _db_pretty fmt (_project, kf) =
    try !Db.Pdg.pretty fmt (!Db.Pdg.get kf)
    with Not_found -> ()

let create_slice s =
  SlicingParameters.debug ~level:1 "[Register.create_slice]";
  SlicingProject.create_slice s

let copy_slice _proj ff =
  SlicingParameters.debug ~level:1 "[Register.copy_slice]";
  Fct_slice.copy_slice ff

let split_slice s =
  SlicingParameters.debug ~level:1 "[Register.split_slice]";
  SlicingProject.split_slice s

let merge_slices proj ff_1 ff_2 ~replace =
  SlicingParameters.debug ~level:1 "[Register.merge_slices]";
  SlicingProject.merge_slices proj ff_1 ff_2 replace

let remove_slice s =
  SlicingParameters.debug ~level:1 "[Register.remove_slice]";
  SlicingProject.remove_ff s

let is_request_empty p =
  SlicingParameters.debug ~level:1 "[Register.is_request_empty]";
  SlicingProject.is_request_empty p

let apply_next_action s =
  SlicingParameters.debug ~level:1 "[Register.apply_next_action]";
  SlicingProject.apply_next_action s

let apply_all_actions p =
  SlicingParameters.debug ~level:1 "[Register.apply_all_actions]";
  SlicingParameters.feedback ~level:1 "applying all slicing requests...";
  SlicingParameters.debug ~level:2 "pending requests:@\n %a@\n"
    !Db.Slicing.Request.pretty p ;
  let r = SlicingProject.apply_all_actions p in
    SlicingParameters.feedback ~level:2 "done (applying all slicing requests).";
    r

let print_extracted_project ?fmt ~extracted_prj =
  if SlicingParameters.Print.get () then
    FC_file.pretty_ast ?fmt ~prj:extracted_prj ()

(** Global data managment *)

module P =
  State_builder.Ref
    (Datatype.Pair
       (Datatype.List(SlicingTypes.Sl_project))
       (Datatype.Option(SlicingTypes.Sl_project)))
    (struct
       let name = "Slicing.Project"
       let dependencies = [] (* others delayed below *)
       let default () = [], None
     end)

let get_all () = fst (P.get ())
let get_project () = snd (P.get ())
let set_project proj_opt = P.set (get_all (),  proj_opt)

let from_unique_name name =
  let all = get_all () in
  try List.find (fun p -> name = SlicingProject.get_name p) all
  with Not_found -> raise Db.Slicing.No_Project

let mk_project name =
(*  try
    let _ = from_unique_name name in
    raise Db.Slicing.Existing_Project
  with Db.Slicing.No_Project -> *)
    !Db.Value.compute () ;
    let project = (SlicingProject.mk_project name) in
    let all,current = P.get () in
    P.set ((project :: all), current);
    project

let from_num_id proj kf num =
  List.find
    (fun f -> num = !Db.Slicing.Slice.get_num_id f)
    (!Db.Slicing.Slice.get_all proj kf)

(** {2 For the journalization of the slicing plug-in } *)

(** {3 For the journalization of the Db.Slicing.Project.functions} *)
let dot_project = PrintSlice.build_dot_project
let dot_project =
  Journal.register
    "Slicing.Project.print_dot"
    (Datatype.func3
       ~label1:("filename", None) Datatype.string
       ~label2:("title", None) Datatype.string
       Db.Slicing.Project.dyn_t
       Datatype.unit)
   dot_project
let dot_project ~filename ~title project =
  dot_project filename title project

let extract f_slice_names = SlicingTransform.extract ~f_slice_names
let extract =
  Journal.register
    "!Db.Slicing.Project.extract"
    (Datatype.func3
       ~label1:("f_slice_names",
               Some (fun () -> !Db.Slicing.Project.default_slice_names))
       (Datatype.func3
          Kernel_function.ty Datatype.bool Datatype.int Datatype.string)
       Datatype.string
       Db.Slicing.Project.dyn_t
       Project.ty)
    extract
let extract new_proj_name
    ?(f_slice_names=(!Db.Slicing.Project.default_slice_names)) =
  extract f_slice_names new_proj_name

let default_slice_names = SlicingTransform.default_slice_names
let () =
  Journal.Binding.add
    (Datatype.func3
       Kernel_function.ty Datatype.bool Datatype.int Datatype.string)
    default_slice_names
    "!Db.Slicing.Project.default_slice_names"

(** {3 For the journalization of the Db.Slicing.Select.functions} *)

let higher_select_stmt set spare = SlicingCmds.select_stmt set ~spare
let higher_select_stmt =
  Journal.register
    "!Db.Slicing.Select.select_stmt"
    (Datatype.func4
       Db.Slicing.Select.dyn_set
       ~label2:("spare", None) Datatype.bool
       Stmt.ty
       Kernel_function.ty
       Db.Slicing.Select.dyn_set)
    higher_select_stmt
let higher_select_stmt set ~spare =
  higher_select_stmt set spare

let higher_select_stmt_ctrl set spare = SlicingCmds.select_stmt_ctrl set ~spare
let higher_select_stmt_ctrl =
  Journal.register
    "!Db.Slicing.Select.select_stmt_ctrl"
    (Datatype.func4
       Db.Slicing.Select.dyn_set
       ~label2:("spare", None) Datatype.bool
       Stmt.ty
       Kernel_function.ty
       Db.Slicing.Select.dyn_set)
    higher_select_stmt_ctrl
let higher_select_stmt_ctrl set ~spare =
  higher_select_stmt_ctrl set spare

let higher_select_stmt_lval_rw set mark rd wr stmt scope eval =
  SlicingCmds.select_stmt_lval_rw set mark ~rd ~wr stmt ~scope ~eval
let higher_select_stmt_lval_rw =
  Journal.register
    "!Db.Slicing.Select.select_stmt_lval_rw"
    (Datatype.func4
       Db.Slicing.Select.dyn_set
       Db.Slicing.Mark.dyn_t
       ~label3:("rd", None) Datatype.String.Set.ty
       ~label4:("wr", None) Datatype.String.Set.ty
       (Datatype.func4
          Stmt.ty
          ~label2:("scope", None) Stmt.ty
          ~label3:("eval", None) Stmt.ty
          Kernel_function.ty
          Db.Slicing.Select.dyn_set))
    higher_select_stmt_lval_rw
let higher_select_stmt_lval_rw set mark ~rd ~wr stmt ~scope ~eval =
  higher_select_stmt_lval_rw set mark rd wr stmt scope eval

let higher_select_stmt_lval set mark lval before stmt scope eval =
  SlicingCmds.select_stmt_lval set mark lval ~before stmt ~scope ~eval
let higher_select_stmt_lval =
  Journal.register
    "!Db.Slicing.Select.select_stmt_lval"
    (Datatype.func4
       Db.Slicing.Select.dyn_set
       Db.Slicing.Mark.dyn_t
       Datatype.String.Set.ty
       ~label4:("before", None) Datatype.bool
       (Datatype.func4
          Stmt.ty
          ~label2:("scope", None) Stmt.ty
          ~label3:("eval", None) Stmt.ty
          Kernel_function.ty
          Db.Slicing.Select.dyn_set))
    higher_select_stmt_lval
let higher_select_stmt_lval set mark lval ~before stmt ~scope ~eval =
  higher_select_stmt_lval set mark lval before stmt scope eval

let higher_select_stmt_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var =
  SlicingCmds.select_stmt_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var
let higher_select_stmt_annots =
  Journal.register
    "!Db.Slicing.Select.select_stmt_annots"
    (Datatype.func4
       Db.Slicing.Select.dyn_set
       Db.Slicing.Mark.dyn_t
       ~label3:("spare", None) Datatype.bool
       ~label4:("threat", None) Datatype.bool
       (Datatype.func4
          ~label1:("user_assert", None) Datatype.bool
          ~label2:("slicing_pragma", None) Datatype.bool
          ~label3:("loop_inv", None) Datatype.bool
          ~label4:("loop_var", None) Datatype.bool
          (Datatype.func2
             Stmt.ty
             Kernel_function.ty
             Db.Slicing.Select.dyn_set)))
    higher_select_stmt_annots
let higher_select_stmt_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var =
    higher_select_stmt_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var

let higher_select_func_lval_rw set mark rd wr scope eval =
  SlicingCmds.select_func_lval_rw set mark ~rd ~wr ~scope ~eval
let higher_select_func_lval_rw =
  Journal.register
    "!Db.Slicing.Select.select_func_lval_rw"
    (Datatype.func4
       Db.Slicing.Select.dyn_set
       Db.Slicing.Mark.dyn_t
       ~label3:("rd", None) Datatype.String.Set.ty
       ~label4:("wr", None) Datatype.String.Set.ty
       (Datatype.func3
          ~label1:("scope", None) Stmt.ty
          ~label2:("eval", None) Stmt.ty
          Kernel_function.ty
          Db.Slicing.Select.dyn_set))
    higher_select_func_lval_rw
let higher_select_func_lval_rw set mark ~rd ~wr ~scope ~eval =
  higher_select_func_lval_rw set mark rd wr scope eval

let higher_select_func_return set spare =
    SlicingCmds.select_func_return set ~spare
let higher_select_func_return =
  Journal.register
    "!Db.Slicing.Select.select_func_return"
    (Datatype.func3
       Db.Slicing.Select.dyn_set
       ~label2:("spare", None) Datatype.bool
       Kernel_function.ty
       Db.Slicing.Select.dyn_set)
    higher_select_func_return
let higher_select_func_return set ~spare = higher_select_func_return set spare

let higher_select_func_calls_to set spare =
  SlicingCmds.select_func_calls_to set ~spare
let higher_select_func_calls_to =
  Journal.register
    "!Db.Slicing.Select.select_func_calls_to"
    (Datatype.func3
       Db.Slicing.Select.dyn_set
       ~label2:("spare", None) Datatype.bool
       Kernel_function.ty
       Db.Slicing.Select.dyn_set)
    higher_select_func_calls_to
let higher_select_func_calls_to set ~spare =
    higher_select_func_calls_to set spare

let higher_select_func_calls_into set spare =
  SlicingCmds.select_func_calls_into set ~spare
let higher_select_func_calls_into =
  Journal.register
    "!Db.Slicing.Select.select_func_calls_into"
    (Datatype.func3
       Db.Slicing.Select.dyn_set
       ~label2:("spare", None) Datatype.bool
       Kernel_function.ty
       Db.Slicing.Select.dyn_set)
    higher_select_func_calls_into
let higher_select_func_calls_into set ~spare =
  higher_select_func_calls_into set spare

let higher_select_func_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var =
  SlicingCmds.select_func_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var
let higher_select_func_annots =
  Journal.register
    "!Db.Slicing.Select.select_func_annots"
    (Datatype.func4
       Db.Slicing.Select.dyn_set
       Db.Slicing.Mark.dyn_t
       ~label3:("spare", None) Datatype.bool
       ~label4:("threat", None) Datatype.bool
       (Datatype.func4
          ~label1:("user_assert", None) Datatype.bool
          ~label2:("slicing_pragma", None) Datatype.bool
          ~label3:("loop_inv", None) Datatype.bool
          ~label4:("loop_var", None) Datatype.bool
          (Datatype.func Kernel_function.ty Db.Slicing.Select.dyn_set)))
    higher_select_func_annots
let higher_select_func_annots set mark ~spare ~threat ~user_assert ~slicing_pragma ~loop_inv ~loop_var =
  higher_select_func_annots set mark spare threat user_assert slicing_pragma loop_inv loop_var

(** {3 For the journalization of the Db.Slicing.Request.functions} *)
let apply_all project propagate_to_callers =
  SlicingCmds.apply_all project ~propagate_to_callers
let apply_all =
  Journal.register
    "!Db.Slicing.Request.apply_all"
    (Datatype.func2
       Db.Slicing.Project.dyn_t
       ~label2:("propagate_to_callers", None) Datatype.bool
       Datatype.unit)
    apply_all
let apply_all project ~propagate_to_callers =
  apply_all project propagate_to_callers

let merge_slices proj ff_1 ff_2 replace =
  merge_slices proj ff_1 ff_2 ~replace
let merge_slices =
  Journal.register
    "!Db.Slicing.Request.merge_slices"
    (Datatype.func4
       Db.Slicing.Project.dyn_t
       Db.Slicing.Slice.dyn_t
       Db.Slicing.Slice.dyn_t
       ~label4:("replace", None) Datatype.bool
       Db.Slicing.Slice.dyn_t)
    merge_slices
let merge_slices proj ff_1 ff_2 ~replace =
  merge_slices proj ff_1 ff_2 replace

let call_ff_in_caller proj caller to_call =
  call_ff_in_caller proj ~caller ~to_call
let call_ff_in_caller =
  Journal.register
    "!Db.Slicing.Request.add_call_slice"
    (Datatype.func3
       Db.Slicing.Project.dyn_t
       ~label2:("caller", None) Db.Slicing.Slice.dyn_t
       ~label3:("to_call", None) Db.Slicing.Slice.dyn_t
       Datatype.unit)
    call_ff_in_caller
let call_ff_in_caller proj ~caller ~to_call =
  call_ff_in_caller proj caller to_call

let call_fsrc_in_caller proj caller to_call =
  call_fsrc_in_caller proj ~caller ~to_call
let call_fsrc_in_caller =
  Journal.register
    "!Db.Slicing.Request.add_call_fun"
    (Datatype.func3
       Db.Slicing.Project.dyn_t
       ~label2:("caller", None) Db.Slicing.Slice.dyn_t
       ~label3:("to_call", None) Kernel_function.ty
       Datatype.unit)
    call_fsrc_in_caller
let call_fsrc_in_caller proj ~caller ~to_call =
  call_fsrc_in_caller proj caller to_call

let call_min_f_in_caller proj caller to_call =
  call_min_f_in_caller proj ~caller ~to_call
let call_min_f_in_caller =
  Journal.register
    "!Db.Slicing.Request.add_call_min_fun"
    (Datatype.func3
       Db.Slicing.Project.dyn_t
       ~label2:("caller", None) Db.Slicing.Slice.dyn_t
       ~label3:("to_call", None) Kernel_function.ty
       Datatype.unit)
    call_min_f_in_caller
let call_min_f_in_caller proj ~caller ~to_call =
  call_min_f_in_caller proj caller to_call


(** {3 For the journalization of the Db.Slicingfunctions} *)

let set_modes calls callers sliceUndef keepAnnotations print () =
  SlicingParameters.Mode.Calls.set calls ;
  SlicingParameters.Mode.Callers.set callers ;
  SlicingParameters.Mode.SliceUndef.set sliceUndef;
  SlicingParameters.Mode.KeepAnnotations.set keepAnnotations;
  SlicingParameters.Print.set print

let set_modes =
  Journal.register
    "!Db.Slicing.set_modes"
    (Datatype.func4
       ~label1:("calls", None) Datatype.int
       ~label2:("callers", None) Datatype.bool
       ~label3:("sliceUndef", None) Datatype.bool
       ~label4:("keepAnnotation", None) Datatype.bool
       (Datatype.func2
          ~label1:("print", None) Datatype.bool
          Datatype.unit
          Datatype.unit))
    set_modes
let set_modes ?(calls=SlicingParameters.Mode.Calls.get ())
    ?(callers=SlicingParameters.Mode.Callers.get ())
    ?(sliceUndef=SlicingParameters.Mode.SliceUndef.get ())
    ?(keepAnnotations=SlicingParameters.Mode.KeepAnnotations.get ())
    ?(print=SlicingParameters.Print.get ())
    () =
  set_modes calls callers sliceUndef keepAnnotations print ()

(** {2 Initialisation of the slicing plug-in} *)

let () =
  Cmdline.run_after_extended_stage
    (fun () ->
       State_dependency_graph.add_codependencies
         ~onto:P.self
         [ !Db.Pdg.self; !Db.Inputs.self_external; !Db.Outputs.self_external ])

(** {3 Register external functions into Db.Slicing}  *)
let () =
  Db.Slicing.self := P.self;
  Db.Slicing.set_modes := set_modes  (* Journalized *)

(** {3 Register external functions into Db.Slicing.Project}  *)
let () =
  Db.Slicing.Project.print_dot := dot_project; (* Journalized *)
  Db.Slicing.Project.extract := extract ; (* Journalized *)
  Db.Slicing.Project.default_slice_names := default_slice_names ; (* Journalized *)
  Db.register
    (Db.Journalize
       ("Slicing.Project.mk_project",
        Datatype.func Datatype.string Db.Slicing.Project.dyn_t))
    Db.Slicing.Project.mk_project
    mk_project;
  Db.register
    (Db.Journalize
       ("Slicing.Project.set_project",
        Datatype.func (Datatype.option Db.Slicing.Project.dyn_t) Datatype.unit))
    Db.Slicing.Project.set_project
    set_project;
  Db.register
    (Db.Journalize
       ("Slicing.Project.change_slicing_level",
        Datatype.func3
          Db.Slicing.Project.dyn_t
          Kernel_function.ty
          Datatype.int
          Datatype.unit))
    Db.Slicing.Project.change_slicing_level
    SlicingMacros.change_slicing_level ;

  (* No needs of Journalization for others Db.Slicing.Project.functions *)
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.print_extracted_project print_extracted_project;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.from_unique_name from_unique_name;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.get_all get_all;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.get_project get_project;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.get_name SlicingProject.get_name;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.pretty SlicingProject.print_project_and_worklist ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.is_directly_called_internal  SlicingMacros.is_src_fun_called ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.is_called SlicingTransform.is_src_fun_called ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Project.has_persistent_selection SlicingMacros.has_persistent_selection

(** {3 Register external functions into Db.Slicing.Select}  *)
let () =
  (* No needs of Journalization for low-level Db.Slicing.Select.functions.
   * [Note:] They can be Journalized. In that case, functions computing [Db.Slicing.Select.t]
   *         values have to be Journalized
   *)
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.add_to_selects_internal")
    Db.Slicing.Select.add_to_selects_internal Selections.add_to_selects;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_stmt_internal")
    Db.Slicing.Select.select_stmt_internal select_stmt_computation;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_label_internal")
    Db.Slicing.Select.select_label_internal select_label;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_stmt_ctrl_internal")
    Db.Slicing.Select.select_stmt_ctrl_internal select_stmt_ctrl ; (* TODO? Journalized *)
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_entry_point_internal")
    Db.Slicing.Select.select_entry_point_internal select_entry_point;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_return_internal")
    Db.Slicing.Select.select_return_internal select_return;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_decl_var_internal")
    Db.Slicing.Select.select_decl_var_internal select_decl_var;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_min_call_internal")
    Db.Slicing.Select.select_min_call_internal select_minimal_call;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_merge_internal")
    Db.Slicing.Select.merge_internal merge_db_select;

  (* No needs of Journalization for low-level Db.Slicing.Select.functions.
   * [Note:] They can be Journalized. In that case, functions computing [Db.Slicing.Select.t]
   *         [Pdg.node] values have to be Journalized
   *)
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_pdg_nodes_internal")
    Db.Slicing.Select.select_pdg_nodes_internal select_pdg_nodes;

  (* No needs of Journalization for low-level Db.Slicing.Select.functions.
   * [Note:] They can be Journalized. In that case, functions computing [Db.Slicing.Select.t]
   *         [Location.Zone.t] values have to be Journalized
   *)
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_stmt_zone_internal")
    Db.Slicing.Select.select_stmt_zone_internal select_stmt_zone;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_zone_at_entry_internal")
    Db.Slicing.Select.select_zone_at_entry_point_internal select_zone_at_entry;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_zone_at_end_internal")
    Db.Slicing.Select.select_zone_at_end_internal select_zone_at_end;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_modified_output_zone_internal")
    Db.Slicing.Select.select_modified_output_zone_internal select_modified_output_zone;

  (* No needs of Journalization for intermediate-level Db.Slicing.Select.functions *)
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_stmt_term")
    (* [Note] Can be Journalized -> Functions computing [term] values have to be Journalized *)
    Db.Slicing.Select.select_stmt_term SlicingCmds.select_stmt_term ;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_stmt_pred")
    (* [Note] Can be Journalized -> Functions computing [predicate named] values have to be Journalized *)
    Db.Slicing.Select.select_stmt_pred SlicingCmds.select_stmt_pred ;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_stmt_annot")
    (* [Note] Can be Journalized -> Functions computing [code_annotation] values have to be Journalized *)
    Db.Slicing.Select.select_stmt_annot SlicingCmds.select_stmt_annot ;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_pdg_nodes")
    (* [Note] Can be Journalized -> Functions computing [Pdg.node] values have to be Journalized *)
    Db.Slicing.Select.select_pdg_nodes SlicingCmds.select_pdg_nodes ;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_stmt_zone")
    (* [Note] Can be Journalized -> Functions computing [Locations.Zone.t] values have to be Journalized *)
    Db.Slicing.Select.select_stmt_zone SlicingCmds.select_stmt_zone ;
  Db.register (Db.Journalization_must_not_happen "Slicing.Select.select_func_zone")
    (* [Note] Can be Journalized -> Functions computing [Locations.Zone.t] values have to be Journalized *)
    Db.Slicing.Select.select_func_zone SlicingCmds.select_func_zone ;

  (* higher level function from slicingCmds *)
  Db.Slicing.Select.select_stmt := higher_select_stmt ; (* Journalized *)
  Db.Slicing.Select.select_stmt_ctrl := higher_select_stmt_ctrl ; (* Journalized *)
  Db.Slicing.Select.select_stmt_lval_rw := higher_select_stmt_lval_rw ; (* Journalized *)
  Db.Slicing.Select.select_stmt_lval := higher_select_stmt_lval ; (* Journalized *)
  Db.Slicing.Select.select_stmt_annots := higher_select_stmt_annots ; (* Journalized *)
  Db.Slicing.Select.select_func_lval_rw := higher_select_func_lval_rw ; (* Journalized *)
  Db.register
    (Db.Journalize
       ("Slicing.Select.select_func_lval",
        Datatype.func4
          Db.Slicing.Select.dyn_set
          Db.Slicing.Mark.dyn_t
          Datatype.String.Set.ty
          Kernel_function.ty
          Db.Slicing.Select.dyn_set))
    Db.Slicing.Select.select_func_lval
    SlicingCmds.select_func_lval ;
  Db.Slicing.Select.select_func_return := higher_select_func_return ; (* Journalized *)
  Db.Slicing.Select.select_func_calls_to := higher_select_func_calls_to ; (* Journalized *)
  Db.Slicing.Select.select_func_calls_into := higher_select_func_calls_into ; (* Journalized *)
  Db.Slicing.Select.select_func_annots := higher_select_func_annots ; (* Journalized *)

  (* No needs of Journalization for others Db.Slicing.Select.functions *)
  Db.register
    Db.Journalization_not_required
    Db.Slicing.Select.iter_selects_internal
    Selections.iter_selects_internal ;
  Db.register
    Db.Journalization_not_required
    Db.Slicing.Select.get_function
    get_select_kf;
  Db.register
    Db.Journalization_not_required
    Db.Slicing.Select.pretty
    print_select

(** {3 Register external functions into Db.Slicing.Slice}  *)
let () =
  Db.register (Db.Journalize
       ("Slicing.Slice.create",
        Datatype.func2
          Db.Slicing.Project.dyn_t
          Kernel_function.ty
          Db.Slicing.Slice.dyn_t))
    Db.Slicing.Slice.create
    create_slice ;
  Db.register
    (Db.Journalize
       ("Slicing.Slice.remove",
        Datatype.func2 Db.Slicing.Project.dyn_t Db.Slicing.Slice.dyn_t Datatype.unit))
    Db.Slicing.Slice.remove
    remove_slice ;

  (* higher level function from slicingCmds *)
  Db.register
    (Db.Journalize
       ("Slicing.Slice.remove_uncalled",
        Datatype.func Db.Slicing.Project.dyn_t Datatype.unit))
    Db.Slicing.Slice.remove_uncalled
    SlicingProject.remove_uncalled_slices ;

  (* No needs of Journalization for others Db.Slicing.Slice.functions *)
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_all SlicingProject.get_slices ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_callers SlicingProject.get_slice_callers ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_called_slice get_called_slice ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_called_funcs get_called_funcs ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.pretty SlicingProject.pretty_slice ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_mark_from_stmt Fct_slice.get_stmt_mark;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_mark_from_label Fct_slice.get_label_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_mark_from_formal get_mark_from_param ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_mark_from_local_var Fct_slice.get_local_var_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_user_mark_from_inputs Fct_slice.merge_inputs_m1_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_callers SlicingProject.get_slice_callers ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_function SlicingMacros.get_ff_kf;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.get_num_id SlicingMacros.get_ff_id;
  Db.register Db.Journalization_not_required
    Db.Slicing.Slice.from_num_id from_num_id

(** {3 Register external functions into Db.Slicing.Request}  *)
let () =
  (* intermediate-level Db.Slicing.Request.functions *)
  Db.register (Db.Journalization_must_not_happen "Db.Slicing.Request.add_slice_selection_internal")
    (* [Note] Can be Journalized -> Functions computing [Db.Slicing.Select.t] values have to be Journalized *)
    Db.Slicing.Request.add_slice_selection_internal add_ff_selection ;
  Db.register (Db.Journalization_must_not_happen "Db.Slicing.Request.add_selection_internal")
    (* [Note] Can be Journalized -> Functions computing [Db.Slicing.Select.t] values have to be Journalized *)
    Db.Slicing.Request.add_selection_internal add_fi_selection ;
  (* higher level Db.Slicing.Request.functions *)
  Db.register
    (Db.Journalize
       ("Slicing.Request.propagate_user_marks",
        Datatype.func Db.Slicing.Project.dyn_t Datatype.unit))
    Db.Slicing.Request.propagate_user_marks
    SlicingCmds.topologic_propagation ;
  Db.register
    (Db.Journalize
       ("Slicing.Request.add_selection",
        Datatype.func2
          Db.Slicing.Project.dyn_t Db.Slicing.Select.dyn_set Datatype.unit))
    Db.Slicing.Request.add_selection
    SlicingCmds.add_selection ;
  Db.register
    (Db.Journalize
       ("Slicing.Request.add_persistent_selection",
        Datatype.func2
          Db.Slicing.Project.dyn_t Db.Slicing.Select.dyn_set Datatype.unit))
    Db.Slicing.Request.add_persistent_selection
    SlicingCmds.add_persistent_selection ;
  Db.register
    (Db.Journalize
       ("Slicing.Request.add_persistent_cmdline",
        Datatype.func Db.Slicing.Project.dyn_t Datatype.unit))
    Db.Slicing.Request.add_persistent_cmdline
    SlicingCmds.add_persistent_cmdline ;
  Db.Slicing.Request.add_call_slice := call_ff_in_caller ; (* Journalized *)
  Db.Slicing.Request.add_call_fun := call_fsrc_in_caller ; (* Journalized *)
  Db.Slicing.Request.add_call_min_fun := call_min_f_in_caller ; (* Journalized *)
  Db.Slicing.Request.merge_slices := merge_slices ; (* Journalized *)
  Db.register
    (Db.Journalize
       ("Slicing.Request.copy_slice",
        Datatype.func2
          Db.Slicing.Project.dyn_t
          Db.Slicing.Slice.dyn_t
          Db.Slicing.Slice.dyn_t))
    Db.Slicing.Request.copy_slice
    copy_slice ;
  Db.register
    (Db.Journalize
       ("Slicing.Request.split_slice",
        Datatype.func2
          Db.Slicing.Project.dyn_t
          Db.Slicing.Slice.dyn_t
          (Datatype.list Db.Slicing.Slice.dyn_t)))
    Db.Slicing.Request.split_slice
    split_slice ;
  Db.Slicing.Request.apply_all := apply_all ;  (* Journalized *)
  Db.register
    (Db.Journalize
       ("Slicing.Request.apply_next_internal",
        Datatype.func Db.Slicing.Project.dyn_t Datatype.unit))
  Db.Slicing.Request.apply_next_internal
    apply_next_action ;
  Db.register
    (Db.Journalize
       ("Slicing.Request.apply_all_internal",
        Datatype.func Db.Slicing.Project.dyn_t Datatype.unit))
    Db.Slicing.Request.apply_all_internal
    apply_all_actions;

  (* No needs of Journalization for Db.Slicing.Request.functions *)
  Db.register Db.Journalization_not_required
    Db.Slicing.Request.is_request_empty_internal is_request_empty;
  Db.register Db.Journalization_not_required
    Db.Slicing.Request.is_already_selected_internal is_already_selected ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Request.pretty SlicingProject.print_proj_worklist

(** {3 Register external functions into Db.Slicing.Mark}  *)
let () =
  (* No needs of Journalization for Db.Slicing.Mark.functions *)
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.compare SlicingMarks.compare_marks ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.pretty SlicingMarks.pretty_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.make SlicingMarks.mk_user_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.is_bottom SlicingMarks.is_bottom_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.is_spare SlicingMarks.is_spare_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.is_ctrl SlicingMarks.is_ctrl_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.is_addr SlicingMarks.is_addr_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.is_data SlicingMarks.is_data_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.is_data SlicingMarks.is_data_mark ;
  Db.register Db.Journalization_not_required
    Db.Slicing.Mark.get_from_src_func Fct_slice.get_mark_from_src_fun

let main () =
  if SlicingParameters.is_on () then begin
    SlicingParameters.feedback ~level:1 "slicing requests in progress...";

    (* have to do the value analysis before the selections
     * because some functions use its results,
     * and the value analysis is not launched automatically. *)
    !Db.Value.compute ();

    let project_name = SlicingParameters.ProjectName.get () in
    let project = !Db.Slicing.Project.mk_project project_name  in
    !Db.Slicing.Project.set_project (Some project);
    !Db.Slicing.Request.add_persistent_cmdline project;
      (* Apply all pending requests. *)
    if !Db.Slicing.Request.is_request_empty_internal project then
      begin
 	SlicingParameters.warning "No internal slicing request from the command line." ;
	if SlicingParameters.Mode.Callers.get () then
	  let select_entry =
	    let spare_mark =
	      !Db.Slicing.Mark.make ~data:false ~addr:false ~ctrl:false
	    in
	    let kf_entry, _library = Globals.entry_point () in
	      SlicingParameters.warning "Adding an extra request on the entry point of function: %a." Kernel_function.pretty kf_entry;
	      !Db.Slicing.Select.select_entry_point_internal kf_entry spare_mark
	  in !Db.Slicing.Request.add_selection_internal project select_entry
      end;

    !Db.Slicing.Request.apply_all_internal project;

    if SlicingParameters.Mode.Callers.get () then
      !Db.Slicing.Slice.remove_uncalled project;
    let sliced_project_name =
      project_name ^ (SlicingParameters.ExportedProjectPostfix.get ())
    in
    SlicingParameters.set_off ();
    let sliced_project =
      !Db.Slicing.Project.extract sliced_project_name project
    in
      Project.on sliced_project SlicingParameters.clear ();
    if SlicingParameters.Print.get () then begin
      FC_file.pretty_ast ~prj:sliced_project ();
      SlicingParameters.result ~level:2 "Results :@ %a@."
        !Db.Slicing.Project.pretty project
    end;
    SlicingParameters.feedback ~level:2 "done (slicing requests in progress).";
  end

(** Register the function [main] as a main entry point. *)
let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
