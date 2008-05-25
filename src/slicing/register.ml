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

open Cil_types

module T = SlicingTypes.Internals
module M = SlicingMacros
module U = SlicingMacros
module C = SlicingCmds
module Act = SlicingActions

type appli = Project.t

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

let pretty_list pretty fmt l = List.iter (pretty fmt) l

let print_select fmt db_select =
  let db_fvar, select = db_select in
    Format.fprintf fmt "In %a : %a@\n"
      Ast_info.pretty_vname db_fvar Act.print_f_crit select

let get_select_kf (fvar, _select) = Globals.Functions.get fvar

let check_db_select fvar db_select =
  let db_fvar, select = db_select in
  if db_fvar.vid <> fvar.vid then
    begin
      Format.printf "slice name = %s <> select = %a@\n"
        (fvar.vname) print_select db_select;
      raise (Invalid_argument
           "This selection doesn't belong to the given function");
    end;
  fvar, select

let empty_db_select kf = (Kernel_function.get_vi kf, T.CuSelect [])
let top_db_select kf = (Kernel_function.get_vi kf, T.CuTop)
let check_kf_db_select kf = check_db_select (Kernel_function.get_vi kf)
let check_fi_db_select fi = check_db_select (M.fi_svar fi)
let check_ff_db_select ff = check_db_select (M.ff_svar ff)

let bottom_msg kf =
  Cil.log "[slicing] bottom PDG for function '%s': ignore selection@\n"
    (Kernel_function.get_name kf)

let select_pdg_nodes kf ?(select=empty_db_select kf) nodes mark =
  if M.debug1 () then
    Format.printf "[slicing] select_pdg_nodes@\n" ;
  let fvar, sel = check_kf_db_select kf select in
  match sel with 
    | T.CuTop -> select
    | T.CuSelect sel ->
        let pdg = !Db.Pdg.get kf in
        let nd_marks = Act.build_node_and_dpds_selection mark in
        (* let nd_marks = Act.build_simple_node_selection mark in *)
        let crit = [(nodes, nd_marks)] in
        let sel = 
          Act.translate_crit_to_select pdg ~to_select:sel crit  in
        let sel = T.CuSelect sel in
          (fvar, sel)

let select_stmt_zone kf ?(select=empty_db_select kf) stmt ~before loc mark =
  if M.debug1 () then
    Format.printf "[slicing] select_stmt_zone %a %s stmt %d (m=%a)@\n" 
      Locations.Zone.pretty loc 
      (if before then "before" else "after") stmt.sid
       SlicingMarks.pretty_mark mark;
  let fvar, sel = check_kf_db_select kf select in
  match sel with 
    | T.CuTop -> select
    | T.CuSelect sel ->
        try
          let pdg = !Db.Pdg.get kf in
          let nodes, undef = 
            !Db.Pdg.find_location_nodes_at_stmt pdg stmt before loc in
          let nd_marks = Act.build_simple_node_selection mark in
          let crit = [(nodes, nd_marks)] in
          let sel = Act.translate_crit_to_select pdg ~to_select:sel crit in
          let sel = PdgMarks.add_undef_in_to_select sel undef mark in
          let sel = T.CuSelect sel in
            (fvar, sel)
        with 
          | Db.Pdg.NotFound -> (* stmt probably unreachable *)
          let msg = M.sprintf "%a %s stmt %d"
                      Locations.Zone.pretty loc 
                      (if before then "before" else "after") stmt.sid
          in Format.printf "Nothing to select for %s@." msg;
             select
          | Db.Pdg.Top -> top_db_select kf
          | Db.Pdg.Bottom -> bottom_msg kf; select
          

(** this one is similar to [select_stmt_zone] with the return statement 
* when the function is defined, but it can also be used for undefined functions. *)
let select_output_zone kf ?(select=empty_db_select kf) loc mark =
  if M.debug1 () then
    Format.printf "[slicing] select_output_zone %a (m=%a)@\n" 
      Locations.Zone.pretty loc SlicingMarks.pretty_mark mark;
  let fvar, sel = check_kf_db_select kf select in
  match sel with 
    | T.CuTop -> select
    | T.CuSelect sel ->
        try
          let pdg = !Db.Pdg.get kf in
          let nodes, undef = !Db.Pdg.find_location_nodes_at_end pdg loc in
          let nd_marks = Act.build_simple_node_selection mark in
          let crit = [(nodes, nd_marks)] in
          let sel = Act.translate_crit_to_select pdg ~to_select:sel crit in
          let sel = PdgMarks.add_undef_in_to_select sel undef mark in
          let sel = T.CuSelect sel in
            (fvar, sel)
        with 
          | Db.Pdg.NotFound ->
              Format.printf "Nothing to select for %a@." 
                Locations.Zone.pretty loc;
              select
          | Db.Pdg.Top -> top_db_select kf
          | Db.Pdg.Bottom -> bottom_msg kf; select

let stmt_nodes_to_select pdg stmt =
  let rec add_stmt_nodes nodes s = 
    let stmt_nodes = 
      try !Db.Pdg.find_stmt_nodes pdg s with Db.Pdg.NotFound -> []
    in
      (* TODO : add this when visibility of anotations are ok 
let stmt_nodes =
  if List.length stmt_nodes > 1 then
    begin (* this is surely a call statement *)
      let out_and_ctrl node =
        let key = PdgTypes.Node.elem_key node in 
    match key with
      | PdgIndex.Key.SigCallKey (_, (PdgIndex.Signature.In _))
        -> false
            | PdgIndex.Key.SigCallKey (_, (PdgIndex.Signature.InCtrl))
                | PdgIndex.Key.SigCallKey (_, (PdgIndex.Signature.Out _))
                  -> true
            | _ -> assert false
  in
    List.filter out_and_ctrl stmt_nodes
      end
          else
            stmt_nodes
  in
        *)
      if M.debug2 () then
        Format.printf "   add_stmt_nodes on stmt %d (%a)@\n" s.sid
          (fun fmt l -> List.iter (!Db.Pdg.pretty_node true fmt) l) 
          stmt_nodes; 
      let nodes = stmt_nodes @ nodes in
      let add_block_stmts_nodes node_list blk =
        List.fold_left add_stmt_nodes node_list blk.bstmts
      in
        match s.skind with
          | Switch (_,blk,_,_) | Loop (_, blk, _, _, _) | Block blk -> 
              if M.debug2 () then
                Format.printf 
                  "   select_stmt_computation on composed stmt %d@\n" s.sid;
              add_block_stmts_nodes nodes blk
          | If (_,bthen,belse,_) ->
              let nodes = add_block_stmts_nodes nodes bthen in
                add_block_stmts_nodes nodes belse
          | _ -> nodes
  in
  let nodes = add_stmt_nodes [] stmt in
    nodes

let select_stmt_computation kf ?(select=empty_db_select kf) stmt mark =
  if M.debug1 () then
    Format.printf "[slicing] select_stmt_computation on stmt %d@\n" stmt.sid;
  let fvar, sel = check_kf_db_select kf select in
  match sel with 
    | T.CuTop -> select
    | T.CuSelect sel ->
        try
          let pdg = !Db.Pdg.get kf in
          let stmt_nodes = stmt_nodes_to_select pdg stmt in
          let nd_marks = Act.build_node_and_dpds_selection mark in
          let crit = [(stmt_nodes, nd_marks)] in
          let sel = 
            Act.translate_crit_to_select pdg ~to_select:sel crit in
          let sel = T.CuSelect sel in
            (fvar, sel)
        with Db.Pdg.Top -> top_db_select kf
          | Db.Pdg.Bottom -> bottom_msg kf; select

(** marking a call node means that a [choose_call] will have to decide that to
 * call according to the slicing-level, but anyway, the call will be visible.
 *)
let select_minimal_call kf ?(select=empty_db_select kf) stmt m =
  if M.debug1 () then
    Format.printf "[slicing] select_minimal_call@\n";
  let fvar, sel = check_kf_db_select kf select in
  match sel with 
    | T.CuTop -> select
    | T.CuSelect sel ->
        try
          let pdg = !Db.Pdg.get kf in
          let call = check_call stmt true in
          let call_node = !Db.Pdg.find_call_ctrl_node pdg call in
          let nd_marks = Act.build_simple_node_selection m in
          let crit = [([call_node], nd_marks)] in
          let sel = 
            Act.translate_crit_to_select pdg ~to_select:sel crit in
          let sel = T.CuSelect sel in
            (fvar, sel)
        with Db.Pdg.Top -> top_db_select kf
          | Db.Pdg.Bottom -> bottom_msg kf; select

let select_stmt_ctrl kf stmt =
  if M.debug1 () then
    Format.printf "[slicing] select_stmt_ctrl of sid:%d@\n" stmt.sid;
  let fvar = Kernel_function.get_vi kf in
  try 
    let pdg = !Db.Pdg.get kf in
    let nodes = !Db.Pdg.find_stmt_nodes pdg stmt in
    let mark = SlicingMarks.mk_user_mark ~ctrl:true ~data:false ~addr:false in
    let nd_marks = Act.build_ctrl_dpds_selection mark in
    let sel = Act.translate_crit_to_select pdg [(nodes, nd_marks)] in
    let sel = T.CuSelect sel in
      (fvar, sel)
  with Db.Pdg.Top -> top_db_select kf
    | Db.Pdg.Bottom -> bottom_msg kf; empty_db_select kf

let merge_select select1 select2 =
  let select = match select1, select2 with
      | T.CuTop, _ | _, T.CuTop -> T.CuTop
      | T.CuSelect select1, T.CuSelect select2 -> 
          (* TODO : we can probably do better...*)
          T.CuSelect (select1 @ select2) 
  in select

let merge_db_select db_select1 db_select2 =
  let fvar, select1 = db_select1 in
  let _, select2 = check_db_select fvar db_select2 in
  let select = merge_select select1 select2 in
    (fvar, select)

let empty_selects () = Cilutil.VarinfoHashtbl.create 7

let add_to_selects db_select hsel =
  let vf, select = db_select in
  let select =
    try let old_selection = Cilutil.VarinfoHashtbl.find hsel vf in
      merge_select old_selection select 
    with Not_found -> select
  in Cilutil.VarinfoHashtbl.replace hsel vf select

let fold_selects f hsel acc =
  let dof v sel acc = f (v, sel) acc in
  Cilutil.VarinfoHashtbl.fold dof hsel acc

let iter_selects f hsel = fold_selects (fun s _ -> f s) hsel ()

(** add [hsel1] to [hsel2] *)
let add_selects_to_selects hsel1 hsel2 =
  let add (vf, sel) () = add_to_selects (vf, sel) hsel2 in 
    fold_selects add hsel1 ()

let print_fct_stmts fmt (_proj, kf) =
  try
    let pdg = !Db.Pdg.get kf in
    PrintSlice.print_fct_from_pdg fmt pdg;
    Format.pp_print_flush fmt ()
  with Not_found -> ()

let add_crit_ff_change_call proj ff_caller call f_to_call =
  let crit = Act.mk_crit_change_call ff_caller call f_to_call in
    SlicingProject.add_filter proj crit

(** change the call to call the given slice.
 * This is a user request, so it might be the case that
 * the new function doesn't compute enough outputs :
 * in that case, add outputs first.
 *)
let call_ff_in_caller proj ~caller ~to_call =
  let kf_caller = M.get_ff_kf caller in
  let kf_to_call = M.get_ff_kf to_call in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller  kf_to_call in
  let ff_to_call = T.CallSlice to_call in
  let add_change_call stmt =
    add_crit_ff_change_call proj caller stmt ff_to_call ;
    match Fct_slice.check_outputs_before_change_call proj caller
                             stmt to_call with
      | [] -> ()
      | [c] -> SlicingProject.add_filter proj c
      | _ -> assert false

  in List.iter add_change_call call_stmts

let call_fsrc_in_caller proj ~caller ~to_call =
  let kf_caller = M.get_ff_kf caller in
  let fi_to_call = M.get_kf_fi proj to_call in
  let kf_to_call = M.get_fi_kf fi_to_call in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller kf_to_call in
  let add_change_call stmt =
    add_crit_ff_change_call proj caller stmt (T.CallSrc (Some fi_to_call))
  in List.iter add_change_call call_stmts

let call_min_f_in_caller proj ~caller ~to_call =
  let kf_caller = M.get_ff_kf caller in
  let pdg = U.get_ff_pdg caller in
  let call_stmts = !Db.Pdg.find_call_stmts ~caller:kf_caller to_call in
  let call_nodes = List.map (!Db.Pdg.find_call_ctrl_node pdg) call_stmts in
  let m = SlicingMarks.mk_user_spare in
  let nd_marks = Act.build_simple_node_selection m in
  let select = Act.translate_crit_to_select pdg [(call_nodes, nd_marks)] in
    SlicingProject.add_fct_ff_filter proj caller (T.CuSelect select)

let is_already_selected ff db_select =
  let _, select = check_ff_db_select ff db_select in
    match select with 
      | T.CuTop -> assert false
      | T.CuSelect to_select ->
          (* let pdg = !Db.Pdg.get (Globals.Functions.get fvar) in *)
          let new_marks = Fct_slice.filter_already_in ff to_select in
            if M.debug1 () then 
              Format.printf "[slicing] is_already_selected %a ?@\n"
                !Db.Slicing.Select.pretty db_select;
            let ok = if new_marks = [] then true else false in
              if M.debug1 () then 
                if ok then Format.printf "\t--> yes@\n"
                else Format.printf "\t--> no (missing %a)@\n" 
                       Act.print_sel_marks_list new_marks;
              ok

let add_ff_selection proj ff db_select =
  if M.debug1 () then 
    Format.printf "[slicing:add_ff_selection] %a to %s@\n"
      !Db.Slicing.Select.pretty db_select (M.ff_name ff);
  let _, select = check_ff_db_select ff db_select in
      SlicingProject.add_fct_ff_filter proj ff select

(** add a persistant selection to the function.
* This might change its slicing level in order to call slices later on. *)
let add_fi_selection proj db_select =
  if M.debug1 () then 
    Format.printf "[slicing:add_fi_selection] %a@\n"
                      !Db.Slicing.Select.pretty db_select;
  let kf = get_select_kf db_select in
  let fi = M.get_kf_fi proj kf in
  let _, select = db_select in
    SlicingProject.add_fct_src_filter proj fi select;
    match M.fi_slicing_level fi with
      |  T.DontSlice |  T.DontSliceButComputeMarks -> 
          M.change_fi_slicing_level fi  T.MinNbSlice;
          if M.debug1 () then 
            Format.printf "[slicing] changing %s slicing level to %s@\n"
              (M.fi_name fi)
              (M.str_level_option (M.fi_slicing_level fi))

      |  T.MinNbSlice |  T.MaxNbSlice -> ()

let get_mark_from_param ff var =
  let kf = M.get_ff_kf ff in
  let param_list = Kernel_function.get_formals kf in
  let rec find n var_list = match var_list with
  | [] -> raise Not_found
  | v :: var_list -> if v.vid = var.vid then n
                     else find (n+1) var_list
  in let n = find 1 param_list in
  Fct_slice.get_param_mark ff n

let get_called_slice ff stmt =
  match stmt.skind with
  | Instr (Call _) -> fst (Fct_slice.get_called_slice ff stmt)
  | _ -> None

let get_called_funcs ff stmt =
  match stmt.skind with
  | Instr (Call (_,expr_f,_,_)) ->
      if snd (Fct_slice.get_called_slice ff stmt)
      then snd (!Db.Value.expr_to_kernel_function
                  (Kstmt stmt)
                  ~with_alarms:CilE.warn_none_mode
                  ~deps:None
                  expr_f)
      else
        []
  | _ -> []


let db_pretty fmt (_project, kf) =
    try !Db.Pdg.pretty fmt (!Db.Pdg.get kf)
    with Not_found -> ()

let dot_project ~filename ~title project =
  PrintSlice.build_dot_project filename title project

let create_slice = 
  if M.debug1 () then Format.printf "[slicing] create_slice@\n";
  SlicingProject.create_slice 
let copy_slice _proj ff = 
  if M.debug1 () then Format.printf "[slicing] copy_slice@\n";
  Fct_slice.copy_slice ff
let split_slice = 
  if M.debug1 () then Format.printf "[slicing] split_slice@\n";
  SlicingProject.split_slice 
let merge_slices proj ff_1 ff_2 ~replace =
  if M.debug1 () then Format.printf "[slicing] merge_slices@\n";
  SlicingProject.merge_slices proj ff_1 ff_2 replace
let remove_slice = 
  if M.debug1 () then Format.printf "[slicing] remove_slice@\n";
  SlicingProject.remove_ff 

let apply_next_action = 
  if M.debug1 () then Format.printf "[slicing] apply_next_action@\n";
  SlicingProject.apply_next_action 

let apply_all_actions = 
  if M.debug1 () then Format.printf "[slicing] apply_all_actions@\n";
  SlicingProject.apply_all_actions



(** {2 Initialisation of the slicing plugin } *)

(** Register external functions into Db.
  *)
let () =
  Db.Slicing.Project.create_internal := SlicingProject.mk_project;
  Db.Slicing.Project.pretty := SlicingProject.print_project_and_worklist ;
  Db.Slicing.Project.print_dot := dot_project ;
  Db.Slicing.Project.extract := SlicingTransform.extract ;
  Db.Slicing.Project.is_directly_called_internal := M.is_src_fun_called ;
  Db.Slicing.Project.is_called := SlicingTransform.is_src_fun_called ;
  Db.Slicing.Project.has_persistent_selection := M.has_persistent_selection ;
  Db.Slicing.Project.change_slicing_level := M.change_slicing_level ;

  Db.Slicing.Select.select_stmt_internal := select_stmt_computation ;
  Db.Slicing.Select.select_min_call_internal := select_minimal_call ;
  Db.Slicing.Select.select_stmt_ctrl_internal := select_stmt_ctrl ;
  Db.Slicing.Select.select_stmt_zone_internal := select_stmt_zone ;
  Db.Slicing.Select.select_output_zone_internal := select_output_zone ;
  Db.Slicing.Select.select_pdg_nodes_internal := select_pdg_nodes ;

  Db.Slicing.Select.empty_selects := empty_selects ;
  Db.Slicing.Select.add_to_selects_internal := add_to_selects ;
  Db.Slicing.Select.iter_selects_internal := iter_selects ;

  Db.Slicing.Select.merge_internal := merge_db_select ;
  Db.Slicing.Select.get_function := get_select_kf;
  Db.Slicing.Select.pretty := print_select;

  (* higher level function from slicingCmds *)
  Db.Slicing.Select.select_pdg_nodes := C.select_pdg_nodes ;
  Db.Slicing.Select.select_stmt := C.select_stmt ;
  Db.Slicing.Select.select_stmt_ctrl := C.select_stmt_ctrl ;
  Db.Slicing.Select.select_stmt_lval := C.select_stmt_lval ;
  Db.Slicing.Select.select_stmt_lval_rw := C.select_stmt_lval_rw ;
  Db.Slicing.Select.select_stmt_zone := C.select_stmt_zone ;
  Db.Slicing.Select.select_stmt_pred := C.select_stmt_pred ;
  Db.Slicing.Select.select_stmt_term := C.select_stmt_term ;
  Db.Slicing.Select.select_stmt_annot := C.select_stmt_annot ;
  Db.Slicing.Select.select_stmt_annots := C.select_stmt_annots ;
  Db.Slicing.Select.select_func_annots := C.select_func_annots ;
  Db.Slicing.Select.select_func_lval := C.select_func_lval ;
  Db.Slicing.Select.select_func_lval_rw := C.select_func_lval_rw ;
  Db.Slicing.Select.select_func_zone := C.select_func_zone ;
  Db.Slicing.Select.select_func_return := C.select_func_return ;
  Db.Slicing.Select.select_func_calls_to := C.select_func_calls_to ;
  Db.Slicing.Select.select_func_calls_into := C.select_func_calls_into ;

  Db.Slicing.Slice.create := create_slice ;
  Db.Slicing.Slice.remove := remove_slice ;
  Db.Slicing.Slice.remove_uncalled := SlicingProject.remove_uncalled_slices ;
  Db.Slicing.Slice.get_all := SlicingProject.get_slices ;
  Db.Slicing.Slice.get_callers := SlicingProject.get_slice_callers ;
  Db.Slicing.Slice.get_called_slice := get_called_slice ;
  Db.Slicing.Slice.get_called_funcs := get_called_funcs ;
  Db.Slicing.Slice.pretty := SlicingProject.pretty_slice ;
  Db.Slicing.Slice.get_mark_from_stmt := Fct_slice.get_stmt_mark;
  Db.Slicing.Slice.get_mark_from_label := Fct_slice.get_label_mark ;
  Db.Slicing.Slice.get_mark_from_formal := get_mark_from_param ;
  Db.Slicing.Slice.get_mark_from_local_var := Fct_slice.get_local_var_mark ;
  Db.Slicing.Slice.get_user_mark_from_inputs := Fct_slice.merge_inputs_m1_mark ;
  Db.Slicing.Slice.get_callers := SlicingProject.get_slice_callers ;
  Db.Slicing.Slice.get_function := M.get_ff_kf ;

  (* higher level function from slicingCmds *)
  Db.Slicing.Request.propagate_user_marks := C.topologic_propagation ;
  Db.Slicing.Request.add_selection := C.add_selection ;
  Db.Slicing.Request.add_persistent_selection := C.add_persistent_selection ;

  Db.Slicing.Request.is_already_selected_internal := is_already_selected ;
  Db.Slicing.Request.add_slice_selection_internal := add_ff_selection ;
  Db.Slicing.Request.add_selection_internal := add_fi_selection ;
  Db.Slicing.Request.add_call_slice := call_ff_in_caller ;
  Db.Slicing.Request.add_call_fun := call_fsrc_in_caller ;
  Db.Slicing.Request.add_call_min_fun := call_min_f_in_caller ;
  Db.Slicing.Request.merge_slices := merge_slices ;
  Db.Slicing.Request.copy_slice := copy_slice ;
  Db.Slicing.Request.split_slice := split_slice ;
  Db.Slicing.Request.apply_next_internal := apply_next_action ;
  Db.Slicing.Request.apply_all_internal := apply_all_actions ;
  Db.Slicing.Request.apply_all := C.apply_all ;
  Db.Slicing.Request.pretty := SlicingProject.print_proj_worklist ;

  Db.Slicing.Mark.compare := SlicingMarks.compare_marks ;
  Db.Slicing.Mark.pretty := SlicingMarks.pretty_mark ;
  Db.Slicing.Mark.make := SlicingMarks.mk_user_mark ;
  Db.Slicing.Mark.is_bottom := SlicingMarks.is_bottom_mark ;
  Db.Slicing.Mark.is_spare := SlicingMarks.is_spare_mark ;
  Db.Slicing.Mark.is_ctrl := SlicingMarks.is_ctrl_mark ;
  Db.Slicing.Mark.is_addr := SlicingMarks.is_addr_mark ;
  Db.Slicing.Mark.is_data := SlicingMarks.is_data_mark ;
;;
  
(** Register the plugin options *)
let () =
  Options.add_plugin ~name:"slicing" ~descr:"slicing analysis"
    [
      "-slice-print",
      Arg.Unit Cmdline.Slicing.Print.on,
      ": pretty print the sliced code";
      
      "-slice-undef-functions",
      Arg.Unit Cmdline.Slicing.Mode.SliceUndef.on,
      ": allow the use of the -slicing-level option for calls to undefined functions\n"
      ^"\t(by default, don't slice the prototype of undefined functions)";
      
      "-slicing-level",
      Arg.Int (fun n -> match n with
                 | 0 | 1 | 2 | 3 -> Cmdline.Slicing.Mode.Calls.set n
                 | _ -> raise
                     (Arg.Help ("Invalid argument for -slicing-level option:" ^
                        " not in range [0-3]\n"))
      ),
      "n : set the default level of slicing used to propagate to the calls\n"
      ^"\t0 : don't slice the called functions\n"
      ^"\t1 : don't slice the called functions but propagate the marks anyway\n"
      ^"\t2 : try to use existing slices, create at most one\n"
      ^"\t3 : most precise slices\n"
      ^"  note: this value" 
      ^ (Format.sprintf " (defaults to %d) " (Cmdline.Slicing.Mode.Calls.get ()))
      ^"is not used for calls to undefined functions\n"
      ^"\texcept when '-slice-undef-functions' option is set."
      ^"\n"; (* A new line as separator for
                next options related to slicing requests.
                Alphabetic order is used for them. *)
  
      "-slice-assert",
      Arg.String Cmdline.Slicing.Select.Assert.add_set,
      "f1,...,fn : select the assertions of functions f1,...,fn";
      
      "-slice-calls",
      Arg.String Cmdline.Slicing.Select.Calls.add_set,
      "f1,...,fn : select every calls to functions f1,...,fn, and all their effect";
      
      "-slice-loop-inv",
      Arg.String Cmdline.Slicing.Select.LoopInv.add_set,
      "f1,...,fn : select the loop invariants of functions f1,...,fn";
      
      "-slice-loop-var",
      Arg.String Cmdline.Slicing.Select.LoopVar.add_set,
      "f1,...,fn : select the loop variants of functions f1,...,fn";
      
      "-slice-pragma",
      Arg.String Cmdline.Slicing.Select.Pragma.add_set,
      "f1,...,fn : use the slicing pragmas in the code of functions f1,...,fn as slicing criteria\n"
      ^"\t//@slice pragma ctrl; : to reach this control-flow point\n"
      ^"\t//@slice pragma expr <expr_desc;> : to preserve the value of an expression at this control-flow point\n"
      ^"\t//@slice pragma stmt; : to preserve the effect of the next statement";
      
      "-slice-return",
      Arg.String Cmdline.Slicing.Select.Return.add_set,
      "f1,...,fn : select the result (returned value) of functions f1,...,fn";
      
      "-slice-threat",
      Arg.String Cmdline.Slicing.Select.Threat.add_set,
      "f1,...,fn : select the threats of functions f1,...,fn";
      
      "-slice-value",
      Arg.String Cmdline.Slicing.Select.Value.add_set,
      "v1,...,vn : select the result of left-values v1,...,vn at the end of the function given as entry point\n"
      ^"\t (addresses are evaluated at the beginning of the function given as entry point)";
      
      "-slice-rd",
      Arg.String Cmdline.Slicing.Select.RdAccess.add_set,
      "v1,...,vn : select the read accesses to left-values v1,...,vn\n"
      ^"\t (addresses are evaluated at the beginning of the function given as entry point)";
      
      "-slice-wr",
      Arg.String Cmdline.Slicing.Select.WrAccess.add_set,
      "v1,...,vn : select the write accesses to left-values v1,...,vn\n"
      ^"\t (addresses are evaluated at the beginning of the function given as entry point)"
      ^"\n"; (* A new line as separator for
                next options related to debug. *)
    ]
    ~debug:[ "-debug",
        Arg.Int Cmdline.Slicing.Mode.Verbose.set,
        " n : set the level of slicing debug to n";
        
        "-verbose",
        Arg.Unit Cmdline.Slicing.Mode.Verbose.incr,
        ": increment the level of slicing debug";
                                              
        "-no-slice-callers",
        Arg.Unit Cmdline.Slicing.Mode.Callers.off,
        ": don't propagate the slicing to the function callers";
           ]
      
;;
