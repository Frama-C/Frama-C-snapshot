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

(** Handle the project global object. *)

(**/**)

module T = SlicingInternals
module M = SlicingMacros

(**/**)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Building project } *)

(** API function : see {!val: Db.Slicing.Project.mk_project}.  *)
let mk_project name =
  SlicingParameters.feedback ~level:1 "making slicing project '%s'..." name;
  let r = { T.name = name ;
            T.application = Project.current () ;
            T.functions = Cil_datatype.Varinfo.Hashtbl.create 17;
            T.actions = [];
          } in
  SlicingParameters.feedback ~level:2 "done (making slicing project '%s')." name;
  r

let get_name proj = proj.T.name
(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Managing the slices} *)

let add_proj_actions proj actions = proj.T.actions <- actions @ proj.T.actions

(** Add a new slice for the function. It can be the case that it create actions
* if the function has some persistent selection, that make function calls to
* choose.
* @raise SlicingTypes.NoPdg when the function has no PDG.
* *)
let create_slice proj kf =
  let ff, actions = Fct_slice.make_new_ff (M.get_kf_fi proj kf) true in
  add_proj_actions proj actions; ff

(** Delete [ff_to_remove] if it is not called.
* @raise T.CantRemoveCalledFf if it is.
*)
let remove_ff proj ff_to_remove =
  let rec remove ff_list ff_num = match ff_list with
    | [] -> raise Not_found
    | ff :: tail ->
        if ff.T.ff_id = ff_num then (Fct_slice.clear_ff proj ff; tail)
        else ff :: (remove tail ff_num)
  in let fi = ff_to_remove.T.ff_fct in
  let ff_num = ff_to_remove.T.ff_id in
  let new_ff_list = remove fi.T.fi_slices ff_num in
    fi.T.fi_slices <- new_ff_list

let call_src_and_remove_all_ff proj fi =
  let do_call actions (ff_caller, call_id) =
    let new_actions =
      Fct_slice.apply_change_call proj ff_caller call_id (T.CallSrc (Some fi))
    in new_actions @ actions
  in
  let do_ff actions ff =
    let calls = ff.SlicingInternals.ff_called_by in
    let actions = List.fold_left do_call actions calls in
      remove_ff proj ff;
      actions
  in
  List.fold_left do_ff [] fi.T.fi_slices

let rec remove_uncalled_slices proj =
  let kf_entry, _ = Globals.entry_point () in
  let entry_name = Kernel_function.get_name kf_entry in
  let check_ff changes ff =
    match ff.T.ff_called_by with [] -> remove_ff proj ff; true | _ -> changes
  in let check_fi changes fi =
    if (M.fi_name fi) <> entry_name then
      List.fold_left check_ff changes (M.fi_slices fi)
    else changes
  in let changes = M.fold_fi check_fi false proj in
    if changes then remove_uncalled_slices proj else ()

(** Build a new slice [ff] which contains the marks of [ff1] and [ff2]
* and generate everything that is needed to choose the calls in [ff].
* If [replace] also generate requests call [ff] instead of [ff1] and [ff2]. *)
let merge_slices proj ff1 ff2 replace =
  let ff, ff_actions = Fct_slice.merge_slices ff1 ff2 in
    if replace then
      begin
        let add actions (caller, call) =
          let rq = SlicingActions.mk_crit_change_call caller call
                                                      (T.CallSlice ff) in
            rq :: actions
        in
        let actions = List.fold_left add [] ff2.T.ff_called_by in
        let actions = List.fold_left add actions ff1.T.ff_called_by in
          add_proj_actions proj actions
      end;
    add_proj_actions proj ff_actions;
    ff

let split_slice proj ff =
  let add (actions, slices) (caller, call) =
    let new_ff = Fct_slice.copy_slice ff in
    let rq = SlicingActions.mk_crit_change_call caller call
               (T.CallSlice new_ff) in
      rq::actions, new_ff::slices
  in
  let calls = List.tl ff.T.ff_called_by in (* keep ff for the first call *)
  let actions, slices = List.fold_left add ([], [ff]) calls in
    add_proj_actions proj actions;
    slices

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Getting information } *)

let get_slices proj kf = M.fi_slices (M.get_kf_fi proj kf)

let get_slice_callers ff = List.map (fun (ff, _) -> ff) ff.T.ff_called_by

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Adding requests } *)

let add_filter proj filter =
  proj.T.actions <- filter :: proj.T.actions

                                (*
let add_fct_filter proj f_id criterion =
  let ff_res =
    match f_id with
      | T.FctSrc fi -> Fct_slice.make_new_ff fi
      | T.FctSliced ff -> ff
  in let filter = SlicingActions.mk_ff_user_crit ff_res criterion in
  let _ = add_filter proj filter in
    ff_res
    *)

(** Add an action to the action list to filter the function [fct_id] with
  the given criterion. The filter gives a name to the result of the filter
  which is a new slice if the function to filter is the source one,
  or the given slice otherwise.
  *)
let add_fct_src_filter proj fi to_select =
  match to_select with
    (* T.CuSelect []  : don't ignore empty selection because
                        the input control node has to be selected anyway... *)
    | T.CuSelect select ->
        let filter = SlicingActions.mk_crit_fct_user_select fi select in
          add_filter proj filter
    | T.CuTop m ->
        let filter = SlicingActions.mk_crit_fct_top fi m in
          add_filter proj filter

    (*
let add_fct_src_filters proj fi actions =
  List.iter (fun a -> ignore (add_fct_src_filter proj fi a)) actions
                                                             *)

let add_fct_ff_filter proj ff to_select =
  match to_select with
      | T.CuSelect [] ->
          SlicingParameters.debug ~level:1
            "[SlicingProject.add_fct_ff_filter] (ignored empty selection)"
      | T.CuSelect select ->
          let filter = SlicingActions.mk_ff_user_select ff select in
            add_filter proj filter
      | T.CuTop _ -> assert false

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Print} *)

let print_project fmt proj =
  let get_slices var_fct =
    let kf = Globals.Functions.get var_fct in
    let fct_info = M.get_kf_fi proj kf in
    M.fi_slices fct_info
  in
  let print_var_decl glob var _spec = (* might be a function prototype *)
    (* TODO: print the spec also *)
    match var.Cil_types.vtype with
    | Cil_types.TFun _ -> (* function prototype TODO *)
      PrintSlice.print_original_glob fmt glob
    | _ -> PrintSlice.print_original_glob fmt glob
           (* TODO use global marks *)
  in
  let print glob =
    match glob with
    | Cil_types.GVarDecl (spec, var, _) -> 
      print_var_decl glob var spec
    | Cil_types.GFun (func, _) -> (* function definition *)
      let slices = get_slices func.Cil_types.svar in
      List.iter (PrintSlice.print_marked_ff fmt) slices
      (* TODO see if we have to print the original function *)
    | _ ->  
      PrintSlice.print_original_glob fmt glob
  in
  let source = Ast.get () in
  let global_decls = source.Cil_types.globals in
  List.iter print global_decls


let print_proj_worklist fmt proj =
  Format.fprintf fmt "Slicing project worklist [%s/%s] =@\n%a@.@."
    (Project.get_name proj.T.application)
    proj.T.name
    SlicingActions.print_list_crit proj.T.actions

let print_project_and_worklist fmt proj =
  print_project fmt proj;
  print_proj_worklist fmt proj

let pretty_slice fmt ff =
  PrintSlice.print_marked_ff fmt ff;
  Format.pp_print_newline fmt ()

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Managing (and applying) requests} *)

(** apply the given criterion and returns the list of new criterions to
    add to the project worklist. *)
let apply_fct_crit ff to_select =
  let actions = Fct_slice.apply_add_marks ff to_select in
    actions

let apply_appli_crit proj appli_crit =
  match appli_crit with
  | T.CaCall fi_to_call ->
    let kf_to_call = M.get_fi_kf fi_to_call in
    let add_actions actions (kf_caller,_) =
      let fi_caller = M.get_kf_fi proj kf_caller in
      let mark = SlicingMarks.mk_user_spare in
      let action =
        SlicingActions.mk_crit_mark_calls fi_caller kf_to_call mark in
      action :: actions
    in List.fold_left add_actions [] (!Db.Value.callers kf_to_call)
  | _ ->
    SlicingParameters.not_yet_implemented
      "This slicing criterion on application"

(** Add persistent the marks [node_marks] in [fi] and also add the marks
* to existing slices if any.
* If the propagation is ON, some actions are generated to propagate the
* persistent marks to the callers, and other actions are generated to
* make all the calls to [fi] visible.
* If there is no slice for [fi] we create a new one
* if it is the original request.
* It will be automatically created with the persistent marks.
* If it is a propagation, no need to create a new slice
* because it will be created when the call will be selected anyway.
* *)
let add_persistant_marks proj fi node_marks orig propagate actions =
  let new_fi_marks, actions =
    Fct_slice.add_marks_to_fi proj fi node_marks propagate actions
  in
  let actions = match M.fi_slices fi with
    | [] -> (* no slice *)
        let actions =
          if orig then
            let _ff, new_actions = Fct_slice.make_new_ff fi true in
              (* TODO catch NoPdg and mark fi as Top *)
              new_actions @ actions
          else actions
        in actions
    | slices ->
        let add_filter acc ff =
          let a = SlicingActions.mk_ff_user_select ff node_marks in a::acc
        in
          List.fold_left add_filter actions slices
  in
  let actions =
    if propagate && new_fi_marks then
      let a = SlicingActions.mk_appli_select_calls fi in actions @ [a]
    else actions
  in actions

let apply_fct_action proj fct_crit =
  match fct_crit.T.cf_fct with
  | T.FctSliced ff ->
    let _ = M.get_ff_pdg ff in
    let new_filters =
      match fct_crit.T.cf_info with
      | T.CcUserMark (T.CuSelect []) ->
        SlicingParameters.debug ~level:1
          "[apply_fct_action] ignore empty selection on existing slice";
        []
      | T.CcUserMark (T.CuSelect crit) -> apply_fct_crit ff crit
      | T.CcUserMark (T.CuTop _) -> assert false (* impossible on ff ! *)
      | T.CcChangeCall (call, f) ->
        Fct_slice.apply_change_call proj ff call f
      | T.CcChooseCall call ->
        Fct_slice.apply_choose_call proj ff call
      | T.CcMissingInputs (call, input_marks, more_inputs) ->
        Fct_slice.apply_missing_inputs proj ff call
          (input_marks, more_inputs)
      | T.CcMissingOutputs (call, output_marks, more_outputs) ->
        Fct_slice.apply_missing_outputs proj ff call
          output_marks more_outputs
      | T.CcPropagate _ -> assert false (* not for ff at the moment *)
      | T.CcExamineCalls marks ->
        Fct_slice.apply_examine_calls ff marks
    in
    SlicingParameters.debug ~level:4 "[slicingProject.apply_fct_action] result =@\n%a"
      PrintSlice.print_marked_ff ff;
    new_filters
  | T.FctSrc fi -> (* the marks have to be added to all slices *)
    let propagate = SlicingParameters.Mode.Callers.get () in
    match fct_crit.T.cf_info with
    | T.CcUserMark (T.CuSelect to_select) ->
      add_persistant_marks proj fi to_select true propagate []
    | T.CcUserMark (T.CuTop m) ->
      SlicingParameters.result ~level:1 "unable to slice %s (-> TOP)"
        (M.fi_name fi);
      let filters = call_src_and_remove_all_ff proj fi in
      Fct_slice.add_top_mark_to_fi fi m propagate filters
    | T.CcPropagate [] ->
      SlicingParameters.debug ~level:1
        "[apply_fct_action] nothing to propagate";
      []
    | T.CcPropagate node_marks ->
      add_persistant_marks proj fi node_marks false propagate []
    | T.CcExamineCalls _
    | _ ->
      SlicingParameters.not_yet_implemented
        "This slicing criterion on source function"

(** apply [filter] and return a list of generated filters *)
let apply_action proj filter =
  SlicingParameters.debug ~level:1 "[SlicingProject.apply_action] : %a" SlicingActions.print_crit filter;
  let new_filters =
    try match filter with
      | T.CrFct fct_crit ->
          begin
            try (apply_fct_action proj fct_crit)
            with PdgTypes.Pdg.Bottom ->
              SlicingParameters.debug ~level:1 "  -> action ABORTED (PDG is bottom)" ;
              []
          end
    | T.CrAppli appli_crit ->
          apply_appli_crit proj appli_crit
    with Not_found -> (* catch unprocessed Not_found here *) assert false
  in
    SlicingParameters.debug ~level:1 "  -> %d generated filters : %a@."
        (List.length new_filters)
        SlicingActions.print_list_crit new_filters;
    new_filters

let get_next_filter proj =
  match proj.T.actions with
    | [] -> SlicingParameters.debug ~level:2 "[SlicingProject.get_next_filter] No more filter";
            raise Not_found
    | f :: tail -> proj.T.actions <- tail; f

let apply_next_action proj =
  SlicingParameters.debug ~level:2 "[SlicingProject.apply_next_action]";
  let filter = get_next_filter proj in
  let new_filters = apply_action proj filter in
    proj.T.actions <- new_filters @ proj.T.actions

let is_request_empty proj =
  proj.T.actions = []

let apply_all_actions proj =
  let nb_actions = List.length proj.T.actions in
  let rec apply actions = match actions with [] -> ()
    | a::actions ->
        SlicingParameters.feedback ~level:2 "applying sub action...";
        let new_filters = apply_action proj a in
          apply new_filters;
          apply actions
  in
  SlicingParameters.feedback ~level:1 "applying %d actions..." nb_actions;
  let rec apply_user n =
    try
      let a = get_next_filter proj in
      SlicingParameters.feedback ~level:1 "applying actions: %d/%d..."
        n nb_actions;
      let new_filters = apply_action proj a in
      apply new_filters;
      apply_user (n+1)
    with Not_found ->
      if nb_actions > 0 then
        SlicingParameters.feedback
          ~level:2 "done (applying %d actions." nb_actions
  in
  apply_user 1

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
