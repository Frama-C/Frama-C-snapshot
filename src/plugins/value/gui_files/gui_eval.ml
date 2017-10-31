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
open Gui_types

let results_kf_computed kf =
  Db.Value.is_computed () &&
  match kf with
  | { fundec = Definition (fundec, _) } ->
    Mark_noresults.should_memorize_function fundec
  | { fundec = Declaration _ } -> true (* This value is not really used *)

let term_c_type t =
  Logic_const.plain_or_set
    (fun ltyp -> match Logic_utils.unroll_type ltyp with
       | Ctype typ -> Some typ
       | _ -> None
    ) (Logic_utils.unroll_type t.term_type)

let classify_pre_post kf ip =
  let open Property in
  match ip with
  | IPPredicate (PKEnsures (_, Normal),_,_,_) ->
    Some (GL_Post kf)
  | IPPredicate (PKEnsures _,_,_,_) | IPAxiom _ | IPAxiomatic _ | IPLemma _
  | IPTypeInvariant _ | IPGlobalInvariant _
  | IPOther _ | IPCodeAnnot _ | IPAllocation _ | IPReachable _
  | IPBehavior _ ->
    None
  (* TODO: instances are not shown as localizable, so they cannot be selected.
     If it becomes possible, they should probably be evaluated in the
     state corresponding to the statement they are at, except that the
     formals are not yet in scope... *)
  | IPPropertyInstance (_kfopt, _, _ip) -> None
  | IPPredicate (PKRequires _,_,_,_ | PKAssumes _,_,_,_ | PKTerminates ,_,_,_)
  | IPComplete _ | IPDisjoint _  | IPAssigns _ | IPFrom _ | IPDecrease _ ->
    Some (GL_Pre kf)

let gui_loc_logic_env lm =
  (* According to the ACSL spec, 'Pre' is not available in preconditions,
     but in practice it is parsed *)
  let pre () =
    let e = Logic_typing.Lenv.empty () in
    Logic_typing.(append_pre_label (append_init_label (append_here_label e)))
  in
  let stmt () = pre () in (*TODO: add LoopEntry and LoopCurrent when supported*)
  let post () = Logic_typing.append_old_and_post_labels (stmt ()) in
  match lm with
  | GL_Stmt _ -> stmt ()
  | GL_Pre _ -> pre ()
  | GL_Post _ -> post ()


type 'a gui_selection_data = {
  alarm: bool;
  before: 'a gui_res;
  before_string: string Lazy.t;
  after: 'a gui_after;
  after_string: string Lazy.t;
}

let gui_selection_data_empty = {
  alarm = false;
  before = GR_Empty;
  before_string = lazy "";
  after = GA_NA;
  after_string = lazy "";
}

let clear_caches () =
  Cvalue.V_Offsetmap.clear_caches ();
  Cvalue.Model.clear_caches ();
  Locations.Location_Bytes.clear_caches ();
  Locations.Zone.clear_caches ();
  Function_Froms.Memory.clear_caches ()

module type S = sig
  module Analysis : Analysis.S

  type ('env, 'expr, 'v) evaluation_functions = {
    eval_and_warn: 'env -> 'expr -> 'v * bool;
    env: Analysis.Dom.t -> Value_types.callstack -> 'env;
    equal: 'v -> 'v -> bool;
    bottom: 'v;
    join: 'v -> 'v -> 'v;
    expr_to_gui_selection: 'expr -> gui_selection;
    res_to_gui_res: 'expr -> 'v -> Analysis.Val.t gui_res;
  }

  val lval_as_offsm_ev: (Analysis.Dom.t, lval, gui_offsetmap_res) evaluation_functions
  val lval_zone_ev: (Analysis.Dom.t, lval, Locations.Zone.t) evaluation_functions
  val null_ev: (Analysis.Dom.t, unit, gui_offsetmap_res) evaluation_functions
  val exp_ev: (Analysis.Dom.t, exp, Analysis.Val.t Bottom.or_bottom) evaluation_functions
  val lval_ev: (Analysis.Dom.t, lval, Analysis.Val.t Eval.flagged_value) evaluation_functions

  val tlval_ev:
    gui_loc -> (Eval_terms.eval_env, term, gui_offsetmap_res) evaluation_functions
  val tlval_zone_ev:
    gui_loc -> (Eval_terms.eval_env, term, Locations.Zone.t) evaluation_functions
  val term_ev:
    gui_loc ->
    (Eval_terms.eval_env, term, Analysis.Val.t Bottom.or_bottom) evaluation_functions

  val predicate_ev:
    gui_loc ->
    (Eval_terms.eval_env,
     predicate,
     Eval_terms.predicate_status Bottom.or_bottom
    ) evaluation_functions

  val make_data_all_callstacks:
    ('a, 'b, 'c) evaluation_functions -> gui_loc ->  'b ->
    (gui_callstack * Analysis.Val.t gui_selection_data) list * exn list
end


module Make (X: Analysis.S) = struct

  module Analysis = X

  let get_cvalue_state =
    match X.Dom.get Cvalue_domain.key with
    | None -> fun _ -> Cvalue.Model.top
    | Some get -> fun state -> get state

  let get_precise_loc =
    match X.Loc.get Main_locations.ploc_key with
    | None -> fun _ -> Precise_locs.loc_top
    | Some get -> fun loc -> get loc


  module AGui_types = Gui_types.Make (X.Val)
  open AGui_types

  type ('env, 'expr, 'v) evaluation_functions = {
    eval_and_warn: 'env -> 'expr -> 'v * bool;
    env: X.Dom.t -> Value_types.callstack -> 'env;
    equal: 'v -> 'v -> bool;
    bottom: 'v;
    join: 'v -> 'v -> 'v;
    expr_to_gui_selection: 'expr -> gui_selection;
    res_to_gui_res: 'expr -> 'v -> X.Val.t gui_res;
  }

  (* Special function for l-values (Var vi, NoOffset). Since allocated variables
     may have an incomplete array type, it is simpler to extract the entire
     offsetmap and return it (instead of performing a copy of the offsetmap with
     a wacky size). For "normal" variables, this code is correct too.
     The returned boolean 'ok' means that the operation was possible. *)
  let extract_single_var state vi =
    let b = Base.of_varinfo vi in
    try
      match Cvalue.Model.find_base b state with
      | `Bottom -> GO_InvalidLoc, false
      | `Value m -> GO_Offsetmap m, true
      | `Top -> GO_Top, false
    with Not_found ->
      GO_InvalidLoc, false

  (* Evaluate the given location in [state]. Catch an unreachable state, an
     invalid location, or another error during the evaluation. The returned
     boolean means 'ok', i.e. that no error occurred. *)
  let reduce_loc_and_eval state loc =
    if Cvalue.Model.is_top state then
      GO_Top, false
    else
    if Cvalue.Model.is_reachable state then
      if Int_Base.(equal loc.Locations.size zero) then GO_Empty, true
      else
        let loc' = Locations.valid_part ~for_writing:false loc in
        if Locations.is_bottom_loc loc' then
          GO_InvalidLoc, false
        else
          try
            let size = Int_Base.project loc'.Locations.size in
            match Cvalue.Model.copy_offsetmap loc'.Locations.loc size state with
            | `Bottom -> GO_Bottom, false
            | `Value offsm ->
              let ok = Locations.is_valid ~for_writing:false loc in
              GO_Offsetmap offsm, ok
          with Abstract_interp.Error_Top -> GO_Top, false
    else (* Bottom state *)
      GO_Bottom, true

  let lval_to_offsetmap state lv =
    let loc, alarms = X.eval_lval_to_loc state lv in
    let ok = Alarmset.is_empty alarms in
    let state = get_cvalue_state state in
    let aux loc (acc_res, acc_ok) =
      let res, ok =
        match lv with (* catch simplest pattern *)
        | Var vi, NoOffset -> extract_single_var state vi
        | _ -> reduce_loc_and_eval state loc
      in
      match acc_res, res with
      | GO_Offsetmap o1, GO_Offsetmap o2 ->
        GO_Offsetmap (Cvalue.V_Offsetmap.join o1 o2), acc_ok && ok
      | GO_Bottom, v | v, GO_Bottom -> v, acc_ok && ok
      | GO_Empty, v | v, GO_Empty -> v, acc_ok && ok
      | GO_Top, GO_Top -> GO_Top, acc_ok && ok
      | GO_InvalidLoc, GO_InvalidLoc -> GO_InvalidLoc, false
      | GO_InvalidLoc, GO_Offsetmap _ -> res, false
      | GO_Offsetmap _, GO_InvalidLoc -> acc_res, false
      | GO_Top, (GO_InvalidLoc | GO_Offsetmap _ as r)
      | (GO_InvalidLoc | GO_Offsetmap _ as r), GO_Top ->
        r, acc_ok && ok (* cannot happen, we should get Top everywhere *)
    in
    match loc with
    | `Bottom -> GO_InvalidLoc, ok
    | `Value loc ->
      let ploc = get_precise_loc loc in
      Precise_locs.fold aux ploc (GO_Bottom, ok)

  let lv_offsetmap_res_to_gui_res lv offsm =
    let typ = Some (Cil.unrollType (Cil.typeOfLval lv)) in
    GR_Offsm (offsm, typ)

  let id_env state _ = state

  let lval_as_offsm_ev =
    {eval_and_warn=lval_to_offsetmap;
     env = id_env;
     equal=equal_gui_offsetmap_res;
     bottom=GO_Bottom;
     join=join_gui_offsetmap_res;
     expr_to_gui_selection = (fun lv -> GS_LVal lv);
     res_to_gui_res = lv_offsetmap_res_to_gui_res;
    }

  let lval_zone_ev =
    let lv_to_zone state lv =
      let loc, _alarms = X.eval_lval_to_loc state lv in
      match loc with
      | `Bottom -> Locations.Zone.bottom, false
      | `Value loc ->
        let ploc = get_precise_loc loc in
        let z = Precise_locs.enumerate_valid_bits ~for_writing:false ploc in
        z, false
    in
    {eval_and_warn=lv_to_zone;
     env = id_env;
     equal=Locations.Zone.equal;
     bottom=Locations.Zone.bottom;
     join=Locations.Zone.join;
     expr_to_gui_selection = (fun lv -> GS_LVal lv);
     res_to_gui_res = (fun _ z -> GR_Zone z);
    }

  let null_to_offsetmap state (_:unit) =
    let state = get_cvalue_state state in
    match Cvalue.Model.find_base_or_default Base.null state with
    | `Bottom -> GO_InvalidLoc, false
    | `Top -> GO_Top, false
    | `Value m -> GO_Offsetmap m, true

  let null_ev =
    {eval_and_warn=null_to_offsetmap;
     env = id_env;
     equal=equal_gui_offsetmap_res;
     bottom=GO_Bottom;
     join=join_gui_offsetmap_res;
     expr_to_gui_selection = (fun _ -> GS_AbsoluteMem);
     res_to_gui_res = (fun _ offsm -> GR_Offsm (offsm, None));
    }

  let exp_ev =
    let eval_exp_and_warn state e =
      let r = X.eval_expr state e in
      fst r, Alarmset.is_empty (snd r)
    in
    let res_to_gui_res e v =
      let flagged_value = Eval.{v; initialized=true; escaping=false; } in
      GR_Value (flagged_value, Some (Cil.typeOf e))
    in
    {eval_and_warn=eval_exp_and_warn;
     env = id_env;
     equal=Bottom.equal X.Val.equal;
     bottom=`Bottom;
     join=Bottom.join X.Val.join;
     expr_to_gui_selection = (fun e -> GS_Expr e);
     res_to_gui_res;
    }

  let lval_ev =
    let eval_and_warn state lval =
      let r = X.copy_lvalue state lval in
      let flagged_value = match fst r with
        | `Bottom -> Eval.Flagged_Value.bottom
        | `Value v -> v
      in
      flagged_value, Alarmset.is_empty (snd r)
    in
    {
      eval_and_warn;
      env = id_env;
      bottom = Eval.Flagged_Value.bottom;
      equal = Eval.Flagged_Value.equal X.Val.equal;
      join = Eval.Flagged_Value.join X.Val.join;
      expr_to_gui_selection = (fun lv -> GS_LVal lv);
      res_to_gui_res = (fun lv v -> GR_Value (v, Some (Cil.typeOfLval lv)));
    }

  let pre_kf kf callstack =
    match Db.Value.get_initial_state_callstack kf with
    | None -> Cvalue.Model.top (* should not happen *)
    | Some h ->
      try Value_types.Callstack.Hashtbl.find h callstack
      with Not_found -> Cvalue.Model.top (* should not happen either *)

  let env_here kf here callstack =
    let pre = pre_kf kf callstack in
    let here = get_cvalue_state here in
    let c_labels = Eval_annots.c_labels kf callstack in
    Eval_terms.env_annot ~c_labels ~pre ~here ()

  let env_pre _kf here _callstack =
    let here = get_cvalue_state here in
    Eval_terms.env_pre_f ~pre:here ()

  let env_post kf post callstack =
    let pre = pre_kf kf callstack in
    let post = get_cvalue_state post in
    let result =
      if !Db.Value.use_spec_instead_of_definition kf then
        None
      else
        let ret_stmt = Kernel_function.find_return kf in
        match ret_stmt.skind with
        | Return (Some ({enode = Lval (Var vi, NoOffset)}),_) -> Some vi
        | Return (None,_) -> None
        | _ -> assert false
    in
    let c_labels = Eval_annots.c_labels kf callstack in
    Eval_terms.env_post_f ~c_labels ~pre ~post ~result ()

  (* Maps from callstacks to Value states before and after a GUI location.
     The 'after' map is not always available. *)
  type states_by_callstack = {
    states_before: X.Dom.t Value_types.Callstack.Hashtbl.t Eval.or_top_or_bottom;
    states_after: X.Dom.t Value_types.Callstack.Hashtbl.t Eval.or_top_or_bottom;
  }

  let top_states_by_callstacks = { states_before = `Top; states_after = `Top }

  (* For statements: results are available only if the statement is reachable.
     After states are available only for instructions. *)
  let callstacks_at_stmt kf stmt =
    if results_kf_computed kf then
      (* Show 'after' states only in instructions. On blocks and if/switch
         statements, the notion of 'after' is counter-intuitive. *)
      let is_instr = match stmt.skind with Instr _ -> true | _ -> false in
      let states_before = X.get_stmt_state_by_callstack ~after:false stmt in
      let states_after = match states_before with
        | `Top | `Bottom as x -> x
        | `Value _ ->
          if is_instr
          then X.get_stmt_state_by_callstack ~after:true stmt
          else `Top
      in
      { states_before; states_after }
    else top_states_by_callstacks

  (* For pre-states: results are available only if the function is called,
     and correspond to the states before reduction by any precondition.
     After states are not available. *)
  let callstacks_at_pre kf =
    if results_kf_computed kf then
      let states_before = X.get_initial_state_by_callstack kf in
      { states_before; states_after = `Top }
    else top_states_by_callstacks

  (* For post-states: results are available only for functions with a body, for
     normal termination, and only when the function is called.
     After states are not available. *)
  let callstacks_at_post kf =
    if not (!Db.Value.use_spec_instead_of_definition kf) && results_kf_computed kf
    then
      let ret = Kernel_function.find_return kf in
      let states_before = X.get_stmt_state_by_callstack ~after:true ret in
      { states_before; states_after = `Top }
    else top_states_by_callstacks

  let callstacks_at_gui_loc = function
    | GL_Stmt (kf, stmt) -> callstacks_at_stmt kf stmt
    | GL_Pre kf -> callstacks_at_pre kf
    | GL_Post kf -> callstacks_at_post kf

  let env_gui_loc = function
    | GL_Stmt (kf, _) -> env_here kf
    | GL_Pre kf -> env_pre kf
    | GL_Post kf -> env_post kf

  let tlval_ev lm =
    let tlval_to_offsetmap env tlv =
      let alarms = ref false in
      let alarm_mode = Eval_terms.Track alarms in
      let loc = Eval_terms.eval_tlval_as_location env ~alarm_mode tlv in
      let state = Eval_terms.env_current_state env in
      let offsm, ok = reduce_loc_and_eval state loc in
      offsm, not !alarms && ok
    in
    {eval_and_warn=tlval_to_offsetmap;
     env = env_gui_loc lm;
     equal=equal_gui_offsetmap_res;
     bottom=GO_Bottom;
     join=join_gui_offsetmap_res;
     expr_to_gui_selection = (fun tlv -> GS_TLVal tlv);
     res_to_gui_res = (fun tlv offsm -> GR_Offsm (offsm, term_c_type tlv))
    }

  let tlval_zone_ev gl =
    let tlv_to_zone env tlv =
      let alarms = ref false in
      let alarm_mode = Eval_terms.Track alarms in
      let for_writing = false in
      let z = Eval_terms.eval_tlval_as_zone ~for_writing env ~alarm_mode tlv in
      z, not !alarms
    in
    {eval_and_warn=tlv_to_zone;
     env = env_gui_loc gl;
     equal=Locations.Zone.equal;
     bottom=Locations.Zone.bottom;
     join=Locations.Zone.join;
     expr_to_gui_selection = (fun tlv -> GS_TLVal tlv);
     res_to_gui_res = (fun _ z -> GR_Zone z);
    }

  let term_ev lm =
    let eval_term_and_warn env t =
      let alarms = ref false in
      let alarm_mode = Eval_terms.Track alarms in
      let r = Eval_terms.(eval_term ~alarm_mode env t) in
      `Value (from_cvalue r.Eval_terms.eover), not !alarms
    in
    let res_to_gui_res t v =
      let flagged_value = Eval.{v; initialized=true; escaping=false; } in
      GR_Value (flagged_value, term_c_type t)
    in
    {eval_and_warn=eval_term_and_warn;
     env = env_gui_loc lm;
     equal=Bottom.equal X.Val.equal;
     bottom=`Bottom;
     join=Bottom.join X.Val.join;
     expr_to_gui_selection = (fun t -> GS_Term t);
     res_to_gui_res;
    }

  let predicate_ev lm =
    let eval_predicate_and_warn env t =
      let r = Eval_terms.eval_predicate env t in
      `Value r, true (* TODO *)
    in
    let to_status = function
      | `Bottom -> Eval_terms.True
      | `Value s -> s
    in
    {eval_and_warn = eval_predicate_and_warn;
     env = env_gui_loc lm;
     equal = (=);
     bottom = `Bottom;
     join = Bottom.join Eval_terms.join_predicate_status;
     expr_to_gui_selection = (fun p -> GS_Predicate p);
     res_to_gui_res = (fun _ s -> GR_Status (to_status s));
    }

  let data ~ok ~before ~after = {
    before; after; alarm = not ok;
    before_string = lazy (Pretty_utils.to_string pretty_gui_res before);
    after_string = (match after with
        | GA_NA | GA_Unchanged | GA_Bottom -> lazy "" (* won't be used *)
        | GA_After after -> lazy (Pretty_utils.to_string pretty_gui_res after));
  }

  type before_after = BABefore | BAAfter

  (* Evaluation of [exp] in [before] and [after] using [ev]. [set_ba] must
     be called before each evaluation, with the state in which the evaluation
     will be done. *)
  let make_data ev set_ba ~before ~after exp =
    set_ba BABefore;
    let vbefore, ok = ev.eval_and_warn before exp in
    let before = ev.res_to_gui_res exp vbefore in
    match after with
    | `Top -> data ~before ~after:GA_NA ~ok
    | `Bottom -> data ~before ~after:(GA_Bottom) ~ok
    | `Value after ->
      set_ba BAAfter;
      (* Currently, we do not warn for alarms in the post-state. *)
      let vafter, _okafter = ev.eval_and_warn after exp in
      if ev.equal vbefore vafter
      then data ~before ~after:GA_Unchanged ~ok
      else data ~before ~after:(GA_After (ev.res_to_gui_res exp vafter)) ~ok

  let make_data_all_callstacks_from_states ev ~before ~after expr =
    let exn = ref [] in
    let single_callstack = (Value_types.Callstack.Hashtbl.length before) = 1 in
    let v_join_before = ref ev.bottom in
    let v_join_after = ref ev.bottom in
    let ok_join = ref true in
    let rba = ref BABefore in
    let set_ba ba = rba := ba in
    (* Change [ev] to store intermediate results for 'consolidated' line *)
    let eval_and_warn states e =
      let v, ok as r = ev.eval_and_warn states e in
      begin match !rba with
        | BABefore ->
          v_join_before := ev.join !v_join_before v;
          ok_join := !ok_join && ok;
        | BAAfter ->
          v_join_after := ev.join !v_join_after v;
      end;
      r
    in
    let ev = { ev with eval_and_warn } in
    (* Rows by callstack *)
    let list =
      Value_types.Callstack.Hashtbl.fold
        (fun callstack before acc ->
           let before = ev.env before callstack in
           let after = match after with
             | `Top | `Bottom as x -> x
             | `Value after ->
               try
                 let after = Value_types.Callstack.Hashtbl.find after callstack in
                 `Value (ev.env after callstack)
               (* If a callstack exists before the statement but is not found
                  after, then the post state for this callstack is bottom.  *)
               with Not_found -> `Bottom
           in
           let callstack = if single_callstack
             then GC_Single callstack
             else GC_Callstack callstack
           in
           try (callstack, (make_data ev set_ba ~before ~after expr)) :: acc
           with e -> exn := e :: !exn; acc
        ) before []
    in
    (* Consolidated row, only if there are multiple callstacks *)
    let list =
      if single_callstack
      then list
      else
        let callstack = GC_Consolidated in
        let before = ev.res_to_gui_res expr !v_join_before in
        let after = match after with
          | `Top | `Bottom -> GA_NA
          | `Value _ ->
            if ev.equal !v_join_before !v_join_after
            then GA_Unchanged
            else GA_After (ev.res_to_gui_res expr !v_join_after)
        in
        (callstack, (data ~before ~after ~ok:!ok_join)) :: list
    in
    list, !exn

  let make_data_all_callstacks ev loc v =
    let {states_before; states_after} = callstacks_at_gui_loc loc in
    match states_before with
    | `Top    -> [], [] (* Happens if none of the domains has saved its states.
                           In this case, nothing is displayed by the GUI. *)
    | `Bottom -> [], [] (* Bottom case: nothing is displayed either. *)
    | `Value before ->
      Cil.CurrentLoc.set (gui_loc_loc loc);
      clear_caches ();
      make_data_all_callstacks_from_states ev ~before ~after:states_after v
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
