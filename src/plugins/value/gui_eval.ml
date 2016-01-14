(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

type ('env, 'expr, 'v) evaluation_functions = {
  eval_and_warn: 'env -> 'expr -> 'v * bool;
  env: Cvalue.Model.t -> Value_types.callstack -> 'env;
  equal: 'v -> 'v -> bool;
  bottom: 'v;
  join: 'v -> 'v -> 'v;
  expr_to_gui_selection: 'expr -> gui_selection;
  res_to_gui_res: 'expr -> 'v -> gui_res;
}

(* special [with_alarms] value that logs important alarms, but allows execution
   to continue *)
let log_alarms () =
  let ok = ref true in
  let not_ok () = ok := false in
  let with_alarms = {
    CilE.others = {CilE.a_ignore with CilE.a_call=not_ok};
    unspecified = {CilE.a_ignore with CilE.a_call=not_ok};
    defined_logic =       CilE.a_ignore;
    imprecision_tracing = CilE.a_ignore;
  } in
  with_alarms, ok

(* Evaluate the given location in [state]. Catch an unreachable state, an
   invalid location, or another error during the evaluation. *)
let reduce_loc_and_eval ~with_alarms state ref_ok loc =
  if Cvalue.Model.is_top state then
    GO_Top, true
  else if Cvalue.Model.is_reachable state then
    let loc' = Locations.valid_part ~for_writing:false loc in
    if Locations.is_bottom_loc loc' then
      GO_InvalidLoc, false
    else
      try
        let size = Int_Base.project loc'.Locations.size in
        match Eval_op.copy_offsetmap
                ~with_alarms loc'.Locations.loc size state
        with
        | `Bottom -> GO_Bottom, false
        | `Top -> GO_Top, true
        | `Map offsm ->
          let ok = !ref_ok && (Locations.loc_equal loc loc') in
          GO_Offsetmap offsm, ok
      with Int_Base.Error_Top -> GO_Top, true
  else
    GO_Bottom, true

let lval_to_offsetmap state lv =
  let with_alarms, ok = log_alarms () in
  let ploc = Eval_exprs.lval_to_precise_loc ~with_alarms state lv in
  let aux loc (acc_res, acc_ok) =
    let res, ok = reduce_loc_and_eval ~with_alarms state ok loc in
    match acc_res, res with
    | GO_Offsetmap o1, GO_Offsetmap o2 ->
      GO_Offsetmap (Cvalue.V_Offsetmap.join o1 o2), acc_ok && ok
    | GO_Bottom, v | v, GO_Bottom -> v, acc_ok && ok
    | GO_Top, GO_Top -> GO_Top, acc_ok && ok
    | GO_InvalidLoc, GO_InvalidLoc -> GO_InvalidLoc, false
    | GO_InvalidLoc, GO_Offsetmap _ -> res, false
    | GO_Offsetmap _, GO_InvalidLoc -> acc_res, false
    | GO_Top, (GO_InvalidLoc | GO_Offsetmap _ as r)
    | (GO_InvalidLoc | GO_Offsetmap _ as r), GO_Top ->
      r, acc_ok && ok (* cannot happen, we should get Top everywhere *)
  in
  Precise_locs.fold aux ploc (GO_Bottom, true)

let lv_offsetmap_res_to_gui_res lv offsm =
  let typ = Some (Cil.unrollType (Cil.typeOfLval lv)) in
  GR_Offsm (offsm, typ)

let id_env state _ = state

let lval_ev =
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
    let with_alarms = CilE.warn_none_mode in
    let ploc = Eval_exprs.lval_to_precise_loc ~with_alarms state lv in
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
  match Cvalue.Model.find_base_or_default Base.null state with
  | `Bottom -> GO_InvalidLoc, true
  | `Top -> GO_Top, true
  | `Map m -> GO_Offsetmap m, true

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
    let with_alarms,ok = log_alarms () in
    let r = Eval_exprs.eval_expr ~with_alarms state e in
    r, !ok
  in
  {eval_and_warn=eval_exp_and_warn;
   env = id_env;
   equal=Cvalue.V.equal;
   bottom=Cvalue.V.bottom;
   join=Cvalue.V.join;
   expr_to_gui_selection = (fun e -> GS_Expr e);
   res_to_gui_res = (fun e v -> GR_Value (v, Some (Cil.typeOf e)));
  }

let pre_kf kf callstack =
  match Db.Value.get_initial_state_callstack kf with
  | None -> Cvalue.Model.top (* should not happen *)
  | Some h ->
    try Value_types.Callstack.Hashtbl.find h callstack
    with Not_found -> Cvalue.Model.top (* should not happen either *)

let env_here kf here callstack =
  let pre = pre_kf kf callstack in
  let c_labels = Eval_annots.c_labels kf callstack in
  Eval_terms.env_annot ~c_labels ~pre ~here ()

let env_pre _kf here _callstack =
  Eval_terms.env_pre_f ~pre:here ()

let env_post kf post callstack =
  let pre = pre_kf kf callstack in
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

type states_by_callstack = {
  states_before: Cvalue.Model.t Value_types.Callstack.Hashtbl.t;
  states_after: Cvalue.Model.t Value_types.Callstack.Hashtbl.t option;
}

let callstacks_at_stmt kf stmt =
  if results_kf_computed kf then
    (* Show 'after' states only in instructions. On blocks and if/switch
       statements, the notion of 'after' is counter-intuitive. *)
    let is_instr = match stmt.skind with Instr _ -> true | _ -> false in
    match Db.Value.get_stmt_state_callstack ~after:false stmt with
    | Some states_before ->
      let states_after =
        if is_instr
        then Db.Value.get_stmt_state_callstack ~after:true stmt
        else None
      in
      Some {states_before; states_after}
    | None -> None
  else None
;;

let callstacks_at_pre kf =
  if results_kf_computed kf then
    match Db.Value.get_initial_state_callstack kf with
    | Some states_before -> Some { states_before; states_after = None}
    | None -> None
  else None
;;

let callstacks_at_post kf =
  if not (!Db.Value.use_spec_instead_of_definition kf) && results_kf_computed kf
  then
    let ret = Kernel_function.find_return kf in
    match Db.Value.get_stmt_state_callstack ~after:true ret with
    | Some states_before -> Some { states_before; states_after = None}
    | None -> None
  else
    None
;;

let callstacks_at_gui_loc = function
  | GL_Stmt (kf, stmt) -> callstacks_at_stmt kf stmt
  | GL_Pre kf -> callstacks_at_pre kf
  | GL_Post kf -> callstacks_at_post kf


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

let env_gui_loc = function
  | GL_Stmt (kf, _) -> env_here kf
  | GL_Pre kf -> env_pre kf
  | GL_Post kf -> env_post kf

let tlval_ev lm =
  let tlval_to_offsetmap env tlv =
    let with_alarms, ok = log_alarms () in
    let loc = Eval_terms.eval_tlval_as_location env ~with_alarms tlv in
    let state = Eval_terms.env_current_state env in
    reduce_loc_and_eval ~with_alarms state ok loc
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
    let with_alarms, ok = log_alarms () in
    let z = Eval_terms.eval_tlval_as_zone
        ~for_writing:false env ~with_alarms tlv
    in
    z, !ok
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
    let with_alarms,ok = log_alarms () in
    let r = Eval_terms.((eval_term ~with_alarms env t).eover) in
    r, !ok
  in
  {eval_and_warn=eval_term_and_warn;
   env = env_gui_loc lm;
   equal=Cvalue.V.equal;
   bottom=Cvalue.V.bottom;
   join=Cvalue.V.join;
   expr_to_gui_selection = (fun t -> GS_Term t);
   res_to_gui_res = (fun t v -> GR_Value (v, term_c_type t));
}

let predicate_ev lm =
  let open Abstract_interp.Bot in 
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
   join = join_or_bottom Eval_terms.join_predicate_status;
   expr_to_gui_selection = (fun p -> GS_Predicate p);
   res_to_gui_res = (fun _ s -> GR_Status (to_status s));
}

type gui_selection_data = {
  alarm: bool;
  before: gui_res;
  before_string: string Lazy.t;
  after: gui_after;
  after_string: string Lazy.t;
}

let gui_selection_data_empty = {
  alarm = false;
  before = GR_Empty;
  before_string = lazy "";
  after = GA_NA;
  after_string = lazy "";
}

let data ~ok ~before ~after = {
  before; after; alarm = not ok;
  before_string = lazy (Pretty_utils.to_string pretty_gui_res before);
  after_string = (match after with
      | GA_NA | GA_Unchanged -> lazy "" (* won't be used *)
      | GA_After after -> lazy (Pretty_utils.to_string pretty_gui_res after));
}

type before_after = BABefore | BAAfter

(* Evaluation of [exp] in [before] and [after] using [ev]. [set_ba] must
   be called before each evaluation, with the state in which the evaluation
   will be done. *)
let make_data ev set_ba ~before ~after exp =
  set_ba BABefore;
  let vbefore, okbef = ev.eval_and_warn before exp in
  set_ba BAAfter;
  let res_after = Extlib.opt_map (fun a -> ev.eval_and_warn a exp) after in
  let ok = okbef (* currently, we do not warn for alarms in the post-state *) in
  match res_after with
  | Some (vafter, _okafter) ->
    if ev.equal vbefore vafter then
      data ~before:(ev.res_to_gui_res exp vbefore) ~after:GA_Unchanged ~ok
    else
      data ~before:(ev.res_to_gui_res exp vbefore)
           ~after:(GA_After (ev.res_to_gui_res exp vafter)) ~ok
  | None -> data ~before:(ev.res_to_gui_res exp vbefore) ~after:GA_NA ~ok

let make_data_all_callstacks append ev ~before ~after expr =
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
  Value_types.Callstack.Hashtbl.iter
    (fun callstack before ->
       let before = ev.env before callstack in
       let after =
         Extlib.opt_map
           (fun cafter ->
              let after =
                try Value_types.Callstack.Hashtbl.find cafter callstack
                with Not_found -> Cvalue.Model.bottom
              in
              ev.env after callstack)
           after
       in
       let callstack = if single_callstack
         then GC_Single callstack
         else GC_Callstack callstack
       in
       let cmexpr = ev.expr_to_gui_selection expr in
       try append cmexpr callstack (make_data ev set_ba ~before ~after expr)
       with e -> exn := e :: !exn
    ) before;
  (* Consolidated row, only if there are multiple callstacks *)
  if not single_callstack then begin
    let callstack = GC_Consolidated in
    let before = ev.res_to_gui_res expr !v_join_before in
    let after = match after with
      | None -> GA_NA
      | Some _ ->
        if ev.equal !v_join_before !v_join_after
        then GA_Unchanged
        else GA_After (ev.res_to_gui_res expr !v_join_after)
    in
    let expr = ev.expr_to_gui_selection expr in
    append expr callstack (data ~before ~after ~ok:!ok_join)
  end;
  !exn
;;

let gui_loc_logic_env lm =
  (* According to the ACSL spec, 'Pre' is not available in preconditions,
     but in practice it is parsed *)
  let pre () =
    let e = Logic_typing.Lenv.empty () in
    Logic_typing.(append_pre_label (append_init_label (append_here_label e)))
  in
  let stmt () = pre () in (* TODO: add LoopInit and LoopCurrent when supported*)
  let post () = Logic_typing.append_old_and_post_labels (stmt ()) in
  match lm with
  | GL_Stmt _ -> stmt ()
  | GL_Pre _ -> pre ()
  | GL_Post _ -> post ()



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
