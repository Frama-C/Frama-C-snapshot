(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
open Cil
open Value_util
open Eval_annots

module AB = Eval_annots.ActiveBehaviors

(** Evaluate the assigns [assigns] of [kf] (for one or more behaviors)
    in the state [with_formals].
    [per_behavior] indicates that the assigns clause is computed separately
    for each behavior. It is used to control the emission of warnings. *)
let compute_assigns kf assigns return_used sclob ~with_formals ~per_behavior =
  let with_alarms = CilE.warn_none_mode in
  let vi = Kernel_function.get_vi kf in
  if (not (Cvalue.Model.is_reachable with_formals)) ||
    Cil.hasAttribute "noreturn" vi.vattr
  then
    None, Cvalue.Model.bottom
  else
    let returned_value, with_formals =
      Library_functions.returned_value kf with_formals
    in
    let returned_value = ref returned_value in
    let env = Eval_terms.env_assigns with_formals in
    let pp_eval_error fmt e =
      if e <> Eval_terms.CAlarm then
        Format.fprintf fmt "@ (%a)" Eval_terms.pretty_logic_evaluation_error e
    in
    (* Treat one assign ... \from ... clause. Update [state] accordingly,
       as well as [returned_value] and [sclob] *)
    let treat_assign state ({it_content = out}, ins as asgn) =
      (* Evaluate the contents of one element of the from clause, topify them,
         and add them to the current state of the evaluation in acc *)
      let one_from_contents acc { it_content = t } =
        let r = Eval_terms.eval_term ~with_alarms env t in
	Cvalue.V.join acc (Cvalue.V.topify_arith_origin r.Eval_terms.eover)
      in
      (* evaluation of the entire from clause *)
      let froms_contents =
        match ins with
          | FromAny -> Cvalue.V.top_int
          | From l ->
            try
              let filter x = not(List.mem "indirect" x.it_content.term_name) in
              let direct = List.filter filter l in
              List.fold_left one_from_contents Cvalue.V.top_int direct
            with Eval_terms.LogicEvalError e ->
	      warning_once_current "cannot interpret@ 'from' clause@ \
                '%a'@ of function %a%a" Printer.pp_from asgn
                Kernel_function.pretty kf pp_eval_error e;
              Cvalue.V.top
      in
      (* Treat one location coming from the evaluation of [out] *)
      let treat_output_loc acc loc =
        let valid = Locations.valid_part ~for_writing:true loc in
        if Locations.is_bottom_loc valid then
          (if  (not (Locations.is_bottom_loc loc))
	   then (Value_parameters.warning ~current:true ~once:true
		   "@[Completely invalid destination@ for assigns@ clause %a.@ \
                 Ignoring.@]" Printer.pp_term out);
	   acc)
        else (
          Locals_scoping.remember_if_locals_in_value sclob loc froms_contents;
	  let state' =
            snd (Cvalue.Model.add_binding ~exact:false acc loc froms_contents)
	  in
          if Cvalue.Model.equal Cvalue.Model.top state' then (
            Value_parameters.error ~once:true ~current:true
              "Cannot@ handle@ assigns@ for %a,@ location@ is@ too@ imprecise@ \
                 (%a).@ Assuming@ it@ is@ not@ assigned,@ but@ be@ aware@ this\
                 @ is@ incorrect." Printer.pp_term out Locations.pretty loc;
            acc)
          else state')
      in
      (* Treat the output part of the assigns clause *)
      if Logic_utils.is_result out then (
        (* Special case for \result *)
        returned_value := Cvalue.V.join froms_contents !returned_value;
        state
      ) else
        try
          (* TODO: warn about errors during evaluation *)
	  let loc = Eval_terms.eval_tlval_as_location ~with_alarms env out in
	  treat_output_loc state loc
        with
          | Eval_terms.LogicEvalError e ->
            warning_once_current
              "cannot interpret assigns %a@ in function %a%a; effects will be \
                 ignored"
              Printer.pp_term out Kernel_function.pretty kf pp_eval_error e;
            state
    in
    (* Treat all the assigns for the function *)
    let state =
      match assigns with
        | WritesAny ->
          (* No need to warn for missing assigns when evaluating a behavior,
             we can always use those of the default behavior as a fallback. *)
          if not per_behavior then
            warning_once_current "Cannot handle empty assigns clause. Assuming assigns \\nothing: be aware this is probably incorrect.";
          with_formals
        | Writes l ->
          (* Warn for clauses without \from *)
          let no_from = List.filter (fun (_, from) -> from = FromAny) l in
          (match no_from with
            | (out, _) :: _ as l ->
              let source = fst out.it_content.term_loc in
              Value_parameters.warning ~source ~once:true
                "@[no \\from part@ for clause '%a' of@ function %a@]"
                Printer.pp_assigns (Writes l) Kernel_function.pretty kf
            | [] -> ()
          );
          (* Warn in case the 'assigns \result' clause is missing *)
          (if return_used then
              let for_result (out, _) = Logic_utils.is_result out.it_content in
              let result = List.filter for_result l in
              if result = [] then
                let source = fst (Kernel_function.get_location kf) in
                Value_parameters.warning ~once:true ~source
                  "@[no 'assigns \\result@ \\from ...'@ clause@ specified \
                    for@ function %a@]" Kernel_function.pretty kf
          );  
          (* Compute the effects of the assigns clause *)
          List.fold_left treat_assign with_formals l
    in
    let retres_vi, state =
      let return_type = getReturnType vi.vtype in
      if isVoidType return_type
      then None, state
      else
        let offsetmap = Eval_op.offsetmap_of_v return_type !returned_value in
        let retres_vi = Library_functions.get_retres_vi kf in
        let retres_base = Base.of_varinfo retres_vi in
        let state = Cvalue.Model.add_base retres_base offsetmap state in
        Some retres_vi, state
    in
    retres_vi, state

(* Performs the join of two varinfo option, used for the return value.
   If both are Some, then they should be the same. *)
let join_rvi rvi1 rvi2 = Extlib.merge_opt
    (fun () vi1 vi2 ->
       assert (Cil_datatype.Varinfo.equal vi1 vi2);
       vi1
    ) () rvi1 rvi2

(* Returns the assigns clause to be used during per-behavior processing.
   The specification states that, if a behavior has no assigns clause,
   then the assigns clause of the default behavior must be used instead. *)
let get_assigns_for_behavior ab b =
  match b.b_assigns with
  | WritesAny -> (* no assigns clause, using the default behavior's *)
    let def_b = AB.behavior_from_name ab Cil.default_behavior_name in
    def_b.b_assigns
  | _ -> b.b_assigns

let compute_assigns_and_post_conds_for_behavior kf ab ~with_formals bhv_states_after_requires return_used sclob b_name =
  let b = AB.behavior_from_name ab b_name in
  let states_after_requires = List.assoc b_name bhv_states_after_requires in
  let retres_vi = ref None in
  let states_after_assigns =
    State_set.fold (fun acc_st (state, trace) ->
        let rvi, state_after_assigns =
          let assigns = get_assigns_for_behavior ab b in
          compute_assigns kf assigns return_used sclob state true
        in
        retres_vi := join_rvi !retres_vi rvi;
        State_set.add (state_after_assigns, trace) acc_st
      ) State_set.empty states_after_requires
  in
  let states_after_post_conds =
    Eval_annots.check_fct_postconditions_for_behavior kf ab b Normal
      ~result:!retres_vi ~per_behavior:true ~pre_state:with_formals
      states_after_assigns
  in
  (b_name, states_after_post_conds)

(* When there is at least one behavior whose active status is [True], we can
   perform the intersection of the states and assigns clauses, and compute the
   result for every [True] state at once. Here, [b_names] is a list of True
   behaviors. *)
let compute_merged_assigns_and_post_conds_for_behaviors kf ab bhv_states_after_requires return_used sclob b_names =
  if b_names = [] then State_set.empty
  else
    let bs = List.map (AB.behavior_from_name ab) b_names in
    let states_after_requires_list =
      Extlib.filter_map (fun (b_name, _) -> List.mem b_name b_names) snd
        bhv_states_after_requires
    in
    let state_after_requires, trace =
      State_set.(join (narrow_list  states_after_requires_list))
    in
    let retres_vi = ref None in
    let state_after_assigns =
      List.fold_left (fun st0 b ->
          let rvi, state_after_assigns =
            let assigns = get_assigns_for_behavior ab b in
            compute_assigns kf assigns return_used sclob st0 true
          in
          retres_vi := join_rvi !retres_vi rvi;
          state_after_assigns
        ) state_after_requires bs
    in
    let states_after_post_conds =
      Eval_annots.check_fct_postconditions kf ab bs Normal ~result:!retres_vi
        ~per_behavior:true ~pre_state:state_after_requires
        (State_set.singleton (state_after_assigns, trace))
    in
    let l = State_set.to_list states_after_post_conds in
    let lr = List.map (fun s -> s,trace) l in
    State_set.of_list lr

(** Computes and returns three disjoint sets, [b_t], [b_u] and [b_f],
   where [b_t] contains all behaviors which are certainly active
   (status [True], and not empty after requires), [b_u] contains
   behaviors which are possibly active (status [Unknown], and
   not empty after requires), and [b_f] contains behaviors which
   are empty.
   The default behavior is never included in the returned sets.
   Note that [b_f] does NOT contain behaviors which were previously
   known to be inactive (set to [False] by the assumes clause).
   [bhv_states_post_requires] is an association list from
   behaviors to their states after applying requires clauses.
*)
let partition_behaviors_after_requires ab bhv_states_after_requires =
  (* We filter the default behavior here *)
  let bhv_states_after_requires' =
    List.filter
      (fun (b_name, _) -> b_name <> Cil.default_behavior_name)
      bhv_states_after_requires
  in
  List.fold_left (fun (b_t0, b_u0, b_f0) (b_name, stateset) ->
      if State_set.is_empty stateset then
        (* falsely active behavior: requires clauses not satisfied *)
        (b_t0, b_u0, b_name :: b_f0)
      else
        (* requires clauses did not change the behavior's status *)
        match ab.AB.is_active (AB.behavior_from_name ab b_name) with
        | Eval_terms.True -> (b_name :: b_t0, b_u0, b_f0)
        | Eval_terms.Unknown -> (b_t0, b_name :: b_u0, b_f0)
        | Eval_terms.False -> (b_t0, b_u0, b_name :: b_f0)
    ) ([],[],[]) bhv_states_after_requires'

(**
   Promotes [Unknown] behaviors from [b_u] to [True] when they are the only
   possible choice in a given complete set.
   Returns the new sets [b_t] and [b_u], of [True] and [Unknown] behaviors.
*)
let promote_complete_unknown_behaviors comp_lists b_t b_u =
  ListLabels.fold_left ~init:(b_t,b_u) comp_lists
    ~f:(fun (acc_t,acc_u as acc) comp_set ->
        let unk_bhvs_in_set = List.filter (ListLabels.mem ~set:b_u) comp_set in
        match unk_bhvs_in_set with
        | [] -> (* no Unknown behaviors, nothing to promote *)acc
        | [b_unk] -> (* a single Unknown behavior, will be promoted to True *)
          b_unk :: acc_t,List.filter (fun b -> b <> b_unk) acc_u
        | _ -> (* more than one Unknown behavior, cannot promote *)
          acc_t,acc_u
      )

(* Applies the given [assumes] clauses of a given behavior [b] to the states
   passed as argument, in order to reduce them (no status is emitted). *)
let reduce_by_assumes_of_behavior kf states b =
  let build_prop assume = Property.ip_of_assumes kf Kglobal b assume in
  let build_env pre = Eval_terms.env_pre_f ~pre () in
  eval_and_reduce_p_kind
    kf b ~active:true Assumes b.b_assumes build_prop build_env states

(* Reduce the state by the assumes and requires clauses for behavior [b],
   and emit statuses for the requires. *)
let compute_assumes_and_requires_for_behavior kf ab b call_kinstr states =
  let states_after_assumes = reduce_by_assumes_of_behavior kf states b in
  check_fct_preconditions_for_behavior kf ab ~per_behavior:true
    call_kinstr states_after_assumes b


let compute_using_specification_single_behavior kf spec ~call_kinstr ~with_formals =
  let ab = AB.create_from_spec with_formals spec in
  let stateset =
    Eval_annots.check_fct_preconditions kf ab call_kinstr with_formals in
  let (with_formals,trace) = State_set.join stateset in
  let return_used = match call_kinstr with
    | Kglobal -> true
    | Kstmt {skind = Instr (Call (lv, _, _, _))} ->
      lv <> None || Value_util.postconditions_mention_result spec
    | _ -> assert false
  in
  let sclob = Locals_scoping.bottom () in
  let retres_vi, result_state =
    let assigns = Ast_info.merge_assigns (AB.active_behaviors ab) in
    compute_assigns kf assigns return_used sclob ~with_formals ~per_behavior:false
  in
  let result_states =
    Eval_annots.check_fct_postconditions kf ab (Annotations.behaviors kf) Normal
      ~result:retres_vi ~per_behavior:false ~pre_state:with_formals
      (State_set.singleton (result_state,trace))
  in
  let aux state =
    match retres_vi with
    | None -> None, state
    | Some vi ->
      match state with
      | Cvalue.Model.Bottom -> None, state
      | Cvalue.Model.Top -> Warn.warn_top ()
      | Cvalue.Model.Map _ ->
        let retres_base = Base.of_varinfo vi in
        let without_ret = Cvalue.Model.remove_base retres_base state in
        match Cvalue.Model.find_base retres_base state with
        | `Map m -> Some m, without_ret
        | `Bottom (*tested above*) | `Top (*state is not top*)-> assert false
  in
  { Value_types.c_values = List.map aux (State_set.to_list result_states);
    c_clobbered = sclob.Locals_scoping.clob;
    c_cacheable = Value_types.Cacheable;
    c_from = None;
  }


let compute_using_specification_multiple_behaviors kf spec ~call_kinstr ~with_formals =
    let ab = AB.create_from_spec with_formals spec in
    let sclob = Locals_scoping.bottom () in
    let complete_bhvs_lists = ab.AB.funspec.spec_complete_behaviors in
    let maybe_active_behaviors =
      ListLabels.filter ab.AB.funspec.spec_behavior
        ~f:(fun b -> ab.AB.is_active b <> Eval_terms.False &&
                     not (Cil.is_default_behavior b))
    in
    let def_bhv = AB.behavior_from_name ab Cil.default_behavior_name in
    (* TODO: integrate slevel *)
    let init_trace = Trace.initial kf in
    let init_state_set = State_set.singleton (with_formals, init_trace) in
    let states_after_global_requires =
      Eval_annots.check_fct_preconditions_for_behavior kf ab
        ~per_behavior:true call_kinstr init_state_set
        def_bhv
    in
    (* joined_state_after_global_requires is an overapproximation of the
       disjunction of states after the global requires clause. It is used
       in some places, but the actual disjunction is more precise and should be
       used when possible. *)
    let (joined_state_after_global_requires,trace) =
      State_set.join states_after_global_requires in
    (* Notify user about inactive behaviors *)
    Eval_annots.process_inactive_behaviors
      kf ab (joined_state_after_global_requires,trace);
    (* In order to know which behaviors will be considered by the analysis,
       we need to compute the \requires clause to eliminate empty behaviors,
       such as "assumes x < 0; requires x > 0;". Otherwise, we will later
       incorrectly consider such cases as if we had Bottom (empty state sets),
       and the narrow operator will give an incorrect result. *)
    let final_states =
      (* bhv_states_after_requires: association list (name, stateset),
         from (possibly active) behavior names to their post-requires
         sets of disjoint states. *)
      let bhv_states_after_requires =
        (* requires for default behavior already computed *)
        (Cil.default_behavior_name, states_after_global_requires) ::
        ListLabels.map maybe_active_behaviors
          ~f:(fun b -> b.b_name,
                       compute_assumes_and_requires_for_behavior
                         kf ab b call_kinstr states_after_global_requires)
      in
      let return_used = match call_kinstr with
        | Kglobal -> true
        | Kstmt {skind = Instr (Call (lv, _, _, _))} ->
          lv <> None || Value_util.postconditions_mention_result spec
        | _ -> assert false
      in
      let (b_t, b_u, b_f) =
        partition_behaviors_after_requires ab bhv_states_after_requires
      in
      (* If there are behaviors with invalid preconditions, notify the
         user. *)
      Eval_annots.process_inactive_postconds kf
        (Extlib.filter_map (fun (b,_st) -> List.mem b b_f)
            (fun (b,st) -> AB.behavior_from_name ab b, (State_set.join st))
            bhv_states_after_requires);
      (* To obtain maximum precision, we consider behaviors according to
         these rules:
         1) Inactive behaviors are never considered.
         3) All behaviors which are [True] (including the default behavior)
            have their assigns/ensures clauses computed as in the case of a
            single specification, to avoid a combinatorial explosion and to
            obtain the equivalent of a narrowed state S_t.
         4) [Unknown] behaviors are added to S_t. For each set of complete
            behaviors, we join its [Unknown] states. We obtain different
            states S_c_1, S_c_2, etc., for each set of complete states c_i.
            We then narrow these states to obtain the final result.
      *)
      let b_t,b_u =
        promote_complete_unknown_behaviors
          ab.AB.funspec.spec_complete_behaviors b_t b_u
      in
      (* If there is at least one "complete behaviors" clause, then we ignore
         the default behavior when computing a "true state"
         (intersection of True behaviors).
         Otherwise, we add the default behavior to the set of True behaviors.
      *)
      let b_t =
        if complete_bhvs_lists = [] then Cil.default_behavior_name :: b_t
        else b_t
      in
      let true_states =
        compute_merged_assigns_and_post_conds_for_behaviors
          kf ab bhv_states_after_requires return_used sclob b_t
      in
      (* If there are no "complete behaviors" clauses, we add a set
         containing the default behavior. *)
      let complete_sets =
        if complete_bhvs_lists = []
        then [[Cil.default_behavior_name]]
        else complete_bhvs_lists
      in
      (* From now on, we compute the state corresponding to the behaviors
         with status Unknown *)
      (* We only compute states for useful behaviors: those that are present
         in some of the complete_bhvs_lists and that are [Unknown] (because
         they are in the true state), plus the default behavior. *)
      let bhvs_to_compute =
        Extlib.sort_unique Pervasives.compare
          (List.filter (ListLabels.mem ~set:b_u) (List.flatten complete_sets))
      in
      let bhv_states_after_post_conds =
        List.map
          (compute_assigns_and_post_conds_for_behavior kf ab
             ~with_formals:joined_state_after_global_requires
             bhv_states_after_requires return_used sclob) bhvs_to_compute
      in
      (* For each set [c_i] of complete behaviors, compute a state set
         [stateset_per_c_i] with its unknown behaviors, then narrow the
         resulting state sets to obtain a more precise result. *)
      let stateset_per_c_i_list =
        List.map (
          Extlib.filter_map
            (fun b -> List.mem b bhvs_to_compute)
            (fun b -> List.assoc b bhv_states_after_post_conds)
          ) complete_sets
      in
      let stateset_per_c_i =
        List.map
          (fun c_i_stateset_list ->
             List.fold_left
               (fun acc_st stateset ->
                  State_set.merge stateset acc_st)
               State_set.empty c_i_stateset_list
          ) stateset_per_c_i_list in
      (* Finally, we narrow the result obtained for each set c_i of complete
         behaviors. The more sets there are, the more precise the final result
         will be. *)
      let unk_state =
        State_set.narrow_list stateset_per_c_i
      in
      (* Finally, we merge the states for the behaviors with status True
         and Unknown*)
      State_set.merge true_states unk_state
    in
    let rvi = Kernel_function.get_vi kf in
    let return_type = getReturnType rvi.vtype in
    let infer_rvi state =
      if isVoidType return_type || Cil.hasAttribute "noreturn" rvi.vattr
       || not (Cvalue.Model.is_reachable state)
      then None
      else Some (Library_functions.get_retres_vi kf)
    in
    let aux state =
      match infer_rvi state with
      | None -> None, state
      | Some vi ->
        match state with
        | Cvalue.Model.Bottom -> None, state
        | Cvalue.Model.Top -> Warn.warn_top ()
        | Cvalue.Model.Map _ ->
          let retres_base = Base.of_varinfo vi in
          let without_ret = Cvalue.Model.remove_base retres_base state in
          match Cvalue.Model.find_base retres_base state with
          | `Map m -> Some m, without_ret
          | `Bottom (*tested above*) | `Top (*state is not top*)-> assert false
    in
    { Value_types.c_values = List.map aux (State_set.to_list final_states);
      c_clobbered = sclob.Locals_scoping.clob;
      c_cacheable = Value_types.Cacheable;
      c_from = None;
    }
