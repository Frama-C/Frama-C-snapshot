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
open Eval

module AB = Transfer_logic.ActiveBehaviors

module LogicDomain = struct
  include Cvalue.Model
  type state = t
  let join_and_is_included a b = let r = join a b in r, equal r b
  let widen kf stmt a b =
    let hint = Widen.getWidenHints kf stmt in
    widen hint a b

  (* Evaluation environment. *)
  type eval_env = Eval_terms.eval_env
  let env_current_state env =
    let t = Eval_terms.env_current_state env in
    if is_reachable t then `Value t else `Bottom
  let env_annot ~pre ~here () = Eval_terms.env_annot ~pre ~here ()
  let env_pre_f ~pre () = Eval_terms.env_pre_f ~pre ()
  let env_post_f ~pre ~post ~result () =
    Eval_terms.env_post_f ~pre ~post ~result ()
  let eval_predicate env pred =
    match Eval_terms.eval_predicate env pred with
    | Eval_terms.True -> Alarmset.True
    | Eval_terms.False -> Alarmset.False
    | Eval_terms.Unknown -> Alarmset.Unknown
  let reduce_by_predicate env b pred = Eval_terms.reduce_by_predicate env b pred
end

module States = struct
  include Powerset.Make (LogicDomain)
  let singleton state =
    if Cvalue.Model.is_reachable state
    then singleton state
    else empty
  let add state states =
    if Cvalue.Model.is_reachable state
    then add state states
    else states
end
module Logic = Transfer_logic.Make (LogicDomain) (States)

(* Computes [narrow] with all the state sets in [stl].
   [stl] must not be empty.  *)
let narrow_states_list stl =
  let stl = List.map (fun s -> States.to_list s) stl in
  let join_list acc list = match list with
    | [] -> acc
    | hd :: tl -> (List.fold_left Cvalue.Model.join hd tl) :: acc
  in
  let s = List.fold_left join_list [] stl in
  let snarrow =
    List.fold_left Cvalue.Model.narrow Cvalue.Model.top s
  in
  List.fold_left
    (fun acc st ->
       let narrowed_st = List.map (fun s -> Cvalue.Model.narrow s snarrow) st in
       let states = States.of_list narrowed_st in
       fst (States.merge ~into:states acc)) States.empty stl


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
        Cvalue.V.join acc (Cvalue.V.topify_leaf_origin r.Eval_terms.eover)
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
            Value_util.warning_once_current
              "cannot interpret@ 'from' clause@ '%a'@ of function %a%a"
              Printer.pp_from asgn
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
            Cvalue.Model.add_binding ~exact:false acc loc froms_contents
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
          Value_util.warning_once_current
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
          Value_util.warning_once_current
            "Cannot handle empty assigns clause. Assuming assigns \\nothing: \
             be aware this is probably incorrect.";
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
      match Library_functions.get_retres_vi kf with
      | None -> None, state
      | Some retres_vi ->
        let return_type = Cil.getReturnType vi.vtype in
        let offsetmap = Eval_op.offsetmap_of_v return_type !returned_value in
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

let compute_assigns_and_post_conds_for_behavior kf ab ~with_formals
    bhv_states_after_requires return_used sclob b_name =
  let b = AB.behavior_from_name ab b_name in
  let states_after_requires = List.assoc b_name bhv_states_after_requires in
  let retres_vi = ref None in
  let states_after_assigns =
    States.fold (fun state acc ->
        let rvi, state_after_assigns =
          let assigns = get_assigns_for_behavior ab b in
          compute_assigns kf assigns return_used sclob state true
        in
        retres_vi := join_rvi !retres_vi rvi;
        States.add state_after_assigns acc
      ) states_after_requires States.empty
  in
  let states_after_post_conds =
    Logic.check_fct_postconditions_for_behaviors kf ab [b] Normal
      ~result:!retres_vi ~per_behavior:true ~pre_state:with_formals
      ~post_states:states_after_assigns
  in
  (b_name, states_after_post_conds)

(* When there is at least one behavior whose active status is [True], we can
   perform the intersection of the states and assigns clauses, and compute the
   result for every [True] state at once. Here, [b_names] is a list of True
   behaviors. *)
let compute_merged_assigns_and_post_conds_for_behaviors kf ab
    bhv_states_after_requires return_used sclob b_names =
  if b_names = [] then `Value States.empty
  else
    let bs = List.map (AB.behavior_from_name ab) b_names in
    let states_after_requires_list =
      Extlib.filter_map (fun (b_name, _) -> List.mem b_name b_names) snd
        bhv_states_after_requires
    in
    States.join (narrow_states_list  states_after_requires_list)
    >>-: fun state_after_requires ->
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
    Logic.check_fct_postconditions_for_behaviors kf ab bs Normal
      ~per_behavior:true ~result:!retres_vi
      ~pre_state:state_after_requires
      ~post_states:(States.singleton state_after_assigns)

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
      if States.is_empty stateset then
        (* falsely active behavior: requires clauses not satisfied *)
        (b_t0, b_u0, b_name :: b_f0)
      else
        (* requires clauses did not change the behavior's status *)
        match AB.is_active ab (AB.behavior_from_name ab b_name) with
        | Alarmset.True -> (b_name :: b_t0, b_u0, b_f0)
        | Alarmset.Unknown -> (b_t0, b_name :: b_u0, b_f0)
        | Alarmset.False -> (b_t0, b_u0, b_name :: b_f0)
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

(* Reduce the state by the assumes and requires clauses for behavior [b],
   and emit statuses for the requires. *)
let compute_assumes_and_requires_for_behavior kf ab b call_kinstr states =
  let states_after_assumes = Logic.reduce_by_assumes_of_behavior kf b states in
  Logic.check_fct_preconditions_for_behavior kf ab ~per_behavior:true
    call_kinstr states_after_assumes b

let compute_using_specification call spec ~call_kinstr ~with_formals =
  let kf = call.kf in
  let ab = Logic.create_from_spec with_formals spec in
  let sclob = Locals_scoping.bottom () in
  let complete_bhvs_lists = spec.spec_complete_behaviors in
  let maybe_active_behaviors =
    ListLabels.filter spec.spec_behavior
      ~f:(fun b -> AB.is_active ab b <> Alarmset.False &&
                   not (Cil.is_default_behavior b))
  in
  let def_bhv = AB.behavior_from_name ab Cil.default_behavior_name in
  (* TODO: integrate slevel *)
  let init_state_set = States.singleton with_formals in
  let states_after_global_requires =
    Logic.check_fct_preconditions_for_behavior kf ab
      ~per_behavior:true call_kinstr init_state_set
      def_bhv
  in
  (* state_after_global_requires is an overapproximation of the
     disjunction of states after the global requires clause. It is used
     in some places, but the actual disjunction is more precise and should be
     used when possible. *)
  let state_after_global_requires = States.join states_after_global_requires in
  (* Notify user about inactive behaviors *)
  Transfer_logic.process_inactive_behaviors kf call_kinstr ab;
  state_after_global_requires >>- fun state_after_global_requires ->
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
      | Kstmt {skind = Instr (Local_init(_,ConsInit(_,_,Constructor),_)) } ->
        Value_util.postconditions_mention_result spec
      | Kstmt {skind=Instr(Local_init(_,ConsInit(_,_,Plain_func),_))} -> true
      | _ -> assert false
    in
    let (b_t, b_u, b_f) =
      partition_behaviors_after_requires ab bhv_states_after_requires
    in
    (* If there are behaviors with invalid preconditions, notify the
       user. *)
    Transfer_logic.process_inactive_postconds kf
      (Extlib.filter_map (fun (b,_st) -> List.mem b b_f)
         (fun (b,_st) -> AB.behavior_from_name ab b)
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
    let b_t, b_u =
      promote_complete_unknown_behaviors spec.spec_complete_behaviors b_t b_u
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
    compute_merged_assigns_and_post_conds_for_behaviors
      kf ab bhv_states_after_requires return_used sclob b_t
    >>-: fun true_states ->
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
           ~with_formals:state_after_global_requires
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
                fst (States.merge stateset acc_st))
             States.empty c_i_stateset_list
        ) stateset_per_c_i_list in
    (* Finally, we narrow the result obtained for each set c_i of complete
       behaviors. The more sets there are, the more precise the final result
       will be. *)
    let unk_state = narrow_states_list stateset_per_c_i in
    (* Finally, we merge the states for the behaviors with status True
       and Unknown*)
    fst (States.merge true_states unk_state)
  in
  final_states >>-: fun final_states ->
  let rvi = Kernel_function.get_vi kf in
  let return_type = Cil.getReturnType rvi.vtype in
  let infer_rvi state =
    if Cil.isVoidType return_type || Cil.hasAttribute "noreturn" rvi.vattr
       || not (Cvalue.Model.is_reachable state)
    then None
    else call.return
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
        | `Value m -> Some m, without_ret
        | `Bottom (*tested above*) | `Top (*state is not top*)-> assert false
  in
  { Value_types.c_values = List.map aux (States.to_list final_states);
    c_clobbered = sclob.Locals_scoping.clob;
    c_cacheable = Value_types.Cacheable;
    c_from = None;
  }


(* Eval: under-approximation of the term.  Note that ACSL states
   that assigns clauses are evaluated in the pre-state.
   We skip [\result]: it is meaningless when evaluating the 'assigns' part,
   and a special treatment must be done in [from] clauses anyway. *)
let eval_assigns_from pre_state it =
  let term = it.it_content in
  if Logic_utils.is_result it.it_content then
    Locations.Zone.bottom
  else
    let eval_env = Eval_terms.env_assigns pre_state in
    fst (Eval_terms.eval_tlval_as_zone_under_over
           ~with_alarms:CilE.warn_none_mode ~for_writing:false eval_env term)

(** Compute the validity status for [from] in [pre_state], assuming the
    entire clause is [assigns asgn \from from]. The inferred dependencies
    are [found_froms], while [asgn] evaluates to [assigns_zone]. *)
let check_from pre_state asgn assigns_zone from found_froms =
  let open Locations in
  let found_deps =
    let open Function_Froms in
    if Logic_utils.is_result asgn.it_content then
      found_froms.deps_return
    else
      Memory.find_precise found_froms.deps_table assigns_zone
  in
  let (indirect_deps,direct_deps) =
    let filter x = List.mem "indirect" x.it_content.term_name in
    List.partition filter from
  in
  (* Under-approximation of the union. *)
  let link zones = List.fold_left Zone.link Zone.bottom zones in
  let eval = eval_assigns_from pre_state in
  let stated_indirect_deps = link (List.map eval indirect_deps) in
  let stated_direct_deps = link (List.map eval direct_deps) in
  let found_direct_deps = found_deps.Function_Froms.Deps.data in
  let found_indirect_deps = found_deps.Function_Froms.Deps.indirect in
  let res_for_unknown txt =
    Value_parameters.debug "found_direct deps %a stated_direct_deps %a \
                            found_indirect_deps %a stated_indirect_deps %a"
      Zone.pretty found_direct_deps Zone.pretty stated_direct_deps
      Zone.pretty found_indirect_deps Zone.pretty stated_indirect_deps;
    "unknown (cannot validate "^txt^" dependencies)",
    Alarmset.Unknown
  in
  match (Zone.is_included found_direct_deps stated_direct_deps,
         Zone.is_included found_indirect_deps stated_indirect_deps) with
  | true,true -> "valid", Alarmset.True
  | false,true -> res_for_unknown "direct"
  | false,false -> res_for_unknown "direct and indirect"
  | true,false -> res_for_unknown "indirect"


(* Emits a status and a notification message. Returns the message callback. *)
let emit_status ppt status =
  Property_status.emit ~distinct:true Value_util.emitter ~hyps:[] ppt status


(* Display the message as result/warning depending on [status] *)
let msg_status status ?current ?once ?source fmt =
  if status = Alarmset.True then
    if Value_parameters.ValShowProgress.get ()
    then Value_parameters.result ?current ?once ?source fmt
    else Value_parameters.result ?current ?once ?source ~level:2 fmt
  else
  if Value_parameters.AlarmsWarnings.get () then
    Value_parameters.warning ?current ?once ?source fmt
  else
    Value_parameters.result
      ?current ?once ?source ~dkey:Value_parameters.dkey_alarm fmt


let pp_bhv fmt b =
  if not (Cil.is_default_behavior b)
  then Format.fprintf fmt ", behavior %s" b.b_name

let pp_header kf fmt b =
  Format.fprintf fmt "function %a%a"
    Kernel_function.pretty kf pp_bhv b


let conv_status = function
  | Alarmset.False -> Property_status.False_if_reachable;
  | Alarmset.True -> Property_status.True;
  | Alarmset.Unknown -> Property_status.Dont_know


let check_fct_assigns kf ab ~pre_state found_froms =
  let open Locations in
  let open Alarmset in
  let behaviors = Annotations.behaviors kf in
  (* Under-approximation of the union. *)
  let link zones = List.fold_left Zone.link Zone.bottom zones in
  let outputs = Function_Froms.outputs found_froms in
  let check_for_behavior b =
    let activity = AB.is_active ab b in
    match activity with
    | False -> ()
    | True | Unknown ->
      let pp_activity fmt activity = match activity with
        | False -> assert false
        | True -> ()
        (* If unknown, the error may be because we did not notice
           that the behavior is inactive.  *)
        | Unknown -> Format.fprintf fmt "(the behavior may be inactive)"
      in
      (match b.b_assigns with
       | WritesAny -> ()
       | Writes(assigns_deps) ->
         let bol = Property.Id_contract (Datatype.String.Set.empty,b) in
         let ip = Extlib.the (Property.ip_of_assigns kf Kglobal bol b.b_assigns)
         in
         let source = fst (Property.location ip) in
         (* First, check the assigns. *)
         let assigns = List.map fst assigns_deps in
         let assigns_zones = List.map (eval_assigns_from pre_state) assigns in
         let assigns_union = link assigns_zones in
         let status_txt, vstatus, status =
           if not (Zone.is_included outputs assigns_union)
           then (
             Value_parameters.debug "found_assigns %a stated_assigns %a"
               Zone.pretty outputs Zone.pretty assigns_union;
             "unknown", Unknown, Property_status.Dont_know)
           else "valid", True, Property_status.True
         in
         msg_status vstatus ~once:true ~source
           "%a: assigns got status %s.%a%t"
           (pp_header kf) b
           status_txt
           pp_activity activity
           Value_util.pp_callstack;
         emit_status ip status;
         (* Now, checks the individual froms. *)
         let check_from ((asgn,deps) as from) assigns_zone =
           match deps with
           | FromAny -> ()
           | From deps ->
             let status_txt, status =
               check_from pre_state asgn assigns_zone deps found_froms
             in
             let ip = Extlib.the (Property.ip_of_from kf Kglobal bol from) in
             let source = fst (asgn.it_content.term_loc) in
             msg_status status ~once:true ~source
               "%a: \\from ... part in assign clause got status %s.%a%t"
               (pp_header kf) b
               status_txt
               pp_activity activity
               Value_util.pp_callstack;
             emit_status ip (conv_status status)
         in
         List.iter2 check_from assigns_deps assigns_zones)
  in List.iter check_for_behavior behaviors

let verify_assigns_from kf ~pre froms =
  let ab = Logic.create pre kf in
  check_fct_assigns kf ab ~pre_state:pre froms;;


Db.Value.verify_assigns_froms := verify_assigns_from;;
