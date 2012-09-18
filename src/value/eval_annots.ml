(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Eval_terms

let emit_status ppt s =
  Property_status.emit ~distinct:true Value_util.emitter_value ~hyps:[] ppt s

let emit_unreachable ppt =
  let reach_p = Property.ip_reachable_ppt ppt in
  Property_status.emit ~distinct:false Value_util.emitter_value ~hyps:[]
    reach_p Property_status.False_and_reachable


module ActiveBehaviors = struct

  let header b =
    if Cil.is_default_behavior b then ""
    else ", behavior " ^ b.b_name

  let is_active_aux init_states b =
    let assumes =
      (Logic_const.pands
         (List.map Logic_const.pred_of_id_pred b.b_assumes))
    in
    fold_join_predicate State_set.fold
      (fun init -> eval_predicate ~result:None (env_pre_f init) assumes)
      init_states

  type t = {
    init_states: State_set.t;
    funspec: funspec;
    is_active: funbehavior -> predicate_value
  }

  module HashBehaviors = Hashtbl.Make(
    struct
      type t = funbehavior
      let equal b1 b2 = b1.b_name = b2.b_name
      let hash b = Hashtbl.hash b.b_name
    end)

  let create init_states kf =
    let funspec = Annotations.funspec kf in
    let h = HashBehaviors.create 3 in
    { is_active =
        (fun b ->
           try HashBehaviors.find h b
           with Not_found ->
             let active = is_active_aux init_states b in
             HashBehaviors.add h b active;
             active
        );
      init_states = init_states;
      funspec = funspec;
    }

  let active ba = ba.is_active

  let is_active ba b = active ba b != False

  exception No_such_behavior

  let behavior_from_name ab b =
    try List.find (fun b' -> b'.b_name = b) ab.funspec.spec_behavior
    with Not_found -> raise No_such_behavior

  let active_behaviors ab =
    List.filter (is_active ab) ab.funspec.spec_behavior

  (* Is the behavior b the only one currently active. Check if it is in a
     group of complete behaviors, and the only one active in its group.
     TODO: we should also check that we can prove the 'complete' clause *)
  let only_active ab b =
    assert (is_active ab b);
    let none_other_active group =
      let other_not_active b'_name =
        b'_name = b.b_name ||
        (let b' = behavior_from_name ab b'_name in not (is_active ab b'))
      in
      List.for_all other_not_active group
    in
    try
      let complete =
        List.find (List.mem b.b_name) ab.funspec.spec_complete_behaviors
      in
      none_other_active complete
    with
        Not_found | No_such_behavior -> false

end


let check_postconditions kf kinstr ~result ~slevel header ~init_state ~active_behaviors ~post_state kind behaviors =
  let fused_init = State_set.join init_state in
  (* TODO BY: not optimal in reduce_by_disjunction below *)
  let e_post =
    lazy (env_post_f ~post:(State_set.join post_state) ~pre:fused_init)
  in
  let incorporate_behavior state b =
    if b.b_post_cond = [] then state
    else
      let header = header ^ ActiveBehaviors.header b in
      let posts = List.filter (fun (x,_) -> x = kind) b.b_post_cond in
      let update_status st post =
        let ip = Property.ip_of_ensures kf kinstr b post in
        emit_status ip st
      in
      match ActiveBehaviors.active active_behaviors b with
      | True ->
        List.fold_left
          (fun acc (_,{ip_content=pred;ip_loc=loc} as post) ->
	    let source = fst loc in
            if State_set.is_empty acc then
	      (Value_parameters.result ~once:true ~source
                 "%s: no state left to evaluate postcondition, status not computed.%t"
	         header Value_util.pp_callstack;
               acc)
            else
              let pred = Ast_info.predicate loc pred in
              let res = fold_join_predicate State_set.fold
                (fun state ->
                  let env = env_post_f ~post:state ~pre:fused_init in
                  eval_predicate ~result env pred) acc
              in
	      Value_parameters.result ~once:true ~source
                "%s: postcondition got status %a.%t"
	        header pretty_predicate_value res Value_util.pp_callstack;
              match res with
                | False ->
                    update_status Property_status.False_if_reachable post;
	            State_set.empty
                | True ->
                    update_status Property_status.True post;
                    (* The reduction is needed in the True case,
                       because the function is "reduce_by_disjunction".
                       Example: //@ assert x<0 || x>=0; *)
                    reduce_by_disjunction ~always:false
                      ~result ~env:!!e_post acc slevel pred
                | Unknown ->
                    update_status Property_status.Dont_know post;
                    reduce_by_disjunction ~always:true
                      ~result ~env:!!e_post acc slevel pred
          ) state posts
      | Unknown ->
        List.fold_left
          (fun acc (_,{ip_content=pred;ip_loc=loc} as post) ->
	    let source = fst loc in
            if State_set.is_empty acc then
	      (Value_parameters.result ~once:true ~source
                 "%s: no state left to evaluate postcondition, status not computed.%t"
	         header Value_util.pp_callstack;
               acc)
            else
              let pred = Ast_info.predicate loc pred in
              let res = fold_join_predicate State_set.fold
                (fun state ->
                  let env = env_post_f ~post:state ~pre:fused_init in
                  eval_predicate ~result env pred)
                acc
              in
              Value_parameters.result ~once:true ~source
                "%s: postcondition got status %a.%t"
                header pretty_predicate_value res Value_util.pp_callstack;
              match res with
                | Unknown | False ->
                    update_status Property_status.Dont_know post;
                    Value_parameters.result ~once:true ~source
                      "%s: postcondition got status %a, \
but it is unknown if the behavior is active.%t"
                      header pretty_predicate_value res Value_util.pp_callstack;
                    state
                | True ->
                    update_status Property_status.True post;
                    Value_parameters.result ~once:true ~source
                      "%s: postcondition got status %a, \
but it is unknown if the behavior is active.%t"
                      header pretty_predicate_value res Value_util.pp_callstack;
                    state
          ) state posts
      | False ->
        (* if assumes is false, post-condition status is not updated *)
        (match posts with
        | [] -> ()
        | (_,{ip_loc=(source,_)})::_ ->
          Value_parameters.result ~once:true ~source
            "%s: assumes got status invalid; post-condition not evaluated.%t"
            header Value_util.pp_callstack);
        state
  in
  List.fold_left incorporate_behavior post_state behaviors

let check_fct_postconditions ~result kf ~init_state ~active_behaviors 
    ~post_state kind =
  try
    let bhvs = Annotations.behaviors kf in
    let slevel = Value_util.get_slevel kf in
    check_postconditions kf Kglobal ~result ~slevel
      (Pretty_utils.sfprintf "Function %a@?" Kernel_function.pretty kf)
      ~init_state ~active_behaviors ~post_state kind bhvs
  with Not_found -> 
    post_state

let check_preconditions kf kinstr ~slevel header_text active_behaviors init_state (spec, ki_spec) =
  let env = env_pre_f (State_set.join init_state) in
  let incorporate_behavior states b =
    if b.b_requires = [] then states
    else
      let header = header_text ^ ActiveBehaviors.header b in
      let update_status st vc =
        let ip = Property.ip_of_requires kf ki_spec b vc in
        match kinstr with
          | Kglobal -> (* status of the main function. We update the global
                          status, and pray that there is no recursion.
                          TODO: check what the WP does.*)
              emit_status ip st
          | Kstmt stmt ->
              Statuses_by_call.setup_precondition_proxy kf ip;
              let ip_call = Statuses_by_call.precondition_at_call kf ip stmt in
              emit_status ip_call st
      in
      match ActiveBehaviors.active active_behaviors b with
      | True ->
        List.fold_left (fun state ({ip_content=pr;ip_loc=loc} as pre) ->
	  let source = fst loc in
          if State_set.is_empty state then
	    (Value_parameters.result ~once:true ~source
               "%s: no state in which to evaluate precondition, status not computed.%t"
	       header Value_util.pp_callstack;
             state)
          else
            let pr = Ast_info.predicate loc pr in
            let res = fold_join_predicate State_set.fold
              (fun state ->
                eval_predicate ~result:None (env_pre_f state) pr)
              state
            in
	    Value_parameters.result ~once:true ~source
              "%s: precondition got status %a.%t"
              header pretty_predicate_value res Value_util.pp_callstack;
            match res with
              | False ->
                  update_status Property_status.False_if_reachable pre;
                  State_set.empty
              | True ->
                  update_status Property_status.True pre;
                  (* The reduction is needed in the True case,
                     because the function is "reduce_by_disjunction".
                     Example: //@ assert x<0 || x>=0; *)
                  reduce_by_disjunction ~always:false
                    ~result:None ~env state slevel pr
              | Unknown ->
                  update_status Property_status.Dont_know pre;
                  reduce_by_disjunction  ~always:true
                    ~result:None ~env state slevel pr
        ) states b.b_requires
      | Unknown ->
        List.fold_left
          (fun state ({ip_content=pr;ip_loc=loc} as pre) ->
	    let source = fst loc in
            if State_set.is_empty state then
	      (Value_parameters.result ~once:true ~source
                 "%s: no state in which to evaluate precondition, status not computed.%t"
	         header Value_util.pp_callstack;
               state)
            else
              let pr = Ast_info.predicate loc pr in
              let res = fold_join_predicate State_set.fold
                (fun state ->
                  eval_predicate ~result:None (env_pre_f state) pr)
                state
              in
              Value_parameters.result ~once:true ~source:(fst loc)
                "%s: precondition got status %a.%t"
                header pretty_predicate_value res Value_util.pp_callstack;
              match res with
                | Unknown | False ->
                    update_status Property_status.Dont_know pre;
                    Value_parameters.result ~once:true ~source
                      "%s: precondition got status %a, \
but it is unknown if the behavior is active.%t"
                      header pretty_predicate_value res Value_util.pp_callstack;
                    state
                | True ->
                    update_status Property_status.True pre;
                    Value_parameters.result ~once:true ~source
		      "%s: precondition got status %a.%t"
		      header pretty_predicate_value res Value_util.pp_callstack;
                    state
          ) states b.b_requires
      | False ->
        (match b.b_requires with
        | [] -> ()
        | {ip_loc=(source,_)}::_ ->
          Value_parameters.result ~once:true ~source
            "%s: assumption got status invalid; precondition not evaluated.%t"
            header Value_util.pp_callstack);
        states
  in
  List.fold_left
    incorporate_behavior
    init_state spec.spec_behavior

(** Check the precondition of [kf]. This may result in splitting [init_state]
    into multiple states if the precondition contains disjunctions. The active
    behaviors are computed wrt [init_state], but further computations on [kf]
    will use active behaviors computed wrt the result of this function. *)
let check_fct_preconditions kf ki init_state =
  let init_states = State_set.singleton init_state in
  try
    let spec = Annotations.funspec kf in
    let slevel = Value_util.get_slevel kf in
    let active_behaviors = ActiveBehaviors.create init_states kf in
    check_preconditions kf ki ~slevel
      (Pretty_utils.sfprintf "Function %a@?" Kernel_function.pretty kf)
      active_behaviors init_states (spec, Kglobal)
  with Not_found -> init_states



(* Reduce the given states according to the given code annotations.
   If [record] is true, update the proof state of the code annotation.
   DO NOT PASS record=false unless you known what your are doing *)
let interp_annot kf initial_state slevel active_behaviors states stmt ca record =
  let aux text behav p =
    let in_behavior =
      match behav with
        | [] -> `True
        | behavs ->
          let ab = active_behaviors in
          let all_active = Extlib.filter_map'
            (ActiveBehaviors.behavior_from_name ab)
            (ActiveBehaviors.is_active ab)
            behavs
          in
          if all_active = [] then `False
          else
            let active = ActiveBehaviors.only_active ab in
            if List.exists active all_active then `True else `Unknown
    in
    match in_behavior with
    | `False -> states
    | `True | `Unknown as in_behavior ->
      let result = fold_join_predicate State_set.fold
        (fun here ->
          let env = env_annot ~pre:initial_state ~here in
          eval_predicate ~result:None env p)
        states
      in
      let ip = Property.ip_of_code_annot kf stmt ca in
      let change_status st =
        if record then List.iter (fun p -> emit_status p st) ip
      in
      let message, states =
        (match result, in_behavior with
          | Unknown, _ ->
              change_status Property_status.Dont_know;
              "unknown", states
          | True, _ ->
              change_status Property_status.True;
              "valid", states
          | False, `True ->
              change_status Property_status.False_if_reachable;
              "invalid (stopping propagation)", State_set.empty
          | False, `Unknown ->
              change_status Property_status.False_if_reachable;
              "invalid", states
        )
      in
      if record then 
	Value_parameters.result ~once:true ~current:true
	  "%s got status %s.%t" text message Value_util.pp_callstack;
      if in_behavior = `True then
        let env = env_annot ~pre:initial_state
          ~here:(State_set.join states) in
        reduce_by_disjunction ~always:(result = Unknown)
          ~result:None ~env states slevel p
      else
	states
  in
  match ca.annot_content with
    | AAssert (behav,p) -> aux "Assertion" behav p
    | AInvariant (behav, true, p) -> aux "Loop invariant" behav p
    | APragma _
    | AInvariant (_, false, _)
    | AVariant _ | AAssigns _ | AAllocation _
    | AStmtSpec _ (*TODO*) -> states


let mark_unreachable () =
  let do_stmt stmt _emit rca =
    if not (Db.Value.is_reachable_stmt stmt) then
      let kf = Kernel_function.find_englobing_kf stmt in
      let ca = Annotations.code_annotation_of_rooted rca in
      let ppts = Property.ip_of_code_annot kf stmt ca in
      List.iter (fun p ->
        Value_parameters.debug "Marking property %a as dead"
          Description.pp_property p;
        emit_unreachable p
      ) ppts
  in
  Annotations.iter_all_code_annot do_stmt

let mark_rte () =
  let _, mem, _ = !Db.RteGen.get_memAccess_status () in
  let _, arith, _ = !Db.RteGen.get_divMod_status () in
  let _, signed_ovf, _ = !Db.RteGen.get_signedOv_status () in
  Globals.Functions.iter
    (fun kf ->
      if !Db.Value.is_called kf then (
        mem kf true;
        arith kf true;
        signed_ovf kf true;
      )
    )



let () =
  Db.Value.valid_behaviors :=
    (fun kf state ->
      let ab = ActiveBehaviors.create (State_set.singleton state) kf in
      ActiveBehaviors.active_behaviors ab
    );

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
