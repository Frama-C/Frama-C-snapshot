(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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
  Property_status.emit ~distinct:true Value_util.emitter ~hyps:[] ppt s

let emit_unreachable ppt =
  let reach_p = Property.ip_reachable_ppt ppt in
  Property_status.emit ~distinct:false Value_util.emitter ~hyps:[]
    reach_p Property_status.False_and_reachable


module ActiveBehaviors = struct

  let header b =
    if Cil.is_default_behavior b then ""
    else ", behavior " ^ b.b_name


  let pp_bhv fmt b =
    if not (Cil.is_default_behavior b)
    then Format.fprintf fmt ", behavior %s" b.b_name

  let is_active_aux init_state b =
    let assumes =
      (Logic_const.pands
         (List.map Logic_const.pred_of_id_pred b.b_assumes))
    in
    eval_predicate (env_pre_f ~init:init_state ()) assumes

  type t = {
    init_state: Cvalue.Model.t;
    funspec: funspec;
    is_active: funbehavior -> predicate_value
  }

  module HashBehaviors = Hashtbl.Make(
    struct
      type t = funbehavior
      let equal b1 b2 = b1.b_name = b2.b_name
      let hash b = Hashtbl.hash b.b_name
    end)

  let create_from_spec init_state funspec =
    let h = HashBehaviors.create 3 in
    { is_active =
        (fun b ->
           try HashBehaviors.find h b
           with Not_found ->
             let active = is_active_aux init_state b in
             HashBehaviors.add h b active;
             active
        );
      init_state = init_state;
      funspec = funspec;
    }

  let create init_state kf =
    let funspec = Annotations.funspec kf in
    create_from_spec init_state funspec

  let active ba = ba.is_active

  let is_active ba b = active ba b != False

  exception No_such_behavior

  let behavior_from_name ab b =
    try List.find (fun b' -> b'.b_name = b) ab.funspec.spec_behavior
    with Not_found -> raise No_such_behavior

  let active_behaviors ab =
    List.filter (is_active ab) ab.funspec.spec_behavior

end

let conv_status = function
  | False -> Property_status.False_if_reachable;
  | True -> Property_status.True;
  | Unknown -> Property_status.Dont_know

let behavior_inactive fmt =
  Format.fprintf fmt " (Behavior may be inactive, no reduction performed.)"

let pp_header kf fmt b =
  Format.fprintf fmt "Function %a%a"
    Kernel_function.pretty kf ActiveBehaviors.pp_bhv b

(** [eval_and_reduce preds ab b proj states update_status pp_header env slevel ab b] *)
    
let eval_and_reduce ab b preds states update_status env slevel pp_header str =
  let aux_pred reduce states ({ip_content=pr; ip_loc= (source, _ as loc)} as pred) =
    if State_set.is_empty states then
      (Value_parameters.result ~once:true ~source
         "%a: no state left in which to evaluate %s, status not \
         computed.%t" pp_header b str Value_util.pp_callstack;
       states)
    else
      let pr = Ast_info.predicate loc pr in
      let res = fold_join_predicate State_set.fold
        (fun state -> eval_predicate (env state) pr)
        states
      in
      Value_parameters.result ~once:true ~source
        "%a: %s got status %a.%t%t"
        pp_header b str pretty_predicate_value res
        (if reduce then (fun _ -> ()) else behavior_inactive)
        Value_util.pp_callstack;
      update_status (conv_status res) pred;
      if reduce then
        match res with
          | False ->
              State_set.empty
          | True ->
              let env = env (State_set.join states) in
              (* Reduce in case [pre] is a disjunction *)
              reduce_by_disjunction ~always:false ~env states slevel pr
          | Unknown ->
              let env = env (State_set.join states) in
              (* Reduce in all cases *)
              reduce_by_disjunction  ~always:true ~env states slevel pr
      else
        states
  in
  match ActiveBehaviors.active ab b with
    | True -> List.fold_left (aux_pred true) states preds
    | Unknown -> List.fold_left (aux_pred false) states preds
    | False ->
        (match preds with
           | [] -> ()
           | {ip_loc=(source,_)} ::_ ->
               Value_parameters.result ~once:true ~source
                 "%a: assumes got status invalid; %s not evaluated.%t"
                 pp_header b str Value_util.pp_callstack
        );
        states

let check_fct_postconditions kf ab ~result ~init_state ~post_states kind =
  let behaviors = Annotations.behaviors kf in
  let slevel = Value_util.get_slevel kf in
  let incorporate_behavior states b =
    if b.b_post_cond = [] then states
    else
      let posts = List.filter (fun (x,_) -> x = kind) b.b_post_cond in
      let posts = List.map snd posts in
      let update_status st post =
        let ip = Property.ip_of_ensures kf Kglobal b (kind, post) in
        emit_status ip st
      in
      let env state = env_post_f ~post:state ~pre:init_state ~result () in
      eval_and_reduce ab b posts states update_status env slevel (pp_header kf) "postcondition"
  in
  List.fold_left incorporate_behavior post_states behaviors

(** Check the precondition of [kf]. This may result in splitting [init_state]
    into multiple states if the precondition contains disjunctions. *)
let check_fct_preconditions kf ab call_ki init_state =
  let init_states = State_set.singleton init_state in
  let spec = Annotations.funspec kf in
  let slevel = Value_util.get_slevel kf in
  let incorporate_behavior states b =
    if b.b_requires = [] then states
    else
      let emit st vc =
        let ip = Property.ip_of_requires kf Kglobal b vc in
        match call_ki with
          | Kglobal -> (* status of the main function. We update the global
                          status, and pray that there is no recursion.
                          TODO: check what the WP does.*)
              emit_status ip st
          | Kstmt stmt ->
              Statuses_by_call.setup_precondition_proxy kf ip;
              let ip_call = Statuses_by_call.precondition_at_call kf ip stmt in
              emit_status ip_call st
      in
      eval_and_reduce ab b b.b_requires
        states emit (fun init -> env_pre_f ~init ()) slevel
        (pp_header kf) "precondition"
  in
  List.fold_left incorporate_behavior init_states spec.spec_behavior


(* Reduce the given states according to the given code annotations.
   If [record] is true, update the proof state of the code annotation.
   DO NOT PASS record=false unless you known what your are doing *)
let interp_annot kf ab initial_state slevel states stmt ca record =
  let source = match Cil_datatype.Code_annotation.loc ca with
    | Some loc when not (Cil_datatype.Location.equal
                           loc Cil_datatype.Location.unknown)
        -> fst loc
    | _ -> fst (Cil.CurrentLoc.get ()) (* fallback: current statement *)
  in
  let aux text behav p =
    let in_behavior =
      match behav with
        | [] -> `True
        | behavs ->
          let aux acc b =
            let b = ActiveBehaviors.behavior_from_name ab b in
            match ActiveBehaviors.active ab b with
              | True -> `True
              | Unknown -> if acc = `True then `True else `Unknown
              | False -> acc
          in
          List.fold_left aux `False behavs
    in
    match in_behavior with
    | `False -> states
    | `True | `Unknown as in_behavior ->
      let result = fold_join_predicate State_set.fold
        (fun here ->
          let env = env_annot ~pre:initial_state ~here () in
          eval_predicate env p)
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
	Value_parameters.result ~once:true ~source
	  "%s got status %s." text message;
      if in_behavior = `True then
        let here = State_set.join states in
        let env = env_annot ~pre:initial_state ~here () in
        reduce_by_disjunction ~always:(result = Unknown) ~env states slevel p
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
  let do_stmt stmt _emit ca =
    if not (Db.Value.is_reachable_stmt stmt) then
      let kf = Kernel_function.find_englobing_kf stmt in
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
  let _, unsigned_ovf, _ = !Db.RteGen.get_unsignedOv_status () in
  let signed = Kernel.SignedOverflow.get () in
  let unsigned = Kernel.UnsignedOverflow.get () in
  Globals.Functions.iter
    (fun kf ->
      if !Db.Value.is_called kf then (
        mem kf true;
        arith kf true;
        signed_ovf kf signed;
        unsigned_ovf kf unsigned;
      )
    )

let () =
  Db.Value.valid_behaviors :=
    (fun kf state ->
      let ab = ActiveBehaviors.create state kf in
      ActiveBehaviors.active_behaviors ab
    );

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
