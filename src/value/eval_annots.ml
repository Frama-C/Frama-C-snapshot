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
open Eval_terms

(** Statuses for code annotations and function contracts *)

let emit_status ppt status =
  Property_status.emit ~distinct:true Value_util.emitter ~hyps:[] ppt status

let notify_status ppt status state =
  Value_messages.new_status ppt status state
;;

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
    is_active: funbehavior -> predicate_status
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

(* Does the given function has any requires to evaluate. Use kf.spec as
   a shortcut. *)
let has_requires kf =
  let behav_has_requires b = b.b_requires <> [] in
  List.exists behav_has_requires kf.spec.spec_behavior


let conv_status = function
  | False -> Property_status.False_if_reachable;
  | True -> Property_status.True;
  | Unknown -> Property_status.Dont_know

let behavior_inactive fmt =
  Format.fprintf fmt " (Behavior may be inactive, no reduction performed.)"

let pp_header kf fmt b =
  Format.fprintf fmt "Function %a%a"
    Kernel_function.pretty kf ActiveBehaviors.pp_bhv b


(* The function that puts statuses on pre- and post-conditions is essentially
   agnostic as to which kind of property it operates on. However, the messages
   that get emitted are quite different. The types below distinguish between
   the various possibilities. *)
type postcondition_kf_kind =
| PostLeaf (* The function has no body in the AST *)
| PostBody (* The function has a body, which is used for the evaluation *)
| PostUseSpec (* The function has a body, but its specification is used
                 instead *)
and pre_post_kind = Precondition | Postcondition of postcondition_kf_kind

let pp_pre_post_kind fmt = function
  | Precondition -> Format.pp_print_string fmt "precondition"
  | Postcondition _ -> Format.pp_print_string fmt "postcondition"

let post_kind kf =
  if !Db.Value.use_spec_instead_of_definition kf then
    if Kernel_function.is_definition kf then
      PostUseSpec
    else
      PostLeaf
  else
    PostBody

(* [eval_and_reduce_pre_post kf ab b pre_post ips states build_prop build_env]
   evaluates the pre- or post-conditions [ips] of [kf] in the states [states].
   The states are used simultaneously for evaluation and reduction: if one
   predicate is not valid in one of the states, the status of the predicate is
   set to [Unknown] or [Invalid]. In this case, the state is simultaneously
   reduced (when possible).
   [pre_post] indicates the kind of clause (pre or post) being evaluated.
   [ab] and [b] give respectively the statuses of the behaviors of the function
     (active or not), and the current behavior.
   [build_prop] builds the [Property.t] that corresponds to the pre/post being
     evaluated.
   [build_env] is used to build the environment evaluation, in particular
     the pre- and post-states. *)
let eval_and_reduce_pre_post kf ab b pre_post ips states build_prop build_env =
  let pp_header = pp_header kf in
  let slevel = Value_util.get_slevel kf in
  let aux_pred behav_active states pred =
    let pr = Logic_utils.named_of_identified_predicate pred in
    let source = fst pr.loc in
    if State_set.is_empty states then
      (Value_parameters.result ~once:true ~source
         "%a: no state left in which to evaluate %a, status%a not \
         computed.%t" pp_header b pp_pre_post_kind pre_post
         Description.pp_named pr Value_util.pp_callstack;
       states)
    else
      let ip = build_prop pred in
      let (statuses, reduced_stateset) =
        State_set.fold 
          (fun (accres, accstateset) (state, _trace as stt) ->
            let env = build_env state in
	    let res = eval_predicate env pr in
	    notify_status ip (conv_status res) state;
	    let reduced_states = 
	      if behav_active then
                match res with
                | False -> 
		  State_set.empty
                | True ->
                (* Reduce in case [pre] is a disjunction *)
		  split_disjunction_and_reduce
                    ~reduce:false ~env stt ~slevel pr ip
                | Unknown ->
                (* Reduce in all cases *)
		  split_disjunction_and_reduce
                    ~reduce:true ~env stt ~slevel pr ip
	      else
		State_set.singleton stt
	    in 
	    (res::accres, State_set.merge reduced_states accstateset)
	  ) ([], State_set.empty) states 
      in
      let res = join_list_predicate_status statuses in
      begin match pre_post with
      | Precondition | Postcondition PostBody ->
        Value_parameters.result ~once:true ~source
          "%a: %a%a got status %a.%t%t"
          pp_header b pp_pre_post_kind pre_post Description.pp_named pr
          pretty_predicate_status res
          (if behav_active then (fun _ -> ()) else behavior_inactive)
          Value_util.pp_callstack;
        emit_status ip (conv_status res);
      | Postcondition (PostLeaf | PostUseSpec as postk) ->
        (* Do not display anything for postconditions of leaf functions that
           receive status valid (very rare) or unknown: this brings no
           information. However, warn the user if the status is invalid.
           (unless this is on purpose, using [assert \false]) *)
        let pp_behavior_inactive fmt =
          Format.fprintf fmt ",@ the behavior@ was@ inactive"
        in
        if res = False && pred.ip_content <> Pfalse then
          Value_parameters.result ~once:true ~source
            "@[%a:@ this postcondition@ evaluates to@ false@ in this@ context.\
                @ If it is valid,@ either@ a precondition@ was not@ verified@ \
                for this@ call%t,@ or some assigns/from@ clauses@ are \
                incomplete@ (or incorrect).@]%t"
            pp_header b
            (if behav_active then (fun _ -> ()) else pp_behavior_inactive)  
            Value_util.pp_callstack;
        (* Only emit a status if the function has a body. Otherwise, we would
           overwite the "considered valid" status of the kernel. *)
        if postk = PostUseSpec then
          emit_status ip (conv_status res);
      end;
      reduced_stateset
  in
  match ActiveBehaviors.active ab b with
    | True -> List.fold_left (aux_pred true) states ips
    | Unknown -> List.fold_left (aux_pred false) states ips
    | False ->
        (match ips with
           | [] -> ()
           | {ip_loc=(source,_)} ::_ ->
               Value_parameters.result ~once:true ~source
                 "%a: assumes got status invalid; %a not evaluated.%t"
                 pp_header b pp_pre_post_kind pre_post Value_util.pp_callstack
        );
        states

let check_fct_postconditions kf ab ~result ~init_state ~post_states kind =
  let behaviors = Annotations.behaviors kf in
  let incorporate_behavior states b =
    if b.b_post_cond = [] then states
    else
      let posts = List.filter (fun (x,_) -> x = kind) b.b_post_cond in
      let posts = List.map snd posts in
      let build_prop post =
        Property.ip_of_ensures kf Kglobal b (kind, post)
      in
      let build_env state = env_post_f ~post:state ~pre:init_state ~result () in
      eval_and_reduce_pre_post
        kf ab b (Postcondition (post_kind kf)) posts states build_prop build_env
  in
  List.fold_left incorporate_behavior post_states behaviors

(** Check the precondition of [kf]. This may result in splitting [init_state]
    into multiple states if the precondition contains disjunctions. *)
let check_fct_preconditions kf ab call_ki init_state =
  let init_trace = Trace.initial kf in
  let init_states = State_set.singleton (init_state, init_trace) in
  let spec = Annotations.funspec kf in
  let incorporate_behavior states b =
    if b.b_requires = [] then states
    else
      let build_prop pre =
	let ip_precondition = Property.ip_of_requires kf Kglobal b pre in
        match call_ki with
          | Kglobal -> (* status of the main function. We update the global
                          status, and pray that there is no recursion.
                          TODO: check what the WP does.*)
            ip_precondition
          | Kstmt stmt ->
	      (* Status is set on the copy of the precondition on the call point [stmt]. *)
              Statuses_by_call.setup_precondition_proxy kf ip_precondition;
              let ip_call = Statuses_by_call.precondition_at_call kf ip_precondition stmt in
	      ip_call
      in
      let build_env init = env_pre_f ~init () in
      eval_and_reduce_pre_post
        kf ab b Precondition b.b_requires states build_prop build_env
  in
  List.fold_left incorporate_behavior init_states spec.spec_behavior


(* Reduce the given states according to the given code annotations.
   If [record] is true, update the proof state of the code annotation.
   DO NOT PASS record=false unless you know what your are doing *)
let interp_annot kf ab initial_state slevel states stmt ca record =
  let ips = Property.ip_of_code_annot kf stmt ca in
  let source = match Cil_datatype.Code_annotation.loc ca with
    | Some loc when not (Cil_datatype.Location.equal
                           loc Cil_datatype.Location.unknown)
        -> fst loc
    | _ -> fst (Cil.CurrentLoc.get ()) (* fallback: current statement *)
  in
  let aux_interp text behav p ip =
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
      let statuses, reduced_states =
        State_set.fold
          (fun (accres, accstateset) (here, trace as ht) ->
            let env = env_annot ~pre:initial_state ~here () in
	    let res = eval_predicate env p in
            let reduced_states = match res, in_behavior with
              | _, `Unknown ->
                (* Cannot conclude because behavior might be inactive *)
 		State_set.add ht (accstateset)

              | False, `True -> (* Dead/invalid branch *)
                accstateset

              | (Unknown | True), `True ->
                let env = env_annot ~pre:initial_state ~here () in
                (* Reduce by p if it is a disjunction, or if it did not
                   evaluate to True *)
                let reduce = res = Unknown in
                let reduced_states =
                  split_disjunction_and_reduce ~reduce ~env (here,trace) ~slevel p ip
                in
                State_set.merge reduced_states accstateset
            in
            res :: accres, reduced_states
	  ) ([], State_set.empty) states 
      in
      (* if record [holds], emit statuses in the Kernel, and print a message *)
      if record then begin
        let status = join_list_predicate_status statuses in
        let change_status st =
          List.iter (fun p -> emit_status p st) ips
        in
        let message =
          match status, in_behavior with
          | Unknown, _ ->
            change_status Property_status.Dont_know;
            "unknown"
          | True, _ ->
            change_status Property_status.True;
            "valid"
          | False, `True ->
            change_status Property_status.False_if_reachable;
            "invalid (stopping propagation)"
          | False, `Unknown ->
            change_status Property_status.False_if_reachable;
            "invalid"
        in
	Value_parameters.result ~once:true ~source
	  "%s%a got status %s." text Description.pp_named p message;
      end;
      (* States resulting from disjunctions are reversed compared to the
         'nice' ordering *)
      State_set.reorder reduced_states
  in
  let aux text behav p ip =
    if State_set.is_empty states then (
      if record then
        Value_parameters.result ~once:true ~source
          "no state left in which to evaluate %s, status not \
         computed.%t" (String.lowercase text) Value_util.pp_callstack;
      states
    ) else
      aux_interp text behav p ip
  in
  match ca.annot_content with
    | AAssert (behav,p) ->
      aux "Assertion" behav p (Property.ip_of_code_annot_single kf stmt ca)
    | AInvariant (behav, true, p) ->
      aux "Loop invariant" behav p (Property.ip_of_code_annot_single kf stmt ca)
    | APragma _
    | AInvariant (_, false, _)
    | AVariant _ | AAssigns _ | AAllocation _
    | AStmtSpec _ (*TODO*) -> states


let mark_unreachable () =
  let mark ppt =
    if not (Property_status.automatically_proven ppt) then begin
      Value_parameters.debug "Marking property %a as dead"
        Description.pp_property ppt;
      let emit =
        Property_status.emit ~distinct:false Value_util.emitter ~hyps:[]
      in
      let reach_p = Property.ip_reachable_ppt ppt in
      emit ppt Property_status.True;
      emit reach_p Property_status.False_and_reachable;
      notify_status reach_p Property_status.False_and_reachable Cvalue.Model.bottom
    end
  in
  (* Mark standard code annotations *)
  let do_code_annot stmt _emit ca =
    if not (Db.Value.is_reachable_stmt stmt) then begin
      let kf = Kernel_function.find_englobing_kf stmt in
      let ppts = Property.ip_of_code_annot kf stmt ca in
      List.iter mark ppts;
    end
  in
  (* Mark preconditions of dead calls *)
  let unreach = object
    inherit Visitor.frama_c_inplace

    method! vstmt_aux stmt =
      if not (Db.Value.is_reachable_stmt stmt) then begin
        match stmt.skind with
          | Instr (Call (_, e, _, _)) ->
            (match Kernel_function.get_called e with
              | Some kf ->
                let preconds =
                  Statuses_by_call.all_call_preconditions_at
                    ~warn_missing:false kf stmt
                in
                List.iter (fun (_, p) -> mark p) preconds
              | None -> ())
          | _ -> ()
      end;
      Cil.DoChildren

    method! vinst _ = Cil.SkipChildren
  end
  in
  Annotations.iter_all_code_annot do_code_annot;
  Visitor.visitFramacFile unreach (Ast.get ())

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
