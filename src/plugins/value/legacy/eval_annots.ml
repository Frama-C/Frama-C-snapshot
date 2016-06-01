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
open Eval_terms

(* Statuses for code annotations and function contracts *)

(* Emits a status and a notification message. Returns the message callback. *)
let emit_status ppt status state =
  Property_status.emit ~distinct:true Value_util.emitter ~hyps:[] ppt status;
  Value_messages.new_status ppt status state
;;

(* Display the message as result/warning depending on [status] *)
let msg_status status ?current ?once ?source fmt =
  if status = True then
    if Value_parameters.ValShowProgress.get ()
    then Value_parameters.result ?current ?once ?source fmt
    else Value_parameters.result ?current ?once ?source ~level:2 fmt
  else Value_parameters.warning ?current ?once ?source fmt

module ActiveBehaviors = struct

  let header b =
    if Cil.is_default_behavior b then ""
    else ", behavior " ^ b.b_name


  let pp_bhv fmt b =
    if not (Cil.is_default_behavior b)
    then Format.fprintf fmt ", behavior %s" b.b_name

  let is_active_aux pre_state b =
    let assumes =
      (Logic_const.pands
         (List.map Logic_const.pred_of_id_pred b.b_assumes))
    in
    eval_predicate (env_pre_f ~pre:pre_state ()) assumes

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

let has_requires spec =
  let behav_has_requires b = b.b_requires <> [] in
  List.exists behav_has_requires spec.spec_behavior

let conv_status = function
  | False -> Property_status.False_if_reachable;
  | True -> Property_status.True;
  | Unknown -> Property_status.Dont_know

let behavior_inactive fmt =
  Format.fprintf fmt " (Behavior may be inactive, no reduction performed.)"

let pp_header kf fmt b =
  Format.fprintf fmt "function %a%a"
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
and p_kind = Precondition | Postcondition of postcondition_kf_kind | Assumes

let pp_p_kind fmt = function
  | Precondition -> Format.pp_print_string fmt "precondition"
  | Postcondition _ -> Format.pp_print_string fmt "postcondition"
  | Assumes -> Format.pp_print_string fmt "assumes"

let post_kind kf =
  if !Db.Value.use_spec_instead_of_definition kf then
    if Kernel_function.is_definition kf then
      PostUseSpec
    else
      PostLeaf
  else
    PostBody

let emit_message_and_status kf bhv behav_active ip pre_post pred_status pred named_pred state ~source =
  let pp_header = pp_header kf in
  match pre_post with
  | Precondition | Postcondition PostBody ->
    msg_status ~once:true ~source pred_status
      "%a: %a%a got status %a.%t%t"
      pp_header bhv pp_p_kind pre_post Description.pp_named named_pred
      pretty_predicate_status pred_status
      (if behav_active then (fun _ -> ()) else behavior_inactive)
      Value_util.pp_callstack;
    emit_status ip (conv_status pred_status) state;
  | Postcondition (PostLeaf | PostUseSpec as postk) ->
    (* Do not display anything for postconditions of leaf functions that
       receive status valid (very rare) or unknown: this brings no
       information. However, warn the user if the status is invalid.
       (unless this is on purpose, using [assert \false]) *)
    let pp_behavior_inactive fmt =
      Format.fprintf fmt ",@ the behavior@ was@ inactive"
    in
    if pred_status = False && pred.ip_content <> Pfalse then
      Value_parameters.warning ~once:true ~source
        "@[%a:@ this postcondition@ evaluates to@ false@ in this@ context.\
         @ If it is valid,@ either@ a precondition@ was not@ verified@ \
         for this@ call%t,@ or some assigns/from@ clauses@ are \
         incomplete@ (or incorrect).@]%t"
        pp_header bhv
        (if behav_active then (fun _ -> ()) else pp_behavior_inactive)
        Value_util.pp_callstack;
    (* Only emit a status if the function has a body. Otherwise, we would
       overwite the "considered valid" status of the kernel. *)
    if postk = PostUseSpec then
      emit_status ip (conv_status pred_status) state;
  | Assumes ->
    (* No statuses are emitted for 'assumes' clauses, and for the moment we
       do not emit text either *) ()

(* Emits informative messages about inactive behaviors, and emits a valid
   status for requires and ensures that have not been evaluated. *)
let process_inactive_behaviors kf ab state =
  List.iter
    (fun b ->
       if ab.ActiveBehaviors.is_active b = Eval_terms.False then begin
         let emitted = ref false in
         (* We emit a valid status for every requires and ensures of the
            behavior. *)
         List.iter (fun (tk, _ as post) ->
             if tk = Normal then begin
               emitted := true;
               if post_kind kf <> PostLeaf then
                 let ip = Property.ip_of_ensures kf Kglobal b post in
                 emit_status ip Property_status.True state;
             end
           ) b.b_post_cond;
         List.iter (fun pre ->
             emitted := true;
             let ip = Property.ip_of_requires kf Kglobal b pre in
             emit_status ip Property_status.True state;
           ) b.b_requires;
         if !emitted then
           Value_parameters.result ~once:true ~current:true ~level:2
             "%a: assumes got status invalid; behavior not evaluated.%t"
             (pp_header kf) b Value_util.pp_callstack;
       end
    ) ab.ActiveBehaviors.funspec.spec_behavior

(* Emits informative messages about behavior postconditions not evaluated
   because the _requires_ of the behavior are invalid. *)
let process_inactive_postconds kf inactive_post_state_list =
  List.iter
    (fun (b, state) ->
       let emitted = ref false in
       List.iter (fun (tk, _ as post) ->
           if tk = Normal then begin
             emitted := true;
             if post_kind kf <> PostLeaf then
               let ip = Property.ip_of_ensures kf Kglobal b post in
               emit_status ip Property_status.True state;
           end
         ) b.b_post_cond;
       if !emitted then
         Value_parameters.result ~once:true ~current:true ~level:2
           "%a: requires got status invalid; postconditions not evaluated.%t"
           (pp_header kf) b Value_util.pp_callstack;
    ) inactive_post_state_list

let warn_inactive kf b pre_post ip =
  let source = fst ip.ip_loc in
  Value_parameters.result ~once:true ~source ~level:2
    "%a: assumes got status invalid; %a not evaluated.%t"
    (pp_header kf) b pp_p_kind pre_post Value_util.pp_callstack

(* [per_behavior] indicates if we are processing each behavior separately.
   If this is the case, then [Unknown] and [True] behaviors are treated
   in the same way. *)
let refine_active ab b ~per_behavior =
  match ActiveBehaviors.active ab b with
  | True -> Some true
  | Unknown -> Some per_behavior
  | False -> None


(* [eval_and_reduce_p_kind kf b active p_kind ips states build_prop build_env]
   evaluates the identified predicates [ips] of [kf] in the states [states].
   The states are used simultaneously for evaluation and reduction: if one
   predicate is not valid in one of the states, the status of the predicate is
   set to [Unknown] or [Invalid]. In this case, the state is simultaneously
   reduced (when possible).
   [p_pkind] indicates the kind of clause being evaluated.
   [b] is the behavior to which [ips] belong.
   [active] indicates whether [b] is guaranteed to be active, or maybe active.
   [build_prop] builds the [Property.t] that corresponds to the pre/post being
     evaluated.
   [build_env] is used to build the environment evaluation, in particular
     the pre- and post-states.
 *)
let eval_and_reduce_p_kind kf b ~active p_kind ips build_prop build_env states =
  let pp_header = pp_header kf in
  let slevel = Value_util.get_slevel kf in
  let aux_pred behav_active states pred =
    let pr = Logic_utils.named_of_identified_predicate pred in
    let source = fst pr.loc in
    if State_set.is_empty states then
      (Value_parameters.result ~once:true ~source ~level:2
         "%a: no state left in which to evaluate %a, status%a not \
         computed.%t" pp_header b pp_p_kind p_kind
         Description.pp_named pr Value_util.pp_callstack;
       states)
    else
      let ip = build_prop pred in
      State_set.fold
        (fun accstateset (state, trace as stt) ->
           let env = build_env state in
	   let res = eval_predicate env pr in
           emit_message_and_status kf b behav_active ip p_kind res pred pr
             (state, trace (* TODO: add info *)) ~source;
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
	   (State_set.merge reduced_states accstateset)
	) State_set.empty states
  in
  List.fold_left (aux_pred active) states ips

(** Check the postcondition of [kf] for a given behavior [b].
    This may result in splitting [post_states] if the postconditions contain
    disjunctions. *)
let check_fct_postconditions_of_behavior kf ab b kind ~per_behavior ~result ~pre_state ~post_states =
  let post_conds = b.b_post_cond in
  let posts = List.filter (fun (x,_) -> x = kind) post_conds in
  let posts = List.map snd posts in
  match posts with
    | [] -> post_states
    | ip :: _ ->
      let k = Postcondition (post_kind kf) in
      match refine_active ab b per_behavior with
      | None ->
        warn_inactive kf b k ip;
        post_states
      | Some active ->
        let build_prop p = Property.ip_of_ensures kf Kglobal b (kind, p) in
        let build_env s = env_post_f ~post:s ~pre:pre_state ~result () in
        eval_and_reduce_p_kind
          kf b active k posts build_prop build_env post_states

(* per-behavior is not useful: the default behavior is always active *)
let check_fct_postconditions_default_behavior kf ab kind ~result ~pre_state ~post_states =
  try
    let b = ActiveBehaviors.behavior_from_name ab Cil.default_behavior_name in
    check_fct_postconditions_of_behavior
      kf ab b kind ~per_behavior:true ~result ~pre_state ~post_states
  with ActiveBehaviors.No_such_behavior -> post_states

(** Checks the postconditions of [b] and of the default behavior if it is not
    [b] *)
let check_fct_postconditions_for_behavior kf ab b kind ~result ~per_behavior ~pre_state post_states =
  let post_states =
    check_fct_postconditions_of_behavior kf ab b kind ~per_behavior
      ~result ~pre_state ~post_states
  in
  if Cil.is_default_behavior b then
    post_states
  else
    (* Applies the ensures clause of the global behavior on a given state set
       (this clause is inherited by each behavior). *)
    check_fct_postconditions_default_behavior kf ab kind ~result
      ~pre_state ~post_states

(** Check the postcondition of [kf] for every behavior, treating them
    separately if [per_behavior] is [true], merging them otherwise.
    The postcondition of the global behavior is applied for each behavior,
    to help reduce the final state. The default behavior is done once,
    at the end. *)
let check_fct_postconditions kf ab behaviors kind ~result ~per_behavior ~pre_state post_states =
  let post_states = List.fold_left
    (fun post_states b ->
      if b.b_name <> Cil.default_behavior_name then
        check_fct_postconditions_of_behavior kf ab b kind ~per_behavior
          ~result ~pre_state ~post_states
      else post_states
    ) post_states behaviors
  in
  if behaviors <> [] then
    check_fct_postconditions_default_behavior kf ab kind ~result
      ~pre_state ~post_states
  else post_states


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
    Unknown
  in
  match (Zone.is_included found_direct_deps stated_direct_deps,
         Zone.is_included found_indirect_deps stated_indirect_deps) with
  | true,true -> "valid", True
  | false,true -> res_for_unknown "direct"
  | false,false -> res_for_unknown "direct and indirect"
  | true,false -> res_for_unknown "indirect"


let check_fct_assigns kf ab ~pre_state found_froms =
  let open Locations in
  let behaviors = Annotations.behaviors kf in
  (* Under-approximation of the union. *)
  let link zones = List.fold_left Zone.link Zone.bottom zones in
  let outputs = Function_Froms.outputs found_froms in
  let check_for_behavior b =
    let activity = ActiveBehaviors.active ab b in
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
          emit_status ip status (pre_state, Trace.top);
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
            emit_status ip (conv_status status) (pre_state, Trace.top)
        in
        List.iter2 check_from assigns_deps assigns_zones)
  in List.iter check_for_behavior behaviors
;;

let verify_assigns_from kf ~pre froms =
  let ab = ActiveBehaviors.create pre kf in
  check_fct_assigns kf ab ~pre_state:pre froms
;;

Db.Value.verify_assigns_froms := verify_assigns_from;;

(** Check the precondition of [kf] for a given behavior [b].
    This may result in splitting [states] if the precondition contains
    disjunctions. *)
let check_fct_preconditions_for_behavior kf ab ~per_behavior call_ki states b =
  match b.b_requires with
  | [] -> states
  | ip :: _ ->
    let k = Precondition in
    match refine_active ab b per_behavior with
    | None ->
      warn_inactive kf b k ip;
      states
    | Some active ->
      let build_prop pre =
        let ip_precondition = Property.ip_of_requires kf Kglobal b pre in
        match call_ki with
        | Kglobal -> (* status of the main function. We update the global
                        status, and pray that there is no recursion.
                        TODO: check what the WP does.*)
          ip_precondition
        | Kstmt stmt ->
	  (* choose the copy of the precondition on the call point [stmt]. *)
          Statuses_by_call.setup_precondition_proxy kf ip_precondition;
          Statuses_by_call.precondition_at_call kf ip_precondition stmt
      in
      let build_env pre = env_pre_f ~pre () in
      eval_and_reduce_p_kind
        kf b ~active k b.b_requires build_prop build_env states

(*  Check the precondition of [kf]. This may result in splitting [init_state]
    into multiple states if the precondition contains disjunctions. *)
let check_fct_preconditions kf ab call_ki init_state =
  let init_trace = Trace.initial kf in
  let init_states = State_set.singleton (init_state, init_trace) in
  let spec = Annotations.funspec kf in
  List.fold_left
    (check_fct_preconditions_for_behavior ~per_behavior:false kf ab call_ki)
    init_states spec.spec_behavior

let code_annotation_text ca =
  match ca.annot_content with
  | AAssert _ ->  "assertion"
  | AInvariant _ ->  "loop invariant"
  | APragma _  | AVariant _ | AAssigns _ | AAllocation _ | AStmtSpec _ ->
    assert false (* currently not treated by Value *)

(* location of the given code annotation. If unknown, use the location of the
   statement instead. *)
let code_annotation_loc ca stmt =
  match Cil_datatype.Code_annotation.loc ca with
  | Some loc when not (Cil_datatype.Location.(equal loc unknown)) -> loc
  | _ -> Cil_datatype.Stmt.loc stmt

(* Reduce the given states according to the given code annotations.
   If [record] is true, update the proof state of the code annotation.
   DO NOT PASS record=false unless you know what your are doing *)
let interp_annot kf ab initial_state slevel states stmt ca record =
  let ips = Property.ip_of_code_annot kf stmt ca in
  let source, _ = code_annotation_loc ca stmt in
  let aux_interp ca behav p =
    let ip = Property.ip_of_code_annot_single kf stmt ca in
    let text = code_annotation_text ca in
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
      (* if record [holds], emit statuses in the Kernel, and print a message *)
      let emit status state =
        if record then begin
          let change_status st =
            List.iter (fun p -> emit_status p st state) ips
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
          msg_status status ~once:true ~source
            "%s%a got status %s." text Description.pp_named p message;
        end;
      in
      let reduced_states =
        State_set.fold
          (fun accstateset (here, _trace as ht) ->
            let env = env_annot ~pre:initial_state ~here () in
            let res = eval_predicate env p in
            emit res ht;
            match res, in_behavior with
              | _, `Unknown ->
                (* Cannot conclude because behavior might be inactive *)
                State_set.add ht accstateset

              | False, `True -> (* Dead/invalid branch *)
                accstateset

              | (Unknown | True), `True ->
                let env = env_annot ~pre:initial_state ~here () in
                (* Reduce by p if it is a disjunction, or if it did not
                   evaluate to True *)
                let reduce = res = Unknown in
                let reduced_states =
                  split_disjunction_and_reduce ~reduce ~env ht ~slevel p ip
                in
                State_set.merge reduced_states accstateset
          ) State_set.empty states
      in
      (* States resulting from disjunctions are reversed compared to the
         'nice' ordering *)
      State_set.reorder reduced_states
  in
  let aux ca behav p =
    if State_set.is_empty states then (
      if record then begin
        let text = code_annotation_text ca in
        Value_parameters.result ~once:true ~source ~level:2
          "no state left in which to evaluate %s, status not \
         computed.%t" (String.lowercase text) Value_util.pp_callstack;
      end;
      states
    ) else
      aux_interp ca behav p
  in
  match ca.annot_content with
    | AAssert (behav,p)
    | AInvariant (behav, true, p) -> aux ca behav p
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
        Property_status.emit ~distinct:true Value_util.emitter ~hyps:[]
      in
      let reach_p = Property.ip_reachable_ppt ppt in
      emit ppt Property_status.True;
      emit_status reach_p Property_status.False_and_reachable
        (Cvalue.Model.bottom, Trace.top)
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
               (* Do not put "unreachable" statuses on preconditions of
                  functions overriden by builtins. We do not evaluate those
                  preconditions on reachable calls, and the consolidation
                  gives very bad results when reachable and unrechable calls
                  coexist (untried+dead -> unknown). *)
               if not (Value_parameters.BuiltinsOverrides.mem kf) &&
                  not (!Db.Value.mem_builtin (Kernel_function.get_name kf))
               then begin
                 (* Setup all precondition statuses for [kf]: maybe it has
                    never been called anywhere. *)
                 Statuses_by_call.setup_all_preconditions_proxies kf;
                 (* Now mark the statuses at this particular statement as dead*)
                 let preconds =
                   Statuses_by_call.all_call_preconditions_at
                     ~warn_missing:false kf stmt
                 in
                 List.iter (fun (_, p) -> mark p) preconds
               end
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

let c_labels kf cs =
  if !Db.Value.use_spec_instead_of_definition kf then
    Cil_datatype.Logic_label.Map.empty
  else
    let fdec = Kernel_function.get_definition kf in
    let aux acc stmt =
      if stmt.labels != [] then
        try
          let hstate = Db.Value.Table_By_Callstack.find stmt in
          let state = Value_types.Callstack.Hashtbl.find hstate cs in
          Cil_datatype.Logic_label.Map.add (StmtLabel (ref stmt)) state acc
        with Not_found -> acc
      else acc
    in
    List.fold_left aux Cil_datatype.Logic_label.Map.empty fdec.sallstmts

(* Evaluates [p] at [stmt], using per callstack states for maximum precision. *)
(* TODO: we can probably factor some code with the GUI *)
let eval_by_callstack kf stmt p =
  (* This is actually irrelevant for alarms: they never use \old *)
  let pre = Db.Value.get_initial_state kf in
  let aux_callstack callstack state acc_status =
    let c_labels = c_labels kf callstack in
    let env = Eval_terms.env_annot ~c_labels ~pre ~here:state () in
    let status = Eval_terms.eval_predicate env p in
    let join = Eval_terms.join_predicate_status in
    match Bottom.join join acc_status (`Value status) with
    | `Value Unknown -> raise Exit (* shortcut *)
    | _ as r -> r
  in
  match Db.Value.get_stmt_state_callstack ~after:false stmt with
  | None -> (* dead; ignore, those will be marked 'unreachable' elsewhere *)
    Unknown
  | Some states ->
    try
      match Value_types.Callstack.Hashtbl.fold aux_callstack states `Bottom with
      | `Bottom -> Eval_terms.Unknown (* probably never reached *)
      | `Value status -> status
    with Exit -> Eval_terms.Unknown

(* Detection of terms \at(_, L) where L is a C label *)
class contains_c_at = object
  inherit Visitor.frama_c_inplace

  method! vterm t = match t.term_node with
    | Tat (_, StmtLabel _) -> raise Exit
    | _ -> Cil.DoChildren
end

let contains_c_at ca =
  let vis = new contains_c_at in
  let loc = Cil.CurrentLoc.get () in
  let r =
    try
      ignore (Visitor.visitFramacCodeAnnotation vis ca);
      false
    with Exit -> true
  in
  Cil.CurrentLoc.set loc;
  r

(* Re-evaluate all alarms, and see if we can put a 'green' or 'red' status,
   which would be more precise than those we have emitted during the current
   analysis. *)
let mark_green_and_red () =
  let do_code_annot stmt _e ca  =
    (* We reevaluate only alarms, in the hope that we can emit an 'invalid'
       status, or user assertions that mention a C label. The latter are
       currently skipped during evaluation. *)
    if contains_c_at ca || (Alarms.find ca <> None) then
      match ca.annot_content with
      | AAssert (_, p) | AInvariant (_, true, p) ->
        let loc = code_annotation_loc ca stmt in
        Cil.CurrentLoc.set loc;
        let kf = Kernel_function.find_englobing_kf stmt in
        let ip = Property.ip_of_code_annot_single kf stmt ca in
        (* This status is exact: we are _not_ refining the statuses previously
           emitted, but writing a synthetic more precise status. *)
        let distinct = false in
        let emit status =
          let status, text_status = match status with
            | `True -> Property_status.True, "valid"
            | `False -> Property_status.False_if_reachable, "invalid"
          in
          Property_status.emit ~distinct Value_util.emitter ~hyps:[] ip status;
          let source = fst loc in
          let text_ca = code_annotation_text ca in
          Value_parameters.result ~once:true ~source "%s%a got final status %s."
            text_ca Description.pp_named p text_status;
        in
        begin
          match eval_by_callstack kf stmt p with
          | Eval_terms.False -> emit `False
          | Eval_terms.True -> (* should not happen for an alarm that has been
              emitted during this Value analysis. However, this is perfectly
              possible for an 'old' alarm. *)
            emit `True
          | Eval_terms.Unknown -> ()
        end
      | AInvariant (_, false, _) | AStmtSpec _ | AVariant _ | AAssigns _
      | AAllocation _ | APragma _ -> ()
  in
  Annotations.iter_all_code_annot do_code_annot

(* Special evaluation for the alarms on the very first statement of the
   main function. We put 'Invalid' statuses on them using this function. *)
let mark_invalid_initializers () =
  let kf = fst (Globals.entry_point ()) in
  let first_stmt = Kernel_function.find_first_stmt kf in
  let do_code_annot _e ca  =
    match Alarms.find ca with (* We only check alarms *)
    | None -> ()
    | Some _ ->
      match ca.annot_content with
      | AAssert (_, p) ->
        let ip = Property.ip_of_code_annot_single kf first_stmt ca in
        (* Evaluate in a fully empty state. Only predicates that do not
           depend on the memory will result in 'False' *)
        let bot = Cvalue.Model.bottom in
        let env = Eval_terms.env_annot ~pre:bot ~here:bot () in
        begin match Eval_terms.eval_predicate env p with
        | True | Unknown -> ()
        | False ->
          let status = Property_status.False_and_reachable in
          let distinct = false (* see comment in mark_green_and_red above *) in
          Property_status.emit ~distinct Value_util.emitter ~hyps:[] ip status;
        end
      | _ -> ()
    in
    Annotations.iter_code_annot do_code_annot first_stmt


let () =
  Db.Value.valid_behaviors :=
    (fun kf state ->
      let ab = ActiveBehaviors.create state kf in
      ActiveBehaviors.active_behaviors ab
    );

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
