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

(* -------------------------- Message emission ------------------------------ *)

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
  | Precondition    -> Format.pp_print_string fmt "precondition"
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

let conv_status = function
  | Alarmset.False -> Property_status.False_if_reachable;
  | Alarmset.True -> Property_status.True;
  | Alarmset.Unknown -> Property_status.Dont_know

let emit_status ppt status =
  Property_status.emit ~distinct:true Value_util.emitter ~hyps:[] ppt status

(* Display the message as result/warning depending on [status] *)
let msg_status status ?current ?once ?source fmt =
  if status = Alarmset.True then
    if Value_parameters.ValShowProgress.get ()
    then Value_parameters.result ?current ?once ?source fmt
    else Value_parameters.result ?current ?once ?source ~level:2 fmt
  else
    Value_util.alarm_report ?current ?once ?source fmt

let behavior_inactive fmt =
  Format.fprintf fmt " (Behavior may be inactive, no reduction performed.)"

let pp_bhv fmt b =
  if not (Cil.is_default_behavior b)
  then Format.fprintf fmt ", behavior %s" b.b_name

let pp_header kf fmt b =
  Format.fprintf fmt "function %a%a"
    Kernel_function.pretty kf pp_bhv b

let emit_message_and_status kind kf behavior active property named_pred status =
  let pp_header = pp_header kf in
  let source = fst named_pred.Cil_types.pred_loc in
  match kind with
  | Precondition | Postcondition PostBody ->
    msg_status status ~once:true ~source
      "%a: %a%a got status %a.%t%t"
      pp_header behavior pp_p_kind kind Description.pp_named named_pred
      Alarmset.Status.pretty status
      (if active then (fun _ -> ()) else behavior_inactive)
      Value_util.pp_callstack;
    emit_status property (conv_status status);
  | Postcondition (PostLeaf | PostUseSpec as postk) ->
    (* Only emit a status if the function has a body. Otherwise, we would
       overwrite the "considered valid" status of the kernel. *)
    if postk = PostUseSpec then
      emit_status property (conv_status status)
  | Assumes ->
    (* No statuses are emitted for 'assumes' clauses, and for the moment we
       do not emit text either *) ()

let create_conjunction l=
  let loc = match l with
    | [] -> None
    | p :: _ -> Some p.ip_content.pred_loc
  in
  Logic_const.(List.fold_right (fun p1 p2 -> pand ?loc (p1, p2)) (List.map pred_of_id_pred l) ptrue)

(* -------------------------- Active behaviors ------------------------------ *)

module ActiveBehaviors = struct

  type t = {
    funspec: funspec;
    is_active: funbehavior -> Alarmset.status
  }

  module HashBehaviors = Hashtbl.Make(
    struct
      type t = funbehavior
      let equal b1 b2 = b1.b_name = b2.b_name
      let hash b = Hashtbl.hash b.b_name
    end)

  let is_active eval_predicate b =
    let assumes = create_conjunction b.b_assumes in
    eval_predicate assumes

  let create eval_predicate funspec =
    let h = HashBehaviors.create 3 in
    let is_active = fun b ->
      try HashBehaviors.find h b
      with Not_found ->
        let active = is_active eval_predicate b in
        HashBehaviors.add h b active;
        active
    in
    { is_active; funspec }

  let is_active ab behavior = ab.is_active behavior

  let active_behaviors ab =
    List.filter
      (fun b -> is_active ab b != Alarmset.False)
      ab.funspec.spec_behavior

  exception No_such_behavior
  let behavior_from_name ab b =
    try List.find (fun b' -> b'.b_name = b) ab.funspec.spec_behavior
    with Not_found -> raise No_such_behavior
end

let () =
  Db.Value.valid_behaviors :=
    (fun kf state ->
       let funspec = Annotations.funspec kf in
       let eval_predicate pred =
         match Eval_terms.(eval_predicate (env_pre_f ~pre:state ()) pred) with
         | Eval_terms.True -> Alarmset.True
         | Eval_terms.False -> Alarmset.False
         | Eval_terms.Unknown -> Alarmset.Unknown
       in
       let ab = ActiveBehaviors.create eval_predicate funspec in
       ActiveBehaviors.active_behaviors ab
    )

let ip_from_precondition kf call_ki b pre =
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

(* Emits informative messages about inactive behaviors, and emits a valid
   status for requires and ensures that have not been evaluated. *)
let process_inactive_behavior kf call_ki behavior =
  let emitted = ref false in
  (* We emit a valid status for every requires and ensures of the behavior. *)
  List.iter (fun (tk, _ as post) ->
      if tk = Normal then begin
        emitted := true;
        if post_kind kf <> PostLeaf then
          let ip = Property.ip_of_ensures kf Kglobal behavior post in
          emit_status ip Property_status.True;
      end
    ) behavior.b_post_cond;
  List.iter (fun pre ->
      emitted := true;
      let ip = ip_from_precondition kf call_ki behavior pre in
      emit_status ip Property_status.True;
    ) behavior.b_requires;
  if !emitted then
    Value_parameters.result ~once:true ~current:true ~level:2
      "%a: assumes got status invalid; behavior not evaluated.%t"
      (pp_header kf) behavior Value_util.pp_callstack

let process_inactive_behaviors call_ki kf behaviors =
  List.iter (process_inactive_behavior kf call_ki) behaviors

(* Emits informative messages about behavior postconditions not evaluated
   because the _requires_ of the behavior are invalid. *)
let process_inactive_postconds kf inactive_bhvs =
  List.iter
    (fun b ->
       let emitted = ref false in
       List.iter (fun (tk, _ as post) ->
           if tk = Normal then begin
             emitted := true;
             if post_kind kf <> PostLeaf then
               let ip = Property.ip_of_ensures kf Kglobal b post in
               emit_status ip Property_status.True;
           end
         ) b.b_post_cond;
       if !emitted then
         Value_parameters.result ~once:true ~current:true ~level:2
           "%a: requires got status invalid; postconditions not evaluated.%t"
           (pp_header kf) b Value_util.pp_callstack;
    ) inactive_bhvs

(* -------------------------------- Functor --------------------------------- *)

module type S = sig
  type state
  type states

  val create: state -> kernel_function -> ActiveBehaviors.t
  val create_from_spec: state -> spec -> ActiveBehaviors.t

  val check_fct_preconditions_for_behaviors:
    kinstr -> kernel_function -> behavior list -> Alarmset.status ->
    states -> states

  val check_fct_preconditions:
    kinstr -> kernel_function -> ActiveBehaviors.t -> state -> states or_bottom

  val check_fct_postconditions_for_behaviors:
    kernel_function -> behavior list -> Alarmset.status ->
    pre_state:state -> post_states:states -> result:varinfo option -> states

  val check_fct_postconditions:
    kernel_function -> ActiveBehaviors.t -> termination_kind ->
    pre_state:state -> post_states:states -> result:varinfo option ->
    states or_bottom

  val evaluate_assumes_of_behavior: state -> behavior -> Alarmset.status

  val interp_annot:
    limit:int -> record:bool ->
    kernel_function -> ActiveBehaviors.t -> stmt -> code_annotation ->
    initial_state:state -> states -> states
end

module type LogicDomain = sig
  type t
  val top: t
  val equal: t -> t -> bool
  val evaluate_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> Alarmset.status
  val reduce_by_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> bool -> t or_bottom
end

module Make
    (Domain: LogicDomain)
    (States: Powerset.S with type state = Domain.t)
= struct

  type state = Domain.t
  type states = States.t

  let pre_env ~pre =
    let states = function
      | BuiltinLabel Pre -> pre
      | BuiltinLabel Here -> pre
      | BuiltinLabel _ | FormalLabel _ | StmtLabel _ -> Domain.top
    in
    Abstract_domain.{ states; result = None }

  let post_env ~pre ~post ~result =
    let states = function
      | BuiltinLabel Pre -> pre
      | BuiltinLabel Old -> pre
      | BuiltinLabel Post -> post
      | BuiltinLabel Here -> post
      | BuiltinLabel _ | FormalLabel _ | StmtLabel _ -> Domain.top
    in
    Abstract_domain.{ states; result }

  let here_env  ~pre ~here =
    let states = function
      | BuiltinLabel Pre -> pre
      | BuiltinLabel Here -> here
      | BuiltinLabel _ | FormalLabel _ | StmtLabel _ -> Domain.top
    in
    Abstract_domain.{ states; result = None }

  let create_from_spec pre funspec =
    let eval_predicate = Domain.evaluate_predicate (pre_env ~pre) pre in
    ActiveBehaviors.create eval_predicate funspec

  let create init_state kf =
    let funspec = Annotations.funspec kf in
    create_from_spec init_state funspec

  exception Does_not_improve

  let rec fold_on_disjunction f p acc =
    match p.pred_content with
    | Por (p1,p2 ) -> fold_on_disjunction f p2 (fold_on_disjunction f p1 acc)
    | _ -> f p acc

  let count_disjunction p = fold_on_disjunction (fun _pred -> succ) p 0

  let split_disjunction_and_reduce ~reduce ~limit env state pred =
    let nb = count_disjunction pred in
    if nb <= 1 && not reduce then
      States.singleton state (* reduction not required, nothing to split *)
    else if nb <= limit
    then begin (* Can split and maybe reduce *)
      let treat_subpred pred acc =
        match Domain.reduce_by_predicate env state pred true with
        | `Bottom -> acc
        | `Value current_state ->
          if Domain.equal current_state state then
            (* This part of the disjunction will contain the entire state.
               Reduction has failed, there is no point in propagating the
               smaller states in acc, that are contained in this one. *)
            raise Does_not_improve
          else
            States.add current_state acc
      in
      try fold_on_disjunction treat_subpred pred States.empty
      with Does_not_improve -> States.singleton state
    end
    else if reduce then
      (* Not enough slevel to split, but we should reduce in a global way *)
      match Domain.reduce_by_predicate env state pred true with
      | `Bottom -> States.empty
      | `Value s -> States.singleton s
    else (* Not enough slevel to split, and reduction not required *)
      States.singleton state

  let eval_split_and_reduce limit active pred build_env state =
    let env = build_env state in
    let status = Domain.evaluate_predicate env state pred in
    let  reduced_states =
      if active then
        match status with
        | Alarmset.False   -> States.empty
        | Alarmset.True    ->
          (* Reduce in case [pre] is a disjunction *)
          split_disjunction_and_reduce ~reduce:false ~limit env state pred
        | Alarmset.Unknown ->
          (* Reduce in all cases *)
          split_disjunction_and_reduce ~reduce:true ~limit env state pred
      else
        States.singleton state
    in
    status, reduced_states

  let warn_inactive kf b pre_post ip =
    let source = fst ip.ip_content.pred_loc in
    Value_parameters.result ~once:true ~source ~level:2
      "%a: assumes got status invalid; %a not evaluated.%t"
      (pp_header kf) b pp_p_kind pre_post Value_util.pp_callstack

  (* Do not display anything for postconditions of leaf functions that
     receive status valid (very rare) or unknown: this brings no
     information. However, warn the user if the status is invalid.
     (unless this is on purpose, using [assert \false]) *)
  let check_ensures_false kf behavior active pr kind statuses =
    let source = fst pr.Cil_types.pred_loc in
    let pp_header = pp_header kf in
    let pp_behavior_inactive fmt =
      Format.fprintf fmt ",@ the behavior@ was@ inactive"
    in
    if (Alarmset.Status.join_list statuses) = Alarmset.False &&
       (match kind with
        | Postcondition (PostLeaf | PostUseSpec) -> true
        | _ -> false)
       && pr.pred_content <> Pfalse then
      Value_parameters.warning ~once:true ~source
        "@[%a:@ this postcondition@ evaluates to@ false@ in this@ context.\
         @ If it is valid,@ either@ a precondition@ was not@ verified@ \
         for this@ call%t,@ or some assigns/from@ clauses@ are \
         incomplete@ (or incorrect).@]%t"
        pp_header behavior
        (if active then (fun _ -> ()) else pp_behavior_inactive)
        Value_util.pp_callstack

  (* [per_behavior] indicates if we are processing each behavior separately.
     If this is the case, then [Unknown] and [True] behaviors are treated
     in the same way. *)
  let refine_active ~per_behavior behavior status =
    match status with
    | Alarmset.True -> Some true
    | Alarmset.Unknown -> Some (per_behavior || Cil.is_default_behavior behavior)
    | Alarmset.False -> None

  (* [eval_and_reduce_p_kind kf b active p_kind ips states build_prop build_env]
     evaluates the identified predicates [ips] of [kf] in the states [states].
     The states are used simultaneously for evaluation and reduction: if one
     predicate is not valid in one of the states, the status of the predicate is
     set to [Unknown] or [Invalid]. In this case, the state is simultaneously
     reduced (when possible).
     - [p_pkind] indicates the kind of clause being evaluated.
     - [b] is the behavior to which [ips] belong.
     - [active] indicates whether [b] is guaranteed to be active, or maybe active.
     - [build_prop] builds the [Property.t] that corresponds to the pre/post
       being evaluated.
     - [build_env] is used to build the environment evaluation, in particular
       the pre- and post-states. *)
  let eval_and_reduce_p_kind kf behavior active kind ips states build_prop build_env =
    let pp_header = pp_header kf in
    let limit = Value_util.get_slevel kf in
    let aux_pred states pred =
      let pr = Logic_const.pred_of_id_pred pred in
      let source = fst pr.Cil_types.pred_loc in
      if States.is_empty states then
        (Value_parameters.result ~once:true ~source ~level:2
           "%a: no state left in which to evaluate %a, status%a not \
            computed.%t" pp_header behavior pp_p_kind kind
           Description.pp_named pr Value_util.pp_callstack;
         states)
      else
        let property = build_prop pred in
        let (statuses, reduced_states) =
          States.fold
            (fun state (acc_status, acc_states) ->
               let status, reduced_states =
                 eval_split_and_reduce limit active pr build_env state
               in
               (status :: acc_status,
                fst (States.merge ~into:acc_states reduced_states)))
            states ([], States.empty)
        in
        let () =
          List.iter
            (fun status ->
               emit_message_and_status kind kf behavior active property pr status)
            statuses
        in
        check_ensures_false kf behavior active pr kind statuses;
        States.reorder reduced_states
    in
    List.fold_left aux_pred states ips

  let eval_and_reduce kf behavior refine kind ips states build_prop build_env =
    match ips with
    | [] -> states
    | ip :: _ ->
      match refine with
      | None        -> warn_inactive kf behavior kind ip; states
      | Some active ->
        eval_and_reduce_p_kind
          kf behavior active kind ips states build_prop build_env

  (** Check the postcondition of [kf] for the list of [behaviors].
      This may result in splitting [post_states] if the postconditions contain
      disjunctions. *)
  let check_fct_postconditions_of_behaviors kf behaviors is_active kind
      ~per_behavior ~pre_state ~post_states ~result =
    if behaviors = [] then post_states
    else
      let build_env s = post_env ~pre:pre_state ~post:s ~result in
      let k = Postcondition (post_kind kf) in
      let check_one_behavior states b =
        let posts = List.filter (fun (x, _) -> x = kind) b.b_post_cond in
        let posts = List.map snd posts in
        let refine = refine_active ~per_behavior b (is_active b) in
        let build_prop p = Property.ip_of_ensures kf Kglobal b (kind, p) in
        eval_and_reduce kf b refine k posts states build_prop build_env
      in
      List.fold_left check_one_behavior post_states behaviors

  (** Check the postcondition of [kf] for the list [behaviors] and for
      the default behavior, treating them separately if [per_behavior] is [true],
      merging them otherwise. *)
  let check_fct_postconditions_for_behaviors kf behaviors status
      ~pre_state ~post_states ~result =
    let behaviors =
      if List.exists Cil.is_default_behavior behaviors && behaviors <> []
      then behaviors
      else match Cil.find_default_behavior kf.spec with
        | None -> behaviors
        | Some default -> default :: behaviors
    in
    let is_active _ = status in
    let kind = Normal in
    check_fct_postconditions_of_behaviors kf behaviors is_active kind
      ~per_behavior:true ~pre_state ~post_states ~result

  (** Check the postcondition of [kf] for every behavior.
      The postcondition of the global behavior is applied for each behavior,
      to help reduce the final state. *)
  let check_fct_postconditions kf ab kind ~pre_state ~post_states ~result =
    let behaviors = Annotations.behaviors kf in
    let is_active = ActiveBehaviors.is_active ab in
    let states =
      check_fct_postconditions_of_behaviors
        kf behaviors is_active kind ~per_behavior:false
        ~pre_state ~post_states ~result
    in
    if States.is_empty states then `Bottom else `Value states


  let check_fct_preconditions_of_behaviors call_ki kf ~per_behavior behaviors
      is_active states =
    if behaviors = [] then states
    else
      let build_env pre = pre_env ~pre in
      let k = Precondition in
      let check_one_behavior states b =
        let refine = refine_active ~per_behavior b (is_active b) in
        let build_prop assume = Property.ip_of_assumes kf Kglobal b assume in
        let states =
          eval_and_reduce kf b refine Assumes b.b_assumes states build_prop build_env
        in
        if refine = None then process_inactive_behavior kf call_ki b;
        let build_prop = ip_from_precondition kf call_ki b in
        let states =
          eval_and_reduce kf b refine k b.b_requires states build_prop build_env
        in
        if States.is_empty states
        then process_inactive_postconds kf [b];
        states
      in
      List.fold_left check_one_behavior states behaviors

  (** Check the precondition of [kf] for a given behavior [b].
      This may result in splitting [states] if the precondition contains
      disjunctions. *)
  let check_fct_preconditions_for_behaviors call_ki kf behaviors status states =
    let is_active _ = status in
    check_fct_preconditions_of_behaviors call_ki kf ~per_behavior:true
      behaviors is_active states

  (*  Check the precondition of [kf]. This may result in splitting [init_state]
      into multiple states if the precondition contains disjunctions. *)
  let check_fct_preconditions call_ki kf ab init_state =
    let init_states = States.singleton init_state in
    let behaviors = Annotations.behaviors kf in
    let is_active = ActiveBehaviors.is_active ab in
    let states =
      check_fct_preconditions_of_behaviors call_ki kf ~per_behavior:false
        behaviors is_active init_states
    in
    if States.is_empty states then `Bottom else `Value states

  let evaluate_assumes_of_behavior state =
    let pre_env = pre_env ~pre:state in
    fun behavior ->
      let assumes = create_conjunction behavior.b_assumes in
      Domain.evaluate_predicate pre_env state assumes

  let code_annotation_text ca =
    match ca.annot_content with
    | AAssert _ ->  "assertion"
    | AInvariant _ ->  "loop invariant"
    | APragma _  | AVariant _ | AAssigns _ | AAllocation _ | AStmtSpec _ 
    | AExtended _ ->
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
  let interp_annot ~limit ~record kf ab stmt code_annot ~initial_state states =
    let ips = Property.ip_of_code_annot kf stmt code_annot in
    let source, _ = code_annotation_loc code_annot stmt in
    let aux_interp code_annot behav p =
      let text = code_annotation_text code_annot in
      let in_behavior =
        match behav with
        | [] -> `True
        | behavs ->
          let aux acc b =
            let b = ActiveBehaviors.behavior_from_name ab b in
            match ActiveBehaviors.is_active ab b with
            | Alarmset.True -> `True
            | Alarmset.Unknown -> if acc = `True then `True else `Unknown
            | Alarmset.False -> acc
          in
          List.fold_left aux `False behavs
      in
      match in_behavior with
      | `False -> states
      | `True | `Unknown as in_behavior ->
        let emit status =
          let change_status st = List.iter (fun p -> emit_status p st) ips in
          let message =
            match status, in_behavior with
            | Alarmset.Unknown, _ ->
              change_status Property_status.Dont_know;
              "unknown"
            | Alarmset.True, _ ->
              change_status Property_status.True;
              "valid"
            | Alarmset.False, `True ->
              change_status Property_status.False_if_reachable;
              "invalid (stopping propagation)"
            | Alarmset.False, `Unknown ->
              change_status Property_status.False_if_reachable;
              "invalid"
          in
          msg_status status ~once:true ~source
            "%s%a got status %s." text Description.pp_named p message
        in
        let reduced_states =
          States.fold
            (fun (here: Domain.t) accstateset ->
               let env = here_env ~pre:initial_state ~here in
               let res = Domain.evaluate_predicate env here p in
               (* if record [holds], emit statuses in the Kernel,
                  and print a message *)
               if record then emit res;
               match res, in_behavior with
               | _, `Unknown ->
                 (* Cannot conclude because behavior might be inactive *)
                 States.add here accstateset

               | Alarmset.False, `True -> (* Dead/invalid branch *)
                 accstateset

               | (Alarmset.Unknown | Alarmset.True), `True ->
                 let env = here_env ~pre:initial_state ~here in
                 (* Reduce by p if it is a disjunction, or if it did not
                    evaluate to True *)
                 let reduce = res = Alarmset.Unknown in
                 let reduced_states =
                   split_disjunction_and_reduce ~reduce ~limit env here p
                 in
                 fst (States.merge reduced_states ~into:accstateset)
            ) states States.empty
        in
        (* States resulting from disjunctions are reversed compared to the
           'nice' ordering *)
        States.reorder reduced_states
    in
    let aux code_annot behav p =
      if States.is_empty states then (
        if record then begin
          let text = code_annotation_text code_annot in
          Value_parameters.result ~once:true ~source ~level:2
            "no state left in which to evaluate %s, status not computed.%t"
            (Transitioning.String.lowercase_ascii text) Value_util.pp_callstack;
        end;
        states
      ) else
        aux_interp code_annot behav p
    in
    match code_annot.annot_content with
    | AAssert (behav,p)
    | AInvariant (behav, true, p) -> aux code_annot behav p
    | APragma _
    | AInvariant (_, false, _)
    | AVariant _ | AAssigns _ | AAllocation _ | AExtended _
    | AStmtSpec _ (*TODO*) -> states

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
