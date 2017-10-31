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

(* Applied to the list of behaviors of a function specification, returns the
   default behavior and the list of non-default behaviors. The incoming list
   should not be empty (it contains at least the default behavior). *)
let extract_default_behavior =
  let rec extract acc = function
    | [] -> assert false
    | behavior :: tail ->
      if behavior.b_name = Cil.default_behavior_name
      then behavior, acc @ tail
      else extract (behavior :: acc) tail
  in
  extract []

(* Returns the assigns clause to be used during per-behavior processing.
   The specification states that, if a behavior has no assigns clause,
   then the assigns clause of the default behavior must be used instead. *)
let get_assigns_for_behavior spec b =
  match b.b_assigns with
  | WritesAny -> (* no assigns clause, using the default behavior's *)
    let behaviors = spec.spec_behavior in
    let name = Cil.default_behavior_name in
    let def_b =  List.find (fun b' -> b'.b_name = name) behaviors in
    def_b.b_assigns
  | _ -> b.b_assigns

let pp_eval_error fmt e =
  if e <> Eval_terms.CAlarm then
    Format.fprintf fmt "@ (%a)" Eval_terms.pretty_logic_evaluation_error e

let warn_on_imprecise_assigns assigns =
  match assigns with
  | WritesAny ->
    Value_util.warning_once_current
      "Cannot handle empty assigns clause. Assuming assigns \\nothing: \
       be aware this is probably incorrect."
  | Writes l ->
    (* Warn for clauses without \from *)
    let no_from = List.filter (fun (_, from) -> from = FromAny) l in
    match no_from with
    | [] -> ()
    | (out, _) :: _ ->
      let source = fst out.it_content.term_loc in
      Value_parameters.warning ~source ~once:true
        "@[no \\from part@ for clause '%a'@]"
        Printer.pp_assigns (Writes no_from)

(* Warns in case the 'assigns \result' clause is missing in a behavior
   (only if the return is used at the call site). *)
let warn_on_missing_result_assigns kinstr kf spec =
  let return_used = match kinstr with
    | Kglobal -> true
    | Kstmt {skind = Instr (Call (lv, _, _, _))} ->
      lv <> None || Value_util.postconditions_mention_result spec
    | Kstmt {skind = Instr (Local_init(_,ConsInit(_,_,Constructor),_)) } ->
      Value_util.postconditions_mention_result spec
    | Kstmt {skind=Instr(Local_init(_,ConsInit(_,_,Plain_func),_))} -> true
    | _ -> assert false
  in
  let for_result (out, _) = Logic_utils.is_result out.it_content in
  let assigns_result behavior =
    match behavior.b_assigns with
    | WritesAny -> true
    | Writes l -> List.exists for_result l
  in
  if return_used && not (List.for_all assigns_result spec.spec_behavior)
  then
    let source = fst (Kernel_function.get_location kf) in
    Value_parameters.warning ~once:true ~source
      "@[no 'assigns \\result@ \\from ...'@ clause@ specified for@ function %a@]"
      Kernel_function.pretty kf

let reduce_to_valid_location out loc =
  if Locations.(Location_Bits.(equal top loc.loc)) then
    begin
      Value_parameters.error ~once:true ~current:true
        "Cannot@ handle@ assigns@ for %a,@ location@ is@ too@ imprecise@ \
         (%a).@ Assuming@ it@ is@ not@ assigned,@ but@ be@ aware@ this\
         @ is@ incorrect." Printer.pp_term out Locations.pretty loc;
      `Ignored
    end
  else
    let valid = Locations.valid_part ~for_writing:true loc in
    if Locations.is_bottom_loc valid then
      begin
        if not (Locations.is_bottom_loc loc) then
          Value_parameters.warning ~current:true ~once:true
            "@[Completely invalid destination@ for assigns@ clause %a.@ \
             Ignoring.@]" Printer.pp_term out;
        `Ignored
      end
    else `Result loc

let precise_loc_of_from_clause env out =
  try
    (* TODO: warn about errors during evaluation. *)
    let alarm_mode = Eval_terms.Ignore in
    let loc = Eval_terms.eval_tlval_as_location ~alarm_mode env out in
    reduce_to_valid_location out loc
  with Eval_terms.LogicEvalError e ->
    Value_util.warning_once_current
      "cannot interpret assigns %a%a; effects will be ignored"
      Printer.pp_term out pp_eval_error e;
    `Ignored


module Make
    (Value: Abstract_value.External)
    (Location: Abstract_location.External)
    (Domain: Abstract_domain.External with type value = Value.t
                                       and type location = Location.location)
    (States: Powerset.S with type state = Domain.t)
    (Logic : Transfer_logic.S with type state = Domain.t
                               and type states = States.t)
= struct

  (* Most transfer functions about logic return a set of states instead of a
     single state, and States.empty instead of bottom. We thus use this monad
     to turn `Bottom into States.empty in the following for consistency. *)
  let (>>-) state f = match state with
    | `Bottom -> States.empty
    | `Value state -> f state

  (* The precise narrowing of disjunctive sets of states is the disjunction
     between the narrowing of each combination of states from each sets. The
     complexity is quadratic. *)
  let precise_narrow_states_list states_list =
    let fold = States.fold in
    let fold2 f set1 set2 acc =
      fold (fun s1 acc -> fold (fun s2  acc -> f s1 s2 acc) set2 acc) set1 acc
    in
    let rec disjunctive_narrow states = function
      | [] -> states
      | set :: tail ->
        let narrow s s' acc =  States.add' (Domain.narrow s s') acc in
        let states = fold2 narrow states set States.empty in
        disjunctive_narrow states tail
    in
    disjunctive_narrow (List.hd states_list) (List.tl states_list)

  (* Approximate narrowing of disjunctive sets: we narrow the join of each set,
     and we use this single state to reduce each state of one set, chosen
     arbitrarily.
     TODO: it would be useful to have an heuristic to choose the set to
     be kept. *)
  let approximate_narrow_states_list states_list =
    let joined_list = List.map States.join states_list in
    let narrowed_state = match joined_list with
      | [] -> assert false
      | hd :: tl -> List.fold_left (Bottom.narrow Domain.narrow) hd tl
    in
    narrowed_state >>- fun narrowed_state ->
    States.fold
      (fun state acc -> States.add' (Domain.narrow state narrowed_state) acc)
      (List.hd states_list)
      States.empty

  (* Narrowing of a list of disjunctive sets of states. *)
  let narrow_states_list = function
    | [] -> States.empty
    | [x] -> x
    | states_list ->
      if true
      then approximate_narrow_states_list states_list
      else precise_narrow_states_list states_list

  (* Extraction of the precise location and of the cvalue domain:
     needed to evaluate the location of an assigns clause. *)
  let set_ploc = Location.set Main_locations.ploc_key
  let set_location loc = set_ploc (Main_locations.PLoc.make loc)
  let get_cvalue_state = match Domain.get Cvalue_domain.key with
    | None -> fun _ -> Cvalue.Model.top
    | Some get -> get

  let make_env state = Eval_terms.env_assigns (get_cvalue_state state)

  (* Applies one assign clause. Returns the updated state, and a boolean
     indicating whether the clause has really been processed (it is false if
     the clause has been ignored because too imprecise). *)
  let treat_assign env ~pre state ({it_content = out}, _ as assigns) =
    (* Treats the output part of the assigns clause. *)
    match precise_loc_of_from_clause env out with
    | `Ignored -> state
    | `Result location ->
      let froms_locations = set_location location Location.top in
      Domain.logic_assign assigns froms_locations ~pre state

  let treat_statement_assigns assigns state =
    warn_on_imprecise_assigns assigns;
    match assigns with
    | WritesAny -> state (* The assign is ignored; a warning has been emitted. *)
    | Writes l ->
      let env = make_env state in
      List.fold_left (treat_assign env ~pre:state) state l

  let is_result ({it_content = out}, _) = Logic_utils.is_result out

  (* Interprets one function assign, with a special case for assigns \result. *)
  let treat_function_assign retres_loc env ~pre state assigns =
    if is_result assigns
    then
      match retres_loc with
      | None -> state
      | Some loc -> Domain.logic_assign assigns loc ~pre state
    else treat_assign env state ~pre assigns

  (* Interprets all the assigns of a function. *)
  let treat_function_assigns retres_vi assigns state =
    warn_on_imprecise_assigns assigns;
    match assigns with
    | WritesAny -> state
    | Writes l ->
      let env = make_env state in
      let retres_loc = Extlib.opt_map Location.eval_varinfo retres_vi in
      List.fold_left (treat_function_assign retres_loc env ~pre:state) state l


  (* Reduces the [states] by the assumes and requires clauses of the [behavior]
     of function [kf]. Warns about inactive postconditions if [states] are
     reduced to bottom. *)
  let reduce_by_preconditions = Logic.check_fct_preconditions_for_behaviors

  (* Computes the effects of the assigns clauses of the [behavior] and reduces
     the resulting states by its ensures clauses. [kf) is the called function,
     and [result] is the \result varinfo it returns, if any. *)
  let compute_effects kf spec result behavior status states =
    States.join states >>- fun pre_state ->
    let assigns = get_assigns_for_behavior spec behavior in
    let states = States.map (treat_function_assigns result assigns) states in
    Logic.check_fct_postconditions_for_behaviors kf [behavior] status
      ~result ~pre_state ~post_states:states

  module Behaviors = struct
    type t = funbehavior
    let equal b1 b2 = b1.b_name = b2.b_name
    let hash b = Hashtbl.hash b.b_name
  end
  module HashBehaviors = Hashtbl.Make (Behaviors)

  (* [behaviors] is a list of complete sets of behaviors.  This function
     interprets each complete set of behaviors in [states], and thus returns a
     list of sets of states (each one being the result of a complete set).
     [kf] is the related function, [kinstr] the call site, and [result] the
     \result varinfo returned by the function, if any.
     All behaviors in [behaviors] must have an Unknown status. False behaviors
     should have been removed, and true behaviors should be interpreted by
     [compute_true_behaviors]. *)
  let compute_complete_behaviors kinstr kf spec result behaviors states =
    (* As a behavior may be included in several complete sets, we use a local
       cache for the interpretation of each behavior. *)
    let cache = HashBehaviors.create 3 in
    let compute_behavior behavior =
      try HashBehaviors.find cache behavior
      with Not_found ->
        let s = Alarmset.Unknown in
        let states = reduce_by_preconditions kinstr kf [behavior] s states in
        let states = compute_effects kf spec result behavior s states in
        HashBehaviors.add cache behavior states;
        states
    in
    let compute_complete_set behaviors =
      List.fold_left
        (fun acc b -> fst (States.merge (compute_behavior b) ~into:acc))
        States.empty behaviors
    in
    List.map compute_complete_set behaviors

  (* Interprets a list of behaviors as if they was merged into a single
     behavior. Uses all the preconditions and postconditinos at once to
     reduce the states, and uses the assigns clauses of the first behavior
     only (ideally, we want the intersection of assigns clauses). *)
  let compute_true_behaviors kinstr kf spec result behaviors states =
    let status = Alarmset.True in
    let states = reduce_by_preconditions kinstr kf behaviors status states in
    States.join states >>- fun pre_state ->
    let assigns = get_assigns_for_behavior spec (List.hd behaviors) in
    let states = States.map (treat_function_assigns result assigns) states in
    Logic.check_fct_postconditions_for_behaviors kf behaviors Alarmset.True
      ~result ~pre_state ~post_states:states


  (* Auxiliary function for promote_complete_behaviors. Replaces the status of
     a behavior in an association list binding behaviors to statuses. *)
  let rec replace_in_list elt assoc = function
    | [] -> []
    | (key, data) :: tail ->
      if String.compare key.b_name elt.b_name = 0
      then (elt, assoc) :: tail
      else (key, data) :: replace_in_list elt assoc tail

  (* If a complete set of behaviors contains only one active behavior (whose
     assumes clauses are not false), then this behavior is true.
     If [behaviors] is an association list binding each behavior to the status
     of its assumes clauses, and [complete_list] is the list of complete sets
     of behaviors, then [promote_complete_behaviors] removes false behaviors
     from [complete_list], and binds single active behaviors from complete sets
     to true in [behaviors].
     Returns `Bottom if a all the behaviors of a complete set have a false
     \assumes clause.  *)
  let promote_complete_behaviors behaviors complete_list =
    let module E = struct exception Bottom end in
    let is_not_false b = List.assoc b behaviors <> Alarmset.False in
    let complete_list = List.map (List.filter is_not_false) complete_list in
    let promote acc = function
      (* If a complete set of behaviors is empty here, then it contains only
         false behaviors, and thus its interpretation is bottom. *)
      | [] -> raise E.Bottom
      | [b] -> replace_in_list b Alarmset.True acc
      | _ -> acc
    in
    try `Value (List.fold_left promote behaviors complete_list, complete_list)
    with E.Bottom -> `Bottom

  (* Evaluates the \assumes of each behavior, and returns an association list
     between behaviors and their status. Also removes false behaviors from
     the list of complete behaviors [complete_behaviors], and promotes complete
     sets of one behavior as true behaviors.
     This function also evaluates the \requires clauses of the behaviors that
     will not be used in the interpretation of the specification: false
     behaviors, and unknown behaviors that do not belong to any complete set.
     This ensures that the preconditions of all behaviors will have been
     evaluated, and that consistent status will have been emitted at the end
     of the interpretation of the specification. *)
  let evaluate_preconditions kinstr kf behaviors complete_behaviors states =
    (* Processes all behaviors as inactive and returns bottom. *)
    let all_inactive () =
      Transfer_logic.process_inactive_behaviors kinstr kf behaviors;
      `Bottom
    in
    match States.join states with
    (* If the preconditions of the default behavior led to bottom, all other
       behaviors are inactive. *)
    | `Bottom -> all_inactive ()
    | `Value pre_state ->
      (* Evaluate all assumes clauses, and compute the association list between
         behaviors and their status. *)
      let evaluate = Logic.evaluate_assumes_of_behavior pre_state in
      let behaviors = List.map (fun b -> b, evaluate b) behaviors in
      (* Remove false behaviors from complete sets of behaviors, and promotes
         complete sets of one behavior as true behaviors. *)
      match promote_complete_behaviors behaviors complete_behaviors with
      (* If all behaviors of a complete set have false \assumes, all behaviors
         are inactive. *)
      | `Bottom -> all_inactive ()
      | `Value (behaviors, complete_behaviors) ->
        (* Evaluates \requires for false or non-complete unknown behaviors. *)
        let evaluate_requires (behavior, status) =
          if status = Alarmset.False
          then Transfer_logic.process_inactive_behaviors kinstr kf [behavior]
          else if status = Alarmset.Unknown
               && not (List.exists (List.mem behavior) complete_behaviors)
          then
            ignore (reduce_by_preconditions kinstr kf [behavior] status states)
        in
        List.iter evaluate_requires behaviors;
        `Value (behaviors, complete_behaviors)

  (* Sound over-approximations of the effects of a function can be computed
     through its specification in three different ways:
     - the default behavior is always an over-approximation of the function
       effects, but can be very imprecise. We use it only if the two other ways
       are inapplicable (both are strictly more precise).
     - any behavior whose assumes clause is true in the current state is also a
       sound approximation of the function effects applied to this state.
     - the union of any complete set of behaviors is an over-approximation of
       the function effects.
     To obtain the highest precision, the states resulting from the
     interpretation of any true behavior and of any complete set should be
     intersected. *)
  let compute_specification kinstr kf result spec state =
    (* The default behavior, and the list of other behaviors. *)
    let default_bhv, behaviors = extract_default_behavior spec.spec_behavior in
    let find_behavior name = List.find (fun b -> b.b_name = name) behaviors in
    (* List of complete sets of behaviors. *)
    let complete_behaviors =
      List.map (List.map find_behavior) spec.spec_complete_behaviors
    in
    (* Reduction by the preconditions of the default behavior. The resulting
       state is the pre state for any further computation. *)
    let states =
      Logic.check_fct_preconditions_for_behaviors
        kinstr kf [default_bhv] Alarmset.True (States.singleton state)
    in
    evaluate_preconditions kinstr kf behaviors complete_behaviors states
    >>- fun (behaviors, complete_behaviors) ->
    (* List of true behaviors other than the default behavior. *)
    let true_behaviors =
      Extlib.filter_map (fun (_b, st) -> st = Alarmset.True) fst behaviors
    in
    (* Without any true behaviors or complete sets, compute the effects of
       the default behavior. *)
    if true_behaviors = [] && spec.spec_complete_behaviors = []
    then compute_effects kf spec result default_bhv Alarmset.True states
    else
      (* Remove complete sets that contain a true behavior: such behaviors are
         treated afterwards. *)
      let is_true b = List.assoc b behaviors = Alarmset.True in
      let complete_behaviors =
        List.filter (fun l -> not (List.exists is_true l)) complete_behaviors
      in
      (* Interpret each complete set of behaviors. The result is a list of
         state sets, one for each set. The join of each state set is a sound
         approximation at the end of the function call. *)
      let complete_states =
        compute_complete_behaviors
          kinstr kf spec result complete_behaviors states
      in
      (* If there is some true behaviors, interpret them and add the resulting
         state set to the list. All true behaviors have their clauses computed
         as in the case of a single specification. *)
      let sound_states =
        if true_behaviors = []
        then complete_states
        else
          let true_states =
            compute_true_behaviors kinstr kf spec result true_behaviors states
          in
          true_states :: complete_states
      in
      (* As each state set in this list is a sound approximation, narrow them. *)
      narrow_states_list sound_states

  (* Interprets the [call] at [kinstr] in [state], using the specification
     [spec] of the called function. It first reduces by the preconditions, then
     evaluates the assigns, and finally reduces by the post-conditions. *)
  let compute_using_specification kinstr call spec state =
    if Value_parameters.InterpreterMode.get ()
    then Value_parameters.abort "Library function call. Stopping.";
    Value_parameters.feedback ~once:true
      "@[using specification for function %a@]" Kernel_function.pretty call.kf;
    let vi = Kernel_function.get_vi call.kf in
    if Cil.hasAttribute "noreturn" vi.vattr
    then `Bottom
    else
      (* Initializes the variable returned by the function. *)
      let state = match call.return with
        | None -> state
        | Some retres_vi ->
          (* Notify the user about missing assigns \result. *)
          warn_on_missing_result_assigns kinstr call.kf spec;
          let state = Domain.enter_scope call.kf [retres_vi] state in
          let init_kind = Abstract_domain.Spec_Return call.kf  in
          Domain.initialize_variable_using_type init_kind retres_vi state
      in
      let states =
        compute_specification kinstr call.kf call.return spec state
      in
      if States.is_empty states
      then `Bottom
      else `Value (States.to_list states)

end
