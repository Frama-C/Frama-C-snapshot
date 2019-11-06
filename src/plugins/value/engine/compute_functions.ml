(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

let dkey = Value_parameters.register_category "callbacks"

let floats_ok () =
  let u = min_float /. 2. in
  let u = u /. 2. in
  assert (0. < u && u < min_float)

let need_assigns kf =
  let spec = Annotations.funspec ~populate:false kf in
  match Cil.find_default_behavior spec with
  | None -> true
  | Some bhv -> bhv.b_assigns = WritesAny

let options_ok () =
  (* Check that we can parse the values specified for the options that require
     advanced parsing. Just make a query, as this will force the kernel to
     parse them. *)
  let check f = try ignore (f ()) with Not_found -> () in
  check Value_parameters.SplitReturnFunction.get;
  check Value_parameters.BuiltinsOverrides.get;
  check Value_parameters.SlevelFunction.get;
  check Value_parameters.EqualityCallFunction.get;
  let check_assigns kf =
    if need_assigns kf then
      Value_parameters.error "@[no assigns@ specified@ for function '%a',@ for \
                              which@ a builtin@ or the specification@ will be used.@ \
                              Potential unsoundness.@]" Kernel_function.pretty kf
  in
  Value_parameters.BuiltinsOverrides.iter (fun (kf, _) -> check_assigns kf);
  Value_parameters.UsePrototype.iter (fun kf -> check_assigns kf)

(* Do something tasteless in case the user did not put a spec on functions
   for which he set [-val-use-spec]:  generate an incorrect one ourselves *)
let generate_specs () =
  let aux kf =
    if need_assigns kf then begin
      let spec = Annotations.funspec ~populate:false kf in
      Value_parameters.warning "Generating potentially incorrect assigns \
                                for function '%a' for which option %s is set"
        Kernel_function.pretty kf Value_parameters.UsePrototype.option_name;
      (* The function populate_spec may emit a warning. Position a loc. *)
      Cil.CurrentLoc.set (Kernel_function.get_location kf);
      ignore (!Annotations.populate_spec_ref kf spec)
    end
  in
  Value_parameters.UsePrototype.iter aux

let pre_analysis () =
  floats_ok ();
  options_ok ();
  Split_return.pretty_strategies ();
  generate_specs ();
  Widen.precompute_widen_hints ();
  if Value_parameters.WarnBuiltinOverride.get () then
    Builtins.warn_definitions_overridden_by_builtins ();
  Value_perf.reset ();
  (* We may be resuming Value from a previously crashed analysis. Clear
     degeneration states *)
  Value_util.DegenerationPoints.clear ();
  Cvalue.V.clear_garbled_mix ();
  Value_util.clear_call_stack ();
  Db.Value.mark_as_computed ()

let post_analysis_cleanup ~aborted =
  Value_util.clear_call_stack ();
  (* Precompute consolidated states if required *)
  if Value_parameters.JoinResults.get () then
    Db.Value.Table_By_Callstack.iter
      (fun s _ -> ignore (Db.Value.get_stmt_state s));
  if not aborted then begin
    (* Keep memexec results for users that want to resume the analysis *)
    Mem_exec.cleanup_results ();
    if not (Value_parameters.SaveFunctionState.is_empty ()) then
      State_import.save_globals_state ();
  end

let post_analysis () =
  (* Garbled mix must be dumped here -- at least before the call to
     mark_green_and_red -- because fresh ones are created when re-evaluating
     all the alarms, and we get an unpleasant "ghost effect". *)
  Value_util.dump_garbled_mix ();
  (* Mark unreachable and RTE statuses. Only do this there, not when the
     analysis was aborted (hence, not in post_cleanup), because the
     propagation is incomplete. Also do not mark unreachable statutes if
     there is an alarm in the initializers (bottom initial state), as we
     would end up marking the alarm as dead. *)
  Eval_annots.mark_unreachable ();
  (* Try to refine the 'Unknown' statuses that have been emitted during
     this analysis. *)
  Eval_annots.mark_green_and_red ();
  Eval_annots.mark_rte ();
  post_analysis_cleanup ~aborted:false;
  (* Remove redundant alarms *)
  if Value_parameters.RmAssert.get () then !Db.Value.rm_asserts ()

(* Register a signal handler for SIGUSR1, that will be used to abort Value *)
let () =
  let prev = ref (fun _ -> ()) in
  let handler (_signal: int) =
    !prev Sys.sigusr1; (* Call previous signal handler *)
    Value_parameters.warning "Stopping analysis at user request@.";
    Iterator.signal_abort ()
  in
  try
    match Sys.signal Sys.sigusr1 (Sys.Signal_handle handler) with
    | Sys.Signal_default | Sys.Signal_ignore -> ()
    | Sys.Signal_handle f -> prev := f
  with Invalid_argument _ -> () (* Ignore: SIGURSR1 is not available on Windows,
                                   and possibly on other platforms. *)

module Make (Abstract: Abstractions.Eva) = struct

  module PowersetDomain = Powerset.Make (Abstract.Dom)

  module Transfer = Transfer_stmt.Make (Abstract)
  module Logic = Transfer_logic.Make (Abstract.Dom) (PowersetDomain)
  module Spec = Transfer_specification.Make (Abstract) (PowersetDomain) (Logic)
  module Init = Initialization.Make (Abstract.Dom) (Abstract.Eval) (Transfer)

  module Computer =
    Iterator.Computer
      (Abstract) (PowersetDomain) (Transfer) (Init) (Logic) (Spec)

  let initial_state = Init.initial_state

  let get_cval =
    match Abstract.Val.get Main_values.CVal.key with
    | None -> fun _ -> assert false
    | Some get -> fun value -> get value

  let get_ploc =
    match Abstract.Loc.get Main_locations.PLoc.key with
    | None -> fun _ -> assert false
    | Some get -> fun location -> get location

  (* Compute a call to [kf] in the state [state]. The evaluation will
     be done either using the body of [kf] or its specification, depending
     on whether the body exists and on option [-val-use-spec]. [call_kinstr]
     is the instruction at which the call takes place, and is used to update
     the statuses of the preconditions of [kf]. If [show_progress] is true,
     the callstack and additional information are printed. *)
  let compute_using_spec_or_body call_kinstr call state =
    let kf = call.kf in
    Value_results.mark_kf_as_called kf;
    let global = match call_kinstr with Kglobal -> true | _ -> false in
    let pp = not global && Value_parameters.ValShowProgress.get () in
    let call_stack = Value_util.call_stack () in
    if pp then
      Value_parameters.feedback
        "@[computing for function %a.@\nCalled from %a.@]"
        Value_types.Callstack.pretty_short call_stack
        Cil_datatype.Location.pretty (Cil_datatype.Kinstr.loc call_kinstr);
    let use_spec =
      if call.recursive then
        `Spec (Recursion.empty_spec_for_recursive_call kf)
      else
        match kf.fundec with
        | Declaration (_,_,_,_) -> `Spec (Annotations.funspec kf)
        | Definition (def, _) ->
          if Kernel_function.Set.mem kf (Value_parameters.UsePrototype.get ())
          then `Spec (Annotations.funspec kf)
          else `Def def
    in
    let cvalue_state = Abstract.Dom.get_cvalue_or_top state in
    let resulting_states, cacheable = match use_spec with
      | `Spec spec ->
        Db.Value.Call_Type_Value_Callbacks.apply
          (`Spec spec, cvalue_state, call_stack);
        if Value_parameters.InterpreterMode.get ()
        then Value_parameters.abort "Library function call. Stopping.";
        Value_parameters.feedback ~once:true
          "@[using specification for function %a@]" Kernel_function.pretty kf;
        let vi = Kernel_function.get_vi kf in
        if Cil.hasAttribute "fc_stdlib" vi.vattr then
          Library_functions.warn_unsupported_spec vi.vorig_name;
        Spec.compute_using_specification ~warn:true call_kinstr call spec state,
        Value_types.Cacheable
      | `Def _fundec ->
        Db.Value.Call_Type_Value_Callbacks.apply (`Def, cvalue_state, call_stack);
        Computer.compute kf call_kinstr state
    in
    if pp then
      Value_parameters.feedback
        "Done for function %a" Kernel_function.pretty kf;
    Transfer.{ states = resulting_states; cacheable; builtin=false }


  (* Mem Exec *)

  module MemExec = Mem_exec.Make (Abstract.Val) (Abstract.Dom)

  let compute_and_cache_call stmt call init_state =
    let default () = compute_using_spec_or_body (Kstmt stmt) call init_state in
    if Value_parameters.MemExecAll.get () then
      let args =
        List.map (fun {avalue} -> Eval.value_assigned avalue) call.arguments
      in
      match MemExec.reuse_previous_call call.kf init_state args with
      | None ->
        let call_result = default () in
        let () =
          if not (!Db.Value.use_spec_instead_of_definition call.kf)
          && call_result.Transfer.cacheable = Value_types.Cacheable
          then
            let final_states = call_result.Transfer.states in
            MemExec.store_computed_call call.kf init_state args final_states
        in
        call_result
      | Some (states, i) ->
        let stack = Value_util.call_stack () in
        let cvalue = Abstract.Dom.get_cvalue_or_top init_state in
        Db.Value.Call_Type_Value_Callbacks.apply (`Memexec, cvalue, stack);
        (* Evaluate the preconditions of kf, to update the statuses
           at this call. *)
        let spec = Annotations.funspec call.kf in
        if not (Value_util.skip_specifications call.kf) &&
           Eval_annots.has_requires spec
        then begin
          let ab = Logic.create init_state call.kf in
          ignore (Logic.check_fct_preconditions
                    (Kstmt stmt) call.kf ab init_state);
        end;
        if Value_parameters.ValShowProgress.get () then begin
          Value_parameters.feedback ~current:true
            "Reusing old results for call to %a" Kernel_function.pretty call.kf;
          Value_parameters.debug ~dkey
            "calling Record_Value_New callbacks on saved previous result";
        end;
        let stack_with_call = Value_util.call_stack () in
        Db.Value.Record_Value_Callbacks_New.apply
          (stack_with_call, Value_types.Reuse i);
        (* call can be cached since it was cached once *)
        Transfer.{states; cacheable = Value_types.Cacheable; builtin=false}
    else
      default ()

  let get_cvalue_call call =
    let lift_left left = { left with lloc = get_ploc left.lloc } in
    let lift_flagged_value value = { value with v = value.v >>-: get_cval } in
    let lift_assigned = function
      | Assign value -> Assign (get_cval value)
      | Copy (lval, value) -> Copy (lift_left lval, lift_flagged_value value)
    in
    let lift_argument arg = { arg with avalue = lift_assigned arg.avalue } in
    let arguments = List.map lift_argument call.arguments in
    let rest = List.map (fun (e, assgn) -> e, lift_assigned assgn) call.rest in
    { call with arguments; rest }

  let join_states = function
    | [] -> `Bottom
    | [state] -> `Value state
    | s :: l  -> `Value (List.fold_left Abstract.Dom.join s l)

  let compute_call_or_builtin stmt call state =
    match Builtins.find_builtin_override call.kf with
    | None -> compute_and_cache_call stmt call state
    | Some (name, builtin, spec) ->
      Value_results.mark_kf_as_called call.kf;
      let kinstr = Kstmt stmt in
      let kf_name = Kernel_function.get_name call.kf in
      if Value_parameters.ValShowProgress.get ()
      then
        Value_parameters.feedback ~current:true "Call to builtin %s%s"
          name (if kf_name = name then "" else " for function " ^ kf_name);
      (* Do not track garbled mixes created when interpreting the specification,
         as the result of the cvalue builtin will overwrite them. *)
      Locations.Location_Bytes.do_track_garbled_mix false;
      let states =
        Spec.compute_using_specification ~warn:false kinstr call spec state
      in
      Locations.Location_Bytes.do_track_garbled_mix true;
      let final_state = states >>- join_states in
      let cvalue_state = Abstract.Dom.get_cvalue_or_top state in
      match final_state with
      | `Bottom ->
        let cs = Value_util.call_stack () in
        Db.Value.Call_Type_Value_Callbacks.apply (`Spec spec, cvalue_state, cs);
        let cacheable = Value_types.Cacheable in
        Transfer.{states; cacheable; builtin=true}
      | `Value final_state ->
        let cvalue_call = get_cvalue_call call in
        let cvalue_states, cacheable =
          Builtins.apply_builtin builtin cvalue_call cvalue_state
        in
        let insert cvalue_state =
          Abstract.Dom.set Cvalue_domain.State.key cvalue_state final_state
        in
        let states = Bottom.bot_of_list (List.map insert cvalue_states) in
        Transfer.{states; cacheable; builtin=true}

  let compute_call =
    if Abstract.Dom.mem Cvalue_domain.State.key
    && Abstract.Val.mem Main_values.CVal.key
    && Abstract.Loc.mem Main_locations.PLoc.key
    then compute_call_or_builtin
    else compute_and_cache_call

  let () = Transfer.compute_call_ref := compute_call

  let store_initial_state kf init_state =
    Abstract.Dom.Store.register_initial_state (Value_util.call_stack ()) init_state;
    let cvalue_state = Abstract.Dom.get_cvalue_or_top init_state in
    Db.Value.Call_Value_Callbacks.apply (cvalue_state, [kf, Kglobal])

  let compute kf init_state =
    try
      Value_util.push_call_stack kf Kglobal;
      store_initial_state kf init_state;
      let call =
        {kf; arguments = []; rest = []; return = None; recursive = false}
      in
      let final_result = compute_using_spec_or_body Kglobal call init_state in
      let final_states = final_result.Transfer.states in
      let final_state = PowersetDomain.(final_states >>-: of_list >>- join) in
      Value_util.pop_call_stack ();
      Value_parameters.feedback "done for function %a" Kernel_function.pretty kf;
      post_analysis ();
      Abstract.Dom.post_analysis final_state;
      Value_results.print_summary ();
    with
    | Db.Value.Aborted ->
      post_analysis_cleanup ~aborted:true;
      (* Signal that a degeneration occurred *)
      if Value_util.DegenerationPoints.length () > 0 then
        Value_parameters.error
          "Degeneration occurred:@\nresults are not correct for lines of code \
           that can be reached from the degeneration point.@."


  let compute_from_entry_point kf ~lib_entry =
    pre_analysis ();
    Value_parameters.feedback "Analyzing a%scomplete application starting at %a"
      (if lib_entry then "n in" else " ")
      Kernel_function.pretty kf;
    let initial_state =
      try Init.initial_state_with_formals ~lib_entry kf
      with Db.Value.Aborted ->
        post_analysis_cleanup ~aborted:true;
        Value_parameters.abort "Degeneration occurred during initialization, aborting."
    in
    match initial_state with
    | `Bottom ->
      Value_parameters.result "Eva not started because globals \
                               initialization is not computable.";
      Eval_annots.mark_invalid_initializers ()
    | `Value init_state ->
      compute kf init_state

  let compute_from_init_state kf init_state =
    pre_analysis ();
    Abstract.Dom.Store.register_global_state (`Value init_state);
    compute kf init_state
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
