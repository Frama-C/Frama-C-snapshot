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

(* Value analysis of entire functions *)

open Cil_types
open Cil
open Value_util

module AB = Eval_annots.ActiveBehaviors

let dkey = Value_parameters.register_category "callbacks"


(*  Compute [kf] in state [with_formals] according to the body [f] of [kf].
    Checks the preconditions of [kf], assuming the call took place at
    [call_kinstr]. The postconditions are checked within the call to
    [Computer.compute]. *)
let compute_using_body (kf, f) ~call_kinstr ~with_formals =
  let with_locals =
    List.fold_left
      (fun acc local ->
        let loc = Locations.loc_of_varinfo local in
        Cvalue.Model.add_initial_binding
          acc loc Cvalue.V_Or_Uninitialized.uninitialized
      ) with_formals f.slocals
  in
  (* Remark: the pre-condition cannot talk about the locals. BUT
     check_fct_preconditions split the state into a stateset, hence
     it is simpler to apply it to the (unique) state with locals *)
  let ab = AB.create with_locals kf in
  let with_locals =
    Eval_annots.check_fct_preconditions kf ab call_kinstr with_locals
  in
  let module Computer =
    Eval_slevel.Computer
      (struct
        let kf = kf
        let initial_states = with_locals (* for future reference *)
        let active_behaviors = ab
       end)
  in
  begin
    try Computer.compute with_locals
    with Db.Value.Aborted as e ->
      (* analysis was aborted: pop the call stack and inform the caller *)
      Computer.mark_degeneration ();
      Computer.merge_results ();
      Db.Value.mark_as_computed ();
      raise e
  end;
  (* Merge consolidated results, call callbacks *)
  let result = Computer.results () in
  if Value_parameters.ValShowProgress.get() then
    Value_parameters.feedback "Recording results for %a"
      Kernel_function.pretty kf;
  Computer.merge_results ();
  (match result.Value_types.c_values with
     | _ :: _ when  hasAttribute "noreturn" f.svar.vattr ->
         warning_once_current
           "function %a may terminate but has the noreturn attribute"
           Kernel_function.pretty kf;
     | _ -> ());
  result

(** Evaluate [kf] in state [with_formals], first by reducing by the
    preconditions, then by evaluating the assigns, then by reducing
    by the post-conditions.
    The computation varies depending on the number of behaviors
    (single or multiple). Both methods are correct for any number
    of behaviors, but one is more efficient and the other is more precise. *)
let compute_using_specification (kf, spec) ~call_kinstr ~with_formals () =
  if Value_parameters.InterpreterMode.get()
  then begin
    warning_once_current "Library function call. Stopping.";
    exit 0
  end;
  Value_parameters.feedback ~once:true "@[using specification for function %a@]"
    Kernel_function.pretty kf;
  Eval_behaviors.compute_using_specification kf spec ~call_kinstr ~with_formals

(*  Compute a call to [kf] in the state [with_formals]. The evaluation will
    be done either using the body of [kf] or its specification, depending
    on whether the body exists and on option [-val-use-spec]. [call_kinstr]
    is the instruction at which the call takes place, and is used to update
    the statuses of the preconditions of [kf]. If [show_progress] is true,
    the callstack and additional information are printed. *)
let compute_using_spec_or_body ~with_formals ~call_kinstr ~show_progress kf =
  Value_results.mark_kf_as_called kf;
  let pp = show_progress && Value_parameters.ValShowProgress.get() in
  if pp then
    Value_parameters.feedback
      "@[computing for function %a.@\nCalled from %a.@]"
      Value_types.Callstack.pretty_short (call_stack ())
      Cil_datatype.Location.pretty (Cil_datatype.Kinstr.loc call_kinstr);
  let use_spec = match kf.fundec with
    | Declaration (_,_,_,_) -> `Spec (Annotations.funspec kf)
    | Definition (def, _) ->
      if Kernel_function.Set.mem kf (Value_parameters.UsePrototype.get ())
      then `Spec (Annotations.funspec kf)
      else `Def def
  in
  let result =  match use_spec with
    | `Spec spec ->
       Db.Value.Call_Type_Value_Callbacks.apply (`Spec, with_formals, call_stack());
        compute_using_specification (kf, spec) ~call_kinstr ~with_formals ()
    | `Def f ->
       Db.Value.Call_Type_Value_Callbacks.apply (`Def, with_formals, call_stack());
        compute_using_body (kf, f) ~call_kinstr ~with_formals
  in
  if pp then
    Value_parameters.feedback "Done for function %a" Kernel_function.pretty kf;
  result


(*  Compute a call to the main function. The initial state is generated
    according to options such as [-lib-entry] and the options of Value governing
    the shape of this state. *)
let compute_from_entry_point () =
  let kf, library = Globals.entry_point () in
  clear_call_stack ();
  Stop_at_nth.clear ();
  Value_results.mark_kf_as_called kf;
  Value_parameters.feedback "Analyzing a%scomplete application starting at %a"
    (if library then "n in" else " ")
    Kernel_function.pretty kf;
  Separate.prologue();
  let initial_state_globals =
    if Db.Value.globals_use_supplied_state () then (
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state supplied by user";
      Value_parameters.printf 
        ~header:(fun fmt -> Format.pp_print_string fmt
          "Values of globals")
        ~level:2 "@[  %a@]" Db.Value.pretty_state r;
      r)
    else
      (Value_parameters.feedback "Computing initial state";
       let r = Db.Value.globals_state () in
       Value_parameters.feedback "Initial state computed";
       Value_parameters.printf ~dkey:Value_parameters.dkey_initial_state
         ~header:(fun fmt -> Format.pp_print_string fmt
                     "Values of globals at initialization")
         "@[  %a@]" Db.Value.pretty_state r;
       r
      ) in
  if not (Db.Value.is_reachable initial_state_globals) 
  then begin
    Value_parameters.result "Value analysis not started because globals \
                               initialization is not computable.";
  end 
  else begin
    let with_formals = 
      match Db.Value.fun_get_args () with
      | None ->
          Function_args.main_initial_state_with_formals kf initial_state_globals
      | Some actuals ->
          let formals = Kernel_function.get_formals kf in
          if (List.length formals) <> List.length actuals then
            raise Db.Value.Incorrect_number_of_arguments;
          let treat_one_formal f a =
            (), Eval_op.offsetmap_of_v ~typ:f.vtype a
          in
          Function_args.actualize_formals
            kf
            initial_state_globals
            (List.map2 treat_one_formal formals actuals)
    in
    push_call_stack kf Kglobal;
    Db.Value.merge_initial_state (call_stack ()) with_formals;
    Db.Value.Call_Value_Callbacks.apply (with_formals, [ kf, Kglobal ]);
    ignore(compute_using_spec_or_body kf
             ~call_kinstr:Kglobal ~with_formals ~show_progress:false);
    pop_call_stack ();
    Value_parameters.feedback "done for function %a" Kernel_function.pretty kf;
    Separate.epilogue();
  end


(*  Compute a call to a possible builtin [kf] in state [state]. [actuals] are
    the arguments of [kf], and have not been bound to its formals. Returns
    [None] if the call must be computed using the Cil function for [kf]. *)
let compute_maybe_builtin kf ~state actuals =
  (* Actuals for builtins include a Cvalue.V arg, which is more convenient
     than the entire offsetmap *)
  let conv_arg with_alarms (exp, offsm) =
    let typ = typeOf exp in
    Valarms.set_syntactic_context (Valarms.SyUnOp exp);
    let v = Eval_op.v_of_offsetmap ~with_alarms ~typ offsm in
    (exp, v, offsm)
  in
  let actuals = lazy (
    let with_alarms = warn_all_quiet_mode () in
    List.map (conv_arg with_alarms) actuals
  )
  in
  let (!!) = Lazy.force in
  let name = Kernel_function.get_name kf in
  match Builtins.find_builtin_override kf with
  | Some abstract_function ->
    (* Mark the function as called, otherwise it would get skipped, eg. from
       the Gui. *)
    Value_results.mark_kf_as_called kf;
    begin
      try
        Some (abstract_function state !!actuals)
      with
      | Builtins.Invalid_nb_of_args n ->
        Value_parameters.error ~current:true
          "Invalid number of arguments for builtin %s: %d expected, %d found"
          name n (List.length !!actuals);
        raise Db.Value.Aborted
      | Db.Value.Outside_builtin_possibilities ->
        Value_parameters.warning ~once:true ~current:true
          "Call to builtin %s failed, aborting." name;
        raise Db.Value.Aborted
    end
  | None ->
    (* Special builtins, such as Frama_C_show_each_foo *)
    if Ast_info.can_be_cea_function name then
      (* A few special functions that are not registered in the builtin table *)
      if Ast_info.is_cea_dump_function name then
        Some (Builtins_misc.dump_state state !!actuals)
      else if Ast_info.is_cea_function name then
        Some (Builtins_misc.dump_args name state !!actuals)
      else if Ast_info.is_cea_dump_file_function name then
        Some (Builtins_misc.dump_state_file name state !!actuals)
      else
        None
    else None

(*  Compute a call to [kf] from [call_kinstr], assuming [kf] is not yet present
    in the callstack. In [state], the value of actuals in [actuals] are not
    yet bound to formals.*)
let compute_non_recursive_call kf ~call_kinstr state actuals =
  let with_formals =
    Function_args.actualize_formals
      ~check:Function_args.check_arg_size kf state actuals
  in
  push_call_stack kf call_kinstr;
  (* Store the initial state, but do not called mark_as_called. Uninteresting
     Value builtins are intentionally skipped *)
  Db.Value.merge_initial_state (call_stack ()) with_formals;
  try 
    let stack_with_call = call_stack () in
    Db.Value.Call_Value_Callbacks.apply (with_formals, stack_with_call);
    let default () = 
      let r = compute_maybe_builtin kf ~state actuals in
      match r with
        | Some r ->
           Db.Value.Call_Type_Value_Callbacks.apply
             (`Builtin r, with_formals, stack_with_call);
           r
	| None ->
            compute_using_spec_or_body kf
              ~with_formals ~call_kinstr ~show_progress:true
    in
    let r =
      let call_site = (kf, call_kinstr) in
      if Value_parameters.MemExecAll.get () then
	match Mem_exec.reuse_previous_call call_site with_formals actuals with
	  | None ->
            let res = default () in
            if not (!Db.Value.use_spec_instead_of_definition kf) then
              Mem_exec.store_computed_call call_site with_formals actuals res;
            res
	  | Some (res, i) ->
             Db.Value.Call_Type_Value_Callbacks.apply
               (`Memexec, with_formals, stack_with_call);
            (* Evaluate the preconditions of kf, to update the statuses
               at this call. *)
            let spec = Annotations.funspec kf in
            if Eval_annots.has_requires spec then begin
              let ab =
                AB.create_from_spec with_formals spec
              in
              ignore (Eval_annots.check_fct_preconditions
                        kf ab call_kinstr with_formals);
            end;
            if Value_parameters.ValShowProgress.get () then begin
              Value_parameters.feedback ~current:true
		"Reusing old results for call to %a" Kernel_function.pretty kf;
              Value_parameters.debug ~dkey
		"calling Record_Value_New callbacks on saved previous result";
            end;
            Db.Value.Record_Value_Callbacks_New.apply
              (stack_with_call, Value_types.Reuse i);
            res
      else
	default ()
    in
    pop_call_stack ();
    r
  with Db.Value.Aborted as e -> 
    pop_call_stack ();
    raise e

let compute_recursive_call kf ~call_kinstr state actuals =
  push_call_stack kf call_kinstr;
  let with_formals =
    Function_args.actualize_formals
      ~check:Function_args.check_arg_size kf state actuals
  in
  (* For formals that might be referenced, we must perform a join with the
     previous values *)
  let with_formals =
    Function_args.merge_referenced_formals kf state with_formals
  in
  Db.Value.merge_initial_state (call_stack ()) with_formals;
  let initial_spec = Annotations.funspec ~populate:false kf in
  let assigns_spec () =
    let assigns = Infer_annotations.assigns_from_prototype kf in
    let bhv = Cil.mk_behavior ~assigns:(Writes assigns) () in
    { (Cil.empty_funspec ()) with spec_behavior = [bhv] }, assigns
    in
  let spec = match Cil.find_default_behavior initial_spec with
      | Some bhv when bhv.b_assigns <> WritesAny -> initial_spec
      | _ ->
          let spec, assigns = assigns_spec () in
          Value_parameters.error ~once:true
            "@[Recursive@ call@ on@ an unspecified@ \
              function.@ Using@ potentially@ invalid@ inferred assigns '%t'@]"
            (fun fmt -> match assigns with
               | [] -> Format.pp_print_string fmt "assigns \\nothing"
               | _ :: _ ->
                   Pretty_utils.pp_list ~sep:"@ " Printer.pp_from fmt assigns);
          (* Merge existing spec into our custom one with assigns *)
          Logic_utils.merge_funspec
            ~silent_about_merging_behav:true spec initial_spec;
          spec
  in
  let r = compute_using_specification (kf, spec) ~call_kinstr ~with_formals() in
  pop_call_stack ();
  (* Restore one formal of [kf] to a correct value: either the value before
     the call if the variable cannot have been modified during this call,
     or a sound approximation *)
  let restore_formal post_state vi =
    let b = Base.of_varinfo vi in
    let old = Cvalue.Model.find_base b state (* Value in previous calls *) in
    let offsm =
      if vi.vaddrof then
        (* Any copy of the formal may have been modified by the call, join
           the possible values *)
        let post = Cvalue.Model.find_base b post_state in
        let r = Bottom.Top.join Cvalue.V_Offsetmap.join old post in
        r
      else
        old
    in
    match offsm with
    | `Top | `Bottom -> assert false
    | `Value offsm -> Cvalue.Model.add_base b offsm post_state
  in
  let formals = Kernel_function.get_formals kf in
  let restore_formals state = List.fold_left restore_formal state formals in
  let restore =
    List.map (fun (retres, state) -> (retres, restore_formals state))
  in
  { r with Value_types.c_values = restore r.Value_types.c_values }


(* Compute a call to [kf], called from [call_kinstr], in the state [state]. In
   this state, the value of actuals in [actuals] are not yet bound to formals.
   [recursive] means that the call is recursive. *)
let compute_call kf ~recursive ~call_kinstr state actuals =
  if recursive then
    compute_recursive_call kf ~call_kinstr state actuals
  else
    compute_non_recursive_call kf ~call_kinstr state actuals


let () = Eval_stmt.compute_call_ref := compute_call

let floats_ok () =
  let u = min_float /. 2. in
  let u = u /. 2. in
  0. < u && u < min_float

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
  let check_assigns kf =
    if need_assigns kf then
      Value_parameters.error "@[no assigns@ specified@ for function '%a',@ for \
        which@ a builtin@ or the specification@ will be used.@ \
        Potential unsoundness.@]" Kernel_function.pretty kf
  in
  Value_parameters.BuiltinsOverrides.iter (fun (kf, _) -> check_assigns kf);
  Value_parameters.UsePrototype.iter (fun kf -> check_assigns kf);
    
;;

(* Preliminary checks before Value starts *)
let check () =
  assert (floats_ok ());
  options_ok ();
  Split_return.pretty_strategies ();
;;

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


let pre () =
  generate_specs ();
  Value_perf.reset();
  (* We may be resuming Value from a previously crashed analysis. Clear
     degeneration states *)
  Value_util.DegenerationPoints.clear ();
  Cvalue.V.clear_garbled_mix ();
;;

let post_cleanup ~aborted =
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
  end;
;;

(* Register a signal handler for SIGUSR1, that will be used to abort Value *)
let () =
  let prev = ref (fun _ -> ()) in
  let handler (_signal: int) =
    !prev Sys.sigusr1; (* Call previous signal handler *)
    Value_parameters.warning "Stopping analysis at user request@.";
    Eval_slevel.signal_abort ()
  in
  try
    match Sys.signal Sys.sigusr1 (Sys.Signal_handle handler) with
    | Sys.Signal_default | Sys.Signal_ignore -> ()
    | Sys.Signal_handle f -> prev := f
  with Invalid_argument _ -> () (* Ignore: SIGURSR1 is not available on Windows,
                                   and possibly on other platforms. *)


let force_compute () =
  Ast.compute ();
  check ();
  try
    pre ();
    ignore (compute_from_entry_point ());
    Db.Value.mark_as_computed ();
    (* Mark unreachable and RTE statuses. Only do this there, not when the
       analysis was aborted (hence, not in post_cleanup), because the
       propagation is incomplete. Also do not mark unreachable statutes if
       there is an alarm in the initialisers (bottom initial state), as we
       would end up marking the alarm as dead. *)
    if (Cvalue.Model.is_reachable (Db.Value.globals_state ()))
    then Eval_annots.mark_unreachable ()
    else Eval_annots.mark_invalid_initializers ();
    Value_util.dump_garbled_mix ();
    (* Try to refine the 'Unknown' statuses that have been emitted during
       this analysis. *)
    Eval_annots.mark_green_and_red ();
    Eval_annots.mark_rte ();
    post_cleanup ~aborted:false;
    (* Remove redundant alarms *)
    if Value_parameters.RmAssert.get() then !Db.Scope.rm_asserts ()
  with
  | Db.Value.Aborted ->
      Db.Value.mark_as_computed ();
      post_cleanup ~aborted:true;
      (* Signal that a degeneration occurred *)
      if Value_util.DegenerationPoints.length () > 0 then
        Value_parameters.error
          "Degeneration occurred:@\nresults are not correct for lines of code \
that can be reached from the degeneration point.@."
  | Globals.No_such_entry_point _ as exn -> raise exn
  | exn -> Db.Value.mark_as_computed (); raise exn


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
