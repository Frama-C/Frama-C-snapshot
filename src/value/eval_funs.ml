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

(** Value analysis of entire functions *)

open Cil_types
open Cil
open Value_util

let dkey = Value_parameters.register_category "callbacks"


(** Compute [kf] in state [with_formals] according to the body [f] of [kf].
    Checks the preconditions of [kf], assuming the call took place at
    [call_kinstr]. The postconditions are checked within the call to
    [Computer.compute]. *)
let compute_using_body (kf, f) ~call_kinstr ~with_formals =
  let with_locals =
    List.fold_left
      (fun acc local ->
        Initial_state.add_unitialized acc (Locations.loc_of_varinfo local)
      ) with_formals f.slocals
  in
  (* Remark: the pre-condition cannot talk about the locals. BUT
     check_fct_preconditions split the state into a stateset, hence
     it is simpler to apply it to the (unique) state with locals *)
  let ab = Eval_annots.ActiveBehaviors.create with_locals kf in
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
  let module Compute = Dataflow2.Forwards(Computer) in
  let start = Kernel_function.find_first_stmt kf in
  (* Init the dataflow state for the first statement *)
  let dinit = { Computer.to_propagate = with_locals} in
  let dinit = Computer.computeFirstPredecessor start dinit in
  Computer.StmtStartData.add start dinit;
  begin
    try  Compute.compute [start]
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


(** Evaluate the assigns of [kf] active according to [active_behaviors] in
    the state [with_formals]. *)
let compute_assigns kf ab return_used ~with_formals =
  let with_alarms = CilE.warn_none_mode in
  let vi = Kernel_function.get_vi kf in
  if (not (Cvalue.Model.is_reachable with_formals)) ||
    Cil.hasAttribute "noreturn" vi.vattr
  then
    None, Cvalue.Model.bottom, Base.SetLattice.bottom
  else
    let behaviors = Eval_annots.ActiveBehaviors.active_behaviors ab in
    let assigns = Ast_info.merge_assigns behaviors in
    let returned_value, with_formals =
      Library_functions.returned_value kf with_formals
    in
    let returned_value = ref returned_value in
    let clob = Locals_scoping.bottom () in
    let env = Eval_terms.env_assigns with_formals in
    (* Treat one assign ... \from ... clause. Update [state] accordingly,
       as well as [returned_value] and [clobbered_set] *)
    let pp_eval_error fmt e =
      if e <> Eval_terms.CAlarm then
        Format.fprintf fmt "@ (%a)" Eval_terms.pretty_logic_evaluation_error e
    in
    let treat_assign state ({it_content = out}, ins as asgn) =
      (* Evaluate the contents of one element of the from clause, topify them,
         and add them to the current state of the evaluation in acc *)
      let one_from_contents acc { it_content = t } =
        let r = Eval_terms.eval_term ~with_alarms env t in
        List.fold_left
          (fun acc v -> Cvalue.V.join acc (Cvalue.V.topify_arith_origin v))
          acc r.Eval_terms.evalue
      in
      (* evaluation of the entire from clause *)
      let froms_contents =
        match ins with
          | FromAny -> Cvalue.V.top_int
          | From l ->
            try
              List.fold_left one_from_contents Cvalue.V.top_int l
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
          (Value_parameters.warning ~current:true ~once:true
             "@[Completely invalid destination@ for assigns@ clause %a.@ \
                 Ignoring.@]" Printer.pp_term out;
           acc)
        else (
          Locals_scoping.remember_if_locals_in_value clob loc froms_contents;
	  let state' = Cvalue.Model.add_binding ~with_alarms
            ~exact:false acc loc froms_contents
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
          let locs, _deps =
            Eval_terms.eval_tlval_as_locations ~with_alarms env out
          in
          List.fold_left treat_output_loc state locs
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
        let rvi, state =
          Library_functions.add_retres_to_state ~with_alarms kf offsetmap state
        in
        Some rvi, state
    in
    retres_vi, state, clob.Locals_scoping.clob


(** Evaluate [kf] in state [with_formals], first by reducing by the
    preconditions, then by evaluating the assigns, then by reducing
    by the post-conditions. The resulting states contain formals only
    if [clear_formals] is false. *)
let compute_using_specification (kf, spec) ?(clear_formals=true) ~call_kinstr ~with_formals () =
  if Value_parameters.InterpreterMode.get()
  then begin
    warning_once_current "Library function call. Stopping.";
    exit 0
  end;
  Value_parameters.feedback ~once:true "@[using specification for function %a@]"
    Kernel_function.pretty kf;
  let ab = Eval_annots.ActiveBehaviors.create_from_spec with_formals spec in
  let stateset =
    Eval_annots.check_fct_preconditions kf ab call_kinstr with_formals in
  (* TODO: This is a hack. Use a function that checks preconditions without
     multiplying the states instead -- or compute_assigns several times, while
     taking behaviors into account *)
  let (with_formals,trace) = State_set.join stateset in
  let return_used = match call_kinstr with
    | Kglobal -> true
    | Kstmt {skind = Instr (Call (lv, _, _, _))} -> lv <> None
    | _ -> assert false
  in
  let retres_vi, result_state, sclob =
    compute_assigns kf ab return_used ~with_formals
  in
  let result_state =
    Eval_annots.check_fct_postconditions kf ab
      ~result:retres_vi ~init_state:with_formals
      ~post_states:(State_set.singleton (result_state,trace))
      Normal
  in
  let aux state =
    let ret, state = match retres_vi with
      | None -> None, state
      | Some vi ->
        if not (Cvalue.Model.is_reachable state) then 
          (* This test prevents the call to Model.find_base that would
             raise Not_found in this case. *)
          None, state
        else
          let retres_base = Base.of_varinfo vi in
          let without_ret = Cvalue.Model.remove_base retres_base state in
          (Some (Cvalue.Model.find_base retres_base state)),
          without_ret
    in
    if clear_formals then
      let formals = Kernel_function.get_formals kf in
      let without = Value_util.remove_formals_from_state formals state in
      ret, without
    else
      ret, state
  in
  { Value_types.c_values = List.map aux (State_set.to_list result_state);
    c_clobbered = sclob;
    c_cacheable = Value_types.Cacheable;
  }


(** Compute a call to [kf] in the state [with_formals]. The evaluation will
    be done either using the body of [kf] or its specification, depending
    on whether the body exists and on option [-val-use-spec]. [call_kinstr]
    is the instruction at which the call takes place, and is used to update
    the statuses of the preconditions of [kf]. If [show_progress] is true,
    the callstack and additional information are printed. *)
let compute_using_spec_or_body ~with_formals ~call_kinstr ~show_progress kf =
  Kf_state.mark_as_called kf;
  let pp = show_progress && Value_parameters.ValShowProgress.get() in
  let entry_time = if pp then Unix.time () else 0. in
  if pp then
    Value_parameters.feedback
      "@[computing for function %a.@\nCalled from %a.@]"
      pretty_call_stack_short (call_stack ())
      Cil_datatype.Location.pretty
	 (Cil_datatype.Kinstr.loc (CilE.current_stmt()));
  let use_spec = match kf.fundec with
    | Declaration (_,_,_,_) -> `Spec (Annotations.funspec kf)
    | Definition (def, _) ->
        if Datatype.String.Set.mem
          def.svar.vname (Value_parameters.UsePrototype.get ())
        then `Spec (Annotations.funspec kf)
        else `Def def
  in
  let result =  match use_spec with
    | `Spec spec ->
        compute_using_specification (kf, spec) ~call_kinstr ~with_formals ()
    | `Def f ->
        compute_using_body (kf, f) ~call_kinstr ~with_formals
  in
  if pp then begin
    let compute_time = (Unix.time ()) -. entry_time in
      if compute_time > Value_parameters.FloatTimingStep.get ()
      then Value_parameters.feedback "Done for function %a, in %a seconds."
             Kernel_function.pretty kf
             Datatype.Float.pretty  compute_time
      else Value_parameters.feedback "Done for function %a"
             Kernel_function.pretty kf
  end;
  result


(** Compute a call to the main function. The initial state is generated
    according to options such as [-lib-entry] and the options of Value governing
    the shape of this state. *)
let compute_from_entry_point () =
  let kf, library = Globals.entry_point () in
  clear_call_stack ();
  Stop_at_nth.clear ();
  Kf_state.mark_as_called kf;
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
    else (
      Value_parameters.feedback "Computing initial state";
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state computed";
      if Value_parameters.ValShowInitialState.get ()
      then
	Value_parameters.printf
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
    Mark_noresults.run();
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
    Db.Value.merge_initial_state kf with_formals;
    push_call_stack kf Kglobal;
    Db.Value.Call_Value_Callbacks.apply (with_formals, [ kf, Kglobal ]);
    ignore(compute_using_spec_or_body kf
             ~call_kinstr:Kglobal ~with_formals ~show_progress:false);
    Value_parameters.feedback "done for function %a" Kernel_function.pretty kf;
    Separate.epilogue();
  end


(** Compute a call to a possible builtin [kf] in state [state]. [actuals] are
    the arguments of [kf], and have not been bound to its formals. Returns
    [None] if the call must be computed using the Cil function for [kf]. *)
let compute_maybe_builtin kf ~state actuals =
  (* Actuals for builtins include a Cvalue.V arg, which is more convenient
     than the entire offsetmap *)
  let conv_arg with_alarms (exp, offsm) =
    let typ = typeOf exp in
    CilE.set_syntactic_context (CilE.SyUnOp exp);
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
  try
    let name, override =
      (* Advanced builtins which override a Cil function with a Caml one, but
         use the Cil one as backup if the Caml one fails. (None by default) *)
      try
        let name = Value_parameters.BuiltinsOverrides.find name in
        (* This is an interesting C function. Mark it as called, otherwise
           it would get skipped, eg. from the Gui. *)
        Kf_state.mark_as_called kf;
        name, true
      with Not_found -> name, false
    in
    (* Standard builtins with constant names, e.g. Frama_C_cos *)
    let abstract_function = Builtins.find_builtin name in
    (try
       Some (abstract_function state !!actuals)
     with Db.Value.Outside_builtin_possibilities ->
       if override then None
       else (
         Value_parameters.warning ~once:true ~current:true
           "Call to builtin %s failed, aborting." name;
         raise Db.Value.Aborted
       )
    )
  with Not_found ->
    (* Special builtins, such as Frama_C_show_each_foo *)
    if Ast_info.can_be_cea_function name then
      (* A few special functions that are not registered in the builtin table *)
      if Ast_info.is_cea_dump_function name then
        Some (Builtins.dump_state state !!actuals)
      else if Ast_info.is_cea_function name then
        Some (Builtins.dump_args name state !!actuals)
      else if Ast_info.is_cea_dump_file_function name then
        Some (Builtins.dump_state_file name state !!actuals)
      else
        None
    else None

(** Compute a call to [kf] from [call_kinstr], assuming [kf] is not yet present
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
  Db.Value.merge_initial_state kf with_formals;
  try 
    let stack_with_call = call_stack () in
    Db.Value.Call_Value_Callbacks.apply (with_formals, stack_with_call);
    let default () = 
      let r = compute_maybe_builtin kf ~state actuals in
      match r with
	| Some r -> r
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
            (* Evaluate the preconditions of kf, to update the statuses
               at this call. *)
            if Eval_annots.has_requires kf then begin
              let ab = Eval_annots.ActiveBehaviors.create with_formals kf in
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
  (* Update formals. For formals that might be referenced, we must perform
     a join with the previous values *)
  let exact vi = not vi.vaddrof in
  let with_formals =
    Function_args.actualize_formals
      ~check:Function_args.check_arg_size ~exact kf state actuals
  in
  Db.Value.merge_initial_state kf with_formals;
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
  let r = compute_using_specification (kf, spec)
    ~clear_formals:false ~call_kinstr ~with_formals ()
  in
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
        let r = Cvalue.V_Offsetmap.join old post in
        r
      else
        old
    in
    Cvalue.Model.add_base b offsm post_state
  in
  let formals = Kernel_function.get_formals kf in
  let restore_formals state = List.fold_left restore_formal state formals in
  let restore =
    List.map (fun (retres, state) -> (retres, restore_formals state))
  in
  { r with Value_types.c_values = restore r.Value_types.c_values }


(** Compute a call to [kf], called from [call_kinstr], in the state [state]. In
   this state, the value of actuals in [actuals] are not yet bound to formals.*)
let compute_call kf ~call_kinstr state actuals =
  if Warn.check_no_recursive_call kf then
    compute_non_recursive_call kf ~call_kinstr state actuals
  else
    compute_recursive_call kf ~call_kinstr state actuals



let () = Eval_stmt.compute_call_ref := compute_call

let floats_ok () =
  let u = min_float /. 2. in
  let u = u /. 2. in
  0. < u && u < min_float

let options_ok () =
  (* Check that we can parse the values specified for the options that require
     advanced parsing. Just make a query, as this will force the kernel to
     parse them. *)
  let check f = try ignore (f "") with Not_found -> () in
  check Value_parameters.SplitReturnFunction.find;
  check Value_parameters.BuiltinsOverrides.find;
  check Value_parameters.SlevelFunction.find;
;;

(* Preliminary checks before Value starts *)
let check () =
  assert (floats_ok ());
  options_ok ();
;;


(* Do something tasteless in case the user did not put a spec on functions
   for which he set [-val-use-spec]:  generate an incorrect one ourselves *)
let generate_specs () =
  let aux kf_name =
    try
      let kf = Globals.Functions.find_by_name kf_name in
      let spec = Annotations.funspec kf in
      let open Cil_types in
      let need_generation =
        match Cil.find_default_behavior spec with
        | None -> true
        | Some bhv -> bhv.b_assigns = WritesAny
      in
      if need_generation then begin
        Value_parameters.warning "Generating potentially incorrect assigns \
          for function '%s' for which option %s is set"
          kf_name Value_parameters.UsePrototype.option_name;
        (* The function populate_spec may emit a warning. Position a loc. *)
        Cil.CurrentLoc.set (Kernel_function.get_location kf);
        ignore (!Annotations.populate_spec_ref kf spec)
      end
    with Not_found ->
      Value_parameters.error "Unknown function '%s' for option %s"
        kf_name Value_parameters.UsePrototype.option_name
  in
  Value_parameters.UsePrototype.iter aux


let pre () =
  generate_specs ();
  Value_perf.reset();
  (* We may be resuming Value from a previously crashed analysis. Clear
     degeneration states *)
  Value_util.DegenerationPoints.clear ();
;;

let post_cleanup ~aborted =
  if not aborted then begin
    (* Keep memexec results for users that want to resume the analysis *)
    Mem_exec.cleanup_results ();
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
    (* Mark unreachable and RTE statuses. Do not do this in post_cleanup,
       as all reachable code has not been analysed. *)
    Eval_annots.mark_unreachable ();
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


let _self =
  Db.register_compute "Value.compute"
    [ Db.Value.self ]
    Db.Value.compute
    force_compute

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
