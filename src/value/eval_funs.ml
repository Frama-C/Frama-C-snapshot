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

(** Value analysis of entire functions *)

open Cil_types
open Cil
open Locations
open Abstract_interp
open Cvalue
open Value_util
open Locals_scoping


(** Check that [kf] is not already present in the call stack *)
let check_recursive_call kf =
  List.iter
    (function {called_kf = g} ->
      if kf == g
      then begin
        if Value_parameters.IgnoreRecursiveCalls.get()
        then begin
          warning_once_current
            "ignoring recursive call during value analysis of %a (%a <- %a)"
            Kernel_function.pretty kf Kernel_function.pretty kf
            pretty_call_stack (call_stack ());
          Db.Value.recursive_call_occurred kf;
          raise Eval_stmts.Recursive_call
        end
        else begin
	  warning_once_current "@[@[detected@ recursive@ call@ (%a <- %a)@]@;@[Use %s@ to@ ignore@ (beware@ this@ will@ make@ the analysis@ unsound)@]@]"
            Kernel_function.pretty kf
            pretty_call_stack (call_stack ())
            Value_parameters.IgnoreRecursiveCalls.option_name;
          Value_parameters.not_yet_implemented "recursive call"

        end
      end)
    (call_stack ())

let compute_using_cfg (kf, f) initial_states =
  let module Computer =
    Eval_stmts.Computer
      (struct
        let kf = kf
        let slevel = get_slevel kf
        let initial_states = initial_states (* for future reference *)
        let active_behaviors =
          Eval_annots.ActiveBehaviors.create initial_states kf
        let local_slevel_info = Local_slevel_types.empty_info ()
       end)
  in
  let module Compute = Dataflow.Forwards(Computer) in

  let add_to_worklist stmt = Queue.add stmt Compute.worklist in
  Computer.add_to_worklist := add_to_worklist;

  call_stack_set_merge_current Computer.merge_current;

  let start = Kernel_function.find_first_stmt kf in
  (* Init the dataflow state for the first statement *)
  let dinit = { Computer.counter_unroll = 0; value = initial_states} in
  let dinit = Computer.computeFirstPredecessor start dinit in
  Computer.StmtStartData.add start dinit;

  begin
    try  Compute.compute [start]
    with Db.Value.Aborted as e ->
      (* State_builder.was aborted: pop the call stack and inform the caller *)
      pop_call_stack ();
      raise e
  end;

  let final_states =
    let final_states = Computer.finalStates () in
    Computer.mergeResults () (* Merge consolidated results, call callbacks *);
    let states,_ as result = Computer.externalize final_states in
    (match states with
      | _ :: _ when  hasAttribute "noreturn" f.svar.vattr ->
        warning_once_current
          "function %a may terminate but has the noreturn attribute"
          Kernel_function.pretty kf;
      | _ -> ());
    result
  in
  final_states

let compute_using_prototype kf ~active_behaviors ~state_with_formals =
  let with_alarms = CilE.warn_none_mode in
  let vi = Kernel_function.get_vi kf in
  if (not (Cvalue.Model.is_reachable state_with_formals)) ||
    Cil.hasAttribute "noreturn" vi.vattr
  then
    None, Cvalue.Model.bottom, Location_Bits.Top_Param.bottom
  else
    let behaviors =
      Eval_annots.ActiveBehaviors.active_behaviors active_behaviors in
    let assigns = Ast_info.merge_assigns behaviors in
    let returned_value, state_with_formals =
      Library_functions.returned_value kf state_with_formals
    in
    let returned_value = ref returned_value in
    let clobbered_set = ref Location_Bits.Top_Param.bottom in
    let env = Eval_terms.env_pre_f state_with_formals in

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
        (* TODO: catch errors in evaluation of the lval *)
        let locs = Eval_terms.eval_tlval_as_locations ~with_alarms env None t in
        List.fold_left
          (fun acc input_loc ->
            let cur = Cvalue.Model.find ~with_alarms
              ~conflate_bottom:true state_with_formals input_loc
            in
            Cvalue.V.join acc (Cvalue.V.topify_arith_origin cur)
          ) acc locs
      in
      (* evaluation of the entire from clause *)
      let froms_contents =
        match ins with
          | FromAny -> Cvalue.V.top_int
          | From l ->
            try
              List.fold_left one_from_contents Cvalue.V.top_int l
            with Eval_terms.LogicEvalError e ->
	      warning_once_current "cannot interpret@ 'from' clause@ in \
                assigns %a@ in function %a%a" Cil.d_from asgn
                Kernel_function.pretty kf pp_eval_error e;
              Cvalue.V.top
      in
      (* Treat one location coming from the evaluation of [out] *)
      let treat_output_loc acc loc =
        let valid = Locations.valid_part ~for_writing:true loc in
        if Location_Bits.equal Location_Bits.bottom valid.loc then
          (Value_parameters.warning ~current:true ~once:true
             "@[Completely invalid destination@ for assigns@ clause %a.@ \
                 Ignoring.@]" d_term out;
           acc)
        else (
          remember_bases_with_locals clobbered_set loc froms_contents;
	  let state' = Cvalue.Model.add_binding ~with_alarms
            ~exact:false acc loc froms_contents
	  in
          if Cvalue.Model.equal Cvalue.Model.top state' then (
            Value_parameters.error ~once:true ~current:true
              "Cannot@ handle@ assigns@ for %a,@ location@ is@ too@ imprecise@ \
                 (%a).@ Assuming@ it@ is@ not@ assigned,@ but@ be@ aware@ this\
                 @ is@ incorrect." d_term out Locations.pretty loc;
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
          let locs =
            Eval_terms.eval_tlval_as_locations ~with_alarms env None out in
          List.fold_left treat_output_loc state locs
        with
          | Eval_terms.LogicEvalError e ->
            warning_once_current
              "cannot interpret assigns %a@ in function %a%a; effects will be \
                 ignored"
              Cil.d_term out Kernel_function.pretty kf pp_eval_error e;
            state
    in
    (* Treat all the assigns for the function *)
    let state =
      match assigns with
        | WritesAny ->
            warning_once_current "Cannot handle empty assigns clause. Assuming assigns \\nothing: be aware this is probably incorrect.";
            state_with_formals
        | Writes l -> List.fold_left treat_assign state_with_formals l
    in
    let retres_vi, state =
      let return_type = getReturnType vi.vtype in
      if isVoidType return_type
      then None, state
      else
        let offsetmap =
          V_Offsetmap.update_ival
            ~with_alarms
            ~validity:Base.All
            ~offsets:Ival.zero
            ~exact:true
            ~size:(Int.of_int (bitsSizeOf return_type))
            V_Offsetmap.empty
            (Cvalue.V_Or_Uninitialized.initialized !returned_value)
        in
        let rvi, state =
          Library_functions.add_retres_to_state ~with_alarms kf offsetmap state
        in
        Some rvi, state
    in
    retres_vi, state, !clobbered_set


let compute_using_declaration kf kinstr with_formals =
  Kf_state.mark_as_called kf;
    Value_parameters.feedback ~once:true "@[using specification for function %a@]"
      Kernel_function.pretty kf;
  let stateset =
    Eval_annots.check_fct_preconditions kf kinstr with_formals in
  (* TODO: This is a hack. Use a function that checks preconditions without
   multiplying the states instead -- or compute_using_prototype several times *)
  let active_behaviors = Eval_annots.ActiveBehaviors.create stateset kf in
  let state_with_formals = State_set.join stateset in
  let retres_vi, result_state, thing =
    compute_using_prototype kf ~active_behaviors ~state_with_formals in
  let result_state =
    Eval_annots.check_fct_postconditions ~result:retres_vi kf ~active_behaviors
      ~init_state:(State_set.singleton state_with_formals)
      ~post_state:(State_set.singleton result_state)
      Normal
  in
  let aux state =
    let ret, with_formals = match retres_vi with
      | None -> None, state
      | Some vi ->
        if not (Cvalue.Model.is_reachable state) then 
          (* This test prevents the call to Model.find_base that would
             raise Not_found in this case. *)
          None, state
        else
          let retres_base = Base.create_varinfo vi in
          let without_ret = Cvalue.Model.remove_base retres_base state in
          (Some (Cvalue.Model.find_base retres_base state)),
          without_ret
    in
    let formals = Kernel_function.get_formals kf in
    let without = Value_util.remove_formals_from_state formals with_formals in
    ret, without
  in
  List.map aux (State_set.to_list result_state),
  thing

(* In the state [initial_state] globals and formals are present
   but locals of [kf] are not.*)
let compute_with_initial_state ~call_kinstr kf with_formals =
  match kf.fundec with
    | Declaration _ -> compute_using_declaration kf call_kinstr with_formals
    | Definition (f,_) ->
        let with_locals =
          List.fold_left
            (fun acc local ->
              Cvalue.Model.add_binding_not_initialized
                 acc
                 (Locations.loc_of_varinfo local))
            with_formals
            f.slocals
        in
        (* Remark: the pre-condition cannot talk about the locals. BUT
           check_fct_preconditions split the state into a stateset, hence
           it is simpler to apply it to the (unique) state with locals *)
        let initial_states =
          Eval_annots.check_fct_preconditions kf call_kinstr with_locals in
        compute_using_cfg (kf, f) initial_states

let compute_entry_point kf ~library =
  clear_call_stack ();
  Kf_state.mark_as_called kf;
  Value_parameters.feedback "Analyzing a%scomplete application starting at %a"
    (if library then "n in" else " ")
    Kernel_function.pretty kf;

  Separate.prologue();

  let initial_state_globals =
    if Db.Value.globals_use_supplied_state () then (
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state supplied by user";
      Value_parameters.debug "@[<hov 0>Values of globals@\n%a@]"
        Db.Value.pretty_state_without_null r;
      r)
    else (
      Value_parameters.feedback "Computing initial state";
      let r = Db.Value.globals_state () in
      Value_parameters.feedback "Initial state computed";
      Value_parameters.result
        "@[<hov 0>Values of globals at initialization@\n%a@]"
        Db.Value.pretty_state_without_null r;
      r
    ) in
  if not (Db.Value.is_reachable initial_state_globals) 
  then begin
    Value_parameters.result "Value analysis not started because globals \
                               initialization is not computable.";
    [None, initial_state_globals], Locations.Location_Bits.Top_Param.empty
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
            (), a, Cvalue_convert.offsetmap_of_value ~typ:f.vtype a
          in
          Function_args.actualize_formals
            kf
            initial_state_globals
            (List.map2 treat_one_formal formals actuals)
    in
    push_call_stack kf Kglobal;
    Db.Value.Call_Value_Callbacks.apply (with_formals, [ kf, Kglobal ]);
    let result =
      compute_with_initial_state kf ~call_kinstr:Kglobal with_formals
    in
    Value_parameters.feedback "done for function %a"
      Kernel_function.pretty kf;
    Separate.epilogue();
    result
  end

let compute_call_to_cil_function kf with_formals call_kinstr =
  let print_progress = Value_parameters.ValShowProgress.get() in
  let entry_time = if print_progress then Unix.time () else 0. in
  if print_progress then
    Value_parameters.feedback
      "@[computing for function %a.@\nCalled from %a.@]"
      pretty_call_stack (call_stack ())
      pretty_loc_simply (CilE.current_stmt());
  let result = match kf.fundec with
    | Declaration (_,_,_,_) ->
        compute_using_declaration kf call_kinstr with_formals
    | Definition (def, _) ->
        Kf_state.mark_as_called kf;
        if Datatype.String.Set.mem
          def.svar.vname (Value_parameters.UsePrototype.get ())
        then
          compute_using_declaration kf call_kinstr with_formals
        else
          compute_with_initial_state kf ~call_kinstr with_formals
  in
  if print_progress then begin
    let  compute_time = (Unix.time ()) -. entry_time in
      if compute_time > Value_parameters.FloatTimingStep.get ()
      then Value_parameters.feedback "Done for function %a, in %a seconds."
             Kernel_function.pretty kf
             Datatype.Float.pretty  compute_time
      else Value_parameters.feedback "Done for function %a"
             Kernel_function.pretty kf
  end;
  result

(* Compute a call to a possible builtin. Returns [Some result], or [None]
   if the call must be computed using the Cil function *)
let compute_call_to_builtin kf initial_state actuals =
  let name = Kernel_function.get_name kf in
  try
    let name, override =
      (* Advanced builtins which override a Cil function with a Caml one, but
         use the Cil one as backup if the Caml one fails. (None by default) *)
      try
        let name = Value_parameters.BuiltinsOverrides.find name in
        Kf_state.mark_as_called kf;
        name, true
      with Not_found -> name, false
    in
    (* Standard builtins with constant names, e.g. Frama_C_cos *)
    let abstract_function = Builtins.find_builtin name in
    (try
       Some (abstract_function initial_state actuals)
     with Db.Value.Outside_builtin_possibilities ->
       if override then None
       else (
         Value_parameters.warning ~once:true ~current:true
           "Call to builtin %s failed, aborting." name;
         do_degenerate None;
         raise Db.Value.Aborted
       )
    )
  with Not_found ->
    (* Special builtins, such as Frama_C_show_each_foo *)
    if Ast_info.can_be_cea_function name then
      (* A few special functions that are not registered in the builtin table *)
      if Ast_info.is_cea_dump_function name then
        Some (Builtins.dump_state initial_state actuals)
      else if Ast_info.is_cea_function name then
        Some (Builtins.dump_args name initial_state actuals)
      else if Ast_info.is_cea_dump_file_function name then
        Some (Builtins.dump_state_file name initial_state actuals)
      else
        None
    else None


let compute_call kf ~call_kinstr initial_state actuals =
  let with_formals =
    Function_args.actualize_formals
      ~check:Function_args.check_arg_size kf initial_state actuals
  in
  Db.Value.merge_initial_state kf with_formals;
  check_recursive_call kf;
  push_call_stack kf call_kinstr;
  let stack_with_call = for_callbacks_stack () in
  Db.Value.Call_Value_Callbacks.apply (with_formals, stack_with_call);
  let default () = 
    let r = compute_call_to_builtin kf initial_state actuals in
    match r with
      | Some r -> r.Db.Value.builtin_values, r.Db.Value.builtin_clobbered
      | None -> compute_call_to_cil_function kf with_formals call_kinstr
  in
  let r =
    if Value_parameters.MemExecAll.get () then
    match Mem_exec.reuse_previous_call (kf, call_kinstr) with_formals with
      | None ->
          let res = default () in
          Mem_exec.store_computed_call (kf, call_kinstr) with_formals res;
          res
      | Some (res, i) ->
          if Value_parameters.ValShowProgress.get () then begin
            Value_parameters.feedback ~current:true
              "Reusing old results for call to %a" Kernel_function.pretty kf;
            Value_parameters.debug ~dkey:"callbacks"
              "calling Record_Value_New callbacks on saved previous result";
          end;
          Db.Value.Record_Value_Callbacks_New.apply
            (stack_with_call, Value_aux.Reuse i);
          res
    else
      default ()
  in
  pop_call_stack ();
  r
;;


let () = Eval_stmts.compute_call_ref := compute_call

let floats_ok () =
  let u = min_float /. 2. in
  let u = u /. 2. in
  0. < u && u < min_float

let cleanup () = 
  StmtCanReachCache.clear ();
  Mem_exec.cleanup_results ();
;;

let force_compute () =
  assert (floats_ok ());
  try
    let kf, library = Globals.entry_point () in
    ignore (compute_entry_point kf ~library);
    Db.Value.mark_as_computed ();
    cleanup ();
    (* Remove redundant alarms *)
    if Value_parameters.RmAssert.get() then !Db.Scope.rm_asserts ()
  with
  | Db.Value.Aborted ->
      cleanup ();
      (* This case is reached only if [do_degenerate] did not raise another
         exception to handle abortion properly. See the behavior of the GUI
         in case of degeneration to understand the machinery. *)
      Db.Value.mark_as_computed ();
      Value_parameters.abort
        "Degeneration occured:@\nresults are not correct for lines of code \
that can be reached from the degeneration point."
  | Globals.No_such_entry_point _ as exn -> raise exn
  | exn -> Db.Value.mark_as_computed (); raise exn

let _self =
  Db.register_compute "Value.compute"
    [ Db.Value.self ]
    Db.Value.compute
    (fun () ->
      if not (Db.Value.is_computed ()) then (force_compute ());
(* Mark unreachable annotations here, independently of whether Value has just
   computed something. This way, if a plugin dynamically add dead annotations,
   Value will flag them as such *)
(*      Eval_annots.mark_unreachable (); *)
(*      Eval_annots.mark_rte (); *)
    )


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
