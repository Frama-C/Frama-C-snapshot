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
open Eval

let dkey = Value_parameters.register_category "callbacks"


module Make
    (Val: Abstract_value.S)
    (Loc: Abstract_location.External with type value = Val.t)
    (Dom: Abstract_domain.External with type location = Loc.location
                                    and type value = Val.t)
    (Eva: Evaluation.S with type value = Dom.value
                        and type origin = Dom.origin
                        and type loc = Dom.location
                        and type state = Dom.t)
    (Init: Initialization.S with type state := Dom.t)
= struct

  module Domain = struct
    include Dom
    let enter_scope kf vars state = match vars with
      | [] -> state
      | _ ->  enter_scope kf vars state
    let leave_scope kf vars state = match vars with
      | [] -> state
      | _ ->  leave_scope kf vars state
  end
  module States = Partitioning.Make_Set (Domain)
  module Domain_Transfer = struct
    include Domain.Transfer (Eva.Valuation)
    let leave_scope = Domain.leave_scope
    module Store = Domain.Store
    include (Domain : Datatype.S with type t = state)
  end
  module Transfer = Transfer_stmt.Make (Val) (Loc) (Domain_Transfer) (Eva)
  module Logic = Transfer_logic.Make (Domain) (Partitioning.Make_Set (Domain))

  module Computer =
    Partitioned_dataflow.Computer (Domain) (States) (Transfer) (Logic)

  let get_cvalue =
    match Domain.get Cvalue_domain.key with
    | None -> fun _ -> Cvalue.Model.top
    | Some get -> fun state -> get state

  (* Compute a call to [kf] in the state [state]. The evaluation will
     be done either using the body of [kf] or its specification, depending
     on whether the body exists and on option [-val-use-spec]. [call_kinstr]
     is the instruction at which the call takes place, and is used to update
     the statuses of the preconditions of [kf]. If [show_progress] is true,
     the callstack and additional information are printed. *)
  let compute_using_spec_or_body call_kinstr kf state =
    Value_results.mark_kf_as_called kf;
    let global = match call_kinstr with Kglobal -> true | _ -> false in
    let pp = not global && Value_parameters.ValShowProgress.get () in
    if pp then
      Value_parameters.feedback
        "@[computing for function %a.@\nCalled from %a.@]"
        Value_types.Callstack.pretty_short (Value_util.call_stack ())
        Cil_datatype.Location.pretty (Cil_datatype.Kinstr.loc call_kinstr);
    let use_spec = match kf.fundec with
      | Declaration (_,_,_,_) -> `Spec (Annotations.funspec kf)
      | Definition (def, _) ->
        if Kernel_function.Set.mem kf (Value_parameters.UsePrototype.get ())
        then `Spec (Annotations.funspec kf)
        else `Def def
    in
    let cvalue_state = get_cvalue state
    and call_stack = Value_util.call_stack () in
    let result = match use_spec with
      | `Spec spec ->
        Db.Value.Call_Type_Value_Callbacks.apply (`Spec, cvalue_state, call_stack);
        Domain.compute_using_specification call_kinstr (kf, spec) state,
        Value_types.Cacheable
      | `Def _fundec ->
        Db.Value.Call_Type_Value_Callbacks.apply (`Def, cvalue_state, call_stack);
        Computer.compute kf call_kinstr state
    in
    if pp then
      Value_parameters.feedback
        "Done for function %a" Kernel_function.pretty kf;
    result


  (* Mem Exec *)

  module MemExec = Mem_exec2.Make (Val) (Domain)

  let compute_call call_kinstr call init_state =
    let default () =
      compute_using_spec_or_body call_kinstr call.kf init_state
    in
    if Value_parameters.MemExecAll.get () then
      let args =
        List.map (fun {avalue} -> Eval.value_assigned avalue) call.arguments
      in
      match MemExec.reuse_previous_call call.kf init_state args with
      | None ->
        let res, cacheable = default () in
        if not (!Db.Value.use_spec_instead_of_definition call.kf)
        && cacheable = Value_types.Cacheable
        then
          MemExec.store_computed_call call.kf init_state args res;
        res, cacheable
      | Some (res, i) ->
        Db.Value.Call_Type_Value_Callbacks.apply
          (`Memexec, get_cvalue init_state, Value_util.call_stack ());
        (* Evaluate the preconditions of kf, to update the statuses
           at this call. *)
        let spec = Annotations.funspec call.kf in
        if Eval_annots.has_requires spec then begin
          let ab = Logic.create init_state call.kf in
          ignore (Logic.check_fct_preconditions
                    call.kf ab call_kinstr init_state);
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
        res, Value_types.Cacheable
    else
      default ()

  let () = Transfer.compute_call_ref := compute_call


  (* Compute a call to the main function. The initial state is generated
     according to options such as [-lib-entry] and the options of Value governing
     the shape of this state. *)
  let compute_from_entry_point kf =
    Init.initial_state_with_formals kf >>-: fun init_state ->
    Value_util.push_call_stack kf Kglobal;
    Domain.Store.register_initial_state (Value_util.call_stack ()) init_state;
    let cvalue_state = get_cvalue init_state in
    Db.Value.Call_Value_Callbacks.apply (cvalue_state, [kf, Kglobal]);
    ignore (compute_using_spec_or_body Kglobal kf init_state);
    Value_util.pop_call_stack ()

end


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
  Widen.precompute_widen_hints ();
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
    Mem_exec2.cleanup_results ();
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
    Partitioned_dataflow.signal_abort ()
  in
  try
    match Sys.signal Sys.sigusr1 (Sys.Signal_handle handler) with
    | Sys.Signal_default | Sys.Signal_ignore -> ()
    | Sys.Signal_handle f -> prev := f
  with Invalid_argument _ -> () (* Ignore: SIGURSR1 is not available on Windows,
                                   and possibly on other platforms. *)

(* Analysis. *)

let run compute_from_entry_point ?(library=false) kf =
  check ();
  try
    pre ();
    Value_util.clear_call_stack ();
    Stop_at_nth.clear ();
    Value_results.mark_kf_as_called kf;
    Value_parameters.feedback "Analyzing a%scomplete application starting at %a"
      (if library then "n in" else " ")
      Kernel_function.pretty kf;
    Separate.prologue ();
    match compute_from_entry_point kf with
    | `Bottom ->
      Value_parameters.result "Value analysis not started because globals \
                               initialization is not computable.";
      Db.Value.mark_as_computed ();
      Eval_annots.mark_invalid_initializers ()
    | `Value () ->
      Value_parameters.feedback "done for function %a" Kernel_function.pretty kf;
      Separate.epilogue ();
      Db.Value.mark_as_computed ();
      (* Garbled mix must be dumped here -- at least before the call to
         mark_green_and_red -- because fresh ones are created when re-evaluating
         all the alarms, and we get an unpleasant "ghost effect". *)
      Value_util.dump_garbled_mix ();
      (* Mark unreachable and RTE statuses. Only do this there, not when the
         analysis was aborted (hence, not in post_cleanup), because the
         propagation is incomplete. Also do not mark unreachable statutes if
         there is an alarm in the initialisers (bottom initial state), as we
         would end up marking the alarm as dead. *)
      Eval_annots.mark_unreachable ();
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
