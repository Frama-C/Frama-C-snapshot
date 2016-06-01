(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Cil
open Cil_datatype
open Cvalue
open Value_util
open Eval_exprs

let dkey_callbacks = Value_parameters.register_category "callbacks"

let check_signals, signal_abort =
  let signal_emitted = ref false in
  (fun () ->
    if !signal_emitted then begin
      signal_emitted := false;
      raise Db.Value.Aborted
    end),
  (fun () -> signal_emitted := true)

module type Arg = sig
  val kf: kernel_function
  val initial_states : State_set.t
  val active_behaviors: Eval_annots.ActiveBehaviors.t
end

module Computer(AnalysisParam: Arg) =
struct

   let current_kf = AnalysisParam.kf
   let current_fundec = Kernel_function.get_definition current_kf
   let return = Kernel_function.find_return current_kf
   let return_lv =
     match return.skind with
       | Return (Some ({enode = Lval lv}),_) -> Some lv
       | Return (None,_) -> None
       | _ -> assert false (* Cil invariant *)

   let is_natural_loop = Loop.is_natural current_kf
   let is_basic_loop s = match s.skind with Loop _ -> true | _ -> false
   (* Widening will be performed the statements verifying this predicate. *)
   let is_loop =
     let non_natural = Loop.get_non_naturals current_kf in
     if Stmt.Set.is_empty non_natural then
       (fun s -> is_natural_loop s || is_basic_loop s)
     else
       (fun s ->
         is_natural_loop s || is_basic_loop s || Stmt.Set.mem s non_natural)

   let obviously_terminates = 
     Value_parameters.ObviouslyTerminatesAll.get() (* TODO: by function *)      

   let slevel = 
     if obviously_terminates
     then Per_stmt_slevel.Global max_int
     else Per_stmt_slevel.local current_kf

   let slevel stmt = match slevel with
     | Per_stmt_slevel.Global i -> i
     | Per_stmt_slevel.PerStmt f -> f stmt

   (* This function decides whether we should merge all incoming states
      on the given statement before treating it. *)
   let merge =
     (* Ideally, we would like to merge only the states propagated along the
        back edges of the loop. Since this is not currently easy, we
        use an approximation that consists in merging all the states on the
        loop node. *)
     let after_loop =
       Kernel_function.Set.mem current_kf
         (Value_parameters.SlevelMergeAfterLoop.get ())
     in
     match Per_stmt_slevel.merge current_kf with
     | Per_stmt_slevel.NoMerge ->
       if after_loop then
         (fun stmt -> match stmt.skind with
            | Loop _ -> true
            | _ -> false)
       else (fun _ -> false)
     | Per_stmt_slevel.Merge fun_s ->
       (fun stmt ->
          fun_s stmt ||
          (after_loop && match stmt.skind with Loop _ -> true | _ -> false))


   let (initial_state,_) = State_set.join AnalysisParam.initial_states

   (*  State propagated by the dataflow, that contains only 'new' states
       (i.e. not propagated before). *)
   type diff = { mutable to_propagate : State_set.t ; }

   (*  The real state for a given statement, used in particular to detect
       convergence. Stored by us, not by the dataflow itself. *)
   type stmt_state = {
     (* All the state that have been propagated separately, by slevel *)
     superposition : State_imp.t;

     (*  Bottom if we have never consumed all the slevel allocated. If no
         more slevel is available, the state that is being propagated. This
         state is *not* present in [superposition]. *)
     mutable widening_state : Cvalue.Model.t ;

     (*  should we widen the statement at the current iteration.
         [widening_state] is decremented each time we visit the statement,
         unless it is equal to zero. (In which case we widen, and set
         [widening_state] to a non-zero value, currently 1.) *)
     mutable widening : int;

     (*  Number of states that were put in [superposition]; i.e. the
	 sum of the cardinals of the state sets that were added with
	 [update_and_tell_if_changed]. It may be different
	 (i.e. larger) from the cardinal of [state_imp], that merge
	 states that are equal. *)
     mutable counter_unroll : int ;
   }

   let empty_record () = {
     superposition = State_imp.empty () ;
     widening = Value_parameters.WideningLevel.get () ;
     widening_state = Cvalue.Model.bottom ;
     counter_unroll = 0;
   }

   type t = stmt_state Stmt.Hashtbl.t

   let current_table : t = Stmt.Hashtbl.create 128

   let stmt_state s =
     try Stmt.Hashtbl.find current_table s
     with Not_found ->
       let record = empty_record () in
       Stmt.Hashtbl.add current_table s record;
       record

   let stmt_widening_info s =
     let r = stmt_state s in
     r.widening, r.widening_state

   (* merges [set] into the state associated to [stmt], and returns the subset
      of [set] that was not already in the superposition. *)
   let update_stmt_states stmt set =
     let record = stmt_state stmt in
     if Cvalue.Model.is_reachable record.widening_state
     then
       let (state, tr) = State_set.join set in
       if Cvalue.Model.is_included state record.widening_state
       then State_set.empty
       else State_set.singleton (state, tr)
     else
       State_imp.merge_set_return_new set record.superposition

   let update_stmt_widening_info stmt wcounter wstate =
     let record = stmt_state stmt in
     record.widening <- wcounter;
     record.widening_state <- wstate

   let states_unmerged_for_callbacks () =
     let r = Stmt.Hashtbl.create (Stmt.Hashtbl.length current_table) in
     let aux stmt record =
       let states = State_imp.to_list record.superposition in
       let states =
         if Cvalue.Model.is_reachable record.widening_state
         then record.widening_state :: states
         else states
       in
       Stmt.Hashtbl.add r stmt states
     in
     Stmt.Hashtbl.iter aux current_table;
     r

   let states_for_callbacks () =
     let r = Stmt.Hashtbl.create (Stmt.Hashtbl.length current_table) in
     let aux stmt record =
       Stmt.Hashtbl.add r stmt
         (Cvalue.Model.join
            (State_imp.join record.superposition)
            record.widening_state)
     in
     Stmt.Hashtbl.iter aux current_table;
     r

   let states_unmerged s =
     let record = stmt_state s in
     let s = State_imp.to_set record.superposition in
     if Cvalue.Model.is_reachable record.widening_state
       (* Forget about the trace. TODO: preserve the trace. *)
     then State_set.add (record.widening_state, Trace.top) s
     else s

   let states_after = Cil_datatype.Stmt.Hashtbl.create 5

   (* During the dataflow analysis, if required by a callback, we store the
      state after a statement, but only if either the following conditions 
      is met ([succ] being a successor of [s])
      - [s] is an instr (the control flow statements such as [goto] and [if]
	do not change the state (union of the states in the case of if))
	AND there is a control-flow join on [succ]
      - [s] is the last instruction of a block that contains
	local variables
      For statements for which the function below returns false, we deduce
      the state after by the state before [succ] or another successor of [s].
      This avoids potentially useless computations
   *)
   let store_state_after_during_dataflow s succ =
     ((match s.skind with Instr _ -> true | _ -> false) &&
	(match succ.preds with [_] -> false | _ -> true))
     || (let b1 = Kernel_function.find_enclosing_block s
	and b2 = Kernel_function.find_enclosing_block succ in
	not (Cil_datatype.Block.equal b1 b2) && b1.blocals <> [])

   (* Computation of the per-function 'after statement' states *)
   let local_after_states superposed =
     lazy (
       let superposed = Lazy.force superposed in
       Stmt.Hashtbl.iter
	 (fun stmt state ->
	    List.iter
	      (fun pred ->
		 if not (store_state_after_during_dataflow pred stmt) then
		   try
		     let cur = Stmt.Hashtbl.find states_after pred in
		     Stmt.Hashtbl.replace states_after pred
		       (Cvalue.Model.join state cur)
		   with Not_found -> Stmt.Hashtbl.add states_after pred state
	      ) stmt.preds;
	 ) superposed;
       (* Since the return instruction has no successor, it is not visited
	  by the iter above. We fill it manually *)
       (try
	  let s = Stmt.Hashtbl.find superposed return in
	  Stmt.Hashtbl.add states_after return s
	with Kernel_function.No_Statement | Not_found -> ()
       );
       states_after
     )

   (* Table storing whether conditions on 'if' have been evaluated
      to true or false *)
   let conditions_table = Cil_datatype.Stmt.Hashtbl.create 5

   let merge_results () =
     let superposed = lazy (states_for_callbacks ()) in
     let after_full = local_after_states superposed in
     let stack_for_callbacks = call_stack () in
     if Mark_noresults.should_memorize_function current_fundec then begin
       Value_results.merge_states_in_db superposed stack_for_callbacks;
       Db.Value.merge_conditions conditions_table;
       Value_results.merge_after_states_in_db after_full stack_for_callbacks;
     end;
     if not (Db.Value.Record_Value_Superposition_Callbacks.is_empty ())
     then begin
       let current_superpositions = lazy (states_unmerged_for_callbacks ()) in
       if Value_parameters.ValShowProgress.get () then
	 Value_parameters.debug ~dkey:dkey_callbacks
	   "now calling Record_Value_Superposition callbacks";
       Db.Value.Record_Value_Superposition_Callbacks.apply
	 (stack_for_callbacks, current_superpositions);
     end ;
     if not (Db.Value.Record_Value_Callbacks.is_empty ())
     then begin
       if Value_parameters.ValShowProgress.get () then
	 Value_parameters.debug ~dkey:dkey_callbacks
           "now calling Record_Value callbacks";
       Db.Value.Record_Value_Callbacks.apply
	 (stack_for_callbacks, superposed)
     end;
     if not (Db.Value.Record_Value_Callbacks_New.is_empty ())
     then begin
       if Value_parameters.ValShowProgress.get () then
	 Value_parameters.debug ~dkey:dkey_callbacks
           "now calling Record_Value_New callbacks";
       if Value_parameters.MemExecAll.get () then
         Db.Value.Record_Value_Callbacks_New.apply
           (stack_for_callbacks,
            Value_types.NormalStore ((superposed, after_full),
                                     (Mem_exec.new_counter ())))
       else
         Db.Value.Record_Value_Callbacks_New.apply
           (stack_for_callbacks,
            Value_types.Normal (superposed, after_full))
     end;
     if not (Db.Value.Record_Value_After_Callbacks.is_empty ())
     then begin
       if Value_parameters.ValShowProgress.get () then
	 Value_parameters.debug ~dkey:dkey_callbacks
	   "now calling Record_After_Value callbacks";
       Db.Value.Record_Value_After_Callbacks.apply
	 (stack_for_callbacks, after_full);
     end;
   ;;

  (* Clobbered list for bases containing addresses of local variables. *)
  let clob = Locals_scoping.bottom ()

  let cacheable = ref Value_types.Cacheable

  module DataflowArg: Dataflow2.ForwardsTransfer
    with type t = diff
  = struct

   let debug = false
   let name = "Values analysis"

   module StmtStartData =
     Dataflow2.StartData(struct type t = diff let size = 107 end)

   type t = diff

   let copy (d: t) = d

   let display_one fmt v =
     State_set.iter (fun (values, trace) ->
       if not (Cvalue.Model.is_reachable values) then
	 Format.fprintf fmt "Statement (x) with trace %a : UNREACHABLE@\n"
	   Trace.pretty trace
       else
	 Format.fprintf fmt "Statement (x) with trace %a : @\n%a"
	   Trace.pretty trace Cvalue.Model.pretty values
     ) v.to_propagate

   let pretty fmt (d: t) = display_one fmt d

   let computeFirstPredecessor (s: stmt) states =
     let v = states.to_propagate in
     let v = State_set.add_statement v s in
     (stmt_state s).counter_unroll <- State_set.length v;
     (* Create an impure state for this statement. It will be mutated by
        the other functions *)
     { to_propagate = v;}

   let counter_unroll_target = ref (Value_parameters.ShowSlevel.get())

   let is_return s = match s.skind with Return _ -> true | _ -> false

   let combinePredecessors (s: stmt) ~old new_ =
     let new_v = new_.to_propagate in
     if State_set.is_empty new_v
     then None
     else begin
       (* Update loc, which can appear in garbled mix origins. *)
       let old_loc = Cil.CurrentLoc.get () in
       Cil.CurrentLoc.set (Cil_datatype.Stmt.loc s);
       (* Note: When we join traces, they must lead to the same statement;
	  thus we need to add the statement here (instead of e.g. in doStmt,
	  which would be too late). *)
       let new_v = State_set.add_statement new_v s in
       let current_info = stmt_state s in
       let old_counter = current_info.counter_unroll in
       (* Check whether there is enough slevel available. If not, merge all
          states together. However, do not perform merge on return
          instructions. This needelessly degrades precision for
          postconditions and option -split-return. *)
       let r =
         if old_counter > slevel s && not (is_return s)
	 then
           let new_state, new_trace = State_set.join new_v in
           let old_state, old_trace = State_set.join old.to_propagate in
           let join =
             Model.join new_state old_state,
             Trace.join new_trace old_trace
           in
           old.to_propagate <- State_set.singleton join;
	   Some old
	 else begin
           try
	     let merged = State_set.merge_into new_v ~into:old.to_propagate in
	     let length_new = State_set.length new_v in
	     let new_counter_unroll = old_counter + length_new in
	     if new_counter_unroll >= !counter_unroll_target
	     then begin
	       let period = Value_parameters.ShowSlevel.get() in
	       let reached = new_counter_unroll / period * period in
	       Value_parameters.feedback ~once:true
		 "Semantic level unrolling superposing up to %d states"
		 reached;
	     counter_unroll_target := reached + period;
	     end;
	     current_info.counter_unroll <- new_counter_unroll;
             old.to_propagate <- merged;
	     Some old
	   with State_set.Unchanged -> None
	 end
       in
       Cil.CurrentLoc.set old_loc;
       r
     end

  let interp_call stmt lval_to_assign funcexp argl d_value =
    let with_alarms = warn_all_quiet_mode () in
    let aux state =
      Eval_stmt.interp_call
        ~with_alarms clob stmt lval_to_assign funcexp argl state
    in
    State_set.fold
      (fun acc (state, trace) ->
        let results, call_cacheable = aux state (* xxx: add trace argument. *) in
        if call_cacheable = Value_types.NoCacheCallers then
          (* Propagate info that the current call cannot be cached either *)
          cacheable := Value_types.NoCacheCallers;
        List.fold_left
          (fun acc state -> State_set.add (state, trace) acc) acc results
      ) State_set.empty d_value

  let doInstr stmt (i: instr) (d: t) =
    !Db.progress ();
    Valarms.start_stmt (Kstmt stmt);
    let d_states = d.to_propagate in
    let unreachable = State_set.is_empty d_states in
    let result =
      if unreachable then d
      else begin
          let with_alarms = warn_all_quiet_mode () in
          let propagate states =
            (* Create a transient propagation result, that will be passed
               to the successors of stmt by the dataflow module *)
            { to_propagate = states }
          in
          let apply_each_state f =
            let states_after_i =
              State_set.fold
                (fun acc (state, trace) ->
                  Value_messages.set_current_state (state,trace);
                  State_set.add (f state, trace) acc
                ) State_set.empty d_states
            in
            propagate states_after_i
          in
          (* update current statement *)
          match i with
          | Set (lv,exp,_loc) ->
              apply_each_state
                (fun state_value ->
                   Eval_stmt.do_assign ~with_alarms
                     current_kf clob state_value lv exp)
          (* TODOBY: this should be transferred as a builtin. However, this
             is not possible for va_arg currently *)
          | Call (_,
                 {enode = Lval (Var {vname=("__builtin_va_start"|
                                            "__builtin_va_end")},NoOffset)},
                 [{enode = Lval lv}],_loc) ->
              apply_each_state
                (fun state ->
                   let loc = Eval_exprs.lval_to_loc ~with_alarms state lv in
                   Valarms.set_syntactic_context (Valarms.SyMem lv);
                   Eval_op.add_binding ~with_alarms
                     ~exact:true state loc V.top_int 
                )
          | Call (_,
                 {enode = Lval (Var {vname=("__builtin_va_arg")},NoOffset)},
                 [_; size; dst],_loc) ->
              apply_each_state
                (fun state ->
                   let vsize = eval_expr ~with_alarms state size in
                   let size =
                     try
                       let i = V.project_ival vsize in
                       let i = Ival.project_int i in
                       let ibytes = Integer.mul i (Bit_utils.sizeofchar ()) in
                       Int_Base.inject ibytes
                     with V.Not_based_on_null | Ival.Not_Singleton_Int ->
                       Int_Base.top
                   in
                   let locbytes = eval_expr ~with_alarms state dst in
                   let locbits = Locations.loc_bytes_to_loc_bits locbytes in
                   let loc = Locations.make_loc locbits size in
                   Eval_op.add_binding ~with_alarms
                     ~exact:true state loc V.top_int
                )
          | Call (lval_to_assign,funcexp,argl,_loc) ->
            propagate (interp_call stmt lval_to_assign funcexp argl d_states)
          | Asm _ ->
              warning_once_current
                "assuming assembly code has no effects in function %t"
                pretty_current_cfunction_name;
              d
          | Skip _ -> d
          | Code_annot (_,_) -> d (* processed direcly in doStmt from the
                                     annotation table *)
        end
    in
    Valarms.end_stmt ();
    result

  let doStmtSpecific s _d states =
    match s.skind with
      | Loop _ ->
	  let current_info = stmt_state s in
	  let counter = current_info.counter_unroll in
          if counter > slevel s then
            Value_parameters.feedback ~level:1 ~once:true ~current:true
              "entering loop for the first time";
          states

      | UnspecifiedSequence seq ->
        (try
	   if Kernel.UnspecifiedAccess.get () 
	   then begin
             State_set.iter
               (fun (state, _trace) ->
                 Eval_stmt.check_unspecified_sequence state seq
               ) states;
	   end;
           states
         with Eval_stmt.AlwaysOverlap -> State_set.empty
        )
      | _ -> states

  let doStmt (s: stmt) (d: t) =
    Valarms.start_stmt (Kstmt s);
    check_signals ();
    (* Merge incoming states if the user requested it *)
    if merge s then
      d.to_propagate <- State_set.singleton (State_set.join d.to_propagate);
    let states = d.to_propagate in
    Db.Value.Compute_Statement_Callbacks.apply (s, call_stack(), 
						State_set.to_list states);
    (* Cleanup function, to be called on all exit paths *)
    let ret result =
      (* Do this as late as possible, as a non-empty to_propagate field
         is shown in a special way in case of degeneration *)
      d.to_propagate <- State_set.empty;
      Valarms.end_stmt ();
      result
    in
    if State_set.is_empty states then ret Dataflow2.SDefault
    else
    let states =
      if obviously_terminates
      then states
      else update_stmt_states s states (* Remove states already present *)
    in
    if State_set.is_empty states then ret Dataflow2.SDefault
    else
    (* We do not interpret annotations that come from statement contracts
       and everything previously emitted by Value (currently, alarms) *)
    let annots = Annotations.fold_code_annot
      (fun e ca acc ->
        if Logic_utils.is_contract ca || Emitter.equal e Value_util.emitter
        then acc
        else ca :: acc
      ) s []
    in
    let slevel = slevel s in
    let interp_annot record states annot =
      Eval_annots.interp_annot
        current_kf AnalysisParam.active_behaviors initial_state slevel
        states s annot record
    in
    let states = List.fold_left (interp_annot true) states annots in
    if State_set.is_empty states then ret Dataflow2.SDefault
    else
    let is_return = is_return s in
    let current_info = stmt_state s in
    let old_counter = current_info.counter_unroll in
    let new_states =
      if (old_counter > slevel && not is_return)
        || (is_return && obviously_terminates)
      then (* No slevel left, perform some join and/or widening *)
        let curr_wcounter, curr_wstate = stmt_widening_info s in
	(* Note: curr_wstate is the previous widening state, so there is no
	   need to attach any trace to it: it would just be a prefix of the
	   currently propagated trace. *)
        let (state,trace) = State_set.join states in
        let joined = Cvalue.Model.join curr_wstate state in
        if Model.equal joined curr_wstate then
          State_set.empty (* [state] is included in the last propagated state.
                             Nothing remains to do *)
        else
	  if obviously_terminates
	  then begin (* User thinks the analysis will terminate: do not widen *)
	    update_stmt_widening_info s 0 joined;
	    states
	  end
	  else
            let r =
	      if is_loop s && curr_wcounter = 0 then
                let widen_hints = Widen.getWidenHints current_kf s in
                Cvalue.Model.widen widen_hints curr_wstate joined
	      else
                joined
            in
            let new_wcounter =
              if curr_wcounter = 0 then 1 else pred curr_wcounter
            in
            let new_state = State_set.singleton (r, trace) in
            if Cvalue.Model.equal r joined then (
	      update_stmt_widening_info s new_wcounter r;
	      new_state)
            else begin (* Try to correct over-widenings *)
	      let new_states =
              (* Do *not* record the status after interpreting the annotation
                 here. Possible unproven assertions have already been recorded
                 when the assertion has been interpreted the first time higher
                 in this function. *)
                List.fold_left (interp_annot false) new_state annots
              in
              let (new_joined,tr) = State_set.join new_states in
              update_stmt_widening_info s new_wcounter new_joined;
              State_set.singleton (new_joined,tr)
            end
      else states
    in
    let states = doStmtSpecific s d new_states in
    (* This temporary propagation value will be passed on to the successors
       of [s] *)
    ret (Dataflow2.SUse { to_propagate = states })

  let doEdge s succ d =
    let kinstr = Kstmt s in
    let states = d.to_propagate in
    Valarms.start_stmt kinstr;
    (* We store the state after the execution of [s] for the callback
       {Value.Record_Value_After_Callbacks}. This is done here
       because we want to see the values of the variables local to the block *)
    if store_state_after_during_dataflow s succ
    then (
      let old =
        try Cil_datatype.Stmt.Hashtbl.find states_after s
        with Not_found -> Cvalue.Model.bottom
      in
      let updated = State_set.fold
	(fun acc (state, trace) ->
          Value_messages.set_current_state (state,trace);
          Cvalue.Model.join acc state) old states in
      Cil_datatype.Stmt.Hashtbl.replace states_after s updated
    );
    (* Variables exiting their scope *)
    let states =
      match Kernel_function.blocks_closed_by_edge s succ with
      | [] -> states
      | closed_blocks ->
          (* Partial application is useful, do not inline *)
          let block_top =
            Locals_scoping.block_top_addresses_of_locals
              current_fundec clob closed_blocks
          in
            State_set.fold
              (fun set (state, trace) ->
                let state =
                  Cvalue.Model.uninitialize_blocks_locals closed_blocks state
                in
                State_set.add (block_top state, trace) set)
              State_set.empty
              states;
    in
    (* Variables entering in scope *)
    let opened_blocks = Kernel_function.blocks_opened_by_edge s succ in
    let states = List.fold_left bind_block_locals states opened_blocks in
    Valarms.end_stmt ();
    d.to_propagate <- states;
    d

  let doGuardOneCond stmt context exp t =
    if State_set.is_empty (t.to_propagate)
    then Dataflow2.GUnreachable
    else begin
        Valarms.start_stmt (Kstmt stmt);
        let with_alarms = warn_all_quiet_mode () in
        let new_values =
          State_set.fold
            (fun acc (state, trace) ->
              let state, _, test =
                eval_expr_with_deps_state None ~with_alarms state exp
              in
              Valarms.set_syntactic_context context;
              Value_messages.set_current_state (state,trace);
              let warn = not (Warn.are_comparable Abstract_interp.Comp.Eq
                                V.singleton_zero test) in
              let do_it =
                (warn && Value_parameters.UndefinedPointerComparisonPropagateAll.get ()) ||
                  let t1 = unrollType (typeOf exp) in
                  if isIntegralType t1 || isPointerType t1
                  then V.contains_non_zero test
                  else true (* TODO: a float condition is true iff != 0.0 *)
              in
              if do_it then
                try
                  State_set.add
                    (reduce_by_cond state {positive = true; exp = exp}, trace)
                    acc
                with Reduce_to_bottom -> acc
              else acc)
            State_set.empty
            t.to_propagate
        in
        let result =
          if State_set.is_empty new_values then Dataflow2.GUnreachable
          else Dataflow2.GUse { to_propagate = new_values}
        in
        Valarms.end_stmt ();
        result
      end

  let mask_then = Db.Value.mask_then
  let mask_else = Db.Value.mask_else
  let mask_both = mask_then lor mask_else

  let doGuard stmt exp t =
    let not_exp = new_exp ~loc:exp.eloc (UnOp(LNot, exp, intType)) in
    let th, el as thel =
      let context = Valarms.SyUnOp exp in
      doGuardOneCond stmt context exp t, doGuardOneCond stmt context not_exp t
    in
    let th_reachable =
      match th with
        Dataflow2.GUse _ | Dataflow2.GDefault -> mask_then
      | Dataflow2.GUnreachable -> 0
    in
    let el_reachable =
      match el with
        Dataflow2.GUse _ | Dataflow2.GDefault -> mask_else
      | Dataflow2.GUnreachable -> 0
    in
    let reachable = th_reachable lor el_reachable in
    if Value_parameters.InterpreterMode.get() && (reachable = mask_both)
    then begin
	warning_once_current "Do not know which branch to take. Stopping.";
	exit 0
      end;
    let current_condition_status =
      try
        Cil_datatype.Stmt.Hashtbl.find conditions_table stmt
      with Not_found -> 0
    in
    let new_status = 
      current_condition_status lor reachable 
    in
    if new_status <> 0
    then Cil_datatype.Stmt.Hashtbl.replace conditions_table stmt new_status;
    Separate.filter_if stmt thel

  end

  module Dataflow = Dataflow2.Forwards(DataflowArg)

  (* Walk through all the statements for which [to_propagate] is not empty.
     Those statements are marked as "not fully propagated", for ulterior
     display in the gui. Also mark the current statement as root if relevant.*)
  let mark_degeneration () =
    DataflowArg.StmtStartData.iter
      (fun stmt v ->
        if not (State_set.is_empty v.to_propagate) then
          Value_util.DegenerationPoints.replace stmt false);
    match Valarms.current_stmt () with
    | Kglobal -> ()
    | Kstmt s ->
      let kf = Kernel_function.find_englobing_kf s in
      if Kernel_function.equal kf current_kf then (
        Value_util.DegenerationPoints.replace s true;
        Valarms.end_stmt ())


  (* Check that the dataflow is indeed finished *)
  let checkConvergence () =
    DataflowArg.StmtStartData.iter (fun k v ->
      if not (State_set.is_empty (v.to_propagate)) then
        Value_parameters.fatal "sid:%d@\n%a@\n"
          k.sid State_set.pretty v.to_propagate
    )

  (* Final states of the function, reduced by the post-condition *)
  let final_states () =
    let states = states_unmerged return in
    (* Reduce final states according to the function postcondition *)
    let result = match return_lv with
      | Some (Var v, NoOffset) -> Some v
      | Some _ -> assert false
      | None -> None
    in
    Eval_annots.check_fct_postconditions
      current_kf AnalysisParam.active_behaviors
      (Annotations.behaviors current_kf) ~result ~per_behavior:false
      Normal (* termination kind*)  ~pre_state:initial_state states

  let externalize states =
    Valarms.start_stmt (Kstmt return);
    let with_alarms = warn_all_quiet_mode () in
    (* Partial application is useful, do not inline *)
    let externalize =
      Eval_stmt.externalize ~with_alarms current_kf ~return_lv clob
    in
    let states = Split_return.join_final_states current_kf ~return_lv states in
    let r = List.map externalize states in
    Valarms.end_stmt ();
    r

  let results () =
    if DataflowArg.debug then checkConvergence ();
    let final_states = final_states () in
    let externalized = externalize final_states in {
      Value_types.c_values = externalized;
      c_clobbered = clob.Locals_scoping.clob;
      c_cacheable = !cacheable;
      c_from = None;
    }

  let compute states =
    let start = Kernel_function.find_first_stmt AnalysisParam.kf in
    let states = bind_block_locals states current_fundec.sbody in
    (* Init the dataflow state for the first statement *)
    let dinit = { to_propagate = states} in
    let dinit = DataflowArg.computeFirstPredecessor start dinit in
    DataflowArg.StmtStartData.add start dinit;
    Dataflow.compute [start]

end



(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
