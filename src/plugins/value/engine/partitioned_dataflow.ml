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


let check_signals, signal_abort =
  let signal_emitted = ref false in
  (fun () ->
     if !signal_emitted then begin
       signal_emitted := false;
       raise Db.Value.Aborted
     end),
  (fun () -> signal_emitted := true)

let dkey_callbacks = Value_parameters.register_category "callbacks"


module Make_Dataflow
    (Domain : Abstract_domain.External)
    (States : Partitioning.StateSet with type state = Domain.t)
    (Transfer : Transfer_stmt.S with type state = Domain.t
                                 and type return = Domain.return)
    (Logic : Transfer_logic.S with type state = Domain.t
                               and type states = States.t)
    (AnalysisParam : sig
       val kf: kernel_function
       val call_kinstr: kinstr
       val initial_state : Domain.t
     end)
= struct

  let with_alarms = Value_util.warn_all_quiet_mode ()

  module Partition = Partitioning.Make_Partition (Domain) (States)

  let current_kf = AnalysisParam.kf
  let current_fundec = Kernel_function.get_definition current_kf
  let return = Kernel_function.find_return current_kf
  let return_lv = match return.skind with
    | Return (Some ({enode = Lval ((Var v, NoOffset) as lv)} as exp), _) ->
      Some (exp, lv, v)
    | Return (None, _) -> None
    | _ -> assert false (* Cil invariant *)

  let is_natural_loop = Loop.is_natural current_kf
  let is_basic_loop s = match s.skind with Loop _ -> true | _ -> false
  (* Widening will be performed the statements verifying this predicate. *)
  let is_loop =
    let non_natural = Loop.get_non_naturals current_kf in
    if Cil_datatype.Stmt.Set.is_empty non_natural then
      fun s -> is_natural_loop s || is_basic_loop s
    else
      fun s -> is_natural_loop s || is_basic_loop s
               || Cil_datatype.Stmt.Set.mem s non_natural

  let obviously_terminates =
    Value_parameters.ObviouslyTerminatesAll.get () (* TODO: by function *)

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
      if after_loop
      then is_basic_loop
      else fun _ -> false
    | Per_stmt_slevel.Merge fun_s ->
      fun stmt -> fun_s stmt || (after_loop && is_basic_loop stmt)

  let active_behaviors = Logic.create AnalysisParam.initial_state current_kf

  let initial_states =
    let state = AnalysisParam.initial_state
    and kf = current_kf
    and call_kinstr = AnalysisParam.call_kinstr
    and ab = active_behaviors in
    let locals = current_fundec.sbody.blocals in
    let state = Domain.enter_scope current_kf locals state in
    (* Remark: the pre-condition cannot talk about the locals. BUT
       check_fct_preconditions split the state into a stateset, hence
       it is simpler to apply it to the (unique) state with locals *)
    Logic.check_fct_preconditions kf ab call_kinstr state

  let initial_state =
    match initial_states with
    | `Bottom -> Domain.top (* No analysis in this case. *)
    | `Value states -> match States.join states with
      | `Bottom -> assert false
      | `Value state -> state

  (* State propagated by the dataflow, that contains only 'new' states
      (i.e. not propagated before). *)
  type diff = { mutable to_propagate : States.t }

  (* The real state for a given statement, used in particular to detect
      convergence. Stored by us, not by the dataflow itself. *)
  type stmt_state = {
    (* All the state that have been propagated separately, by slevel *)
    superposition : Partition.t;

    (* Bottom if we have never consumed all the slevel allocated. If no
       more slevel is available, the state that is being propagated. This
       state is *not* present in [superposition]. *)
    mutable widening_state : Domain.t or_bottom;

    (* should we widen the statement at the current iteration.
       [widening_state] is decremented each time we visit the statement,
       unless it is equal to zero. (In which case we widen, and set
       [widening_state] to a non-zero value, currently 1.) *)
    mutable widening : int;

    (* Number of states that were put in [superposition]; i.e. the
       sum of the cardinals of the state sets that were added with
       [update_and_tell_if_changed]. It may be different
       (i.e. larger) from the cardinal of [state_imp], that merge
       states that are equal. *)
    mutable counter_unroll : int ;
  }

  let empty_record () = {
    superposition = Partition.empty () ;
    widening = Value_parameters.WideningLevel.get () ;
    widening_state = `Bottom ;
    counter_unroll = 0;
  }

  module StmtHtbl = struct
    include Cil_datatype.Stmt.Hashtbl
    let replace_value t stmt = function
      | `Bottom -> ()
      | `Value s -> replace t stmt s
  end
  type tt = stmt_state StmtHtbl.t

  let current_table : tt = StmtHtbl.create 128

  let stmt_state s =
    try StmtHtbl.find current_table s
    with Not_found ->
      let record = empty_record () in
      StmtHtbl.add current_table s record;
      record

  let stmt_widening_info s =
    let r = stmt_state s in
    r.widening, r.widening_state

  (* merges [set] into the state associated to [stmt], and returns the subset
     of [set] that was not already in the superposition. *)
  let update_stmt_states stmt set =
    let record = stmt_state stmt in
    match record.widening_state with
    | `Bottom ->
      Partition.merge_set_return_new set record.superposition
    | `Value widening_state ->
      match States.join set with
      | `Bottom -> States.empty
      | `Value state ->
        if Domain.is_included state widening_state
        then States.empty
        else (
          let join = Domain.join widening_state state in
          record.widening_state <-`Value join;
          States.singleton join
        )

  let update_stmt_widening_info stmt wcounter wstate =
    let record = stmt_state stmt in
    record.widening <- wcounter;
    record.widening_state <-
      Bottom.join Domain.join record.widening_state wstate


  let states_unmerged s =
    let record = stmt_state s in
    let s = Partition.to_set record.superposition in
    match record.widening_state with
    | `Bottom -> s
    | `Value state -> States.add state s

  let states_after : Domain.state StmtHtbl.t = StmtHtbl.create 5

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

  let cacheable = ref Value_types.Cacheable

  (* ---------------------------------------------------------------------- *)
  (*                 Dataflow Argument : Forwards Transfer                  *)
  (* ---------------------------------------------------------------------- *)

  module DataflowArg = struct

    let debug = false
    let name = "Values analysis"

    module StmtStartData =
      Dataflow2.StartData(struct type t = diff let size = 107 end)

    type t = diff

    let copy (d: t) = d

    let display_one fmt v =
      States.iter
        (fun s -> Format.fprintf fmt "Statement (x) : @\n%a" Domain.pretty s)
        v.to_propagate

    let pretty fmt (d: t) = display_one fmt d

    let computeFirstPredecessor (s: stmt) states =
      let v = states.to_propagate in
      (stmt_state s).counter_unroll <- States.length v;
      (* Create an impure state for this statement. It will be mutated by
         the other functions *)
      { to_propagate = v; }

    let counter_unroll_target = ref (Value_parameters.ShowSlevel.get ())

    let is_return s = match s.skind with Return _ -> true | _ -> false

    let combinePredecessors stmt ~old new_ =
      let new_v = new_.to_propagate in
      if States.is_empty new_v
      then None
      else
        (* Update loc, which can appear in garbled mix origins. *)
        let old_loc = Cil.CurrentLoc.get () in
        Cil.CurrentLoc.set (Cil_datatype.Stmt.loc stmt);
        let current_info = stmt_state stmt in
        let old_counter = current_info.counter_unroll in
        (* Check whether there is enough slevel available. If not, merge all
           states together. However, do not perform merge on return
           instructions. This needelessly degrades precision for
           postconditions and option -split-return. *)
        let r =
          if old_counter > slevel stmt && not (is_return stmt)
          then
            let new_state = States.join new_v in
            let old_state = States.join old.to_propagate in
            let join = Bottom.join Domain.join new_state old_state in
            old.to_propagate <- States.singleton' join;
            Some old
          else
            let merged, unchanged =
              States.merge ~into:old.to_propagate new_v in
            if unchanged
            then None
            else
              let length_new = States.length new_v in
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
        in
        Cil.CurrentLoc.set old_loc;
        r


    let interp_call stmt lval_option funcexp args state acc =
      let results, call_cacheable =
        Transfer.call with_alarms stmt lval_option funcexp args state
      in
      if call_cacheable = Value_types.NoCacheCallers then
        (* Propagate info that the current call cannot be cached either *)
        cacheable := Value_types.NoCacheCallers;
      List.fold_left
        (fun acc state -> States.add state acc)
        acc (Bottom.list_of_bot results)

    let doInstr stmt (i: instr) (d: t) =
      !Db.progress ();
      Alarmset.start_stmt (Kstmt stmt);
      let d_states = d.to_propagate in
      let unreachable = States.is_empty d_states in
      let result =
        if unreachable then d
        else begin
          let propagate states =
            (* Create a transient propagation result, that will be passed
               to the successors of stmt by the dataflow module *)
            { to_propagate = states }
          in
          let apply_each_state f =
            let states_after_i =
              States.fold
                (fun state acc -> States.add' (f state) acc)
                d_states States.empty
            in
            propagate states_after_i
          in
          (* update current statement *)
          match i with
          | Set (lv,exp,_loc) ->
            apply_each_state
              (fun s ->
                 Transfer.assign ~with_alarms s current_kf stmt lv exp)
          | Call (lval_option, funcexp, args, _loc) ->
            let process = interp_call stmt lval_option funcexp args in
            propagate (States.fold process d_states States.empty)
          | Asm _ ->
            Value_util.warning_once_current
              "assuming assembly code has no effects in function %t"
              Value_util.pretty_current_cfunction_name;
            d
          | Skip _ -> d
          | Code_annot (_,_) -> d (* processed direcly in doStmt from the
                                     annotation table *)
        end
      in
      Alarmset.end_stmt ();
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
        if Kernel.UnspecifiedAccess.get ()
        then
          let with_alarms = Value_util.warn_all_mode in
          let check = Transfer.check_unspecified_sequence in
          States.fold
            (fun s acc -> match check ~with_alarms s seq with
               | `Bottom -> acc
               | `Value () -> States.add s acc)
            states
            States.empty
        else states
      | _ -> states

    let get_cvalue = Domain.get Cvalue_domain.key
    let gather_cvalue = match get_cvalue with
      | Some get -> fun state acc -> get state :: acc
      | None -> fun _ acc -> acc

    let doStmt (s: stmt) (d: t) =
      Alarmset.start_stmt (Kstmt s);
      check_signals ();
      (* Merge incoming states if the user requested it *)
      if merge s then
        d.to_propagate <- States.singleton' (States.join d.to_propagate);
      let states = d.to_propagate in
      (* TODO: apply on all domains. *)
      let cvalue_states = States.fold gather_cvalue states [] in
      Db.Value.Compute_Statement_Callbacks.apply
        (s, Value_util.call_stack (), cvalue_states);
      (* Cleanup function, to be called on all exit paths *)
      let ret result =
        (* Do this as late as possible, as a non-empty to_propagate field
           is shown in a special way in case of degeneration *)
        d.to_propagate <- States.empty;
        Alarmset.end_stmt ();
        result
      in
      if States.is_empty states then ret Dataflow2.SDefault
      else
        (* Snapshot the currently propagated state (curr_wstate) here,
           because this information is modified imperatively by
           [update_stmt_states] *)
        let curr_wcounter, curr_wstate = stmt_widening_info s in
        let states =
          if obviously_terminates
          then states
          else
            (* store the states that we are propagating, and remove the states
               that have already been propagated. Notice that, if the slevel
               is exhausted, the field [widening_state] will contain the
               join of [states], but in practice we may propagate *less*,
               because the states are reduced by assertions/loop invariants. *)
            update_stmt_states s states 
        in
        if States.is_empty states then ret Dataflow2.SDefault
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
            Logic.interp_annot
              ~limit:slevel ~record
              current_kf active_behaviors s annot
              ~initial_state states
          in
          let states = List.fold_left (interp_annot true) states annots in
          if States.is_empty states then ret Dataflow2.SDefault
          else
            let is_return = is_return s in
            let current_info = stmt_state s in
            let old_counter = current_info.counter_unroll in
            let new_states =
              if (old_counter > slevel && not is_return)
              || (is_return && obviously_terminates)
              then (* No slevel left, perform some join and/or widening *)
                let state = States.join states in
                let joined = Bottom.join Domain.join curr_wstate state in
                if Bottom.equal Domain.equal joined curr_wstate then
                  States.empty (* [state] is included in the last propagated
                                  state. Nothing remains to do *)
                else
                if obviously_terminates
                then begin (* User thinks the analysis will terminate: do not widen *)
                  update_stmt_widening_info s 0 joined;
                  states
                end
                else
                  let r =
                    if is_loop s && curr_wcounter = 0 then
                      Bottom.join (Domain.widen current_kf s) curr_wstate joined
                    else
                      joined
                  in
                  let new_wcounter =
                    if curr_wcounter = 0 then 1 else pred curr_wcounter
                  in
                  let new_state = States.singleton' r in
                  if Bottom.equal Domain.equal r joined then (
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
                    let new_joined = States.join new_states in
                    update_stmt_widening_info s new_wcounter new_joined;
                    States.singleton' new_joined
                  end
              else states
            in
            let new_states = doStmtSpecific s d new_states in
            (* This temporary propagation value will be passed on to the successors
               of [s] *)
            ret (Dataflow2.SUse { to_propagate = new_states })

    let doEdge s succ d =
      let kinstr = Kstmt s in
      let states = d.to_propagate in
      Alarmset.start_stmt kinstr;
      (* We store the state after the execution of [s] for the callback
         {Value.Record_Value_After_Callbacks}. This is done here
         because we want to see the values of the variables local to the block *)
      if store_state_after_during_dataflow s succ
      then (
        let old =
          try `Value (StmtHtbl.find states_after s)
          with Not_found -> `Bottom
        in
        let updated =
          States.fold
            (fun s acc -> Bottom.join Domain.join acc (`Value s))
            states old
        in
        StmtHtbl.replace_value states_after s updated
      );
      let index = Wto_statement.wto_index_of_stmt s
      and loops_left, loops_entered =
        Wto_statement.wto_index_diff_of_stmt s succ
      in
      let succ_is_loop_head = match index with
        | s' :: _  -> s'.sid = succ.sid
        | _ -> false
      in
      let blocks_closed = Kernel_function.blocks_closed_by_edge s succ in
      let blocks_opened = Kernel_function.blocks_opened_by_edge s succ in
      (* TODO: scopes and WTOs are linked, so we should try to synchronize
         the two of them. *)
      let do_edge state =
        let enter_block state block =
          Domain.enter_scope current_kf block.blocals state
        in
        let close_block state block =
          Domain.leave_scope current_kf block.blocals state
        in
        let enter_loop = Extlib.swap Transfer.enter_loop in
        let leave_loop = Extlib.swap Transfer.leave_loop in
        let state = List.fold_left close_block state blocks_closed in
        let state = List.fold_left leave_loop state loops_left in
        let state =
          if succ_is_loop_head
          then Transfer.incr_loop_counter s state
          else state
        in
        let state = List.fold_left enter_loop state loops_entered in
        let state = List.fold_left enter_block state blocks_opened in 
        state
      in
      (* We do a simple 'map' here. Duplicates will be removed by States.merge
         later on. *)
      let states = States.map do_edge states in
      Alarmset.end_stmt ();
      d.to_propagate <- states;
      d

    let doGuardOneCond stmt exp positive t =
      if States.is_empty (t.to_propagate)
      then Dataflow2.GUnreachable
      else begin
        Alarmset.start_stmt (Kstmt stmt);
        let new_values =
          States.fold
            (fun state acc ->
               match Transfer.assume ~with_alarms state stmt exp positive with
               | `Bottom -> acc
               | `Value state -> States.add state acc)
            t.to_propagate
            States.empty
        in
        let result =
          if States.is_empty new_values then Dataflow2.GUnreachable
          else Dataflow2.GUse { to_propagate = new_values}
        in
        Alarmset.end_stmt ();
        result
      end

    let mask_then = Db.Value.mask_then
    let mask_else = Db.Value.mask_else
    let mask_both = mask_then lor mask_else

    (* Table storing whether conditions on 'if' have been evaluated
       to true or false *)
    let conditions_table = Cil_datatype.Stmt.Hashtbl.create 5

    let doGuard stmt exp t =
      let th, el as thel =
        doGuardOneCond stmt exp true t, doGuardOneCond stmt exp false t
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
        Value_util.warning_once_current "Do not know which branch to take. Stopping.";
        exit 0
      end;
      let current_condition_status =
        try StmtHtbl.find conditions_table stmt
        with Not_found -> 0
      in
      let new_status =
        current_condition_status lor reachable
      in
      if new_status <> 0
      then StmtHtbl.replace conditions_table stmt new_status;
      Separate.filter_if stmt thel

  end

  (* Check that the dataflow is indeed finished *)
  let checkConvergence () =
    DataflowArg.StmtStartData.iter (fun k v ->
        if not (States.is_empty (v.to_propagate)) then
          Value_parameters.fatal "sid:%d@\n%a@\n"
            k.sid States.pretty v.to_propagate
      )

  (* Walk through all the statements for which [to_propagate] is not empty.
     Those statements are marked as "not fully propagated", for ulterior
     display in the gui. Also mark the current statement as root if relevant.*)
  let mark_degeneration () =
    DataflowArg.StmtStartData.iter
      (fun stmt v ->
         if not (States.is_empty v.to_propagate) then
           Value_util.DegenerationPoints.replace stmt false);
    match Alarmset.current_stmt () with
    | Kglobal -> ()
    | Kstmt s ->
      let kf = Kernel_function.find_englobing_kf s in
      if Kernel_function.equal kf current_kf then (
        Value_util.DegenerationPoints.replace s true;
        Alarmset.end_stmt ())


  let join_final_states states =
    let split i =
      match return_lv with
      | None -> States.join states >>-: States.singleton
      | Some (return_expr, _, _) ->
        let states = States.to_list states in
        let states_list =
          Transfer.split_final_states current_kf return_expr i states
        in
        let states_list = List.map States.of_list states_list in
        let process acc states =
          Bottom.add_to_list (States.join states) acc
        in
        Bottom.bot_of_list (List.fold_left process [] states_list)
        >>-: fun states ->
        States.of_list states
    in
    match Split_return.kf_strategy current_kf with
    | Split_strategy.SplitEqList i -> split i
    | Split_strategy.NoSplit       -> States.join states >>-: States.singleton
    | Split_strategy.FullSplit     -> `Value states
    | Split_strategy.SplitAuto     -> assert false (* transformed into SplitEqList*)

  let results_aux () =
    if DataflowArg.debug then checkConvergence ();
    let final_states = states_unmerged return in
    (* Reduce final states according to the function postcondition *)
    let result = match return_lv with
      | Some (_, _, varinfo) -> Some varinfo
      | None -> None
    in
    Logic.check_fct_postconditions
      current_kf active_behaviors Normal
      ~init_state:initial_state ~post_states:final_states ~result
    >>- fun states ->
    join_final_states states >>- fun states ->
    let return_lval = match return_lv with
      | Some (_, lval, _) -> Some lval
      | None -> None
    in
    let process state acc =
      let return =
        Transfer.return ~with_alarms current_kf return return_lval state
      in
      Bottom.add_to_list return acc
    in
    let states = States.fold process states [] in
    Bottom.bot_of_list states

  let results () =
    Alarmset.start_stmt (Kstmt return);
    let r = results_aux () in
    Alarmset.end_stmt ();
    r

  module Computer = Dataflow2.Forwards (DataflowArg)

  let compute () =
    let start = Kernel_function.find_first_stmt AnalysisParam.kf in
    initial_states >>- fun states ->
    (* Ugly fix because otherwise we are never notified that we are
       entering a loop. *)
    let states =
      if is_loop start
      then States.map (Transfer.enter_loop start) states
      else states
    in
    (* Init the dataflow state for the first statement *)
    let dinit = { to_propagate = states } in
    let dinit = DataflowArg.computeFirstPredecessor start dinit in
    DataflowArg.StmtStartData.add start dinit;
    Computer.compute [start];
    results ()


  let states_before_stmt () =
    let r = StmtHtbl.create (StmtHtbl.length current_table) in
    let aux stmt record =
      let superposed = Partition.join record.superposition
      and widened = record.widening_state in
      let state = Bottom.join Domain.join widened superposed in
      match state with
      | `Bottom -> ()
      | `Value state -> StmtHtbl.add r stmt state
    in
    StmtHtbl.iter aux current_table;
    r

  let states_after_stmt states_before =
    let states_before = Lazy.force states_before in
    StmtHtbl.iter
      (fun stmt state ->
         List.iter
           (fun pred ->
              if not (store_state_after_during_dataflow pred stmt) then
                try
                  let cur = StmtHtbl.find states_after pred in
                  let state = Domain.join state cur in
                  StmtHtbl.replace states_after pred state
                with Not_found ->
                  StmtHtbl.add states_after pred state
           ) stmt.preds;
      ) states_before;
    (* Since the return instruction has no successor, it is not visited
       by the iter above. We fill it manually *)
    (try
       let s = StmtHtbl.find states_before return in
       StmtHtbl.add states_after return s
     with Kernel_function.No_Statement | Not_found -> ()
    );
    states_after

  let register_states pre_states post_states =
    if Mark_noresults.should_memorize_function current_fundec then begin
      let callstack = Value_util.call_stack () in
      let pre_states = Lazy.force pre_states
      and post_states = Lazy.force post_states in
      let process_before_stmt stmt state =
        Domain.Store.register_state_before_stmt callstack stmt state
      in
      Cil_datatype.Stmt.Hashtbl.iter process_before_stmt pre_states;
      let process_after_stmt stmt state =
        Domain.Store.register_state_after_stmt callstack stmt state
      in
      Cil_datatype.Stmt.Hashtbl.iter process_after_stmt post_states;
    end


  let extract_cvalue = Cvalue_domain.extract Domain.get

  let lift_to_cvalues tbl =
    let tbl = Lazy.force tbl in
    let h = StmtHtbl.create (StmtHtbl.length tbl) in
    let process stmt state =
      StmtHtbl.replace h stmt (extract_cvalue (`Value state))
    in
    StmtHtbl.iter process tbl;
    h

  let states_unmerged_for_callbacks () =
    let r = StmtHtbl.create (StmtHtbl.length current_table) in
    let aux stmt record =
      let states = Partition.to_list record.superposition in
      let states = Bottom.add_to_list record.widening_state states in
      let states = List.map (fun v -> extract_cvalue (`Value v)) states in
      StmtHtbl.add r stmt states
    in
    StmtHtbl.iter aux current_table;
    r

  (* TODO: store results for all domains. *)
  let merge_results () =
    let pre_states = lazy (states_before_stmt ()) in
    let post_states = lazy (states_after_stmt pre_states) in
    register_states pre_states post_states;
    let superposed = lazy (lift_to_cvalues pre_states) in
    let after_full = lazy (lift_to_cvalues post_states) in
    let current_superpositions = lazy (states_unmerged_for_callbacks ()) in
    let stack_for_callbacks = Value_util.call_stack () in
    if Mark_noresults.should_memorize_function current_fundec then
      Db.Value.merge_conditions DataflowArg.conditions_table;
    if not (Db.Value.Record_Value_Superposition_Callbacks.is_empty ())
    then begin
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
                                    (Mem_exec2.new_counter ())))
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

end


module Computer
    (Domain : Abstract_domain.External)
    (States : Partitioning.StateSet with type state = Domain.t)
    (Transfer : Transfer_stmt.S with type state = Domain.t
                                 and type value = Domain.value
                                 and type return = Domain.return)
    (Logic : Transfer_logic.S with type state = Domain.t
                               and type states = States.t)
= struct

  let compute kf call_kinstr state =
    let module Dataflow =
      Make_Dataflow
        (Domain)
        (States)
        (Transfer)
        (Logic)
        (struct
          let kf = kf
          let call_kinstr = call_kinstr
          let initial_state = state
        end)
    in
    try
      let results = Dataflow.compute () in
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.feedback "Recording results for %a"
          Kernel_function.pretty kf;
      Dataflow.merge_results ();
      let f = Kernel_function.get_definition kf in
      (match results with
       | `Value (_::_) when Cil.hasAttribute "noreturn" f.svar.vattr ->
         Value_util.warning_once_current
           "function %a may terminate but has the noreturn attribute"
           Kernel_function.pretty kf;
       | _ -> ());
      results, !Dataflow.cacheable
    with Db.Value.Aborted as e ->
      (* analysis was aborted: pop the call stack and inform the caller *)
      Dataflow.mark_degeneration ();
      Db.Value.mark_as_computed ();
      Dataflow.merge_results ();
      raise e

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
