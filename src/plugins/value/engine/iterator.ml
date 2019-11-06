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
open Interpreted_automata
open Bottom.Type

[@@@warning "-42"]


let check_signals, signal_abort =
  let signal_emitted = ref false in
  (fun () ->
     if !signal_emitted then begin
       signal_emitted := false;
       raise Db.Value.Aborted
     end),
  (fun () -> signal_emitted := true)

let dkey = Value_parameters.dkey_iterator
let dkey_callbacks = Value_parameters.dkey_callbacks

let blocks_share_locals b1 b2 =
  match b1.blocals, b2.blocals with
  | [], [] -> true
  | v1 :: _, v2 :: _ -> v1.vid = v2.vid
  | _, _ -> false

module Make_Dataflow
    (Abstract : Abstractions.Eva)
    (States : Powerset.S with type state = Abstract.Dom.t)
    (Transfer : Transfer_stmt.S with type state = Abstract.Dom.t)
    (Init: Initialization.S with type state := Abstract.Dom.t)
    (Logic : Transfer_logic.S with type state = Abstract.Dom.t
                               and type states = States.t)
    (Spec: sig
       val treat_statement_assigns: assigns -> Abstract.Dom.t -> Abstract.Dom.t
     end)
    (AnalysisParam : sig
       val kf: kernel_function
       val call_kinstr: kinstr
       val initial_state : Abstract.Dom.t
     end)
    ()
= struct

  module Domain = Abstract.Dom

  (* --- Analysis parameters --- *)

  let kf = AnalysisParam.kf
  let fundec = Kernel_function.get_definition kf
  let cacheable = ref Value_types.Cacheable


  (* --- Plugin parameters --- *)

  type descending_strategy = NoIteration | FullIteration | ExitIteration

  let descending_iteration : descending_strategy =
    match Value_parameters.DescendingIteration.get () with
    | "no" -> NoIteration
    | "exits" -> ExitIteration
    | "full" -> FullIteration
    | _ -> assert false

  let hierachical_convergence : bool =
    Value_parameters.HierarchicalConvergence.get ()

  let interpreter_mode =
    Value_parameters.InterpreterMode.get ()

  (* Ideally, the slevel parameter should not be used anymore in this file
     but it is still required for logic interpretation *)
  let slevel =
    let module P = Partitioning_parameters.Make (AnalysisParam) in
    P.slevel


  (* --- Abstract values storage --- *)

  module Partition = Trace_partitioning.Make (Abstract) (Transfer) (AnalysisParam)

  type store = Partition.store
  type flow = Partition.flow
  type tank = Partition.tank
  type widening = Partition.widening

  type edge_info = {
    mutable fireable : bool (* Does any states survive the transition ? *)
  }


  (* --- Interpreted automata --- *)

  let automaton = get_automaton kf
  let graph = automaton.graph
  let control_point_count = G.nb_vertex graph
  let transition_count = G.nb_edges graph


  (* --- Initial state --- *)

  let active_behaviors = Logic.create AnalysisParam.initial_state kf

  (* Compute the locals that we must enter in scope when we start the analysis
     of [block]. The other ones will be introduced on the fly, when we
     encounter a [Local_init] instruction. *)
  let block_toplevel_locals block =
    List.filter (fun vi -> not vi.vdefined) block.blocals

  let initial_states =
    let state = AnalysisParam.initial_state
    and call_kinstr = AnalysisParam.call_kinstr
    and ab = active_behaviors in
    if Value_util.skip_specifications kf then
      States.singleton state
    else match Logic.check_fct_preconditions call_kinstr kf ab state with
      | `Bottom -> States.empty
      | `Value set -> set

  let initial_state =
    match States.join initial_states with
    | `Bottom -> Domain.top (* No analysis in this case. *)
    | `Value state -> state

  let initial_tank =
    Partition.initial_tank (States.to_list initial_states)
  let get_initial_flow () =
    -1 (* Dummy edge id *), Partition.drain initial_tank

  let post_conditions = ref false

  (* --- Analysis state --- *)

  (* Reference to the current statement processed by the analysis.
     Only needed when the analysis aborts, to mark the current statement
     as the degeneration point.*)
  let current_ki = ref Kglobal

  module VertexTable = struct
    include Interpreted_automata.Vertex.Hashtbl
    let find_or_add (t : 'a t) (key : key) ~(default : unit -> 'a) : 'a =
      try find t key
      with Not_found ->
        let x = default () in add t key x; x
  end

  module EdgeTable = struct
    include Interpreted_automata.Edge.Hashtbl
    let find_or_add (t : 'a t) (key : key) ~(default : unit -> 'a) : 'a =
      try find t key
      with Not_found ->
        let x = default () in add t key x; x
  end

  (* The stored state on vertex and edges *)
  let v_table : store VertexTable.t =
    VertexTable.create control_point_count
  let w_table : widening VertexTable.t =
    VertexTable.create 7
  let e_table : (tank * edge_info) EdgeTable.t =
    EdgeTable.create transition_count

  (* Default (Initial) stores on vertex and edges *)
  let default_vertex_store (v : vertex) () : store =
    Partition.empty_store ~stmt:v.vertex_start_of
  let default_vertex_widening (v : vertex) () : widening =
    Partition.empty_widening ~stmt:v.vertex_start_of
  let default_edge_tank () : tank * edge_info =
    Partition.empty_tank (), { fireable = false }

  (* Get the stores associated to a control point or edge *)
  let get_vertex_store (v : vertex) : store =
    VertexTable.find_or_add v_table v ~default:(default_vertex_store v)
  let get_vertex_widening (v : vertex) : widening =
    VertexTable.find_or_add w_table v ~default:(default_vertex_widening v)
  let get_edge_data (e : vertex edge) : tank * edge_info =
    EdgeTable.find_or_add e_table e ~default:default_edge_tank
  let get_succ_tanks (v : vertex) : tank list =
    List.map (fun (_,e,_) -> fst (get_edge_data e)) (G.succ_e graph v)

  module StmtTable = struct
    include Cil_datatype.Stmt.Hashtbl
    let map (f : key -> 'a  -> 'b) (t : 'a t) : 'b t =
      let r = create (length t) in
      iter (fun k v -> add r k (f k v)) t;
      r

    let map' (f : key -> 'a  -> 'b or_bottom) (t : 'a t) : 'b t =
      let r = create (length t) in
      let aux k v =
        match f k v with
        | `Bottom -> ()
        | `Value x -> add r k x
      in
      iter aux t;
      r
  end


  (* --- Transfer functions application --- *)

  type state = Domain.t

  type transfer_function = state -> state list

  let id : transfer_function = fun x -> [x]

  (* Thse lifting function helps to uniformize the transfer functions to a
     common signature *)

  let lift (f : state -> state) : transfer_function =
    fun x -> [f x]

  let lift' (f : state -> state or_bottom) : transfer_function =
    fun x -> Bottom.to_list (f x)

  let sequence (f1 : transfer_function) (f2 : transfer_function)
    : transfer_function =
    fun x -> List.fold_left (fun acc y -> f2 y @ acc) [] (f1 x)


  (* Tries to evaluate \assigns … \from … clauses for assembly code. *)
  let transfer_asm (stmt : stmt) : transfer_function =
    let asm_contracts = Annotations.code_annot stmt in
    match Logic_utils.extract_contract asm_contracts with
    | [] ->
      Value_util.warning_once_current
        "assuming assembly code has no effects in function %t"
        Value_util.pretty_current_cfunction_name;
      id
    (* There should be only one statement contract, if any. *)
    | (_, spec) :: _ ->
      let assigns = Ast_info.merge_assigns_from_spec ~warn:false spec in
      lift (Spec.treat_statement_assigns assigns)

  let transfer_assume (stmt : stmt) (exp : exp) (kind : guard_kind)
    : transfer_function =
    let positive = (kind = Then) in
    lift' (fun s -> Transfer.assume s stmt exp positive)

  let transfer_assign (stmt : stmt) (dest : Cil_types.lval) (exp : exp)
    : transfer_function =
    lift' (fun s -> Transfer.assign s (Kstmt stmt) dest exp)

  let transfer_enter (block : block) : transfer_function =
    let vars = block_toplevel_locals block in
    if vars = [] then id else lift (Transfer.enter_scope kf vars)

  let transfer_leave (block : block) : transfer_function =
    let vars = block.blocals in
    if vars = [] then id else lift (Domain.leave_scope kf vars)

  let transfer_call (stmt : stmt) (dest : lval option) (callee : exp)
      (args : exp list) (state : state) : state list =
    let result, call_cacheable =
      Transfer.call stmt dest callee args state
    in
    if call_cacheable = Value_types.NoCacheCallers then
      (* Propagate info that the current call cannot be cached either *)
      cacheable := Value_types.NoCacheCallers;
    Bottom.list_of_bot result

  let transfer_instr (stmt : stmt) (instr : instr) : transfer_function =
    match instr with
    | Local_init (vi, AssignInit exp, _loc) ->
      let transfer state =
        let state = Domain.enter_scope kf [vi] state in
        Init.initialize_local_variable stmt vi exp state
      in
      lift' transfer
    | Local_init (vi, ConsInit (f, args, k), loc) ->
      let as_func dest callee args _loc state =
        (* This variable enters the scope too early, as it should
           be introduced after the call to [f] but before the assignment
           to [v]. This is currently not possible, at least without
           splitting Transfer.call in two. *)
        let state = Domain.enter_scope kf [vi] state in
        transfer_call stmt dest callee args state
      in
      Cil.treat_constructor_as_func as_func vi f args k loc
    | Set (dest, exp, _loc) ->
      transfer_assign stmt dest exp
    | Call (dest, callee, args, _loc) ->
      transfer_call stmt dest callee args
    | Asm _ ->
      transfer_asm stmt
    | Skip _loc -> id
    | Code_annot (_,_loc) -> id (* already done in process_statement
                                   from the annotation table *)

  let transfer_return (stmt : stmt) (return_exp : exp option)
    : transfer_function =
    (* Deconstruct return statement *)
    let return_var = match return_exp with
      | Some {enode = Lval (Var v, NoOffset)} -> Some v
      | None -> None
      | _ -> assert false (* Cil invariant *)
    in
    (* Check postconditions *)
    let check_postconditions = fun state ->
      post_conditions := true;
      if Value_util.skip_specifications kf then
        [state]
      else match
          Logic.check_fct_postconditions kf active_behaviors Normal
            ~pre_state:initial_state ~post_states:(States.singleton state)
            ~result:return_var
        with
        | `Bottom -> []
        | `Value v -> States.to_list v
    (* Assign the return value *)
    and assign_retval =
      match return_exp with
      | None -> id
      | Some return_exp ->
        let vi_ret = Extlib.the (Library_functions.get_retres_vi kf) in
        let return_lval = Var vi_ret, NoOffset in
        let kstmt = Kstmt stmt in
        fun state ->
          let state = Domain.enter_scope kf [vi_ret] state in
          let state' = Transfer.assign state kstmt return_lval return_exp in
          Bottom.to_list state'
    in
    sequence check_postconditions assign_retval

  let transfer_transition (t : vertex transition) : transfer_function =
    match t with
    | Skip ->                     id
    | Return (return_exp,stmt) -> transfer_return stmt return_exp
    | Guard (exp,kind,stmt) ->    transfer_assume stmt exp kind
    | Instr (instr,stmt) ->       transfer_instr stmt instr
    | Enter (block) ->            transfer_enter block
    | Leave (block) when blocks_share_locals fundec.sbody block ->
      (* The variables from the toplevel block will be removed by the caller *)
      id
    | Leave (block) ->            transfer_leave block
    | Prop _ -> id (* Annotations are interpreted in [transfer_statement]. *)

  let transfer_annotations (stmt : stmt) ~(record : bool) : transfer_function =
    let annots =
      (* We do not interpret annotations that come from statement contracts
         and everything previously emitted by Value (currently, alarms) *)
      let filter e ca =
        not (Logic_utils.is_contract ca || Emitter.equal e Value_util.emitter)
      in
      List.map fst (Annotations.code_annot_emitter ~filter stmt)
    in
    fun state ->
      let interp_annot states ca =
        Logic.interp_annot
          ~limit:(slevel stmt) ~record
          kf active_behaviors stmt ca
          ~initial_state states
      in
      States.to_list
        (List.fold_left interp_annot (States.singleton state) annots)

  let transfer_statement (stmt : stmt) (state : state) : state list =
    (* Interpret annotations *)
    let states = transfer_annotations stmt ~record:true state in
    (* Check unspecified sequences *)
    match stmt.skind with
    | UnspecifiedSequence seq when Kernel.UnspecifiedAccess.get () ->
      let check s =
        Transfer.check_unspecified_sequence stmt s seq = `Value ()
      in
      List.filter check states
    | _ -> states


  (* --- Iteration strategy ---*)

  let process_partitioning_transitions (v1 : vertex) (v2 : vertex)
      (transition : vertex transition) (flow : flow) : flow =
    (* Split return *)
    let flow = match transition with
      | Return (return_exp, _) -> Partition.split_return flow return_exp
      | _ -> flow
    in
    (* Loop transitions *)
    let the_stmt v = Extlib.the v.vertex_start_of in
    let enter_loop f v =
      let f = Partition.enter_loop f (the_stmt v) in
      Partition.transfer (lift (Domain.enter_loop (the_stmt v))) f
    and leave_loop f v =
      let f = Partition.leave_loop f (the_stmt v) in
      Partition.transfer (lift (Domain.leave_loop (the_stmt v))) f
    and incr_loop_counter f v =
      let f = Partition.next_loop_iteration f (the_stmt v) in
      Partition.transfer (lift (Domain.incr_loop_counter (the_stmt v))) f
    in
    let loops_left, loops_entered =
      Interpreted_automata.get_wto_index_diff kf v1 v2
    and loop_incr =
      Interpreted_automata.is_back_edge kf (v1,v2)
    in
    let flow = List.fold_left leave_loop flow loops_left in
    let flow = List.fold_left enter_loop flow loops_entered in
    if loop_incr then incr_loop_counter flow v2 else flow

  let process_edge (v1,e,v2 : G.edge) : flow =
    let {edge_transition=transition; edge_kinstr=kinstr} = e in
    let tank,edge_info = get_edge_data e in
    let flow = Partition.drain tank in
    !Db.progress ();
    check_signals ();
    current_ki := kinstr;
    Cil.CurrentLoc.set e.edge_loc;
    let flow = Partition.transfer (transfer_transition transition) flow in
    let flow = process_partitioning_transitions v1 v2 transition flow in
    if not (Partition.is_empty_flow flow) then
      edge_info.fireable <- true;
    flow

  let gather_cvalues states = match Domain.get_cvalue with
    | Some get -> List.map get states
    | None -> []

  let call_statement_callbacks (stmt : stmt) (f : flow) : unit =
    (* TODO: apply on all domains. *)
    let states = Partition.contents f in
    let cvalue_states = gather_cvalues states in
    Db.Value.Compute_Statement_Callbacks.apply
      (stmt, Value_util.call_stack (), cvalue_states)

  let update_vertex ?(widening : bool = false) (v : vertex)
      (sources : ('branch * flow) list) : bool =
    begin match v.vertex_start_of with
      | Some stmt ->
        (* Set location *)
        current_ki := Kstmt stmt;
        let current_loc = Cil_datatype.Stmt.loc stmt in
        Cil.CurrentLoc.set current_loc
      | None -> ()
    end;
    (* Get vertex store *)
    let store = get_vertex_store v in
    (* Join incoming s tates *)
    let flow = Partition.join sources store in
    let flow =
      match v.vertex_start_of with
      | Some stmt ->
        (* Callbacks *)
        call_statement_callbacks stmt flow;
        (* Transfer function associated to the statement *)
        Partition.transfer (transfer_statement stmt) flow
      | _ -> flow
    in
    (* Widen if necessary *)
    let flow =
      if widening && not (Partition.is_empty_flow flow) then begin
        let flow = Partition.widen (get_vertex_widening v) flow in
        (* Try to correct over-widenings *)
        let correct_over_widening stmt =
          (* Do *not* record the status after interpreting the annotation
             here. Possible unproven assertions have already been recorded
             when the assertion has been interpreted the first time higher
             in this function. *)
          Partition.transfer (transfer_annotations stmt ~record:false) flow
        in
        Extlib.may_map correct_over_widening ~dft:flow v.vertex_start_of
      end else
        flow
    in
    (* Dispatch to successors *)
    List.iter (fun into -> Partition.fill flow ~into) (get_succ_tanks v);
    (* Return whether the iterator should stop or not *)
    Partition.is_empty_flow flow

  let process_vertex ?(widening : bool = false) (v : vertex) : bool =
    (* Process predecessors *)
    let process_source (_,e,_ as edge) =
      e.edge_key, process_edge edge
    in
    let sources = List.map process_source (G.pred_e graph v) in
    (* Add initial source *)
    let sources =
      if v <> automaton.entry_point
      then sources
      else get_initial_flow () :: sources
    in
    (* Update the vertex *)
    update_vertex ~widening v sources

  let rec simulate (v : vertex) (source : 'branch * flow) : unit =
    (* Update the current vertex *)
    ignore (update_vertex v [source]);
    (* Try every possible successor *)
    let add_if_fireable (_,e,succ as edge) acc =
      let f = process_edge edge in
      if Partition.is_empty_flow f
      then acc
      else (e.edge_key,f,succ) :: acc
    in
    let successors = G.fold_succ_e add_if_fireable graph v [] in
    (* How many possible successors ? *)
    match successors with
    | [] -> () (* No successor - end of simulation *)
    | [b,f,succ] -> (* One successor - continue simulation *)
      simulate succ (b,f)
    | _ -> (* Several successors - failure *)
      Value_parameters.abort "Do not know which branch to take. Stopping."

  let reset_component (vertex_list : vertex list) : unit =
    let reset_edge (_,e,_) =
      let t,_ = get_edge_data e in
      Partition.reset_tank t
    in
    let reset_vertex v =
      let s = get_vertex_store v
      and w = get_vertex_widening v in
      Partition.reset_store s;
      Partition.reset_widening w;
      List.iter reset_edge (G.succ_e graph v)
    in
    List.iter reset_vertex vertex_list

  let rec iterate_list (l : wto) =
    List.iter iterate_element l
  and iterate_element = function
    | Wto.Node v ->
      ignore (process_vertex v)
    | Wto.Component (v, w) as component ->
      (* Reset the component if hierachical_convergence is set.
         Otherwise, only resets the widening counter for this component. This
         is especially useful for nested loops. *)
      if hierachical_convergence
      then reset_component (v :: Wto.flatten w)
      else Partition.reset_widening_counter (get_vertex_widening v);
      (* Iterate until convergence *)
      let iteration_count = ref 0 in
      while
        not (process_vertex ~widening:true v) || !iteration_count = 0
      do
        Value_parameters.debug ~dkey "iteration %d" !iteration_count;
        iterate_list w;
        incr iteration_count;
      done;
      (* Descending sequence *)
      let l =  match descending_iteration with
        | NoIteration -> []
        | ExitIteration ->
          Value_parameters.debug ~dkey
            "propagating descending values through exit paths";
          Wto.flatten (exit_strategy graph component)
        | FullIteration ->
          Value_parameters.debug ~dkey
            "propagating descending values through the loop";
          v :: Wto.flatten w
      in
      List.iter (fun v -> ignore (process_vertex v)) l

  (* Walk through all the statements for which [needs_propagation] returns
     true. Those statements are marked as "not fully propagated", for
     ulterior display in the gui. Also mark the current statement as root if
     relevant.*)
  let mark_degeneration () =
    let f stmt (v,_) =
      let l = get_succ_tanks v in
      if not (List.for_all Partition.is_empty_tank l) then
        Value_util.DegenerationPoints.replace stmt false
    in
    StmtTable.iter f automaton.stmt_table;
    match !current_ki with
    | Kglobal -> ()
    | Kstmt s ->
      let englobing_kf = Kernel_function.find_englobing_kf s in
      if Kernel_function.equal englobing_kf kf then (
        Value_util.DegenerationPoints.replace s true)

  (* If the postconditions have not been evaluated, mark them as true. *)
  let mark_postconds_as_true () =
    ignore (Logic.check_fct_postconditions kf active_behaviors Normal
              ~pre_state:initial_state ~post_states:States.empty ~result:None)

  let compute () : state list or_bottom =
    if interpreter_mode then
      simulate automaton.entry_point (get_initial_flow ())
    else begin
      let wto = Interpreted_automata.get_wto kf in
      iterate_list wto
    end;
    if not !post_conditions then mark_postconds_as_true ();
    let final_store = get_vertex_store automaton.return_point in
    Bottom.bot_of_list (Partition.expanded final_store)


  (* --- Results conversion --- *)

  let merge_conditions () =
    let table = StmtTable.create 5 in
    let fill (_,e,_) =
      match e.edge_transition with
      | Guard (_exp,kind,stmt) ->
        let mask = match kind with
          | Then -> Db.Value.mask_then
          | Else -> Db.Value.mask_else
        in
        let edge_info = snd (get_edge_data e) in
        let old_status =
          try StmtTable.find table stmt
          with Not_found -> 0
        and status =
          if edge_info.fireable then mask else 0
        in
        let new_status = old_status lor status in
        StmtTable.replace table stmt new_status;
      | _ -> ()
    in
    G.iter_edges_e fill graph;
    Db.Value.merge_conditions table

  let is_instr s = match s.skind with Instr _ -> true | _ -> false

  let states_after_stmt states_before states_after =
    let return_stmt = Kernel_function.find_return kf in
    let states_before = Lazy.force states_before in
    let states_after = Lazy.force states_after in
    StmtTable.iter
      (fun stmt state ->
         List.iter
           (fun pred ->
              if not (is_instr pred) then
                try
                  let cur = StmtTable.find states_after pred in
                  let state = Domain.join state cur in
                  StmtTable.replace states_after pred state
                with Not_found ->
                  StmtTable.add states_after pred state
           ) stmt.preds;
      ) states_before;
    (* Since the return instruction has no successor, it is not visited
       by the iter above. We fill it manually *)
    (try
       let s = StmtTable.find states_before return_stmt in
       StmtTable.add states_after return_stmt s
     with Kernel_function.No_Statement | Not_found -> ()
    );
    states_after

  let merge_results () =
    let get_merged_states =
      let merged_states = VertexTable.create control_point_count
      and get_smashed_store v =
        let store = get_vertex_store v in
        Partition.smashed store
      in
      fun ~all stmt (v : vertex) ->
        if all || is_instr stmt
        then VertexTable.memo merged_states v get_smashed_store
        else `Bottom
    and lift_to_cvalues table =
      StmtTable.map (fun _ s -> Domain.get_cvalue_or_top s) (Lazy.force table)
    in
    let merged_pre_states = lazy
      (StmtTable.map' (fun s (v,_) -> get_merged_states ~all:true s v) automaton.stmt_table)
    in
    let merged_post_states = lazy
      (StmtTable.map' (fun s (_,v) -> get_merged_states ~all:false s v) automaton.stmt_table)
    in
    let merged_post_states = lazy
      (states_after_stmt merged_pre_states merged_post_states)
    in
    let unmerged_pre_cvalues = lazy
      (StmtTable.map (fun _stmt (v,_) ->
           let store = get_vertex_store v in
           let states = Partition.expanded store in
           List.map (fun x -> Domain.get_cvalue_or_top x) states)
          automaton.stmt_table)
    in
    let merged_pre_cvalues = lazy (lift_to_cvalues merged_pre_states)
    and merged_post_cvalues = lazy (lift_to_cvalues merged_post_states) in
    let callstack = Value_util.call_stack () in
    if Mark_noresults.should_memorize_function fundec then begin
      let register_pre = Domain.Store.register_state_before_stmt callstack
      and register_post = Domain.Store.register_state_after_stmt callstack in
      StmtTable.iter register_pre (Lazy.force merged_pre_states);
      StmtTable.iter register_post (Lazy.force merged_post_states);
      merge_conditions ();
    end;
    if not (Db.Value.Record_Value_Superposition_Callbacks.is_empty ())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value_Superposition callbacks";
      Db.Value.Record_Value_Superposition_Callbacks.apply
        (callstack, unmerged_pre_cvalues);
    end;
    if not (Db.Value.Record_Value_Callbacks.is_empty ())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value callbacks";
      Db.Value.Record_Value_Callbacks.apply
        (callstack, merged_pre_cvalues)
    end;
    if not (Db.Value.Record_Value_Callbacks_New.is_empty ())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_Value_New callbacks";
      if Value_parameters.MemExecAll.get () then
        Db.Value.Record_Value_Callbacks_New.apply
          (callstack,
           Value_types.NormalStore ((merged_pre_cvalues, merged_post_cvalues),
                                    (Mem_exec.new_counter ())))
      else
        Db.Value.Record_Value_Callbacks_New.apply
          (callstack,
           Value_types.Normal (merged_pre_cvalues, merged_post_cvalues))
    end;
    if not (Db.Value.Record_Value_After_Callbacks.is_empty ())
    then begin
      if Value_parameters.ValShowProgress.get () then
        Value_parameters.debug ~dkey:dkey_callbacks
          "now calling Record_After_Value callbacks";
      Db.Value.Record_Value_After_Callbacks.apply
        (callstack, merged_post_cvalues);
    end;

end


module Computer
    (Abstract : Abstractions.Eva)
    (States : Powerset.S with type state = Abstract.Dom.t)
    (Transfer : Transfer_stmt.S with type state = Abstract.Dom.t)
    (Init: Initialization.S with type state := Abstract.Dom.t)
    (Logic : Transfer_logic.S with type state = Abstract.Dom.t
                               and type states = States.t)
    (Spec: sig
       val treat_statement_assigns: assigns -> Abstract.Dom.t -> Abstract.Dom.t
     end)
= struct

  let compute kf call_kinstr state =
    let module Dataflow =
      Make_Dataflow
        (Abstract) (States) (Transfer) (Init) (Logic) (Spec)
        (struct
          let kf = kf
          let call_kinstr = call_kinstr
          let initial_state = state
        end)
        ()
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
