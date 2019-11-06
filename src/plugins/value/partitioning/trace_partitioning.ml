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
open Bottom.Type
open Partition

module Make
    (Abstract: Abstractions.Eva)
    (Transfer : Transfer_stmt.S with type state = Abstract.Dom.t)
    (Kf : sig val kf: kernel_function end) =
struct
  module Parameters = Partitioning_parameters.Make (Kf)

  open Kf
  open Parameters

  module Domain = Abstract.Dom

  module Index = Partitioning_index.Make (Domain)
  module Flow = Partition.MakeFlow (Abstract)

  type state = Domain.t

  type store = {
    rationing: Partition.rationing; (* slevel rationing at this point *)
    flow_actions : action list; (* partitioning actions to be applied *)
    store_stmt : stmt option;
    store_index : Index.t; (* Index of all states stored: used to quickly remove
                              new states that have already been propagated. *)
    mutable store_partition : state partition; (* partition of states *)
    mutable incoming_states : int; (* number of incoming states. *)
  }

  type flow = Flow.t

  type tank = {
    mutable tank_states : state partition;
  }

  type widening_state = {
    mutable widened_state : state option;
    mutable previous_state : state;
    mutable widening_counter : int;
  }

  type widening = {
    widening_stmt : stmt;
    mutable widening_partition : widening_state partition;
  }

  (* Constructors *)

  let empty_store ~(stmt : stmt option) : store =
    let limit, merge, flow_actions = match stmt with
      | None -> max_int, false, []
      | Some stmt -> slevel stmt, merge stmt, flow_actions stmt
    in
    let rationing = Partition.new_rationing ~limit ~merge in
    {
      rationing; flow_actions;
      store_stmt = stmt;
      store_index = Index.empty ();
      store_partition = Partition.empty;
      incoming_states = 0;
    }

  let empty_flow : flow = Flow.empty

  let empty_tank () : tank =
    { tank_states = Partition.empty }

  let empty_widening ~(stmt : stmt option) : widening =
    {
      widening_stmt = Extlib.opt_conv Cil.invalidStmt stmt;
      widening_partition = Partition.empty;
    }

  let initial_tank (states : state list) : tank =
    let flow = Flow.initial states in
    (* Split the initial partition according to the global split seetings *)
    let states = List.fold_left Flow.transfer_keys flow universal_splits in
    { tank_states = Flow.to_partition states }


  (* Pretty printing *)

  let pretty_store (fmt : Format.formatter) (s : store) : unit =
    Partition.iter (fun _key state -> Domain.pretty fmt state) s.store_partition

  let pretty_flow (fmt : Format.formatter) (flow : flow) =
    Flow.iter (Domain.pretty fmt) flow


  (* Accessors *)

  let expanded (s : store) : state list =
    Partition.to_list s.store_partition

  let smashed (s : store) : state or_bottom =
    match expanded s with
    | [] -> `Bottom
    | v1 :: l -> `Value (List.fold_left Domain.join v1 l)

  let contents (flow : flow) : state list =
    Flow.to_list flow

  let is_empty_store (s : store) : bool =
    Partition.is_empty s.store_partition

  let is_empty_flow (flow : flow) : bool =
    Flow.is_empty flow

  let is_empty_tank (t : tank) : bool =
    Partition.is_empty t.tank_states

  let store_size (s : store) : int =
    Partition.size s.store_partition

  let flow_size (flow : flow) : int =
    Flow.size flow

  let tank_size (t : tank) : int =
    Partition.size t.tank_states


  (* Partition transfer functions *)

  let enter_loop (flow : flow) (i : stmt) : flow =
    Flow.transfer_keys flow (Enter_loop (unroll i))

  let leave_loop (flow : flow) (_i : stmt) : flow =
    Flow.transfer_keys flow Leave_loop

  let next_loop_iteration (flow : flow) (_i : stmt) : flow =
    Flow.transfer_keys flow Incr_loop

  let empty_rationing = new_rationing ~limit:0 ~merge:false

  let split_return (flow : flow) (return_exp : exp option) : flow =
    let strategy = Split_return.kf_strategy kf in
    if strategy = Split_strategy.FullSplit
    then flow
    else
      let apply action =
        Flow.join_duplicate_keys (Flow.transfer_keys flow action)
      in
      match Split_return.kf_strategy kf with
      (* SplitAuto already transformed into SplitEqList. *)
      | Split_strategy.FullSplit | Split_strategy.SplitAuto -> assert false
      | Split_strategy.NoSplit -> apply (Ration empty_rationing)
      | Split_strategy.SplitEqList i ->
        match return_exp with
        | None -> apply (Ration empty_rationing)
        | Some return_exp ->
          if Cil.isIntegralOrPointerType (Cil.typeOf return_exp)
          then apply (Restrict (return_exp, i))
          else apply (Ration empty_rationing)

  (* Reset state (for hierchical convergence) *)

  let reset_store (s : store) : unit =
    let is_eternal key _state = not (Key.exceed_rationing key) in
    s.store_partition <- Partition.filter is_eternal s.store_partition

  let reset_tank (t : tank) : unit =
    t.tank_states <- Partition.empty

  let reset_widening (w : widening) : unit =
    w.widening_partition <- Partition.empty

  let reset_widening_counter (w : widening) : unit =
    let reset w =
      { w with widening_counter = max w.widening_counter (widening_period - 1) }
    in
    w.widening_partition <- Partition.map reset w.widening_partition


  (* Operators *)

  let drain (t : tank) : flow =
    let flow = Flow.of_partition t.tank_states in
    t.tank_states <- Partition.empty;
    flow

  let fill ~(into : tank) (flow : flow) : unit =
    let new_states = Flow.to_partition flow in
    let join _key dest src = match dest, src with
      | Some dest, Some src -> Some (Domain.join dest src)
      | Some v, None | None, Some v -> Some v
      | None, None -> None
    in
    into.tank_states <- Partition.merge join into.tank_states new_states

  let transfer = Flow.transfer_states

  let output_slevel : int -> unit =
    let slevel_display_step = Value_parameters.ShowSlevel.get () in
    let max_displayed = ref 0 in
    fun x ->
      if x >= !max_displayed + slevel_display_step
      then
        let rounded = x / slevel_display_step * slevel_display_step in
        Value_parameters.feedback ~once:true ~current:true
          "Trace partitioning superposing up to %d states"
          rounded;
        max_displayed := rounded

  let partitioning_feedback dest flow stmt =
    output_slevel dest.incoming_states;
    (* Debug information. *)
    Value_parameters.debug ~dkey:Value_parameters.dkey_iterator ~current:true
      "reached statement %d with %d incoming states, %d to propagate"
      stmt.sid dest.incoming_states (flow_size flow)

  let join (sources : (branch*flow) list) (dest : store) : flow =
    let is_loop_head =
      match dest.store_stmt with
      | Some {skind=Cil_types.Loop _} -> true
      | _ -> false
    in
    (* Get every source flow *)
    let sources_states =
      match sources with
      | [(_,flow)] -> [flow]
      | sources ->
        (* Several branches -> partition according to the incoming branch *)
        let get (b,flow) =
          Flow.transfer_keys flow (Branch (b,history_size))
        in
        List.map get sources
    in
    (* Merge incomming flows *)
    let flow_states =
      List.fold_left Flow.union Flow.empty sources_states
    in
    (* Handle ration stamps *)
    dest.incoming_states <- dest.incoming_states + Flow.size flow_states;
    let rationing_action = Ration dest.rationing in
    (* Handle Split / Merge operations *)
    let flow_actions = Update_dynamic_splits :: dest.flow_actions in
    (* Execute actions *)
    let actions = rationing_action :: flow_actions in
    let flow_states =
      List.fold_left Flow.transfer_keys flow_states actions
    in
    (* Add states to the store but filter out already propagated states *)
    let update key current_state =
      (* Inclusion test *)
      let state =
        try
          let previous_state = Partition.find key dest.store_partition in
          if Domain.is_included current_state previous_state then
            (* The current state is included in the previous; stop *)
            None
          else begin
            (* Propagate the join of the two states *)
            if is_loop_head then
              Value_parameters.feedback ~level:1 ~once:true ~current:true
                "starting to merge loop iterations";
            Some (Domain.join previous_state current_state)
          end
        with
        (* There is no previous state, propagate normally *)
          Not_found -> Some current_state
      in
      (* Add the propagated state to the store *)
      let add s =
        dest.store_partition <- Partition.replace key s dest.store_partition;
      in
      Extlib.may add state;
      (* Filter out already propagated states (only at statements). *)
      if dest.store_stmt = None
      then state
      else Extlib.opt_filter (fun s -> Index.add s dest.store_index) state
    in
    let flow = Flow.join_duplicate_keys flow_states in
    let flow = Flow.filter_map update flow in
    Extlib.may (partitioning_feedback dest flow) dest.store_stmt;
    flow

  let widen (w : widening) (flow : flow) : flow =
    let stmt = w.widening_stmt in
    (* Apply widening to each leaf *)
    let widen_one key curr =
      try
        (* Search for an already existing widening state *)
        let w = Partition.find key w.widening_partition in
        let previous_state = w.previous_state in
        (* Update the widening state *)
        w.previous_state <- curr;
        w.widening_counter <- w.widening_counter - 1;
        (* Propagated state decreases, stop propagating *)
        if Domain.is_included curr previous_state then
          None
          (* Widening is delayed *)
        else if w.widening_counter >= 0 then
          Some curr
          (* Apply widening *)
        else begin
          Value_parameters.feedback ~level:1 ~once:true ~current:true
            ~dkey:Value_parameters.dkey_widening
            "applying a widening at this point";
          (* We join the previous widening state with the previous iteration
             state so as to allow the intermediate(s) iteration(s) (between
             two widenings) to stabilize at least a part of the state. *)
          let prev = match w.widened_state with
            | Some v -> Domain.join previous_state v
            | None -> previous_state
          in
          let next = Domain.widen kf stmt prev (Domain.join prev curr) in
          w.previous_state <- next;
          w.widened_state <- Some next;
          w.widening_counter <- widening_period - 1;
          Some next
        end
      with Not_found ->
        (* The key is not in the widening state; add the state if slevel is
           exceeded. *)
        if Key.exceed_rationing key then begin
          let ws =
            { widened_state = None;
              previous_state = curr;
              widening_counter = widening_delay - 1; }
          in
          w.widening_partition <- Partition.replace key ws w.widening_partition
        end;
        Some curr
    in
    let flow = Flow.join_duplicate_keys flow in
    Flow.filter_map widen_one flow
end
