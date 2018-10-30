(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
open State_partitioning


module Make (Domain : Domain) (Param : Param) =
struct
  include Param
  module Partition = Partitioning.Make (Domain)

  type state = Domain.t

  (** Stores contains what have been already computed at a fixed control
      point.
      Propagation contrains what have changed since last iteration and needs
      to be propagated.
      Widening records previous states used in widening operations and keep
      tracks of the number of iterations remaining before next widening.

      How states are stored:
      - Eternal states are states which, once propagated, we are sure they
        will stay until the end of the analysis. This means they have not
        been widened, and thus cannot descend or disapear.
      - When states have been widened and/or joined due to the lack of
        slevel, they are put in the ultimate state.
      - Eternal states that can't be propagated due to the lack of slevel
        are doomed to wander into the limbo state. It is a join of all those
        states.
      With these rules, the ultimate state at one point is the join of the
      limbo state and the ultimate states of the predecessors.
      The field [size] store a value compatible with the legacy way of
      counting the number of eternal states in the store. *)
  type store = {
    size_limit : int;
    merge : bool;
    mutable eternal_states : Partition.t;
    mutable ultimate_state : state or_bottom;
    mutable limbo_state : state or_bottom;
    mutable size : int;
  }

  type propagation = {
    mutable eternal_propagation : state list;
    mutable ultimate_propagation : state or_bottom option;
    mutable widening_depth : int;
    mutable current_depth : int;
  }

  type shadow = {
    mutable transfered_eternals : int;
    mutable shadowed_ultimate : state or_bottom;
  }

  type widening = {
    stmt : stmt;
    mutable widened_state : state or_bottom;
    mutable previous_state : state or_bottom;
    mutable widening_counter : int;
  }

  (* Constructors *)

  let empty_store ~(stmt : stmt option) : store =
    let size_limit, merge = match stmt with
      | None -> max_int, false
      | Some stmt -> slevel stmt, merge stmt
    in
    {
      size_limit; merge;
      eternal_states = Partition.empty ();
      ultimate_state = `Bottom;
      limbo_state = `Bottom;
      size = 0;
    }

  let empty_propagation () : propagation =
    {
      eternal_propagation = [];
      ultimate_propagation = None;
      widening_depth = max_int;
      current_depth = 0;
    }

  let empty_shadow () : shadow =
    {
      transfered_eternals = 0;
      shadowed_ultimate = `Bottom;
    }

  let empty_widening ~(stmt : stmt option) : widening =
    {
      stmt = Extlib.opt_conv Cil.invalidStmt stmt;
      widened_state = `Bottom;
      previous_state = `Bottom;
      widening_counter = widening_delay;
    }

  let initial_propagation (states : state list) =
    let empty = empty_propagation () in
    { empty with eternal_propagation = states }

  (* Pretty printing *)

  let pretty_eternal (fmt : Format.formatter) (v : Domain.t) : unit =
    Format.fprintf fmt "eternal state %a@\n" Domain.pretty v

  let pretty_ultimate (fmt : Format.formatter) (v : Domain.t) : unit =
    Format.fprintf fmt "ultimate state %a@\n" Domain.pretty v

  let pretty_store (fmt : Format.formatter) (s : store) : unit =
    List.iter (pretty_eternal fmt) (Partition.to_list s.eternal_states);
    if not (Bottom.is_bottom s.ultimate_state) then
      pretty_ultimate fmt (Bottom.non_bottom s.ultimate_state)

  let pretty_propagation (fmt : Format.formatter) (p : propagation) =
    List.iter (pretty_eternal fmt) p.eternal_propagation;
    Extlib.may (Bottom.pretty pretty_ultimate fmt) p.ultimate_propagation

  (* Accessors *)

  let expanded (s : store) : state list =
    Bottom.add_to_list s.ultimate_state (Partition.to_list s.eternal_states)

  let smashed (s : store) : state or_bottom =
    let l = expanded s in
    Domain.join_list l

  let is_empty_store (s : store) : bool =
    s.size = 0 && Bottom.is_bottom s.ultimate_state

  let is_empty_propagation (p : propagation) : bool =
    p.eternal_propagation = [] && not (Extlib.has_some p.ultimate_propagation)

  let is_empty_shadow (s : shadow) : bool =
    s.transfered_eternals = 0 && Bottom.is_bottom s.shadowed_ultimate

  let store_size (s : store) : int =
    s.size

  let propagation_size (p : propagation) : int =
    List.length p.eternal_propagation +
    match p.ultimate_propagation with
    | None | Some `Bottom -> 0
    | Some (`Value _) -> 1

  (* Reset state (for hierchical convergence) *)

  let reset_store (s : store) : unit =
    s.ultimate_state <- `Bottom

  let reset_propagation (p : propagation) : unit =
    p.ultimate_propagation <- None;
    p.widening_depth <- max_int

  let reset_shadow (s : shadow) : unit =
    s.shadowed_ultimate <- `Bottom

  let reset_widening (w : widening) : unit =
    w.widened_state <- `Bottom;
    w.previous_state <- `Bottom;
    w.widening_counter <- widening_delay

  (* Operators *)

  let clear_propagation (p : propagation) : unit =
    p.eternal_propagation <- [];
    p.ultimate_propagation <- None;
    p.widening_depth <- max_int;
    p.current_depth <- 0

  let transfer (f : state list -> state list) (p : propagation) : unit =
    let transfer_ultimate state' =
      state' >>- fun x -> Domain.join_list (f [x])
    in
    if p.eternal_propagation <> [] then
      p.eternal_propagation <- f p.eternal_propagation;
    p.ultimate_propagation <-
      Extlib.opt_map transfer_ultimate p.ultimate_propagation

  let merge ~(into : propagation) (source : propagation) : unit =
    if Extlib.has_some source.ultimate_propagation then
      into.ultimate_propagation <- source.ultimate_propagation;
    into.eternal_propagation <-
      source.eternal_propagation @ into.eternal_propagation;
    into.widening_depth <- source.widening_depth;
    into.current_depth <- source.current_depth

  let join (sources : (propagation*shadow) list) (dest : store): propagation =
    let ultimates_changed = ref false in
    (* Update source shadow with source propagation *)
    let update (eternals,ultimates) (p,s) =
      begin match p.ultimate_propagation with
        | None -> ()
        | Some state' ->
          if not (Bottom.equal Domain.equal state' s.shadowed_ultimate) then
            ultimates_changed := true;
          s.shadowed_ultimate <- state'
      end;
      s.transfered_eternals <- s.transfered_eternals +
                               List.length p.eternal_propagation;
      p.eternal_propagation @ eternals,
      Bottom.add_to_list s.shadowed_ultimate ultimates
    in
    let eternals, ultimates = List.fold_left update ([],[]) sources in
    (* Create a new propagation *)
    let p = empty_propagation () in
    let current_depth acc (p,_s) = max p.current_depth acc
    and widening_depth acc (p,_s) = min p.widening_depth acc in
    p.current_depth <- List.fold_left current_depth 0 sources;
    p.widening_depth <- List.fold_left widening_depth max_int sources;
    (* Add all eternal states *)
    dest.size <- dest.size + List.length eternals;
    let states = Partition.merge_set_return_new eternals dest.eternal_states in
    (* Merge / Merge after loop : join eternal states being propagated *)
    let states =
      if dest.merge
      then Bottom.to_list (Domain.join_list states)
      else states
    in
    (* Do we have too many eternal states ? *)
    if dest.size > dest.size_limit then
      begin
        (* Send excess states into limbo *)
        dest.limbo_state <- Domain.join_list ~into:dest.limbo_state states;
        ultimates_changed := true
      end else
      p.eternal_propagation <- states;
    (* Join ultimate states *)
    if !ultimates_changed then
      begin
        let inputs = Bottom.add_to_list dest.limbo_state ultimates in
        let state' = Domain.join_list inputs in
        if not (Bottom.equal Domain.equal state' dest.ultimate_state) then
          p.ultimate_propagation <- Some state';
        dest.ultimate_state <- state'
      end;
    p

  let widen (_s : store) (w : widening) (p : propagation) : bool =
    let ultimate_stable =
      match p.ultimate_propagation with
      | None -> true
      | Some current_state ->
        let previous_state = w.previous_state in
        w.previous_state <- current_state;
        w.widening_counter <- w.widening_counter - 1;
        match previous_state, current_state with
        | _, `Bottom -> true
        | `Bottom, `Value _ -> false
        | `Value prev, `Value curr ->
          if Domain.is_included curr prev then
            true
          else if w.widening_counter >= 0 then
            false
          else begin
            Value_parameters.feedback ~level:1 ~once:true ~current:true
              ~dkey:Value_parameters.dkey_widening
              "applying a widening at this point";
            let prev = match w.widened_state with
              | `Value v -> Domain.join prev v
              | `Bottom -> prev
            in
            let next = Domain.widen kf w.stmt prev (Domain.join prev curr) in
            p.ultimate_propagation <- Some (`Value next);
            p.widening_depth <- min p.widening_depth p.current_depth;
            w.previous_state <- `Value next;
            w.widened_state <- `Value next;
            w.widening_counter <- widening_period - 1;
            false
          end
    in
    ultimate_stable && p.eternal_propagation = []

  let enter_loop (p : propagation) (_ : loop) =
    p.current_depth <- p.current_depth + 1

  let leave_loop (p : propagation) (_ : loop) =
    p.current_depth <- p.current_depth - 1;
    match p.ultimate_propagation with
    (* We leave the loop where the ultimate have been widened.
       It becomes eternal again. *)
    | Some state' when p.current_depth < p.widening_depth ->
      p.ultimate_propagation <- None;
      p.eternal_propagation <- Bottom.add_to_list state' p.eternal_propagation
    | _ -> ()

  let next_loop_iteration (_p : propagation) (_ : loop) = ()
end
