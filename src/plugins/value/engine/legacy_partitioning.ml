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

  type store = {
    size_limit : int;
    merge : bool;
    mutable eternal_states : Partition.t;
    mutable ultimate_state : state or_bottom;
    mutable size : int;
  }

  type propagation = {
    mutable states : state list;
  }

  type shadow = {
    mutable transfered_states : int;
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
      size = 0;
    }

  let empty_propagation () : propagation =
    { states = [] }

  let empty_shadow () : shadow =
    { transfered_states = 0 }

  let empty_widening ~(stmt : stmt option) : widening =
    {
      stmt = Extlib.opt_conv Cil.invalidStmt stmt;
      widened_state = `Bottom;
      previous_state = `Bottom;
      widening_counter = widening_delay;
    }

  let initial_propagation (states : state list) : propagation =
    { states }

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
    List.iter (pretty_eternal fmt) p.states

  (* Accessors *)

  let expanded (s : store) : state list =
    Bottom.add_to_list s.ultimate_state (Partition.to_list s.eternal_states)

  let smashed (s : store) : state or_bottom =
    let l = expanded s in
    Domain.join_list l

  let is_empty_store (s : store) : bool =
    s.size = 0 && Bottom.is_bottom s.ultimate_state

  let is_empty_propagation (p : propagation) : bool =
    p.states = []

  let is_empty_shadow (s : shadow) : bool =
    s.transfered_states = 0

  let store_size (s : store) : int =
    s.size

  let propagation_size (p : propagation) : int =
    List.length p.states

  (* Reset state (for hierchical convergence) *)

  let reset_store (s : store) : unit =
    s.ultimate_state <- `Bottom

  let reset_propagation (p : propagation) : unit =
    p.states <- []

  let reset_shadow (_s : shadow) : unit = ()

  let reset_widening (w : widening) : unit =
    w.widened_state <- `Bottom;
    w.previous_state <- `Bottom;
    w.widening_counter <- widening_delay

  (* Operators *)

  let clear_propagation (p : propagation) : unit =
    p.states <- []

  let transfer (f : state list -> state list) (p : propagation) : unit =
    if p.states <> [] then
      p.states <- f p.states

  let merge ~(into : propagation) (source : propagation) : unit =
    into.states <- source.states @ into.states

  let join (sources : (propagation*shadow) list) (dest : store) : propagation =
    (* Update source stores with source propagation *)
    let update acc (p,s) =
      let size = List.length p.states in
      s.transfered_states <- s.transfered_states + size;
      dest.size <- dest.size + size;
      Partition.merge_set_return_new p.states dest.eternal_states @ acc
    in
    let new_states = List.fold_left update [] sources in
    (* Create a new propagation *)
    let p = { states = new_states } in
    (* Merge / Merge after loop : join eternal states being propagated *)
    if dest.merge then
      p.states <- Bottom.to_list (Domain.join_list p.states);
    (* Do we have too many eternal states ? *)
    if dest.size > dest.size_limit then
      begin
        let state' = Domain.join_list ~into:dest.ultimate_state p.states in
        if Bottom.is_included Domain.is_included state' dest.ultimate_state then
          p.states <- []
        else begin
          dest.ultimate_state <- state';
          p.states <- Bottom.to_list state'
        end
      end;
    p

  let widen (s : store) (w : widening) (p : propagation) : bool =
    let current_state = s.ultimate_state
    and previous_state = w.previous_state in
    if not (Bottom.is_bottom current_state) then begin
      w.previous_state <- current_state;
      w.widening_counter <- w.widening_counter - 1;
      match previous_state, current_state with
      | _, `Bottom | `Bottom, _ -> ()
      | `Value prev, `Value curr ->
        if Domain.is_included curr prev then
          p.states <- []
        else if w.widening_counter < 0 then begin
          Value_parameters.feedback ~level:1 ~once:true ~current:true
            ~dkey:Value_parameters.dkey_widening
            "applying a widening at this point";
          let prev = match w.widened_state with
            | `Value v -> Domain.join prev v
            | `Bottom -> prev
          in
          let next = Domain.widen kf w.stmt prev (Domain.join prev curr) in
          p.states <- [next];
          w.previous_state <- `Value next;
          w.widened_state <- `Value next;
          w.widening_counter <- widening_period - 1
        end
    end;
    p.states = []

  let enter_loop (_p : propagation) (_ : loop) = ()
  let leave_loop (_p : propagation) (_ : loop) = ()
  let next_loop_iteration (_p : propagation) (_ : loop) = ()
end
