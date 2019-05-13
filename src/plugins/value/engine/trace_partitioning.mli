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

open Bottom.Type

module Make
    (Abstract : Abstractions.Eva)
    (Transfer : Transfer_stmt.S with type state = Abstract.Dom.t)
    (Kf : sig val kf: Cil_types.kernel_function end) :
sig
  type state = Abstract.Dom.t     (** The states being partitioned *)
  type store       (** The storage of all states ever met at a control point *)
  type tank        (** The set of states that remains to propagate from a
                       control point. *)
  type flow        (** A set of states which are currently propagated *)
  type widening    (** Widening information *)

  (* --- Constructors --- *)

  val empty_store : stmt:Cil_types.stmt option -> store
  val empty_flow : flow
  val empty_tank : unit -> tank
  val empty_widening : stmt:Cil_types.stmt option -> widening

  (** Build the initial tank for the entry point of a function. *)
  val initial_tank : state list -> tank

  (* --- Pretty printing --- *)

  val pretty_store : Format.formatter -> store -> unit
  val pretty_flow : Format.formatter -> flow -> unit

  (* --- Accessors --- *)

  val expanded : store -> state list
  val smashed : store -> state or_bottom
  val contents : flow -> state list
  val is_empty_store : store -> bool
  val is_empty_flow : flow -> bool
  val is_empty_tank : tank -> bool
  val store_size : store -> int
  val flow_size : flow -> int
  val tank_size : tank -> int

  (* --- Reset state (for hierchical convergence) --- *)

  (* These functions reset the part of the state of the analysis which has
     been obtained after a widening. *)
  val reset_store : store -> unit
  val reset_tank : tank -> unit
  val reset_widening : widening -> unit

  (** Resets (or just delays) the widening counter. Used on nested loops, to
      postpone the widening of the inner loop when iterating on the outer
      loops. This is especially useful when the inner loop fixpoint does not
      depend on the outer loop. *)
  val reset_widening_counter : widening -> unit

  (* --- Partition transfer functions --- *)

  val enter_loop : flow -> Cil_types.stmt -> flow
  val leave_loop : flow -> Cil_types.stmt -> flow
  val next_loop_iteration : flow -> Cil_types.stmt -> flow
  val split_return : flow -> Cil_types.exp option -> flow

  (* --- Operators --- *)

  (** Remove all states from the tank, leaving it empty as if it was just
      created by [empty_tank] *)
  val drain : tank -> flow

  (** Fill the states of the flow into the tank, modifying [into] inplace. *)
  val fill : into:tank -> flow -> unit

  (** Apply a transfer function to all the states of a propagation. *)
  val transfer : (state -> state list) -> flow -> flow

  (** Join all incoming propagations into the given store. This function returns
      a set of states which still need to be propagated past the store.

      If a state from the propagations is included in another state which has
      already been propagated, it may be removed from the output propagation.
      Likewise, if a state from a propagation is included in a state from
      another propagation of the list (coming from another edge or iteration),
      it may also be removed.

      This function also interprets partitioning annotations at the store
      vertex (slevel, splits, merges, ...) which will generally change the
      current partitioning. *)
  val join : (Partition.branch * flow) list -> store -> flow

  (** Widen a flow. The widening object keeps track of the previous widenings
      and previous propagated states to ensure termination. *)
  val widen : widening -> flow -> flow

end
