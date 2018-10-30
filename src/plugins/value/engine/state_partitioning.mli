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

open Bottom.Type

module type Param =
sig
  type loop
  val kf : Cil_types.kernel_function
  val widening_delay : int
  val widening_period : int
  val slevel : Cil_types.stmt -> int
  val merge : Cil_types.stmt -> bool
  val unroll : loop -> int
end

module type Domain =
sig
  include Partitioning.Domain
  val join_list : ?into:t or_bottom -> t list -> t or_bottom
end

module type Partition =
sig
  type loop        (** Loops identifiers *)
  type state       (** The states being partitioned *)
  type store       (** The storage of a partition *)
  type propagation (** Only contains states which needs to be propagated, i.e.
                       states which have not been propagated yet *)
  type shadow      (** The shadow of a propagation remembers all the previous
                       propagations ; shadows are useful before joins during
                       descending sequences or to find if a transition is
                       fireable *)
  type widening    (** Widening informations *)


  (* --- Constructors --- *)

  val empty_store : stmt:Cil_types.stmt option -> store
  val empty_propagation : unit -> propagation
  val empty_shadow : unit -> shadow
  val empty_widening : stmt:Cil_types.stmt option -> widening

  (** Build the initial propagation for the entry point of a function. *)
  val initial_propagation : state list -> propagation


  (* --- Pretty printing --- *)

  val pretty_store : Format.formatter -> store -> unit
  val pretty_propagation : Format.formatter -> propagation -> unit


  (* --- Accessors --- *)

  val expanded : store -> state list
  val smashed : store -> state or_bottom
  val is_empty_store : store -> bool
  val is_empty_propagation : propagation -> bool
  val is_empty_shadow : shadow -> bool
  val store_size : store -> int
  val propagation_size : propagation -> int


  (* --- Reset state (for hierchical convergence) --- *)

  (* These functions reset the part of the state of the analysis which has
     been obtained after a widening. *)

  val reset_store : store -> unit
  val reset_propagation : propagation -> unit
  val reset_shadow : shadow -> unit
  val reset_widening : widening -> unit


  (* --- Partition transfer functions --- *)

  val enter_loop : propagation -> loop -> unit
  val leave_loop : propagation -> loop -> unit
  val next_loop_iteration : propagation -> loop -> unit


  (* --- Operators --- *)

  (** Remove all states from the propagation, leaving it empty as if it was just
      created by [empty_propagation] *)
  val clear_propagation : propagation -> unit

  (** Apply a transfer function to all the states of a propagation. *)
  val transfer : (state list -> state list) -> propagation -> unit

  (** Merge two propagations together, modifying [into] inplace. At the return
      of the function, [into] should contain all the states of both original
      propagations, or an overapproximation of this union: joining two states
      together inside the propagation is allowed. *)
  val merge : into:propagation -> propagation -> unit

  (** Join all incoming propagations into the given store. Each propagation is
      paired with a shadow of the previous propagations on the same edge. This
      function returns a set of states which still need to be propagated past
      the store.

      If a state from the propagations is included in another state which has
      already been propagated, it may be removed from the output propagation.
      Likewise, if a state from a propagation is included in a state from
      another propagation of the list (coming from another edge or iteration),
      it may also be removed.

      This function also interprets partitioning annotations at the store
      vertex (slevel, splits, merges, ...) which will generally change the
      current partitioning. *)
  val join : (propagation * shadow) list -> store -> propagation

  (** Widen a propagation at the position of the given store. The widening
      object keeps track of the previous widenings to ensure termination. The
      result is true when it is correct to end the propagation here, i.e. when
      the current propagation is only carrying states which are included into
      already propagated states.

      Note that the propagation given to [widen] *must* have been produced by
      the [join] on the same store. *)
  val widen : store -> widening -> propagation -> bool

end

module type Partitioning =
  functor (Domain : Domain) (Param : Param) ->
    Partition with type state = Domain.t
               and type loop = Param.loop
