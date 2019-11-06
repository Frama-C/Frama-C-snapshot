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

(** A partition is a collection of states, each identified by a unique key.
    The keys define the states partition: states with identical keys are joined
    together, while states with different keys are maintained separate.
    A key contains the reason for which a state must be kept separate from
    others, or joined with similar states.

    Partitioning actions allow updating the keys or spliting some states to
    define or change the partition. Actions are applied to flows, in which
    states with the same key are *not* automatically joined. This allows
    applying mutliple actions before recomputing the partitions. Flows can then
    be converted into partitions, thus merging states with identical keys.

    Flows are used to transfer states from one partition to another. Transfer
    functions can be applied to flows; keys are maintained through transfer
    functions, until partitioning actions update them.  *)

(** {2 Keys and partitions.} *)

(** Partitioning keys attached to states. *)
type key

module Key : sig
  val zero : key (** Initial key: no partitioning. *)
  val compare : key -> key -> int
  val pretty : Format.formatter -> key -> unit
  val exceed_rationing: key -> bool
end

(** Collection of states, each identified by a unique key. *)
type 'state partition

val empty : 'a partition
val is_empty : 'a partition -> bool
val size : 'a partition -> int
val to_list : 'a partition -> 'a list
val find : key -> 'a partition -> 'a
val replace : key -> 'a -> 'a partition -> 'a partition
val merge : (key -> 'a option -> 'b option -> 'c option) -> 'a partition ->
  'b partition -> 'c partition
val iter : (key -> 'a -> unit) -> 'a partition -> unit
val filter : (key -> 'a -> bool) -> 'a partition -> 'a partition
val map : ('a  -> 'a) -> 'a partition -> 'a partition


(** {2 Partitioning actions.} *)

type branch = int (** Junction branch id in the control flow *)

(** Rationing are used to keep separate the [n] first states propagated at
    a point, by creating unique stamp until the limit is reached.
    Implementation of the option -eva-slevel. *)
type rationing

(** Creates a new rationing, that can be used successively on several flows. *)
val new_rationing: limit:int -> merge:bool -> rationing

(** The unroll limit of a loop. *)
type unroll_limit =
  | ExpLimit of Cil_types.exp
  (** Value of the expression for each incoming state. The expression must
      evaluate to a singleton integer in each state.  *)
  | IntLimit of int
  (** Integer limit. *)
  | AutoUnroll of Cil_types.stmt * int * int
  (** [AutoUnroll(stmt, min, max)] requests to find a "good" unrolling limit
      between [min] and [max] for the loop [stmt]. *)

(** Splits on an expression can be static or dynamic:
    - static splits are processed once: the expression is only evaluated at the
      split point, and the key is then kept unchanged until a merge.
    - dynamic splits are regularly redone: the expression is re-evaluated, and
      states are then split or merged accordingly. *)
type split_kind = Static | Dynamic

(** Split monitor: prevents splits from generating too many states. *)
type split_monitor

(** Creates a new monitor that allows to split up to [split_limit] states. *)
val new_monitor: split_limit:int -> split_monitor

(** These actions redefine the partitioning by updating keys or spliting states.
    They are applied to all the pair (key, state) in a flow. *)
type action =
  | Enter_loop of unroll_limit
  (** Enters a loop in which the n first iterations will be kept separate:
      creates an iteration counter at 0 for each states in the flow; states at
      different iterations will be kept separate, untill reaching the
      [unroll_limit]. Counters are incremented by the [Incr_loop] action. *)
  | Leave_loop
  (** Leaves the current loop: removes its iteration counter. States that were
      kept separate only by this iteration counter will be joined together. *)
  | Incr_loop
  (** Increments the iteration counter of the current loop for all states in
      the flow. States with different iteration counter are kept separate. *)
  | Branch of branch * int
  (** Identifies all the states in the flow as coming from [branch].
      They will be kept separated from states coming from other branches.
      The integer is the maximum number of successive branches kept in the keys:
      this action also removes the oldest branches from the keys to meet this
      constraint. *)
  | Ration of rationing
  (** Ensures that the first states encountered are kept separate, by creating a
      unique ration stamp for each new state until the [limit] is reached. The
      same rationing can be used on multiple flows. Applying a new rationing
      replaces the previous one.
      If the rationing has been created with [merge:true], all the states from
      each flow receive the same stamp, but states from different flows receive
      different stamps, until [limit] states have been tagged. *)
  | Restrict of Cil_types.exp * Integer.t list
  (** [Restrict (exp, list)] restricts the rationing according to the evaluation
      of the expression [exp]:
      – for each integer [i] in [list], states in which [exp] evaluates exactly
        to the singleton [i] receive the same unique stamp, and will thus be
        joined together but kept separate from other states;
      – all other states are joined together.
      Previous rationing is erased and replaced by this new stamping.
      Implementation of the option -eva-split-return. *)
  | Split of Cil_types.exp * split_kind * split_monitor
  (** [Split (exp, kind, monitor)] tries to separate states such as the [exp]
      evaluates to a singleton value in each state in the flow. If necessary and
      possible, splits states into multiple states. States in which the [exp]
      evaluates to different values will be kept separate. Gives up the split
      if [exp] evaluates to more than [limit] values, [limit] being the split
      limit of the [monitor]. A same monitor can be used for successive splits
      on different flows. *)
  | Merge of Cil_types.exp * split_kind
  (** Forgets the split of an expression: states that were kept separate only
      by the split of this expression will be joined together. *)
  | Update_dynamic_splits
  (** Updates dynamic splits by evaluating the expression and spliting the
      states accordingly. *)

exception InvalidAction


(** {2 Flows.} *)

(** Flows are used to transfer states from one partition to another, by
    applying transfer functions and partitioning actions. They do not enforce
    the unicity of keys. *)
module MakeFlow (Abstract: Abstractions.Eva) :
sig
  type state = Abstract.Dom.t
  type t

  val empty : t

  val initial : state list -> t
  val to_list : t -> state list
  val of_partition : state partition -> t
  val to_partition : t -> state partition

  val is_empty : t -> bool
  val size : t -> int

  val union : t -> t -> t

  val transfer_keys : t -> action -> t
  val transfer_states : (state -> state list) -> t -> t

  val iter : (state -> unit) -> t -> unit
  val filter_map: (key -> state -> state option) -> t -> t

  val join_duplicate_keys: t -> t
end
