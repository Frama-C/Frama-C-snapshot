(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** A state selection is a set of states with operations for easy handling of
    state dependencies.
    @since Carbon-20101201
    @plugin development guide *)

(* ************************************************************************** *)
(** {2 Type declarations} *)
(* ************************************************************************** *)

type t
(** Type of a state selection.
    @since Carbon-20101201 *)

val ty: t Type.t
(** Type value representing {!t}.
    @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Generic Builders} *)
(* ************************************************************************** *)

val empty: t
(** The empty selection.
    @since Carbon-20101201 *)

val full: t
(** The selection containing all the states.
    @since Carbon-20101201 *)

val singleton: State.t -> t
(** The selection containing only the given state.
    @since Carbon-20101201 *)

val of_list: State.t list -> t
(** The selection containing only the given list of states.
    @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {2 Generic Getters} *)
(* ************************************************************************** *)

val is_empty: t -> bool
(** @return [true] iff the selection is empty.
    @since Carbon-20101201 *)

val is_full: t -> bool
(** @return [true] iff the selection contains all the states.
    @since Carbon-20101201 *)

val mem: t -> State.t -> bool

(* ************************************************************************** *)
(** {2 Specific selections} *)
(* ************************************************************************** *)

(** Operations over selections which depend on a State Dependency Graph
    implementation.
    @since Carbon-20101201 *)
module type S = sig

  (* ************************************************************************ *)
  (** {2 Builders from dependencies} *)
  (* ************************************************************************ *)

  val with_dependencies: State.t -> t
  (** The selection containing the given state and all its dependencies.
      @since Carbon-20101201 
      @plugin development guide *)

  val only_dependencies: State.t -> t
  (** The selection containing all the dependencies of the given state (but not
      this state itself).
      @since Carbon-20101201 *)

  val with_codependencies: State.t -> t
  (** The selection containing the given state and all its co-dependencies.
      @since Carbon-20101201 *)

  val only_codependencies: State.t -> t
  (** The selection containing all the co-dependencies of the given state (but
      not this state itself).
      @since Carbon-20101201 *)

  (* ************************************************************************ *)
  (** {2 Builders by operations over sets} *)
  (* ************************************************************************ *)

  val union: t -> t -> t
  (** Union of two selections.
      @since Carbon-20101201 *)

  val list_union: t list -> t
    (** Union of an arbitrary number of selection (0 gives an empty selection)
        @since Oxygen-20120901
     *)

  val list_state_union: ?deps:(State.t -> t) -> State.t list -> t
    (** Union of an arbitrary number of states (0 gives an empty selection).
        Optional [deps] arguments indicates how to handle dependencies. Defaults
        to {! State_selection.singleton}
        @since Oxygen-20120901 *)

  val diff: t -> t -> t
  (** Difference between two selections.
      @since Carbon-20101201 *)

  (* ************************************************************************ *)
  (** {2 Specific Getters} *)
  (* ************************************************************************ *)

  val cardinal: t -> int
  (** Size of a selection.
      @since Carbon-20101201 *)

  val to_list: t -> State.t list
  (** Convert a selection into a list of states.
      @since Fluorine-20130401 *)

  val pretty: Format.formatter -> t -> unit
  (** Display a selection iff kernel debug mode is on.
      @since Carbon-20101201 *)

  (** {3 Iterators} *)

  val iter_succ: (State.t -> unit) -> t -> State.t -> unit
  (** Iterate over the successor of a state in a selection.
      The order is unspecified.
      @since Carbon-20101201 *)

  val fold_succ: (State.t -> 'a -> 'a) -> t -> State.t -> 'a -> 'a
  (** Iterate over the successor of a state in a selection.
      The order is unspecified.
      @since Carbon-20101201 *)

  val iter: (State.t -> unit) -> t -> unit
  (** Iterate over a selection. The order is unspecified.
      @since Carbon-20101201 *)

  val fold: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold over a selection. The order is unspecified.
      @since Carbon-20101201 *)

  val iter_in_order: (State.t -> unit) -> t -> unit
  (** Iterate over a selection in a topological ordering compliant with the
      State Dependency Graph. Less efficient that {!iter}.
      @since Carbon-20101201 *)

  val fold_in_order: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold over a selection in a topological ordering compliant with the
      State Dependency Graph. Less efficient that {!iter}.
      @since Carbon-20101201 *)

end

(** Operations over selections which depend on
    {!State_dependency_graph.graph}.
    @since Carbon-20101201
    @deprecated Oxygen-20120901 directly use equivalent top-level function
    instead. *)
module Static: S
include S

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
