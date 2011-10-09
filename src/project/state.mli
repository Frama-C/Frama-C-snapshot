(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** A state is a project-compliant mutable value.
    @since Carbon-20101201 *)

open Project_skeleton

(* ************************************************************************** *)
(** {2 Type declarations} *)
(* ************************************************************************** *)

type standard_kind =
  [
  | `Correctness (** The state has an impact on the correctness of a result. *)
  | `Internal    (** The state is for internal purpose only:
                     it is hidden to the external user. *)
  ]

(** Type of state kinds.
    @since Carbon-20101201 *)
type user_kind =
  [
  | standard_kind
  | `Tuning      (** The state has an impact on a result,
                     but it does not change its correctness.
                     For instance, it just improves the preciseness. *)
  | `Irrelevant  (** The state has no impact on any result.
                     If any analyser is run, then its result is not modified by
                     setting this state. *)
  ]

type kind =
  [
  | user_kind
  | `Proxy of standard_kind
  ]

include Datatype.S_with_collections

(** Operations on the local state required for registering a new state via
    {!State_builder.Register}.
    The local state is the mutable value which you would like to be
    project-compliant. *)
module type Local = sig

  type t
    (** Type of the state to register. *)

  val create: unit -> t
    (** How to create a new fresh state which must be equal to the initial
        state: that is, if you never change the state, [create ()] and [get
        ()] must be equal (see invariant 1 below). *)

  val clear: t -> unit
    (** How to clear a state. After clearing, the state should be
        observationaly the same that after its creation (see invariant 2
        below).
        @plugin development guide *)

  val get: unit -> t
    (** How to access to the current state. Be aware of invariants 3 and 4
        below. *)

  val set: t -> unit
    (** How to change the current state. Be aware of invariants 3 and 4
        below. *)

  (** The four following invariants must hold.
      {ol
      {- [create ()] returns a fresh value}
      {- forall [(p:t)] [copy p] returns a fresh value}
      {- forall [(p:t)], [create () = (clear p; set p; get ())]}
      {- forall [(p1:t),(p2:t)] such that [p1 != p2], [(set p1; get ()) != s2]}
      } *)

  val clear_some_projects: (Project_skeleton.t -> bool) -> t -> bool
  (** [clear_if_project f x] must clear any value [v] of type project of [x]
      such that [f v] is [true]. Of course, if the type [t] does not contain
      any object of type [project], this function should do nothing and
      safely returns [fun _ -> false].
      @return [true] iff at least one element of [x] has been cleared.
      @since Boron-20100401 *)

end

(* ************************************************************************** *)
(** {2 Getters and setters} *)
(* ************************************************************************** *)

val get_name: t -> string
(** Name of a state.
    @since Carbon-20101201 *)

val set_name: t -> string -> unit
(** Set the name of the given state.
    @since Carbon-20101201 *)

val get_unique_name: t -> string
(** Unique name of a state.
    @since Carbon-20101201 *)

val unique_name_from_name: string -> string
(** @return a fresh unique state name from the given name.
    @since Nitrogen-20111001 *)

val kind: t -> kind
(** Kind of a state.
    @since Carbon-20101201 *)

val dummy: t
(** A dummy state.
    @since Carbon-20101201 *)

val dummy_unique_name: string

val is_dummy: t -> bool
(** @return true if the given state is {!dummy}.
    @since Carbon-20101201 *)

exception Unknown
val get: string -> t
(** @return the state corresponding to the given unique name.
    @raise Unknown if there is no such state.
    @since Carbon-20101201 *)

val get_descr: t -> Structural_descr.pack
(** @since Carbon-20101201 *)

val add_hook_on_update: t -> (unit -> unit) -> unit
(** Add an hook which is applied each time the project library changes the local
    value of the state.
    @since Nitrogen-20111001 *)

(* ************************************************************************** *)
(** {2 Clusters} *)
(* ************************************************************************** *)

(** Cluster of states for grouping some states together.
    @since Carbon-20101201 *)
module Cluster: sig

  val create: string -> t list -> unit
  (** Group togethers a list of states. Such a group is a so-called `cluster'.
      The given string is the cluster name. It must be distinct of each other
      cluster name. When one state of the cluster is updated, all the others are
      also automatically updated.
      @since Carbon-20101201 *)

  val extend: string -> t list -> unit
  (** Extend a cluster with some additional states.
      @since Carbon-20101201 *)

  val states: t -> t list
  (** @return all the states (included [s]) in the same
      cluster of [s], if any. Otherwise, returns the empty list.
      @since Carbon-20101201 *)

  val name: t -> string option
    (** [cluster_name s] returns the name of cluster of [s], if any.
        @since Carbon-20101201 *)

  (** {2 Internal Stuff} *)

  val unmarshal: string option -> t -> unit
    (** How to unmarshal a cluster stored in a state, previously marshaled with
        its name.
        @since Carbon-20101201 *)

  val after_load: unit -> unit
    (** Must be called after each project loading.
        Exported for breaking mutual dependencies with [Project].
        @since Carbon-20101201 *)

end

(* ************************************************************************** *)
(** {2 Internals}

    All this stuff should not be used outside of the Project library.*)
(* ************************************************************************** *)

(** @since Carbon-20101201 *)
type state_on_disk =
    { on_disk_value: Obj.t;
      on_disk_computed: bool;
      on_disk_saved: bool;
      on_disk_digest: Digest.t }

(** @since Carbon-20101201 *)
type private_ops = private
    { descr: Structural_descr.pack;
      create: project -> unit;
      remove: project -> unit;
      mutable clear: project -> unit;
      mutable clear_some_projects: (project -> bool) -> project -> bool;
      copy: project -> project -> unit;
      commit: project -> unit;
      update: project -> unit;
      on_update: (unit -> unit) -> unit;
      clean: unit -> unit;
      serialize: project -> state_on_disk;
      unserialize: project -> state_on_disk -> unit }

val dummy_state_on_disk: state_on_disk

val private_ops: t -> private_ops
(** @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {3 Managing the set of known states} *)
(* ************************************************************************** *)

module States: Local
(** @since Carbon-20101201 *)

module States_datatype: Datatype.S with type t = States.t

val delete: t -> unit
(** @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {3 State generators} *)
(* ************************************************************************** *)

val unusable: name:string -> string -> t
(** @return a fresh named version of {!dummy}.
    @since Carbon-20101201 *)

val is_usable: t -> bool
(** @since Carbon-20101201 *)

val update_unusable: t -> kind -> (project -> unit) -> unit
(** @since Carbon-20101201 *)

val create:
  descr:Structural_descr.pack ->
  create:(project -> unit) ->
  remove:(project -> unit) ->
  clear:(project -> unit) ->
  clear_some_projects:((project -> bool) -> project -> bool) ->
  copy:(project -> project -> unit) ->
  commit:(project -> unit) ->
  update:(project -> unit) ->
  on_update:((unit -> unit) -> unit) ->
  clean:(unit -> unit) ->
  serialize:(project -> state_on_disk) ->
  unserialize:(project -> state_on_disk -> unit) ->
  unique_name:string ->
  name:string ->
  kind ->
  t
(** @since Carbon-20101201
    @modify Nitrogen-20111001 add the [on_update] argument *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
