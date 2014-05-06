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

(** A state is a project-compliant mutable value.
    @since Carbon-20101201
    @plugin development guide *)

open Project_skeleton

(* ************************************************************************** *)
(** {2 Type declarations} *)
(* ************************************************************************** *)

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
        below). *)

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

val dummy: t
(** A dummy state.
    @since Carbon-20101201
    @plugin development guide *)

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
    { mutable descr: Structural_descr.pack;
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
      unserialize: project -> state_on_disk -> unit
      (** @raise Incompatible_datatype if [state_on_disk] is not 
                 compatible with the datatype expected by Frama-C's state *) 
    }

exception Incompatible_datatype of string

val dummy_state_on_disk: state_on_disk

val private_ops: t -> private_ops
(** @since Carbon-20101201 *)

(* ************************************************************************** *)
(** {3 State generators} *)
(* ************************************************************************** *)

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
  t
(** @since Carbon-20101201
    @modify Nitrogen-20111001 add the [on_update] argument *)

val delete: t -> unit
(** @since Carbon-20101201 *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
