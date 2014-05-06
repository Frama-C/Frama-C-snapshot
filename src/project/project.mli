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

(** Projects management.

    A project groups together all the internal states of Frama-C. An internal
    state is roughly the result of a computation which depends of an AST. It is
    possible to have many projects at the same time. For registering a new
    state in the Frama-C projects, apply the functor {!State_builder.Register}.

    @plugin development guide *)

(* ************************************************************************* *)
(** {2 Types for project} *)
(* ************************************************************************* *)

include Datatype.S_no_copy with type t = Project_skeleton.t
module Datatype: Datatype.S with type t = Project_skeleton.t

(* re-exporting record fields *)
type project = Project_skeleton.t =
    private
      { pid : int;
        mutable name : string;
        mutable unique_name : string }
      (** Type of a project. *)

(* ************************************************************************* *)
(** {2 Operations on all projects} *)
(* ************************************************************************* *)

val create: string -> t
  (** Create a new project with the given name and attach it after the existing
      projects (so the current project, if existing, is unchanged).
      The given name may be already used by another project.
      If there is no other project, then the new one is the current one. *)

val register_create_hook: (t -> unit) -> unit
  (** [register_create_hook f] adds a hook on function [create]: each time a
      new project [p] is created, [f p] is applied.

      The order in which hooks are applied is the same than the order in which
      hooks are registered. *)

exception NoProject
  (** May be raised by [current]. *)

val current: unit -> t
  (** The current project.
      @raise NoProject if there is no project.
      @plugin development guide *)

val is_current: t -> bool
  (** Check whether the given project is the current one or not. *)

val iter_on_projects: (t -> unit) -> unit
  (** iteration on project starting with the current one. *)

val fold_on_projects: ('a -> t -> 'a) -> 'a -> 'a
  (** folding on project starting with the current one.
      @since Boron-20100401 *)

val find_all: string -> t list
  (** Find all projects with the given name. *)

val clear_all: unit -> unit
  (** Clear all the projects: all the internal states of all the projects are
      now empty (wrt the action registered with
      {!register_todo_after_global_clear} and {!register_todo_after_clear}. *)

(* ************************************************************************* *)
(** {2 Operations on one project}

    Most operations have one additional selection as argument. If it
    is specified, the operation is only applied on the states of the
    given selection on the given project. Beware that the project may
    become inconsistent if your selection is incorrect. *)
(* ************************************************************************* *)

val get_name: t -> string
  (** Project name. Two projects may have the same name. *)

val get_unique_name: t -> string
  (** @return a project name based on {!name} but different of each others
      [unique_name]. *)

val set_name: t -> string -> unit
  (** Set the name of the given project.
      @since Boron-20100401 *)

val from_unique_name: string -> t
  (** Return a project based on {!unique_name}.
      @raise Not_found if no project has this unique name. *)

val set_current: ?on:bool -> ?selection:State_selection.t -> t -> unit
  (** Set the current project with the given one.
      The flag [on] is not for casual users.
      @raise Invalid_argument if the given project does not exist anymore.
      @plugin development guide *)

val register_after_set_current_hook: user_only:bool -> (t -> unit) -> unit
  (** [register_after_set_current_hook f] adds a hook on function
      {!set_current}. The project given as argument to [f] is the old current
      project.
      - If [user_only] is [true], then each time {!set_current} is directly
      called by an user of this library, [f ()] is applied.
      - If [user_only] is [false], then each time {!set_current} is applied
      (even indirectly through {!Project.on}), [f ()] is applied.
      The order in which each hook is applied is unspecified. *)

val on: ?selection:State_selection.t -> t -> ('a -> 'b) -> 'a -> 'b
  (** [on p f x] sets the current project to [p], computes [f x] then
      restores the current project. You should use this function if you use a
      project different of [current ()].
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection].
      @plugin development guide *)

val copy: ?selection:State_selection.t -> ?src:t -> t -> unit
  (** Copy a project into another one. Default project for [src] is [current
      ()]. Replace the destination by [src].
      For each state to copy, the function [copy] given at state registration
      time must be fully implemented.
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection]. *)

val create_by_copy: ?selection:State_selection.t -> ?src:t -> string -> t
  (** Return a new project with the given name by copying some states from the
      project [src]. All the other states are initialized with their default
      values.
      Use the save/load mechanism for copying. Thus it does not require that
      the copy function of the copied state is implemented. All the hooks
      applied when loading a project are applied (see {!load}).
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection]. *)

val create_by_copy_hook: (t -> t -> unit) -> unit
  (** Register a hook to call at the end of {!create_by_copy}. The first
      argument of the registered function is the copy source while the
      second one is the created project. *)

val clear: ?selection:State_selection.t -> ?project:t -> unit -> unit
  (** Clear the given project. Default project is [current ()]. All the
      internal states of the given project are now empty (wrt the action
      registered with {!register_todo_on_clear}).
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection]. 
      @plugin development guide *)

val register_todo_before_clear: (t -> unit) -> unit
  (** Register an action performed just before clearing a project.
      @since Boron-20100401 *)

val register_todo_after_clear: (t -> unit) -> unit
  (** Register an action performed just after clearing a project.
      @since Boron-20100401 *)

exception Cannot_remove of string
  (** Raised by [remove] *)

val remove: ?project:t -> unit -> unit
  (** Default project is [current ()]. If the current project is removed, then
      the new current project is the previous current project if it still
      exists (and so on).
      @raise Cannot_remove if there is only one project. *)

val register_before_remove_hook: (t -> unit) -> unit
  (** [register_before_remove_hook f] adds a hook called just before removing
      a project.
      @since Beryllium-20090902 *)

(* ************************************************************************* *)
(** {3 Inputs/Outputs} *)
(* ************************************************************************* *)

exception IOError of string

val save: ?selection:State_selection.t -> ?project:t -> string -> unit
  (** Save a given project in a file. Default project is [current ()].
      @raise IOError if the project cannot be saved.
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection].
      @plugin development guide *)

val load: ?selection:State_selection.t -> ?name:string -> string -> t
  (** Load a file into a new project given by its name.
      More precisely, [load only except name file]:
      {ol
      {- creates a new project;}
      {- performs all the registered [before_load] actions;}
      {- loads the (specified) states of the project according to its
      description; and}
      {- performs all the registered [after_load] actions.}
      }
      @raise IOError if the project cannot be loaded
      @return the new project containing the loaded data.
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection].
      @plugin development guide *)

val save_all: ?selection:State_selection.t -> string -> unit
  (** Save all the projects in a file.
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection].
      @raise IOError a project cannot be saved. *)

val load_all: ?selection:State_selection.t -> string -> unit
  (** First remove all the existing project, then load all the projects from a
      file. For each project to load, the specification is the same than
      {!Project.load}. Furthermore, after loading, all the hooks registered by
      [register_after_set_current_hook] are applied.
      @modify Carbon-20101201 replace the optional arguments [only] and
      [except] by a single one [selection].
      @raise IOError if a project cannot be loaded. *)

val register_before_load_hook: (unit -> unit) -> unit
  (** [register_before_load_hook f] adds a hook called just before loading
      **each project** (more precisely, the project exists and but is empty
      while the hook is applied): if [n] projects are on disk, the same hook
      will be called [n] times (one call by project).

      Besides, for each project, the order in which the hooks are applied is
      the same than the order in which hooks are registered. *)

val register_after_load_hook: (unit -> unit) -> unit
  (** [register_after_load_hook f] adds a hook called just after loading
      **each project**: if [n] projects are on disk, the same hook will be
      called [n] times (one call by project).

      Besides, for each project, the order in which the hooks are applied is
      the same than the order in which hooks are registered. *)

val register_after_global_load_hook: (unit -> unit) -> unit
  (** [register_after_load_hook f] adds a hook called just after loading
      **all projects**. [f] must not set the current project.
      @since Boron-20100401 *)

(* ************************************************************************* *)
(** {3 Handling the selection} *)
(* ************************************************************************* *)

val get_current_selection: unit -> State_selection.t
(** If an operation on a project is ongoing, then [get_current_selection ()]
    returns the selection which is applied on.
    The behaviour is unspecified if this function is called when no operation
    depending on a selection is ongoing. *)

(* ************************************************************************* *)
(** {2 Projects are comparable values} *)
(* ************************************************************************* *)

val compare: t -> t -> int
val equal: t -> t -> bool
val hash: t -> int

(* ************************************************************************* *)
(** {2 Undoing} *)
(* ************************************************************************* *)

module Undo: sig
  val breakpoint: unit -> unit
  val restore: unit -> unit
  val clear_breakpoint: unit -> unit
end

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
