(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
    state in the Frama-C projects, apply the functor {!Computation.Register}.

    @plugin development guide *)

(* ************************************************************************* *)
(** {2 Types for project} *)
(* ************************************************************************* *)

type t
  (** Type of a project.
      @plugin development guide *)

type project = t
    (** Alias for the project type. *)

val ty: t Type.t
  (** Identifier for type [t]. *)

val dummy: t
  (** A dummy project: should only be used to initialized reference but must
      never be put something inside. *)

(* ************************************************************************* *)
(** {2 Useful functions} *)
(* ************************************************************************* *)

val identity: 'a -> 'a
  (** The identity function. *)

val is_identity: ('a -> 'a) -> bool
  (** @return [true] iff the given function is (physically) {!identity}. *)

val no_descr: Unmarshal.t

(* ************************************************************************* *)
(** {2 Kinds dealing with Project}

    There are two kinds of kinds in Frama-C: datatypes and computations. They
    are merely used for theirs dependencies:
    - The datatypes dependencies are useful when loading: mainly we have to
    re-hashcons all the values in a "good" order.
    - The computations dependencies are useful for actions which use
    selection (e.g. project copying): for example, we have to be able to
    copying some states with all their dependencies. *)
(* ************************************************************************* *)

(** Common signature of kinds.
    @see <kind.html> kind. *)
module type KIND = sig

  type t
    (** Type of kinds. *)

  val dummy : t
    (** A dummy kind.
	@plugin development guide *)

  val name: t -> string
    (** Name of a kind. *)

  val get_from_name: string -> t
    (** Reverse of [name] (as names are uniques for kinds, this function is the
	injection from kinds to names).
	@raise Not_found if there is no kind with this name.
	@since Boron-20100401 *)

  val add_dependency: t -> t -> unit
    (** [add_dependency k1 k2] indicates that the state kind [k1] depends on
	the state kind [k2], that is an action of the state kind [k2] must be
	done before one of the state kind [k1]. Actions are cleaning, copying,
	loading and saving.
	@plugin development guide *)

  val equal: t -> t -> bool
    (** @since Boron-20100401 *)

  val compare: t -> t -> int
    (** @since Boron-20100401 *)

  val hash: t -> int
    (** @since Boron-20100401 *)

end

(** Datatype implementation and how to register them. *)
module Datatype : sig

  module type INPUT = sig

    type t
      (** The datatype to register. *)

    val descr: Unmarshal.t
      (** Memory representation of the datatype. Used for unmarshalling.
	  @since Beryllium-20090901 *)

    val copy: t -> t
      (** How to deeply copy the datatype.
	  The following invariant must hold: forall (p:t), copy s != s. *)

    val name: string
      (** Name of the datatype.
	  Have to be different of others registered datatypes. *)

  end

  (** Output of {!Datatype.Register}. *)
  module type S = sig

    include INPUT
      (** Exportation of inputs (easier use of [Datatype.Register]). *)

    val register_comparable:
      ?compare:(t -> t -> int) ->
      ?equal:(t -> t -> bool) ->
      ?hash:(t -> int) ->
      unit -> unit
      (** Allow to register a specific [compare], [equal] and [hash] functions
	  for the datatype.

	  [hash] and [equal] have to be compatible, that is:
	  forall x y, equal x y ==> hash x = hash y.

	  - there is no default value for [compare];
	  - default value for [equal] is [fun x y -> compare x y = 0] if
	  [compare] is provided; no default value otherwise.
	  - default value for [hash] is Hashtbl.hash;

	  Never call [register_comparable] is equivalent to call
	  [register_comparable ()].

	  Note that, as usual in ocaml, the default values for [equal] and
	  [hash] are not compatible for all datastructures (though for the most
	  ones). *)

    val is_comparable_set: unit -> bool
      (** @return false iff [register_comparable] has never been called. *)

    (* ******************************************************************* *)
    (** {3 Access to the functions registered by {!registered_comparable}} *)
    (* ******************************************************************* *)

    val hash: t -> int
    val equal: t -> t -> bool
    val compare: t -> t -> int

    val mem_project: ((project -> bool) -> t -> bool) option ref
      (** [!mem_project] must be [Some g] with [g f x] returning [true] iff [x]
	  contains one project [p] such that [f p] returns [true].
	  [!mem_project] should be equal to [None] if there is no value of type
	  [project] in [x].
	  @since Boron-20100401 *)

  end

  (** Register a new kind of datatype by side-effects.
      @plugin development guide *)
  module Register(Datatype:INPUT) : S with type t = Datatype.t

  (** Register a single datatype, not affected by hashconsing.
      @plugin development guide *)
  module Imperative(X:sig type t val copy: t -> t val name: string end) :
    S with type t = X.t

  (** Register a single datatype, not affected by hashconsing and copying.
      @plugin development guide *)
  module Persistent(X:sig type t val name: string end) :
    S with type t = X.t

  module Own : S with type t = project
    (** @since Beryllium-20090901 *)

(* ************************************************************************* *)
  (** {3 Create a name from predefined ones}

      See module {!Namespace}. *)
(* ************************************************************************* *)

  val extend_name: string -> string -> string
  val extend_name2: string -> string -> string -> string
  val extend_name3: string -> string -> string -> string -> string

end

(** Internal state (aka Computation) representation and how to register them.
    An internal state contains the result of a computation. *)
module Computation : sig

  include KIND
    (** Common operations. *)

  type selection
    (** Just an alias for [Project.Selection.t]. *)

  (** Main input signature of {!Computation.Register}. *)
  module type INPUT = sig

    type t
      (** Type of the state to register. *)

    val create: unit -> t
      (** How to create a new fresh state which must be equal to the initial
	  state: that is, if you never change the state, [create ()] and [get
	  ()] must be equal (see invariant 1 below). *)

    val clear: t -> unit
      (** How to clear a state. After cleaning, the state should be
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

    val clear_some_projects: (project -> bool) -> t -> bool
      (** [clear_if_project f x] must clear any value [v] of type project of [x]
	  such that [f v] is [true]. Of course, if the type [t] does not contain
	  any object of type [project], this function should do nothing and
	  safely returns [fun _ -> false].
	  @return [true] iff at least one element of [x] has been cleared.
	  @since Boron-20100401 *)

  end

  (** Some additional informations used by {!Computation.Register}. *)
  module type INFO = sig
    val name: string (** Name of the internal state. *)
    val dependencies : t list (** Dependencies of this internal state. *)
  end

  (** @since Boron-20100401 *)
  module type MINIMAL_OUTPUT = sig

    val self: t
      (** The kind of the registered state.
	  @plugin development guide *)

    val select: Kind.how -> selection -> selection
      (** [select sel] add the registered state to the given selection in a
	  functional way. *)

    val depend: t -> unit
      (** [depend k] adds a dependencies from [k] to [me]. *)

    val name: string

  end


  (** Output signature of {!Computation.Register}. *)
  module type OUTPUT = sig

    include MINIMAL_OUTPUT

    val mark_as_computed: ?project:project -> unit -> unit
      (** Indicate that the registered state will not change again for the
	  given project (default is [current ()]). *)

    val is_computed: ?project:project -> unit -> bool
      (** Returns [true] iff the registered state will not change again for the
	  given project (default is [current ()]). *)

    val do_not_save: unit -> unit
      (** Call this function if the registered state must not be save/load
	  on/from disk. When loading, a new state (generated using [create]) is
	  used instead. *)

    (** Exportation of some inputs (easier use of [Computation.Register]). *)

    module Datatype: Datatype.S

    val howto_marshal: (Datatype.t -> 'a) -> ('a -> Datatype.t) -> unit
      (** [howto_marshal marshal unmarshal] registers a custom couple of
	  countions [(marshal, unmarshal)] to be used for serialization.
	  Default functions are identities. In particular, calling this
	  function must be used if [Datatype.t] is not marshallable and
	  [do_not_save] is not called.
	  @since Boron-20100401 *)

  end

  (** [Register(Datatype)(State)(Info)] registers a new kind of computation by
      side-effect.
      [Datatype] represents the datatype of a state, [State] explains how to
      deal with a state and [Info] mainly details the dependencies of the
      computation (i.e. what computations should be done before this one).
      @plugin development guide *)
  module Register
    (Datatype: Datatype.S)
    (State: INPUT with type t = Datatype.t)
    (Info: INFO)
    : OUTPUT with module Datatype = Datatype

  (** Generate a fresh dynamic state dependency graph.
      @since Boron-20100401 *)
  module Dynamic
    (Local: sig
       val restore: t -> (project -> unit)
	 (** How to restore a just-unmarshaled state kind.
	     This function must return a closure which clears the state in the
	     given project. *)
     end)
    (Info: INFO) :
  sig

    val add_dependency: t -> t -> unit
      (** [add_dependency k1 k2] indicates that the state [k1] depends on the
	  state kind [k2] in the underlying dynamic graph, that is an action of
	  the state kind [k2] must be done before one of the state kind [k1].
	  @since Boron-20100401 *)

    val remove_computation: reset:bool -> t -> unit
      (** Remove a state kind from the underlying dynamic graph.
	  [reset] must be [true] iff the dependencies of this state kind must
	  be cleared.
	  @since Boron-20100401 *)

    val self: t
      (** The state kind corresponding to the dynamic graph itself.
	  @since Boron-20100401 *)

    (** Register a new kind in the underlying dynamic graph.
	@since Boron-20100401 *)
    module Register
      (State: sig val clear: project -> unit end)
      (Info: INFO)
      : MINIMAL_OUTPUT

  end

  val dump_dependencies:
    ?only:selection -> ?except:selection -> string -> unit
    (** Debugging purpose only. *)

  val dump_dynamic_dependencies:
    ?only:selection -> ?except:selection -> string -> unit
    (** Debugging purpose only. *)

end

(** Selection of kinds of computation.
    @plugin development guide *)
module Selection : Kind.SELECTION with type kind = Computation.t
				  and type t = Computation.selection

(* ************************************************************************* *)
(** {2 Operations on all projects} *)
(* ************************************************************************* *)

val create: string -> t
  (** Create a new project with the given name and attach it after the existing
      projects (so the current project, if existing, is unchanged).
      The given name may be already used by another project.
      If there is no other project, then the new one is the current one.
      @plugin development guide *)

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
(** {3 Inputs/Outputs} *)
(* ************************************************************************* *)

exception IOError of string
  (** @plugin development guide *)

val save:
  ?only:Selection.t -> ?except:Selection.t -> ?project:t -> string -> unit
  (** Save a given project in a file. Default project is [current ()].
      @raise IOError if the project cannot be saved.
      @plugin development guide *)

val load:
  ?only:Selection.t -> ?except:Selection.t -> ?name:string -> string -> t
  (** Load a file into a new project given by its name.
      More precisely, [load only except name file]:
      {ol
      {- creates a new project;}
      {- performs all the registered [before_load] actions, following the
      datatype dependencies;}
      {- loads the (specified) states of the project according to its
      description; and}
      {- performs all the registered [after_load] actions.}
      }
      @raise IOError if the project cannot be loaded
      @return the new project containing the loaded data.
      @plugin development guide *)

val save_all: ?only:Selection.t -> ?except:Selection.t -> string -> unit
  (** Save all the projects in a file.
      @raise IOError a project cannot be saved. *)

val load_all: ?only:Selection.t -> ?except:Selection.t -> string -> unit
  (** First remove all the existing project, then load all the projects from a
      file. For each project to load, the specification is the same than
      {!Project.load}. Furthermore, after loading, all the hooks registered by
      [register_after_set_current_hook] are applied.
      @raise IOError if a project cannot be loaded. *)

val register_before_load_hook: (unit -> unit) -> unit
  (** [register_before_load_hook f] adds a hook called just before loading
      **each project** (more precisely, the project exists but is empty while
      the hook is applied): if [n] projects are on disk, the same hook will be
      called [n] times (one call by project).

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
      **all projects**.
      @since Boron-20100401 *)

(* ************************************************************************* *)
(** {2 Operations on one project}

    Most operations have two optional arguments, called [only] and [except] of
    type [selection].
    - If [only] is specified, only the selected state kinds are copied.
    - If [except] is specified, those selected state kinds are not copied (even
    if they are also selected by [only]).
    - If both [only] and [except] are specifid, the operation only applied on
    the [only] states, except the [except] ones.

    Use it carefuly because Frama-C may become lost and inconsistent if these
    specifications are incorrects. *)
(* ************************************************************************* *)

val name: t -> string
  (** Project name. Two projects may have the same name. *)

val unique_name: t -> string
  (** Return a project name based on {!name} but different of each others
      [unique_name]. *)

val set_name: t -> string -> unit
  (** Set the name of the given project.
      @since Boron-20100401 *)

val from_unique_name: string -> t
  (** Return a project based on {!unique_name}.
      @raise Not_found if no project has this unique name. *)

val set_current:
  ?on:bool -> ?only:Selection.t -> ?except:Selection.t -> t -> unit
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

val on:
  ?only:Selection.t -> ?except:Selection.t -> t -> ('a -> 'b) -> 'a -> 'b
  (** [on p f x] sets the current project to [p], computes [f x] then
      restores the current project. You should use this function if you use a
      project different of [current ()].
      @plugin development guide *)

val copy:
  ?only:Selection.t -> ?except:Selection.t -> ?src:t -> t -> unit
  (** Copy a project into another one. Default project for [src] is [current
      ()]. Replace the destination by [src].
      For each state to copy, the function [copy] given at state registration
      time must be fully implemented.
      @plugin development guide *)

val create_by_copy:
  ?only:Selection.t -> ?except:Selection.t -> ?src:t -> string -> t
  (** Return a new project with the given name by copying some states from the
      project [src]. All the other states are initialized with their default
      values.
      Use the same/load mechanism for copying. Thus it does not require that
      the copy function of the copied state is implemented. All the hooks
      applied when loading a project are applied (see {!load}). *)

val create_by_copy_hook: (t -> t -> unit) -> unit
  (** Register a hook to call at the end of {!create_by_copy}. The first
      argument of the registered function is the copy source while the
      second one is the created project. *)

val clear:
  ?only:Selection.t -> ?except:Selection.t -> ?project:t -> unit -> unit
  (** Clear the given project. Default project is [current ()]. All the
      internal states of the given project are now empty (wrt the action
      registered with {!register_todo_on_clear}). *)

val register_todo_on_clear: (t -> unit) -> unit
  (** @deprecated since Boron-20100401.
      Replaced by {!register_todo_before_clear} *)

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
(** {3 Projects are comparable values} *)
(* ************************************************************************* *)

val compare: t -> t -> int
val equal: t -> t -> bool
val hash: t -> int

(* ************************************************************************* *)
(** {3 Undoing} *)
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
