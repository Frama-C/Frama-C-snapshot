(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: project.mli,v 1.11 2008/05/30 08:29:49 uid568 Exp $ *)

(** Projects management. 

    A project groups together all the internal states of Frama-C. An internal
    state is roughly the result of a computation which depends of an AST. It is
    possible to have many projects at the same time. For registering a new
    state in the Frama-C projects, apply the functor {!Computation.Register}. 

    @plugin developer guide *)

val set_debug_level: int -> unit
  (** Set the level of debug: 0 = no level; 1 = small debugging messages and so
      on. *)

(* ************************************************************************* *)
(** {2 Datatypes} *)
(* ************************************************************************* *)

type t
  (** Type of a project. 
      @plugin developer guide *)

type project = t 
    (** Alias for the project type. *)

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
	@plugin developer guide *)

  val name: t -> string
    (** Name of a kind. *)

  exception Circular_Dependency of t * t
    (** May be raised by [add_dependency]. *)

  val add_dependency: t -> t -> unit
    (** [add_dependency k1 k2] indicates that the state kind [k1] depends on
	the state kind [k2], that is an action of the state kind [k2] must be
	done before one of the state kind [k1]. Actions are cleaning, copying,
	loading and saving. 
	@raise Circular_Dependency if there is a circular dependency between
	two state kinds. 
	@plugin developer guide *)

end

(** Signature for building names. 
    Such names are required in input of registering functors. *)
module type NAME = sig

  type t
    (** Type of a name. 
	@plugin developer guide *)

  exception AlreadyExists of string
    (** May be raised by [make]. *)

  val make: string -> t
    (** [make s] create a new name [s]. 
	@raise AlreadyExists if such a name already exists. 
	@plugin developer guide *)

  val extend: string -> t -> t
    (** [extend s n] extends [s] by [n] in order to build a name of
	the form [s(n)]. *)

  val extend2: string -> t -> t -> t
    (** [extend s n1 n2] extends [s] by [n1] and [n2] in order to build a name
	of the form [s(n1,n2)]. *)

  val get: t -> string
    (** Return a string corresponding to the name. *)

end

(** Datatype implementation and how to register them. *)
module Datatype : sig

  include KIND
    (** Common operations. *)

  module Name: NAME
    (** Name implementation for datatypes. *)

  (** Input signature of {!Datatype.Register}. *)
  module type INPUT = sig

    val dependencies : t list 
      (** Dependencies of this datatype. *)

    type t
      (** The datatype to register. *)

    val before_load: unit -> unit
      (** Action to perform before loading a project (related to this
	  datatype). 
	  @plugin developer guide *) 

    val after_load: unit -> unit
      (** Action to perform after loading a project (related to this
	  datatype). 
	  @plugin developer guide *)

    val rehash: t -> t
      (** How to rehashcons the datatype. *)

    val copy: t -> t
      (** How to deeply copy the datatype. *)

    val name: Name.t
      (** Name of the datatype. Has to be different of all other existing
	  names. *)

  end

  (** Output of {!Datatype.Register}. *)
  module type OUTPUT = sig

    val self: t
      (** The kind of the registered datatype. *)

    val depend: t -> unit
      (** [depend k] adds a dependencies from [k] to [me]. *)

    include INPUT
      (** Exportation of inputs (easier use of [Datatype.Register]). *)

  end

  (** Register a new kind of datatype by side-effects. 
      @plugin developer guide *)
  module Register(Datatype:INPUT) : OUTPUT with type t = Datatype.t

  (** Register a single datatype, not affected by hashconsing. 
      @plugin developer guide *)
  module Imperative(X:sig type t val copy: t -> t end) : 
    OUTPUT with type t = X.t

  (** Register a single datatype, not affected by hashconsing and copying. 
      @plugin developer guide *)
  module Persistent(X:sig type t end) : OUTPUT with type t = X.t

  val dump_dependencies: string -> unit
    (** Debugging purpose. *)

end

(** Internal state (aka Computation) representation and how to register them. 
    An internal state contains the result of a computation. *)
module Computation : sig

  include KIND
    (** Common operations. *)

  type selection
    (** Just an alias for [Project.Selection.t]. *)

  module Name: NAME
    (** Name implementation for internal states. *)

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
	  @plugin developer guide *)

    val get: unit -> t
      (** How to access to the current state. Be aware of invariants 3 and 4
	  below. *)

    val set: t -> unit
      (** How to change the current state. Be aware of invariants 3 and 4
	  below. *)

  (** The four following invariants must hold.
      {ol
      {- [create () != create ()]}
      {- forall [(p:t)], [create () = (clear p; set p; get ())]}
      {- forall [(p:t)], [set p; p == get ()]}
      {- forall [(p1:t),(p2:t)], 
      [set p1; let p = get () in set p2; p != get ()]}
      }
  *)
  end

  (** Some additional informations used by {!Computation.Register}. *)
  module type INFO = sig
    val name: Name.t (** Name of the datatype. *)
    val dependencies : t list (** Dependencies of this datatype. *)
  end

  (** Output signature of {!Computation.Register}. *)
  module type OUTPUT = sig 

    val self: t
      (** The kind of the registered state. 
	  @plugin developer guide *)

    val select: Kind.how -> selection -> selection
      (** [select sel] add the registered state to the given selection in a
	  functional way. *)

    val depend: t -> unit
      (** [depend k] adds a dependencies from [k] to [me]. *)

    val mark_as_computed: ?project:project -> unit -> unit
      (** Indicate that the registered state will not change again for the
	  given project (default is [current ()]). *)

    val is_computed: ?project:project -> unit -> bool
      (** Returns [true] iff the registered state will not change again for the
	  given project (default is [current ()]). *)

    (** Exportation of some inputs (easier use of [Computation.Register]). *)

    module Datatype: Datatype.OUTPUT
    val name: Name.t

  end

  (** [Register(Datatype)(State)(Info)] registers a new kind of computation by
      side-effect.
      [Datatype] represents the datatype of a state, [State] explains how to
      deal with a state and [Info] mainly details the dependencies of the
      computation (i.e. what computations should be done before this one). 
      @plugin developer guide *)
  module Register
    (Datatype: Datatype.OUTPUT)
    (State: INPUT with type t = Datatype.t)
    (Info: INFO) 
    : OUTPUT with module Datatype = Datatype

  val dump_dependencies: 
    ?only:selection -> ?except:selection -> string -> unit
    (** Debugging purpose only. *)

end

(** Selection of kinds of computation. 
    @plugin developer guide *)
module Selection : Kind.SELECTION with type kind = Computation.t 
				  and type t = Computation.selection

(* ************************************************************************* *)
(** {2 Operations on all projects} *)
(* ************************************************************************* *)

val create: string -> t
  (** Create a new project with the given name and attach it after the existing
      projects (so the current project, if existing, is unchanged). 
      The given name may be already used by another project. 
      @plugin developer guide *)

exception NoProject
  (** May be raised by [current]. *)

val current: unit -> t
  (** The current project. 
      @raise NoProject if there is no project. 
      @plugin developer guide *)

val is_current: t -> bool
  (** Check whether the given project is the current one or not. *)

val iter_on_projects: (t -> unit) -> unit
  (** iteration on project starting with the current one. *)

val find_all: string -> t list
  (** Find all projects with the given name. *)

val clear_all: unit -> unit
  (** Clear all the projects: all the internal states of all the projects are
      now empty (wrt the action registered with {!register_todo_on_clear}). *)

(** {3 Inputs/Outputs} *)

exception IOError of string
  (** @plugin developer guide *)

val save_all: string -> unit
  (** Save all the projects in a file. 
      @raise IOError a project cannot be saved. *)

val load_all: string -> unit
  (** Load all the projects from a file.
      For each project to load, the specification is the same than
      {!Project.load}.
      @raise IOError if a project cannot be loaded. 
      @plugin developer guide *)

(* ************************************************************************* *)
(** {2 Operations on one project} 

    Most operations have two optional arguments, called [only] and [except] of
    type [selection]. 
    - If [only] is specified, only the selected state kinds are copied.
    - If [except] is specified, those selected state kinds are not copied (even
    if they are also selected by [only]).
    Use it carefuly because Frama-C may become lost and inconsistent if these
    specifications are incorrects. *)
(* ************************************************************************* *)

val name: t -> string
  (** Project name. Two projects may have the same name. *)

val unique_name: t -> string
  (** Return a project name based on {!name} but different of each others
      [unique_name]. *)

val set_current: ?only:Selection.t -> ?except:Selection.t -> t -> unit
  (** Set the current project with the given one.
      @raise Invalid_argument if the given project does not exist anymore. 
      @plugin developer guide *)

val on: 
  ?only:Selection.t -> ?except:Selection.t -> t -> ('a -> 'b) -> 'a -> 'b
  (** [on p f x] sets the current project to [p], computes [f x] then 
      restores the current project. You should use this function if you use a
      project different of [current ()]. 
      @plugin developer guide *)

val copy: 
  ?only:Selection.t -> ?except:Selection.t -> ?src:t -> t -> unit
  (** Copy a project into another one. Default project for [src] is [current
      ()]. Replace the destination by [src]. 
      @plugin developer guide *)

val clear: 
  ?only:Selection.t -> ?except:Selection.t -> ?project:t -> unit -> unit
  (** Clear the given project. Default project is [current ()]. All the
      internal states of the given project are now empty (wrt the action
      registered with {!register_todo_on_clear}). *)

val register_todo_on_clear: (unit -> unit) -> unit
  (** Register action to perform just before clearing a project. *)

val remove: ?project:t -> unit -> unit
  (** Default project is [current ()]. If the current project is removed, then
      the new current project is the previous current project if it still
      exists (and so on). *)

(** {3 Projects are comparable values} *)

val compare: t -> t -> int
val equal: t -> t -> bool
val hash: t -> int

(** {3 Inputs/Outputs} *)

val save: 
  ?only:Selection.t -> ?except:Selection.t -> ?project:t -> string -> unit
  (** Save a given project in a file. Default project is [current ()]. 
      @raise IOError if the project cannot be saved. 
      @plugin developer guide *)

val load: 
  ?only:Selection.t -> ?except:Selection.t -> name:string -> string -> t
  (** Load a file into a new project given by its name. 
      More precisely, [load only except name file]:
      {ol
      {- creates a new project;}
      {- performs all the registered [before_load] actions, following the
      datatype dependencies;}
      {- loads the (specified) states of the project and rehashcons them
      (following the computation dependencies); and}
      {- performs all the registered [after_load] actions.}
      }
      @raise IOError if the project cannot be loaded
      @return the new project containing the loaded data. 
      @plugin developer guide *)

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.. -j"
  End:
*)
