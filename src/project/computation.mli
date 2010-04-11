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

(** Internal state builders.
    Provide ways to implement signature [Project.Computation.OUTPUT] without
    directly apply functor [Project.Computation.Register].
    Depending on the builder, also provide some additional useful
    information.
    @plugin development guide *)

(* ************************************************************************* *)
(** {3 Useful operations} *)
(* ************************************************************************* *)

val apply_once:
  string -> Project.Computation.t list -> (unit -> unit) ->
  (unit -> unit) * Project.Computation.t
    (** [apply_once name dep f] returns a closure applying [f] only once and the
	state internally used. [name] and [dep] are respectively the name and
	the dependencies of the local state created by this function.  Should
	be used partially applied. If [f] raises an exception, then it is
	considered as not applied. *)

(** {2 Builders} *)

(* ************************************************************************* *)
(** {3 References} *)
(* ************************************************************************* *)

(** Signature of the stored data. *)
module type REF_INPUT = sig
  include Project.Datatype.S
  val default: unit -> t
end

(** Output signature of [Ref]. *)
module type REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type data
    (** Type of the referenced value. *)
  val set: data -> unit
    (** Change the referenced value. *)
  val get: unit -> data
    (** Get the referenced value. *)
  val clear: unit -> unit
    (** Reset the reference to its default value. *)
end

(** @plugin development guide *)
module Ref(Data:REF_INPUT)(Info:Signature.NAME_DPDS)
  : REF_OUTPUT with type data = Data.t

(** Output signature of [OptionRef]. *)
module type OPTION_REF_OUTPUT = sig
  include REF_OUTPUT
  val memo: ?change:(data -> data) -> (unit -> data) -> data
    (** Memoization. Compute on need the stored value.
	If the data is already computed (i.e. is not [None]),
	it is possible to change with [change]. *)
  val map: (data -> data) -> data option
  val may: (data -> unit) -> unit
  val get_option : unit -> data option
    (** @since Beryllium-20090901 *)
end

(** Build a reference on an option. *)
module OptionRef(Data:Project.Datatype.S)(Info:Signature.NAME_DPDS) :
  OPTION_REF_OUTPUT with type data = Data.t

(** Output signature of [ListRef].
    @since Boron-20100401 *)
module type LIST_REF_OUTPUT = sig
  type data_in_list
  include REF_OUTPUT
  val iter: (data_in_list -> unit) -> unit
  val fold_left: ('a -> data_in_list -> 'a) -> 'a -> 'a
end

(** Build a reference on a list.
    @since Boron-20100401 *)
module ListRef(Data:Project.Datatype.S)(Info:Signature.NAME_DPDS) :
  LIST_REF_OUTPUT with type data = Data.t list and type data_in_list = Data.t

(* ************************************************************************* *)
(** {3 Dashtables}

    Dashtbl are projectified hashtbl in which each binding may have a
    dependency (not only the full table) and may be used as internal states. By
    using dashtable, you will have a very fine-grain control over the
    dependencies.

    IMPORTANT:
    - that is INCORRECT to add the [self] value of a dashtbl into a selection
    without also adding its dependencies.
    - that is INCORRECT to use dashtable if keys or values have a custom
    [rehash] function (see {!Project.DATATYPE_OUTPUT.rehash}) *)
(* ************************************************************************* *)

(** Sub-signature of hashtables required for building projectified hashtables
    and dashtables. *)
module type HASHTBL = sig
  include Datatype.HASHTBL
  val clear: 'a t -> unit
  val find: 'a t -> key -> 'a
  val remove: 'a t -> key -> unit
  val mem: 'a t -> key -> bool
end

(** Output signature of dashtables.
    @since Boron-20100401 *)
module type DASHTBL_OUTPUT = sig

  (** {3 Datatypes} *)

  include Project.Computation.OUTPUT
    (** A dashtable is a standard computation.
	BUT:
	- that is INCORRECT to add the [self] value of a dashtbl into a
	selection without also adding its dependencies.
	- that is INCORRECT to use dashtable if keys or values have a custom
	[rehash] function (see {!Project.DATATYPE_OUTPUT.rehash})
	@since Boron-20100401 *)

  type key
    (** Type of keys of the table.
	@since Boron-20100401 *)

  type data
    (** Type of values of the table.
	@since Boron-20100401 *)

  (** {3 Modifying the table} *)

  val add: key -> Project.Computation.t list -> data -> unit
    (** Add a new binding [key, data] in the tables.
	The dependencies are the states required for computing the binding.
	More precisely, a binding is a triple [key --> state --> data] and [add
	k l d] adds as many bindings as the length of the list, but all these
	bindings corresponds to the very same state.

	Be aware that [add k [ s1; s2 ] v] is NOT equivalent to [add k [ s1
	] v; add k [ s2 ] v].
	- In the former, it looks like having only one binding [k, v] in the
	table which requires both [s1] and [s2] to be computed. If you clear
	the dependencies of [s1], this binding is removed from the table.
	- In the latter, it looks like having have two disting bindings [k, v]
	in the table. The first one is computed by using [s1] while the second
	one (containing the same value) is computed by using [s2]. If you clear
	the dependencies of [s1], only the first binding is removed from the
	table, but the second one is still present.

	@since Boron-20100401 *)

  val replace: reset:bool -> key -> Project.Computation.t list -> data -> unit
    (** Similar to [add] but replace the existing bindings if any (same
	difference that [Hashtbl.replace] wrt [Hashtbl.add].
	If [reset] to [true], all the dependencies of old bindings are cleared.
	It is always safe to put [reset] to [true], but it may be unsafe to
	put it to [false].
	[reset] era
	@since Boron-20100401 *)

  val memo:
    reset:bool -> (data list -> data) -> key -> Project.Computation.t list ->
    data
    (** [memo f k l] replaces the bindings in the table by a new one computed
	by applying [f]. The argument of [f] is the list of existing bindings
	(the empty list if there is no binding).
	If [reset] to [true], all the dependencies of old bindings are cleared.
	It is always safe to put [reset] to [true], but it may be unsafe to
	put it to [false].
	@since Boron-20100401 *)

  val clear: reset:bool -> unit -> unit
    (** Remove all the bindings of the table.
	If [reset] is [true], all the dependencies of the table are also
	cleared.
	It is always safe to put [reset] to [true], but it may be unsafe to
	put it to [false].
	@since Boron-20100401 *)

  val remove: reset:bool -> key -> Project.Computation.t -> unit
    (** Remove all the bindings associated to the given key and state. Do
	nothing if there is no such binding.
	If [reset] is [true], clear all athe dependencies of the removed
	binding.
	It is always safe to put [reset] to [true], but it may be unsafe to
	put it to [false].
	@since Boron-20100401 *)

  val remove_all: reset:bool -> key -> unit
    (** Remove all the bindings added and associated to the given key.
	Do nothing if there is no such binding.
	If [reset] is [true], clear all the dependencies of each removed
	binding.
	It is always safe to put [reset] to [true], but it may be unsafe to
	put it to [false].
	@since Boron-20100401 *)

  val filter:
    reset:bool -> (key -> Project.Computation.t -> data -> bool) -> key -> unit
    (** Remove all the bindings added and associated to the given key and
	which does not satisfy the given condition.
	Do nothing if there is no such binding.
	If [reset] is [true], clear all the dependencies of each removed
	binding.
	It is always safe to put [reset] to [true], but it may be unsafe to
	put it to [false].
	@since Boron-20100401 *)

  (** {3 Finders} *)

  val mem: key -> bool
    (** @return [true] if there is a binding with the given key.
	@since Boron-20100401 *)

  val find:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    data * Project.Computation.t
      (** Get the binding associated to the given key and state.
	  if [who] is set, automatically adds dependency from the found state
	  to each of states of [who].
	  @raise Not_found if there is no such binding
	  @since Boron-20100401 *)

  val find_data:
    ?who:Project.Computation.t list -> key -> Project.Computation.t -> data
    (** Get the data associated to the given key and state.
	if [who] is set, automatically adds dependency from the state
	corresponding to the given data	to each of states of [who].
	@raise Not_found if there is no such binding
	@since Boron-20100401 *)

  val find_state:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    Project.Computation.t
      (** Get the state associated to the given key and state.
	  if [who] is set, automatically adds dependency from the found state
	  to each of states of [who].
	  @raise Not_found if there is no such binding
	  @since Boron-20100401 *)

  val find_all_local:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    (data * Project.Computation.t) list
      (** Get all the bindings associated to the given key and state.
	  if [who] is set, automatically adds dependency from each found state
	  to each of states of [who].
	  @since Boron-20100401 *)

  val find_all_local_data:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    data list
      (** Get all the data associated to the given key and state.
	  if [who] is set, automatically adds dependency from the state
	  corresponding to each data to each of states of [who].
	  @since Boron-20100401 *)

  val find_all_local_state:
    ?who:Project.Computation.t list -> key -> Project.Computation.t ->
    Project.Computation.t list
      (** Get all the states associated to the given key and state.
	  if [who] is set, automatically adds dependency from each found state
	  to each of states of [who].
	  @since Boron-20100401 *)

  val find_all:
    ?who:Project.Computation.t list -> key ->
    (data * Project.Computation.t) list
      (** Get all bindings associated to the given key.
	  if [who] is set, automatically adds dependency from each found state
	  to each of states of [who].
	  @since Boron-20100401 *)

  val find_all_data:
    ?who:Project.Computation.t list -> key -> data list
    (** Get all data associated to the given key.
	if [who] is set, automatically adds dependency from the state
	correspondin to of each found data to each of states of [who].
	@since Boron-20100401 *)

  val find_all_states:
    ?who:Project.Computation.t list -> key -> Project.Computation.t list
    (** Get all states associated to the given key.
	if [who] is set, automatically adds dependency from each found state
	to each of states of [who].
	@since Boron-20100401 *)

  (** {3 Iterators} *)

  val iter: (key -> Project.Computation.t -> data -> unit) -> unit
    (** Iterator on each binding of the table.
	@since Boron-20100401 *)

  val iter_key: (Project.Computation.t -> data -> unit) -> key -> unit
    (** Iterator on each binding of the table associated to the given key.
	@since Boron-20100401 *)

  val fold: (key -> Project.Computation.t -> data -> 'a -> 'a) -> 'a -> 'a
    (** Folder on each binding of the table.
	@since Boron-20100401 *)

  val fold_key: (Project.Computation.t -> data -> 'a -> 'a) -> key -> 'a -> 'a
    (** Folder on each binding of the table associated to the given key.
	@since Boron-20100401 *)

  (** {3 Miscellaneous} *)

  val length: unit -> int
    (** Number of bindings in the table.
	@since Boron-20100401 *)

  val add_dependency: Project.Computation.t -> Project.Computation.t -> unit
    (** Add a dependency local to the dash-table.
	At least one of the two given states must be either [self] (the state
	of the dashtable) or the state of one binding of the table.
	@since Boron-20100401 *)

end

(** Build a dashtable from an implementation of hashtable, a datatype for
    values and usual additional information.
    @since Boron-20100401 *)
module Make_Dashtbl
  (H:HASHTBL)(Data:Project.Datatype.S)(Info:Signature.NAME_SIZE_DPDS)
  : DASHTBL_OUTPUT with type key = H.key and type data = Data.t

(** Build a dashtable from hashable keys, a datatype for values and usual
    additionnal information.
    @since Boron-20100401 *)
module Dashtbl
  (Key:Hashtbl.HashedType)
  (Data:Project.Datatype.S)
  (Info:Signature.NAME_SIZE_DPDS) :
  DASHTBL_OUTPUT with type key = Key.t and type data = Data.t

(* ************************************************************************* *)
(** {3 Weak Hashtbl} *)
(* ************************************************************************* *)

(** Output signature of builders of hashtables.
    @since Boron-20100401 *)
module type WEAK_HASHTBL_OUTPUT = sig

  include Project.Computation.OUTPUT
    (** Hashtbl are a standard computation.
	BUT it is INCORRECT to use projectified hashtables if keys have a
	custom [rehash] function (see {!Project.DATATYPE_OUTPUT.rehash}) *)

  type data
    (** @since Boron-20100401 *)
  val merge: data -> data
    (** [merge x] returns an instance of [x] found in the table if any, or else
	adds [x] and return [x].
	@since Boron-20100401 *)
  val add: data -> unit
    (** [add x] adds [x] to the table. If there is already an instance of [x],
	it is unspecified which one will be returned by subsequent calls to
	[find] and [merge].
	@since Boron-20100401 *)
  val clear: unit -> unit
    (** Clear the table.
	@since Boron-20100401 *)
  val count: unit -> int
    (** Length of the table.
	@since Boron-20100401 *)
  val iter: (data -> unit) -> unit
    (** @since Boron-20100401 *)
  val fold: (data -> 'a -> 'a) -> 'a -> 'a
    (** @since Boron-20100401 *)
  val find: data -> data
    (** [find x] returns an instance of [x] found in table.
	@Raise Not_found if there is no such element.
	@since Boron-20100401 *)
  val find_all: data -> data list
    (** [find_all x] returns a list of all the instances of [x] found in t.
	@since Boron-20100401 *)
  val mem: data -> bool
    (** [mem x] returns [true] if there is at least one instance of [x] in the
	table, [false] otherwise.
	@since Boron-20100401 *)
  val remove: data -> unit
    (** [remove x] removes from the table one instance of [x]. Does nothing if
	there is no instance of [x].
	@since Boron-20100401 *)

end

(** Build a weak hashtbl over a datatype [Data] from a reference implementation
    [W].
    @since Boron-20100401 *)
module Make_WeakHashtbl
  (W:Weak.S)
  (Data: Project.Datatype.S with type t = W.data)
  (Info: Signature.NAME_SIZE_DPDS) :
  WEAK_HASHTBL_OUTPUT with type data = W.data

(** Build a weak hashtbl over a datatype [Data] by using [Weak.Make] provided
    by the ocaml standard library. Note that the table is not saved on disk.
    @since Boron-20100401 *)
module WeakHashtbl(Data: Project.Datatype.S)(Info: Signature.NAME_SIZE_DPDS) :
  WEAK_HASHTBL_OUTPUT with type data = Data.t

(** Weak hashtbl dedicated to hashconsing.
    Note that the resulting table is not saved on disk.
    @since Boron-20100401 *)
module HashconsTbl
  (Data: sig
    type t
       (** The hashconsed type *)
    val name: string
       (** Name of the hashconsed datatype *)
     val equal_internal: t -> t -> bool
       (** Equality on the datatype internally used by the built table. *)
     val hash_internal: t -> int
       (** Hash function for datatype internally used by the built table. *)
     val initial_values: t list
       (** Pre-existing values stored in the built table and shared by all
	   the existing projects. *)
   end)
  (Info: Signature.NAME_SIZE_DPDS) :
  WEAK_HASHTBL_OUTPUT with type data = Data.t

(* ************************************************************************* *)
(** {3 Hashtables}

    IMPORTANT: that is INCORRECT to use projectified hashtables if keys have a
    custom [rehash] function (see {!Project.DATATYPE_OUTPUT.rehash}) *)
(* ************************************************************************* *)

(** Output signature of builders of hashtables. *)
module type HASHTBL_OUTPUT = sig
  include Project.Computation.OUTPUT
    (** Hashtbl are a standard computation.
	BUT that is INCORRECT to use projectified hashtables if keys have a
	custom [rehash] function (see {!Project.DATATYPE_OUTPUT.rehash}) *)

  type key
  type data
  val replace: key -> data -> unit
    (** Add a new binding. The previous one is removed. *)
  val add: key -> data -> unit
    (** Add a new binding. The previous one is only hidden. *)
  val clear: unit -> unit
    (** Clear the table. *)
  val length: unit -> int
    (** Length of the table. *)
  val iter: (key -> data -> unit) -> unit
  val fold: (key -> data -> 'a -> 'a) -> 'a -> 'a
  val memo: ?change:(data -> data) -> (key -> data) -> key -> data
    (** Memoization. Compute on need the data associated to a given key using
	the given function.
	If the data is already computed, it is possible to change with
	[change]. *)
  val find: key -> data
    (** Return the current binding of the given key.
	@raise Not_found if the key is not in the table. *)
  val find_all: key -> data list
    (** Return the list of all data associated with the given key. *)
  val unsafe_find: key -> data
    (** Unsafe version of [find]. Do not raise [Not_found].
	You can use it if you can prove that the given key belongs to the
	state. *)
  val mem: key -> bool
  val remove: key -> unit
end

module Make_Hashtbl
  (H:HASHTBL)(Data:Project.Datatype.S)(Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = H.key and type data = Data.t

module Hashtbl
  (Key:Hashtbl.HashedType)
  (Data:Project.Datatype.S)
  (Info:Signature.NAME_SIZE_DPDS) :
  HASHTBL_OUTPUT with type key = Key.t and type data = Data.t

(* ************************************************************************* *)
(** {3 References on a set} *)
(* ************************************************************************* *)

module type SET = sig
  type elt
  type t
  val descr: Unmarshal.t
  val empty: t
  val singleton: elt -> t
  val is_empty: t -> bool
  val add: elt -> t -> t
  val mem: elt -> t -> bool
  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: (elt -> unit) -> t -> unit
end

(** Output signature of builders of references on a set. *)
module type SET_REF_OUTPUT = sig
  include Project.Computation.OUTPUT
  type elt
  val add: elt -> unit
  val mem: elt -> bool
  val is_empty: unit -> bool
  val fold: (elt -> 'a -> 'a) -> 'a -> 'a
  val iter: (elt -> unit) -> unit
end

module Make_SetRef
  (Set:SET)
  (Data:Project.Datatype.S with type t = Set.elt)
  (Info:Signature.NAME_DPDS) :
  SET_REF_OUTPUT with type elt = Data.t

module SetRef(Data:Project.Datatype.S)(Info:Signature.NAME_DPDS)
  : SET_REF_OUTPUT with type elt = Data.t

(* ************************************************************************* *)
(** {3 Queue} *)
(* ************************************************************************* *)

module type QUEUE = sig
  type elt
  val add: elt -> unit
  val iter: (elt -> unit) -> unit
  val is_empty: unit -> bool
end

module Queue(Data:Project.Datatype.S)(Info:Signature.NAME_DPDS)
  : QUEUE with type elt = Data.t

(* ************************************************************************* *)
(** {3 Project itself} *)
(* ************************************************************************* *)

module Project(Info:Signature.NAME_DPDS)
  : REF_OUTPUT with type data = Project.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
