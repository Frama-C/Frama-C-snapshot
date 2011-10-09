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

(** Dashtbl. It is an hashtbl in which each binding defines a state. Thus it
    may have dependencies. By using a dashtable, you will have a very
    fine-grain control over state dependencies. *)

(** Signature of a dashtbl.
    @since Carbon-20101201 *)
module type S = sig

  include Datatype.S
  (** Datatype of a dashtbl.
      @since Carbon-20101201 *)

  type key
    (** Type of keys of the table.
        @since Boron-20100401 *)

  type data
    (** Type of values of the table.
        @since Boron-20100401 *)

  (** {3 Modifying a table} *)

  val create: int -> t
  (** Create a new dashtbl.
      @since Carbon-20101201 *)

  val add: t -> string -> key -> State.t list -> data -> unit
    (** Add a new binding [key, data] in the table.
        The dependencies are the states required for computing the binding.
        More precisely, a binding is a triple [key --> state --> data] and [add
        k l d] adds as many bindings as the length of the list, but all these
        bindings correspond to the very same state.

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

        @modify Carbon-20101201 adding the string argument
        @since Boron-20100401 *)

  val replace: reset:bool -> t -> string -> key -> State.t list -> data -> unit
    (** Similar to [add] but replace the existing bindings if any (same
        difference that [Hashtbl.replace] wrt [Hashtbl.add].
        If [reset] to [true], all the dependencies of old bindings are cleared.
        It is always safe to put [reset] to [true], but it may be unsafe to
        put it to [false].
        [reset] era
        @modify Carbon-20101201 adding the string argument
        @since Boron-20100401 *)

  val memo:
    reset:bool -> (data list -> data) -> t -> string -> key -> State.t list ->
    data
  (** [memo ~reset f s k l] replaces the bindings in the table by a new one
      computed by applying [f]. The argument of [f] is the list of existing
      bindings (the empty list if there is no binding).  If [reset] is
      [true], all the dependencies of old bindings are cleared.  It is always
      safe to put [reset] to [true], but it may be unsafe to put it to
      [false].
      @modify Carbon-20101201 adding the string argument
      @since Boron-20100401 *)

  val clear: reset:bool -> t -> unit
  (** Remove all the bindings of the table.
      If [reset] is [true], all the dependencies of the table are also
      cleared.
      It is always safe to put [reset] to [true], but it may be unsafe to
      put it to [false].
      @since Boron-20100401 *)

  val remove: reset:bool -> t -> key -> State.t -> unit
    (** Remove all the bindings associated to the given key and state. Do
        nothing if there is no such binding.
        If [reset] is [true], clear all athe dependencies of the removed
        binding.
        It is always safe to put [reset] to [true], but it may be unsafe to
        put it to [false].
        @since Boron-20100401 *)

  val remove_all: reset:bool -> t -> key -> unit
    (** Remove all the bindings added and associated to the given key.
        Do nothing if there is no such binding.
        If [reset] is [true], clear all the dependencies of each removed
        binding.
        It is always safe to put [reset] to [true], but it may be unsafe to
        put it to [false].
        @since Boron-20100401 *)

  val filter:
    reset:bool -> (key -> State.t option -> data -> bool) -> t -> key -> unit
    (** Remove all the bindings added and associated to the given key and
        which does not satisfy the given condition.
        Do nothing if there is no such binding.
        If [reset] is [true], clear all the dependencies of each removed
        binding.
        It is always safe to put [reset] to [true], but it may be unsafe to
        put it to [false].
        @since Boron-20100401 *)

  (** {3 Finders} *)

  val mem: t -> key -> bool
    (** @return [true] if there is a binding with the given key.
        @since Boron-20100401 *)

  val is_local: t -> State.t -> bool
  (** @return [true] if the state corresponds to a binding of the dashtbl.
      @since Carbon-20101201 *)

  val find: ?who:State.t list -> t -> key -> State.t -> data * State.t
      (** Get the binding associated to the given key and state.
          if [who] is set, automatically adds dependency from the found state
          to each of states of [who].
          @raise Not_found if there is no such binding
          @since Boron-20100401 *)

  val find_key: t -> State.t -> (key * State.t) list
  (** Return the keys with their corresponding states which map to the given
      local state.
      @since Carbon-20101201 *)

  val find_data: ?who:State.t list -> t -> key -> State.t -> data
    (** Get the data associated to the given key and state.
        if [who] is set, automatically adds dependency from the state
        corresponding to the given data to each of states of [who].
        @raise Not_found if there is no such binding
        @since Boron-20100401 *)

  val find_state: ?who:State.t list -> t -> key -> State.t -> State.t
      (** Get the state associated to the given key and state.
          if [who] is set, automatically adds dependency from the found state
          to each of states of [who].
          @raise Not_found if there is no such binding
          @since Boron-20100401 *)

  val find_all_local:
    ?who:State.t list -> t -> key -> State.t -> (data * State.t) list
      (** Get all the bindings associated to the given key and state.
          if [who] is set, automatically adds dependency from each found state
          to each of states of [who].
          @since Boron-20100401 *)

  val find_all_local_data:
    ?who:State.t list -> t -> key -> State.t -> data list
      (** Get all the data associated to the given key and state.
          if [who] is set, automatically adds dependency from the state
          corresponding to each data to each of states of [who].
          @since Boron-20100401 *)

  val find_all_local_states:
    ?who:State.t list -> t -> key -> State.t -> State.t list
      (** Get all the states associated to the given key and state.
          if [who] is set, automatically adds dependency from each found state
          to each of states of [who].
          @since Boron-20100401 *)

  val find_all: ?who:State.t list -> t -> key -> (data * State.t) list
      (** Get all bindings associated to the given key.
          if [who] is set, automatically adds dependency from each found state
          to each of states of [who].
          @since Boron-20100401 *)

  val find_all_data: ?who:State.t list -> t -> key -> data list
    (** Get all data associated to the given key.
        if [who] is set, automatically adds dependency from the state
        correspondin to of each found data to each of states of [who].
        @since Boron-20100401 *)

  val find_all_states: ?who:State.t list -> t -> key -> State.t list
    (** Get all states associated to the given key.
        if [who] is set, automatically adds dependency from each found state
        to each of states of [who].
        @since Boron-20100401 *)

  (** {3 Iterators} *)

  val iter: (key -> State.t option -> data * State.t -> unit) -> t -> unit
    (** Iterator on each binding of the table.
        @since Boron-20100401
        @modify Carbon-20101201 *)

  val iter_key: (State.t option -> data * State.t  -> unit) -> t -> key -> unit
    (** Iterator on each binding of the table associated to the given key.
        @since Boron-20100401
        @modify Carbon-20101201 *)

  val fold:
    (key -> State.t option -> data * State.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** Folder on each binding of the table.
        @since Boron-20100401
        @modify Carbon-20101201 *)

  val fold_key:
    (State.t option -> data * State.t -> 'a -> 'a) -> t -> key -> 'a -> 'a
    (** Folder on each binding of the table associated to the given key.
        @since Boron-20100401
        @modify Carbon-20101201 *)

  (** {3 Miscellaneous} *)

  val length: t -> int
    (** Number of bindings in the table.
        @since Boron-20100401 *)

  type marshaled
  val marshaler: (t -> marshaled) * (marshaled -> t)

end

module type Graph = sig
  val create_and_add_state:
    clear:(Project.t -> unit) -> name:string -> deps:State.t list -> State.t
  val add_state: State.t -> unit
  val remove_state: reset:bool -> State.t -> unit
  val self: State.t ref
  val internal_kind: State.kind
end

module type Key = sig
  include Datatype.S_with_collections
  type marshaled
  val marshaler: (t -> marshaled) * (marshaled -> t)
  val equal_marshaled: marshaled -> marshaled -> bool
  val hash_marshaled: marshaled -> int
end

module Default_key_marshaler(K: Datatype.S_with_collections) :
  Key with type t = K.t

module type Data = sig
  include Datatype.S
  type marshaled
  val marshaler: (t -> marshaled) * (marshaled -> t)
end

module Default_data_marshaler(D: Datatype.S) : Data with type t = D.t

module Make(G: Graph)(K: Key)(D: Data)(Info: sig val name: string end) :
  S with type key = K.t and type data = D.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
