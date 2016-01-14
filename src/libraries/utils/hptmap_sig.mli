(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licenses/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Signature for the {!Hptmap} module *)

(** Some functions of this module may optionally use internal caches. It is
    the responsibility of the use to choose whether or not to use a cache,
    and whether this cache will be garbage-collectable by OCaml. *)
type cache_type =
  | NoCache
  (** The results of the function will not be cached. *)
  | PersistentCache of string
  (** The results of the function will be cached, and the function that uses
      the cache is a permanent closure (at the toplevel of an OCaml module).*)
  | TemporaryCache of string
  (** The results of the function will be cached, but the function itself
      is a local function which is garbage-collectable. *)


(** Signature for hptmaps from hash-consed trees to values *)
module type S = sig
  type key (** type of the keys *)
  type v (** type of the values *)
  type 'a shape
  type prefix

  include Datatype.S_with_collections

  (** Bijective function. The ids are positive. *)
  val id: t -> int

  val self : State.t

  val empty : t
  (** the empty map *)

  val is_empty : t -> bool
  (** [is_empty m] returns [true] if and only if the map [m] defines no
      bindings at all. *)

  val add : key -> v -> t -> t
  (** [add k d m] returns a map whose bindings are all bindings in [m], plus
      a binding of the key [k] to the datum [d]. If a binding already exists
      for [k], it is overridden. *)

  val find : key -> t -> v
  val find_check_missing: key -> t -> v
  (** Both [find key m] and [find_check_missing key m] return the value
      bound to [key] in [m], or raise [Not_found] is [key] is unbound.
      [find] is optimised for the case where [key] is bound in [m], whereas
      [find_check_missing] is more efficient for the cases where [m]
      is big and [key] is missing. *)

  val find_key : key -> t -> key
  (** This function is useful where there are multiple distinct keys that
      are equal for [Key.equal]. *)

  val remove : key -> t -> t
  (** [remove k m] returns the map [m] deprived from any binding involving
      [k]. *)

  val mem :  key -> t -> bool
  (** [mem k m] returns true if [k] is bound in [m], and false otherwise. *)

  val iter : (key -> v -> unit) -> t -> unit
  (** [iter f m] applies [f] to all bindings of the map [m]. *)

  val map : (v -> v) -> t -> t
  (** [map f m] returns the map obtained by composing the map [m] with the
      function [f]; that is, the map $k\mapsto f(m(k))$. *)

  val map': (key -> v -> v option) -> t -> t
  (** Same as [map], except if [f k v] returns [None]. In this case, [k] is not
      bound in the resulting map. *)

  val fold : (key -> v -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold f m seed] invokes [f k d accu], in turn, for each binding from
      key [k] to datum [d] in the map [m]. Keys are presented to [f] in
      increasing order according to the map's ordering. The initial value of
      [accu] is [seed]; then, at each new call, its value is the value
      returned by the previous invocation of [f]. The value returned by
      [fold] is the final value of [accu]. *)

  val fold_rev : (key -> v -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold_rev] performs exactly the same job as [fold], but presents keys
      to [f] in the opposite order. *)

  val for_all: (key -> v -> bool) -> t -> bool
  (** [for_all p m] returns true if all the bindings of the map [m] satisfy
      the predicate [p]. *)

  val exists: (key -> v -> bool) -> t -> bool
  (** [for_all p m] returns true if at least one binding of the map [m] satisfies
      the predicate [p]. *)

  type empty_action = Neutral | Absorbing | Traversing of (key -> v -> v option)

  val merge :
    cache:cache_type ->
    symmetric:bool ->
    idempotent:bool ->
    decide_both:(key -> v -> v -> v option) ->
    decide_left:empty_action ->
    decide_right:empty_action ->
    t -> t -> t
  (** Merge of two trees, parameterized by a merge function.
      If [symmetric] holds, the function must verify [merge x y = merge y x].
      If [idempotent] holds, the function must verify [merge x x = x].
      For each key [k] present in both trees, and bound to [v1] and [v2] in the
      left and the right tree respectively, [decide_both k v1 v2] is called. If
      the decide function returns [None], the key will not be in the resulting
      map; otherwise, the new value computed will be bound to [k].
      The [decide_left] action is performed to the left subtree [t] when a right
      subtree is empty (and conversely for the [decide_right] action when a left
      subtree is empty):
      - Neutral returns the subtree [t] unchanged;
      - Absorbing returns the empty tree;
      - (Traversing f) applies the function [f] to each binding of the remaining
        subtree [t] (see [map']).
      The results of the function may be cached, depending on [cache]. If a cache
      is used, then the merge functions must be pure.
  *)

  val generic_join :
    cache:cache_type ->
    symmetric:bool ->
    idempotent:bool ->
    decide:(key -> v option -> v option -> v) ->
    t -> t -> t
  (** Merge of two trees, parameterized by the [decide] function. If [symmetric]
      holds, the function must verify [decide key v1 v2 = decide key v2 v1]. If
      [idempotent] holds, the function must verify [decide k (Some x) (Some x) = x].
      and [merge (Some v) None = v]. *)

  val join :
    cache:cache_type ->
    symmetric:bool ->
    idempotent:bool ->
    decide:(key -> v -> v -> v) ->
    t -> t -> t
  (** Same as [generic_merge], but we assume that
      [decide key None (Some v) = decide key (Some v) None = v] holds. *)

  val inter :
    cache:cache_type ->
    symmetric:bool ->
    idempotent:bool ->
    decide:(key -> v -> v -> v option) ->
    t -> t -> t
  (** Intersection of two trees, parameterized by the [decide] function. If the
      [decide] function returns [None], the key will not be in the resulting map.
  *)

  val inter_with_shape: 'a shape -> t -> t
  (** [inter_with_shape s m] keeps only the elements of [m] that are also
      bound in the  map [s]. No caching is used, but this function is more
      efficient than successive calls to {!remove} or {!add} to build the
      resulting map. *)

  (** {2 Binary predicates} *)

  type decide_fast = Done | Unknown
  (** Shortcut for functions that decide whether a predicate holds on a tree.
      [Done] means that the function returns its default value, which is
      usually [unit]. [Unknown] means that the evaluation must continue in the
      subtrees. *)

  val generic_predicate :
    exn ->
    cache:(string * 'a) ->
    decide_fast:(t -> t -> decide_fast) ->
    decide_fst:(key -> v  -> unit) ->
    decide_snd:(key -> v  -> unit) ->
    decide_both:(v -> v -> unit) ->
    t -> t -> unit
  (** [generic_is_included e (cache_name, cache_size) ~decide_fast
          ~decide_fst ~decide_snd ~decide_both t1 t2] decides whether some
      relation holds between [t1] and [t2]. All [decide] functions must raise
      [e] when the relation does not hold, and do nothing otherwise.

      [decide_fst] (resp. [decide_snd]) is called when one key is present only
      in [t1] (resp. [t2]).

      [decide_both] is called when a key is present in both trees.

      [decide_fast] is called on entire keys. As its name implies, it must be
      fast; in doubt, returning [Unknown] is always correct. Raising [e] means
      that the relation does not hold. Returning [Done] means that the relation
      holds.

      The computation of this relation cached. [cache_name] is used to identify
      the cache when debugging. [cache_size] is currently unused. *)

  (** Existential ([||]) or universal ([&&]) predicates. *)
  type predicate_type = ExistentialPredicate | UniversalPredicate

  (** Does the given predicate hold or not. [PUnknown] indicates that the result
      is uncertain, and that the more aggressive analysis should be used. *)
  type predicate_result = PTrue | PFalse | PUnknown

  val binary_predicate:
    cache_type ->
    predicate_type ->
    decide_fast:(t -> t -> predicate_result) ->
    decide_fst:(key -> v  -> bool) ->
    decide_snd:(key -> v  -> bool) ->
    decide_both:(key -> v -> v -> bool) ->
    t -> t -> bool
  (** Same functionality as [generic_predicate] but with a different signature.
      All decisin functions return a boolean that are combined differently
      depending on whether the predicate is existential or universal. *)

  val generic_symmetric_predicate :
    exn ->
    decide_fast:(t -> t -> decide_fast) ->
    decide_one:(key -> v  -> unit) ->
    decide_both:(v -> v -> unit) ->
    t -> t -> unit
  (** Same as [generic_predicate], but for a symmetric relation. [decide_fst]
      and [decide_snd] are thus merged into [decide_one]. *)

  val symmetric_binary_predicate:
    cache_type ->
    predicate_type ->
    decide_fast:(t -> t -> predicate_result) ->
    decide_one:(key -> v  -> bool) ->
    decide_both:(key -> v -> v -> bool) ->
    t -> t -> bool
  (** Same as [binary_predicate], but for a symmetric relation. [decide_fst]
      and [decide_snd] are thus merged into [decide_one]. *)

  val decide_fast_inclusion: t -> t -> predicate_result
  (** Function suitable for the [decide_fast] argument of [binary_predicate],
      when testing for inclusion of the first map into the second. If the two
      arguments are equal, or the first one is empty, the relation holds. *)

  val decide_fast_intersection: t -> t -> predicate_result
  (** Function suitable for the [decide_fast] argument of
      [symmetric_binary_predicate] when testing for a non-empty intersection
      between two maps. If one map is empty, the intersection is empty.
      Otherwise, if the two maps are equal, the intersection is non-empty. *)

  val cached_fold :
    cache_name:string ->
    temporary:bool ->
    f:(key -> v -> 'b) ->
    joiner:('b -> 'b -> 'b) ->
    empty:'b ->
    t -> 'b

  val cached_map :
    cache:string * int ->
    temporary:bool ->
    f:(key -> v -> v) ->
    t -> t

  val singleton: key -> v -> t
  (** [singleton k d] returns a map whose only binding is from [k] to [d]. *)

  val is_singleton: t -> (key * v) option
  (** [is_singleton m] returns [Some (k, d)] if [m] is a singleton map
      that maps [k] to [d]. Otherwise, it returns [None]. *)

  val cardinal: t -> int
  (** [cardinal m] returns [m]'s cardinal, that is, the number of keys it
      binds, or, in other words, its domain's cardinal. *)

  val min_binding: t -> key * v
  val max_binding: t -> key * v

  val split: key -> t -> t * v option * t

  val compositional_bool: t -> bool
  (** Value of the compositional boolean associated to the tree, as computed
      by the {!Compositional_bool} argument of the functor. *)

  val clear_caches: unit -> unit
  (** Clear all the persistent caches used internally by the functions of this
      module. Those caches are not project-aware, so this function must be
      called at least each time a project switch occurs. *)

  val from_shape: (key -> 'a -> v) -> 'a shape -> t
  (** Build an entire map from another map indexed by the same keys.
      More efficient than just performing successive {!add} the elements
      of the other map *)

  val shape: t -> v shape
  (** Export the map as a value suitable for functions {!inter_with_shape}
      and {!from_shape} *)

  val fold2_join_heterogeneous:
    cache:cache_type ->
    empty_left:('a shape -> 'b) ->
    empty_right:(t -> 'b) ->
    both:(key -> v -> 'a -> 'b) ->
    join:('b -> 'b -> 'b) ->
    empty:'b ->
    t -> 'a shape ->
    'b
  (** [fold2_join_heterogeneous ~cache ~empty_left ~empty_right ~both
        ~join ~empty m1 m2] iterates simultaneously on [m1] and [m2]. If a subtree
      [t] is present in [m1] but not in [m2] (resp. in [m2] but not in [m1]),
      [empty_right t] (resp. [empty_left t]) is called. If a key [k] is present
      in both trees, and bound to to [v1] and [v2] respectively, [both k v1 v2] is
      called. If both trees are empty, [empty] is returned. The values of type
      ['b] returned by the auxiliary functions are merged using [join], which is
      called in an unspecified order. The results of the function may be cached,
      depending on [cache]. *)


  (**/**) (* Undocumented. *)
  val pretty_debug: Format.formatter -> t -> unit

  (* Prefixes. *)
  val comp_prefixes : t -> t -> unit
  val pretty_prefix : prefix -> Format.formatter -> t -> unit

  type subtree
  exception Found_prefix of prefix * subtree * subtree
  val find_prefix : t -> prefix -> subtree option
  val hash_subtree : subtree -> int
  val equal_subtree : subtree -> subtree -> bool
end
