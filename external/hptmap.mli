(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Menhir                               *)
(*                                                                        *)
(*  François Pottier and Yann Régis-Gianas, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright 2005 Institut National de Recherche en Informatique et      *)
(*  en Automatique. All rights reserved. This file is distributed         *)
(*  under the terms of the Q Public License version 1.0, with the         *)
(*  change described in the file licences/Q_MODIFIED_LICENSE.             *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Efficient maps from hash-consed trees to values, implemented as
    Patricia trees. *)

(** This implementation of big-endian Patricia trees follows Chris
    Okasaki's paper at the 1998 ML Workshop in Baltimore.  Maps are
    implemented on top of Patricia trees. A tree is big-endian if it
    expects the key's most significant bits to be tested first. *)


(**/**) (* Undocumented. Needed for advanced users only *)
type prefix
val sentinel_prefix : prefix
(**/**)

type tag

(** Type of the keys of the map. *)
module type Id_Datatype = sig  
    include Datatype.S
    val id: t -> int (** Identity of a key. Must verify [id k >= 0] and
                         [equal k1 k2 ==> id k1 = id k2] *)
end

(** This functor exports the {i shape} of the maps indexed by keys [Key].
    Those shapes can be used by various functions to efficiently build
    new maps whose shape are already known. *)
module Shape (Key : Id_Datatype): sig
  type 'value t
end

module Make
  (Key : Id_Datatype)
  (V : Datatype.S (** Values stored in the map. *))
  (Compositional_bool : sig
     (** A boolean information is maintained for each tree, by composing the
         boolean on the subtrees and the value information present on each leaf.
         See {!Comp_unused} for a default implementation. *)

     val e: bool  (** Value for the empty tree *)
     val f : Key.t -> V.t -> bool  (** Value for a leaf *)
     val compose : bool -> bool -> bool
       (** Composition of the values of two subtrees *)
     val default:bool
   end)
  (Initial_Values : sig
    val v : (Key.t*V.t) list list
    (** List of the maps that must be shared between all instances of Frama-C
        (the maps being described by the list of their elements).
        Must include all maps that are exported at Caml link-time when the
        functor is applied. This usually includes at least the empty map, hence
        [v] nearly always contains [[]]. *)
  end) 
  (Datatype_deps: sig
    val l : State.t list
    (** Dependencies of the hash-consing table. The table will be cleared
        whenever one of those dependencies is cleared. *)
  end)
  :
sig
  type key = Key.t

  type t

  include Datatype.S_with_collections with type t := t

  val self : State.t 

  val empty : t

  val hash : t -> int

  val is_empty : t -> bool
  (** [is_empty m] returns [true] if and only if the map [m] defines no
      bindings at all. *)

  val add : key -> V.t -> t -> t
  (** [add k d m] returns a map whose bindings are all bindings in [m], plus
      a binding of the key [k] to the datum [d]. If a binding already exists
      for [k], it is overridden. *)

  val find : key -> t -> V.t
  val find_key : key -> t -> key
  val remove : key -> t -> t
  (** [remove k m] returns the map [m] deprived from any binding involving
      [k]. *)

  val mem :  key -> t -> bool
  val iter : (Key.t -> V.t -> unit) -> t -> unit

  val map : (V.t -> V.t) -> t -> t
  (** [map f m] returns the map obtained by composing the map [m] with the
      function [f]; that is, the map $k\mapsto f(m(k))$. *)

  val map': (Key.t -> V.t -> V.t option) -> t -> t
  (** Same as [map], except if [f k v] returns [None]. In this case, [k] is not
      bound in the resulting map. *)

  val fold : (Key.t -> V.t -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold f m seed] invokes [f k d accu], in turn, for each binding from
      key [k] to datum [d] in the map [m]. Keys are presented to [f] in
      increasing order according to the map's ordering. The initial value of
      [accu] is [seed]; then, at each new call, its value is the value
      returned by the previous invocation of [f]. The value returned by
      [fold] is the final value of [accu]. *)

  val fold_rev : (Key.t -> V.t -> 'b -> 'b) -> t -> 'b -> 'b
  (** [fold_rev] performs exactly the same job as [fold], but presents keys
      to [f] in the opposite order. *)

  val for_all: (Key.t -> V.t -> bool) -> t -> bool
  val exists: (Key.t -> V.t -> bool) -> t -> bool

  val generic_merge :
    cache:(string * bool) ->
    decide:(Key.t -> V.t option -> V.t option -> V.t) ->
    idempotent:bool ->
    t -> t -> t
  (** Merge of two trees, parameterized by a merge function. If [idempotent]
      holds, the function must verify [merge x x == x]. If [snd cache] is
      [true], an internal cache is used; thus the merge function must be pure.
  *)

  val symmetric_merge :
    cache:(string * 'a) ->
    decide_none:(Key.t -> V.t -> V.t) ->
    decide_some:(V.t -> V.t -> V.t) ->
    t -> t -> t
  (** Merge of two trees, parameterized by a merge function which is supposed
      to verify [merge x y == merge y x], [merge x x == x], and which must
      be pure (as an internal cache is used). *)

  val symmetric_inter :
    cache:(string * 'a) ->
    decide_some:(Key.t -> V.t -> V.t -> V.t option) ->
    t -> t -> t
  (** Intersection of two trees, parameterized by a function which must verify
      [inter x y == inter y x] and [inter x Empty == Empty]. If the intersection
      function returns [None], the key will not be in the resulting map.
      [decide_some] must be pure, as an internal cache is used). *)

  val inter_with_shape: 'a Shape(Key).t -> t -> t
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
    decide_fst:(Key.t -> V.t  -> unit) ->
    decide_snd:(Key.t -> V.t  -> unit) ->
    decide_both:(V.t -> V.t -> unit) ->
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

  (** Some functions of this module may optionally use internal caches. It is
      the responsibility of the use to choose whether or not to use a cache,
      and whether this cache will be garbage-collectable by OCaml. *)
  type cache_type =
    | NoCache (** The results of the function will not be cached. *)
    | PersistentCache of string
    (** The results of the function will be cached, and the function that uses
        the cache is a permanent closure (at the toplevel of an OCaml module).*)
    | TemporaryCache of string
    (** The results of the function will be cached, but the function itself
        is a local function which is garbage-collectable. *)

  val binary_predicate:
    cache_type ->
    predicate_type ->
    decide_fast:(t -> t -> predicate_result) ->
    decide_fst:(Key.t -> V.t  -> bool) ->
    decide_snd:(Key.t -> V.t  -> bool) ->
    decide_both:(Key.t -> V.t -> V.t -> bool) ->
    t -> t -> bool
  (** Same functionality as [generic_predicate] but with a different signature.
      All decisin functions return a boolean that are combined differently
      depending on whether the predicate is existential or universal. *)

  val generic_symmetric_predicate :
    exn -> 
    decide_fast:(t -> t -> decide_fast) ->
    decide_one:(Key.t -> V.t  -> unit) ->
    decide_both:(V.t -> V.t -> unit) ->
    t -> t -> unit
    (** Same as [generic_predicate], but for a symmetric relation. [decide_fst]
        and [decide_snd] are thus merged into [decide_one]. *)

  val symmetric_binary_predicate:
    cache_type ->
    predicate_type ->
    decide_fast:(t -> t -> predicate_result) ->
    decide_one:(Key.t -> V.t  -> bool) ->
    decide_both:(Key.t -> V.t -> V.t -> bool) ->
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
    f:(key -> V.t -> 'b) ->
    joiner:('b -> 'b -> 'b) ->
    empty:'b ->
    t -> 'b

  val cached_map :
    cache:string * int ->
    temporary:bool ->
    f:(key -> V.t -> V.t) ->
    t -> t      

  val singleton: key -> V.t -> t
  (** [singleton k d] returns a map whose only binding is from [k] to [d]. *)

  val is_singleton: t -> (key * V.t) option
  (** [is_singleton m] returns [Some (k, d)] if [m] is a singleton map
      that maps [k] to [d]. Otherwise, it returns [None]. *)

  val cardinal: t -> int
  (** [cardinal m] returns [m]'s cardinal, that is, the number of keys it
      binds, or, in other words, its domain's cardinal. *)

  val min_binding: t -> key * V.t
  val max_binding: t -> key * V.t

  val split: key -> t -> t * V.t option * t

  val compositional_bool: t -> bool
  (** Value of the compositional boolean associated to the tree, as computed
      by the {!Compositional_bool} argument of the functor. *)

  val clear_caches: unit -> unit
  (** Clear all the persistent caches used internally by the functions of this
      module. Those caches are not project-aware, so this function must be
      called at least each time a project switch occurs. *)

  val from_shape: (Key.t -> 'a -> V.t) -> 'a Shape(Key).t -> t
  (** Build an entire map from another map indexed by the same keys.
      More efficient than just performing successive {!add} the elements
      of the other map *)

  val shape: t -> V.t Shape(Key).t
  (** Export the map as a value suitable for functions {!inter_with_shape}
      and {!from_shape} *)

  (**/**) (* Undocumented. *)
  val hash_debug : t -> int
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

(** Default implementation for the [Compositional_bool] argument of the functor
  {!Make}. To be used when no interesting compositional bit can be computed. *)
module Comp_unused : sig 
  val e : bool
  val f : 'a -> 'b -> bool
  val compose : bool -> bool -> bool
  val default : bool
end

(*
Local Variables:
compile-command: "make -C .."
End:
*)
