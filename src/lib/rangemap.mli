(**************************************************************************)
(*                                                                        *)
(*  This file was originally part of Objective Caml                       *)
(*                                                                        *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*  Copyright (C) 1996 INRIA                                              *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  This file is distributed under the terms of the GNU Library General   *)
(*  Public License version 2, with the special exception on linking       *)
(*  described below. See the GNU Library General Public License version   *)
(*  2 for more details (enclosed in the file licenses/LGPLv2).            *)
(*                                                                        *)
(*  As a special exception to the GNU Library General Public License,     *)
(*  you may link, statically or dynamically, a "work that uses the        *)
(*  Library" with a publicly distributed version of the Library to        *)
(*  produce an executable file containing portions of the Library, and    *)
(*  distribute that executable file under terms of your choice, without   *)
(*  any of the additional requirements listed in clause 6 of the GNU      *)
(*  Library General Public License.  By "a publicly distributed version   *)
(*  of the Library", we mean either the unmodified Library as             *)
(*  distributed by INRIA, or a modified version of the Library that is    *)
(*  distributed under the conditions defined in clause 2 of the GNU       *)
(*  Library General Public License.  This exception does not however      *)
(*  invalidate any other reasons why the executable file might be         *)
(*  covered by the GNU Library General Public License.                    *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Association tables over ordered types.

    This module implements applicative association tables, also known as
    finite maps or dictionaries, given a total ordering function
    over the keys.

    All operations over maps are purely applicative (no side-effects).
    The implementation uses balanced binary trees, and therefore searching
    and insertion take time logarithmic in the size of the map.

    Compared to Ocaml's standard libary, this implementation caches at
    each node the hash of the tree (which is computed in an associative
    manner), and contains some functions not yet present in the caml
    implementation.

    @plugin development guide *)

module type S = sig
  type key (** The type of the map keys. *)
  type value

  type rangemap
  (** The type of maps from type [key] to type [value]. *)

  include Datatype.S with type t = rangemap

  val create :  t -> key -> value -> t -> t

  val empty: t
  (** The empty map. *)

  val is_empty: t -> bool
  (** Test whether a map is empty or not. *)

  val add: key -> value -> t -> t
  (** [add x y m] returns a map containing the same bindings as [m], plus a
      binding of [x] to [y]. If [x] was already bound in [m], its previous
      binding disappears. *)

  val singleton: key -> value -> t
    (** [singleton x y] returns the one-element map that contains a binding [y]
        for [x]. *)

  val find: key -> t -> value
  (** [find x m] returns the current binding of [x] in [m],
      or raises [Not_found] if no such binding exists. *)

  val remove: key -> t -> t
  (** [remove x m] returns a map containing the same bindings as [m], except
      for [x] which is unbound in the returned map. *)

  val mem: key -> t -> bool
  (** [mem x m] returns [true] if [m] contains a binding for [x],
      and [false] otherwise. *)

  val iter: (key -> value -> unit) -> t -> unit
  (** [iter f m] applies [f] to all bindings in map [m].
      [f] receives the key as first argument, and the associated value
      as second argument.  The bindings are passed to [f] in increasing
      order with respect to the ordering over the type of the keys.
      Only current bindings are presented to [f]:
      bindings hidden by more recent bindings are not passed to [f]. *)

  val map: (value -> value) -> t -> t
  (** [map f m] returns a map with same domain as [m], where the
      associated value [a] of all bindings of [m] has been
      replaced by the result of the application of [f] to [a].
      The bindings are passed to [f] in increasing order
      with respect to the ordering over the type of the keys. *)

  val mapi: (key -> value -> value) -> t -> t
  (** Same as {!Map.S.map}, but the function receives as arguments both the
      key and the associated value for each binding of the map. *)

  val mapii: (key -> value -> key*value) -> t -> t
  (** Same as {!Map.S.mapi}, but the function also returns a new key.
      the modification applied on the keys must be compatible
      with the order on the keys. *)

  val fold: (key -> value -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
      where [k1 ... kN] are the keys of all bindings in [m]
      (in increasing order), and [d1 ... dN] are the associated data. *)

  val for_all: (key -> value -> bool) -> t -> bool
    (** [for_all p m] checks if all the bindings of the map satisfy
        the predicate [p]. *)

  val exists: (key -> value -> bool) -> t -> bool
    (** [exists p m] checks if at least one binding of the map
        satisfy the predicate [p].  *)

  val filter: (key -> value -> bool) -> t -> t
    (** [filter p m] returns the map with all the bindings in [m]
        that satisfy predicate [p]. *)

  val partition: (key -> value -> bool) -> t -> t * t
    (** [partition p m] returns a pair of maps [(m1, m2)], where
        [m1] contains all the bindings of [s] that satisfy the
        predicate [p], and [m2] is the map with all the bindings of
        [s] that do not satisfy [p]. *)


  val cardinal: t -> int
    (** Return the number of bindings of a map. *)

  val bindings: t -> (key * value) list
    (** Return the list of all bindings of the given map.
        The returned list is sorted in increasing order with respect
        to the ordering on keys *)

  val min_binding: t -> (key * value)
    (** Return the smallest binding of the given map (with respect to the
        [Ord.compare] ordering), or raise [Not_found] if the map is empty. *)

  val max_binding: t -> (key * value)
    (** Same as {!Map.S.min_binding}, but returns the largest binding
        of the given map.  *)

  val choose: t -> (key * value)
    (** Return one binding of the given map, or raise [Not_found] if
        the map is empty. Which binding is chosen is unspecified,
        but equal bindings will be chosen for equal maps. *)


  val merge: (key -> value option -> value option -> value option) -> t -> t -> t
    (** [merge f m1 m2] computes a map whose keys is a subset of keys of [m1]
        and of [m2]. The presence of each such binding, and the corresponding
        value, is determined with the function [f].  *)

  val for_all2: (key -> value option -> value option -> bool) -> t -> t -> bool
    (** [for_all2 f m1 m2] returns true if and only if [f k v1 v2] holds
        for each [k] present in either [m1] and [m2], [v_i] being
        [Some (find k m_i)] if [k] is in [m_i], and [None] otherwise
        (for [i=1] or [i=2]) *)

  val exists2: (key -> value option -> value option -> bool) -> t -> t -> bool
    (** [exists2 f m1 m2] returns true if and only there exists
        [k] present in [m1] or [m2] such that [f k v1 v2] holds,
        [v_i] being [Some (find k m_i)] if [k] is in [m_i], and [None]
          otherwise (for [i=1] or [i=2]) *)

  val iter2: (key -> value option -> value option -> unit) -> t -> t -> unit
    (** [iter2 f m1 m2] computes [f k v1 v2] for each [k] present in either
        [m1] or [m2] (the [k] being presented in ascending order), [v_i] being
        [Some (find k m_i)] if [k] is in [m_i], and [None] otherwise
        (for [i=1] or [i=2]) *)

  val fold2: (key -> value option -> value option -> 'a -> 'a) -> t -> t -> 'a -> 'a
    (** [fold2 f m1 m2 v] computes [(f k_N v1_N v2_N... (f k_1 v1_1 v2_1 a)...)]
        where [k_1 ... k_N] are all the keys of all the bindings in either
        [m1] or [m2] (in increasing order), [vi_j] being [Some (find k_j m_i)]
        if [k_j] is in [m_i], and [None] otherwise (for [i=1] or [i=2]) *)
end


type fuzzy_order = Above | Below | Match

(** Datatype with a function that approximately equality in a constant-time
    way. *)
module type Value = sig
  include Datatype.S

  (** [fast_equal] is used to reduce memory allocation in some cases. It is
      valid to always return [false]; the only constraint is that [true] must
      not be returned if [equal] returns [false]. *)
  val fast_equal: t -> t -> bool
end

(** Extension of the above signature, with specific functions acting
    on range of values *)
module Make (Ord : Datatype.S) (Value : Value): sig

  include S with type key = Ord.t and type value = Value.t


  val fold_range: (key -> fuzzy_order) ->
    (key -> Value.t -> 'a -> 'a) -> t -> 'a -> 'a

  val height: t -> int

  val concerned_intervals:
    (key -> key -> fuzzy_order) ->
    key -> t -> (key*Value.t) list
    (** Intervals that match the given key. The resulting list is sorted in
        decreasing order. *)

   exception Empty_rangemap
   val lowest_binding : t -> key * Value.t
   exception No_such_binding
   val lowest_binding_above : (key -> bool) -> t -> key * Value.t
   val add_whole : (key -> key -> fuzzy_order) -> key -> Value.t -> t -> t
   val remove_whole : (key -> key -> fuzzy_order) -> key -> t -> t

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
