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

(** Sets over ordered types.

    This signatures is a partial copy of the signature of OCaml's [Set.S],
    which we extend with new operations. *)

module type S_Basic_Compare =
  sig
    type elt
    (** The type of the set elements. *)

    type t
    (** The type of sets. *)

    val empty: t
    (** The empty set. *)

    val is_empty: t -> bool
    (** Test whether a set is empty or not. *)

    val mem: elt -> t -> bool
    (** [mem x s] tests whether [x] belongs to the set [s]. *)

    val add: elt -> t -> t
    (** [add x s] returns a set containing all elements of [s],
       plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

    val singleton: elt -> t
    (** [singleton x] returns the one-element set containing only [x]. *)

    val remove: elt -> t -> t
    (** [remove x s] returns a set containing all elements of [s],
       except [x]. If [x] was not in [s], [s] is returned unchanged. *)

    val union: t -> t -> t
    (** Set union. *)

    val inter: t -> t -> t
    (** Set intersection. *)

    (** Set difference. *)
    val diff: t -> t -> t

    val compare: t -> t -> int
    (** Total ordering between sets. Can be used as the ordering function
       for doing sets of sets. *)

    val equal: t -> t -> bool
    (** [equal s1 s2] tests whether the sets [s1] and [s2] are
       equal, that is, contain equal elements. *)

    val subset: t -> t -> bool
    (** [subset s1 s2] tests whether the set [s1] is a subset of
       the set [s2]. *)

    val iter: (elt -> unit) -> t -> unit
    (** [iter f s] applies [f] in turn to all elements of [s].
       The elements of [s] are presented to [f] in increasing order
       with respect to the ordering over the type of the elements. *)

    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
       where [x1 ... xN] are the elements of [s], in increasing order. *)

    val for_all: (elt -> bool) -> t -> bool
    (** [for_all p s] checks if all elements of the set
       satisfy the predicate [p]. *)

    val exists: (elt -> bool) -> t -> bool
    (** [exists p s] checks if at least one element of
       the set satisfies the predicate [p]. *)

    val filter: (elt -> bool) -> t -> t
    (** [filter p s] returns the set of all elements in [s]
       that satisfy predicate [p]. *)

    val partition: (elt -> bool) -> t -> t * t
    (** [partition p s] returns a pair of sets [(s1, s2)], where
       [s1] is the set of all the elements of [s] that satisfy the
       predicate [p], and [s2] is the set of all the elements of
       [s] that do not satisfy [p]. *)

    val cardinal: t -> int
    (** Return the number of elements of a set. *)

    val elements: t -> elt list
    (** Return the list of all elements of the given set.
       The returned list is sorted in increasing order with respect
       to the ordering [Ord.compare], where [Ord] is the argument
       given to {!Set.Make}. *)

    val choose: t -> elt
    (** Return one element of the given set, or raise [Not_found] if
       the set is empty. Which element is chosen is unspecified,
       but equal elements will be chosen for equal sets. *)

    val split: elt -> t -> t * bool * t
    (** [split x s] returns a triple [(l, present, r)], where
          [l] is the set of elements of [s] that are
          strictly less than [x];
          [r] is the set of elements of [s] that are
          strictly greater than [x];
          [present] is [false] if [s] contains no element equal to [x],
          or [true] if [s] contains an element equal to [x]. *)

    val find: elt -> t -> elt
    (** [find x s] returns the element of [s] equal to [x] (according
        to [Ord.compare]), or raise [Not_found] if no such element
        exists.
        @since 4.01.0 *)

    val of_list: elt list -> t
    (** [of_list l] creates a set from a list of elements.
        This is usually more efficient than folding [add] over the list,
        except perhaps for lists with many duplicated elements.
        @since 4.02.0 *)

  end
(** Standard operations on sets. This signature does not assume any
    particular property on the [compare] function used to compare elements,
    except that it implements a total order. These are the functions that
    make sense for an usage of [Set] where only the algorithmic complexity is
    interesting to the user. *)

module type S =
  sig
    include S_Basic_Compare

    val min_elt: t -> elt
    (** Return the smallest element of the given set
       (with respect to the [Ord.compare] ordering), or raise
       [Not_found] if the set is empty. *)

    val max_elt: t -> elt
    (** Same as {min_elt}, but returns the largest element of the
       given set. *)


    (* Frama-C- additions *)

    val nearest_elt_le: elt -> t -> elt
    (** [nearest_elt_le v s] returns the largest element of [s] that is
        smaller or equal to [v].
        @raise Not_found if no such element exists. *)

    val nearest_elt_ge: elt -> t -> elt
    (** [nearest_elt_ge v s] returns the smallest element of [s] that is
        bigger or equal to [v].
        @raise Not_found if no such element exists. *)

  end
(** Output signature of the functor {!FCSet.Make}.

    This signature add functions that assume that the [compare] function between
    elements implements a specific order. In this case, the layout of the
    tree might be interesting to the user.
*)

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t
(** Functor building an implementation of the set structure
   given a totally ordered type. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
