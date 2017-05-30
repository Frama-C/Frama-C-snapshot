(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Signature for {!Equality} module, that implements equalities
    over ordered types *)

type 'a trivial = Trivial | NonTrivial of 'a

type 'a tree = Empty | Leaf of 'a | Node of 'a tree * 'a tree

(** Representation of an equality between a set of elements.
    The signatures is roughly a subset of Ocaml's [Set.S].
    An equality always contains at least two elements; operations that break
    this invariant return `Void. *)
module type S = sig
  include Datatype.S

  type elt (** The type of the equality elements. *)

  val pair : elt -> elt -> t trivial
  (** The equality between two elements. *)

  val mem: elt -> t -> bool
  (** [mem x s] tests whether [x] belongs to the equality [s]. *)

  val add: elt -> t -> t
  (** [add x s] returns the equality between all elements of [s] and [x]. *)

  val remove: elt -> t -> t trivial
  (** [remove x s] returns the equality between all elements of [s], except [x]. *)

  val union: t -> t -> t
  (** Union. *)

  val inter: t -> t -> t trivial
  (** Intersection. *)

  val intersects : t -> t -> bool
  (** [intersect s s'] = true iff the two equalities both involve the same
      element. *)

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool

  val iter: (elt -> unit) -> t -> unit
  (** [iter f s] applies [f] in turn to all elements of [s]. *)

  val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
      where [x1 ... xN] are the elements of [s], in increasing order. *)

  val for_all: (elt -> bool) -> t -> bool
  (** [for_all p s] checks if all elements of the equality
      satisfy the predicate [p]. *)

  val exists: (elt -> bool) -> t -> bool
  (** [exists p s] checks if at least one element of the equality
      satisfies the predicate [p]. *)

  val filter: (elt -> bool) -> t -> t trivial
  (** [filter p s] returns the equality between all elements in [s]
      that satisfy predicate [p]. *)

  val cardinal: t -> int
  (** Return the number of elements of the equality. *)

  val choose: t -> elt
  (** Return the representative of the equality. *)

end


(** Sets of equalities. *)
module type Set = sig

  type element (** The type of the equality elements. *)
  type equality (** The type of the equalities. *)

  include Datatype.S

  (**
     The set operators are redefined so that equalities involving a same term
     are joined: ∀ e₁, e₂ ∈ Set, e₁ ≠ e₂ ⇔ e₁ ∩ e₂ = ∅
  *)

  val empty: t
  val is_empty: t -> bool
  val add: equality -> t -> t
  val singleton: equality -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val iter: (equality -> unit) -> t -> unit
  val fold: (equality -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all: (equality -> bool) -> t -> bool
  val exists: (equality -> bool) -> t -> bool
  val elements: t -> equality list
  val choose : t -> equality

  (** [remove elt set] remove any occurrence of [elt] in [set]. *)
  val remove : element -> t -> t

  (** [unite a b map] unites [a] and [b] in [map]. *)
  val unite : element -> element -> t -> t

  (** [find elt set] return the (single) equality involving [elt]
      that belongs to [set], or raise Not_found if no such equality exists. *)
  val find : element -> t -> equality

  (** Same as [find], but return None in the last case. *)
  val find_option : element -> t -> equality option

  (** [mem equality set] = true iff ∃ eq ∈ set, equality ⊆ eq *)
  val mem : equality -> t -> bool

  (** [contains elt set] = true iff [elt] belongs to an equality of [set]. *)
  val contains : element -> t -> bool

  val deep_fold : (equality -> element -> 'a -> 'a) -> t -> 'a -> 'a

  val cardinal : t -> int

  val elements_only_left: t -> t -> element tree
end


module type S_with_collections = sig
  include S
  module Set : Set with type element = elt and type equality = t
end
