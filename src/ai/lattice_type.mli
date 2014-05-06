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

(** Lattice signatures.
    @plugin developer guide *)

module type Join_Semi_Lattice = sig
  include Datatype.S (** datatype of element of the lattice *)

  val join: t -> t -> t (** over-approximation of union *)
  val is_included: t -> t -> bool (**is first argument included in the second?*)
  val join_and_is_included: t -> t -> (t * bool) (**Do both ops simultaneously*)
end

module type Bounded_Join_Semi_Lattice = sig
  include Join_Semi_Lattice;;
  val bottom: t (** smallest element *)
end

module type With_Top = sig
  type t
  val top: t  (** largest element *)
end

module type With_Error_Top = sig
  exception Error_Top
end

module type With_Error_Bottom = sig
  exception Error_Bottom
end

module type With_Errors = sig
  include With_Error_Top
  include With_Error_Bottom
end

module type With_Narrow = sig
  type t
  val narrow: t -> t -> t (** over-approximation of intersection *)
end

module type With_Under_Approximation = sig
  type t
  val link: t -> t -> t (** under-approximation of union *)
  val meet: t -> t -> t (** under-approximation of intersection *)
end

module type With_Intersects = sig
  type t
  val intersects: t -> t -> bool
end

module type With_Enumeration = sig
  type t
  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** Fold on the elements of the value one by one if possible. Raises
      {!Abstract_interp.Not_less_than} when there is an infinite number of
      elements to enumerate. *)

  val cardinal_less_than: t -> int -> int
(** Raises {!Abstract_interp.Not_less_than} whenever the cardinal of the
    given lattice is strictly higher than the given integer. *)
end

module type With_Diff = sig
  type t
  val diff : t -> t -> t
    (** [diff t1 t2] is an over-approximation of [t1-t2]. [t2] must
        be an under-approximation or exact. *)
end

module type With_Diff_One = sig
  type t
  val diff_if_one : t -> t -> t
    (** [diff_of_one t1 t2] is an over-approximation of [t1-t2].
        @return [t1] if [t2] is not a singleton. *)
end

module type With_Cardinal_One = sig
  type t
  val cardinal_zero_or_one: t -> bool
end

module type With_Widening = sig
  type t

  type widen_hint (** hints for the widening *)

  val widen: widen_hint -> t -> t -> t
    (** [widen h t1 t2] is an over-approximation of [join t1 t2].
        Assumes [is_included t1 t2] *)
end


(** {2 Common signatures} *)

(** Signature shared by some functors of module {!Abstract_interp}. *)
module type AI_Lattice_with_cardinal_one = sig
  include Bounded_Join_Semi_Lattice
  include With_Top with type t:= t
  include With_Widening with type t:= t
  include With_Cardinal_One with type t := t
  include With_Narrow with type t := t
  include With_Under_Approximation with type t := t
  include With_Intersects with type t := t
end

(** Lattice with over- and under-approximation of join and meet, and
    intersection and difference. *)
module type Full_Lattice = sig
  include Bounded_Join_Semi_Lattice
  include With_Top with type t := t
  include With_Narrow with type t := t
  include With_Under_Approximation with type t := t
  include With_Intersects with type t := t
  include With_Diff with type t := t
end

(** Most complete lattices: all operations plus widening, notion of cardinal
    (including enumeration) and difference. *)
module type Full_AI_Lattice_with_cardinality = sig
  include AI_Lattice_with_cardinal_one
  include With_Diff with type t := t
  include With_Diff_One with type t := t
  include With_Enumeration with type t := t
  include With_Error_Top
end


(** {2 Results of generic functors, in module {!Abstract_interp}. } *)

(** Generic signature for the base elements of a lattice *)
module type Lattice_Value = Datatype.S_with_collections

(** Signature for a product lattice in which [Bottom] is handled especially.
    (see {!Abstract_interp.Make_Lattice_Product}). *)
module type Lattice_Product = sig
  type t1
  type t2
  type t = private Product of t1*t2 | Bottom
  include AI_Lattice_with_cardinal_one with type t := t
  val inject : t1 -> t2 -> t
  val fst : t -> t1
  val snd : t -> t2
end

(** Signature for a product lattice
    (see {!Abstract_interp.Make_Lattice_UProduct}). *)
module type Lattice_UProduct = sig
  type t1
  type t2
  type tt = t1*t2
  include AI_Lattice_with_cardinal_one with type t = tt
end

(** Signature for a lattice over a sum type
    (see {!Abstract_interp.Make_Lattice_Sum}). *)
module type Lattice_Sum = sig
  type t1
  type t2
  type sum = private Top | Bottom | T1 of t1 | T2 of t2
  include AI_Lattice_with_cardinal_one with type t = sum
  val inject_t1 : t1 -> t
  val inject_t2 : t2 -> t
end

module type Lattice_Base = sig
  type l
  type t = private Top | Bottom | Value of l
  exception Error_Top
  exception Error_Bottom
  include AI_Lattice_with_cardinal_one with type t := t
  val project : t -> l
  val inject: l -> t
  val transform: (l -> l -> l) -> t -> t -> t
end

(** Signatures for a lattice over a set
    (see {!Abstract_interp.Make_Lattice_Set} or
    {!Abstract_interp.Make_Hashconsed_Lattice_Set}). *)
module type Lattice_Set_Generic = sig
  module O: sig type t type elt end
  exception Error_Top
  type t = private Set of O.t | Top
  include AI_Lattice_with_cardinal_one
  with type t := t
  and type widen_hint = O.t
  val inject_singleton: O.elt -> t
  val inject: O.t -> t
  val empty: t
  val apply2: (O.elt -> O.elt -> O.elt) -> (t -> t -> t)
  val apply1: (O.elt -> O.elt) -> (t -> t)
  val fold: ( O.elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: ( O.elt -> unit) -> t -> unit
  val exists: (O.elt -> bool) -> t -> bool
  val for_all: (O.elt -> bool) -> t -> bool
  val project : t -> O.t
  val mem : O.elt -> t -> bool
end


module type Lattice_Set = sig
  module O: Datatype.Set
  include Lattice_Set_Generic with module O := O
end

module type Lattice_Hashconsed_Set = sig
  module O: sig
    include FCSet.S_Basic_Compare
    include Datatype.S with type t := t
  end
  include Lattice_Set_Generic with module O := O
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
