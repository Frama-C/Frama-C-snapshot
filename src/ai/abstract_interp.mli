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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. 
    @plugin development guide *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

(** Raised by [cardinal_less_than] *)
exception Not_less_than

exception Is_not_included

(** Generic lattice.
    @plugin development guide *)
module type Lattice = sig

  exception Error_Top
  exception Error_Bottom

  include Datatype.S (** datatype of element of the lattice *)
  type widen_hint (** hints for the widening *)

  val join: t -> t -> t (** over-approximation of union *)
  val link: t -> t -> t (** under-approximation of union *)
  val meet: t -> t -> t (** under-approximation of intersection *)
  val narrow: t -> t -> t (** over-approximation of intersection *)
  val bottom: t (** the smallest *)
  val top: t  (** the largest *)

  val is_included: t -> t -> bool
  val is_included_exn: t -> t -> unit
  val intersects: t -> t -> bool

  val widen: widen_hint -> t -> t -> t
    (** [widen h t1 t2] is an over-approximation of [join t1 t2].
        Assumes [is_included t1 t2] *)

  val cardinal_zero_or_one: t -> bool

  val cardinal_less_than: t -> int -> int
    (** [cardinal_less_than t v ]
        @raise Not_less_than whenever the cardinal of [t] is higher than [v] *)

  val tag : t -> int

end

module type Lattice_With_Diff = sig

  include Lattice

  val diff : t -> t -> t
    (** [diff t1 t2] is an over-approximation of [t1-t2]. *)

  val diff_if_one : t -> t -> t
    (** [diff t1 t2] is an over-approximation of [t1-t2].
        @return t1 if [t2] is not a singleton. *)

  val fold_enum :
    split_non_enumerable:int -> (t -> 'a -> 'a) -> t -> 'a -> 'a
  val splitting_cardinal_less_than:
    split_non_enumerable:int -> t -> int -> int

  val pretty_debug : Format.formatter -> t -> unit

end

module type Lattice_Product = sig
  type t1
  type t2
  type tt = private Product of t1*t2 | Bottom
  include Lattice with type t = tt
  val inject : t1 -> t2 -> t
  val fst : t -> t1
  val snd : t -> t2
end

module type Lattice_Sum = sig
  type t1
  type t2
  type sum = private Top | Bottom | T1 of t1 | T2 of t2
  include Lattice with type t = sum
  val inject_t1 : t1 -> t
  val inject_t2 : t2 -> t
end

module type Lattice_Base = sig
  type l
  type tt = private Top | Bottom | Value of l
  include Lattice with type t = tt
  val project : t -> l
  val inject: l -> t
end

module type Lattice_Set = sig
  module O: Hptset.S
  type tt = private Set of O.t | Top
  include Lattice with type t = tt and type widen_hint = O.t
  val inject_singleton: O.elt -> t
  val inject: O.t -> t
  val empty: t
  val apply2: (O.elt -> O.elt -> O.elt) -> (t -> t -> t)
  val apply1: (O.elt -> O.elt) -> (t -> t)
  val fold: ( O.elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter: ( O.elt -> unit) -> t -> unit
  val project : t -> O.t
  val mem : O.elt -> t -> bool
end

module type LatValue = Datatype.S_with_collections

module Int : sig
  include My_bigint.S
  include LatValue with type t = My_bigint.t
  val pretty : Format.formatter -> t -> unit
  val fold : (t -> 'a -> 'a) -> inf:t -> sup:t -> step:t -> 'a -> 'a

end

module Make_Lattice_Base (V : LatValue) : Lattice_Base with type l = V.t

module Make_Pair (V:LatValue)(W:LatValue) : Datatype.S with type t = V.t * W.t

module Make_Lattice_Set (V : LatValue) : Lattice_Set with type O.elt=V.t

module Make_Hashconsed_Lattice_Set
  (V : Hptset.Id_Datatype)
  (O: Hptset.S with type elt = V.t)
  : Lattice_Set with type O.elt=V.t
(** See e.g. Base.ml and Locations.ml to see how this functor shoudl be 
    applied. *)

module LocationSetLattice : sig
  include Lattice_Set with type O.elt = Cil_types.location
  val currentloc_singleton : unit -> t
    val compare:t -> t -> int
end

module type Key = sig
  include Datatype.S
  val is_null : t -> bool
  val null : t
  val id : t -> int
end

module VarinfoSetLattice : Lattice_Set with type O.elt = Cil_types.varinfo

module type Collapse = sig val collapse : bool end

(** If [C.collapse] then [L1.bottom,_] = [_,L2.bottom] = [bottom] *)
module Make_Lattice_Product (L1:Lattice) (L2:Lattice) (C:Collapse):
  Lattice_Product with type t1 = L1.t and type t2 = L2.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
