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

(** @plugin development guide *)

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

module type Value = Datatype.S_with_collections

module type Arithmetic_Value = sig
  include Value
  val gt : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
  val lt : t -> t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val native_div : t -> t -> t
  val rem : t -> t -> t
  val pos_div : t -> t -> t
  val c_div : t -> t -> t
  val c_rem : t -> t -> t
  val cast: size:t -> signed:bool -> value:t -> t
  val abs : t -> t
  val zero : t
  val one : t
  val two : t
  val four : t
  val onethousand : t
  val minus_one : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val pgcd : t -> t -> t
  val ppcm : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val length : t -> t -> t (** b - a + 1 *)
  val of_int : int -> t
  val of_float : float -> t
  val of_int64 : Int64.t -> t
  val to_int : t -> int
  val to_float : t -> float
  val neg : t -> t
  val succ : t -> t
  val pred : t -> t
  val round_up_to_r : min:t -> r:t -> modu:t -> t
  val round_down_to_r : max:t -> r:t -> modu:t -> t
  val pos_rem : t -> t -> t
  val shift_left : t -> t -> t
  val shift_right : t -> t -> t
  val fold : (t -> 'a -> 'a) -> inf:t -> sup:t -> step:t -> 'a -> 'a
  val logand : t -> t -> t
  val logor : t -> t -> t
  val logxor : t -> t -> t
  val lognot : t -> t
  val power_two : int -> t
  val two_power : t -> t
  val extract_bits : start:t -> stop:t -> t -> t
end

module Int : sig
  include Arithmetic_Value with type t = My_bigint.big_int
  val small_nums : t array
  val zero : t
  val four : t
  val eight : t
  val thirtytwo : t
  val div : t -> t -> t

  val billion_one : t
  val tag : t -> int
  val log_shift_right : t -> t -> t
  val shift_right : t -> t -> t
  val shift_left : t -> t -> t

  val to_int : t -> int
  val of_int : int -> t
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
  val of_string : string -> t
  val to_string : t -> string
  val to_float : t -> float
  val of_float : 'a -> 'b
  val minus_one : t
  val pretty_debug : Format.formatter -> t -> unit
  val is_zero : t -> bool
  val is_one : t -> bool
  val pos_div : t -> t -> t
  val pos_rem : t -> t -> t
  val native_div :
    t -> t -> t
  val c_div : t -> t -> t
  val c_rem : t -> t -> t
  val power_two : int -> t
  val extract_bits :
    start:t ->
    stop:t -> t -> t
  val is_even : t -> bool
  val pgcd : t -> t -> t
  val ppcm : t -> t -> t
  val length : t -> t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val round_down_to_zero :
    t -> t -> t
  val round_up_to_r :
    min:t ->
    r:t -> modu:t -> t
  val round_down_to_r :
    max:t ->
    r:t -> modu:t -> t
  val fold :
    (t -> 'a -> 'a) ->
    inf:t ->
    sup:t -> step:t -> 'a -> 'a

end

module Make_Lattice_Base (V : Value) : Lattice_Base with type l = V.t

module Make_Pair (V:Value)(W:Value) : Datatype.S with type t = V.t * W.t

module Make_Lattice_Interval_Set (V:Arithmetic_Value) : sig
  type elt = Make_Pair(V)(V).t
  type tt = private Top | Set of elt list
  include Lattice with type t = tt
  val inject_one : size:V.t -> value:V.t -> t
  val inject_bounds : V.t -> V.t -> t
  val inject : elt list -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val splitting_cardinal_less_than :
    split_non_enumerable:int -> t -> int -> int
end

module Make_Lattice_Set (V : Value) : Lattice_Set with type O.elt=V.t

module Make_Hashconsed_Lattice_Set(V : Hptset.Id_Datatype)
  : Lattice_Set with type O.elt=V.t

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
