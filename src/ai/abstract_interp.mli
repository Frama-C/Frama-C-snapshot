(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(** Raised by [cardinal_less_than] *)
exception Not_less_than

exception Is_not_included

(** Generic lattice *)
module type Lattice = sig
  exception Error_Top
  exception Error_Bottom
  type t (** type of element of the lattice *)
  type widen_hint (** hints for the widening *)

(*  val compare : t -> t -> int
    (** Does not need to be compatible with [is_included].
	Must be a total ordering function *)
*)
  val equal: t -> t -> bool
  val join: t -> t -> t (** over-approximation of union *)
  val link: t -> t -> t (** under-approximation of union *)
  val meet: t -> t -> t (** under-approximation of intersection *)
  val narrow: t -> t -> t (** over-approximation of intersection *)
  val bottom: t (** the smallest *)
  val top: t  (** the largest *)

  val is_included: t -> t -> bool
  val is_included_exn: t -> t -> unit
  val intersects: t -> t -> bool
  val pretty: Format.formatter -> t -> unit

  val widen: widen_hint -> t -> t -> t
    (** [widen h t1 t2] is an over-approximation of [join t1 t2].
        Assumes [is_included t1 t2] *)

  val cardinal_zero_or_one: t -> bool

  val cardinal_less_than: t -> int -> int
    (** [cardinal_less_than t v ]
	@raise Not_less_than whenever the cardinal of [t] is higher than [v] *)

  val tag : t -> int

  module Datatype: Project.Datatype.OUTPUT with type t = t
end

module type Lattice_With_Diff = sig
  include Lattice

  val diff : t -> t -> t
    (** [diff t1 t2] is an over-approximation of [t1-t2]. *)

  val diff_if_one : t -> t -> t
    (** [diff t1 t2] is an over-approximation of [t1-t2].
	@return t1 if [t2] is not a singleton. *)

  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
  val hash : t -> int
  val pretty_debug : Format.formatter -> t -> unit
  val name : string
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
  module O: Set.S
  type tt = private Set of O.t | Top
  include Lattice with type t = tt and type widen_hint = O.t
  val hash : t -> int
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

module type Value = sig
  type t
  val pretty: Format.formatter -> t -> unit
  val compare : t -> t -> int
  val hash: t -> int
  module Datatype: Project.Datatype.OUTPUT with type t = t
end

module type Arithmetic_Value = sig
  include Value
  val gt : t -> t -> bool
  val le : t -> t -> bool
  val ge : t -> t -> bool
  val lt : t -> t -> bool
  val eq : t -> t -> bool
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val native_div : t -> t -> t
  val rem : t -> t -> t
  val pos_div : t -> t -> t
  val c_div : t -> t -> t
  val c_rem : t -> t -> t
  val abs : t -> t
  val zero : t
  val one : t
  val two : t
  val four : t
  val minus_one : t
  val is_zero : t -> bool
  val is_one : t -> bool
  val equal : t -> t -> bool
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
  val extract_bits : start:t -> stop:t -> t -> t
end

module type Card = sig
  type t
  val n : t
end


module type Float_Abstract_Sig =
sig
  type t
  type integer
  exception Nan_or_infinite
  exception Bottom
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val hash : t -> int
  val widen : t -> t -> t
  val is_singleton : t -> bool
  val is_zero : t -> bool
  val contains_zero : t -> bool
  val zero : t
  val meet : t -> t -> t
  val join : t -> t -> t
  val top : t
  val is_included : t -> t -> bool
  val diff : t -> t -> t
  val filter_le : t -> t -> t
  val filter_ge : t -> t -> t
  val filter_lt : t -> t -> t
  val filter_gt : t -> t -> t
end


module Int : sig
  include Arithmetic_Value with type t = My_bigint.big_int
  val pretty_s : unit -> t -> string
  val neq : t -> t -> bool
  val to_int64 : t -> int64
  val zero : t

  val div : t -> t -> t

  val billion_one : t
  val hash : t -> int
  val tag : t -> int
  val equal : t -> t -> bool
  val log_shift_right : t -> t -> t
  val shift_right : t -> t -> t
  val shift_left : t -> t -> t

  val to_int : t -> int
  val of_int : int -> t
  val of_int64 : int64 -> t
  val of_string : string -> t
  val to_string : t -> string
  val to_float : t -> float
  val of_float : 'a -> 'b
  val minus_one : t
  val pretty : Format.formatter -> t -> unit
  val pretty_debug : Format.formatter -> t -> unit
  val is_zero : t -> bool
  val compare : t -> t -> int
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

module Make_Lattice_Base (V : Value) : (Lattice_Base with type l = V.t)

module Make_Lattice_Mod
  (V:Arithmetic_Value)
  (CARD:Card with type t = int)
  (F:Float_Abstract_Sig with type integer = V.t):
sig
  module O : Set.S with type elt = V.t

  type tt = 
      Set of O.t
    | Float of F.t 
    | Top of V.t option * V.t option * V.t * V.t

  module Widen_Hints : sig 
    module V : Arithmetic_Value with type t = V.t
    include SetWithNearest.S with type elt = V.t
    val default_widen_hints : t
  end

  include Lattice with type t = tt and type widen_hint = Widen_Hints.t

  val hash : t -> int
  val equal : t -> t -> bool
  val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
  val diff : t -> t -> t
  val diff_if_one : t -> t -> t
  val add : t -> t -> t
  val neg : t -> t
  val sub : t -> t -> t
  val min_int : t -> V.t option
  val max_int : t -> V.t option
  val min_and_max : t -> V.t option * V.t option
  val bitwise_and : size:int -> t -> t -> t
  val bitwise_or : size:int -> t -> t -> t
    
  val inject_range : V.t option -> V.t option -> t
    (** the interval is inclusive. *)

  val all_positives : V.t option -> bool
  val all_negatives : V.t option -> bool
  val cardinal_zero_or_one : t -> bool
  val is_singleton_int : t -> bool
  val inject_singleton : V.t -> t
  val zero : t
  val one : t
  val is_zero : t -> bool
  val is_one : t -> bool

  val inject_float : F.t -> t
  val top_float : t

  val project_float : t -> F.t 
    (** @raise F.Nan_or_infinite when the float is Nan or infinite. *)

  val in_interval :
    V.t -> V.t option -> V.t option -> V.t -> V.t -> bool
  val contains_zero : t -> bool

  exception Not_Singleton_Int
  val project_int : t -> V.t
    (** @raise Not_Singleton_Int when the cardinal is not 1. *)


  val cardinal_less_than : t -> int -> int
  val inject_top : V.t option -> V.t option -> V.t -> V.t -> t
  val inject_set : O.t -> t
  val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a


  exception Apply_Set_Exn of exn
  val apply_set :
    string -> (V.t -> V.t -> V.t) -> t -> t -> t
  val apply_set_unary : 'a -> (V.t -> V.t) -> t -> t

  val singleton_zero : t
  val singleton_one : t
  val zero_or_one : t
  val contains_non_zero : t -> bool

  val scale : V.t -> t -> t
  val scale_div : pos:bool -> V.t -> t -> t

  val negative : t
  val div : t -> t -> t

  val scale_rem : pos:bool -> V.t -> t -> t

  val cast : size:V.t -> signed:bool -> value:t -> t

(*  val cast_int_to_float : t -> t
  val cast_float_to_int : t -> t
*)		       
  val c_rem : t -> t -> t
  val mul : t -> t -> t
  val shift_left : size:V.t -> t -> t -> t
  val shift_right : size:V.t -> t -> t -> t
  val interp_boolean :
    contains_zero:bool -> contains_non_zero:bool -> t
  val filter_set : (int -> bool) -> V.t -> O.t -> t
  val extract_bits : start:V.t -> stop:V.t -> t -> t
  val create_all_values : modu:V.t -> size:int -> t
  val all_values : size:V.t -> t -> bool

  (*TODO To be hidden *)
  val filter_le_int : V.t option -> t -> t
  val filter_ge_int : V.t option -> t -> t
  val filter_lt_int : V.t option -> t -> t
  val filter_gt_int : V.t option -> t -> t
  val filter_le : t -> t -> t
  val filter_ge : t -> t -> t
  val filter_lt : t -> t -> t
  val filter_gt : t -> t -> t

  val filter_le_float : t -> t -> t
  val filter_ge_float : t -> t -> t
  val filter_lt_float : t -> t -> t
  val filter_gt_float : t -> t -> t

  val compare_C :
    (V.t option ->
       V.t option -> V.t option -> V.t option -> 'a) -> t -> t -> 'a
  val max_max :  V.t option -> V.t option -> V.t option
end

module Make_Pair (V:Value)(W:Value) :
  sig
    type t = V.t * W.t
    val compare : t -> t -> int
    val pretty : Format.formatter -> t -> unit
  end

module Make_Lattice_Interval_Set (V:Arithmetic_Value) : 
sig 
  type elt = Make_Pair(V)(V).t
  type tt = private Top | Set of elt list
  include Lattice with type t = tt
  val hash : t -> int
  val inject_one : size:V.t -> value:V.t -> t
  val inject_bounds : V.t -> V.t -> t
  val inject : elt list -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make_Lattice_Set (V : Value) : (Lattice_Set with type O.elt=V.t)

module LocationSetLattice : 
sig include Lattice_Set with type O.elt = Cil_types.location
    val currentloc_singleton : unit -> t
end

module type Key =
sig
  type t
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val is_null : t -> bool
  val null : t
  val hash : t -> int
  val id : t -> int
  module Datatype : Project.Datatype.OUTPUT with type t = t
end

module VarinfoSetLattice : Lattice_Set with type O.elt = Cil_types.varinfo

module type Collapse = sig
  val collapse : bool
end
  
(** If [C.collapse] then [L1.bottom,_] = [_,L2.bottom] = [bottom] *)
module Make_Lattice_Product (L1:Lattice) (L2:Lattice) (C:Collapse):
  Lattice_Product with type t1 = L1.t and type t2 = L2.t
