(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(** Arithmetic lattices.
    The interfaces of this module may change between
    Frama-C versions. Contact us if you need stable APIs. 
    @plugin development guide *)

module F : sig
  type t
  val of_float : float -> t
  val to_float : t -> float
  exception Nan_or_infinite
  val equal : t -> t -> bool
  val pretty :  Format.formatter -> t -> unit
  val pretty_normal :  use_hex:bool -> Format.formatter -> t -> unit
end

exception Can_not_subdiv

module Float_abstract : sig
  type t
  exception Nan_or_infinite
  exception Bottom
  type rounding_mode = Any | Nearest_Even

    (** [inject] creates an abstract float interval.
        Does not handle infinites.
        Does not enlarge subnormals to handle flush-to-zero modes *)
  val inject : F.t -> F.t -> t

    (** [inject_r] creates an abstract float interval.
        It handles infinites and flush-to-zero.
        the returned boolean is true if there was reduction *)
  val inject_r : F.t -> F.t -> bool * t

  val inject_singleton : F.t -> t
  val min_and_max_float : t -> F.t * F.t
  val top : t
  val add_float : rounding_mode -> t -> t -> bool * t
  val sub_float : rounding_mode -> t -> t -> bool * t
  val mult_float : rounding_mode -> t -> t -> bool * t
  val div_float : rounding_mode -> t -> t -> bool * t
  val contains_zero : t -> bool
  val compare : t -> t -> int
  val pretty : Format.formatter -> t -> unit
  val hash : t -> int
  val zero : t
  val is_zero : t -> bool
    (*    val rounding_inject : F.t -> F.t -> t *)
  val is_included : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t

  val contains_a_zero : t -> bool
  val is_singleton : t -> bool
  val neg_float : t -> t
  val sqrt_float : rounding_mode -> t -> bool * t
  val minus_one_one : t
  val cos_float : t -> t
  val cos_float_precise : t -> t
  val sin_float : t -> t
  val sin_float_precise : t -> t
  val exp_float : t -> t
  val widen : t -> t -> t
  val equal_float_ieee : t -> t -> bool * bool
  val maybe_le_ieee_float : t -> t -> bool
  val maybe_lt_ieee_float : t -> t -> bool
  val diff : t -> t -> t
  val filter_le : bool -> typ_loc:Cil_types.typ -> t -> t -> t
  val filter_ge : bool -> typ_loc:Cil_types.typ -> t -> t -> t
  val filter_lt : bool -> typ_loc:Cil_types.typ -> t -> t -> t
  val filter_gt : bool -> typ_loc:Cil_types.typ -> t -> t -> t
end


module O : sig
  type elt = Abstract_interp.Int.t
  type t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val split : elt -> t -> t * bool * t
end

type tt = private
  | Set of Abstract_interp.Int.t array
  | Float of Float_abstract.t
  | Top of Abstract_interp.Int.t option * Abstract_interp.Int.t option *
      Abstract_interp.Int.t * Abstract_interp.Int.t

module Widen_Hints : sig
  include SetWithNearest.S with type elt = Integer.t
  val default_widen_hints: t
end

exception Error_Top
exception Error_Bottom

include Datatype.S with type t = tt
type widen_hint = Widen_Hints.t
val join : t -> t -> t
val link : t -> t -> t
val meet : t -> t -> t
val narrow : t -> t -> t
val bottom : t
val top : t
val is_bottom : t -> bool
val is_included : t -> t -> bool
val is_included_exn : t -> t -> unit
val intersects : t -> t -> bool
val partially_overlaps : Abstract_interp.Int.t -> t -> t -> bool
val widen : widen_hint -> t -> t -> t
val fold_enum : split_non_enumerable:int -> (t -> 'a -> 'a) -> t -> 'a -> 'a
val diff : t -> t -> t
val diff_if_one : t -> t -> t
val add_int : t -> t -> t
val neg : t -> t
val sub : t -> t -> t

val min_int : t -> Abstract_interp.Int.t option
(** A [None] result means the argument is unbounded. *)
val max_int : t -> Abstract_interp.Int.t option
(** A [None] result means the argument is unbounded. *)
val min_max_r_mod : t ->
  Abstract_interp.Int.t option * Abstract_interp.Int.t option *
      Abstract_interp.Int.t * Abstract_interp.Int.t

val min_and_max :
  t -> Abstract_interp.Int.t option * Abstract_interp.Int.t option
val bitwise_and : size:int -> signed:bool -> t -> t -> t
val bitwise_or : size:int -> t -> t -> t
val bitwise_xor : t -> t -> t

val min_and_max_float : t -> F.t * F.t
val inject_range :
  Abstract_interp.Int.t option -> Abstract_interp.Int.t option -> t
  (** [None] means unbounded. The interval is inclusive. *)

val cardinal_zero_or_one : t -> bool
val is_singleton_int : t -> bool
val inject_singleton : Abstract_interp.Int.t -> t
val zero : t
val one : t
val compare_min_float : t -> t -> int
val compare_max_float : t -> t -> int
val compare_min_int : t -> t -> int
val compare_max_int : t -> t -> int
val is_zero : t -> bool
val is_one : t -> bool
val inject_float : Float_abstract.t -> t
val inject_float_interval : float -> float -> t
val top_float : t
val top_single_precision_float : t
val project_float : t -> Float_abstract.t
    (** @raise F.Nan_or_infinite when the float is Nan or infinite. *)
val force_float: Cil_types.fkind -> t -> bool * t
val in_interval :
  Abstract_interp.Int.t ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t -> Abstract_interp.Int.t -> bool
val contains_zero : t -> bool
exception Not_Singleton_Int
val project_int : t -> Abstract_interp.Int.t
    (** @raise Not_Singleton_Int when the cardinal of the argument is not 1. *)
val cardinal_less_than : t -> int -> int
(** [cardinal_less_than t n] returns the cardinal of [t] is this cardinal
    is at most [n].
    @raise Abstract_interp.Not_less_than is the cardinal of [t]
    is more than [n] *)

val splitting_cardinal_less_than: split_non_enumerable:int -> t -> int -> int

val inject_top :
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t -> Abstract_interp.Int.t -> t
(** [inject_top min max r m] returns the smallest lattice element that
    contains all integers equal to [r] modulo [m] between [min] and
    [max]. [None] means unbounded. *)

val fold : (Abstract_interp.Int.t -> 'a -> 'a) -> t -> 'a -> 'a

val apply_set :
  (Abstract_interp.Int.t -> Abstract_interp.Int.t -> Abstract_interp.Int.t ) ->
  t -> t -> t

val apply_set_unary :
  'a -> (Abstract_interp.Int.t -> Abstract_interp.Int.t ) -> t -> t

val singleton_zero : t
(** The lattice element that contains only the integer zero. *)
val singleton_one : t
(** The lattice element that contains only the integer one. *)
val zero_or_one : t
(** The lattice element that contains only the integers zero and one. *)
val contains_non_zero : t -> bool
val subdiv_float_interval : size:int -> t -> t * t
val subdiv : size:int -> t -> t * t
val scale : Abstract_interp.Int.t -> t -> t
val scale_div : pos:bool -> Abstract_interp.Int.t -> t -> t
val negative : t
val div : t -> t -> t
val scale_rem : pos:bool -> Abstract_interp.Int.t -> t -> t
val c_rem : t -> t -> t
val mul : t -> t -> t
val shift_left : size:Abstract_interp.Int.t option -> t -> t -> t
val shift_right : size:Abstract_interp.Int.t option -> t -> t -> t
val interp_boolean : contains_zero:bool -> contains_non_zero:bool -> t

val set_of_array : Abstract_interp.Int.t array -> O.t

(** Extract bits from [start] to [stop] from the given Ival, [start]
    and [stop] being included. [size]  is the size of the entire ival. *)
val extract_bits :
  start:Abstract_interp.Int.t -> stop:Abstract_interp.Int.t ->
  size:Abstract_interp.Int.t -> t -> t
val create_all_values :
  modu:Abstract_interp.Int.t -> signed:bool -> size:int -> t
val all_values : size:Abstract_interp.Int.t -> t -> bool
val filter_le_int : Abstract_interp.Int.t option -> t -> t
val filter_ge_int : Abstract_interp.Int.t option -> t -> t
val filter_lt_int : Abstract_interp.Int.t option -> t -> t
val filter_gt_int : Abstract_interp.Int.t option -> t -> t
val filter_le : t -> t -> t
val filter_ge : t -> t -> t
val filter_lt : t -> t -> t
val filter_gt : t -> t -> t
val filter_le_float : bool -> typ_loc:Cil_types.typ -> t -> t -> t
val filter_ge_float : bool -> typ_loc:Cil_types.typ -> t -> t -> t
val filter_lt_float : bool -> typ_loc:Cil_types.typ -> t -> t -> t
val filter_gt_float : bool -> typ_loc:Cil_types.typ -> t -> t -> t
val compare_C :
  (Abstract_interp.Int.t option ->
   Abstract_interp.Int.t option ->
   Abstract_interp.Int.t option -> Abstract_interp.Int.t option -> 'a) ->
  t -> t -> 'a
val max_max :
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t option -> Abstract_interp.Int.t option
val scale_int64base : Int_Base.t -> t -> t
val cast_float_to_int :
    signed:bool -> size:int -> t -> (** Top *) bool * (** Overflow *) bool * t
val cast_float_to_int_inverse : single_precision:bool -> tt -> tt
val of_int : int -> t
val of_int64 : int64 -> t
val cast_int_to_float : Float_abstract.rounding_mode -> t -> bool * t
val cast : size:Abstract_interp.Int.t -> signed:bool -> value:t -> t
val cast_float : rounding_mode:Float_abstract.rounding_mode -> t -> bool * t
val cast_double : t -> bool * t
val pretty_debug : Format.formatter -> t -> unit

val get_small_cardinal: unit -> int

(**/**) (* This is automatically set by the Value plugin. Do not use. *)
val set_small_cardinal: int -> unit
(**/**)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
