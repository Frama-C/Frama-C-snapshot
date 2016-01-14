(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
    Frama-C versions. Contact us if you need stable APIs. *)

open Abstract_interp

type t = private
  | Set of Int.t array
  | Float of Fval.t
  (** [Top(min, max, rest, modulo)] represents the interval between
      [min] and [max], congruent to [rest] modulo [modulo]. A value of
      [None] for [min] (resp. [max]) represents -infinity
      (resp. +infinity). [modulo] is > 0, and [0 <= rest < modulo].

      Actual [Top] is thus represented by Top(None,None,Int.zero,Int.one) *)
  | Top of Int.t option * Int.t option * Int.t * Int.t

(** {2 General guidelines of this module}

  - Functions suffixed by [_int] expect arguments that are integers. Hence,
    they will fail on an ival with constructor [Float]. Conversely, [_float]
    suffixed functions expect float arguments: the constructor [Float], or
    the singleton set [ [| Int.zero |] ], that can be tested by {!is_zero}.
    The function {!force_float} forces a bit-level conversion from the integer
    representation to the floating-point one.

  - see the comment in {!Lattice_type} about over- and under-approximations,
    and exact operations.
*)



module Widen_Hints : sig
  include FCSet.S with type elt = Integer.t
  include Datatype.S with type t:=t

  val default_widen_hints: t

  (* max_int, max_long, max_long_long *)
  val hints_for_signed_int_types: unit -> t
end

exception Error_Bottom

include Datatype.S_with_collections with type t := t
include Lattice_type.Full_AI_Lattice_with_cardinality
  with type t := t
  and type widen_hint = Widen_Hints.t

val is_bottom : t -> bool
val partially_overlaps : size:Abstract_interp.Int.t -> t -> t -> bool

val add_int : t -> t -> t
(** Addition of two integer (ie. not [Float]) ivals. *)
val add_int_under : t -> t -> t
(** Underapproximation of the same operation *)
val add_singleton_int: Integer.t -> t -> t
(** Addition of an integer ival with an integer. Exact operation. *)

val neg_int : t -> t
(** Negation of an integer ival. Exact operation. *)
val sub_int : t -> t -> t
val sub_int_under: t -> t -> t

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
val bitwise_or : t -> t -> t
val bitwise_xor : t -> t -> t

val min_and_max_float : t -> Fval.F.t * Fval.F.t


val zero : t
(** The lattice element that contains only the integer 0. *)
val one : t
(** The lattice element that contains only the integer 1. *)
val minus_one : t
(** The lattice element that contains only the integer -1. *)

val zero_or_one : t
(** The lattice element that contains only the integers 0 and 1. *)

val positive_integers : t
(** The lattice element that contains exactly the positive or null integers *)

val negative_integers : t
(** The lattice element that contains exactly the negative or null integers *)


val is_zero : t -> bool
val is_one : t -> bool

val contains_zero : t -> bool
val contains_non_zero : t -> bool

val top_float : t
val top_single_precision_float : t

exception Nan_or_infinite

val project_float : t -> Fval.t
(** @raise Nan_or_infinite when the float may be NaN or infinite. *)

val force_float: Cil_types.fkind -> t -> bool * t
  (** Reinterpret the given value as a float of the given kind. If the
      returned boolean is [true], some of the values may not be representable
      as finite floats. *)


val in_interval :
  Abstract_interp.Int.t ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t -> Abstract_interp.Int.t -> bool


(** Building Ival *)

val inject_singleton : Abstract_interp.Int.t -> t

val inject_float : Fval.t -> t
val inject_float_interval : float -> float -> t

val inject_range :
  Abstract_interp.Int.t option -> Abstract_interp.Int.t option -> t
  (** [None] means unbounded. The interval is inclusive. *)

val inject_top :
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t -> Abstract_interp.Int.t -> t
(** [inject_top min max r m] checks [min], [max], [r] and [m] for consistency
    as arguments of the [Top] constructor
    and returns the lattice element of integers equal to [r] modulo [m] 
    between [min] and [max] (which may be a Set if there are few of these).
    For [min] and [max], [None] means unbounded. *)


(** Cardinality *)

val cardinal_zero_or_one : t -> bool
val is_singleton_int : t -> bool

exception Not_Singleton_Int

val project_int : t -> Abstract_interp.Int.t
    (** @raise Not_Singleton_Int when the cardinal of the argument is not 1,
        or if it is not an integer. *)

val cardinal: t -> Integer.t option
(** [cardinal v] returns [n] if [v] has finite cardinal [n], or [None] if
    the cardinal is not finite. *)

val cardinal_estimate: t -> Integer.t -> Integer.t
(** [cardinal_estimate v size] returns an estimation of the cardinal
    of [v], knowing that [v] fits in [size] bits. *)

val cardinal_less_than : t -> int -> int
(** [cardinal_less_than t n] returns the cardinal of [t] is this cardinal
    is at most [n].
    @raise Abstract_interp.Not_less_than is the cardinal of [t]
    is more than [n] *)

val cardinal_is_less_than: t -> int -> bool
(** Same than cardinal_less_than but just return if it is the case. *)

val fold_int : (Abstract_interp.Int.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on the integer values of the ival in increasing order.
    Raise {!Error_Top} if the argument is a float or a potentially
    infinite integer. *)

val fold_int_decrease : (Abstract_interp.Int.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on the integer values of the ival in decreasing order.
    Raise {!Error_Top} if the argument is a float or a potentially
    infinite integer. *)

val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on every value of the ival. Raise {!Error_Top} if the argument is a
    non-singleton float or a potentially infinite integer. *)

val fold_split : split:int -> (t -> 'a -> 'a) -> t -> 'a -> 'a

val apply_set :
  (Abstract_interp.Int.t -> Abstract_interp.Int.t -> Abstract_interp.Int.t ) ->
  t -> t -> t

val apply_set_unary :
  'a -> (Abstract_interp.Int.t -> Abstract_interp.Int.t ) -> t -> t


val subdiv_float_interval : size:int -> t -> t * t
val subdiv_int: t -> t * t


(** [compare_min_float m1 m2] returns 1 if the float interval [m1] has a
   better min bound (i.e. greater) than the float interval [m2]. *)
val compare_min_float : t -> t -> int
(** [compare_max_float m1 m2] returns 1 if the float interval [m1] has a
   better max bound (i.e. lower) than the float interval [m2]. *)
val compare_max_float : t -> t -> int
(** [compare_min_int m1 m2] returns 1 if the int interval [m1] has a
   better min bound (i.e. greater) than the int interval [m2]. *)
val compare_min_int : t -> t -> int
(** [compare_max_int m1 m2] returns 1 if the int interval [m1] has a
   better max bound (i.e. lower) than the int interval [m2]. *)
val compare_max_int : t -> t -> int

val scale : Abstract_interp.Int.t -> t -> t
(** [scale f v] returns the interval of elements [x * f] for [x] in [v].
    The operation is exact, except when [v] is a float. *)

val scale_div : pos:bool -> Abstract_interp.Int.t -> t -> t
(** [scale_div ~pos:false f v] is an over-approximation of the set of
    elements [x / f] for [x] in [v].

    [scale_div ~pos:true f v] is an over-approximation of the set of
    elements [x pos_div f] for [x] in [v]. *)

val scale_div_under : pos:bool -> Abstract_interp.Int.t -> t -> t
(** [scale_div_under ~pos:false f v] is an under-approximation of the
    set of elements [x / f] for [x] in [v].

    [scale_div_under ~pos:true f v] is an under-approximation of the
    set of elements [x pos_div f] for [x] in [v]. *)

val div : t -> t -> t (** Integer division *)
val scale_rem : pos:bool -> Abstract_interp.Int.t -> t -> t
(** [scale_rem ~pos:false f v] is an over-approximation of the set of
    elements [x mod f] for [x] in [v].

    [scale_rem ~pos:true f v] is an over-approximation of the set of
    elements [x pos_rem f] for [x] in [v]. *)

val c_rem : t -> t -> t
val mul : t -> t -> t
val shift_left:  t -> t -> t
val shift_right: t -> t -> t
val interp_boolean : contains_zero:bool -> contains_non_zero:bool -> t

(** Extract bits from [start] to [stop] from the given Ival, [start]
    and [stop] being included. [size]  is the size of the entire ival. *)
val extract_bits: start:Integer.t -> stop:Integer.t -> size:Integer.t -> t -> t
val create_all_values_modu: modu:Integer.t -> signed:bool -> size:int -> t
val create_all_values: signed:bool -> size:int -> t
val all_values: size:Integer.t -> t -> bool

val filter_le_ge_lt_gt_int : Cil_types.binop -> t -> t -> t
(** [filter_le_ge_lt_gt_int op i1 i2] reduces [i1] into [i1'] so that
    [i1' op i2] holds. [i1] is assumed to be an integer *)

val filter_le_ge_lt_gt_float :
  Cil_types.binop -> bool -> Fval.float_kind -> t -> t -> t
(** Same as [Fval.filter_le_ge_lt_gt], except that the arguments
    are now of type {!t}. The first argument must be a floating-point value.
*)

(** In the results of [min_int] and [max_int], [None] represents the
corresponding infinity. [compare_max_min ma mi] compares [ma] to [mi],
interpreting [None] for [ma] as +infinity and [None] for [mi] as
-infinity. *)
val compare_max_min : Integer.t option -> Integer.t option -> int
(** In the results of [min_int] and [max_int], [None] represents the
corresponding infinity. [compare_min_max mi ma] compares [ma] to [ma],
interpreting [None] for [ma] as +infinity and [None] for [mi] as
-infinity. *)
val compare_min_max : Integer.t option -> Integer.t option -> int
val compare_C :
  (Integer.t option ->
   Integer.t option ->
   Integer.t option -> Integer.t option -> 'a) ->
  t -> t -> 'a
val max_max :
  Integer.t option ->
  Integer.t option -> Integer.t option
val scale_int_base : Int_Base.t -> t -> t
val cast_float_to_int :
    signed:bool -> size:int -> t -> (** non-finite *) bool * (** Overflow, in each direction *) (bool * bool) * t
val cast_float_to_int_inverse : single_precision:bool -> t -> t
val of_int : int -> t
val of_int64 : int64 -> t
val cast_int_to_float : Fval.rounding_mode -> t -> bool * t
val cast : size:Integer.t -> signed:bool -> value:t -> t
val cast_float : rounding_mode:Fval.rounding_mode -> t -> bool * t
val cast_double : t -> bool * t
val pretty_debug : Format.formatter -> t -> unit

val get_small_cardinal: unit -> int
(** Value of of option -ilevel *)

(**/**) (* This is automatically set by the Value plugin. Do not use. *)
val set_small_cardinal: int -> unit
(**/**)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
