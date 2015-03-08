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
    Frama-C versions. Contact us if you need stable APIs. 
    @plugin development guide *)

open Abstract_interp

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

  (** [Nan_or_infinite] means that the operation may not be a finite float.
      (It does not mean that it is not a finite float). *)
  exception Nan_or_infinite
  exception Bottom
  type rounding_mode = Any | Nearest_Even

  type float_kind =
  | Float32 (** 32 bits float (a.k.a 'float' C type) *)
  | Float64 (** 64 bits float (a.k.a 'double' C type) *)

    (** [inject] creates an abstract float interval.
        Does not handle infinites or NaN.
        Does not enlarge subnormals to handle flush-to-zero modes *)
  val inject : F.t -> F.t -> t

  (** [inject_r] creates an abstract float interval. It handles infinites and
      flush-to-zero, but not NaN. The returned boolean is true if a bound was
      infinite. May raise {!Bottom} when no part of the result would be
      finite.  *)
  val inject_r : F.t -> F.t -> bool (* not finite *) * t

  val inject_singleton : F.t -> t
  val min_and_max_float : t -> F.t * F.t
  val top : t
  val add : rounding_mode -> t -> t -> bool * t
  val sub : rounding_mode -> t -> t -> bool * t
  val mul : rounding_mode -> t -> t -> bool * t
  val div : rounding_mode -> t -> t -> bool * t
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

  val minus_one_one : t

  val neg : t -> t

  val cos : t -> t
  val cos_precise : t -> t
  val sin : t -> t
  val sin_precise : t -> t

  val sqrt : rounding_mode -> t -> bool * t

  (** Discussion regarding -all-rounding-modes and the functions below.

     Support for fesetround(FE_UPWARD) and fesetround(FE_DOWNWARD) seems to
     be especially poor, including in not-so-old versions of Glibc
     (https://sourceware.org/bugzilla/show_bug.cgi?id=3976). The code for
     {!exp}, {!log} and {!log10} is correct wrt. -all-rounding-modes ONLY
     if the C implementation of these functions is correct in directed
     rounding modes. Otherwise, anything could happen, including crashes. For
     now, unless the Libc is known to be reliable, these functions should be
     called with [rounding_mode=Nearest_Even] only *)

  val exp : rounding_mode -> t -> bool * t
  val log: rounding_mode -> t -> bool * t
  val log10: rounding_mode -> t -> bool * t
  (** All three functions may raise {!Bottom}. Can only be called to approximate
      a computation on double (float64). *)

  val widen : t -> t -> t
  val equal_float_ieee : t -> t -> bool * bool
  val maybe_le_ieee_float : t -> t -> bool
  val maybe_lt_ieee_float : t -> t -> bool
  val diff : t -> t -> t

  val filter_le_ge_lt_gt : Cil_types.binop -> bool -> float_kind -> t -> t -> t
  (** [filter_le_ge_lt_gt op allroundingmodes fkind f1 f2] attemps to reduce
      [f1] into [f1'] so that the relation [f1' op f2] holds. [fkind] is
      the type of [f1] and [f1'] (not necessarily of [f2]). If
      [allroundingmodes] is set, all possible rounding modes are taken into
      acount. [op] must be [Le], [Ge], [Lt] or [Gt] *)
end


type tt = private
  | Set of Int.t array
  | Float of Float_abstract.t
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

include Datatype.S_with_collections with type t = tt
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

val min_and_max_float : t -> F.t * F.t


val zero : t
val one : t
val minus_one : t

val is_zero : t -> bool
val is_one : t -> bool

val top_float : t
val top_single_precision_float : t
val project_float : t -> Float_abstract.t
    (** @raise F.Nan_or_infinite when the float may be Nan or infinite. *)

val force_float: Cil_types.fkind -> t -> bool * t
  (** Reinterpret the given value as a float of the given kind. If the
      returned boolean is [true], some of the values may not be representable
      as finite floats. *)

val in_interval :
  Abstract_interp.Int.t ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t option ->
  Abstract_interp.Int.t -> Abstract_interp.Int.t -> bool
val contains_zero : t -> bool


(** Building Ival *)

val inject_singleton : Abstract_interp.Int.t -> t

val inject_float : Float_abstract.t -> t
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

val cardinal_less_than : t -> int -> int
(** [cardinal_less_than t n] returns the cardinal of [t] is this cardinal
    is at most [n].
    @raise Abstract_interp.Not_less_than is the cardinal of [t]
    is more than [n] *)

val fold_int : (Abstract_interp.Int.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on the integer values of the ival. Raise {!Error_Top} if the
    argument is a float or a potentially infinite integer. *)

val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on every value of the ival. Raise {!Error_Top} if the argument is a
    non-singleton float or a potentially infinite integer. *)

val fold_split : split:int -> (t -> 'a -> 'a) -> t -> 'a -> 'a

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


val negative : t
val div : t -> t -> t
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
val extract_bits :
  start:Abstract_interp.Int.t -> stop:Abstract_interp.Int.t ->
  size:Abstract_interp.Int.t -> t -> t
val create_all_values :
  modu:Abstract_interp.Int.t -> signed:bool -> size:int -> t
val all_values : size:Abstract_interp.Int.t -> t -> bool

val filter_le_ge_lt_gt_int : Cil_types.binop -> t -> t -> t
(** [filter_le_ge_lt_gt_int op i1 i2] reduces [i1] into [i1'] so that
    [i1' op i2] holds. [i1] is assumed to be an integer *)

val filter_le_ge_lt_gt_float :
  Cil_types.binop -> bool -> Float_abstract.float_kind -> t -> t -> t
(** Same as [Float_abstract.filter_le_ge_lt_gt], except that the arguments
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
val cast_float_to_int_inverse : single_precision:bool -> tt -> tt
val of_int : int -> t
val of_int64 : int64 -> t
val cast_int_to_float : Float_abstract.rounding_mode -> t -> bool * t
val cast : size:Integer.t -> signed:bool -> value:t -> t
val cast_float : rounding_mode:Float_abstract.rounding_mode -> t -> bool * t
val cast_double : t -> bool * t
val pretty_debug : Format.formatter -> t -> unit

val get_small_cardinal: unit -> int
(** Value of of option -ilevel *)

(**/**) (* This is automatically set by the Value plugin. Do not use. *)
val set_small_cardinal: int -> unit
(**/**)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
