(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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
  (** [Top(min, max, rem, modulo)] represents the interval between
      [min] and [max], congruent to [rem] modulo [modulo]. A value of
      [None] for [min] (resp. [max]) represents -infinity
      (resp. +infinity). [modulo] is > 0, and [0 <= rem < modulo].

      Actual [Top] is thus represented by Top(None,None,Int.zero,Int.one) *)
  | Top of Int.t option * Int.t option * Int.t * Int.t

(** {2 General guidelines of this module}

  - Functions suffixed by [_int] expect arguments that are integers. Hence,
    they will fail on an ival with constructor [Float]. Conversely, [_float]
    suffixed functions expect float arguments: the constructor [Float], or
    the singleton set [ [| Int.zero |] ], that can be tested by {!is_zero}.

  - see the comment in {!Lattice_type} about over- and under-approximations,
    and exact operations.
*)



module Widen_Hints : sig
  include FCSet.S with type elt = Integer.t
  include Datatype.S with type t:=t

  val default_widen_hints: t
end

type size_widen_hint = Integer.t
type generic_widen_hint = Widen_Hints.t

include Datatype.S_with_collections with type t := t
include Lattice_type.Full_AI_Lattice_with_cardinality
  with type t := t
  and type widen_hint = size_widen_hint * generic_widen_hint

val is_bottom : t -> bool
val partially_overlaps : size:Integer.t -> t -> t -> bool

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

val min_int : t -> Integer.t option
(** A [None] result means the argument is unbounded.
    Raises [Error_Bottom] if the argument is bottom. *)
val max_int : t -> Integer.t option
(** A [None] result means the argument is unbounded.
    Raises [Error_Bottom] if the argument is bottom. *)
val min_max_r_mod :
  t -> Integer.t option * Integer.t option * Integer.t * Integer.t

val min_and_max :
  t -> Integer.t option * Integer.t option

val min_and_max_float : t -> (Fval.F.t * Fval.F.t) option * bool
(** returns the bounds of the float interval, (or None if the argument is
    exactly NaN), and a boolean indicating the possibility that the value
    may be NaN. *)


val bitwise_and : size:int -> signed:bool -> t -> t -> t
val bitwise_or : t -> t -> t
val bitwise_xor : t -> t -> t
val bitwise_not: t -> t

val bitwise_not_size: size:int -> signed:bool -> t -> t
(** bitwise negation on a finite integer type. The argument is assumed to
    fit within the type. *)

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

val float_zeros : t
(** The lattice element containing exactly -0. and 0. *)

val is_zero : t -> bool
val is_one : t -> bool

val contains_zero : t -> bool
(** contains the zero value (including -0. for floating-point ranges) *)
val contains_non_zero : t -> bool

val top_float : t
val top_single_precision_float : t

val project_float : t -> Fval.t
(** Should be used only when we know it is a float *)


(** Building Ival *)

val inject_singleton : Integer.t -> t

val inject_float : Fval.t -> t
val inject_float_interval : float -> float -> t

val inject_range : Integer.t option -> Integer.t option -> t
(** [None] means unbounded. The interval is inclusive. *)

val inject_interval:
  min: Integer.t option -> max: Integer.t option ->
  rem: Integer.t -> modu: Integer.t ->
  t
(** Builds the set of integers between [min] and [max] included and congruent
    to [rem] modulo [modulo]. For [min] and [max], [None] is the corresponding
    infinity. Checks that [modu] > 0 and 0 <= [rest] < [modu], and fails
    otherwise. *)


(** Cardinality *)

val cardinal_zero_or_one : t -> bool
val is_singleton_int : t -> bool

exception Not_Singleton_Int

val project_int : t -> Integer.t
    (** @raise Not_Singleton_Int when the cardinal of the argument is not 1,
        or if it is not an integer. *)

val cardinal: t -> Integer.t option
(** [cardinal v] returns [n] if [v] has finite cardinal [n], or [None] if
    the cardinal is not finite. *)

val cardinal_estimate: t -> size:Integer.t -> Integer.t
(** [cardinal_estimate v ~size] returns an estimation of the cardinal
    of [v], knowing that [v] fits in [size] bits. *)

val cardinal_less_than : t -> int -> int
(** [cardinal_less_than t n] returns the cardinal of [t] is this cardinal
    is at most [n].
    @raise Abstract_interp.Not_less_than is the cardinal of [t]
    is more than [n] *)

val cardinal_is_less_than: t -> int -> bool
(** Same than cardinal_less_than but just return if it is the case. *)

val fold_int : (Integer.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on the integer values of the ival in increasing order.
    Raise {!Abstract_interp.Error_Top} if the argument is a float or a
    potentially infinite integer. *)

val fold_int_decrease : (Integer.t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on the integer values of the ival in decreasing order.
    Raise {!Abstract_Interp.Error_Top} if the argument is a float or a
    potentially infinite integer. *)

val fold_enum : (t -> 'a -> 'a) -> t -> 'a -> 'a
(** Iterate on every value of the ival. Raise {!Abstract_intrep.Error_Top} if
    the argument is a non-singleton float or a potentially infinite integer. *)

val fold_int_bounds: (t -> 'a -> 'a) -> t -> 'a -> 'a
(** Given [i] an integer abstraction [min..max], [fold_int_bounds f i acc]
    tries to apply [f] to [min], [max], and [i'] successively, where [i']
    is [i] from which [min] and [max] have been removed. If [min] and/or
    [max] are infinite, [f] is called with an argument [i'] unreduced
    in the corresponding direction(s). *)

val apply_set: (Integer.t -> Integer.t -> Integer.t ) -> t -> t -> t
val apply_set_unary: (Integer.t -> Integer.t ) -> t -> t


(** Subdivisions into two intervals *)
val subdivide: size:Integer.t -> t -> t * t


(** [has_greater_min_bound i1 i2] returns 1 if the interval [i1] has a better
    minimum bound (i.e. greater) than the interval [i2]. [i1] and [i2] should
    not be bottom. *)
val has_greater_min_bound : t -> t -> int

(** [has_smaller_max_bound i1 i2] returns 1 if the interval [i1] has a better
    maximum bound (i.e. lower) than the interval [i2]. [i1] and [i2] should not
    be bottom. *)
val has_smaller_max_bound : t -> t -> int

val scale : Integer.t -> t -> t
(** [scale f v] returns the interval of elements [x * f] for [x] in [v].
    The operation is exact, except when [v] is a float. *)

val scale_div : pos:bool -> Integer.t -> t -> t
(** [scale_div ~pos:false f v] is an over-approximation of the set of
    elements [x / f] for [x] in [v].

    [scale_div ~pos:true f v] is an over-approximation of the set of
    elements [x pos_div f] for [x] in [v]. *)

val scale_div_under : pos:bool -> Integer.t -> t -> t
(** [scale_div_under ~pos:false f v] is an under-approximation of the
    set of elements [x / f] for [x] in [v].

    [scale_div_under ~pos:true f v] is an under-approximation of the
    set of elements [x pos_div f] for [x] in [v]. *)

val div : t -> t -> t (** Integer division *)
val scale_rem : pos:bool -> Integer.t -> t -> t
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

(** [all_values ~size v] returns true iff v contains all integer values
    representable in [size] bits. *)
val all_values: size:Integer.t -> t -> bool

val backward_mult_int_left: right:t -> result:t -> t option Bottom.or_bottom

val backward_comp_int_left : Comp.t -> t -> t -> t
(** [backward_comp_int op l r] reduces [l] into [l'] so that
    [l' op r] holds. [l] is assumed to be an integer *)

val backward_comp_float_left_true : Comp.t -> Fval.kind -> t -> t -> t
val backward_comp_float_left_false : Comp.t -> Fval.kind -> t -> t -> t
(** Same as {!backward_comp_int_left}, except that the arguments should now
    be floating-point values. *)

val forward_comp_int: Comp.t -> t -> t -> Comp.result

val scale_int_base : Int_Base.t -> t -> t


val of_int : int -> t
val of_int64 : int64 -> t

(** Casts *)

val cast_int_to_int : size:Integer.t -> signed:bool -> t -> t

val cast_int_to_float : Fval.kind -> t -> t

val cast_float_to_int :
    signed:bool -> size:int -> t -> (** NaN *) alarm * (** Overflow, in each direction *) (alarm * alarm) * t

val cast_float_to_float : Fval.kind -> t -> t
(** Cast the given float to the given size. *)

val cast_float_to_int_inverse:
  single_precision:bool -> t (** integer *) -> t (** floating-point *)
val cast_int_to_float_inverse:
  single_precision:bool -> t (** floating-point *) -> t (** integer *)


val reinterpret_as_float: Cil_types.fkind -> t -> t
(** Bitwise reinterpretation of the given value as a float of the
    given kind. *)

val reinterpret_as_int: size:Integer.t -> signed:bool -> t -> t
(** Bitwise reinterpretation of the given value, of size [size], as an integer
    of the given signedness (and size). *)


val pretty_debug : Format.formatter -> t -> unit

val get_small_cardinal: unit -> int
(** Value of option -ilevel *)

(**/**)

(* This is used by the Value plugin. Do not use. *)
val set_small_cardinal: int -> unit

val rehash: t -> t (* Low-level operation for demarshalling *)
(**/**)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
