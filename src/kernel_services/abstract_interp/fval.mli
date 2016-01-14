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

(** Floating-point intervals, used to construct arithmetic lattices.
    The interfaces of this module may change between
    Frama-C versions. Contact us if you need stable APIs. *)

open Abstract_interp

module F : sig
  type t
  val packed_descr : Structural_descr.pack

  val of_float : float -> t (** fails on NaNs, but allows infinites. *)
  val to_float : t -> float

  val compare : t -> t -> int
  val equal : t -> t -> bool
  (** Those functions distinguish -0. and +0. *)

  val pretty :  Format.formatter -> t -> unit
  val pretty_normal :  use_hex:bool -> Format.formatter -> t -> unit

  val zero : t

  val next_float : float -> float
  (** First double strictly above the argument. Must be called on non-NaN
      floats. Returns +infty on MAX_FLT. Infinites are left unchanged. *)

  val prev_float : float -> float
  (** First double strictly below the argument. Must be called on non-NaN
      floats. Returns -infty on -MAX_FLT. Infinites are left unchanged. *)
end

type t
val packed_descr : Structural_descr.pack


(** [Non_finite] is produced when the result of a computation is
    entirely not-finite, such as [+oo,+oo] (results in [Bottom]). *)
exception Non_finite

type rounding_mode = Any | Nearest_Even

val top_single_precision_float : t

val round_to_single_precision_float : rounding_mode:rounding_mode -> t -> bool * t

val bits_of_float64 : signed:bool -> t -> Int.t * Int.t
val bits_of_float32 : signed:bool -> t -> Int.t * Int.t

(** [has_finite f] returns true iff [f] contains at least one finite
    element. *)
val has_finite : t -> bool

(** Floating-point builtins may produce three kinds of alarms:
    - [APosInf] when the result may contain +oo;
    - [ANegInf] when the result may contain -oo;
    - [ANaN msg] when the result is NaN; an explanation of why the argument
      is invalid is given.
    - [AAssume msg] is a variant of ANaN for debugging purposes, mostly for
      internal use. Ignored when printing alarms.
    Builtins may sometimes raise [Non_finite] when no part of the result
    is finite. *)
type builtin_alarm = APosInf | ANegInf | ANaN of string | AAssume of string
module Builtin_alarms : (Set.S with type elt = builtin_alarm)

type builtin_res = Builtin_alarms.t * t Bot.or_bottom
(** Builtins return structured alarms, in the guise of a set of string
    explaining the problem. *)

type float_kind =
  | Float32 (** 32 bits float (a.k.a 'float' C type) *)
  | Float64 (** 64 bits float (a.k.a 'double' C type) *)

(** Equivalent to the [nextafter/nextafterf] functions in C. *)
val next_after : float_kind -> F.t -> F.t -> F.t

(** [inject] creates an abstract float interval.
    Does not handle infinites or NaN.
    Does not enlarge subnormals to handle flush-to-zero modes *)
val inject : F.t -> F.t -> t

(** [inject_r_f] creates an abstract float interval. It handles infinities and
    flush-to-zero (rounding subnormals if needed), but not NaN.
    The returned booleans are true if the lower/upper bound may be infinite.
    May raise {!Non_finite} when no part of the result would be finite.
    Inputs must be compatible with [float_kind]. *)
val inject_r_f : float_kind -> F.t -> F.t -> bool (*-inf*) * bool (*+inf*) * t

(** Alias for [inject_r_f Float64]. *)
val inject_r : F.t -> F.t -> bool (* not finite *) * t

(** Equivalent to [inject_r_f], but ignores the boolean [not_finite].
    The caller must emit appropriate warnings in the presence of
    non-finite values. *)
val inject_f : float_kind -> F.t -> F.t -> t

val inject_singleton : F.t -> t

val compare_min : t -> t -> int
val compare_max : t -> t -> int

val min_and_max : t -> F.t * F.t
val top : t
val add : rounding_mode -> t -> t -> bool * t
val sub : rounding_mode -> t -> t -> bool * t
val mul : rounding_mode -> t -> t -> bool * t
val div : rounding_mode -> t -> t -> bool * t
val is_a_zero : t -> bool
(** [is_a_zero f] returns true iff f ⊆ [-0.0,+0.0] *)

val fold_split : int -> (t -> 'a -> 'a) -> t -> 'a -> 'a
(** no splitting occurs if the integer argument is less than 2 *)

val contains_zero : t -> bool
val compare : t -> t -> int

val pretty : Format.formatter -> t -> unit
val pretty_overflow: Format.formatter -> t -> unit
(** This pretty-printer does not display -FLT_MAX and FLT_MAX as interval
    bounds. Instead, the specical notation [--.] is used. *)

val hash : t -> int
val zero : t
val is_zero : t -> bool
(*    val rounding_inject : F.t -> F.t -> t *)
val is_included : t -> t -> bool
val join : t -> t -> t
val meet : t -> t -> t Bot.or_bottom

val contains_a_zero : t -> bool
val is_singleton : t -> bool

val minus_one_one : t

val subdiv_float_interval : size:int -> t -> t * t

val neg : t -> t

val cos : t -> t
val cos_precise : t -> t
val sin : t -> t
val sin_precise : t -> t

val atan2: t -> t -> builtin_res
(** Returns atan2(y,x). Does not emit any alarms. *)

val pow: t -> t -> builtin_res
(** Returns pow(x,y). *)

val powf : t -> t -> builtin_res
(** Single-precision version of pow. *)

val fmod: t -> t -> builtin_res
(** Returns fmod(x,y). May return a "division by zero" alarm.
    Raises [Builtin_invalid_domain] if there is certainly a division by zero.
*)

val sqrt : rounding_mode -> t -> builtin_res

(** Discussion regarding -all-rounding-modes and the functions below.

    Support for fesetround(FE_UPWARD) and fesetround(FE_DOWNWARD) seems to
    be especially poor, including in not-so-old versions of Glibc
    (https://sourceware.org/bugzilla/show_bug.cgi?id=3976). The code for
    {!exp}, {!log} and {!log10} is correct wrt. -all-rounding-modes ONLY
    if the C implementation of these functions is correct in directed
    rounding modes. Otherwise, anything could happen, including crashes. For
    now, unless the Libc is known to be reliable, these functions should be
    called with [rounding_mode=Nearest_Even] only.
    Also note that there the Glibc does not guarantee that
    f(FE_DOWNWARD) <= f(FE_TONEAREST) <= f(FE_UPWARD), which implies that
    using different rounding modes to bound the final result does not ensure
    correct bounds. Here's an example where it does not hold (glibc 2.21):
    log10f(3, FE_TONEAREST) < log10f(3, FE_DOWNWARD). *)

val exp : rounding_mode -> t -> Builtin_alarms.t * t
val log: rounding_mode -> t -> builtin_res
val log10: rounding_mode -> t -> builtin_res
(** All three functions may raise {!Non_finite}.
    Can only be called to approximate a computation on double (float64). *)

val floor: rounding_mode -> t -> Builtin_alarms.t * t
val ceil: rounding_mode -> t -> Builtin_alarms.t * t
val trunc: rounding_mode -> t -> Builtin_alarms.t * t
val fround: rounding_mode -> t -> Builtin_alarms.t * t

val expf : rounding_mode -> t -> Builtin_alarms.t * t
val logf : rounding_mode -> t -> builtin_res
val log10f : rounding_mode -> t -> builtin_res
val sqrtf : rounding_mode -> t -> builtin_res
val floorf: rounding_mode -> t -> Builtin_alarms.t * t
val ceilf: rounding_mode -> t -> Builtin_alarms.t * t
val truncf: rounding_mode -> t -> Builtin_alarms.t * t
val froundf: rounding_mode -> t -> Builtin_alarms.t * t
(** Single-precision versions *)

val widen : t -> t -> t
val equal_float_ieee : t -> t -> bool * bool
val maybe_le_ieee_float : t -> t -> bool
val maybe_lt_ieee_float : t -> t -> bool
val diff : t -> t -> t Bot.or_bottom

val filter_le_ge_lt_gt :
  Cil_types.binop -> bool -> float_kind -> t -> t -> t Bot.or_bottom
(** [filter_le_ge_lt_gt op allroundingmodes fkind f1 f2] attemps to reduce
    [f1] into [f1'] so that the relation [f1' op f2] holds. [fkind] is
    the type of [f1] and [f1'] (not necessarily of [f2]). If
    [allroundingmodes] is set, all possible rounding modes are taken into
    acount. [op] must be [Le], [Ge], [Lt] or [Gt] *)
