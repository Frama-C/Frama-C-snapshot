(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** Signature for the floating-point interval semantics. *)

open Bottom.Type

(** Precision of the intervals. *)
type prec = Float_sig.prec

module type S = sig
  type float (** Type of the interval bounds. *)
  type widen_hints (** Type of the widen hints. *)
  type t     (** Type of intervals. *)

  val packed_descr : Structural_descr.pack

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val pretty: Format.formatter -> t -> unit
  val hash: t -> int

  (** Returns the bounds of the float interval, (or None if the argument is
      exactly NaN), and a boolean indicating the possibility that the value
      may be NaN. *)
  val min_and_max: t -> (float * float) option * bool

  (** The NaN singleton *)
  val nan: t

  (** The infinities singleton *)
  val pos_infinity: prec -> t
  val neg_infinity: prec -> t

  (** [inject ~nan b e] creates the floating-point interval [b..e], plus NaN
      if [nan] is true. [b] and [e] must be ordered, and not NaN. They can be
      infinite. *)
  val inject: ?nan:bool -> float -> float -> t

  (** [singleton f] creates the singleton interval [f], without NaN. *)
  val singleton: float -> t

  (** Top interval for a given precision, including infinite and NaN values. *)
  val top: prec -> t

  (** The interval of all finite values in the given precision. *)
  val top_finite: prec -> t

  (** Lattice operators. *)

  val is_included: t -> t -> bool
  val join: t -> t -> t
  val widen: widen_hints -> prec -> t -> t -> t
  val narrow: t -> t -> t or_bottom

  val contains_a_zero: t -> bool
  val contains_pos_zero: t -> bool
  val contains_neg_zero: t -> bool
  val contains_non_zero: t -> bool
  val contains_pos_infinity: t -> bool
  val contains_neg_infinity: t -> bool
  val contains_nan: t -> bool

  (** Returns [true] on NaN. *)
  val is_singleton: t -> bool

  (** [is_negative f] returns [True] iff all values in [f] are negative;
      [False] iff all values are positive; and [Unknown] otherwise.
      Note that we do not keep sign information for NaN, so if [f] may contain
      NaN, the result is always [Unknown]. *)
  val is_negative: t -> Abstract_interp.Comp.result

  val is_finite: t -> Abstract_interp.Comp.result
  val is_not_nan: t -> Abstract_interp.Comp.result
  val backward_is_finite: positive:bool -> prec -> t -> t or_bottom
  val backward_is_nan: positive:bool -> t -> t or_bottom

  (** [has_greater_min_bound f1 f2] returns 1 if the interval [f1] has a better
      minimum bound (i.e. greater) than the interval [f2]. *)
  val has_greater_min_bound: t -> t -> int

  (** [has_smaller_max_bound f1 f2] returns 1 if the interval [f1] has a better
      maximum bound (i.e. lower) than the interval [f2]. *)
  val has_smaller_max_bound: t -> t -> int

  val forward_comp:
    Abstract_interp.Comp.t -> t -> t -> Abstract_interp.Comp.result

  (** [backward_comp_left_true op prec f1 f2] attempts to reduce
      [f1] into [f1'] so that the relation [f1' op f2] holds.
      [prec] is the precision of [f1] and [f1'], but not necessarily of [f2]. *)
  val backward_comp_left_true:
    Abstract_interp.Comp.t -> prec -> t -> t -> t Bottom.or_bottom

  (** [backward_comp_left_false op prec f1 f2] attempts to reduce
      [f1] into [f1'] so that the relation [f1' op f2] doesn't holds.
      [prec] is the precision of [f1] and [f1'], but not necessarily of [f2]. *)
  val backward_comp_left_false:
    Abstract_interp.Comp.t -> prec -> t -> t -> t or_bottom

  val neg: t -> t
  val abs: prec -> t -> t

  val add: prec -> t -> t -> t
  val sub: prec -> t -> t -> t
  val mul: prec -> t -> t -> t
  val div: prec -> t -> t -> t

  val floor: t -> t
  val ceil: t -> t
  val trunc: t -> t
  val fround: t -> t

  val exp: prec -> t -> t
  val log: prec -> t -> t
  val log10: prec -> t -> t
  val sqrt: prec -> t -> t

  val pow: prec -> t -> t -> t
  val fmod: prec -> t -> t -> t

  val cos: prec -> t -> t
  val sin: prec -> t -> t
  val atan2: prec -> t -> t -> t

  val backward_add:
    prec -> left:t -> right:t -> result:t ->
    (t * t) or_bottom

  val backward_sub:
    prec -> left:t -> right:t -> result:t ->
    (t * t) or_bottom

  val forward_cast: dst:prec -> t -> t
  val backward_cast: src:prec -> t -> t or_bottom
  val cast_int_to_float: prec -> Integer.t option -> Integer.t option -> t

  (** Bitwise reinterpretation of a floating-point interval of double precision
      into consecutive ranges of integers. *)
  val bits_of_float64_list : t -> (Integer.t * Integer.t) list

  (** Bitwise reinterpretation of a floating-point interval of single precision
      into consecutive ranges of integers. *)
  val bits_of_float32_list : t -> (Integer.t * Integer.t) list

  (** Subdivides an interval of a given precision into two intervals.
      Raises {!Abstract_interp.Can_not_subdiv} if it can't be subdivided.
      [prec] must be [Single] or [Double].  *)
  val subdivide: prec -> t -> t * t
end
