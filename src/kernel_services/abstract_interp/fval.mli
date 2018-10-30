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

(** Floating-point intervals, used to construct arithmetic lattices.
    The interfaces of this module may change between
    Frama-C versions. Contact us if you need stable APIs. *)

type kind = Float_sig.prec =
  | Single (** 32 bits float (a.k.a 'float' C type) *)
  | Double (** 64 bits float (a.k.a 'double' C type) *)
  | Long_Double
  | Real    (** set of real *)

val pretty_kind: Format.formatter -> kind -> unit

module F : sig
  type t
  val packed_descr : Structural_descr.pack

  val of_float : float -> t (** fails on NaNs, but allows infinities. *)
  val to_float : t -> float

  val compare : t -> t -> int
  val equal : t -> t -> bool
  (** Those functions distinguish -0. and +0. *)

  val pretty :  Format.formatter -> t -> unit
  val pretty_normal :  use_hex:bool -> Format.formatter -> t -> unit

  val plus_zero : t

  val is_finite : t -> bool
  (** Returns true if the float is a finite number. *)

  val next_float : kind -> float -> float
  (** First double strictly above the argument. Must be called on non-NaN
      floats. Returns +infty on MAX_FLT. Infinities are left unchanged. *)

  val prev_float : kind -> float -> float
  (** First double strictly below the argument. Must be called on non-NaN
      floats. Returns -infty on -MAX_FLT. Infinities are left unchanged. *)
end

include Float_interval_sig.S with type float := F.t

val round_to_single_precision_float : t -> t

(** [inject] creates an abstract float interval. It handles
    infinities, flush-to-zero (rounding subnormals if needed) and NaN.
    Inputs must be compatible with [float_kind]. Raises no exceptions
    (unless values are not compatible with [float_kind], in which case
    execution is aborted). The two floating point numbers must be
    ordered (so not NaN). [~nan] indicates if NaN is present. *)
val inject : ?nan:bool -> kind -> F.t -> F.t -> t

val inject_singleton : F.t -> t

val top : t

val plus_zero : t
val minus_zero : t
val zeros: t (** Both positive and negative zero *)

val pi: t (** Real representation of \pi. *)
val e: t  (** Real representation of \e. *)

val contains_plus_zero : t -> bool

val meet: t -> t -> t Bottom.or_bottom

val is_singleton : t -> bool
(** Returns [true] on NaN. We expect this function to be e.g. to perform
    subdivisions/enumerations. The size of the concretization is less
    interesting to us. (And it is also possible to consider that there is
    only one NaN value in the concrete anyway.) *)

exception Not_Singleton_Float

val project_float: t -> F.t
(** @raise Not_Singleton_Float when the interval is not a single float
    (NaN is one such case). *)

val subdiv_float_interval : kind -> t -> t * t
(** Raise {!Can_not_subdiv} if it can't be subdivided *)

(** Discussion regarding [kind] and the 3 functions below.

    Support for fesetround(FE_UPWARD) and fesetround(FE_DOWNWARD) seems to
    be especially poor, including in not-so-old versions of Glibc
    (https://sourceware.org/bugzilla/show_bug.cgi?id=3976). The code for
    {!exp}, {!log} and {!log10} is correct wrt. [kind=Reak] ONLY
    if the C implementation of these functions is correct in directed
    rounding modes. Otherwise, anything could happen, including crashes. For
    now, unless the Libc is known to be reliable, these functions should be
    called with [rounding_mode=Nearest_Even] only.
    Also note that there the Glibc does not guarantee that
    f(FE_DOWNWARD) <= f(FE_TONEAREST) <= f(FE_UPWARD), which implies that
    using different rounding modes to bound the final result does not ensure
    correct bounds. Here's an example where it does not hold (glibc 2.21):
    log10f(3, FE_TONEAREST) < log10f(3, FE_DOWNWARD).

    Also, we have observed bugs in [powf], which is called when [kind=Float32].
 *)

val exp : kind -> t -> t
val log: kind -> t -> t
val log10: kind -> t -> t

val floor: kind -> t -> t
val ceil: kind -> t -> t
val trunc: kind -> t -> t
val fround: kind -> t -> t

val backward_cast_float_to_double: t -> t Bottom.or_bottom
(** [backward_cast_float_to_double d] return all possible float32 [f] such that
    [(double)f = d]. The double of [d] that have no float32 equivalent are
    discarded. *)

val backward_cast_double_to_real: t -> t

val kind: Cil_types.fkind -> kind
(** Classify a [Cil_types.fkind] as either a 32 or 64 floating-point type.
    Long double are over approximated by Reals *)
