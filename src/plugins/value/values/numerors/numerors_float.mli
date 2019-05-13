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

open Numerors_utils


(*-----------------------------------------------------------------------------
 *      Generic signature for a module representing float numbers
 *---------------------------------------------------------------------------*)
type t

val pretty : Format.formatter -> t -> unit

(** Returns a t element representing a positive infinite value *)
val pos_inf   : Precisions.t -> t

(** Returns a t element representing a negative infinite value *)
val neg_inf   : Precisions.t -> t

(** Returns a t element representing a positive zero value *)
val pos_zero  : Precisions.t -> t

(** Returns a t element representing a negative zero value *)
val neg_zero  : Precisions.t -> t

(** This function returns a float of precision ?prec containing the machine
    epsilon divided by two for the mandatory precision parameter. We divide it
    by two because we are only interested in the rounding to nearest mode. *)
val machine_epsilon : ?prec:Precisions.t -> Precisions.t -> t

(** This function returns a float of precision ?prec containing the machine
    delta of the mandatory precision parameter also divided by two for the same
    reason as machine_epsilon. *)
val machine_delta   : ?prec:Precisions.t -> Precisions.t -> t

(** Maximal positive float in the precision *)
val maximal_pos_float : prec:Precisions.t -> t

(** Maximal negative float in the precision *)
val maximal_neg_float : prec:Precisions.t -> t

(** The functions of_<typ> ~rnd ~prec x return a float of precision <prec>
    containing the value of x (of type <typ>) rounding to <rnd>. The default
    values are prec=Precisions.Real and rnd=Rounding.Near *)
val of_int    : ?rnd:Rounding.t -> ?prec:Precisions.t -> int    -> t
val of_float  : ?rnd:Rounding.t -> ?prec:Precisions.t -> float  -> t
val of_string : ?rnd:Rounding.t -> ?prec:Precisions.t -> string -> t

(** Change the precision *)
val change_prec : rnd:Rounding.t -> prec:Precisions.t -> t -> t

(** Comparison functions *)
val compare : t -> t -> int
val eq  : t -> t -> bool
val le  : t -> t -> bool
val lt  : t -> t -> bool
val ge  : t -> t -> bool
val gt  : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t

(** Check if its input is a NaN *)
val is_nan : t -> bool

(** Check if its input is an infinite value *)
val is_inf : t -> bool

(** Check if its input is positive (is_pos NaN = true) *)
val is_pos : t -> bool

(** Check if its input is negative (is_neg NaN = false) *)
val is_neg : t -> bool

(** Check if its input is a zero (positive or negative) *)
val is_a_zero   : t -> bool

(** Check if its input is a positive zero *)
val is_pos_zero : t -> bool

(** Check if its input is a negative zero *)
val is_neg_zero : t -> bool

(** Check if its input is strictly positive (non zero) *)
val is_strictly_pos : t -> bool

(** Check if its input is strictly negative (non zero) *)
val is_strictly_neg : t -> bool

(** Returns the sign of its input. The sign of a NaN is Positive *)
val sign : t -> Sign.t

(** Returns the precision of its input *)
val prec : t -> Precisions.t

(** Returns the exponent of its input *)
val exponent : t -> int

(** Returns the significand of its input. This function is known to generate a
    core dump if the version of your MPFR library is the 3.0.1. The version 4.0
    of the library does not have the bug anymore. *)
val significand : t -> t

(** Returns a element containing the same value as <dst> but with the sign
    of <src> *)
val apply_sign : src:t -> dst:t -> t

(** Returns the previous floating point number of the same precision *)
val prev_float : t -> t

(** Returns the following floating point number of the same precision *)
val next_float : t -> t

(** Negation *)
val neg : t -> t

(** Absolute value *)
val abs : t -> t

(** The following functions perform floating-point arithmetic operations at
    the precision <prec> and using the rounding mode <rnd>. Their default
    values are prec=Precisions.Real and rnd=Rounding.Near. The inputs are
    "casted" to the asked precision if necessary before computing the
    operation *)
val add     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t -> t
val sub     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t -> t
val mul     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t -> t
val div     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t -> t
val pow     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t -> t
val pow_int : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> int -> t
val square  : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t
val sqrt    : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t
val log     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t
val exp     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t
val sin     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t
val cos     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t
val tan     : ?rnd:Rounding.t -> ?prec:Precisions.t -> t -> t
