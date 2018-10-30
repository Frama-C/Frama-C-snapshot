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

(** Interface of floating-point numbers of different precisions. *)

(** Rounding modes defined in the C99 standard. *)
type round = Up | Down | Near | Zero

(** Precision of floating-point numbers:
    - the 'single', 'double' and 'long double' C types;
    - the ACSL 'real' type.  *)
type prec = Single | Double | Long_Double | Real

module type S = sig
  type t

  val packed_descr: Structural_descr.pack
  val hash: t -> int
  val pretty: Format.formatter -> t -> unit

  (** Is the representation of floating-point numbers of a given precision
      exact? Should at least be false for [Real]. *)
  val is_exact: prec -> bool

  (** Converts a caml float into a floating-point number of the given precision.
      Must work on infinities and on negative and positive zeros. *)
  val of_float: round -> prec -> float -> t

  (** Converts a floating-point number of single or double precision into
      a caml float. Never called on better precision than double. *)
  val to_float: t -> float

  (** IEEE comparison: do not distinguish between -0.0 and 0.0.
      This comparison must work between values of different precisions. *)
  val cmp_ieee: t -> t -> int

  (** Comparison that distinguishes -0.0 and 0.0.
      This comparison must work between values of different precisions. *)
  val compare: t -> t -> int

  val is_nan: t -> bool
  val is_finite: t -> bool
  val is_infinite: t -> bool

  (** Is a number negative? Never called on NaN, but must be correct on
      infinities and zeros. *)
  val is_negative: t -> bool

  (** Rounds a number to a given precision. *)
  val round_to_precision: round -> prec -> t -> t

  (** First value above the argument in the given precision.
      Returns the minimum finite value on -infty.
      Returns +0 on -0, and the minimum strictly positive value on +0.
      Returns +infty on the maximum finite value.
      Returns +infty on +infty.
      Undefined on NaN. *)
  val next_float: prec -> t -> t

  (** First value below the argument in the given precision. Inversed
      behavior as [next_float]. *)
  val prev_float: prec -> t -> t

  (** [widen_up f] returns a value strictly larger than [f], such that
      successive applications of [widen_up] converge rapidly to infinity. *)
  val widen_up: t -> t

  (** Floating-point operations. *)

  val neg: t -> t
  val abs: t -> t

  val floor: t -> t
  val ceil: t -> t
  val trunc: t -> t
  val fround: t -> t

  val add: round -> prec -> t -> t -> t
  val sub: round -> prec -> t -> t -> t
  val mul: round -> prec -> t -> t -> t
  val div: round -> prec -> t -> t -> t
  val fmod: round -> prec -> t -> t -> t

  val exp: round -> prec -> t -> t
  val log: round -> prec -> t -> t
  val log10: round -> prec -> t -> t
  val sqrt: round -> prec -> t -> t
  val pow: round -> prec -> t -> t -> t

  val cos: round -> prec -> t -> t
  val sin: round -> prec -> t -> t
  val atan2: round -> prec -> t -> t -> t
end
