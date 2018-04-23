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

(** Extension of [Big_int] compatible with [Zarith]. 
    @since Nitrogen-20111001 *)

type t = Z.t

exception Too_big (** Produced by values whose physical representation is too
                      costly (e.g. in terms of memory usage). *)

val equal : t -> t -> bool
val compare : t -> t -> int

val le : t -> t -> bool
val ge : t -> t -> bool
val lt : t -> t -> bool
val gt : t -> t -> bool

val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t

val shift_left : t -> t -> t
val shift_right : t -> t -> t
val shift_right_logical : t -> t -> t
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val lognot : t -> t

val min : t -> t -> t
val max : t -> t -> t

val native_div : t -> t -> t
val div : t -> t -> t
(** Euclidean division (that returns a positive rem) *)
val pos_div : t -> t -> t
(** Euclidean division. Equivalent to C division if both operands are positive.
    Equivalent to a floored division if b > 0 (rounds downwards),
    otherwise rounds upwards.
    Note: it is possible that pos_div (-a) b <> pos_div a (-b). *)
val divexact: t -> t -> t
(** Faster, but produces correct results only when b evenly divides a. *)
val c_div : t -> t -> t
(** Truncated division towards 0 (like in C99) *)

val rem : t -> t -> t
(** Remainder of the Euclidean division (always positive) *)
val c_rem : t -> t -> t
(** Remainder of the truncated division towards 0 (like in C99) *)
val div_rem: t -> t -> (t * t)
(** [div_rem a b] returns [(pos_div a b, pos_rem a b)] *)
val pos_rem : t -> t -> t
(** Remainder of the Euclidean division (always positive) *)

val pgcd : t -> t -> t
(** [pgcd v 0 == pgcd 0 v == abs v]. Result is always positive *)

val ppcm : t -> t -> t
(** [ppcm v 0 == ppcm 0 v == 0]. Result is always positive *)

val power_int_positive_int: int -> int -> t
(** Exponentiation *)

val cast: size:t -> signed:bool -> value:t -> t

val abs : t -> t
val neg : t -> t
val succ : t -> t
val pred : t -> t

val is_zero : t -> bool
val is_one : t -> bool
val is_even : t -> bool

val zero : t
val one : t
val two : t
val four : t
val eight : t
val sixteen : t
val thirtytwo : t
val onethousand : t
val billion_one : t
val minus_one : t
val max_int64 : t
val min_int64 : t
val two_power_32 : t
val two_power_64 : t

val length : t -> t -> t (** b - a + 1 *)

val of_int : int -> t
val of_int64 : Int64.t -> t
val of_int32 : Int32.t -> t
val to_int64 : t -> int64
val to_int32 : t -> int32

val to_int : t -> int
(** @raise Failure if the argument does not fit in an OCaml int *)

val to_float : t -> float
val of_float : float -> t
(** Converts from a floating-point value. The value is truncated.
    Raises [Overflow] on infinity and NaN arguments. *)

val round_up_to_r : min:t -> r:t -> modu:t -> t
(** [round_up_to_r m r modu] is the smallest number [n] such that
    [n]>=[m] and [n] = [r] modulo [modu] *)

val round_down_to_r : max:t -> r:t -> modu:t -> t
(** [round_down_to_r m r modu] is the largest number [n] such that
    [n]<=[m] and [n] = [r] modulo [modu] *)

val two_power : t -> t
(** [two_power x] computes 2^x. Can raise [Too_big]. *)
val two_power_of_int : int -> t
(** Similar to [two_power x], but x is an OCaml int. *)

val extract_bits : start:t -> stop:t -> t -> t

val hash : t -> int

val of_string : string -> t
(** @raise Failure _ when the string cannot be parsed. *)

val to_string : t -> string

val popcount: t -> int

val pretty : ?hexa:bool -> t Pretty_utils.formatter


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
