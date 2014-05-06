(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

type t

val equal : t -> t -> bool
val compare : t -> t -> int
val le : t -> t -> bool
val ge : t -> t -> bool
val lt : t -> t -> bool
val gt : t -> t -> bool
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val native_div : t -> t -> t
val rem : t -> t -> t
val pos_div : t -> t -> t
val divexact: t -> t -> t (** faster, but produces correct results only when b evenly divides a. *)
val c_div : t -> t -> t
val c_rem : t -> t -> t
val div_rem: t -> t -> (t * t) (** [div_rem a b] returns [(pos_div a b, pos_rem a b)] *)
val cast: size:t -> signed:bool -> value:t -> t
val abs : t -> t
val one : t
val two : t
val four : t
val onethousand : t
val minus_one : t
val is_zero : t -> bool
val is_one : t -> bool
val pgcd : t -> t -> t
val ppcm : t -> t -> t
val min : t -> t -> t
val max : t -> t -> t
val length : t -> t -> t (** b - a + 1 *)
val of_int : int -> t
val of_int64 : Int64.t -> t
val of_int32 : Int32.t -> t
val to_int64 : t -> int64

val to_int : t -> int
(** @raise Failure if the argument does not fit in an OCaml int *)

val to_float : t -> float
val neg : t -> t
val succ : t -> t
val pred : t -> t
val round_up_to_r : min:t -> r:t -> modu:t -> t
val round_down_to_r : max:t -> r:t -> modu:t -> t
val pos_rem : t -> t -> t
val shift_left : t -> t -> t
val shift_right : t -> t -> t
val logand : t -> t -> t
val logor : t -> t -> t
val logxor : t -> t -> t
val lognot : t -> t
val two_power : t -> t			(* [two_power x] computes 2^x. *)
val two_power_of_int : int -> t	        (* Similar to [two_power x], but x is an OCaml int. *)

val extract_bits : start:t -> stop:t -> t -> t

val small_nums : t array
val zero : t
val eight : t
val sixteen : t
val thirtytwo : t
val div : t -> t -> t

val billion_one : t
val hash : t -> int
val shift_right_logical : t -> t -> t

val two_power_64 : t
val max_int64 : t
val min_int64 : t
val bits_of_max_float : t
val bits_of_most_negative_float : t
val of_string : string -> t
val to_string : t -> string
val add_2_64 : t -> t
val add_2_32 : t -> t
val is_even : t -> bool
val round_down_to_zero : t -> t -> t
val power_int_positive_int: int -> int -> t
val to_num : t -> Num.num
val popcount: t -> int

val pretty : ?hexa:bool -> t Pretty_utils.formatter


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
