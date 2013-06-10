(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** Natural arithmetics *)

type t

val zero : t
val one : t
val minus_one : t

(** {2 Operations} *)

val succ : t -> t
val pred : t -> t

val int : int -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val opp : t -> t

val div : t -> t -> t 
  (** Defined to be [q] such that:
      - [abs(q)] is the enclidian quotient of [abs(a)] and [abs(b)],
      - [q] has the sign of [a.b] *)

val remainder : t -> t -> t 
  (** Defined to be [a - q.b] where [q = a div b] *)

val euclidian : t -> t -> t * t
  (** returns div and mod *)

val equal : t -> t -> bool
val not_equal : t -> t -> bool

val leq : t -> t -> bool
val lt  : t -> t -> bool
val positive : t -> bool
val negative : t -> bool
val null : t -> bool
val lt_zero : t -> bool
val gt_zero : t -> bool
val min : t -> t -> t
val max : t -> t -> t

type sign = Null | Positive | Negative
val sign : t -> sign

val two_power : t -> t
val cast_size: size:t -> signed:bool -> value:t -> t
val cast_max: max:t -> signed:bool -> value:t -> t

(** {2 Bitwise operations } *)
val bitwise_shift_left : t -> t -> t
val bitwise_shift_right : t -> t -> t
val bitwise_and : t -> t -> t
val bitwise_or : t -> t -> t
val bitwise_xor : t -> t -> t
val bitwise_not : t -> t

(** {2 Conversions} *)

val of_int : int -> t
val to_int : t -> int option
val to_big_int : t -> Big_int.big_int
val of_big_int : Big_int.big_int -> t
val to_string : t -> string
val of_string : string -> t
val pretty : Format.formatter -> t -> unit

(** {2 Algebraic Data Type} *)

val hash : t -> int
val compare : t -> t -> int
