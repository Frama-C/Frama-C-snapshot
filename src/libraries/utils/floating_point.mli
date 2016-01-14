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

(** Floating-point operations. *)

(** Rounding modes defined in the C99 standard. *)
type c_rounding_mode =
    FE_ToNearest | FE_Upward | FE_Downward | FE_TowardZero

val string_of_c_rounding_mode : c_rounding_mode -> string

external set_round_downward : unit -> unit = "set_round_downward" "noalloc"
external set_round_upward : unit -> unit = "set_round_upward" "noalloc"
external set_round_nearest_even : unit -> unit = 
    "set_round_nearest_even" "noalloc"
external set_round_toward_zero : unit -> unit = "set_round_toward_zero" "noalloc"
external get_rounding_mode: unit -> c_rounding_mode = "get_rounding_mode" "noalloc"
external set_rounding_mode: c_rounding_mode -> unit = "set_rounding_mode" "noalloc"

external round_to_single_precision_float: float -> float = "round_to_float"

val max_single_precision_float: float
val most_negative_single_precision_float: float

val min_denormal: float
val neg_min_denormal: float
val min_single_precision_denormal: float
val neg_min_single_precision_denormal: float

external sys_single_precision_of_string: string -> float = 
    "single_precision_of_string"


(** If [s] is parsed as [(n, l, u)], then [n] is the nearest approximation of
    [s] with the desired precision. Moreover, [l] and [u] are the
    most precise float such that [l <= s <= u], again with this precision.
    
    Consistent with [logic_real] definition in Cil_types. *)
type parsed_float = {
  f_nearest : float ;
  f_lower : float ;
  f_upper : float ;
}

val single_precision_of_string: string -> parsed_float
val double_precision_of_string: string -> parsed_float
val parse_kind: Cil_types.fkind -> string -> parsed_float

val pretty_normal : use_hex : bool -> Format.formatter -> float -> unit
val pretty : Format.formatter -> float -> unit


type sign = Neg | Pos

exception Float_Non_representable_as_Int64 of sign

val truncate_to_integer: float -> Integer.t
(** Raises [Float_Non_representable_as_Int64] if the float value cannot
    be represented as an Int64 or as an unsigned Int64. *)


(** binary representation of -DBL_MAX and DBL_MAX as 64 bits signed integers *)
val bits_of_max_double : Integer.t
val bits_of_most_negative_double : Integer.t

(** binary representation of -FLT_MAX and FLT_MAX as 32 bits signed integers *)
val bits_of_max_float : Integer.t
val bits_of_most_negative_float : Integer.t

(** Rounds to nearest integer, away from zero (like round() in C). *)
external fround: float -> float = "c_round"

(** Rounds to integer, toward zero (like trunc() in C). *)
external trunc: float -> float = "c_trunc"

(** Single-precision (32-bit) floating-point wrappers *)

external expf: float -> float = "c_expf"
external logf: float -> float = "c_logf"
external log10f: float -> float = "c_log10f"
external powf: float -> float -> float = "c_powf"
external sqrtf: float -> float = "c_sqrtf"


(** Auxiliary functions similar to the ones in the C math library *)

val isnan : float -> bool
val isfinite : float -> bool

val nextafter : float -> float -> float
val nextafterf : float -> float -> float

(*
Local Variables:
compile-command: "make -C ../../.. byte"
End:
*)
