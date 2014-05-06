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

(** Floating-point operations.
    @plugin development guide *)

external set_round_downward : unit -> unit = "set_round_downward" "noalloc"
external set_round_upward : unit -> unit = "set_round_upward" "noalloc"
external set_round_nearest_even : unit -> unit = 
    "set_round_nearest_even" "noalloc"

external round_to_single_precision_float: float -> float = "round_to_float"

val max_single_precision_float: float
val most_negative_single_precision_float: float

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


exception Float_Non_representable_as_Int64

val truncate_to_integer: float -> Integer.t
(** Raises [Float_Non_representable_as_Int64] if the float value cannot
    be represented as an Int64 or as an unsigned Int64. *)

(*
Local Variables:
compile-command: "make -C ../.. byte"
End:
*)
