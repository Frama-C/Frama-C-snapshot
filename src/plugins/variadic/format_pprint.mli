(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

open Format_types

val pp_flag : Format.formatter -> flag -> unit
val pp_flags : Format.formatter -> flags -> unit
val pp_fw : Format.formatter -> [< any_field_width] -> unit
val pp_precision : Format.formatter -> precision -> unit
val pp_lm : Format.formatter -> length_modifier -> unit
val pp_cs : Format.formatter -> [< any_conversion_specifier] * bool -> unit
val pp_f_specification :  Format.formatter -> f_conversion_specification -> unit
val pp_s_specification :  Format.formatter -> s_conversion_specification -> unit

val pp_f_format : Format.formatter -> f_format -> unit
val pp_s_format : Format.formatter -> s_format -> unit
val pp_format : Format.formatter -> format -> unit

(** Rewrites the format as its string representation. *)
val f_format_to_cstring : f_format -> string
val s_format_to_cstring : s_format -> string
val format_to_cstring : format -> string
