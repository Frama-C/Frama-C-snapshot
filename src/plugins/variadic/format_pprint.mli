(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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

(** Réécrit le format sous sa forme de chaîne de caractères *)
val f_format_to_cstring : f_format -> string
val s_format_to_cstring : s_format -> string
val format_to_cstring : format -> string
