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

exception Invalid_format

val check_f_specification : f_conversion_specification -> f_conversion_specification
val check_s_specification : s_conversion_specification -> s_conversion_specification
val check_f_format : f_format -> f_format
val check_s_format : s_format -> s_format
val check_format : format -> format

val parse_f_format : string -> f_format
val parse_s_format : string -> s_format
val parse_format : format_kind -> string -> format

