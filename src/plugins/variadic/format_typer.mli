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
open Cil_types

exception Type_not_found of string
exception Invalid_specifier

type arg_dir = [ `ArgIn
               | `ArgInArray of precision option (* for '%.*s' or '%.42s' *)
               | `ArgOut
               | `ArgOutArray ]

type typdef_finder = Logic_typing.type_namespace -> string -> Cil_types.typ

val type_f_specifier : ?find_typedef : typdef_finder -> f_conversion_specification -> typ
val type_s_specifier : ?find_typedef : typdef_finder -> s_conversion_specification -> typ
val type_f_format : ?find_typedef : typdef_finder ->  f_format -> (typ * arg_dir) list
val type_s_format : ?find_typedef : typdef_finder ->  s_format -> (typ * arg_dir) list
val type_format : ?find_typedef : typdef_finder -> format -> (typ * arg_dir) list

