(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

(** Pretty-printing of a parsed logic tree. *)

open Logic_ptree

val print_constant: Format.formatter -> constant -> unit

(** First arguments prints the name of identifier declared with the
    corresponding type (None for pure type. C syntax makes impossible to
    separate printing the type and the identifier in a declaration...
*)
val print_logic_type:
  (Format.formatter -> unit) option -> Format.formatter -> logic_type -> unit

val print_quantifiers: Format.formatter -> quantifiers -> unit

val print_lexpr: Format.formatter -> lexpr -> unit

val print_type_annot: Format.formatter -> type_annot -> unit

val print_typedef: Format.formatter -> typedef -> unit

val print_decl: Format.formatter -> decl -> unit

val print_spec: Format.formatter -> spec -> unit

val print_code_annot: Format.formatter -> code_annot -> unit

val print_assigns: Format.formatter -> assigns -> unit

val print_variant: Format.formatter -> variant -> unit
