(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Lexer for logic annotations *)

val token : Lexing.lexbuf -> Logic_parser.token
(** For plugins that need to call functions of [Logic_parser] themselves *)

val chr : Lexing.lexbuf -> string
val is_acsl_keyword : string -> bool


type 'a parse = Lexing.position * string -> Lexing.position * 'a
(** Generic type for parsing functions built on tip of the lexer. Given
    such a function [f], [f (pos, s)] parses [s], assuming that it starts at
    position [pos]. It returns the final position, and the result of the
    parsing. May raise [Parsing.Parse_error]. *)

val lexpr : Logic_ptree.lexpr parse
val annot : Logic_ptree.annot parse
val spec : Logic_ptree.spec parse
val ext_spec : Logic_ptree.ext_spec parse
