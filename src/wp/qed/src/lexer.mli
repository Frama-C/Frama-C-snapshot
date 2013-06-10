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

(* -------------------------------------------------------------------------- *)
(** Lexer for Terms                                                           *)
(* -------------------------------------------------------------------------- *)

open Syntax

type lexeme =
  | INT     of string
  | REAL    of string
  | STRING  of string
  | IDENT   of string
  | QUOTED  of string
  | KEYWORD of string
  | END
  | EOF

type keymap
val keymap : string list -> keymap
val extend : keymap -> string list -> keymap

include Input.S
 with type token = lexeme
 and type langage = keymap

val skip_pos : input -> position
val skip_key : input -> string -> unit
val skip_ident : input -> id
val pp_lexeme : Format.formatter -> lexeme -> unit

val is_key : input -> string -> bool

val parse_list :
  left:string -> sep:string -> right:string ->
  (input -> 'a) -> input -> 'a list option

val parse_option :
  key:string -> (input -> 'a) -> input -> 'a option
