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
(** Parser for Terms                                                          *)
(* -------------------------------------------------------------------------- *)

open Logic
open Syntax
open Lexer

val keymap : Lexer.keymap
val extend : string list -> Lexer.keymap

(** Parses ['a1 ... 'an t] *)
val parse_typedef : input -> id list * id

(** Parses [t1 -> ... tn -> tr] *)
val parse_signature : input -> ( t list * t )

(** Parses [arg ... arg] *)
val parse_args : input -> arg list

(** Parses [constructor] and [injective] *)
val parse_category : input -> 'a category

(** Parses a type *)
val parse_type : input -> t

(** Parses an expression (or a pattern) *)
val parse_expr : input -> e

