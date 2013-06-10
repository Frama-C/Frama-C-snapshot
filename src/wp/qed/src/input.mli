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
(** Lexer Utilities                                                           *)
(* -------------------------------------------------------------------------- *)

open Syntax

type 'a lexer = Lexing.lexbuf -> 'a

exception SyntaxError of position * string

val merge : position -> position -> position
val merge_list : ('a -> position) -> position -> 'a list -> position
val error_at : position -> ('a,Format.formatter,unit,'b) format4 -> 'a
val pp_position : Format.formatter -> position -> unit
val locate : position -> exn -> exn
val nowhere : position
val string_of_exn : exn -> string

module type Lexer =
sig
  type token
  type langage
  val eof : token
  val create : langage -> token lexer
end

module type S =
sig

  type input
  type token
  type langage

  val open_file : langage -> string -> input
  val open_shell : langage -> string -> input
  val close : input -> unit

  val token    : input -> token
  val skip     : input -> unit
  val context  : input -> string -> unit
  val position : input -> position

  val error : input -> ('a,Format.formatter,unit,'b) format4 -> 'a
end

module Make(L : Lexer) : S
  with type token = L.token
  and type langage = L.langage
