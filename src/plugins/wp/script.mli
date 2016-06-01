(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Lexer for Script files                                             --- *)
(* -------------------------------------------------------------------------- *)

type token =
  | Id of string
  | Key of string
  | Proof of string * string
  | Word
  | Eof

type input

val open_file : string -> input
val close : input -> unit
val skip : input -> unit
val token : input -> token
val error : input -> ('a,Format.formatter,unit,'b) format4 -> 'a

val key : input -> string -> bool
val eat : input -> string -> unit
val ident : input -> string
val idents : input -> string list

val filter : string -> string option

