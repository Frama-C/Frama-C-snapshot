(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Parsing of Integer and Real Constants *)

type base = Dec | Hex
type sign = Pos | Neg

type cst = {
  base : base ;
  sign : sign ;
  man  : string ;
  com  : string ;
  exp  : int ;
}

(** The parser recognizes hexadecimal and decimal numbers with the following
    formats:
    - [sign? "0d"? dec* ["." dec*]? ["e|E" sign? dec+]?] {i Decimal}
    - [sign? "0x"? hex* ["." hex*]? ["p|P" sign? dec+]?] {i Hexadecimal}

    In the above regular expressions, [sign=[+|-]], [dec=[0..9]] are decimal digits,
    [hex=[0..9 a..f A..F]] are hexadecimal ones.

    Notice that, unless a [base] argument is specified, some entries
    can be ambiguous, like "3e2" that can be either hexadecimal or
    decimal. In such cases, decimal format takes the precedence.
*)

val parse : ?base:base -> string -> cst
val pretty : Format.formatter -> cst -> unit

val is_zero : cst -> bool

val big_int_of_hex : string -> Big_int.big_int
(** Returns [0] on empty string *)
val dec_of_hex : string -> string
(** Returns empty string on empty string *)
val power_of_two : int -> string
(** Returns a decimal. Only positive powers. *)
val power_of_ten : int -> string
(** Returns a decimal. Only positive powers. *)
val significant : cst -> string * int
(** Returns all significant digits with a shifted exponent. *)


