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

(** See C11, 7.21.6 *)

type flag = FMinus | FPlus | FSpace | FSharp | FZero
type flags = flag list

type f_field_width = [ `FWStar | `FWInt of int (** positive integer *)] 
type s_field_width = [ `FWInt of int ]
type any_field_width = [ f_field_width | s_field_width ]

type precision = PStar | PInt of int

type length_modifier = [ `hh | `h | `l | `ll | `j | `z | `t | `L ]

type signed_specifier = [ `d | `i ]
type unsigned_specifier = [ `u | `o | `x ]
type integer_specifier = [ signed_specifier | unsigned_specifier ]
type float_specifier = [ `f | `e | `g | `a  ]
type numeric_specifier = [ integer_specifier | float_specifier ]
type capitalizable = [ `x | `f | `e | `g | `a  ]
type has_alternative_form = [ `o | `x | `f | `e | `g | `a  ]

type f_conversion_specifier =
  [ numeric_specifier | `c | `s | `p | `n ]
type s_conversion_specifier =
  [ f_conversion_specifier | `Brackets of string ]
type any_conversion_specifier = 
  [ s_conversion_specifier | f_conversion_specifier ]

type f_conversion_specification = {
  mutable f_flags: flags;
  mutable f_field_width: f_field_width option;
  mutable f_precision: precision option;
  mutable f_length_modifier: length_modifier option;
  mutable f_conversion_specifier: f_conversion_specifier;
  mutable f_capitalize: bool;
}

type s_conversion_specification = {
  mutable s_assignment_suppression: bool;
  mutable s_field_width: s_field_width option;
  mutable s_length_modifier: length_modifier option;
  mutable s_conversion_specifier: s_conversion_specifier;
}

(** A format element is either a character or a conversion specification. *)
type 'spec token =
| Char of char
| Specification of 'spec

type f_format = f_conversion_specification token list
type s_format = s_conversion_specification token list

type format = FFormat of f_format | SFormat of s_format
type format_kind = PrintfLike | ScanfLike
