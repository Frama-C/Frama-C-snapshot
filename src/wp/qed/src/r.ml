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

type t = string

let float = string_of_float
let of_string s = s
let to_string s = s
let hash = Hashtbl.hash
let equal = (=)
let compare = Pervasives.compare

type maybe =
  | Sure_true
  | Sure_false
  | Unknown

let is_zero = function
  | "0" | "0.0" | "0." -> true
  | _ -> false

let eq a b = if a = b then Sure_true else Unknown
let neq a b = if a = b then Sure_false else Unknown
let lt a b = if a = b then Sure_false else Unknown
let leq a b = if a = b then Sure_true else Unknown

let pretty = Format.pp_print_string

let positive a = String.length a <= 0 || a.[0] <> '-'
let negative a = String.length a > 0 && a.[0] = '-'
let opp a =
  if String.length a <= 0 then a else
  if a.[0] = '-' then
    String.sub a 1 (String.length a - 1)
  else
    ("-" ^ a)

