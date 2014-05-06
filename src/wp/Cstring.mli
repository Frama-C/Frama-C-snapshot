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

(* -------------------------------------------------------------------------- *)
(* --- String Constants                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Lang
open Lang.F

type cst = 
  | C_str of string (** String Literal *)
  | W_str of int64 list (** Wide String Literal *)

val pretty : Format.formatter -> cst -> unit

val str_len : cst -> term -> pred
val str_val : cst -> term (** The array containing all [char] of the constant *)
val str_id : cst -> int (** Non-zero integer, unique for each different string literal *)
val char_at : cst -> term -> term

val cluster : unit -> Definitions.cluster
  (** The cluster where all strings are defined. *)

