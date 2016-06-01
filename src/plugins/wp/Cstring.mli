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

