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

(** Real number arithmetics. *)

type t
val to_string : t -> string
val of_string : string -> t

type maybe =
  | Sure_true
  | Sure_false
  | Unknown

val is_zero : t -> bool

val eq  : t -> t -> maybe
val neq : t -> t -> maybe
val lt  : t -> t -> maybe
val leq : t -> t -> maybe

val opp : t -> t
val positive : t -> bool
val negative : t -> bool

val hash : t -> int
val equal : t -> t -> bool (* representation equality *)
val compare : t -> t -> int

val pretty : Format.formatter -> t -> unit
