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
