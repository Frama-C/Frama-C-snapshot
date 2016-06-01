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

open Lang.F

(** Passive Forms *)

type t

val empty : t
val union : t -> t -> t
val bind : fresh:var -> bound:var -> t -> t
val join : var -> var -> t -> t
val conditions : t -> (var -> bool) -> pred list
val apply : t -> pred -> pred

val pretty : Format.formatter -> t -> unit
