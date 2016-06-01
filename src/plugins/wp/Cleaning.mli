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
(* --- Variables Cleaning                                                 --- *)
(* -------------------------------------------------------------------------- *)

open Cil_types
open Lang
open Lang.F

type usage

val create : unit -> usage
val as_atom : usage -> pred -> unit
val as_type : usage -> pred -> unit
val as_have : usage -> pred -> unit
val as_init : usage -> pred -> unit

val filter_type : usage -> pred -> pred
val filter_pred : usage -> pred -> pred
