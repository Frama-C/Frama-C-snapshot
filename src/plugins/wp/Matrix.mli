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
(* --- Array Dimensions                                                   --- *)
(* -------------------------------------------------------------------------- *)

open Ctypes
open Lang.F

type dim = int option
type matrix = c_object * dim list

module MACHINE : Model.Key with type t = matrix
module NATURAL : Model.Key with type t = matrix

val of_array : arrayinfo -> matrix
val id : dim list -> string (** unique w.r.t [equal] *)
val natural_id : c_object -> string (** name for elements in NATURAL *)

val merge : dim list -> dim list -> dim list option

type denv = {
  size_var : var list ; (** size variables *)
  size_val : term list ; (** size values *)
  index_var : var list ; (** index variables *)
  index_val : term list ; (** index values *)
  index_range : pred list ; (** indices are in range of size variables *)
  index_offset : term list ; (** polynomial of indices *)
  monotonic : bool ; (** all dimensions are defined *)
}

val denv : dim list -> denv
val size : matrix -> term list
val tau : c_object -> dim list -> tau

