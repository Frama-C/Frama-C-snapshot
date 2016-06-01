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

(** Usage Variable Analysis *)

open Cil_types

exception NoSize

val degree_of_type : typ -> int (** Dimensions in the type (0 for non-array) *)
val alloc_for_type : typ -> int list (** Size of dimensions in the type (0 for unknown size) *)
val cells_in_type  : typ -> Integer.t (** Number of cells in the type (raise NoSize for unknown size) *)
val type_of_cells  : typ -> typ (** Type of multi-dimensional array cells *)

type usage =
  | NotUsed
  | ByValue
  | ByAddress
  | ByReference
  | ByArray of int list (** Dimension *)
  | ByRefArray of int list (** Dimension *)

val compute : unit -> unit

val of_cvar : varinfo -> usage
val of_formal : varinfo -> usage
val of_lvar : logic_var -> usage
val validated_lvar : logic_var -> bool
val validated_cvar : varinfo -> bool

val dump : unit -> unit

val pretty : name:string -> Format.formatter -> usage -> unit
