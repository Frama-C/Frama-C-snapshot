(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
(** Normalized C-labels                                                       *)
(* -------------------------------------------------------------------------- *)

(**
    Structural representation of logic labels.
    Compatible with pervasives comparison and structural equality.
*)

type c_label
  
val is_here : c_label -> bool
val equal : c_label -> c_label -> bool

module T : sig type t = c_label val compare : t -> t -> int end
module LabelMap : FCMap.S with type key = c_label
module LabelSet : FCSet.S with type elt = c_label

val pre : c_label
val here : c_label
val init : c_label
val post : c_label
val formal : string -> c_label

val stmt : Cil_types.stmt -> c_label
val loop_entry : Cil_types.stmt -> c_label
val loop_current : Cil_types.stmt -> c_label

val to_logic : c_label -> Cil_types.logic_label
val of_logic : Cil_types.logic_label -> c_label
(** Assumes the logic label only comes from normalized or non-ambiguous 
    labels. Ambiguous labels are: Old, LoopEntry and LoopCurrent, since
    they points to different program points dependending on the context. *)

val pretty : Format.formatter -> c_label -> unit

open Cil_types

val name : logic_label -> string
val lookup : (logic_label * logic_label) list -> string -> logic_label
(** [lookup bindings lparam] retrieves the actual label
    for the label in [bindings] for label parameter [lparam]. *)
