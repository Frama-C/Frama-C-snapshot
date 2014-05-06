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
(** Normalized C-labels                                                       *)
(* -------------------------------------------------------------------------- *)

(**
    Structural representation of logic labels.
    Compatible with pervasives comparison and structural equality.
*)

type c_label =
  | Here
  | Pre
  | Post
  | Exit
  | At of string list * int   (** Label name, stmt-id. *)
  | CallAt of int        (** stmt-id *)
  | LabelParam of string (** Logic label name in user-defined
                             function or predicate *)

val equal : c_label -> c_label -> bool

module T : sig type t = c_label val compare : t -> t -> int end
module LabelMap : FCMap.S with type key = c_label
module LabelSet : FCSet.S with type elt = c_label

(** @return a label that represent the first point of a loop body. *)
val loop_head_label : Cil_types.stmt -> Cil_types.logic_label

(** create a virtual label to a statement (it can have no label) *)
val mk_logic_label : Cil_types.stmt -> Cil_types.logic_label

val mk_stmt_label : Cil_types.stmt -> c_label
val mk_loop_label : Cil_types.stmt -> c_label

val c_label : Cil_types.logic_label -> c_label
  (**
      Assumes the logic label only comes from normalized labels.

      This is the case inside [Wp] module, where all ACSL formula comes
      from [WpAnnot], which in turns always preprocess the labels
      through [NormAtLabels].
  *)

val pretty : Format.formatter -> c_label -> unit

open Cil_types

val lookup_name : c_label -> string
val lookup : (logic_label * logic_label) list -> string -> c_label
  (** [lookup bindings lparam] retrieves the actual label
      for the label in [bindings] for label parameter [lparam]. *)
