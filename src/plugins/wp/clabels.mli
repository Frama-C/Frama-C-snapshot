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
(** Normalized C-labels                                                       *)
(* -------------------------------------------------------------------------- *)

(**
    Structural representation of logic labels.
    Compatible with pervasives comparison and structural equality.
*)

type c_label =
  | Here
  | Init
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
