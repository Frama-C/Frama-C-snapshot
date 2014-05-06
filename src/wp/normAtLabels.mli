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

open Cil_types

(* exception LabelError of logic_label *)
val catch_label_error : exn -> string -> string -> unit

type label_mapping

val labels_empty : label_mapping
val labels_fct_pre : label_mapping
val labels_fct_post : label_mapping
val labels_fct_assigns : label_mapping
val labels_assert_before : stmt -> label_mapping
val labels_assert_after : stmt -> logic_label option -> label_mapping
val labels_loop_inv : stmt -> label_mapping
val labels_loop_assigns : stmt -> label_mapping
val labels_stmt_pre : stmt -> label_mapping
val labels_stmt_post : stmt -> logic_label option -> label_mapping
val labels_stmt_assigns : stmt -> logic_label option -> label_mapping
val labels_predicate : (logic_label * logic_label) list -> label_mapping
val labels_axiom : label_mapping

val preproc_annot : label_mapping -> predicate named -> predicate named

val preproc_assigns :
  label_mapping -> identified_term from list -> identified_term from list

val preproc_label : label_mapping -> logic_label -> logic_label
