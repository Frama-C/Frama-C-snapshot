(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(** Fine-tuning for slevel, according to [//@ slevel] directives. *)

open Cil_types

type slevel =
| Global of int (** Same slevel i in the entire function *)
| PerStmt of (stmt -> int) (** Different slevel for different statements *)

(** Slevel to use in this function *)
val local: kernel_function -> slevel


type merge =
| NoMerge (** Propagate states according to slevel in the entire function. *)
| Merge of (stmt -> bool) (** Statements on which multiple states should be
                              merged (instead of being propagated separately) *)

(** Slevel merge strategy for this function *)
val merge: kernel_function -> merge
