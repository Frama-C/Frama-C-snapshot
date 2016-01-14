(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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
