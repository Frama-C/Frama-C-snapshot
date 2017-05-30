(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** E-ACSL tracks a local variable by injecting:
   - a call to [__e_acsl_store_block] at the beginning of its scope, and
   - a call to [__e_acsl_delete_block] at the end of the scope.
   This is not always sufficient to track variables because execution
   may exit a scope early (for instance via a goto or a break statement).
   This module computes program points at which extra `delete_block` statements
   need to be added to handle such early scope exits. *)

open Cil_types
open Cil_datatype

val generate: fundec -> unit
(** Visit a function and populate data structures used to compute exit points *)

val clear: unit -> unit
(** Clear all gathered data *)

val delete_vars: stmt -> Varinfo.Set.t
(** Given a statement which potentially leads to an early scope exit (such as
   goto, break or continue) return the list of local variables which
   need to be removed from tracking before that statement is executed.
   Before calling this function [generate] need to be executed. *)

val store_vars: stmt -> Varinfo.Set.t
(** Compute variables that should be re-recorded before a labelled statement to
   which some goto jumps *)
