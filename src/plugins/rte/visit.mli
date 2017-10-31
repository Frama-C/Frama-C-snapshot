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

open Cil_types

(** Generates RTE for a single function. Uses the status of the various
      RTE options do decide which kinds of annotations must be generated.
*)
val annotate_kf: kernel_function -> unit

(** Generates preconditions RTE for a given function. *)
val do_precond: kernel_function -> unit

(** Generates all RTEs for a given function. *)
val do_all_rte: kernel_function -> unit

(** Generates all RTEs except preconditions for a given function. *)
val do_rte: kernel_function -> unit

val rte_annotations: stmt -> code_annotation list
val do_stmt_annotations: kernel_function -> stmt -> code_annotation list
val do_exp_annotations: kernel_function -> stmt -> exp -> code_annotation list

(** Main entry point of the plug-in, used by [-rte] option: computes
    RTE on the whole AST. Which kind of RTE is generated depends on the
    options given on the command line.
*)
val compute: unit -> unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
