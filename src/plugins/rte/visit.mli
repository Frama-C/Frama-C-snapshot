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

open Cil_types

val annotate_kf: kernel_function -> unit
val compute: unit -> unit
val do_precond: kernel_function -> unit
val do_all_rte: kernel_function -> unit
val do_rte: kernel_function -> unit
val rte_annotations: stmt -> code_annotation list
val do_stmt_annotations: kernel_function -> stmt -> code_annotation list
val do_exp_annotations: kernel_function -> stmt -> exp -> code_annotation list

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
