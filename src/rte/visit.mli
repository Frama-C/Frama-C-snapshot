(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
compile-command: "make -C ../.."
End:
*)
