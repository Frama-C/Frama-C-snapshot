(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Operations on (natural) loops.
    @plugin development guide *)

open Cil_types

exception No_such_while
val get_loop_stmts : kernel_function -> stmt -> Cil_datatype.Stmt.Set.t
  (** Precondition: the kernel function is not a leaf function.
      @raise No_such_while if [stmt.skind] is not a [While]. *)

val is_natural : kernel_function -> stmt -> bool
val get_naturals : kernel_function -> (stmt * stmt list) list

val back_edges : kernel_function -> stmt -> stmt list
val while_for_natural_loop : kernel_function -> stmt -> stmt

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
