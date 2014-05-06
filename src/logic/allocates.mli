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

(** Generation of default [allocates \nothing] clauses. *)

val add_allocates_nothing_funspec: Cil_types.kernel_function -> unit
(** Adds [allocates \nothing] to the default behavior of the function
    if it does not have and allocation clause yet. *)

class vis_add_loop_allocates: Visitor.frama_c_inplace
(** This class adds [loop allocates] clauses to all the statements it visits. *)

val add_allocates_nothing: unit -> unit
(** Add default [allocates \nothing] clauses to all functions and loops. *)
