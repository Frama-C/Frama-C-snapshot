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
open Cil_datatype

val get_data_scope_at_stmt :
  Kernel_function.t -> stmt -> lval ->
  Stmt.Hptset.t * (Stmt.Hptset.t * Stmt.Hptset.t)

val get_prop_scope_at_stmt :
  kernel_function -> stmt -> code_annotation ->
  Stmt.Hptset.t * code_annotation list

val check_asserts : unit -> code_annotation list

val rm_asserts : unit -> unit

(** for internal use *)
module R: Plugin.General_services

val get_lval_zones:
  for_writing:bool ->
  Cil_types.stmt ->
  Cil_types.lval ->
  Locations.Zone.t * bool * Locations.Zone.t
