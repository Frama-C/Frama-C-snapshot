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

val get_defs :
  Kernel_function.t -> Cil_datatype.Stmt.t -> Cil_datatype.Lval.t ->
  (Cil_datatype.Stmt.Hptset.t * Locations.Zone.t option) option

val get_defs_with_type :
  Kernel_function.t -> Cil_datatype.Stmt.t -> Cil_datatype.Lval.t ->
  ((bool * bool) Cil_datatype.Stmt.Map.t * Locations.Zone.t option) option


(* internal use *)
val compute_with_def_type_zone:
  Cil_types.kernel_function -> Cil_types.stmt -> Locations.Zone.t ->
  ((bool * bool) Cil_datatype.Stmt.Map.t * Locations.Zone.t option) option
(** This function is similar to {get_defs_with_type}, except
    that it receives a zone as argument, instead of an l-value *)
