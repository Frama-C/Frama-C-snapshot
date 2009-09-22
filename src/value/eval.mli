(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

val eval_lval :
  with_alarms:CilE.warn_mode ->
  Locations.Zone.t option ->
  Db.Value.state ->
  Cil_types.lval ->
  Db.Value.state * Locations.Zone.t option * Cvalue_type.V.t

val eval_expr :
  with_alarms:CilE.warn_mode ->
  Db.Value.state -> Cil_types.exp -> Cvalue_type.V.t

val lval_to_loc_with_deps :
  deps:Locations.Zone.t ->
  Db.Value.state ->
  Cil_types.lval ->
  with_alarms:CilE.warn_mode ->
  reduce_valid_index:bool ->
  Db.Value.state * Locations.Zone.t option * Locations.location

val lval_to_loc :
  with_alarms:CilE.warn_mode ->
  Db.Value.state -> Cil_types.lval -> Locations.location

val resolv_func_vinfo :
  with_alarms:CilE.warn_mode ->
  Locations.Zone.t option ->
  Db.Value.state ->
  Cil_types.exp -> Locations.Zone.t option * Db_types.kernel_function list

exception Leaf

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
