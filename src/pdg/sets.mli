(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** PDG (program dependence graph) access functions. *)

type t_node = PdgTypes.Node.t
type t_loc =  Locations.Zone.t
type t_pdg = PdgTypes.Pdg.t
type t_dpds_kind = PdgTypes.Dpd.td

type t_nodes_and_undef = (t_node * t_loc option) list * t_loc option

(** {2 PDG nodes for some elements} *)

val find_stmt_node : t_pdg -> Cil_types.stmt -> t_node
val find_simple_stmt_nodes : t_pdg -> Cil_types.stmt -> t_node list
val find_stmt_and_blocks_nodes : t_pdg -> Cil_types.stmt -> t_node list
(*val find_nodes_for_stmt_id : t_pdg -> int -> t_node list*)
val find_location_nodes_at_stmt :
  t_pdg -> Cil_types.stmt -> before:bool -> t_loc -> t_nodes_and_undef
val find_location_nodes_at_end : t_pdg -> t_loc -> t_nodes_and_undef
val find_location_nodes_at_begin : t_pdg -> t_loc -> t_nodes_and_undef
val find_label_node : t_pdg -> Cil_types.stmt -> Cil_types.label -> t_node
val find_decl_var_node : t_pdg -> Cil_types.varinfo -> t_node
val find_input_node : t_pdg -> int -> t_node
val find_output_node : t_pdg -> t_node
val find_all_input_nodes : t_pdg -> t_node list
val find_entry_point_node : t_pdg -> t_node
val find_top_input_node : t_pdg -> t_node
val find_output_nodes :
  t_pdg -> PdgIndex.Signature.t_out_key -> t_nodes_and_undef


val find_call_ctrl_node : t_pdg -> Cil_types.stmt -> t_node
val find_call_num_input_node : t_pdg -> Cil_types.stmt -> int -> t_node
val find_call_input_nodes :
  t_pdg -> Cil_types.stmt -> PdgIndex.Signature.t_in_key -> t_nodes_and_undef
val find_call_output_node : t_pdg -> Cil_types.stmt -> t_node

val find_call_stmts:
  Db_types.kernel_function -> caller:Db_types.kernel_function ->
  Cil_types.stmt list

val find_call_out_nodes_to_select :
  t_pdg -> t_node list -> t_pdg ->  Cil_types.stmt -> t_node list
val find_in_nodes_to_select_for_this_call :
  t_pdg -> t_node list -> Cil_types.stmt -> t_pdg -> t_node list

(* direct dependencies only :
 * This means the nodes that have an edge to the given node. *)
val direct_dpds : t_pdg -> t_node -> t_node list
val direct_x_dpds : t_dpds_kind -> t_pdg -> t_node -> t_node list
val direct_data_dpds : t_pdg -> t_node -> t_node list
val direct_ctrl_dpds : t_pdg -> t_node -> t_node list
val direct_addr_dpds : t_pdg -> t_node -> t_node list

(* transitive closure *)

val find_nodes_all_dpds : t_pdg -> t_node list -> t_node list
val find_nodes_all_x_dpds : t_dpds_kind -> t_pdg -> t_node list -> t_node list
val find_nodes_all_data_dpds : t_pdg -> t_node list -> t_node list
val find_nodes_all_ctrl_dpds : t_pdg -> t_node list -> t_node list
val find_nodes_all_addr_dpds : t_pdg -> t_node list -> t_node list

(* forward *)

val direct_uses : t_pdg -> t_node -> t_node list
val direct_x_uses : t_dpds_kind -> t_pdg -> t_node -> t_node list
val direct_data_uses : t_pdg -> t_node -> t_node list
val direct_ctrl_uses : t_pdg -> t_node -> t_node list
val direct_addr_uses : t_pdg -> t_node -> t_node list

val all_uses : t_pdg -> t_node list -> t_node list

(* others *)

val custom_related_nodes : (t_node -> t_node list) -> t_node list -> t_node list
