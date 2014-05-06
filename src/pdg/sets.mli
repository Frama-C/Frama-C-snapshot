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

(** PDG (program dependence graph) access functions. *)

open Cil_types

type nodes_and_undef = 
    (PdgTypes.Node.t * Locations.Zone.t option) list * Locations.Zone.t option

(** {2 PDG nodes for some elements} *)

val find_stmt_node: PdgTypes.Pdg.t -> stmt -> PdgTypes.Node.t
val find_simple_stmt_nodes: PdgTypes.Pdg.t -> stmt -> PdgTypes.Node.t list
val find_stmt_and_blocks_nodes: PdgTypes.Pdg.t -> stmt -> PdgTypes.Node.t list
val find_location_nodes_at_stmt:
  PdgTypes.Pdg.t -> stmt -> before:bool -> Locations.Zone.t -> nodes_and_undef
val find_location_nodes_at_end: 
  PdgTypes.Pdg.t -> Locations.Zone.t -> nodes_and_undef
val find_location_nodes_at_begin: 
  PdgTypes.Pdg.t -> Locations.Zone.t -> nodes_and_undef
val find_label_node: PdgTypes.Pdg.t -> stmt -> label -> PdgTypes.Node.t
val find_decl_var_node: PdgTypes.Pdg.t -> varinfo -> PdgTypes.Node.t
val find_input_node: PdgTypes.Pdg.t -> int -> PdgTypes.Node.t
val find_output_node: PdgTypes.Pdg.t -> PdgTypes.Node.t
val find_all_input_nodes: PdgTypes.Pdg.t -> PdgTypes.Node.t list
val find_entry_point_node: PdgTypes.Pdg.t -> PdgTypes.Node.t
val find_top_input_node: PdgTypes.Pdg.t -> PdgTypes.Node.t
val find_output_nodes: 
  PdgTypes.Pdg.t -> PdgIndex.Signature.out_key -> nodes_and_undef


val find_call_ctrl_node: PdgTypes.Pdg.t -> stmt -> PdgTypes.Node.t
val find_call_num_input_node: PdgTypes.Pdg.t -> stmt -> int -> PdgTypes.Node.t
val find_call_input_nodes:
  PdgTypes.Pdg.t -> stmt -> PdgIndex.Signature.in_key -> nodes_and_undef
val find_call_output_node: PdgTypes.Pdg.t -> stmt -> PdgTypes.Node.t

val find_call_stmts: kernel_function -> caller:kernel_function -> stmt list

val find_call_out_nodes_to_select:
  PdgTypes.Pdg.t -> PdgTypes.NodeSet.t -> PdgTypes.Pdg.t ->  stmt -> 
  PdgTypes.Node.t list

val find_in_nodes_to_select_for_this_call:
  PdgTypes.Pdg.t -> PdgTypes.NodeSet.t -> stmt -> PdgTypes.Pdg.t -> 
  PdgTypes.Node.t list

(** direct dependencies only:
 * This means the nodes that have an edge to the given node. *)
val direct_dpds: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list
val direct_data_dpds: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list
val direct_ctrl_dpds: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list
val direct_addr_dpds: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list

(** transitive closure *)

val find_nodes_all_dpds:
  PdgTypes.Pdg.t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
val find_nodes_all_data_dpds: 
  PdgTypes.Pdg.t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
val find_nodes_all_ctrl_dpds: 
  PdgTypes.Pdg.t -> PdgTypes.Node.t list -> PdgTypes.Node.t list
val find_nodes_all_addr_dpds: 
  PdgTypes.Pdg.t -> PdgTypes.Node.t list -> PdgTypes.Node.t list

(** forward *)

val direct_uses: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list
val direct_data_uses: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list
val direct_ctrl_uses: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list
val direct_addr_uses: PdgTypes.Pdg.t -> PdgTypes.Node.t -> PdgTypes.Node.t list

val all_uses: PdgTypes.Pdg.t -> PdgTypes.Node.t list -> PdgTypes.Node.t list

(** others *)

val custom_related_nodes: 
  (PdgTypes.Node.t -> PdgTypes.Node.t list) -> PdgTypes.Node.t list -> 
  PdgTypes.Node.t list
