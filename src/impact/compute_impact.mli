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

type nodes = Pdg_aux.NS.t
type result = nodes Kernel_function.Map.t

val initial_nodes:
  skip:Locations.Zone.t -> kernel_function -> stmt -> PdgTypes.Node.t list

val nodes_impacted_by_stmts:
  ?skip:Locations.Zone.t -> ?restrict:Locations.Zone.t -> ?reason:bool ->
  kernel_function -> stmt list ->
  result * (** Initial *) nodes Kernel_function.Map.t * Reason_graph.reason

val nodes_impacted_by_nodes:
  ?skip:Locations.Zone.t -> ?restrict:Locations.Zone.t -> ?reason:bool ->
  kernel_function -> PdgTypes.Node.t list ->
  result * (** Initial *) nodes Kernel_function.Map.t * Reason_graph.reason

val stmts_impacted:
  ?skip:Locations.Zone.t -> reason:bool ->
  kernel_function -> stmt list -> stmt list

val nodes_impacted:
  ?skip:Locations.Zone.t -> reason:bool ->
  kernel_function -> PdgTypes.Node.t list -> nodes


val result_to_nodes: result -> nodes
val nodes_to_stmts: nodes -> stmt list
val impact_in_kf: result -> Cil_types.kernel_function -> nodes

val skip: unit -> Locations.Zone.t
  (** computed from the option [-impact-skip] *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
