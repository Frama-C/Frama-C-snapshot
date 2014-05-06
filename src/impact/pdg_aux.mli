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
open PdgTypes
open Locations

(** Useful functions that are not directly accessible through the other
    Pdg modules. *)


(** Refinement of a PDG node: we add an indication of which zone is really
    impacted *)
type node = Node.t * Zone.t

val pretty_node: node Pretty_utils.formatter


(** Sets of pairs [Node.t * Zone.t], with a special semantics for zones:
    [add n z (add n z' empty)] results in [(n, Zone.join z z')] instead
    of a set with two different elements. All operations see only  instance
    of a node, with the join of all possible zones. Conversely, a node should
    not be present with an empty zone. *)
module NS: sig
  include Datatype.S

  val empty: t
  val is_empty: t -> bool
  val pretty: t Pretty_utils.formatter

  val add': node -> t -> t

  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t

  val remove: Node.t -> t -> t

  val mem: Node.t -> t -> bool
  val mem': node -> t -> bool
  val intersects: t -> t -> bool
  val for_all': (node -> bool) -> t -> bool

  val iter': (node -> unit) -> t -> unit
  val fold: (node -> 'a -> 'a) -> t -> 'a -> 'a
  val filter': (node -> bool) -> t -> t
end


(** Abstract view of a call frontier. An element [n, S] of the list
    is such that [n] is impacted if one of the nodes of [S] is impacted. *)
type call_interface = (PdgTypes.Node.t * NS.t) list


(** [all_call_input_nodes caller callee call_stmt] find all the nodes
    above [call_stmt] in the pdg of [caller] that define the inputs
    of [callee]. Each input node in [callee] is returned with the set
    of nodes that define it in [caller].  *)
val all_call_input_nodes:
  caller:Db.Pdg.t ->  callee:kernel_function * Db.Pdg.t -> stmt ->
  call_interface

(** [all_call_out_nodes ~callee ~caller stmt] find all the nodes of [callee]
    that define the Call/Out nodes of [caller] for the call to [callee]
    that occurs at [stmt]. Each such out node is returned, with the set
    of nodes that define it in [callee] *)    
val all_call_out_nodes :
  callee:Db.Pdg.t ->  caller:Db.Pdg.t -> stmt -> call_interface
