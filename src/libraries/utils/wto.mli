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

(** Weak topological orderings (WTOs) are a hierarchical decomposition of the
    a graph where each layer is topologically ordered and strongly connected
    components are aggregated and ordered recursively. This is a very
    convenient representation to describe an evaluation order to reach a
    fixpoint. *)


(** Each component of the graph is either an individual node of the graph
    (without) self loop, or a strongly connected component where a node is
    designed as the head of the component and the remaining nodes are given
    by a list of components topologically ordered. *)
type 'n component =
  | Component of 'n * 'n partition
    (** A strongly connected component, described by its head node and the
        remaining sub-components topologically ordered *)
  | Node of 'n
    (** A single node without self loop *)

(** A list of strongly connected components, sorted topologically *)
and 'n partition = 'n component list


(** This functor provides the partitioning algorithm constructing a WTO. *)
module Make(Node:sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
    val pretty: Format.formatter -> t -> unit
  end):sig

  (** Implements Bourdoncle "Efficient chaotic iteration strategies with 
  widenings" algorithm to compute a WTO. *)
  val partition: init:Node.t -> succs:(Node.t -> Node.t list) -> Node.t partition

  val pretty_partition: Format.formatter -> Node.t partition -> unit
  val pretty_component: Format.formatter -> Node.t component -> unit    
end
