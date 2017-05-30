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

(* Input signature for Region_analysis. *)

(* type 'node entry_edge = (would be useful if we also want the "before" state)
   | Entry of 'node (Function entry)
   | Back_edge of 'node (Back edge to a loop)
   | Norm_Edge of 'node * 'node
*)

(* Edges exiting from a node. *)
type 'node edge =
  | Edge of 'node * 'node         (* Normal edge. *)
  | Exit of 'node                 (* Function Exit. *)

module type Node = sig
  type node

  val pretty: Format.formatter -> node -> unit

  (* An imperative dictionary with nodes as keys, and a default value. *)
  module Dict: sig
    type 'a t
    (* Create an initial array of size n, with a default value. *)
    val create: int -> 'a -> 'a t
    val get: 'a t -> node -> 'a
    val set: 'a t -> node -> 'a -> unit
    val iter: 'a t -> (node -> 'a -> unit) -> unit
    val copy: 'a t -> 'a t (* Shallow copy *)
  end

  module Set:FCSet.S with type elt = node

  (* The graph of nodes. *)
  module Graph:sig
    val iter_succs: node -> (node -> unit) -> unit
    val iter_preds: node -> (node -> unit) -> unit

    (* Entry, exits, and nodes of the whole graph. *)
    val all_nodes: Set.t
    val entry_node: node
    val exit_nodes: node list
  end

  module DomTree:sig
    val dominates: node -> node -> bool
    (* Postfix iteration on the dominator tree. *)
    val domtree_postfix_iter: (node -> unit) -> unit
  end

  (* An imperative dictionary of edges. No default value: calling get
     on an edge that was never set is forbidden. *)
  module Edge_Dict:sig
    type 'a t
    val set: 'a t -> node edge -> 'a -> unit
    val get: 'a t -> node edge -> 'a
    val create: unit -> 'a t
    val iter: 'a t -> (node edge -> 'a -> unit) -> unit
  end

  (* For now. Can be something else. TODO: Use it to perform loop analysis. *)
  type abstract_value

  (* Compile a node into a function that, given an input abstract
     value, return the output abstract value for each outgoing edge of
     the node. *)
  val compile_node: node -> abstract_value -> (node edge * abstract_value) list

  (* Merge the abstract values coming into a node from different edges. *)
  val join: abstract_value list -> abstract_value

  (* Given a function providing the effect of a loop, and the initial
     value at the end of the loop, return an abstract value describing
     any iteration of a loop. *)
  val mu: (abstract_value -> abstract_value) -> abstract_value -> abstract_value
end
