(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** abstract type of a cfg *)
type t

(** @raise Log.FeatureRequest for non natural loops and 'exception' stmts.
  * @return the graph and the list of unreachable nodes.
  * *)
val get : Kernel_function.t -> t

(** abstract type of the cfg nodes *)
type node
val pp_node : Format.formatter -> node -> unit
val same_node : node -> node -> bool

(** abstract type of the cfg edges *)
type edge
val pp_edge : Format.formatter -> edge -> unit
val same_edge : edge -> edge -> bool

(** get the starting edges *)
val start_edge : t -> edge

(** set of edges *)
module Eset : FCSet.S with type elt = edge

(** node and edges relations *)
val edge_src : edge -> node
val edge_dst : edge -> node
val pred_e : t -> node -> edge list
val succ_e : t -> node -> edge list

(** iterators *)
val fold_nodes : (node -> 'a -> 'a) -> t -> 'a -> 'a
val iter_nodes : (node -> unit) -> t -> unit
val iter_edges : (edge -> unit) -> t -> unit

(** Be careful that only Bstmt are real Block statements *)
type block_type = private
  | Bstmt of stmt | Bthen of stmt | Belse of stmt | Bloop of stmt | Bfct

type call_type =
  | Dynamic of exp
  | Static of kernel_function

val pp_call_type : Format.formatter -> call_type -> unit
val get_call_type : exp -> call_type

type node_type = private
  | Vstart | Vend | Vexit
  | VfctIn | VfctOut
  | VblkIn of block_type * block
  | VblkOut of block_type * block
  | Vstmt of stmt
  | Vcall of stmt * lval option * call_type * exp list
  | Vtest of bool * stmt * exp
  | Vswitch of stmt * exp
  | Vloop of bool option * stmt 
            (** boolean is is_natural.  None means the node has not been 
              * detected as a loop. *)
  | Vloop2 of bool * int

val node_type : node -> node_type
val pp_node_type : Format.formatter -> node_type -> unit

val node_stmt_opt : node -> stmt option
val start_stmt_of_node : node -> stmt option

(** @return the nodes that are unreachable from the 'start' node.
* These nodes have been removed from the cfg already. *)
val unreachable_nodes : t -> node_type list

(** similar to [succ_e g v]
* but tests the branch to return (then-edge, else-edge)
  * @raise Invalid_argument if the node is not a test.
* *)
val get_test_edges : t -> node -> edge * edge

(** similar to [succ_e g v]
but give the switch cases and the default edge *)
val get_switch_edges : t -> node -> (exp list * edge) list * edge

(** similar to [succ_e g v]
but gives the edge to VcallOut first and the edge to Vexit second. *)
val get_call_out_edges : t -> node -> edge * edge

val blocks_closed_by_edge : t -> edge -> block list

val is_back_edge : edge -> bool

(** detect is there are non natural loops or natural loops where we didn't
* manage to compute back edges (see [mark_loops]). Must be empty in the mode
* [-wp-no-invariants]. (see also [very_strange_loops]) *)
val strange_loops : t -> node list

(** detect is there are natural loops where we didn't manage to compute 
* back edges (see [mark_loops]). At the moment, we are not able to handle those
* loops. *)
val very_strange_loops : t -> node list

(** @return the (normalized) labels at the program point of the edge. *)
val get_edge_labels : edge ->  Clabels.c_label list

(** @return None when the edge leads to the end of the function. *)
val get_edge_next_stmt : t -> edge -> stmt option

(** wether an exit edge exists or not *)
val has_exit : t -> bool

(** Find the edges where the precondition of the node statement have to be
* checked. *)
val get_pre_edges : t -> node -> edge list

(** Find the edges where the postconditions of the node statement have to be
* checked. *)
val get_post_edges : t -> node -> edge list

(** Get the label to be used for the Post state of the node contract if any. *)
val get_post_logic_label : t -> node -> logic_label option

(** Find the edges [e] that goes to the [Vexit] node inside the statement
* begining at node [n] *)
val get_exit_edges : t -> node -> edge list

(** Find the edges [e] of the statement node [n] postcondition
* and the set of edges that are inside the statement ([e] excluded).
* For instance, for a single statement node, [e] is [succ_e n],
* and the set is empty. For a test node, [e] are the last edges of the 2
* branches, and the set contains all the edges between [n] and the [e] edges.
* *)
val get_internal_edges : t -> node -> edge list * Eset.t

val cfg_kf : t -> Kernel_function.t
val cfg_spec_only : t -> bool
  (** returns [true] is this CFG is degenerated (no code available) *)

(** signature of a mapping table from cfg edges to some information. *)
module type HEsig =
sig
  type ti
  type t
  val create : int -> t
  val find : t -> edge -> ti
  val find_all : t -> edge -> ti list
  val add : t -> edge -> ti -> unit
  val replace : t -> edge -> ti -> unit
  val remove : t -> edge -> unit
  val clear : t -> unit
end

module HE (I : sig type t end) : HEsig with type ti = I.t

(** type of functions to print things related to edges *)
type pp_edge_fun = Format.formatter -> edge -> unit
