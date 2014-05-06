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

(** Statements graph.
    @plugin development guide *)

open Cil_types
open Cil_datatype

val stmt_can_reach: kernel_function -> stmt -> stmt -> bool
  (** [stmt_can_reach kf s1 s2] is [true] iff the control flow can reach
      [s2] starting at [s1] in function [kf]. *)

val stmt_can_reach_filtered : (stmt -> bool) -> stmt -> stmt -> bool
  (** Just like [stmt_can_reach] but uses a function to filter the nodes of the
      graph it operates on.
      Note that the output of the filter function must be functionally dependent
      on its input *)

val stmt_is_in_cycle : stmt -> bool
  (** [stmt_is_in_cycle s] is [true] iff [s] is reachable through a non trival path
   * starting at [s]. *)

val stmt_is_in_cycle_filtered : (stmt -> bool) -> stmt -> bool
  (** Just like [stmt_is_in_cycle] but uses a function to filter the nodes of
      the graph it operates on.
      Note that the output of the filter function must be functionally dependent
      on its input *)

val reachable_stmts: kernel_function -> stmt -> stmt list

(** Get the statements that compose [s]. For a simple statement (not containing
    blocks), it is only the statement itself. *)
val get_stmt_stmts : stmt -> Stmt.Set.t
val get_block_stmts : block -> Stmt.Set.t

(** Find the last statements in [s], meaning that if [s'] is in the returned
    statements, [s'] is in [s] statements, but a least one of its successor is
    not. *)
val get_all_stmt_last_stmts : stmt -> stmt list
val get_all_block_last_stmts : block -> stmt list

(** Subset of [get_all_stmt_last_stmts] according to [termination_kind].
    [termination_kind = None] means [Goto].
    @raise Invalid_argument for [termination_kind = Some Exits] since
    every call possibly have an [Exits] termination: it should be handled
    differently. *)
val get_stmt_last_stmts : termination_kind option -> stmt -> stmt list
val get_block_last_stmts : termination_kind option -> block -> stmt list

(** Find the entry edges that go inside [s] statements,
* meaning that if the pair [(s1,s2)] is in the returned information,
* [s2] is a successor of [s1] and [s2] is in [s] statements, but [s1] is not.
* @since Nitrogen-20111001
**)
val get_stmt_in_edges : stmt -> (stmt * stmt) list
val get_block_in_edges : block -> (stmt * stmt) list

(** Like [get_stmt_in_edges] but for edges going out of [s] statements.
* Similar to [get_all_stmt_last_stmts] but gives the edge information
* instead of just the first statement.
* @since Nitrogen-20111001
*)
val get_all_stmt_out_edges : stmt -> (stmt * stmt) list
val get_all_block_out_edges : block -> (stmt * stmt) list

(** Split the loop predecessors into:
    - the entry point : coming from outside the loop
    - the back edges.
    Notice that there might be nothing in the entry point when the loop is the
    first statement.
    @raise Invalid_argument if the statement is not a loop. *)
val loop_preds : stmt -> stmt list * stmt list

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
