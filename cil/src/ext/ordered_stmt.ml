(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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


(* Hashtable from stmts to ordered_stmts, an int corresponding to the
   topological ordering. *)
module Order = struct
  include Cil_datatype.Stmt.Hashtbl.Make(Datatype.Int)
  let get h stmt =
    try Cil_datatype.Stmt.Hashtbl.find h stmt
    with Not_found -> assert false
end

(* Table from ordered stmts to stmts. As he topological numbering is
   contiguous, so the 'back' table uses an array. *)
module Unorder = struct
  include Datatype.Array(Cil_datatype.Stmt)
  let get = Array.get
end

(* Array from ordered_stmts to connex_component number. *)
module Connex_components = struct
  include Datatype.Array(Datatype.Int)
end

module Ordered_stmt = Kernel_function.Make_Table
  (Datatype.Triple(Order)(Unorder)(Connex_components))
  (struct
    let name = "Dataflow2.Ordered_stmt"
    let dependencies = [ Ast.self ]
    let size = 17
   end)
;;

(* Skeleton for an OCamlGraph topological sort *)
module CFG = struct
  type t = kernel_function
  module V = Cil_datatype.Stmt

  let iter_vertex f kf =
    List.iter f (Kernel_function.get_definition kf).sallstmts

  (* In order to preserve a pleasant order on If and Switch, we follow
     Cil succs field in reverse order. *)
  let rec rev_iter f = function
    | [] -> ()
    | [s] -> f s
    | [s1; s2] -> f s2; f s1;
    | e :: q -> rev_iter f q; f e

  let iter_succ f _kf stmt = rev_iter f stmt.succs
end

module TopoForward = Graph.Topological.Make(CFG)
module Connex = Graph.Components.Make(CFG)

let get_ordered_stmt kf =
  let stmts = (Kernel_function.get_definition kf).sallstmts in
  let nb_stmts = List.length stmts in

  (* Compute conversion tables between stmt and ordered_stmt. *)
  let stmt_to_ordered = Cil_datatype.Stmt.Hashtbl.create nb_stmts in
  let ordered_to_stmt = Array.make nb_stmts (List.hd stmts) in
  let n = ref 0 in
  let f stmt =
    ordered_to_stmt.(!n) <- stmt;
    Cil_datatype.Stmt.Hashtbl.add stmt_to_ordered stmt !n;
    incr n;
  in
  TopoForward.iter f kf;

  (* Compute the strongly connected components. *)
  let (_nb_scc,f_scc) = Connex.scc kf in
  let sccs = Array.make  nb_stmts (-1) in
  Array.iteri (fun ordered stmt ->
    sccs.(ordered) <- f_scc stmt) ordered_to_stmt;
  (stmt_to_ordered, ordered_to_stmt, sccs);;

type ordered_stmt = int;;
type 'a ordered_stmt_array = 'a array;;
type ordered_to_stmt = stmt array;;
type stmt_to_ordered = ordered_stmt Cil_datatype.Stmt.Hashtbl.t;;

let get_conversion_tables = Ordered_stmt.memo get_ordered_stmt;;
let to_stmt = Unorder.get
let to_ordered = Order.get

(* TODO: The dataflow propagation strategy iterates on strongly
   connected components (scc); and for each scc, it iterates on all
   the statements of the scc in order, before starting a new iteration
   on that scc. To make the dataflow efficient, it is important that
   statements inside the scc are ordered topologically, ignoring back
   edges. This is not what is currently done: as the topological sort
   is global, there is no guarantee on the orders between statements
   inside of a cycle.

   Furthermore, to make the dataflow propogation strategy more
   understandable, the topological sort should be stable with regards
   to program order (i.e. the order between a and b may change only
   when there is a path from a to b, and no path from b to a). For
   cycles, the smallest element of the cycle should be the first
   element in program order, which has a predecessor in one of the
   previous strongly connected components. Ocamlgraph has a
   Make_stable functor, but it does not work when some components are
   not connected. *)
