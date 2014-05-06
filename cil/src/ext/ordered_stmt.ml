(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

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
