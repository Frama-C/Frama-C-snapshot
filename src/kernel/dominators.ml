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

(** Computation of dominators.
    Based on "A Simple, Fast Dominance Algorithm" by K. D. Cooper et al. *)

(* A domination tree, represented as a map from a statement to its
   immediate dominator. The first statement in a function, and
   statically unreachable statements (that do not have idoms), are
   mapped to None. *)
module Dom_tree = State_builder.Hashtbl
  (Cil_datatype.Stmt.Hashtbl)
  (Datatype.Option(Cil_datatype.Stmt))
  (struct
    let name = "dominators.dom_tree"
    let dependencies = [ Ast.self ]
    let size = 197
   end)
;;

(** Compute dominator information for the statements in a function *)
open Cil_types

let dkey = Kernel.register_category "dominators"
(****************************************************************)

module type DIRECTION = sig
  (* Number of statements in the function. *)
  val nb_stmts: int

  (* Conversion between statements and ordered statements. *)
  (* val to_ordered: stmt -> Ordered_stmt.ordered_stmt *)
  val to_stmt: Ordered_stmt.ordered_stmt -> stmt

  (* Iterates on all the statements, except the roots of the
     domination tree; in topological order for dominators, and reverse
     topological order for the post-dominators. *)
  val iter: (Ordered_stmt.ordered_stmt -> unit) -> unit

  (* Entry point (for dominators) or return (for post-dominators),
     that will be the root of the dominator/post-dominator tree. *)
  val root_stmt: Ordered_stmt.ordered_stmt;;

  val is_further_from_root:
    Ordered_stmt.ordered_stmt -> Ordered_stmt.ordered_stmt -> bool

  (* List of all predecessors for the dominators, list of successors
     for the post-dominators (for the post-dominators, it can be seen
     as the predecessors in the reversed control flow graph that goes
     from the sinks to the entry point). *)
  val preds: Ordered_stmt.ordered_stmt -> Ordered_stmt.ordered_stmt list

  val name:string
end

module Compute(D:DIRECTION) = struct

(* Computes the smallest common dominator between two statements. *)
let nearest_common_ancestor domtree ord1 ord2 =
  Kernel.debug ~dkey ~level:2 "computing common ancestor %d %d"
    (D.to_stmt ord1).sid (D.to_stmt ord2).sid;
  let finger1 = ref ord1 in
  let finger2 = ref ord2 in
  while (!finger1 != !finger2) do (
    while ( D.is_further_from_root !finger1 !finger2) do
      finger1 := (match domtree.(!finger1) with
      | None -> assert false
      | Some x -> x)
    done;
    while ( D.is_further_from_root !finger2 !finger1) do
      finger2 := (match domtree.(!finger2) with
      | None -> assert false
      | Some x -> x)
    done;)
  done;
  !finger1
;;

(* Note: None means either unprocessed, or that the statement has no
   predecessor or that all its ancestors are at None *)
(* based on "A Simple, Fast Dominance Algorithm" by K.D. Cooper et al *)
let domtree =
  let domtree = Array.create D.nb_stmts None in

  (* Initialize the dataflow: for each root, add itself to its own
     set of dominators. *)
  domtree.(D.root_stmt) <- Some D.root_stmt;
  let changed = ref true in
  while !changed do
    changed := false;
    D.iter (fun b ->
      let ordered_preds = D.preds b in
      let processed_preds =
	let was_processed p = match domtree.(p) with
	| None -> false
	| Some(_) -> true
	in
	List.filter was_processed ordered_preds
      in
      match processed_preds with
      | [] -> () (* No predecessor (e.g. unreachable stmt): leave it to None.*)
      | first::rest ->
	let new_idom =
	  List.fold_left (nearest_common_ancestor domtree) first rest
	in
	(match domtree.(b) with
	| Some(old_idom) when old_idom == new_idom -> ()
	| _ -> (domtree.(b) <- Some(new_idom); changed := true))
    );
  done;
  (* The roots are not _immediate_ dominators of themselves, so revert
     that now that the dataflow has finished. *)
  domtree.(D.root_stmt) <- None;
  domtree
;;

let display domtree =
  Kernel.debug ~dkey ~level:2 "Root is %d" (D.to_stmt 0).sid;
  Array.iteri (fun orig dest -> match dest with
  | Some(x) -> Kernel.debug ~dkey ~level:2 "%s of %d is %d"
    D.name (D.to_stmt orig).sid (D.to_stmt x).sid
  | None -> Kernel.debug ~dkey ~level:2 "no %s for %d"
    D.name (D.to_stmt orig).sid)
    domtree
;;

end

let compute_dom kf =
  let (stmt_to_ordered,ordered_to_stmt,_) =
    Ordered_stmt.get_conversion_tables kf
  in
  let to_stmt = Ordered_stmt.to_stmt ordered_to_stmt in
  let module Dominator = struct
    let to_ordered = Ordered_stmt.to_ordered stmt_to_ordered;;
    let to_stmt = to_stmt;;
    let nb_stmts = Array.length ordered_to_stmt;;
    let root_stmt = to_ordered (Kernel_function.find_first_stmt kf)
    (* Iterate on all statements, except the entry point. *)
    let iter f =
      for i = 0 to nb_stmts -1 do
	if i != root_stmt
	then f i
      done;;
    let is_further_from_root p1 p2 = p1 > p2
    let preds s = List.map to_ordered (to_stmt s).Cil_types.preds
    let name = "dom"
  end
  in
  let module ComputeDom = Compute(Dominator) in
  let domtree = ComputeDom.domtree in
  (* Fill the project table. *)
  Array.iteri( fun ord idom ->
    Dom_tree.add (to_stmt ord) (Extlib.opt_map to_stmt idom)) domtree;
;;

(* Note: The chosen semantics for postdominator is the following one: a
   post-dominates b if all the paths from b to the return statement
   goes through a.

   Statements on the paths that go only into infinite loop, or to
   __no_return function, do not have any post dominator (they are set
   to None).

   This definition of post-dominator gives a single root to the
   post-domination tree, which is required by the Cooper algorithm
   above. Beware that there are alternative, incompatible, definitions
   to post-domination, e.g. saying that a post dominates b if all the
   paths from b to any return statement or infinite loop go through
   a. *)
(* TODO:
   - For each statement, associate its immediate post-dominator (if it
   exists), and the list of sinks that dominates it

   - Attempt to find the post-dominator by intersection only if the
   list of sinks of the points is the same. Otherwise, state that
   there is no immediate post-dominator, and that the point is
   dominated by the union of the lists of sinks of its successors.
*)
let _compute_pdom kf =
  let (stmt_to_ordered,ordered_to_stmt,_) =
    Ordered_stmt.get_conversion_tables kf
  in
  let module PostDominator = struct
    let to_ordered = Ordered_stmt.to_ordered stmt_to_ordered;;
    let to_stmt = Ordered_stmt.to_stmt ordered_to_stmt;;
    let nb_stmts = Array.length ordered_to_stmt;;
    let root_stmt = to_ordered (Kernel_function.find_return kf)
    let iter f =
      for i = nb_stmts -1 downto 0 do
	if i != root_stmt
	then f i
      done;;
    let is_further_from_root p1 p2 = p1 < p2
    let preds s = List.map to_ordered (to_stmt s).Cil_types.succs
    let name = "postdom"
  end
  in
  let module ComputePDom = Compute(PostDominator) in
  let domtree = ComputePDom.domtree in
  ComputePDom.display domtree
;;

(****************************************************************)
(* For each statement we maintain a set of statements that dominate it *)

(* Try to find the idom, and fill the table if not already computed. *)
let get_idom s =
  try Dom_tree.find s
  with Not_found ->
    let kf = Kernel_function.find_englobing_kf s in
    let _ = (compute_dom kf) in
    try Dom_tree.find s
    with _ -> assert false
;;

(** Check whether one block dominates another. This assumes that the "idom"
    * field has been computed. *)
let rec dominates (s1: stmt) (s2: stmt) =
  s1.sid = s2.sid ||
  match (get_idom s2) with
  | None -> false
  | Some s2idom -> dominates s1 s2idom
