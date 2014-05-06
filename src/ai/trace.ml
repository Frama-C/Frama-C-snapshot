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

open Cil_types;;

(* An interval lattice describing the number of times a basic_block
   has been executed. *)
type execution_count = int * int;;
let empty_execution_count = (0,0);;


(* Nodes in the intra-procedural trace graph. They are identified by
   the stmt that begin them, together with an approximation of the
   number of times the block has been executed. The execution count
   allows to differentiate multiple executions of the same basic
   block, which helps maintaining precise traces.

   The start of the trace is identified with a special element
   Initial. This is necessary for the evaluation of functions without
   a body.

   TODO: Add more elements, such as widen hint.
*)
type trace_node =
| In_basic_block of stmt * execution_count
| Disjunction of Property.t * predicate named * execution_count
| Initial

(* Note: this could be generalized as a functor put in AI,
   representing an abstract domain of paths in a graph. *)
module Ordered_Trace_Node = struct
  type t = trace_node
  let compare = Pervasives.compare
end;;

module Trace_Node_Set = FCSet.Make(Ordered_Trace_Node);;
module Trace_Node_Map = FCMap.Make(Ordered_Trace_Node);;

(* TODO: stmt is used only for verification during propagation, and
   execution_count is also used only during propagation. Structuring
   things differently could allow to save some space. *)

(* Represents an over-approximation of a set of intra-procedural traces. *)
type intra = {
  (* A DAG representing a set of traces, stored as a map "from block
     -> set of to blocks". *)
  dag: Trace_Node_Set.t Trace_Node_Map.t;

  (* The current function we're in. Useful to iterate on a trace from
     the beginning. TODO: replace by "called_by". *)
  (* called_by: call_stack *)
  current_kf: kernel_function;

  (* The current basic block we are in. *)
  current_node: trace_node;

  (* The current statement. Used only for verification. *)
  current_stmt: stmt option;

  (* Number of times each statement at the beginning of a basic block
     has been executed. *)
  execution_count: execution_count Cil_datatype.Stmt.Map.t
}

(* The current function, the instruction that called it,
   and the trace leading to that instruction. (instr,trace) is None
   for the caller of the entry point. *)
and _call_stack = (kernel_function * (instr * intra) option);;

type t =
| Bottom
| Traces of intra
| Top

let bottom = Bottom;;
let top = Bottom;;

(****************************************************************)
(* Pretty-printing of traces. *)

module G = struct
  type t = Trace_Node_Set.t Trace_Node_Map.t
  module V = struct
    type t = trace_node
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (==)
  end
  let iter_succ f graph node =
    let set =
      try Trace_Node_Map.find node graph
      with Not_found -> Trace_Node_Set.empty (* assert false *)
    in Trace_Node_Set.iter f set
  let iter_vertex f graph =
    Trace_Node_Map.iter (fun vertex _succs -> f vertex) graph;;
end

module Sorted = Graph.Topological.Make(G);;

let pretty_trace_node fmt = function
  | In_basic_block(stmt,(mincount,maxcount)) ->
    let strstmt = (string_of_int stmt.sid) in
    Format.fprintf fmt "%s[%d-%d]" strstmt mincount maxcount
  | Initial -> Format.fprintf fmt "initial"
  | Disjunction (ip,pred,(start,end_)) ->
    let name = match pred.name with
      | a::_ -> a
      | _ -> "unnamed"
    in
    Format.fprintf fmt "%a disjunction( %s)[%d-%d]"
      Property.short_pretty ip name start end_
;;

(* TODO: Factorize repeats introduced by loops. *)
let pretty_graph pp_elt fmt graph =
  let list = Sorted.fold (fun x l -> x::l) graph [] in
  let list = List.rev list in

  (* Compute the set of nodes with more than one incoming edge. *)
  let (_,join_nodes) = List.fold_left (fun (seen,seen_twice) x ->
    let set = (try Trace_Node_Map.find x graph with Not_found -> assert false) in
    Trace_Node_Set.fold (fun x (seen,seen_twice) ->
      if Trace_Node_Set.mem x seen
      then (seen, Trace_Node_Set.add x seen_twice)
      else (Trace_Node_Set.add x seen, seen_twice)) set (seen,seen_twice))
    (Trace_Node_Set.empty, Trace_Node_Set.empty) list
  in

  (* Display the string, until the element has two outgoing edges, or two incoming edges. *)
  let has_two_incoming_edges x = Trace_Node_Set.mem x join_nodes in

  (* A "string" is a linear list of blocks, in which all elements
     (except the first and last) have one outgoing edge and one
     incoming edge. Strings are displayed on the same lines; "\n" is
     used to "cut" strings. *)
  let rec display_string = function
    | [] -> []
    | [x] when
	let set =
	  try Trace_Node_Map.find x graph
	  with Not_found -> assert false
	in Trace_Node_Set.cardinal set = 1 ->
      let set = Trace_Node_Map.find x graph in
      let elt = Trace_Node_Set.choose set in
      Format.fprintf fmt "%a -> %a@." pp_elt x pp_elt elt; []
    | x::((y::_) as rest) when
	not (has_two_incoming_edges x) &&
	let set =
	  try Trace_Node_Map.find x graph
	  with Not_found -> assert false
	in Trace_Node_Set.cardinal set = 1 && Trace_Node_Set.mem y set
	-> Format.fprintf fmt "%a -> " pp_elt x; display_string rest
    | x::_ as l (* x has two outgoing or incoming edges. *)
      -> Format.fprintf fmt "%a@." pp_elt x; l
  in
  let rec loop = function
    | [] -> ()
    | l -> loop (display_string l)
  in
  loop list;
;;


let pretty_intra fmt trace =
  match trace.current_stmt with
  | None -> Format.fprintf fmt "stmt null "
  | Some(stmt) -> Format.fprintf fmt "stmt %d " stmt.sid;
  Format.fprintf fmt "current bb: %a" pretty_trace_node trace.current_node;
  Format.fprintf fmt "dag: @. %a" (pretty_graph pretty_trace_node) trace.dag
;;

let pretty fmt = function
  | Bottom -> Format.fprintf fmt "bottom"
  | Top -> Format.fprintf fmt "top"
  | Traces(t) -> pretty_intra fmt t
;;

(****************************************************************)
(* Joining two traces. *)

(* Two trace nodes are compatible if they can be joined. The only
   requirement is that they point to the same location in the program. *)
let compatible_trace_node bb1 bb2 = match (bb1,bb2) with
| In_basic_block(s1,_o1), In_basic_block(s2,_o2) -> s1.sid == s2.sid
| Disjunction(ip1,_p1,_o1), Disjunction(ip2,_p2,_o2) -> Property.equal ip1 ip2
| Initial, Initial -> true
| _ -> false
;;

(* Note: join is an over approximation; when joining 0 -> 1 -> 2 -> 3
   with 0 -> 4 -> 2 -> 5, we get 0 -> (1 | 4) -> 2 -> (3 | 5), but the
   path 0 -> 1 -> 2 -> 5 may not exist. *)
let join_intra t1 t2 =
  (* Kernel.debug "joining %a@. with %a@." pretty_intra t1 pretty_intra t2; *)
  assert (t1.current_kf == t2.current_kf);
  assert (match t1.current_stmt,t2.current_stmt with
  | Some({sid=sid1}), Some { sid = sid2 } when sid1 == sid2 -> true
  | _ -> false);
  assert (compatible_trace_node t1.current_node t2.current_node);
  let merged_dag =
    let merge_fun _key set1 set2 = match set1, set2 with
      | Some set1, Some set2 -> Some (Trace_Node_Set.union set1 set2)
      | None, a | a, None -> a
    in
    Trace_Node_Map.merge merge_fun t1.dag t2.dag
  in
  let merged_execution_count =
    let join_execution_count (a1,b1) (a2,b2) = (min a1 a2, max b1 b2) in
    let merge_fun _key iv1 iv2 = match iv1, iv2 with
      | Some iv1, Some iv2 -> Some (join_execution_count iv1 iv2)
      | None, a | a, None -> a
    in
    Cil_datatype.Stmt.Map.merge merge_fun t1.execution_count t2.execution_count
  in
  { dag = merged_dag;
    current_kf = t1.current_kf; current_stmt = t1.current_stmt;
    current_node = t1.current_node;
    execution_count = merged_execution_count
  }
;;

let join t1 t2  = match t1,t2 with
  | Top, _ | _, Top -> Top
  | Bottom, t | t, Bottom -> t
  | Traces t1, Traces t2 -> Traces (join_intra t1 t2)
;;

(****************************************************************)
(* Precedence. *)

(* Intersection of two graphs (the graph with the nodes and vertices
   present in both graphs) *)
let inter dag1 dag2 =
  let f from t1_tos cur_inter_dag =
    try
      let t2_tos = Trace_Node_Map.find from dag2 in
      let inter_tos = Trace_Node_Set.inter t1_tos t2_tos in
      if Trace_Node_Set.is_empty (inter_tos)
      then cur_inter_dag
      else Trace_Node_Map.add from inter_tos cur_inter_dag
    with Not_found -> cur_inter_dag
  in
  Trace_Node_Map.fold f dag1 Trace_Node_Map.empty
;;


(* Use OCaml graph path checker. From the description it uses
   Dijkstra's algorithm, while we would prefer to perform an early
   exit when the path is found (e.g. interrupting a depth-first
   search). On the other hand, the results are cached, and we reuse it
   for the precedence test.*)
module PathChecker = Graph.Path.Check(struct
  type t = Trace_Node_Set.t Trace_Node_Map.t;;
  module V = struct
    type t = trace_node
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end
  let iter_succ f g v =
    try
      let set = Trace_Node_Map.find v g in
      Trace_Node_Set.iter f set
    with Not_found -> ()
end)

(* [precedes t1 t2] returns a pair of booleans:

   - the first is true iff an event whose trace is in [t1] may have
   happened before an event whose trace is in [t2]. This is possible
   only if there is a path in both [t1] and [t2] that leads to the
   current block of [t1].

   - the second is true iff an event whose trace is in [t2] may have
   happened before an event whose trace is in [t1].

   Note: because of the overapproximation, we cannot answer
   definitively that t1 indeed happened before t2. We can only answer
   definitively when it could not.

   The presence of common path is computed by first computing the
   intersection of the dags, and then checking if there is a path from
   the root to s1 or s2. If for instance there is no path to s1, it
   means that one of the traces never went to s1 with the same path.
*)
let _precedes t1 t2 =
  assert (t1.current_kf == t2.current_kf);
  (* TODO: Early check: is the current basic block of t1 in t2 at all?
     If no, we can early exit. Else, expensive check. *)
  let intersection_dag = inter t1.dag t2.dag in
  let from =
    let first_stmt = Kernel_function.find_first_stmt t1.current_kf in
    In_basic_block (first_stmt, empty_execution_count)
  in
  let checker = PathChecker.create intersection_dag in
  (PathChecker.check_path checker from t1.current_node,
   PathChecker.check_path checker from t2.current_node)


(* TODO: compute the shortest of two traces. Useful to sort traces
   that lead to an alarm.  *)

(****************************************************************)
(* Updating the trace during the abstract interpretation. *)

let incr_execution_count stmt execution_count =
  let incr (a,b) =
    if b = max_int
    then Kernel.fatal "Too many executions per basicblock"
    else (a+1,b+1)
  in
  let old =
    try Cil_datatype.Stmt.Map.find stmt execution_count
    with Not_found -> empty_execution_count
  in
  (old, Cil_datatype.Stmt.Map.add stmt (incr old) execution_count)
;;

(* Returns the dag with a link added from the current node to the [node] *)
let add_node node trace =
  let dag = trace.dag in
  let current_bb = trace.current_node in
  let set =
    try Trace_Node_Set.add node (Trace_Node_Map.find current_bb dag)
    with Not_found -> Trace_Node_Set.singleton node
  in
  let newdag = Trace_Node_Map.add current_bb set dag in
  newdag
;;

let add_basic_block stmt trace =
  let count, exec_count = incr_execution_count stmt trace.execution_count in
  let node = In_basic_block(stmt,count) in
  let newdag = add_node node trace in
  { trace with dag = newdag;
    current_node = node;
    execution_count = exec_count;
    current_stmt = Some stmt
  }
;;

(* A statement with several predecessors is at the beginning of a basic block.
   A statement with several successors is at the end of a basic block.
   Two consecutive statements are in the same basic block iff the first is not
   at the end of a basic block, and the second not at the beginning.*)
let has_one_pred_and_pred_has_one_succ stmt = match stmt.preds with
  | [pred] -> (match pred.succs with
    | [_] -> true
    | _ -> false)
  | _ -> false

(* Map on pointed sets. *)
let map_pointed f = function
  | Bottom -> Bottom
  | Top -> Top
  | Traces(t) -> Traces(f t)
;;

(* A basic block start with a statement with two predecessors, or zero
   for the function entry point. *)
let add_statement stmt = map_pointed (fun trace ->
  (* Kernel.debug "Adding statement %d preds %d" *)
  (*    stmt.sid (List.length stmt.preds); *)
  if has_one_pred_and_pred_has_one_succ stmt
  then { trace with current_stmt = Some stmt }
  else add_basic_block stmt trace)
;;

(* The execution count of the current node. *)
let get_current_execution_count trace =
  match trace.current_node with
  | In_basic_block (_,count) -> count
  | Disjunction (_,_,count) -> count
  | Initial -> empty_execution_count
;;


let add_disjunction ip named_pred = map_pointed (fun trace ->
  let count = get_current_execution_count trace in
  let node = Disjunction (ip, named_pred, count) in
  let newdag = add_node node trace in
  { trace with dag = newdag; current_node = node }
)
;;

(* Should be synchronized with the default value for val-show-trace. *)
let compute_trace = ref false;;
let set_compute_trace b = compute_trace := b;;

(* Initial intra-procedural trace for a given function.

   TODO: Update to keep an inter-procedural trace. *)
let initial kf =
  if not !compute_trace
  then Top
  else
    let stmt = None in
    Traces
      { dag = Trace_Node_Map.empty;
	current_kf = kf;
	current_stmt = stmt;
	current_node = Initial;
	execution_count = Cil_datatype.Stmt.Map.empty
      }
;;
