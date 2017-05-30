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


(* This algorithm is similar to the region-based analysis of the
   dragon book ("Compilers: Principles, Techniques, and Tools (2nd
   Edition)", by Aho, Lam, Sethi and Ullman). But there are some
   important differences:

   - We never build regions; the nesting of natural loops suffice.

   - We do not compose transfer functions. Instead, we rely on the
   fact that Ocaml has first-class functions, and associate to "loop
   edges" functions describing the behaviour of loops.

   The composition of region of the Dragon Book does not fit well the
   translation to terms, for which composition of transfer function
   would mean creation of closures or a costly substitution; this
   algorithm avoids function composition.

   The algorithm could be extended to handle non-natural loops. Instead
   of using the notion of back edges, we could use that of retreating
   edge, starting from a spanning tree of the graph. It should be
   possible to compile strongly connected components with multiple
   entry point several times, introducing the variable at different
   entry points, or inlining part of the loop to "reuse" another entry
   point when feasible. Computation of nested scc can be obtained
   using "Earnest, Balke and Anderson: Analysis of Graphs by Ordering
   of Nodes". It is maybe also possible to use the wto ordering of
   bourdoncle for this purpose. *)

include Region_analysis_sig;;

module Make(N:Node):sig
  (* Function computing from an entry abstract value the "after"
     state, which is a map from each outgoing edge to its respective
     value. *)
  val after: N.abstract_value -> N.abstract_value N.Edge_Dict.t
end =
struct

  let graph_size = N.Set.cardinal N.Graph.all_nodes
  let iter_nodes f = N.Set.iter f N.Graph.all_nodes
  
  (****************************************************************)
  (* Back edges. *)

  (* Return a dict from head nodes to the set of origins of back edges. *)
  let back_edges:N.Set.t N.Dict.t =
    let back_edges = N.Dict.create graph_size N.Set.empty in
    iter_nodes (fun n ->
        N.Graph.iter_succs n (fun head ->
            if N.DomTree.dominates head n
            then N.Dict.set back_edges head 
                (N.Set.add n (N.Dict.get back_edges head))));
    back_edges
  ;;

  let is_back_edge from to_ =
    N.Set.mem from (N.Dict.get back_edges to_)
  ;;

  (****************************************************************)
  (* Natural loops. *)

  type natural_loop = N.Set.t;;

  (* For each header node, the list of the nested natural_loops with
     that header, from outermost to innermost. *)
  let natural_loops: (natural_loop list) N.Dict.t =
    (* Perform a DFS using the "preds" relation, and returns the
       set of visited nodes (we use the set to "mark" visits).  We
       first mark the head so as not to go beyond it. *)
    let natural_loop_for head origin =
      let visited = ref N.Set.empty in
      visited := N.Set.add head !visited;
      let rec loop n =
        visited := N.Set.add n !visited;
        N.Graph.iter_preds n (fun k -> if N.Set.mem k !visited then () else loop k);
      in loop origin;
      !visited
    in

    let natural_loops = N.Dict.create graph_size [] in

    (* Attach natural loops to their headers, they can be nested; if
       they are not, merge them. *)
    let add_natural_loop header nl =
      let nls = N.Dict.get natural_loops header in
      let rec loop = function
        | [] -> [nl]
        | (a::b) as nls ->
          if N.Set.subset a nl
          then nl::nls
          else if N.Set.subset nl a
          then a::(loop b)
          (* Neither is a subset of the other: we merge the loops. *)
          else (N.Set.union a nl)::b
      in N.Dict.set natural_loops header (loop nls)
    in

    N.Dict.iter back_edges (fun header origins ->
        N.Set.iter (fun origin ->
            let natural_loop = natural_loop_for header origin in
            add_natural_loop header natural_loop) origins);
    natural_loops
  ;;

  (* Let us consider the tree of inclusion of natural loops (i.e. each
     node in the tree is a natural loop, and children of that tree are
     the natural loops that it contains). This function performs a
     postfix iteration on that tree (i.e. with outermost natural loops
     lasts).

     Note that the children of the tree are disjoint (i.e. have an
     empty intersection). *)
  let natural_loops_postfix_iter f =
    N.DomTree.domtree_postfix_iter (fun header ->
        let nls = N.Dict.get natural_loops header in
        List.iter (fun nl -> f header nl) (List.rev nls))
  ;;

  (****************************************************************)
  (* Transfer functions of regions.

     A region is a set of nodes with a _header_ that dominates the
     other nodes. *)

  (* For each header node, the transfer function summarizing the
     natural loop starting at this header node (if any). Note that
     there may be several loops starting at a header node; if so
     selects the outermost that has been analyzed so far.

     Also note that loop_transfer_functions contain closures that use
     what is contained in [loop_transfer_functions], which causes a
     recursion avoiding by giving to the closures _copies_ of
     [loop_transfer_functions]. *)
  let loop_transfer_functions:
    (N.abstract_value -> N.abstract_value) option N.Dict.t =
    N.Dict.create graph_size None;;

  (* Given an entire region, returns a function computing the "after"
     state given an abstract value input of the header node. *)
  let compile_region_after loop_transfer_functions header tset =
    fun input_term ->
      let edge_term = N.Edge_Dict.create () in

      (* The idea is to iterate on each node in topological order, so
         that we always find the data on input edges. To avoid a
         topological sort, we allow do_node to call itself on a previous
         node when the result is not ready, and rely on the fact that
         edge_term memoizes the results to ensure that do_node is
         eventually called only once per node. *)
      let rec do_node n =
        let input =
          if n == header then input_term
          else
            let inputs = ref [] in
            N.Graph.iter_preds n (fun pred ->
                if (N.Set.mem n tset) && (not (is_back_edge pred n))
                then
                  let input = get_edge pred n in
                  inputs := input::!inputs );
            N.join !inputs
        in
        let input = match N.Dict.get loop_transfer_functions n with
          | None -> input
          | Some f -> N.mu f input
        in
        let outputs = N.compile_node n input in
        List.iter (fun (edge,output) ->
            N.Edge_Dict.set edge_term edge output) outputs

      (* Compute for previous node if result not yet available. *)
      and get_edge pred n =
        let edge = Edge(pred, n) in
        try N.Edge_Dict.get edge_term edge
        with Not_found -> do_node pred; N.Edge_Dict.get edge_term edge
      in

      (* We can now iterate on any order. *)
      N.Set.iter do_node tset;
      edge_term
  ;;

  (* Given a region that is a natural loop, compute the transfer
     function for the body of the loop. *)
  let compile_loop_transfer_function loop_transfer_functions header tset =
    fun input_term ->
      let edge_term =
        compile_region_after loop_transfer_functions header tset input_term in

      (* Collect the abstract values on the back edges. *)
      let body_exit_term =
        let inputs = ref [] in
        N.Graph.iter_preds header (fun pred ->
            if (N.Set.mem pred tset) && (is_back_edge pred header)
            then
              let edge = Edge(pred,header) in
              let input = N.Edge_Dict.get edge_term edge in
              inputs := input::!inputs );
        N.join !inputs
      in

      body_exit_term
  ;;

  (* Compute the final [loop_transfer_functions]. *)
  natural_loops_postfix_iter (fun header tset ->
      (* Copy [loop_transfer_functions] for the closure, so that it is
         not affected by further modifications. *)
      let copy_ltf = N.Dict.copy loop_transfer_functions in
      let f = compile_loop_transfer_function copy_ltf header tset in
      N.Dict.set loop_transfer_functions header (Some f))
  ;;

  let after input =
    compile_region_after loop_transfer_functions
      N.Graph.entry_node N.Graph.all_nodes input
  ;;

end
