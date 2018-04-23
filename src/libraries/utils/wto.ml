(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

let fold_heads f acc l =
  let rec partition acc l =
    List.fold_left component acc l
  and component acc = function
    | Node _ -> acc
    | Component (h,l) ->
      partition (f acc h) l
  in
  partition acc l


let flatten wto =
  let rec f acc = function
    | [] -> acc
    | Node v :: l -> f (v :: acc) l
    | Component (v,w) :: l -> f (f (v :: acc) w) l
  in
  List.rev (f [] wto)

[@@@ warning "-42"]

(* Bourdoncle's WTO algorithm builds on Tarjan's SCC algorithm. In Tarjan:

   - We visit every node once, starting from root, by following the
     successors; this creates a spanning tree of the graph. SCCs are
     subtrees of this spanning tree, whose root is the head of the SCC
     (although in non-natural SCCs, it is possible to enter into a SCC
     without going through the head).

   - This spanning tree is obtained using DFS. What DFS guarantees is
     that there is no path from a child c of a node n to other
     children of n, provided that there is no path from c to an
     ancestor of n. Thus when we visit other children of n, we know
     that they are no path to them from the descendants of c.

   - We assign consecutive numbers to each node in the order in which
     they have been visited. As the iteration is depth-first search,
     this gives a depth-first numbering (DFN).

   - Each time we visit a node n, we push it on a stack. After the
     visit, n is popped, unless a path exists from n to an element
     earlier on the stack. So the stack contains elements currently
     visited or that belongs to a non-trivial scc. Moreover, they 
     are in topological order.

   About the proof of Tarjan:
   http://ls2-www.cs.uni-dortmund.de/~wegener/papers/connected.pdf
*)

module Make(N:sig
    type t (* = int *)
    val equal: t -> t -> bool
    val hash: t -> int
    val pretty: Format.formatter -> t -> unit
    (* val succ: t -> t list *)
  end) = struct

  let rec equal_component (x:N.t component) (y:N.t component) =
    match x,y with
    | Node x, Node y -> N.equal x y
    | Component (x,cx), Component (y,cy) -> N.equal x y && equal_partition cx cy
    | _ -> false

  and equal_partition x y =
    (try List.for_all2 equal_component x y with Invalid_argument _ -> false)


  let rec pretty_partition fmt part =
    List.iter (fun x -> Format.fprintf fmt "@ %a" pretty_component x) part
  and pretty_component fmt : N.t component -> unit = function
    | Node n -> N.pretty fmt n
    | Component(head,part) ->
      Format.fprintf fmt "@[<hov 1>(%a%a)@]"
        N.pretty head pretty_partition part

  module DFN = Hashtbl.Make(N);;

  type level = int

  (** Status of a visited vertex during the algorithm. *)
  type status =
    | Invisible (** The vertex have already been added into the partition and
                    is hidden until the end of the search. *)
    | Parent of level (** The vertex have been visited and given a [level]. For
                          the algorithm, this implies that there is a path
                          between this vertex and the current vertex. *)

  (** Result of one [visit]  *)
  type loop =
    | NoLoop (** The vertex is not in a loop *)
    | Loop of N.t * level (** The vertex is inside at least one loop, and
                              level is the smallest level of all these loops *)

  let min_loop x y =
    match x, y with
    | NoLoop, z | z, NoLoop -> z
    | Loop(_,xi), Loop(_,yi) ->
      if xi <= yi then x else y

  type state =
    { dfn: status DFN.t; (* Mapping from nodes to its dfn, depth-first
                            numbering.  Note that we replaced the DFN=0
                            test by presence in the Hashtable. *)
      mutable num: level;          (* Number of visited nodes. *)
      succs: N.t -> (N.t list);    (* Successors transition. *)
      stack: N.t Stack.t
    }

  (** Visit [vertex], and all the vertices reachable from [vertex]
      which have not been explored yet (this is a depth-first search).
      Also gives [partition], which is the partition built so far

      Returns a pair (loop,partition) where
      - [loop] tells whether we are in a loop or not and gives the vertex of
        this loop with the lowest level. This vertex is also the deepest in the
        stack and the neareast vertex from the root that is below [vertex] in
        the spanning tree built by the DFS);
      - [partition] is returned completed. *)
  let rec visit ~pref state vertex partition =
    match DFN.find state.dfn vertex with
    (* The vertex is already in the partition *)
    | Invisible -> NoLoop, partition (* skip it *)
    (* The vertex have been visited but is not yet in the partition *)
    | Parent i -> Loop (vertex,i), partition (* we are in a loop *)
    (* The vertex have not been visited yet *)
    | exception Not_found ->
      (* Put the current vertex into the stack *)
      Stack.push vertex state.stack;
      (* Number it and mark it as visited *)
      let n = state.num + 1 in
      state.num <- n;
      DFN.replace state.dfn vertex (Parent n);
      (* Visit all its successors *)
      let succs = state.succs vertex in
      let (loop,partition) = List.fold_left (fun (loop,partition) succ ->
          let (loop',partition) = visit ~pref state succ partition in
          let loop = min_loop loop loop' in
          (loop,partition)
        ) (NoLoop,partition) succs
      in
      match loop with
      (* We are not in a loop. Add the vertex to the partition *)
      | NoLoop ->
        let _ = Stack.pop state.stack in
        DFN.replace state.dfn vertex Invisible;
        (NoLoop,Node(vertex)::partition)
      (* We are in a loop and the current vertex is the head of this loop *)
      | Loop(head,_) when N.equal head vertex ->
        (* Unmark all vertices in the loop, and, if pref is given, try to
           return a better head *)
        let rec reset_SCC best_head =
          (** pop until vertex *)
          let element = Stack.pop state.stack in
          DFN.remove state.dfn element;
          if not (N.equal element vertex) then begin
            let best_head = match pref with
              (** the strict is important because we are conservative *)
              | Some cmp when cmp best_head element < 0 -> element
              | _ -> best_head
            in
            reset_SCC best_head
          end
          else
            best_head
        in
        let best_head = reset_SCC vertex in
        let vertex, succs =
          if N.equal best_head vertex
          then vertex,succs
          else best_head, state.succs best_head
        in
        (* Makes [vertex] invisible in the subsequents visits. *)
        DFN.replace state.dfn vertex Invisible;
        (* Restart the component analysis *)
        let component = List.fold_left
            (fun component succ ->
               let (loop,component) = visit ~pref state succ component in
               (* Since we reset the component we should have no loop *)
               assert (loop = NoLoop);
               component
            ) [] succs
        in
        (NoLoop,Component(vertex,component)::partition)
      | _ ->
        (* [vertex] is part of a strongly connected component but is not the
           head. Do not update partition; the vertex will
           be added during the second visit of the SCC. *)
        (loop,partition)

  type pref = N.t -> N.t -> int

  let partition ?pref ~init ~succs =
    let state = {dfn = DFN.create 17; num = 0; succs;
                 stack = Stack.create () } in
    let loop,component = visit ~pref state init [] in
    assert (loop = NoLoop);
    component

end
