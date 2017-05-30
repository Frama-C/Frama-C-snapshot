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

type 'n component =
  | Component of 'n * 'n partition
  | Node of 'n
and 'n partition = 'n component list

let node n = Node(n)

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

  module DFN = Hashtbl.Make(N);;

  type state =
    { dfn: int DFN.t; (* Mapping from nodes to its dfn, depth-first
                         numbering.  Note that we replaced the DFN=0
                         test by presence in the Hashtable. *)
      mutable num: int;            (* Number of visited nodes. *)
      succs: N.t -> (N.t list);    (* Successors transition. *)
      stack: N.t Stack.t
    }

  (* Visit [vertex], and all the vertices reachable from [vertex]
     which have not been explored yet (this is a depth-first search).
     Also gives [partition], which is the partition built so far

     Returns a pair (lowlink,partition) where
     - [lowlink] is the deepest node in the stack, that is the
       successor of an explored node (i.e. a node which is below than
       [vertex] in the spanning tree built by the DFS);
     - [partition] is returned completed.

     Except for the call to component, and changing the DFN of vertex
     to max_int, this function exactly performs Tarjan's SCC
     algorithm. *)
  let rec visit state vertex partition =
    Stack.push vertex state.stack;
    let n = state.num + 1 in
    state.num <- n;
    DFN.replace state.dfn vertex n;
    let succs = state.succs vertex in
    let (head,loop,partition) = List.fold_left (fun (head,loop,partition) succ ->
        let (min,partition) =
          (* If already visited. *)
          if DFN.mem state.dfn succ
          (* Is another branch, which can have been visited before or after [vertex]. *)
          then (succ,partition)
          else visit state succ partition in
        (* OPTIM: On doit pouvoir sauvegarder le dfn de head et min pour gagner des lookups. *)
        assert (DFN.mem state.dfn min);
        assert (DFN.mem state.dfn head);
        (* If an element below in the spanning tree links to deepest
           in the stack than [vertex]. *)
        if (DFN.find state.dfn min) <= (DFN.find state.dfn head)
          then (min,true,partition)
          else (head,loop,partition)
      ) (vertex,false,partition) succs in
    (* If no element below in the spanning tree link to deepest than [vertex]. *)    
    if N.equal head vertex then begin
      (* Makes edges to [vertex] invisible in the second visit. *)
      DFN.replace state.dfn vertex max_int;
      let element = Stack.pop state.stack in
      (* If an element below in the spanning tree links to [vertex],
           which is the head of the SCC.  *)
      if loop then begin
        let rec loop element =
          if not @@ N.equal element vertex then begin
            (* Remove from DFN for a second visit. *)
            DFN.remove state.dfn element;
            loop (Stack.pop state.stack)
          end
          else ()
        in loop element;
        (head,component state vertex::partition)
      end
      else (head,node vertex::partition)
    end
    (* [vertex] is part of a strongly connected component, and is not
       the root.  Do not update partition; the vertex will be added in
       the next call to [component]. *)
    else (head,partition)

  (* We found a SCC; revisit it with the [edge] to its head made
     invisible. *)
  and component state vertex =
    let succs = state.succs vertex in
    let partition = List.fold_left (fun partition succ ->
        (* Select only the branches of the spanning tree which are in a SCC with [vertex];
           and which have been removed from the DFN by [visit]. *)
        if DFN.mem state.dfn succ
        then partition
        else let (_,part) =  visit state succ partition in part
      ) [] succs
    in
    Component(vertex,partition)
  ;;

  
  let partition ~init ~succs =
    let state = {dfn = DFN.create 17; num = 0; succs;
                 stack = Stack.create () } in
    snd @@ visit state init []
  ;;


  (* Follow's Bourdoncle convention *)
  let rec pretty_partition fmt part =
    List.iter (fun x -> Format.fprintf fmt "@ %a" pretty_component x) part
  and pretty_component fmt = function
    | Node n -> N.pretty fmt n
    | Component(head,part) ->
      Format.fprintf fmt "@[<hov 1>(%a%a)@]" N.pretty head pretty_partition part
end
