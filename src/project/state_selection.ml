(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

module Selection = Graph.Persistent.Digraph.ConcreteBidirectional(State)

type state_selection =
  | Full
  | Subset of Selection.t

let empty = Subset Selection.empty
let full = Full
let singleton s = Subset (Selection.add_vertex Selection.empty s)
let of_list l = Subset (List.fold_left Selection.add_vertex Selection.empty l)

let is_empty s = s = Subset Selection.empty
let is_full s = s = Full
let mem sel s = match sel with
  | Full -> true
  | Subset sel -> Selection.mem_vertex sel s

include Datatype.Make
(struct
  include Datatype.Undefined
  type t = state_selection
  let name = "State_selection"
  let reprs = [ full; empty; singleton State.dummy ]
  let internal_pretty_code p_caller fmt = function
    | Full -> Format.fprintf fmt "@[State_selection.full@]"
    | Subset sel ->
      match Selection.fold_vertex (fun s acc -> s :: acc) sel [] with
      | [] -> Format.fprintf fmt "@[State_selection.empty@]"
      | [ s ] ->
        let pp fmt =
          Format.fprintf fmt "@[<hv 2>State_selection.singleton@;%a@]"
            (State.internal_pretty_code Type.Call)
            s
        in
        Type.par p_caller Type.Call fmt pp
      | l ->
        let module D = Datatype.List(State) in
        let pp fmt =
          Format.fprintf fmt "@[<hv 2>State_selection.of_list@;%a@]"
            (D.internal_pretty_code Type.Call)
            l
        in
        Type.par p_caller Type.Call fmt pp
 end)

module type S = sig
  val with_dependencies: State.t -> t
  val only_dependencies: State.t -> t
  val with_codependencies: State.t -> t
  val only_codependencies: State.t -> t
  val union: t -> t -> t
  val diff: t -> t -> t
  val cardinal: t -> int
  val pretty: Format.formatter -> t -> unit
  val iter_succ: (State.t -> unit) -> t -> State.t -> unit
  val fold_succ: (State.t -> 'a -> 'a) -> t -> State.t -> 'a -> 'a
  val iter: (State.t -> unit) -> t -> unit
  val fold: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_in_order: (State.t -> unit) -> t -> unit
  val fold_in_order: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
  val remove_useless_states: t -> t
end

module Make(G: State_dependency_graph.S) = struct

  let transitive_closure next_vertices s =
    let rec visit acc v =
      next_vertices
        (fun v' acc ->
          let e = v, v' in
          if Selection.mem_edge_e acc e then acc
          else visit (Selection.add_edge_e acc e) v')
        G.graph v acc
    in
    (* add [s] in the selection even if it has no ingoing/outgoing edges *)
    visit (Selection.add_vertex Selection.empty s) s

  let with_dependencies s = Subset (transitive_closure G.G.fold_succ s)
  let with_codependencies s = Subset (transitive_closure G.G.fold_pred s)

  let only_dependencies s =
    let g = transitive_closure G.G.fold_succ s in
    Subset (Selection.remove_vertex g s)

  let only_codependencies s =
    let g = transitive_closure G.G.fold_pred s in
    Subset (Selection.remove_vertex g s)

  let diff sel1 sel2 =
    Subset
      (match sel1, sel2 with
      | _, Full -> Selection.empty
      | Full, Subset sel2 ->
        let selection =
          G.G.fold_vertex
            (fun v acc ->
              if Selection.mem_vertex sel2 v then acc
              else Selection.add_vertex acc v)
            G.graph
            Selection.empty
        in
        G.G.fold_edges
          (fun v1 v2 acc ->
            if Selection.mem_vertex sel2 v1 || Selection.mem_vertex sel2 v2
            then acc
            else Selection.add_edge acc v1 v2)
          G.graph
          selection
      | Subset sel1, Subset sel2 ->
        Selection.fold_vertex
          (fun v acc -> Selection.remove_vertex acc v) sel2 sel1)

  let union =
    let module O = Graph.Oper.P(Selection) in
    fun sel1 sel2 -> match sel1, sel2 with
    | Full, _ | _, Full -> Full
    | Subset sel1, Subset sel2 -> Subset (O.union sel1 sel2)

  let cardinal = function
    | Full -> G.G.nb_vertex G.graph
    | Subset sel -> Selection.nb_vertex sel

  let iter_succ f sel v = match sel with
    | Full -> G.G.iter_succ f G.graph v
    | Subset sel -> Selection.iter_succ f sel v

  let fold_succ f sel v acc = match sel with
    | Full -> G.G.fold_succ f G.graph v acc
    | Subset sel -> Selection.fold_succ f sel v acc

  let iter f = function
    | Full -> G.G.iter_vertex f G.graph
    | Subset sel -> Selection.iter_vertex f sel

  let fold f s acc = match s with
    | Full -> G.G.fold_vertex f G.graph acc
    | Subset sel -> Selection.fold_vertex f sel acc

  module TG = State_topological.Make(G.G)
  module TS = State_topological.Make(Selection)

  let iter_in_order f = function
    | Full -> TG.iter f G.graph
    | Subset sel -> TS.iter f sel

  let fold_in_order f s acc = match s with
    | Full -> TG.fold f G.graph acc
    | Subset sel -> TS.fold f sel acc

  let pretty fmt sel =
    Format.fprintf fmt "contents of the selection:@\n";
    let mem s =
      State_dependency_graph.Static.G.mem_vertex
        State_dependency_graph.Static.graph
        s
    in
    iter_in_order
      (fun s ->
        Format.fprintf fmt "\t state %S%s@\n"
          (State.get_unique_name s)
          (if mem s then "" else "(\"" ^ State.get_name s ^ "\")"))
      sel;
    Format.pp_print_flush fmt ()

  let remove_useless_states = function
    | Full -> assert false
    | Subset sel ->
      let module R =
            State_dependency_graph.Remove_useless_states(Selection)(State)
      in
      Subset (R.get sel)

end

module Static = Make(State_dependency_graph.Static)

module Dynamic = struct

  include Make(State_dependency_graph.Dynamic)

  module Dot(A:State_dependency_graph.Attributes) = struct
    module DG = State_dependency_graph.Dynamic.Dot(A)
    module DS = Graph.Graphviz.Dot(struct include A include Selection end)
    let dump s filename =
      match s with
      | Full -> DG.dump filename
      | Subset s ->
        let cout = open_out filename in
        DS.output_graph cout s;
        close_out cout
  end

  include Dot(State_dependency_graph.Dynamic.Attributes)

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
