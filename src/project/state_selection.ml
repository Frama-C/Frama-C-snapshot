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
  val list_union: t list -> t
  val list_state_union: ?deps:(State.t -> t) -> State.t list -> t
  val diff: t -> t -> t
  val cardinal: t -> int
  val to_list: t -> State.t list
  val pretty: Format.formatter -> t -> unit
  val iter_succ: (State.t -> unit) -> t -> State.t -> unit
  val fold_succ: (State.t -> 'a -> 'a) -> t -> State.t -> 'a -> 'a
  val iter: (State.t -> unit) -> t -> unit
  val fold: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_in_order: (State.t -> unit) -> t -> unit
  val fold_in_order: (State.t -> 'a -> 'a) -> t -> 'a -> 'a
end

module Static = struct

  let transitive_closure next_vertices s =
    let rec visit acc v =
      next_vertices
        (fun v' acc ->
          let e = v, v' in
          if Selection.mem_edge_e acc e then acc
          else visit (Selection.add_edge_e acc e) v')
        State_dependency_graph.graph v acc
    in
    (* add [s] in the selection even if it has no ingoing/outgoing edges *)
    visit (Selection.add_vertex Selection.empty s) s

  let with_dependencies s = 
    Subset (transitive_closure State_dependency_graph.G.fold_succ s)

  let with_codependencies s = 
    Subset (transitive_closure State_dependency_graph.G.fold_pred s)

  let only_dependencies s =
    let g = transitive_closure State_dependency_graph.G.fold_succ s in
    Subset (Selection.remove_vertex g s)

  let only_codependencies s =
    let g = transitive_closure State_dependency_graph.G.fold_pred s in
    Subset (Selection.remove_vertex g s)

  let diff sel1 sel2 =
    match sel1, sel2 with
      | _, Full -> Subset Selection.empty
      | Full, sel2 when is_empty sel2 -> Full
      | Full, Subset sel2 ->
        let selection =
          State_dependency_graph.G.fold_vertex
            (fun v acc ->
              if Selection.mem_vertex sel2 v then acc
              else Selection.add_vertex acc v)
            State_dependency_graph.graph
            Selection.empty
        in
        let sel =
          State_dependency_graph.G.fold_edges
            (fun v1 v2 acc ->
              if Selection.mem_vertex sel2 v1 || Selection.mem_vertex sel2 v2
              then acc
              else Selection.add_edge acc v1 v2)
            State_dependency_graph.graph
            selection
        in
        Subset sel
      | Subset sel1, Subset sel2 ->
        Subset
          (Selection.fold_vertex
             (fun v acc -> Selection.remove_vertex acc v) sel2 sel1)

  let union =
    let module O = Graph.Oper.P(Selection) in
    fun sel1 sel2 -> match sel1, sel2 with
    | Full, _ | _, Full -> Full
    | Subset sel1, Subset sel2 -> Subset (O.union sel1 sel2)

  let list_union l = List.fold_left union (Subset Selection.empty) l
    
  let list_state_union ?(deps=singleton) l =
    List.fold_left
      (fun acc state -> union acc (deps state)) (Subset Selection.empty) l

  let cardinal = function
    | Full -> State_dependency_graph.G.nb_vertex State_dependency_graph.graph
    | Subset sel -> Selection.nb_vertex sel

  let iter_succ f sel v = match sel with
    | Full -> 
      State_dependency_graph.G.iter_succ f State_dependency_graph.graph v
    | Subset sel -> Selection.iter_succ f sel v

  let fold_succ f sel v acc = match sel with
    | Full -> 
      State_dependency_graph.G.fold_succ f State_dependency_graph.graph v acc
    | Subset sel -> Selection.fold_succ f sel v acc

  let iter f = function
    | Full -> 
      State_dependency_graph.G.iter_vertex f State_dependency_graph.graph
    | Subset sel -> Selection.iter_vertex f sel

  let fold f s acc = match s with
    | Full -> 
      State_dependency_graph.G.fold_vertex f State_dependency_graph.graph acc
    | Subset sel -> Selection.fold_vertex f sel acc

  let to_list s = fold (fun s acc -> s :: acc) s []

  module TG = State_topological.Make(State_dependency_graph.G)
  module TS = State_topological.Make(Selection)

  let iter_in_order f = function
    | Full -> TG.iter f State_dependency_graph.graph
    | Subset sel -> TS.iter f sel

  let fold_in_order f s acc = match s with
    | Full -> TG.fold f State_dependency_graph.graph acc
    | Subset sel -> TS.fold f sel acc

  let pretty fmt sel =
    Format.fprintf fmt "contents of the selection:@\n";
    let mem s =
      State_dependency_graph.G.mem_vertex
        State_dependency_graph.graph
        s
    in
    iter_in_order
      (fun s ->
        Format.fprintf fmt "\t state %S%s@\n"
          (State.get_unique_name s)
          (if mem s then "" else "(\"" ^ State.get_name s ^ "\")"))
      sel;
    Format.pp_print_flush fmt ()

end

include Static

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
