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

module type S = sig
  module G: Graph.Sig.G with type V.t = State.t 
			and type E.t = State.t * State.t
  val graph: G.t
  val add_dependencies: from:State.t -> State.t list -> unit
  val add_codependencies: onto:State.t -> State.t list -> unit
  val remove_dependencies: from:State.t -> State.t list -> unit
  val remove_codependencies: onto:State.t -> State.t list -> unit
end

module type Attributes = sig
  open Graph.Graphviz
  val graph_attributes: 'a -> DotAttributes.graph list
  val default_vertex_attributes: 'a -> DotAttributes.vertex list
  val vertex_name : State.t -> string
  val vertex_attributes: State.t -> DotAttributes.vertex list
  val default_edge_attributes: 'a -> DotAttributes.edge list
  val edge_attributes: State.t * State.t -> DotAttributes.edge list
  val get_subgraph : State.t -> DotAttributes.subgraph option
end

module Dependency_graph = Graph.Imperative.Digraph.ConcreteBidirectional(State)

module Static = struct

  module G = Dependency_graph   
  let graph = Dependency_graph.create ~size:7 ()

  let add_dependencies ~from deps = 
    List.iter (Dependency_graph.add_edge graph from) deps

  let add_codependencies ~onto codeps =
    List.iter (fun c -> Dependency_graph.add_edge graph c onto) codeps

  let remove_dependencies ~from deps =
    List.iter (Dependency_graph.remove_edge graph from) deps

  let remove_codependencies ~onto codeps =
    List.iter (fun c -> Dependency_graph.remove_edge graph c onto) codeps

  let add_state v deps =
    Dependency_graph.add_vertex graph v;
    add_codependencies ~onto:v deps

end

include Static

module Attributes = struct
  let vertex_name s = "\"" ^ State.get_unique_name s ^ "\""
  let graph_attributes _ = [ `Ratio (`Float 0.25) ]
  let default_vertex_attributes _ = []
  let vertex_attributes s = [ `Label (String.escaped (State.get_name s)) ]
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot(A:Attributes) = struct
  module D = Graph.Graphviz.Dot(struct include A include Dependency_graph end)
  let dump filename =
    let cout = open_out filename in
    D.output_graph cout graph;
    close_out cout
end
include Dot(Attributes)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
 *)
