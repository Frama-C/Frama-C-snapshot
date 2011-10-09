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

module type G = sig
  module V: Graph.Sig.VERTEX with type t = State.t and type label = State.t
  module E: Graph.Sig.EDGE with type vertex = State.t
                           and type t = State.t * State.t
  type t
  val iter_vertex: (V.t -> unit) -> t -> unit
  val fold_vertex: (V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_edges_e : (E.t -> unit) -> t -> unit
  val fold_edges : (V.t -> V.t -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter_succ : (V.t -> unit) -> t -> V.t -> unit
  val fold_succ: (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val fold_pred: (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
  val mem_vertex: t -> V.t -> bool
  val mem_edge_e: t -> E.t -> bool
  val in_degree : t -> V.t -> int
  val out_degree : t -> V.t -> int
  val nb_vertex: t -> int
end

module type S = sig
  module G: G
  val graph: G.t
  val add_dependencies: from:State.t -> State.t list -> unit
  val add_codependencies: onto:State.t -> State.t list -> unit
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

module Single_graph = struct

  module V = Dependency_graph.V
  module E = Dependency_graph.E

  type graphs =
      { internal_graph: Dependency_graph.t;
        mutable external_graph: Dependency_graph.t }

  (* invariant:
     for each edge in external_graph,
     - src(edge) \in internal_graph
     - dst(edge) \notin internal_graph
     - \exists s in another internal_graph such dst(edge) = s *)

  type t = { id: int; mutable graphs: graphs }

  let equal = (==)

  let create_graphs ?size () =
    { internal_graph = Dependency_graph.create ?size ();
      external_graph = Dependency_graph.create ?size () }

  let create =
    let cpt = ref 0 in
    fun ?size () ->
      incr cpt;
      { id = !cpt; graphs = create_graphs ?size () }

(*  let clear g = Dependency_graph.clear g.graphs.internal_graph*)
  let mem_vertex g = Dependency_graph.mem_vertex g.graphs.internal_graph
  let mem_edge g = Dependency_graph.mem_edge g.graphs.internal_graph
  let add_vertex g = Dependency_graph.add_vertex g.graphs.internal_graph
  let remove_vertex g = Dependency_graph.remove_vertex g.graphs.internal_graph
  let add_edge g v dep = Dependency_graph.add_edge g.graphs.internal_graph v dep
  let iter_vertex f g = Dependency_graph.iter_vertex f g.graphs.internal_graph
  let fold_vertex f g = Dependency_graph.fold_vertex f g.graphs.internal_graph
  let iter_edges f g = Dependency_graph.iter_edges f g.graphs.internal_graph
  let iter_edges_e f g = Dependency_graph.iter_edges_e f g.graphs.internal_graph
  let fold_edges f g = Dependency_graph.fold_edges f g.graphs.internal_graph
  let fold_edges_e f g = Dependency_graph.fold_edges_e f g.graphs.internal_graph
  let iter_succ f g = Dependency_graph.iter_succ f g.graphs.internal_graph
  let fold_succ f g = Dependency_graph.fold_succ f g.graphs.internal_graph
  let fold_pred f g = Dependency_graph.fold_pred f g.graphs.internal_graph
  let mem_vertex g = Dependency_graph.mem_vertex g.graphs.internal_graph
  let mem_edge_e g = Dependency_graph.mem_edge_e g.graphs.internal_graph
  let in_degree g = Dependency_graph.in_degree g.graphs.internal_graph
  let out_degree g = Dependency_graph.out_degree g.graphs.internal_graph
  let nb_vertex g = Dependency_graph.nb_vertex g.graphs.internal_graph

  let external_clear g = Dependency_graph.clear g.graphs.external_graph
  let external_mem_vertex g =
    Dependency_graph.mem_vertex g.graphs.external_graph
  let external_mem_edge g = Dependency_graph.mem_edge g.graphs.external_graph
  let external_add_vertex g =
    Dependency_graph.add_vertex g.graphs.external_graph
  let external_remove_vertex g =
    Dependency_graph.remove_vertex g.graphs.external_graph
  let external_add_edge g v dep =
    Dependency_graph.add_edge g.graphs.external_graph v dep
  let external_iter_vertex f g =
    Dependency_graph.iter_vertex f g.graphs.external_graph
  let external_fold_vertex f g =
    Dependency_graph.fold_vertex f g.graphs.external_graph
  let external_iter_edges f g =
    Dependency_graph.iter_edges f g.graphs.external_graph
  let external_iter_edges_e f g =
    Dependency_graph.iter_edges_e f g.graphs.external_graph
  let external_fold_edges f g =
    Dependency_graph.fold_edges f g.graphs.external_graph
  let external_fold_edges_e f g =
    Dependency_graph.fold_edges_e f g.graphs.external_graph
  let external_iter_succ f g =
    Dependency_graph.iter_succ f g.graphs.external_graph
  let external_fold_succ f g =
    Dependency_graph.fold_succ f g.graphs.external_graph
  let external_fold_pred f g =
    Dependency_graph.fold_pred f g.graphs.external_graph
  let external_mem_edge_e g =
    Dependency_graph.mem_edge_e g.graphs.external_graph
  let external_in_degree g = Dependency_graph.in_degree g.graphs.external_graph
  let external_out_degree g =
    Dependency_graph.out_degree g.graphs.external_graph
  let external_nb_vertex g = Dependency_graph.nb_vertex g.graphs.external_graph

end

(* Which graph a vertex belongs to? *)
module Vertices = struct

  let vertices = ref (State.Hashtbl.create 997)
  let statics = State.Hashtbl.create 997

  type t = Single_graph.t State.Hashtbl.t
  let create () = State.Hashtbl.copy statics
  let clear _h = () (* will be done by dynamic graph *)
  let get () = !vertices
  let set v = vertices := v
  let clear_some_projects _ _ = false

  let find v = try State.Hashtbl.find !vertices v with Not_found -> assert false
  let iter f = State.Hashtbl.iter f !vertices
  let fold f = State.Hashtbl.fold f !vertices

  let remove v = State.Hashtbl.remove !vertices v
  let add v g is_static =
    let h = !vertices in
    assert (not (State.Hashtbl.mem h v));
    State.Hashtbl.add h v g;
    if is_static then State.Hashtbl.add statics v g

end

module Vertices_datatype =
  Datatype.Make
    (struct
      include Datatype.Undefined
      type t = Vertices.t
      let name = "State_dependency_graph.Vertices_datatype"
      let reprs = [ Vertices.statics ]
     end)

(* Dynamic implementation of Graph.Sig.G. *)
module G = struct

  (* Non-empty list of all existing graphs.
     The head is the static graph. *)
  type t = Single_graph.t list ref

  module V = Single_graph.V
  module E = Single_graph.E

  let is_directed = true

  let iter_vertex f _g = Vertices.iter (fun v _ -> f v)
  let fold_vertex f _g = Vertices.fold (fun v _ -> f v)

  let iter_edges_e f g =
    let g = !g in
    let app iter = List.iter (iter f) g in
    app Single_graph.iter_edges_e;
    app Single_graph.external_iter_edges_e

  let fold_edges f g acc =
    let g = !g in
    let app fold acc = List.fold_left (fun acc g -> fold f g acc) acc g in
    let acc = app Single_graph.external_fold_edges acc in
    app Single_graph.fold_edges acc

  let fold_edges_e f g acc =
    let g = !g in
    let app fold acc = List.fold_left (fun acc g -> fold f g acc) acc g in
    let acc = app Single_graph.external_fold_edges_e acc in
    app Single_graph.fold_edges_e acc

  let mem_vertex _g _v = true (* there is no state outside the graph *)

  let mem_edge_e g e =
    let g = !g in
    let app mem =
      try List.iter (fun g -> if mem g e then raise Exit) g; false
      with Exit -> true
    in
    app Single_graph.mem_edge_e || app Single_graph.external_mem_edge_e

  let on_vertex f v = f (Vertices.find v) v

  let iter_succ f _g v =
    on_vertex
      (fun g v ->
        Single_graph.iter_succ f g v;
        try Single_graph.external_iter_succ f g v
        with Invalid_argument _ -> ())
      v

  let fold_succ f _g v acc =
    on_vertex
      (fun g v acc ->
        let acc = Single_graph.fold_succ f g v acc in
        try
          Single_graph.external_fold_succ
            (fun v acc -> try f v acc with Invalid_argument _ -> assert false)
            g v acc
        with Invalid_argument _ ->
          acc)
      v
      acc

  let fold_pred f g v acc =
    let g = !g in
    List.fold_left
      (fun acc g ->
        try Single_graph.external_fold_pred f g v acc
        with Invalid_argument _ -> acc)
      (on_vertex (Single_graph.fold_pred f) v acc)
      g

  let in_degree g v =
    let g = !g in
    List.fold_left
      (fun n g ->
        try Single_graph.external_in_degree g v + n
        with Invalid_argument _ -> n)
      (on_vertex Single_graph.in_degree v)
      g

  let out_degree _g v =
    on_vertex
      (fun g v ->
        let n = Single_graph.out_degree g v in
        try Single_graph.external_out_degree g v + n
        with Invalid_argument _ -> n)
      v

  let nb_vertex g = fold_vertex (fun _ -> succ) g 0

end

module Dynamic = struct

  type t = Single_graph.t list
  let graph = ref []
  let get () = !graph

  let add_graph g = match !graph with
    | [] -> assert false
    | static :: l -> graph := static :: g :: l

  module G = G

  let add_dependency from dep =
(*    Format.printf "adding edge %S --> %S@." (State.get_name from) (State.get_name dep);*)
    let gfrom = Vertices.find from in
    let gdep = Vertices.find dep in
    if Single_graph.equal gfrom gdep then Single_graph.add_edge gfrom from dep
    else Single_graph.external_add_edge gfrom from dep

  let add_dependencies ~from deps = List.iter (add_dependency from) deps
  let add_codependencies ~onto codeps =
    List.iter (fun c -> add_dependency c onto) codeps

  (* This function does not remove the vertex from the internal graphs list *)
  let partial_remove_vertex v =
    List.iter (fun g -> Single_graph.external_remove_vertex g v) !graph

  module Attributes = struct

    open Graph.Graphviz

    let get_graph s =
      try (Vertices.find s).Single_graph.id
      with Not_found ->
(*      Format.printf "state %S (%S) not found@."
          (State.get_unique_name s) (State.get_name s);*)
        assert false

    let color n = `Color (Extlib.number_to_color n)
    let vertex_name s = "\"" ^ State.get_unique_name s ^ "\""
    let graph_attributes _ = [ `Ratio (`Float 0.25) ]
    let default_vertex_attributes _ = []
    let vertex_attributes s =
      [ `Label (String.escaped (State.get_name s)); color (get_graph s) ]
    let default_edge_attributes _ = []

    let edge_attributes (src, dst) =
      let n1 = get_graph src in
      let n2 = get_graph dst in
      if n1 = n2 then [ color n1 ] else [ `Color (n1 + n2 / 2) ]

    let get_subgraph s =
      let n = get_graph s in
      Some
        { DotAttributes.sg_name = string_of_int n;
          sg_attributes = [ color n ] }

  end

  module Dot(A:Attributes) = struct
    module D = Graph.Graphviz.Dot(struct include A include G end)
    let dump filename =
      let cout = open_out filename in
      D.output_graph cout graph;
      close_out cout
  end
  include Dot(Attributes)

end

module Static = struct

  module G = Single_graph
  open G

  let graph = create ~size:7 ()
  let () = Dynamic.graph := graph :: !Dynamic.graph

  let add_dependencies ~from deps = List.iter (add_edge graph from) deps
  let add_codependencies ~onto codeps =
    List.iter (fun c -> add_edge graph c onto) codeps

  let add_state v deps =
    add_vertex graph v;
    Vertices.add v graph true;
    add_codependencies ~onto:v deps

  type t = Dependency_graph.t
  let create () = Dependency_graph.create ~size:7 ()
  let get () = graph.graphs.external_graph
  let set g = graph.graphs.external_graph <- g
  let clear _ = ()
  let clear_some_projects _ _ = false

end

module Static_datatype =
  Datatype.Make
    (struct
      include Datatype.Undefined
      type t = Static.t
      let name = "State_dependency_graph.Static_datatype"
      let reprs = [ Dependency_graph.create () ]
     end)

type local_search = Begin | Static | Dynamic of Single_graph.t

let add_state_like_the_others states s =
  let rec search go = function
    | [] ->
      go
    | s' :: l ->
      let g' = Vertices.find s' in
      let go = match go with
        | Begin | Static ->
          if Single_graph.equal g' Static.graph then Static else Dynamic g'
        | Dynamic _ -> go
      in
      search go l
  in
  match search Begin states with
  | Begin | Static -> Static.add_state s []
  | Dynamic g ->
    Single_graph.add_vertex g s;
    Vertices.add s g false

module Make_dynamic(T: sig val name: string end) = struct

  (* Implement {!Project.State_builder.INPUT}.
     Be careful that the specified invariant must hold. *)

  module G = Single_graph
  open G

  let graph = create ~size:7 ()
  let () = Dynamic.add_graph graph

  let add_state v deps =
    add_vertex graph v;
    Vertices.add v graph false;
    Dynamic.add_codependencies ~onto:v deps

  let remove_state v =
    remove_vertex graph v;
    Vertices.remove v;
    Dynamic.partial_remove_vertex v

  module Local = struct

    type t = graphs
    let create () = create_graphs ~size:7 ()
    let clear _g = () (* clearing will be done by dashtbl *)
    let get () = graph.graphs
    let set g =
      (* assume that other graphs are set accordingly *)
      graph.graphs <- g

    let clear_some_projects _ _ = false

  end
  include Local

  module Datatype =
    Datatype.Make
      (struct
        include Datatype.Undefined
        type t = Local.t
        let name = "State_dependency_graph.Make_dynamic(" ^ T.name ^ ").t"
        let reprs = [ graph.graphs ]
     end)
  let () = Type.set_ml_name Datatype.ty None

  let real_clear () =
    let g = graph.graphs.internal_graph in
    Dependency_graph.iter_vertex
      (fun s -> State.delete s; Vertices.remove s) g;
    Dependency_graph.clear g;
    List.iter Single_graph.external_clear !Dynamic.graph

end

module Remove_useless_states
  (G:Graph.Sig.P)
  (X:sig val kind:G.V.t -> State.kind end) =
struct

  let get g =
    let module D = Graph.Traverse.Dfs(G) in
    let module H = Hashtbl.Make(G.V) in
    let to_be_removed = H.create 17 in
    let h = H.create 17 in
    let post s =
      let kept = H.mem h s in
      match X.kind s with
      | `Irrelevant -> assert false
      | `Proxy `Internal | `Internal when not kept -> H.add to_be_removed s ()
      | `Internal
      | `Tuning
      | `Correctness
      | `Proxy (`Internal | `Correctness) ->
        G.iter_pred (fun s' -> H.replace h s' ()) g s
    in
    D.iter ~post g;
    H.fold (fun v () g -> G.remove_vertex g v) to_be_removed g

end

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
 *)
