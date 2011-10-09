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

module Make
  (G: sig
     type t
     module V: sig
       include Graph.Sig.HASHABLE
       val id: t -> int
       val name: t -> string
       val attributes: t -> Graph.Graphviz.DotAttributes.vertex list
       val entry_point: unit -> t option
     end
     val iter_vertex : (V.t -> unit) -> t -> unit
     val iter_succ : (V.t -> unit) -> t -> V.t -> unit
     val iter_pred : (V.t -> unit) -> t -> V.t -> unit
     val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
     val in_degree: t -> V.t -> int
     val datatype_name: string
   end) =
struct

  type vertex = { node: G.V.t; mutable is_root: bool; mutable root: vertex }
  type edge = Inter_services | Inter_functions | Both

  module Vertex = struct
    type t = vertex
    let id v = (G.V.id v.node)
    let compare v1 v2 = Datatype.Int.compare (id v1) (id v2)
    let equal v1 v2 = (id v1) = (id v2)
    let hash = id
  end

  module Edge = struct
    type t = edge
    let default = Inter_functions
    let compare : t -> t -> _ = Extlib.compare_basic
  end

  module CallG = struct
    module M = Graph.Imperative.Digraph.ConcreteLabeled(Vertex)(Edge)
    include M
    module Datatype =
      Datatype.Make
        (struct
          (* [JS 2010/09/27] TODO: do better? *)
          include Datatype.Serializable_undefined
          type t = M.t
          let name = G.datatype_name ^ " Service_graph.CallG.t"
          let reprs = [ M.create () ]
          let mem_project = Datatype.never_any_project
         end)
    let () = Type.set_ml_name Datatype.ty None
    let add_labeled_edge g src l dst =
      if mem_edge g src dst then begin
        remove_edge g src dst;
        add_edge_e g (E.create src Both dst)
      end else
        add_edge_e g (E.create src l dst)
  end

  type root = Is_root | In_service of vertex

  type incomming_service =
    | Fresh_if_unchanged
    | To_be_confirmed of vertex
    | Final of vertex

  type service = Maybe_fresh of vertex | In_service of vertex

  module Vertices = struct
    module H = Hashtbl.Make(G.V)
    let vertices : (vertex * service) H.t = H.create 7
    let find = H.find vertices
    let add = H.add vertices
    let clear () = H.clear vertices
  end

  let edge_invariant src dst = function
    | Inter_functions ->
        assert
          (if Vertex.equal src.root dst.root || dst.is_root then
             true
           else begin
             Format.printf
               "Src:%s in %s (is_root:%b) Dst:%s in %s (is_root:%b)@."
               (G.V.name src.node)
               (G.V.name src.root.node)
               src.is_root
               (G.V.name dst.node)
               (G.V.name dst.root.node)
               dst.is_root;
             false
           end)
    | Inter_services | Both -> assert (src.is_root && dst.is_root)

  let check_invariant callg =
    CallG.iter_edges_e
      (fun e ->
         edge_invariant (CallG.E.src e) (CallG.E.dst e) (CallG.E.label e))
      callg

  let mem initial_roots node =
    Datatype.String.Set.mem (G.V.name node) initial_roots

  (* [merge_service] is not symmetric *)
  exception Cannot_merge
  let merge_service s1 s2 = match s1, s2 with
    | Fresh_if_unchanged, (Maybe_fresh v2 | In_service v2) ->
        To_be_confirmed v2
    | (To_be_confirmed v1 | Final v1), In_service v2 when Vertex.equal v1 v2 ->
        s1
    | (To_be_confirmed v1 | Final v1), Maybe_fresh v2
        when Vertex.equal v1 v2 ->
        To_be_confirmed v2
    | (To_be_confirmed v1 | Final v1), (Maybe_fresh v2 | In_service v2) ->
        assert (not (Vertex.equal v1 v2));
        raise Cannot_merge

  let entry_point_ref = ref None

  let make_vertex g callg initial_roots node =
    let mk incomming_s =
      let v = match incomming_s with
        | Fresh_if_unchanged ->
            let rec v = { node = node; is_root = true; root = v } in v
        | To_be_confirmed root | Final root ->
            { node = node; is_root = false; root = root }
      in
      (match G.V.entry_point () with
      | Some e when G.V.equal node e -> entry_point_ref := Some v
      | None | Some _ -> ());
      let s = match incomming_s with
        | Fresh_if_unchanged | Final _ -> In_service v.root
        | To_be_confirmed root -> Maybe_fresh root
      in
      (*        Format.printf "%s; root %s; final: %b@."
                (G.V.name node) (G.V.name v.root.node)
                (match s with In_service _ -> true | Maybe_fresh _ -> false);*)
      Vertices.add node (v, s);
      CallG.add_vertex callg v
    in
    if mem initial_roots node then
      mk Fresh_if_unchanged
    else
      try
        let service =
          G.fold_pred
            (fun node' acc ->
               try
                 let _, s' = Vertices.find node' in
                 merge_service acc s'
               with Not_found ->
                 acc)
            g
            node
            Fresh_if_unchanged
        in
        (* if Fresh_if_unchanged at this point,
           either node without predecessor or dominator cycle detected *)
        mk service
      with Cannot_merge ->
        mk Fresh_if_unchanged

  let update_vertex g node =
    try
      let v, s = Vertices.find node in
      match s with
      | In_service root -> assert (Vertex.equal v.root root)
      | Maybe_fresh root ->
          assert (Vertex.equal v.root root);
          try
            G.iter_pred
              (fun node' ->
                 try
                   let v', _ = Vertices.find node' in
                   if not (Vertex.equal root v'.root) then raise Exit
                 with Not_found ->
                   assert false)
              g
              node
              (* old status is confirmed: nothing to do *)
          with Exit ->
            (* update *)
            v.is_root <- true;
            v.root <- v
    with Not_found ->
      assert false

  let add_edges g callg =
    let find node =
      try fst (Vertices.find node) with Not_found -> assert false
    in
    G.iter_vertex
      (fun node ->
         let v = find node in
         G.iter_succ
           (fun node' ->
              let succ = find node' in
              CallG.add_labeled_edge callg v Inter_functions succ;
              let src_root = v.root in
              let dst_root = succ.root in
              if not (Vertex.equal src_root dst_root) then begin
                CallG.add_labeled_edge callg src_root Inter_services dst_root
                (* JS: no need of a `service_to_function' edge since
                   it is not possible to have an edge starting from a
                   not-a-root vertex and going to another service.

                   no need of a `function_to_service' edge since the only
                   possible edges between two services go to a root. *)
              end)
           g
           node)
      g

  let compute g initial_roots =
    entry_point_ref := None;
    let module Go = Graph.Topological.Make(G) in
    let callg = CallG.create () in
    Go.iter (make_vertex g callg initial_roots) g;
    Go.iter (update_vertex g) g;
    add_edges g callg;
    check_invariant callg;
    Vertices.clear ();
    callg

  let entry_point () = !entry_point_ref

  (* *********************************************************************** *)
  (* Pretty-print *)
  (* *********************************************************************** *)

  module TP = struct

    include CallG
    let root_id v = G.V.id v.root.node

    let graph_attributes _ = [ `Ratio (`Float 0.5) ]

    let vertex_name s =
      Format.sprintf "\"UV %s (%d)\"" (G.V.name s.node) (G.V.id s.node)

    let vertex_attributes s =
      let attr =
        `Label
          (Pretty_utils.sfprintf "@[%a@]" !Ast_printer.d_ident
             (G.V.name s.node))
        :: (`Color (Extlib.number_to_color (G.V.id s.root.node)))
        :: G.V.attributes s.node
      in
      if s.is_root then `Shape `Diamond :: attr else attr

    let default_vertex_attributes _ = []

    let edge_attributes e =
      let sr = root_id (CallG.E.src e) in
      [ `Color (Extlib.number_to_color sr) ]

    let default_edge_attributes _ = []

    let get_subgraph v =
      let id = root_id v in
      let cs = string_of_int id in
      Some
        { Graph.Graphviz.DotAttributes.sg_name = cs;
          sg_attributes =
            [ `Label ("S " ^ cs);
              `Color (Extlib.number_to_color id);
              `Style `Bold ] }

  end

  include Graph.Graphviz.Dot(TP)

(*
  (* Computing a graph of services whose nodes are nodes of the initial
     callgraph *)

  module SS =
    Set.Make(struct
               type t = G.V.t
               let compare x y = Pervasives.compare (G.V.id x) (G.V.id y)
             end)

  type service_vertex =
      { service: int; mutable root: G.V.t; mutable nodes: SS.t }

  module SG = struct
    module M = Graph.Imperative.Digraph.ConcreteLabeled
      (struct
         type t = service_vertex
         let equal x y = x.service = y.service
         let compare x y = Pervasives.compare x.service y.service
         let hash x = x.service
       end)
      (struct
         type t = bool ref (* [true] for inter-service edge *)
         let default = ref false
         let compare = Pervasives.compare
       end)
    include M
    type tt = t
    module Datatype =
      Project.Datatype.Imperative
        (struct
           include M
           let name = Project.Datatype.extend_name "Service_graph.SG " G.name
         end)
  end

  let get_service_id v = match v.mark with
    | Service s -> s
    | Nothing | JustMet _ -> assert false

  let compute_services cg =
    let sg = SG.create () in
    let vertices = Hashtbl.create 7 in
    let get_service v =
      let id = get_service_id v in
      let node = v.node in
      try
        let vertex = Hashtbl.find vertices id in
        (* the service already exists *)
        vertex.nodes <- SS.add node vertex.nodes;
        if v.is_service then vertex.root <- node;
        vertex
      with Not_found ->
        (* the service does not exist yet *)
        let vertex = { service = id; root = node; nodes= SS.singleton node } in
        SG.add_vertex sg vertex;
        Hashtbl.add vertices id vertex;
        vertex
    in
    CallG.iter_edges
      (fun v1 v2 ->
         let s1 = get_service v1 in
         let s2 = get_service v2 in
         match v1.is_service, v2.is_service with
         | true, true ->
             (try
                let b = SG.E.label (SG.find_edge sg s1 s2) in
                b := true
              with Not_found ->
                SG.add_edge sg s1 s2)
         | true, false ->
             assert false
         | false, true ->
             ()
         | false, false ->
             if not (SG.mem_edge sg s1 s2 || SG.V.equal s1 s2) then
               SG.add_edge sg s1 s2)
      cg;
    sg

  (* Pretty-print *)

  module PP_SG = struct

    include SG

    let graph_attributes _ = []

    let vertex_name s =
      Format.sprintf "\"%s (%d)\"" (G.V.name s.root) (SS.cardinal s.nodes)

    let vertex_attributes s =
      let n = 0.2 *. float (SS.cardinal s.nodes) in
      `Height n
      :: `Width n
      :: `Shape `Box
      :: (`Color (number_to_color s.service))
      :: G.V.attributes s.root

    let default_vertex_attributes _ = []

    let edge_attributes e =
      if !(SG.E.label e) then [ `Arrowhead `None; `Arrowtail `Inv ] else [ ]

    let default_edge_attributes _ = []
    let get_subgraph _ = None

  end

  module SGD = Graph.Graphviz.Dot(PP_SG)
  let output_services = SGD.output_graph
*)
end (* functor Service *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
