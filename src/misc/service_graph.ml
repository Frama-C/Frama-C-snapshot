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

let inter_services_ref = ref false
let frama_c_display b = inter_services_ref := b

module Make
  (G: sig
     type t
     module V: sig
       include Graph.Sig.COMPARABLE
       val id: t -> int
       val name: t -> string
       val attributes: t -> Graph.Graphviz.DotAttributes.vertex list
       val entry_point: unit -> t option
     end
     val iter_vertex : (V.t -> unit) -> t -> unit
     val iter_succ : (V.t -> unit) -> t -> V.t -> unit
     val iter_pred : (V.t -> unit) -> t -> V.t -> unit
     val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
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

  type incomming_service =
    | Fresh_if_unchanged
    | Unknown_cycle
    | To_be_confirmed of vertex
    | Final of vertex

  type service = Maybe_fresh of vertex | In_service of vertex

  module Vertices = struct
    module H = Hashtbl.Make(G.V)
    let vertices : (vertex * service) H.t = H.create 7
    let find = H.find vertices
    let add = H.add vertices
    let replace = H.replace vertices
    let clear () = H.clear vertices
  end

  let edge_invariant src dst = function
    | Inter_functions ->
      if not (Vertex.equal src.root dst.root || dst.is_root) then
	Kernel.failure
          "Correctness bug when computing services.\n\
PLEASE REPORT AS MAJOR BUG on http://bts.frama-c.com with the following info.\n\
Src:%s in %s (is_root:%b) Dst:%s in %s (is_root:%b)"
          (G.V.name src.node)
          (G.V.name src.root.node)
          src.is_root
          (G.V.name dst.node)
          (G.V.name dst.root.node)
          dst.is_root
    | Inter_services | Both -> 
      if not (src.is_root && dst.is_root) then
	Kernel.failure
          "Correctness bug when computing services.\n\
PLEASE REPORT AS MAJOR BUG on http://bts.frama-c.com with the following info.\n\
Src root:%s in %s (is_root:%b) Dst:%s in %s (is_root:%b) [2d case]"
          (G.V.name src.node)
          (G.V.name src.root.node)
          src.is_root
          (G.V.name dst.node)
          (G.V.name dst.root.node)
          dst.is_root

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
    | Fresh_if_unchanged, In_service v2 -> Final v2
    | Unknown_cycle, In_service v2 -> To_be_confirmed v2
    | (Fresh_if_unchanged | Unknown_cycle), Maybe_fresh v2 -> To_be_confirmed v2
    | (To_be_confirmed v1 | Final v1), In_service v2 when Vertex.equal v1 v2 ->
      s1
    | (To_be_confirmed v1 | Final v1), Maybe_fresh v2 when Vertex.equal v1 v2 ->
      To_be_confirmed v2
    | (To_be_confirmed v1 | Final v1), (Maybe_fresh v2 | In_service v2) ->
      assert (not (Vertex.equal v1 v2));
      raise Cannot_merge

  let entry_point_ref = ref None

  let make_vertex g callg initial_roots node =
    let mk incomming_s =
      let v = match incomming_s with
        | Fresh_if_unchanged | Unknown_cycle ->
          let rec v = { node = node; is_root = true; root = v } in v
        | To_be_confirmed root | Final root ->
          { node = node; is_root = false; root = root }
      in
      (match G.V.entry_point () with
      | Some e when G.V.equal node e -> entry_point_ref := Some v
      | None | Some _ -> ());
      let s = match incomming_s with
        | Fresh_if_unchanged | Unknown_cycle | Final _ -> In_service v.root
        | To_be_confirmed root -> Maybe_fresh root
      in
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
		(* cycle *)
		match acc with
		| Fresh_if_unchanged | Unknown_cycle -> Unknown_cycle
		| To_be_confirmed v | Final v -> To_be_confirmed v)
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
          v.root <- v;
          Vertices.replace node (v, In_service v);
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
        `Label (G.V.name s.node)
        :: `Color (Extlib.number_to_color (G.V.id s.root.node))
        :: G.V.attributes s.node
      in
      if s.is_root then `Shape `Diamond :: attr else attr

    let default_vertex_attributes _ = []

    let edge_attributes e =
      let color e =
        let sr = root_id (CallG.E.src e) in
        [ `Color (Extlib.number_to_color sr) ]
      in
      if !inter_services_ref then 
        color e
      else
        match CallG.E.label e with
        | Inter_services -> [ `Style [`Invis] ]
        | Inter_functions | Both -> color e

    let default_edge_attributes _ = []

    let get_subgraph v =
      let id = root_id v in
      let cs = string_of_int id in
      Some
        { Graph.Graphviz.DotAttributes.sg_name = cs;
          sg_parent = None;
          sg_attributes =
            [ `Label ("S " ^ cs);
              `Color (Extlib.number_to_color id);
              `Style [`Bold] ] }

  end

  include Graph.Graphviz.Dot(TP)

end (* functor Service *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
