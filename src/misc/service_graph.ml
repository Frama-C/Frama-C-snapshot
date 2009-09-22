(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

let number_to_color n =
  let color = ref 0 in
  let number = ref n in
  for i = 0 to 7 do
    color := (!color lsl 1) +
      (if !number land 1 <> 0 then 1 else 0) +
      (if !number land 2 <> 0 then 256 else 0) +
      (if !number land 4 <> 0 then 65536 else 0);
    number := !number lsr 3
  done;
  !color

module Make
  (G: sig
     type t
     module V: sig
       include Graph.Sig.HASHABLE
       val id: t -> int
       val name: t -> string
       val attributes: t -> Graph.Graphviz.DotAttributes.vertex list
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
  type edge = Inter_services | Inter_functions | Function_to_service 
  
  module Vertex = struct
    type t = vertex
    let id v = (G.V.id v.node)
    let compare v1 v2 = Pervasives.compare (id v1) (id v2)
    let equal v1 v2 = (id v1) = (id v2)
    let hash = id
  end

  module Edge = struct
    type t = edge
    let default = Inter_functions
    let compare = Pervasives.compare
  end

  module CallG = struct
    module M = Graph.Imperative.Digraph.ConcreteLabeled(Vertex)(Edge)
    include M
    type tt = t
    module Datatype = 
      Project.Datatype.Imperative
	(struct 
	   include M 
	   let name = 
	     Project.Datatype.extend_name
	       "Service_graph.CallG " G.datatype_name
	 end)
    let add_labeled_edge g src l dst = add_edge_e g (E.create src l dst) 
  end

  type root = Is_root | In_service of vertex

  type incomming_service =  
    | Unknown
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

  let check_invariant callg =
    CallG.iter_edges_e
      (fun e -> 
	 let src = CallG.E.src e in
	 let dst = CallG.E.dst e in
	 (match CallG.E.label e with
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
	  | Inter_services -> assert (src.is_root && dst.is_root)
	  | Function_to_service -> 
	      assert (not (Vertex.equal src.root dst.root) && dst.is_root)))
      callg

  let mem initial_roots node =
    Cilutil.StringSet.mem (G.V.name node) initial_roots

  (* [merge_service] is not symmetric *)
  exception Cannot_merge
  let merge_service s1 s2 = match s1, s2 with
    | Unknown, Maybe_fresh v2 -> 
	To_be_confirmed v2
    | Unknown, In_service v2 ->
	Final v2
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

  let make_vertex g callg initial_roots node =
    let mk incomming_s =
      let v = match incomming_s with 
	| Unknown | Fresh_if_unchanged ->
	    let rec v = { node = node; is_root = true; root = v } in v 
	| To_be_confirmed root | Final root -> 
	    { node = node; is_root = false; root = root }
      in
      let s = match incomming_s with
	| Unknown | Fresh_if_unchanged | Final _ -> In_service v.root
	| To_be_confirmed root -> Maybe_fresh root
      in
      (*	Format.printf "%s; root %s; final: %b@."
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
		 match acc with Unknown -> Fresh_if_unchanged | _ -> acc)
	    g
	    node
	    Unknown
	in
	(* if unknown at this point, node without predecessor *)
	(* if Fresh_if_unchanged at this point, then dominator cycle detected *)
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
		CallG.add_labeled_edge callg src_root Inter_services dst_root;
		CallG.add_labeled_edge callg v Function_to_service dst_root
	      end)
	   g 
	   node)
      g

  let compute g initial_roots =
    let module Go = Graph.Topological.Make(G) in
    let callg = CallG.create () in
    Go.iter (make_vertex g callg initial_roots) g;
    Go.iter (update_vertex g) g;
    add_edges g callg;
    check_invariant callg;
    Vertices.clear ();
    callg

  (* *********************************************************************** *)
  (* Pretty-print *)
  (* *********************************************************************** *)

  module TP = struct

    include CallG
    let root_id v = G.V.id v.root.node

    let graph_attributes _ = []

    let vertex_name s =
      Format.sprintf "\"UV %s (%d)\"" (G.V.name s.node) (G.V.id s.node)

    let vertex_attributes s =
      let attr =
	`Label
          (Pretty_utils.sfprintf "@[%a@]" !Ast_printer.d_ident 
	     (G.V.name s.node))
	:: (`Color (number_to_color (G.V.id s.root.node)))
	:: G.V.attributes s.node
      in
      if s.is_root then `Shape `Diamond :: attr else attr
      
    let default_vertex_attributes _ = []

    let edge_attributes e =
      match CallG.E.label e with
      | Inter_functions ->
	  let sr = root_id (CallG.E.src e) in
	  let dr = root_id (CallG.E.dst e) in
	  if sr = dr then [ `Color (number_to_color sr); `Comment "ff" ]
	  else [ `Color 0x1478ac; `Comment "ff" ]
      | Inter_services -> [  `Color 0xf3862f; `Comment "ss" ]
      | Function_to_service -> [ `Color 0x067c06; `Comment "fs"  ]

    let default_edge_attributes _ = []

    let get_subgraph v = 
      let id = root_id v in
      let cs = string_of_int id in
      Some
        { Graph.Graphviz.DotAttributes.sg_name = cs;
          sg_attributes = 
	    [ `Label ("S "^cs);
	      `Color (number_to_color id);
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
compile-command: "LC_ALL=C make -C ../.."
End:
*)
