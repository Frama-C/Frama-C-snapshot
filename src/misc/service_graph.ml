(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

module Make
  (G: sig
     type t
     module V: sig
       type t
       val id: t -> int (* assume is >= 0 and unique for each vertices of the graph *)
       val name: t -> string
       val attributes: t -> Graph.Graphviz.DotAttributes.vertex list
     end
     val iter_vertex: (V.t -> unit) -> t -> unit
     val callees: t -> V.t -> V.t list
     val callers: t -> V.t -> V.t list
     val name: string
   end) =
struct

  module V = struct
    type t = G.V.t 
    let compare t1 t2 = 
      Pervasives.compare (G.V.id t1) (G.V.id t2)
    let hash t1 = G.V.id t1
    let equal t1 t2 = G.V.id t1 = G.V.id t2
  end

  module CallNodeComparable = struct
    type m = Nothing | Service of int | JustMet of int
    type t = { node : G.V.t; mutable mark : m; mutable visited : bool; is_service : bool }
    let compare t1 t2 = 
      let n = V.compare t1.node t2.node in
      if n = 0 then Pervasives.compare t1.is_service t2.is_service else n
        
    let hash t1 = V.hash t1.node
    let equal t1 t2 = t1.is_service = t2.is_service && V.equal t1.node t2.node
  end
  type m = CallNodeComparable.m = Nothing | Service of int | JustMet of int
  type vertex = CallNodeComparable.t = 
      { node : G.V.t; mutable mark : m; mutable visited : bool; is_service : bool }

  open CallNodeComparable

  module CallNodeSet = Set.Make(CallNodeComparable)

  let verbose = ref false

  let pretty m s =
    if !verbose
    then Format.printf
      "Doing %s for Node: %s(%d) Visited: %b Mark:%s@."
      s
      (G.V.name m.node) (G.V.id m.node)
      m.visited
      (match m.mark with
       | Nothing -> "Nothing"
       | Service n -> "S"^(string_of_int n)
       | JustMet n -> "JustMet"^(string_of_int n))

  module CallG = struct
    module M = Graph.Imperative.Digraph.Concrete(CallNodeComparable)
    include M
    type tt = t
    module Datatype = 
      Project.Datatype.Imperative
	(struct 
	   include M 
	   let name = Project.Datatype.extend_name "service_graph " G.name
	 end)
  end

  (* check the invariant: for any edge a-->b with a <> b,
     if a and b have same service
     then all predecessors of b also have this service *)

  let check ocg =
    try
      CallG.iter_edges
        (
          fun v1 v2 ->
	    if not (equal v1 v2)
            then match v1.mark, v2.mark with
	    | Service s1, Service s2 when s1 = s2 ->
	        CallG.iter_pred
	          (fun p -> match p.mark with
		   | Service s when s = s2 -> ()
		   | Service _ ->
		       Format.eprintf "Cg.check: case 1a@."; raise Exit
		   | _ ->
		       Format.eprintf "Cg.check: case 1b@."; raise Exit)
	          ocg v2
	    | JustMet _, _ | _, JustMet _ ->
	        Format.eprintf "Cg.check: case 2@."; raise Exit
	    | _ -> ()
        ) ocg;
      true
    with Exit ->
      false

  (* Pretty-print *)

  module TP = struct
    include CallG
    let graph_attributes _ = []
    open Pretty

    let vertex_name s =
      Format.sprintf "\"UV %s (%d)\""
        (G.V.name s.node)
        (G.V.id s.node)

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

    let vertex_attributes s =
      match s.mark with
      | Nothing -> [`Color 0x00FF00 ; `Style `Dashed]
      | JustMet _ -> [`Color 0x0000FF ; `Style `Dashed]
      | Service c ->
	  if s.is_service then
	    [ `Width 0.0; `Height 0.0; `Style `Invis; `Label "" ]
	  else
            (`Label
               (match s.mark with
                | Service _ ->
                    (Pretty_utils.sfprintf "@[%a@]" 
                       !Ast_printer.d_ident (G.V.name s.node))
                | _ -> "NA"))
             ::
             (`Color (number_to_color c))
             ::
               G.V.attributes s.node

    let default_vertex_attributes _ = []

    let edge_attributes e =
      let dst = CallG.E.dst e in
      let src = CallG.E.src e in
      if dst.is_service then
        if src.is_service then
	  (* inter-service edge *)
	  [ `Style `Dotted; `Arrowhead `None; `Arrowtail `Inv ]
        else
	  (* edge from internal vertex to service vertex *)
          [ `Style `Invis; `Minlen 0 ]
      else
        (* true node edge *)
        if src.is_service then
          (* service/true edge *)
	  assert false
        else
          match src.mark,dst.mark with
          | Service sc, Service dc ->
              if sc = dc then  []
              else
                [ `Style `Dotted ]
          | _ -> [] (*assert false*)

    let default_edge_attributes _ = []

    let get_subgraph v = match v.mark with
    | Nothing | JustMet _ -> None
    | Service c ->
        let cs = string_of_int c in
        Some
          { Graph.Graphviz.DotAttributes.sg_name = cs;
            sg_attributes = [
              `Label ("S "^cs);
              `Color (number_to_color c);
              `Style `Bold]}

  end

  include Graph.Graphviz.Dot(TP)
    
  (* Service assignment *)

  let services = Inthash.create 997

  let add_service v cg =
    match v.mark with
    | Service s ->
        let vs =
          try Inthash.find services s
          with Not_found ->
            let vs = { v with is_service = true } in
            CallG.add_vertex cg vs;
	    Inthash.add services s vs;
            vs
        in
        CallG.add_edge cg v vs
    | _ ->
        assert false

  let fresh_mark =
    let counter = ref 0 in
    (function () ->
       incr counter;
       !counter)

  let mark_with_service n ocg =
    pretty n "BEFORE MARK W S";
    let new_mark = Service (fresh_mark ()) in
    n.mark <- new_mark;
    add_service n ocg; (* do it after setting the mark *)
    pretty n "AFTER MARK W S"

  exception NoNumber
    
  let dump ocg = CallG.iter_vertex (fun x -> pretty x "RECAP") ocg

  let decompose ocg l =
    let _info () = Format.printf "OCG: edges:%d vertex:%d@\n"
      (CallG.nb_edges ocg)
      (CallG.nb_vertex ocg)
    in
    (* At least all nodes accessible from root will be marked. *)
    (* contains only Service's marked nodes *)
    let todo_queue = Queue.create () in

    let rec new_mark n acc parent_service =
      pretty n "new_mark";
      match n.mark with
      | Service s ->
          (match acc with
           | None -> Some s
           | Some old when old = s -> acc
           | _ -> raise NoNumber)
      | JustMet n ->
          (match acc with
           | None -> Some n
           | Some old when old = n -> acc
           | _ -> raise NoNumber)
      | Nothing ->
          new_service n parent_service;
          match n.mark with Service s -> (match acc with
	                                    Some old when old = s -> acc
	                                  | None -> Some s
	                                  | _ -> raise NoNumber)
          | _ -> assert false
    and new_service elt parent_service =
      pretty elt "new_service";
      (try
         elt.mark <- JustMet parent_service;
         let new_service = CallG.fold_pred
           (fun n acc ->
              if equal n elt
              then acc
              else new_mark n acc parent_service)
           ocg
           elt
           None
         in
         match new_service with
         | None -> mark_with_service elt ocg
         | Some s -> elt.mark <- Service s; add_service elt ocg;
             pretty elt "CHOSE COMMON SERV"
       with NoNumber -> mark_with_service elt ocg);
      pretty elt "new_service result";
      dump ocg;
    in
    let do_mark elt parent_service =
      pretty elt "do_mark";
      elt.visited <- true;
      begin match elt.mark with
      | Service _ ->  ()
      | JustMet s -> new_service elt s
      | Nothing -> new_service elt parent_service
      end;
      match elt.mark with
      | Service s ->
          CallG.iter_succ
            (fun x -> Queue.push (x,s) todo_queue)
            ocg
            elt
      | _ -> () (*assert false*)
    in
    let one_node () =
      while not (Queue.is_empty todo_queue) do
        let current,s = Queue.pop todo_queue in
        pretty current "one_node";
        if not current.visited then do_mark current s;
      done
    in
    List.iter (fun x -> Queue.push x todo_queue) l ;
    one_node ()

  let get_fresh =
    let module H = Hashtbl.Make(V) in
    let memo = H.create 7 in
    fun c -> try H.find memo c
    with Not_found ->
      let node_func = {node = c; mark = Nothing; visited = false; is_service=false} in
      H.add memo c node_func;
      node_func

  exception Found of int
  let merge_edges cg =
    let new_cg = CallG.copy cg in
    let merge e =
      let src = CallG.E.src e in
      let dst = CallG.E.dst e in
      match src.mark, dst.mark with
      | Service s, Service d ->
	  if Cmdline.Debug.get () > 0 && s <> d && (not src.is_service) then begin
	    let find v =
	      try
	        CallG.iter_succ
		  (fun s -> if s.is_service then raise (Found (G.V.id s.node)))
		  cg
		  v;
	        assert false
	      with Found s ->
	        Inthash.find services s
	    in
	    CallG.add_edge new_cg (find src) (find dst)
	  end
      | _ -> () (*assert false*)
    in
    CallG.iter_edges_e merge new_cg;
    new_cg

  let compute cg init_nodes =
    let ocg = CallG.create () in
    let roots = ref [] in
    let all = ref [] in
    G.iter_vertex
      (fun func ->
         let node_func = get_fresh func in
         let func_name = G.V.name func in
         let has_callees = match G.callees cg func with [] -> false | _ :: _ -> true in
         let has_callers = match G.callers cg func with [] -> false | _ :: _ -> true in
         if Cilutil.StringSet.mem func_name init_nodes 
           || (not has_callers && has_callees) 
         then roots := node_func::!roots;
         
         all := node_func::!all;
         if has_callees || has_callers then CallG.add_vertex ocg node_func;

         List.iter
           (fun func_callee ->
              let node_callee = get_fresh func_callee in
              CallG.add_edge ocg node_func node_callee)
           (G.callees cg func))
      cg;
    let roots =
      List.map (fun c -> let s = fresh_mark ()
                in c.mark <- Service s; add_service c ocg; (c, s)) !roots
    in
    List.iter (fun x -> pretty x "BEFORE DEC") !all;
    decompose ocg roots;
    List.iter (fun x -> pretty x "AFTER DEC") !all;
    (* assert (check ocg); *)
    merge_edges ocg

end (* functor Service *)
