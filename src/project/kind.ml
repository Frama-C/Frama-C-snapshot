(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

type how =
  | Do_Not_Select_Dependencies
  | Select_Dependencies
  | Only_Select_Dependencies

module type SELECTION = sig
  type kind
  type t
  val ty: t Type.t
  val empty: t
  val is_empty: t -> bool
  val add: kind -> how -> t -> t
  val singleton : kind -> how -> t
  val remove: kind -> t -> t
  val iter: (kind -> how -> unit) -> t -> unit
  val fold: (kind -> how -> 'a -> 'a) -> t -> 'a -> 'a
end

let version = ref ""

module Make
  (T: sig  
     type t 
     val dummy: t
     val name: string 
     val kind_name: t -> string
   end) = 
struct

  module V = struct
    type t = { mutable label: T.t; mutable mark: int }
    let create x = { label = x; mark = 0 }
    let compare x y = 
      String.compare (T.kind_name x.label) (T.kind_name y.label)
(*    let equal x y = T.kind_name x.label = T.kind_name y.label*)
    let equal = (==) (* vertices are hash-consed *)
    let hash x = Hashtbl.hash (T.kind_name x.label)
  end
  let compare = V.compare
  let equal = V.equal
  let hash = V.hash

  let dummy = V.create T.dummy

  module D = Graph.Imperative.Digraph.ConcreteBidirectional(V)
  type t = D.V.t

  let deps = D.create ()
    (** The dependencies graph. *)

  let value x = x.V.label
  let node_name v = T.kind_name (value v)

  let iter f p = D.iter_vertex (fun v -> f (value v) p) deps
  let nb_kinds () = D.nb_vertex deps

  exception DependencyAlreadyExists of string * string

  let gen_add_dependency g v1 v2 =
    assert( 
      (* do not check with -noassert *)
      if D.mem_edge g v2 v1 then 
	raise (DependencyAlreadyExists(node_name v1, node_name v2));
      true);
    D.add_edge g v2 v1
     
  let add_dependency = gen_add_dependency deps

  let vertices = Hashtbl.create 997
  let get_from_name = Hashtbl.find vertices

  let gen_create =
    let module S = Set.Make(V) in
    fun g k dependencies ->
      let v = 
	let name = T.kind_name k in
	try 
	  Hashtbl.find vertices name
	with Not_found -> 
	  let v = V.create k in
	  Hashtbl.add vertices name v;
	  v
      in
      begin try
	(* if [v] already belongs to [g], check that it is exactly
	   registered with the same dependencies. *)
	let check v l =
	  assert (* do not check with -noassert *)
	    (if not (List.exists (V.equal v) l) then begin
	       Format.eprintf 
		 "The kind %s of %s is created twice with different \
dependencies:@\n%s@\n%s@." 
		 (T.kind_name k) 
		 T.name 
		 (List.fold_left 
		    (fun acc v -> node_name v ^ " " ^ acc) "" dependencies)
		 (D.fold_pred (fun v acc -> node_name v ^ " " ^ acc) g v "");
	       false
	     end else
	       true)
	in
	(* [D.iter_pred] raises [Invalid_argument] iff [v] \notin [g]. *)
	D.iter_pred (fun v' -> check v' dependencies) g v;
	List.iter (fun v' -> check v' (D.pred g v)) dependencies
      with Invalid_argument _s ->
	(* [v] does not belong to [g] *)
	D.add_vertex g v;
	let already = ref S.empty in
	List.iter 
	  (fun v' ->
	     (* consider each dependency only once in order to don't break the
		invariant of [add_dependency]. *)
	     if not (S.mem v' !already) then begin
	       already := S.add v' !already;
	       gen_add_dependency g v v'
	     end) 
	  dependencies;
      end;
      v

  let create = gen_create deps

  module Dynamic = struct

    type kind = t
    type graph = D.t
    type t = D.t ref

    let graphs : t list ref = ref []
    let unmarshals : (kind -> unit) list ref = ref []

    let create_graph () = D.create ()

    let create () =
      let dyn = ref (D.create ()) in
      graphs := dyn :: !graphs;
      dyn

    let remove_vertex g v =
      let del v =
	D.remove_vertex g v;
	if not (List.exists (fun dyn -> D.mem_vertex !dyn v) !graphs) 
	  && not (D.mem_vertex deps v)
	then 
	  Hashtbl.remove vertices (node_name v)
      in
      try
	(* also remove predecessors with no other successor *)
	D.iter_pred
	  (fun v -> 
	     assert (D.in_degree g v = 0);
	     if D.out_degree g v = 1 then del v) 
	  g 
	  v;
	del v
      with Invalid_argument _ ->
	(* [v] already removed in this iteration *)
	()

    let clear_graph g = D.clear g
 
    let add_dependency dyn = gen_add_dependency !dyn
    let add_kind dyn = gen_create !dyn
    let remove_kind dyn = remove_vertex !dyn

    module V' = struct
      include String
      let equal = (=)
      let hash = Hashtbl.hash
    end
    module D' = Graph.Imperative.Digraph.ConcreteBidirectional(V')

    type marshalled_graph = D'.t

    let marshal g =
      let g' = D'.create () in
      D.iter_edges
	(fun v1 v2 -> D'.add_edge g' (node_name v1) (node_name v2)) 
	g;
      g'

    let already_unmarshaled = Hashtbl.create 17
    let before_load () = assert (Hashtbl.length already_unmarshaled = 0)
    let after_load () = Hashtbl.clear already_unmarshaled

    let unmarshal =
      let module Map = 
	Graph.Gmap.Edge
	  (struct 
	     type t = D'.t
	     let fold_edges_e = D'.fold_edges_e
	     module E = Graph.Util.HTProduct(D'.V)(D'.V)
	   end)
	  (struct 
	     type t = D.t
	     type edge = D.edge
	     let empty () = D.create () 
	     let add_edge_e g e = D.add_edge_e g e; g 
	   end) 
      in
      fun new_dyn update ->
	Map.map 
	  (fun (src', dst') -> 
	     let mk s' =
	       let s = 
		 try Hashtbl.find vertices s'
		 with Not_found ->
		   let v =
		     V.create
		       (try (get_from_name s').V.label 
			with Not_found -> new_dyn s')
		   in
		   Hashtbl.add vertices s' v;
		   v
	       in
	       if 
		 not (Hashtbl.mem already_unmarshaled s || D.mem_vertex deps s) 
	       then begin
		 Hashtbl.add already_unmarshaled s ();
		 update s
	       end;
	       s
	     in
	     mk src', mk dst')

  end

  module Selection = struct
    type kind = V.t
    module M = Map.Make(V)
    type t = how M.t
    type selection = t
    let empty = M.empty
    let is_empty = M.is_empty
    let add = M.add
    let remove = M.remove
    let find k v = try Some (M.find k v) with Not_found -> None
    let singleton s d = M.add s d M.empty
    let iter = M.iter
    let fold = M.fold
    let ty = 
      Type.register 
	~name:(T.name ^ ".Selection.t") 
	~value_name:(Some "Project.Selection.ty")
	(* Too difficult to print non-empty selection.
	   It is correct to always print a selection as empty
	   since the empty selection is the "biggest" one affecting
	   each of its states :). 
	   [JS 2010/03/04] well it may break previous behaviour... So not so
	   correct :-( *)
	~pp:(fun _ fmt _ -> Format.fprintf fmt "Project.Selection.empty")
	[ M.empty ]
  end

  module Make_Topological
    (G: sig
       val iter_vertex: (V.t -> unit) -> unit
       val iter_succ: (V.t -> unit) -> V.t -> unit
       val in_degree: V.t -> int
       val fold_pred: (V.t -> 'a ->  'a) -> V.t -> 'a -> 'a
     end)
    = 
  struct

    module Mark = struct
      let clear () = G.iter_vertex (fun v -> v.V.mark <- 0)
      let get v = v.V.mark
      let set v n = v.V.mark <- n
    end

    module D_As_Arg = struct
      type t = D.t
      module V = V
      let iter_vertex f _ = G.iter_vertex f
      let iter_succ f _ = G.iter_succ f
      let in_degree _ = G.in_degree
    end

    module T = Graph.Topological.Make(D_As_Arg)

    exception Inconsistent_Selection

    let apply only except f acc =
      Mark.clear ();
      let skip_only = Selection.is_empty only in
      let compute v acc =
	(* possible values for marks:
	   0 ==> No indication
	   1 ==> Explicitely continue to visit the dependencies
	   2 ==> Do not continue to visit the dependencies 
	   3 ==> No indication, but already visited *)
	let ponly = Selection.find v only in
	let pexcept = Selection.find v except in
	let old_mark = Mark.get v in
	let f v acc = if old_mark = 0 then f v acc else acc in
	Mark.set v 3;
	let continue = 
	  G.fold_pred
	    (fun p acc ->
	       match acc, Mark.get p with
	       | _, (0 | 3) -> 
(*		   Format.printf "case 1 for parent %S@." (node_name p);*)
		   acc
	       | Some true, (1 as m) | Some false, (2 as m) -> 
(*		   Format.printf "case 2 for parent %S@." (node_name p);*)
		   Mark.set v m;
		   acc
	       | Some true, 2 | Some false, 1 ->
		   raise Inconsistent_Selection;
	       | None, 1 ->
(*		   Format.printf "case 4 for parent %S@." (node_name p);*)
		   Mark.set v 1;
		   Some true
	       | None, 2 ->
(*		   Format.printf "case 5 for parent %S@." (node_name p);*)
		   Mark.set v 2;
		   Some false
	       | _, _ -> 
		   assert false)
	    v
	    None
	in
(*	Format.printf "visiting %s with mark %d; old mark %d@." 
	  (node_name v) (Mark.get v) old_mark;*)
	(* Do not unify patterns below: order matters because it checks
	   [pexcept] before [ponly]. *)
	match skip_only, ponly, pexcept, continue with
	| true, None, None, (None | Some true) ->
	    (* Nothing special for this node *)
	    f v acc
	| false, None, None, Some true ->
	    (* Not selected but one parent has selected its dependencies *)
	    f v acc
	| _, None, None, (None | Some false) ->
	    (* Not selected and 
	       (no parent selected or one parent has deselected its
	       dependencies) *)
	    acc
	| _, _, Some Do_Not_Select_Dependencies, _ ->
	    (* Explicitely not selected, but do not concern the dependencies *)
	    acc
	| _, _, Some Select_Dependencies, _ ->
	    (* Explicitely not selected, and do also concern the dependencies *)
	    Mark.set v 2;
	    acc
	| _, _, Some Only_Select_Dependencies, _ ->
	    (* Do explicitely not select the dependencies *)
	    Mark.set v 2;
	    f v acc
	| true, Some _, _, _ ->
	    (* [only] is empty and cannot contain anything *)
	    assert false
	| false, Some Do_Not_Select_Dependencies, None, _ ->
	    (* Explicitely selected, but do not concern the dependencies *)
	    f v acc
	| false, Some Select_Dependencies, None, _ ->
	    (* Explicitely selected, and do also concern the dependencies *)
	    Mark.set v 1;
	    f v acc
	| false, Some Only_Select_Dependencies, None, _ ->
	    (* Do explicitely select the dependencies *)
	    Mark.set v 1;
	    acc
      in
      T.fold compute deps acc

    let fold only except f = apply only except (fun v -> f (value v))
    let iter only except f p = fold only except (fun v () -> f v p) ()

    let number_of_applicants only except = 
      if Selection.is_empty only && Selection.is_empty except then None
      else Some (apply only except (fun _ -> succ) 0)

  end
    
  module Static_Topological = 
    Make_Topological
      (struct
	 let iter_vertex f = D.iter_vertex f deps
	 let iter_succ f = D.iter_succ f deps
	 let in_degree v = D.in_degree deps v
	 let fold_pred f = D.fold_pred f deps
       end)

  let apply_in_order = Static_Topological.apply
  let fold_in_order = Static_Topological.fold
  let iter_in_order = Static_Topological.iter
  let number_of_applicants = Static_Topological.number_of_applicants

  module Dynamic_Topological = 
    Make_Topological
      (struct
	 let iter f = f deps; List.iter (fun dyn -> f !dyn) !Dynamic.graphs
	 let iter_vertex f = 
	   let visited = Hashtbl.create 7 in
	   let f v = 
	     let s = node_name v in
	     if not (Hashtbl.mem visited s) then begin
	       Hashtbl.add visited s ();
	       f v
	     end
	   in
	   iter (D.iter_vertex f)
	 let iter_succ f v = 
	   let safe g = try D.iter_succ f g v with Invalid_argument _ -> () in
	   iter safe
	 let fold f acc = 
	   let safe g acc = try f g acc with Invalid_argument _ -> acc in
	   let acc = safe deps acc in
	   List.fold_left (fun acc dyn -> safe !dyn acc) acc !Dynamic.graphs
	 let in_degree v = fold (fun g acc -> acc + D.in_degree g v) 0
	 let fold_pred f v acc = fold (fun g acc -> D.fold_pred f g v acc) acc
       end)
      
  let full_apply_in_order = Dynamic_Topological.apply
  let full_iter_in_order = Dynamic_Topological.iter
  let full_number_of_applicants = Dynamic_Topological.number_of_applicants

  module Make_Display
    (G: sig
       val apply_in_order:
	 Selection.t -> Selection.t -> (t -> 'a -> 'a) -> 'a -> 'a
       val iter_succ: (V.t -> unit) -> V.t -> unit
       val get_graph: V.t -> string option
     end) =
  struct

    open Graph.Graphviz
    
    module Attributes = struct
      include D

      let color s = `Color (Extlib.number_to_color (int_of_string s))

      let vertex_name v = 
	let s = node_name v in
	"\"" 
	^
	  (*      ^ (if String.length s > 15 then 
		  String.sub s 0 5 ^ " ... " ^ String.sub s 10 5
		  else
		  s)*)s
	^ "\""

      let graph_attributes _ = []

      let default_vertex_attributes _ = []
      let vertex_attributes v = 
	match G.get_graph v with | None -> [] | Some s -> [ color s ]

      let default_edge_attributes _ = []
      let edge_attributes (src, dst) = 
	match G.get_graph src, G.get_graph dst with
	| None, None -> []
	| None, Some _ | Some _, None -> assert false
	| Some s1, Some s2 when s1 = s2 -> [ color s1 ]
	| Some _, Some _ -> [ `Color 0xff0000 (* red *) ]

      let get_subgraph v = 
	Extlib.opt_map
	  (fun s -> { DotAttributes.sg_name = s; sg_attributes = [ color s ] })
	  (G.get_graph v)

    end

    module Dot = Dot(Attributes)

    let dump_dependencies 
	?(only=Selection.empty) ?(except=Selection.empty) fname = 
      let g = D.create () in
      (* Copy the selected vertices in [g] *)
      G.apply_in_order only except (fun s () -> D.add_vertex g s) ();
      (* Copy the edges between the selected vertices in [g] *)
      D.iter_vertex 
	(fun v ->
	   G.iter_succ
	     (fun v' -> if D.mem_vertex g v' then D.add_edge g v v') v)
	g;
      (* output the graph *)
      let cout = open_out fname in
      Dot.output_graph cout g;
      close_out cout

  end

  module Static_Display = 
    Make_Display
      (struct
	 let iter_succ f = D.iter_succ f deps
	 let apply_in_order = apply_in_order
	 let get_graph _ = None
       end)

  let dump_dependencies = Static_Display.dump_dependencies

  module Dynamic_Display =
    Make_Display
      (struct
	 let iter f = f deps; List.iter (fun dyn -> f !dyn) !Dynamic.graphs
	 let iter_succ f v = 
	   let safe g = try D.iter_succ f g v with Invalid_argument _ -> () in
	   iter safe
	 let apply_in_order = full_apply_in_order
	 exception Found of int
	 let get_graph s =
	   if D.mem_vertex deps s then
	     Some (string_of_int 0)
	   else
	     try
	       let mem n g = 
		 try if D.in_degree g s = 0 then succ n else raise (Found n)
		 with Invalid_argument _ -> succ n
	       in
	       ignore (List.fold_left (fun n g -> mem n !g) 1 !Dynamic.graphs);
	       assert false
	     with Found n ->
	       Some (string_of_int n)
       end)

  let dump_dynamic_dependencies = Dynamic_Display.dump_dependencies

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
