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

(* $Id: kind.ml,v 1.8 2008/07/11 09:18:50 uid568 Exp $ *)

type how =
  | Do_Not_Select_Dependencies
  | Select_Dependencies
  | Only_Select_Dependencies

module type SELECTION = sig
  type kind
  type t
  val empty : t
  val add: kind -> how -> t -> t
  val singleton : kind -> how -> t
  val remove: kind -> t -> t
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
    type t = { label: T.t; mutable mark: int }
    let create x = { label = x; mark = 0 }
    let compare x y = 
      String.compare (T.kind_name x.label) (T.kind_name y.label)
    let equal x y = T.kind_name x.label = T.kind_name y.label
    let hash x = Hashtbl.hash (T.kind_name x.label)
  end

  module D = Graph.Imperative.Digraph.ConcreteBidirectional(V)

  type t = D.V.t

  let deps = D.create ()
    (** The dependencies graph. *)

  module Mark = struct
    open V
    let clear () = D.iter_vertex (fun v -> v.mark <- 0) deps
    let get v = v.mark
    let set v n = v.mark <- n
  end

  let value x = x.V.label
  let node_name v = T.kind_name (value v)

  let iter f = D.iter_vertex f deps

  let nb_kinds () = D.nb_vertex deps

  exception Circular of D.V.t * D.V.t
  exception DependencyAlreadyExists of string * string

  let add_dependency k1 k2 =
    assert( 
      (* do not check with -noassert *)
      if D.mem_edge deps k2 k1 then 
	raise (DependencyAlreadyExists(node_name k1, node_name k2));
      if D.mem_edge deps k1 k2 then raise (Circular(k1, k2));
      true);
    D.add_edge deps k2 k1
     
  let create =
    let module S = Set.Make(V) in
    fun k dependencies ->
      let v = V.create k in
      begin try
	(* if [v] already belongs to [deps], check that it is exactly
	   registered with the same dependencies. *)
	let check v l =
	  assert (* do not check with -noassert *)
	    (if not (List.exists (V.equal v) l) then begin
	       Format.eprintf 
		 "The kind %s of %s is created twice with different dependencies:
%s@.%s@." 
		 (T.kind_name k) 
		 T.name 
		 (List.fold_left 
		    (fun acc v -> node_name v ^ " " ^ acc) "" dependencies)
		 (D.fold_pred (fun v acc -> node_name v ^ " " ^ acc) deps v "");
	       false
	     end else
	       true)
	in
	(* [D.iter_pred] raises [Invalid_argument] iff [v] \notin [deps]. *)
	D.iter_pred (fun v' -> check v' dependencies) deps v;
	List.iter (fun v' -> check v' (D.pred deps v)) dependencies
      with Invalid_argument _s ->
	(* [v] does not belong to [deps] *)
	D.add_vertex deps v;
	let already = ref S.empty in
	List.iter 
	  (fun v' ->
	     (* consider each dependency only once in order to don't break the
		invariant of [add_dependency]. *)
	     if not (S.mem v' !already) then begin
	       already := S.add v' !already;
	       add_dependency v v'
	     end) 
	  dependencies
      end;
      v

  let dummy = V.create T.dummy
    
  module Topological = Graph.Topological.Make(D)

  let digest () = 
    (*let names = 
      Topological.fold 
	(fun v acc -> 
(*	   Format.printf "digest for %s@." (node_name v);*)
	   node_name v ^ acc) 
	deps 
	"" 
    in*)
    (*Digest.string (!version ^ names)*)
    Digest.string !version

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
  end

  let iter f p = iter (fun v -> f (value v) p)

  exception Inconsistent_Selection

  let apply_in_order only except f acc =
    Mark.clear ();
    let skip_only = Selection.is_empty only in
    let compute v acc =
      (* possible values for marks:
	 0 ==> no indication
	 1 ==> Explicitely continue to visit the dependencies
	 2 ==> Do not continue to visit the dependencies *)
      (*      let f v acc = 
	      Cil.log "apply %s@." (kind v).sname; f v acc 
	      in*)
      let ponly = Selection.find v only in
      let pexcept = Selection.find v except in
      let continue = 
	D.fold_pred
	  (fun p acc ->
	     match acc, Mark.get p with
	     | _, 0 -> acc
	     | Some true, (1 as m) | Some false, (2 as m) -> 
		 Mark.set v m;
		 acc
	     | Some true, 2 | Some false, 1 -> 
		 raise Inconsistent_Selection
	     | None, 1 ->
		 Mark.set v 1;
		 Some true
	     | None, 2 ->
		 Mark.set v 2;
		 Some false
	     | _, _ -> 
		 assert false)
	  deps
	  v
	  None
      in
(*      Cil.log "continue for %s: %b@." 
	(kind v).sname
	continue;*)
      (*let b b = Some (v, b) in*)
      (* Do not unify patterns below: order is important because check first
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
	     dependencies *)
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
    Topological.fold compute deps acc

  let fold_in_order only except f =
    apply_in_order only except (fun v -> f (value v))

  let iter_in_order only except f p =
    fold_in_order only except (fun v () -> f v p) ()

  module Display = struct
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
    include D
    let vertex_name v = "\"" ^ node_name v ^ "\""
  end
  module Dot = Graph.Graphviz.Dot(Display)

  let dump_dependencies 
      ?(only=Selection.empty) ?(except=Selection.empty) fname = 
    let g = D.create () in
    (* Copy the selected vertices in [g] *)
    apply_in_order only except 
      (fun s () -> D.add_vertex g s) ();
    (* Copy the edges between the selected vertices in [g] *)
    D.iter_vertex 
      (fun v -> 
	 D.iter_succ 
	   (fun v' -> if D.mem_vertex g v' then D.add_edge g v v') deps v)
      g;
    (* output the graph *)
    let cout = open_out fname in
    Dot.output_graph cout g;
    close_out cout

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
