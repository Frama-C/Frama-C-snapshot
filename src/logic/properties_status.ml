(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Db_types
module H = Hashtbl (* [Hashtbl] redefined by [Property] *)
open Property

let emitters = H.create 7

let max_state s1 s2 =
  if s2 = State.dummy || H.mem emitters s1 then s1 else s2

let strongest_status fold =
  fold
    (fun (s, st1 as acc) (x, st2 as v) -> match s, x with
    | Unknown, _ -> v
    | Checked _, Unknown -> acc
    | Checked {valid=v1}, Checked {valid=v2} ->
      match v1, v2 with
      | False, True ->
          Kernel.debug "Inconsistent status: %a/%a"
            Cil.d_annotation_status s
            Cil.d_annotation_status x;
          acc
      | True, False ->
          Kernel.debug "Inconsistent status: %a/%a"
            Cil.d_annotation_status s
            Cil.d_annotation_status x;
          v
      | True, Maybe | False, Maybe -> acc
      | Maybe, True | Maybe, False -> v
      | True, True | False, False | Maybe, Maybe -> s, max_state st1 st2)
    (Unknown, State.dummy)

let weakest_status l =
  List.fold_left
    (fun acc x -> match acc, x with
    | Unknown, _ | _,Unknown -> Unknown
    | Checked {valid=v1} as c1, (Checked {valid=v2} as c2) ->
      match v1, v2 with
      | True, _ -> c2
      | _, True -> c1
      | Maybe, _ -> c1
      | _, Maybe-> c2
      | False, False -> c1)
    (Checked {valid=True; emitter="nothing to prove"})
    l

(* this function must be called on an identified_property and code_annotation
   which are already known to be tied to the same statement.
   It performs too many tests: all identified_properties belonging to a
   spec can be directly tied to the stmtspec since there is at most one per
   statement. *)
let ip_is_in_code_annot annot ca =
  match annot, ca.annot_content with
    | IPBlob _, _ -> false
    | IPPredicate(PKAssumes _,_,_,_), AStmtSpec _ -> true
    | IPPredicate(PKAssumes _,_,_,_), _ -> false
    | IPPredicate(PKEnsures _,_,_,_), AStmtSpec _ -> true
    | IPPredicate(PKEnsures _,_,_,_), _ -> false
    | IPPredicate(PKTerminates,_,_,_), AStmtSpec _ -> true
    | IPPredicate(PKTerminates,_,_,_), _ -> false
    | IPPredicate(PKRequires _,_,_,_), AStmtSpec _ -> true
    | IPPredicate(PKRequires _,_,_,_), _ -> false
    | IPAxiom _,_ -> false
    | IPComplete(_,_,_), AStmtSpec _ -> true
    | IPComplete(_,_,_), _ -> false
    | IPDisjoint(_,_,_), AStmtSpec _ -> true
    | IPDisjoint _, _ -> false
    | IPAssigns (_,_,Id_behavior _,_), AStmtSpec _ -> true
    | IPAssigns (_,_,Id_behavior _,_),_ -> false
    | IPAssigns (_,_,Id_code_annot ca1,_), _ -> ca1.annot_id = ca.annot_id
    | IPFrom (_,_,Id_behavior _,_), AStmtSpec _ -> true
    | IPFrom (_,_,Id_behavior _,_),_ -> false
    | IPFrom (_,_,Id_code_annot ca1,_), _ -> ca1.annot_id = ca.annot_id
    | IPDecrease (_,_,None,_), AStmtSpec _ -> true
    | IPDecrease (_,_,None,_), _ -> false
    | IPDecrease (_,_,Some ca1,_),_ -> ca1.annot_id = ca.annot_id
    | IPBehavior (_,_,_), AStmtSpec _ -> true
    | IPBehavior _,_ -> false
    | IPCodeAnnot (_,_,ca1),_ -> ca1.annot_id = ca.annot_id

let get_dependencies annot = match get_kinstr annot with
  | Kglobal -> []
  | Kstmt stmt ->
    List.fold_left
      (fun acc (a, s) -> match a with
      | Before (User ca | AI(_, ca))
      | After (User ca | AI(_, ca)) ->
	if ip_is_in_code_annot annot ca then s :: acc
	else acc)
      []
      (Annotations.get_all stmt)

(* mutually recursive modules cannot be safely evaluated here *)
module Ref_graph = struct
  let create_and_add_state = ref (fun ~clear:_ ~name:_ ~deps:_ -> assert false)
  let add_state = ref (fun _ -> assert false)
  let remove_state = ref (fun ~reset:_ _ -> assert false)
  let self = ref State.dummy
end

module Dash =
  Dashtbl.Make
    (struct
      open Ref_graph
      let create_and_add_state ~clear ~name ~deps =
	!create_and_add_state ~clear ~name ~deps
      let add_state s = !add_state s
      let remove_state ~reset s = !remove_state ~reset s
      let self = self
      let internal_kind = `Correctness
     end)
    (Dashtbl.Default_key_marshaler(Datatype.Unit))
    (Dashtbl.Default_data_marshaler
       (Datatype.Ref(Cil_datatype.Annotation_status)))
    (struct let name = "Properties_status.Dash" end)

module Status =
  State_builder.Dashtbl
    (struct
      include Property
      type marshaled = t
      let marshal = function
	| IPBlob s -> ip_axiom ("$" ^ State.get_unique_name s)
	| x -> x
      let unmarshal = function
	| IPAxiom s when try s.[0] = '$' with Invalid_argument _ -> false ->
	  ip_blob (State.get (String.sub s 1 (String.length s - 1)))
	| x -> x
      let marshaler = marshal, unmarshal
      let equal_marshaled = equal
      let hash_marshaled = hash
     end)
    (Dash)
    (struct
       let name = "property status"
       let size = 7
       let dependencies = [ Ast.self ]
       let kind = `Internal
       let internal_kind = `Correctness
     end)

let () =
  Ref_graph.create_and_add_state := Status.Graph.create_and_add_state;
  Ref_graph.add_state := Status.Graph.add_state;
  Ref_graph.remove_state := Status.Graph.remove_state;
  Ref_graph.self := !Status.Graph.self

(* Rebuild dependencies for the internal dashtables *)
let () =
  Project.register_after_load_hook
    (fun () ->
      Status.iter
	(fun _ _ (d, _) ->
	  Dash.iter
	    (fun _ s (_, s') ->
	      assert (not (State.is_dummy s'));
 	      let from =
 		match s with
		| None -> [ !Status.Graph.self ]
		| Some s -> [ s; !Status.Graph.self ]
 	      in
 	      State_dependency_graph.Dynamic.add_codependencies ~onto:s' from)
	    d))

let get_name annot =
  let old = Parameters.UseUnicode.get () in
  Parameters.UseUnicode.set false;
  let s = Pretty_utils.sfprintf "%a" Property.pretty annot in
  Parameters.UseUnicode.set old;
  s

let rec generic_memo f ?who annot =
  match f ?who annot with
  | [] ->
    let h = Dash.create 7 in
    Status.add (get_name annot) annot (get_dependencies annot) h;
    generic_memo f ?who annot
  | [ x ] ->
    x
  | _ :: _ :: _ ->
    assert false

let memo_tbl = generic_memo Status.find_all_data
let memo_state_tbl = generic_memo Status.find_all_states
let memo_tbl_full = generic_memo Status.find_all

let get_all ?who annot =
  let h = memo_tbl annot in
  List.map (!) (Dash.find_all_data ?who h ())

(* when getting the strongest status of a behavior, we need to update its
   dependencies which are automatically computed by getters (see function
   [get] of [Make_updater]) *)
let get_all_behavior_ref = ref []

let strongest annot =
  let status, state = match annot with
    | IPBehavior _ ->
      let l = List.map (fun f -> f annot) !get_all_behavior_ref in
      strongest_status
	(fun f acc ->
	  List.fold_left
	    (fun acc b -> f acc b)
	    acc
	    l)
    | _ ->
      let h = memo_tbl annot in
      strongest_status
	(fun f acc -> Dash.fold (fun _ _ (v, s) acc -> f acc (!v, s)) h acc)
  in
  status, if State.is_dummy state then None else Some state

let get_state prop state = Status.find_state prop state

module Consolidation_tree = struct

  type 'a value =
      { value: 'a;
	hypothesis: forest;
	dependencies: State.t value list }

  and t =
      { property: identified_property;
	state: State.t;
	mutable status: (annotation_status * State.t) value list }

  and forest = t list

  type vertex =
    | Property of t
    | State of State.t value
    | Status of (annotation_status * State.t) value

  let state_of_vertex = function
    | Property p -> p.state
    | Status s -> snd s.value
    | State s -> s.value

  module Visited = H.Make(State)

  let get_binding visited annot (dash, dash_state) properties =
    let visit_property p s l b =
(*      Format.printf "visiting ppt %S: %d@." (State.name s) (List.length l);*)
      try
	match Visited.find visited s with
	| Property v as p, old_b ->
	  assert (not old_b && b && v.status = []);
	  v.status <- l;
	  Visited.replace visited s (p, b);
	  v
	| _ ->
	  assert false
      with Not_found ->
	let v = { property = p; state = s; status = l } in
	Visited.replace visited s (Property v, b);
	v
    in
    let rec get_hyps_deps s =
      State_dependency_graph.Dynamic.G.fold_pred
	(fun s (hyps, deps) ->
	  try
	    match Visited.find visited s with
	    | Property p, _ ->
	      (if State.equal p.state dash_state then hyps else p :: hyps),
	      deps
	    | State s, _ -> hyps, s :: deps
	    | Status _, _ -> assert false
	  with Not_found ->
	    (* break mutually recursive states *)
	    Visited.add
	      visited
	      s
	      (State { value = s; hypothesis = []; dependencies = [] }, true);
	    let v = visit_state s in
	    hyps, v :: deps)
	State_dependency_graph.Dynamic.graph
	s
	([], [])
    and visit_state s =
      let h, d = get_hyps_deps s in
      let v = { value = s; hypothesis = h; dependencies = d } in
      Visited.replace visited s (State v, true);
      v
    in
    let visit_status s v =
      let h, d = get_hyps_deps s in
      let v = { value = v, s; hypothesis = h; dependencies = d } in
      Visited.add visited s (Status v, true);
      v
    in
    let annot_codependencies =
      State_selection.Dynamic.only_codependencies dash_state
    in
    let get_status dash =
      Dash.fold
	(fun () _ (status, status_state) acc ->
	  if Visited.mem visited status_state then
	    acc
	  else begin
	    match !status with
	    | Unknown ->
	      Visited.add
		visited
		status_state
		(Status
		   { value = Unknown,
		     status_state; hypothesis = [];
		     dependencies = [] },
		 true);
	      acc (* do not add [v] *)
	    | Checked _ as status ->
	      State_selection.Dynamic.iter_in_order
		(fun s ->
		  if not (Visited.mem visited s) then
		    try
		      match Status.find_key s with
		      (* [s] corresponds to a property never seen yet *)
		      | [] -> assert false
		      | (p, _) :: tl ->
			(* each property of the list should be the same *)
			assert
			  (List.for_all
			     (fun (p', _) -> Property.equal p p') tl);
			ignore (visit_property p s [] false)
		    with Not_found ->
		      (* [s] does not correspond to any property *)
		      ignore (visit_state s))
		(State_selection.Dynamic.union
		   (State_selection.Dynamic.only_codependencies status_state)
		   annot_codependencies);
	      visit_status status_state status :: acc
	  end)
	dash
	[]
    in
    try
      match Visited.find visited dash_state with
      (* [annot] already visited as an hypothesis of another property:
	 only update its status. *)
      | Property p, false ->
	visit_property p.property p.state (get_status dash) true :: properties
      | Property p, true -> p :: properties
      | (State _ | Status _), _ ->
	assert false
    with Not_found ->
      (* [annot] never visited *)
      let s = get_status dash in
      visit_property annot dash_state s true :: properties

  let rec get_property visited annot =
    let dash_and_state = memo_tbl_full annot in
    let properties = get_binding visited annot dash_and_state [] in
    Visited.iter
      (fun _ v -> match v with
      | _, true -> () (* already done *)
      | Property p, false ->
	(* update it *)
	ignore (get_property visited p.property)
      | (State _ | Status _), false -> assert false)
      visited;
    match properties with
    | [] | _ :: _ :: _ -> assert false
    | [ p ] -> p

  let get annot = get_property (Visited.create 17) annot

  let get_all () =
    let visited = Visited.create 17 in
    Status.fold
      (fun a _ dash_and_state acc -> get_binding visited a dash_and_state acc)
      []

  type edge = And | Or

  module G =
    Graph.Persistent.Digraph.ConcreteLabeled
      (struct
	type t = vertex
	let compare x y = match x, y with
	  | Property p1, Property p2 -> State.compare p1.state p2.state
	  | Status s1, Status s2 ->
	    State.compare (snd s1.value) (snd s2.value)
	  | State s1, State s2 -> State.compare s1.value s2.value
	  | Property _, (State _ | Status _) -> -1
	  | (State _ | Status _), Property _ -> 1
	  | State _, Status _ -> -1
	  | Status _, State _ -> 1
	let equal x y = compare x y = 0
	let hash = function
	  | Property p -> H.hash (0, State.hash p.state)
	  | State s -> H.hash (1, State.hash s.value)
	  | Status s -> H.hash (2, State.hash (snd s.value))
       end)
      (struct
	type t = edge
	let default = And
	let compare : edge -> edge -> int = Extlib.compare_basic
       end)

  let rec add_property visited g p =
    let state = p.state in
    if Visited.mem visited state then
      g
    else begin
      Visited.add visited state ();
      let pp = Property p in
      let status = p.status in
      match status with
      | [] ->
	G.add_vertex g pp
      | _ :: _ ->
	List.fold_left
	  (fun g s ->
	    let g = G.add_edge_e g (pp, Or, Status s) in
	    add_status visited g s)
	  g
	  status
    end

  and add_status visited g s =
    let state = snd s.value in
    assert (not (Visited.mem visited state));
    Visited.add visited state ();
    let ss = Status s in
    let g =
      List.fold_left
	(fun g h ->
	  let g = G.add_edge_e g (ss, And, Property h) in
	  add_property visited g h)
	g
	s.hypothesis
    in
    List.fold_left
      (fun g d ->
	let g = G.add_edge_e g (ss, And, State d) in
	add_state visited g d)
      g
      s.dependencies

  (* could be merged with [add_status] in OCaml 3.12:
     requires polymorphic recursion *)
  and add_state visited g s =
    let state = s.value in
    if Visited.mem visited state then
      g
    else begin
      Visited.add visited state ();
      let ss = State s in
      let g =
	List.fold_left
	  (fun g h ->
	    let g = G.add_edge_e g (ss, And, Property h) in
	    add_property visited g h)
	  g
	  s.hypothesis
      in
      List.fold_left
	(fun g d ->
	  let g = G.add_edge_e g (ss, And, State d) in
	  add_state visited g s)
	g
	s.dependencies
    end

  let generic_get_graph f =
    let t = f () in
    let g = List.fold_left (add_property (Visited.create 17)) G.empty t in
    let module R =
	  State_dependency_graph.Remove_useless_states
	    (G)(struct let kind v = State.kind (state_of_vertex v) end)
    in
    R.get g

  let get_full_graph () = generic_get_graph get_all
  let get_graph p = generic_get_graph (fun () -> [ get p ])

  let dump graph dot_file =
    let module Dot =
	  Graph.Graphviz.Dot
	    (struct
	      include G
	      let status_color = function
		| Checked { valid = False } -> 0xff0000
		| Checked { valid = True } -> 0x00ff00
		| Unknown | Checked { valid = Maybe } -> 0xffa500 (* orange *)
	      let graph_attributes _ = [(* `Ratio (`Float 0.25)*) ]
	      let vertex_name s =
		"\"" ^ State.get_unique_name (state_of_vertex s) ^ "\""
	      let vertex_attributes s =
		let label s = `Label (String.escaped (State.get_name s)) in
		match s with
		| Property p ->
		  let s = p.state in
		  assert (State.kind s = `Correctness);
		  let fontcolor = status_color (fst (strongest p.property)) in
		  [ label s; `Color 0x4682b4; `Fontcolor fontcolor;
		    `Shape `Diamond; `Style `Filled ]
		| Status status ->
		  let v, s = status.value in
		  assert (State.kind s = `Correctness);
		  let color = status_color v in
		  [ label s; `Color color; `Style `Filled; `Shape `Box ]
		| State s ->
		  let s = s.value in
		  match State.kind s with
		  | `Irrelevant ->
		    Kernel.abort
		      "State %s is not relevant here"
		      (State.get_name s)
		  | `Internal | `Proxy `Internal ->
		    [ `Label ""; `Height 0.; `Width 0.; `Style `Filled ]
		  | `Tuning -> [ label s; `Style `Dotted ]
		  | `Correctness | `Proxy `Correctness ->
		    [ label s; `Color 0xb0c4de; `Style `Filled ]
	      let edge_attributes (_src, lab, dst) =
		let s = state_of_vertex dst in
		match State.kind s with
		| `Internal | `Proxy `Internal ->
		  [ `Constraint false; `Arrowhead `None ]
		| `Tuning
		| `Correctness | `Proxy `Correctness ->
		  (match lab with
		  | And ->
		    let c = 0x8b4513 in
		    [ `Label "AND"; `Color c; `Fontcolor c; `Style `Bold ]
		  | Or ->
		    let c = 0x228b22 in
		    [ `Label "OR"; `Color c; `Fontcolor c; `Style `Bold ])
		| `Irrelevant ->
		  Kernel.abort
		    "State %s is not relevant here"
		    (State.get_name s)
	      let default_vertex_attributes _ = []
	      let default_edge_attributes _ = []
	      let get_subgraph _ = None
       end)
    in
    let cout = open_out dot_file in
    Dot.output_graph cout graph;
    close_out cout

end

let pretty_all fmt annot =
  let all_status =
    let h = memo_tbl annot in
    List.fold_left
      (fun acc s -> match !s with Unknown -> acc | Checked _ as s -> s :: acc)
      []
      (Dash.find_all_data h ())
  in
  Pretty_utils.pp_list ~sep:";" Cil.d_annotation_status fmt all_status

module Make_updater
  (P: sig
     val name: string
     val emitter: State.t
   end) =
struct

  let () = H.add emitters P.emitter ()

  let get_name annot s =
    let old = Parameters.UseUnicode.get () in
    Parameters.UseUnicode.set false;
    let status_name = function
      | False -> "Invalid"
      | True -> "Valid"
      | Maybe -> "Maybe"
    in
    let s = match s with
      | Unknown ->
	Pretty_utils.sfprintf "Unknown: %a" Property.pretty annot
      | Checked c  ->
(*	assert (c.emitter = State.name P.emitter);*)
	Pretty_utils.sfprintf  "%s for %s: %a"
	  (status_name c.valid)
	  c.emitter
	  Property.pretty annot
    in
    Parameters.UseUnicode.set old;
    s

  let get_state_hypothesis hyps =
    List.fold_left
      (fun acc h -> memo_state_tbl h :: acc)
      [ P.emitter ]
      hyps

  let full_update must_add annot hyps f =
    let h, hs = memo_tbl_full annot in
    match Dash.find_all_local h () P.emitter with
    | [] ->
      let new_s = f Unknown in
      if must_add || new_s <> Unknown then begin
	let state_hyps = get_state_hypothesis hyps in
	(* TODO: should detect more general cycles in the dependency graph *)
	if List.exists (State.equal hs) state_hyps then
	  Kernel.fatal
	    "inconsistency detected: property %a depends of itself."
	    Property.pretty annot;
	Dash.add
	  h
	  (get_name annot new_s)
	  ()
	  (hs :: state_hyps)
	  (ref new_s)
      end;
      new_s
    | [ d, s ] ->
      (* TODO: does not try yet to update the hypothesis:
	 Should we do? *)
      d := f !d;
      let new_d = !d in
      State.set_name s (get_name annot new_d);
      new_d
    | _ :: _ :: _ ->
      assert false

  let rec compute_behavior kf st b =
    let all_ip = Property.ip_post_cond_of_behavior kf st b in
    let all_status = List.map get all_ip in
    weakest_status all_status

  and get annot = match annot with
    | IPBehavior(kf, st, b) ->
      let post_conds = Property.ip_post_cond_of_behavior kf st b in
      full_update false annot post_conds (fun _ -> compute_behavior kf st b)
    | _ ->
      full_update false annot [] (fun x -> x)

  let () =
    get_all_behavior_ref :=
      (fun annot ->
	let status = get annot in
	let h = memo_tbl annot in
	let state =
	  try Dash.find_state h () P.emitter
	  with Not_found ->
	    assert (status = Unknown);
	    State.dummy
	in
	status, state)
    :: !get_all_behavior_ref

  let update annot hyps f =
    match annot with
    | IPBehavior _ -> Kernel.fatal "cannot modify a behavior status"
    | _ -> full_update true annot hyps f

  let set annot hyps status =
    ignore (full_update true annot hyps (fun _ -> status))

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
