(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** traces domain *)

(** This domain build an over-approximation of all the traces that leads to a statement.
    These sets of traces is represented using CFGs.
*)

module OCamlGraph = Graph
module Frama_c_File = File
open Cil_types
open Cil_datatype

[@@@ warning "-40-42"]

module Node : sig
  include Datatype.S_with_collections
  val id: t -> int
  val start: t
  val dumb: t
  val next: unit -> t
end = struct
  include Datatype.Int
  let id x = x
  let start = 0
  let dumb = (- 1)
  module Counter =
    State_builder.Counter
      (struct let name = "Value.Traces_domain.Node.Counter" end)
  let next () = Counter.next ()
end

(** Can't use Graph.t it needs an impossible recursive module *)
module GraphShape = Hptmap.Shape(Node)

type node = Node.t

type transition =
  | Assign of kinstr * lval * typ * exp
  | Assume of stmt * exp * bool
  | EnterScope of kernel_function * varinfo list
  | LeaveScope of kernel_function * varinfo list
  (** For call of functions without definition *)
  | CallDeclared of kernel_function * exp list * lval option
  | Loop of stmt * node (** start *) * edge list GraphShape.t
  | Msg of string

and edge = {
  edge_trans : transition;
  edge_dst : node;
}

module rec Edge : sig
  include Datatype.S_with_collections with type t = edge
  val succ : t -> node
  val change_next : node -> t -> t

  val has_transition : transition -> edge -> bool

  val pretty_list : t list Pretty_utils.formatter
end = struct
  module T = struct
    type t = edge
    let name = "Value.Traces_domain.Edge"

    include Datatype.Serializable_undefined

    let reprs = [ {
        edge_trans = Msg "msg";
        edge_dst = Node.start;
      }]

    let structural_descr = Structural_descr.t_abstract

    let compare (e1: t) (e2 : t) =
      let c = Node.compare e1.edge_dst e2.edge_dst in
      if c <> 0 then c else
        Transition.compare e1.edge_trans e2.edge_trans

    let equal = Datatype.from_compare

    let pretty fmt e =
      Format.fprintf fmt "@[<hv 2>%a @[-> %a@]@]"
        Transition.pretty e.edge_trans Node.pretty e.edge_dst

    let hash e =
      Hashtbl.seeded_hash (Node.hash e.edge_dst) (Transition.hash e.edge_trans)
  end

  include Datatype.Make_with_collections(T)

  let has_transition t e = Transition.equal t e.edge_trans

  let pretty_list fmt l = Pretty_utils.pp_list ~sep:";@ " pretty fmt l

  let succ e = e.edge_dst
  let change_next n e = { e with edge_dst = n }
end
and Transition : sig
  type t = transition

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val pretty : t Pretty_utils.formatter
end = struct
  type t = transition

  let compare (t1: t) (t2: t) = match t1, t2 with
    | Assign (_, loc1, typ1, exp1), Assign (_, loc2, typ2, exp2) ->
      let c = Lval.compare loc1 loc2 in
      if c <> 0 then c else
        let c = Typ.compare typ1 typ2 in
        if c <> 0 then c else
          ExpStructEq.compare exp1 exp2
    | Assume (_, e1, b1), Assume (_, e2, b2) ->
      let c = ExpStructEq.compare e1 e2 in
      if c <> 0 then c else
        Transitioning.Stdlib.compare b1 b2
    | EnterScope (_, vs1), EnterScope (_, vs2) ->
      Extlib.list_compare Varinfo.compare vs1 vs2
    | LeaveScope (_, vs1), LeaveScope (_, vs2) ->
      Extlib.list_compare Varinfo.compare vs1 vs2
    | CallDeclared (kf1, es1, lv1), CallDeclared (kf2, es2, lv2) ->
      let c = Kernel_function.compare kf1 kf2 in
      if c <> 0 then c else
        let c = Extlib.list_compare ExpStructEq.compare es1 es2 in
        if c <> 0 then c else
          Extlib.opt_compare Lval.compare lv1 lv2
    | Msg s1, Msg s2 ->
      String.compare s1 s2
    | Loop (stmt1, s1, g1), Loop (stmt2, s2, g2) ->
      let c = Stmt.compare stmt1 stmt2 in
      if c <> 0 then c else
        let c = Node.compare s1 s2 in
        if c <> 0 then c else
          GraphShape.compare (Extlib.list_compare Edge.compare) g1 g2
    | Assign _, _ -> -1
    | _ , Assign _ -> 1
    | Assume _, _ -> -1
    | _ , Assume _ -> 1
    | EnterScope _, _ -> -1
    | _ , EnterScope _ -> 1
    | LeaveScope _, _ -> -1
    | _ , LeaveScope _ -> 1
    | CallDeclared _, _ -> -1
    | _ , CallDeclared _ -> 1
    | Msg _, _ -> -1
    | _, Msg _ -> 1

  let equal t1 t2 = (compare t1 t2 = 0)

  let pretty fmt = function
    | Assign (_, loc, _typ, exp) ->
      Format.fprintf fmt "Assign:@ %a = %a"
        Lval.pretty loc ExpStructEq.pretty exp
    | Assume (_, e, b) ->
      Format.fprintf fmt "Assume:@ %a %b" ExpStructEq.pretty e b
    | EnterScope (_, vs) ->
      Format.fprintf fmt "EnterScope:@ %a"
        (Pretty_utils.pp_list ~sep:"@ " Varinfo.pretty) vs
    | LeaveScope (_, vs) ->
      Format.fprintf fmt "LeaveScope:@ %a"
        (Pretty_utils.pp_list ~sep:"@ " Varinfo.pretty) vs
    | CallDeclared(kf1, exp1, lval1) ->
      Format.fprintf fmt "CallDeclared:@ %a%s(%a)"
        (Pretty_utils.pp_opt ~pre:"" ~suf:" =@ " Lval.pretty) lval1
        (Kernel_function.get_name kf1)
        (Pretty_utils.pp_list ~sep:",@ " ExpStructEq.pretty) exp1
    | Msg s -> Format.fprintf fmt "%s" s
    | Loop(stmt, s, g) ->
      Format.fprintf fmt "@[Loop(%a) %a@] %a"
        Stmt.pretty_sid stmt
        Node.pretty s
        (GraphShape.pretty (Edge.pretty_list)) g

  let hash = function
    | Assume (_, e, b) ->
      Hashtbl.seeded_hash (Hashtbl.hash b) (ExpStructEq.hash e)
    | Assign (_, lv, t, e) ->
      Hashtbl.seeded_hash (ExpStructEq.hash e)
        (Hashtbl.seeded_hash (Typ.hash t)
           (Hashtbl.seeded_hash 2 (Lval.hash lv)))
    | EnterScope (_, vs) ->
      List.fold_left
        (fun acc e -> Hashtbl.seeded_hash acc (Varinfo.hash e)) 3 vs
    | LeaveScope (_, vs) ->
      List.fold_left
        (fun acc e -> Hashtbl.seeded_hash acc (Varinfo.hash e)) 5 vs
    | CallDeclared (kf, es, lv) ->
      let x = Kernel_function.hash kf in
      let x = Hashtbl.seeded_hash x (Extlib.opt_hash Lval.hash lv) in
      List.fold_left
        (fun acc e -> Hashtbl.seeded_hash acc (ExpStructEq.hash e)) x es
    | Msg s -> Hashtbl.seeded_hash 7 s
    | Loop (stmt, s, g) ->
      Hashtbl.seeded_hash (Stmt.hash stmt)
        (Hashtbl.seeded_hash (GraphShape.hash g)
           (Hashtbl.seeded_hash 11 (Node.hash s)))
end

module EdgeList = struct
  include Datatype.List_with_collections(Edge)
      (struct let module_name = "Value.Traces_domain.EdgeList" end)
  let pretty = Edge.pretty_list
  let pretty_debug = pretty
end

module Graph = struct
  include Hptmap.Make(Node)(EdgeList)(Hptmap.Comp_unused)
      (struct let v = [[]] end)
      (struct let l = [Ast.self] end)

  let is_included =
    let cache = Hptmap_sig.NoCache in
    let decide_fast = decide_fast_inclusion in
    let decide_fst _n _v1 = false in
    let decide_snd _n _v2 = true in
    let rec decide_both k l1 l2 =
      match l1, l2 with
      | [], _ -> true
      | _, [] -> false
      | h1 :: t1, h2 :: t2 ->
        let c = Edge.compare h1 h2 in
        if c = 0 then decide_both k t1 t2
        else if c < 0 then false
        else decide_both k l1 t2
    in
    binary_predicate cache UniversalPredicate
      ~decide_fast ~decide_fst ~decide_snd ~decide_both

  let join =
    let cache = Hptmap_sig.NoCache in
    let rec merge_edge k l1 l2 =
      match l1, l2 with
      | [], l2 -> l2
      | l1, [] -> l1
      | h1 :: t1, h2 :: t2 ->
        let c = Edge.compare h1 h2 in
        if c = 0 then h1 :: merge_edge k t1 t2
        else if c < 0 then h1 :: merge_edge k t1 l2
        else h2 :: merge_edge k l1 t2
    in
    join ~cache ~symmetric:true ~idempotent:true ~decide:merge_edge

  let diff =
    let cache = Hptmap_sig.NoCache in
    let decide_left = Neutral in
    let decide_right = Absorbing in
    let rec diff_list k l1 l2 =
      match l1, l2 with
      | [], _ -> []
      | l1, [] -> l1
      | h1 :: t1, h2 :: t2 ->
        let c = Edge.compare h1 h2 in
        if c = 0 then diff_list k t1 t2
        else if c < 0 then h1 :: diff_list k t1 l2
        else diff_list k l1 t2
    in
    let decide_both k l1 l2 = match diff_list k l1 l2 with [] -> None | l -> Some l in
    merge ~cache ~symmetric:false ~idempotent:false
      ~decide_both ~decide_left ~decide_right

  let succs (n : Node.t) g = try find n g with Not_found -> []

  let rec epsilon_path current stop g =
    Node.equal current stop ||
    begin
      Node.compare current stop <= 0 &&
      match find current g with
      | exception Not_found -> false
      | l ->
        let exists = function
          | { edge_dst; edge_trans = Msg _ } -> epsilon_path edge_dst stop g
          | _ -> false
        in
        List.exists exists l
    end
end

let create_edge all_edges_ever_created current e =
  let m = Graph.singleton current [e] in
  let old = !all_edges_ever_created in
  let new_ = Graph.join old m in
  (* if not (Graph.equal old new_) then *)
  (*   Format.printf "@[<hv>@[create_edge: %a ->@]@ %a@]@." *)
  (*     Node.pretty current Edge.pretty e; *)
  all_edges_ever_created := new_;
  m

let join_path ~all_edges_ever_created g c1 c2 =
  if Graph.epsilon_path c1 c2 g then (c2, g)
  else if Graph.epsilon_path c2 c1 g then (c1, g)
  else
    let t_join = Msg "join" in
    let is_join e = Transition.equal e.edge_trans t_join in
    let e =
      let succs1 = Graph.succs c1 !all_edges_ever_created in
      let succs2 = Graph.succs c2 !all_edges_ever_created in
      let succs1 = List.filter is_join succs1 in
      let find s1 = List.exists (Edge.equal s1) succs2 in
      begin match List.find find succs1 with
        | exception Not_found ->
          { edge_dst = Node.next () ; edge_trans = t_join }
        | m ->
          { edge_dst = Edge.succ m ; edge_trans = t_join }
      end
    in
    let m1 = create_edge all_edges_ever_created c1 e in
    let m2 = create_edge all_edges_ever_created c2 e in
    let g = Graph.join (Graph.join m1 g) m2 in
    (e.edge_dst, g)

(* A loop .*)
type loops =
  | Base of Node.t * Graph.t (* current last *)
  | OpenLoop of Cil_types.stmt * Node.t (* start node *) * Graph.t (* last iteration *) * Node.t (** current *) * Graph.t * loops
  | UnrollLoop of Cil_types.stmt * loops

module Loops = struct
  type t = loops

  let rec is_included l1 l2 = match l1, l2 with
    | Base _, (OpenLoop _ | UnrollLoop _)
    | (OpenLoop _ | UnrollLoop _), Base _ ->
      (* not in the same number of loops *)
      false
    | Base (c1,_), Base (c2,g2) ->
      Graph.epsilon_path c1 c2 g2
    | (OpenLoop(stmt1,_,_,_,_,_) | UnrollLoop(stmt1,_)),
      (OpenLoop(stmt2,_,_,_,_,_) | UnrollLoop(stmt2,_)) when not (Stmt.equal stmt1 stmt2) ->
      (* not same loop *)
      false
    | OpenLoop(_,s1,_,_,_,_), OpenLoop(_,s2,_,_,_,_) when not (Node.equal s1 s2) ->
      (* not entered in the loop at the same time, take arbitrarily one of them *)
      false
    | OpenLoop(_,_,last1,c1,g1,l1), OpenLoop(_,_,last2,c2,g2,l2) ->
      let g2' = Graph.join last2 g2 in
      is_included l1 l2 &&
      Graph.is_included last1 last2 &&
      Graph.is_included g1 g2' &&
      Graph.epsilon_path c1 c2 g2'
    | UnrollLoop(_,l1), UnrollLoop(_,l2) ->
      is_included l1 l2
    | OpenLoop(_,_,_,_,_,_), UnrollLoop(_,_) ->
      false
    | UnrollLoop(_,l1), OpenLoop(_,_,_,_,_,l2) ->
      is_included l1 l2

  let rec diff l1 l2 = match l1, l2 with
    | Base _, (OpenLoop _ | UnrollLoop _) | (OpenLoop _ | UnrollLoop _), Base _ ->
      (* not in the same number of loops *)
      `Bottom
    | Base (c1,g1), Base (_,g2) ->
      let g = Graph.diff g1 g2 in
      `Value (Base (c1, g))
    | (OpenLoop(stmt1,_,_,_,_,_) | UnrollLoop(stmt1,_)),
      (OpenLoop(stmt2,_,_,_,_,_) | UnrollLoop(stmt2,_)) when not (Stmt.equal stmt1 stmt2) ->
      (* not same loop *)
      `Bottom
    | OpenLoop(stmt1,s1,last1,c1,g1,l1), OpenLoop(_,s2,_,_,_,l2) when not (Node.equal s1 s2) ->
      (* not entered in the loop at the same time, take arbitrarily one of them *)
      begin match diff l1 l2 with
        | `Bottom -> `Bottom
        | `Value(l) -> `Value(OpenLoop(stmt1,s1,last1,c1,g1,l))
      end
    | OpenLoop(stmt,s,last1,c1,g1,l1), OpenLoop(_,_,last2,_,g2,l2) ->
      begin match diff l1 l2 with
        | `Bottom -> `Bottom
        | `Value(l) ->
          let last = Graph.diff last1 last2 in
          let g = Graph.diff g1 g2 in
          `Value(OpenLoop(stmt,s,last,c1,g,l))
      end
    | UnrollLoop(stmt,l1), UnrollLoop(_,l2) ->
      begin match diff l1 l2 with
        | `Bottom -> `Bottom
        | `Value l -> `Value (UnrollLoop(stmt,l))
      end
    | (OpenLoop(stmt,s,last,c,g,l1), UnrollLoop(_,l2)) ->
      begin match diff l1 l2 with
        | `Bottom -> `Bottom
        | `Value l -> `Value (OpenLoop(stmt,s,last,c,g,l))
      end
    | (UnrollLoop(stmt,l2), OpenLoop(_,_,_,_,_,l1)) ->
      begin match diff l1 l2 with
        | `Bottom -> `Bottom
        | `Value l -> `Value (UnrollLoop(stmt,l))
      end

  let rec compare l1 l2 = match l1, l2 with
    | Base (c1,g1), Base (c2,g2) ->
      let c = Node.compare c1 c2 in
      if c <> 0 then c else
        Graph.compare g1 g2
    | OpenLoop(stmt1,s1, last1, c1, g1, l1), OpenLoop(stmt2,s2, last2, c2, g2, l2) ->
      let c = Stmt.compare stmt1 stmt2 in
      if c <> 0 then c else
        let c = Node.compare s1 s2 in
        if c <> 0 then c else
          let c = Graph.compare last1 last2 in
          if c <> 0 then c else
            let c = Node.compare c1 c2 in
            if c <> 0 then c else
              let c = Graph.compare g1 g2 in
              if c <> 0 then c else
                compare l1 l2
    | UnrollLoop(stmt1,l1), UnrollLoop(stmt2,l2) ->
      let c = Stmt.compare stmt1 stmt2 in
      if c <> 0 then c else
        compare l1 l2
    | Base _, _ -> -1
    | _, Base _ -> 1
    | OpenLoop _, _ -> -1
    | _, OpenLoop _ -> 1

  let rec pretty fmt = function
    | Base (c,g) ->
      Format.fprintf fmt "@[<hv>%a @[at %a@]@]"
        Graph.pretty g Node.pretty c
    | OpenLoop(stmt,s,last,c,g,l) ->
      Format.fprintf fmt "@[<hv 1>@[loop(%a) %a@]@ @[<hv 1>@[last:@]@ %a@]@  @[<hv 1>@[c:@]@ %a@]@ @[at %a@]@]@ %a"
        Stmt.pretty_sid stmt
        Node.pretty s Graph.pretty last Graph.pretty g Node.pretty c pretty l
    | UnrollLoop(stmt,l) ->
      Format.fprintf fmt "@[<hv>@[unroll(%a)@]@ %a"
        Stmt.pretty_sid stmt
        pretty l

  let rec hash = function
    | Base (c,g) ->
      Hashtbl.seeded_hash (Hashtbl.seeded_hash 1 (Graph.hash g)) (Node.hash c)
    | OpenLoop(stmt,s,last,c,g,l) ->
      Hashtbl.seeded_hash 2
        (Stmt.hash stmt, Node.hash s, Graph.hash last, Node.hash c,
         Graph.hash g, hash l)
    | UnrollLoop(stmt,l) ->
      Hashtbl.seeded_hash 2
        (Stmt.hash stmt, hash l)
end

let rec join_loops ~all_edges_ever_created l1 l2 =
  match l1, l2 with
  | Base _, (OpenLoop _ | UnrollLoop _) | (OpenLoop _ | UnrollLoop _), Base _ ->
    (* not in the same number of loops *)
    `Top
  | Base (c1,g1), Base (c2,g2) ->
    let g = Graph.join g1 g2 in
    let (n,g) = join_path ~all_edges_ever_created g c1 c2 in
    `Value( Base (n, g))
  | (OpenLoop(stmt1,_,_,_,_,_) | UnrollLoop(stmt1,_)),
    (OpenLoop(stmt2,_,_,_,_,_) | UnrollLoop(stmt2,_)) when not (Stmt.equal stmt1 stmt2) ->
    (* not same loop *)
    `Top
  | OpenLoop(stmt1,s1,last1,c1,g1,l1), OpenLoop(_,s2,_,_,_,l2) when not (Node.equal s1 s2) ->
    (* not entered in the loop at the same time, take arbitrarily one of them *)
    begin match join_loops ~all_edges_ever_created l1 l2 with
      | `Top -> `Top
      | `Value(l) -> `Value(OpenLoop(stmt1,s1,last1,c1,g1,l))
    end
  | OpenLoop(stmt,s,last1,c1,g1,l1), OpenLoop(_,_,last2,c2,g2,l2) ->
    begin match join_loops ~all_edges_ever_created l1 l2 with
      | `Top -> `Top
      | `Value(l) ->
        let last = Graph.join last1 last2 in
        let g = Graph.join g1 g2 in
        let (n,g) = join_path ~all_edges_ever_created g c1 c2 in
        `Value(OpenLoop(stmt,s,last,n,g,l))
    end
  | UnrollLoop(stmt,l1), UnrollLoop(_,l2) ->
    begin match join_loops ~all_edges_ever_created l1 l2 with
      | `Top -> `Top
      | `Value l -> `Value (UnrollLoop(stmt,l))
    end
  | (OpenLoop(stmt,s,last,c,g,l1), UnrollLoop(_,l2))
  | (UnrollLoop(_,l2), OpenLoop(stmt,s,last,c,g,l1)) ->
    begin match join_loops ~all_edges_ever_created l1 l2 with
      | `Top -> `Top
      | `Value l -> `Value (OpenLoop(stmt,s,last,c,g,l))
    end


type state = { start : Node.t; current : loops;
               call_declared_function: bool;
               globals : Cil_types.varinfo list;
               main_formals : Cil_types.varinfo list;
               (** kind of memoization of the edges *)
               all_edges_ever_created : Graph.t ref;
               all_loop_start : (Node.t * Graph.t) Stmt.Hashtbl.t;
             }

let start s = s.start
let current s = s.current
let globals s = s.globals
let entry_formals s = s.main_formals

(* Lattice structure for the abstract state above *)
module Traces = struct

  (** impossible for normal values start must be bigger than current *)
  let new_empty () = { start = Node.start; current = Base (Node.start, Graph.empty);
                       call_declared_function = false;
                       globals = []; main_formals = [];
                       all_edges_ever_created = ref Graph.empty;
                       all_loop_start = Stmt.Hashtbl.create 10;
                     }
  let empty = new_empty ()
  let top = { (new_empty ()) with current = Base (Node.dumb, Graph.empty); }

  (* Frama-C "datatype" for type [inout] *)
  include Datatype.Make_with_collections(struct
      include Datatype.Serializable_undefined

      type t = state
      let name = "Value.Traces_domain.Traces.state"

      let reprs = [empty]

      let structural_descr = Structural_descr.t_record
          [| Descr.pack Datatype.Int.descr;
             Descr.pack Datatype.Int.descr;
             Descr.pack Graph.descr;
             Descr.pack Datatype.Bool.descr;
             Structural_descr.pack Structural_descr.t_abstract;
             Structural_descr.pack Structural_descr.t_abstract;
          |]

      let compare m1 m2 =
        let c = Node.compare m1.start m2.start in
        if c <> 0 then c else
          let c = Loops.compare m1.current m2.current in
          if c <> 0 then c else
            let c = Datatype.Bool.compare m1.call_declared_function m2.call_declared_function in
            if c <> 0 then c else
              0

      let equal = Datatype.from_compare

      let pretty fmt m =
        if m == top then Format.fprintf fmt "TOP"
        else
          Format.fprintf fmt "@[<hv>@[@[start: %a;@]@ @[globals = %a;@]@ @[main_formals = %a;@]@]@ %a@]"
            Node.pretty m.start
            (Pretty_utils.pp_list ~sep:",@ " Varinfo.pretty) m.globals
            (Pretty_utils.pp_list ~sep:",@ " Varinfo.pretty) m.main_formals
            Loops.pretty m.current

      let hash m =
        Hashtbl.seeded_hash (Node.hash m.start) (Loops.hash m.current)

      let copy c = c

    end)

  let view m =
    if m == top then `Top
    else `Other m

  let map_base f state =
    let rec aux = function
      | Base (c, g) ->
        let c, g = f (c, g) in Base (c, g)
      | OpenLoop (stmt, s, last, c, g, l) ->
        let c, g = f (c, g) in OpenLoop(stmt, s, last, c, g, l)
      | UnrollLoop (stmt, l) -> UnrollLoop (stmt, aux l)
    in { state with current = aux state.current }

  let move_to c g = map_base (fun _ -> c, g)
  let replace_to c = map_base (fun (_, g) -> c, g)

  let get_current state =
    let rec aux = function
      | Base (c,g) -> (c,g)
      | OpenLoop(_,_,_,c,g,_) -> (c,g)
      | UnrollLoop(_,l) ->
        aux l in
    aux state.current

  let add_trans_aux state t =
    let add_edge (current, graph) =
      let e =
        (** try to reuse an edge from the pool *)
        let succs = Graph.succs current !(state.all_edges_ever_created) in
        try List.find (Edge.has_transition t) succs
        with Not_found ->
          (** create a new edge *)
          { edge_trans = t; edge_dst = Node.next () }
      in
      let n = e.edge_dst in
      let m = create_edge state.all_edges_ever_created current e in
      let graph = Graph.join m graph in
      (n, graph)
    in
    map_base add_edge state

  let add_trans c t =
    if c == top then c
    else if c.call_declared_function then c (** forget intermediary state *)
    else
      let c = if c == empty then new_empty () else c in
      add_trans_aux c t

  let copy_edges s old_current_node g state =
    let cache = Node.Hashtbl.create 10 in
    let rec aux old_current_node state =
      let current_node = (fst (get_current state)) in
      let succs = Graph.succs old_current_node g in
      let fold state e =
        let next_old = Edge.succ e in
        let state = match Node.Hashtbl.find cache next_old with
          | exception Not_found ->
            let state = add_trans state e.edge_trans in
            Node.Hashtbl.add cache next_old (fst (get_current state));
            let state = aux next_old state in
            replace_to current_node state
          | next ->
            let (_,g) = get_current state in
            let e = Edge.change_next next e in
            let m = create_edge state.all_edges_ever_created current_node e in
            let g = Graph.join m g in
            move_to next g state
        in
        replace_to current_node state
      in
      List.fold_left fold state succs
    in
    let state = aux s state in
    let c = Node.Hashtbl.find cache old_current_node in
    replace_to c state

  let is_included c1 c2 =
    (* start is the same *)
    let r =
      c1.start = c2.start &&
      Loops.is_included c1.current c2.current in
    if not r && compare c1 c2 = 0 then
      Printf.printf "bad is_included@.";
    r

  let not_same_origin c1 c2 =
    c1.start != c2.start ||
    c1.globals != c2.globals ||
    c1.main_formals != c2.main_formals ||
    c1.all_edges_ever_created != c2.all_edges_ever_created

  let join c1 c2 =
    if c1.call_declared_function <> c2.call_declared_function
    then
      Value_parameters.fatal "@[<hv>@[At the same time inside and outside a function call:@]@ %a@ %a@]"
        pretty c1 pretty c2
    else
      match view c1, view c2 with
      | `Top, _ -> c1
      | _, `Top -> c2
      | `Other c1, `Other c2 when is_included c1 c2 -> c2
      | `Other c1, `Other c2 when is_included c2 c1 -> c1
      | `Other c1, `Other c2 ->
        if not_same_origin c1 c2 then assert false
        else
          let all_edges_ever_created = c1.all_edges_ever_created in
          match join_loops ~all_edges_ever_created c1.current c2.current with
          | `Top -> top
          | `Value(current) -> {c1 with current}

  let add_loop stmt state =
    let (n,g) = get_current state in
    let succs = Graph.succs n g in
    let rec find_same_loop = function
      | [] ->
        Stmt.Hashtbl.memo state.all_loop_start stmt (fun _ -> Node.next (),Graph.empty)
      | edge :: tl ->
        match edge.edge_trans with
        | Loop (stmt',s,last) when Stmt.equal stmt' stmt ->
          s, Graph.from_shape_id last
        | _ -> find_same_loop tl
    in
    let s,last = find_same_loop succs in
    let current = OpenLoop(stmt,s,last,s,Graph.empty,state.current) in
    { state with current }

  let widen _ stmt' c1 c2 =
    if false then
      begin
        if Loops.compare c1.current c2.current = 0
        then
          Format.printf "@[<hv 2>@[widen %a: same loops, states are%s equal @]@]@."
            Stmt.pretty_sid stmt' (if compare c1 c2 = 0 then "" else " not")
        else
          let c1' = Loops.diff c1.current c2.current in
          let c2' = Loops.diff c2.current c1.current in
          if (Bottom.compare Loops.compare) c1' c2' = 0 then
            Format.printf "@[<hv 2>@[widen %a diff equal:@]@ @[<hv 1>@[c1:@]@ %a@]@ @[<hv 1>@[c2:@]@ %a@]@]@."
              Stmt.pretty_sid stmt'
              Loops.pretty c1.current
              Loops.pretty c2.current

          else
            Format.printf "@[<hv 2>@[widen %a diff different:@]@ @[<hv 1>@[c1':@]@ %a@]@ @[<hv 1>@[c2':@]@ %a@]@]@."
              Stmt.pretty_sid stmt'
              (Bottom.pretty Loops.pretty) c1'
              (Bottom.pretty Loops.pretty) c2'
      end;
    if false then
      begin
        if Loops.compare c1.current c2.current = 0
        then
          Format.printf "@[<hv 2>@[widen %a: same loops, states are%s equal @]@]@."
            Stmt.pretty_sid stmt' (if compare c1 c2 = 0 then "" else " not")
        else
          Format.printf "@[<hv 2>@[widen %a@]@]@." Stmt.pretty_sid stmt'
      end;
    if not (Value_parameters.TracesUnrollLoop.get ())
    then c2
    else begin
      match c2.current with
      | Base _ -> assert false (** must be in a loop *)
      | OpenLoop(stmt,_,_,_,_,_) ->
        assert (Stmt.equal stmt' stmt);
        c2
      | UnrollLoop(stmt,l) ->
        assert (Stmt.equal stmt' stmt);
        add_loop stmt' {c2 with current = l}
    end

  let narrow _c1 c2 = `Value c2
end


module GraphDot = OCamlGraph.Graphviz.Dot(struct
    module V = struct type t = {node : Node.t; loops : Node.t list} end
    module E = struct
      open V
      type t =
        | Usual of Node.t * Edge.t * Node.t list
        | Head of Node.t * Node.t list * Node.t * Node.t list
        | Back of Node.t * Node.t list * Node.t
      let src = function
        | Usual (src,_,loops) -> {node=src;loops}
        | Head (src,loops,_,_) -> {node=src;loops}
        | Back (_,loops,src) -> {node=src;loops}
      let dst = function
        | Usual (_,edge,loops) -> {node=Edge.succ edge;loops}
        | Head (_,_,s,loops) -> {node=s;loops}
        | Back (dst,loops,_) -> {node=dst;loops}
    end
    open V
    open E
    type t = Graph.t
    let iter_vertex f g =
      let rec iter_edge k (l: Node.t list) e = match e.edge_trans with
        | Loop(_,_,g) -> iter_vertex (k::l) g
        | _ -> ()
      and iter_vertex l g =
        GraphShape.iter (fun k e -> f {node=k;loops=l}; List.iter (iter_edge k l) e) g
      in
      iter_vertex [] (Graph.shape g)
    let iter_edges_e f g =
      let rec iter_edge k l e =
        f (Usual(k,e,l));
        match e.edge_trans with
        | Loop(_,s,g) ->
          let l' = (k::l) in
          f (Head(k,l,s,l'));
          iter_vertex (Some s) l' g
        | _ -> ()
      and iter_vertex back l g =
        GraphShape.iter (fun k e ->
            match e, back with
            | [], Some back -> f (Back(back,l,k))
            | e, _ -> List.iter (iter_edge k l) e) g
      in
      iter_vertex None [] (Graph.shape g)

    let graph_attributes _ = []
    let default_vertex_attributes :
      t -> OCamlGraph.Graphviz.DotAttributes.vertex list = fun _ -> []
    let subgraph_name loops =
      Format.asprintf "S%a"
        (fun fmt -> List.iter (fun s -> Format.fprintf fmt "L%a" Node.pretty s))
        loops
    let vertex_name v = Format.asprintf "n%a%s" Node.pretty v.node
        (subgraph_name v.loops)
    let vertex_attributes :
      V.t -> OCamlGraph.Graphviz.DotAttributes.vertex list =
      fun n -> [`Label (Format.asprintf "%a" Node.pretty n.node)]
    let get_subgraph v =
      match v.loops with
      | [] -> None
      | _::l -> Some
                  {OCamlGraph.Graphviz.DotAttributes.sg_name = subgraph_name v.loops;
                   sg_attributes = [];
                   sg_parent = if l = [] then None else Some (subgraph_name l); }
    let default_edge_attributes :
      t -> OCamlGraph.Graphviz.DotAttributes.edge list = fun _ -> []
    let edge_attributes : E.t -> OCamlGraph.Graphviz.DotAttributes.edge list =
      function
      | Usual(_,{edge_trans = Loop _},_) -> [`Label (Format.asprintf "leave_loop")]
      | Usual(_,e,_) -> [`Label (Format.asprintf "@[<h>%a@]" Transition.pretty e.edge_trans)]
      | Head _ -> []
      | Back(_,_,_) -> [`Constraint false]
  end)

(** adds n -> [] for leaves *)
let rec complete_graph (graph:Graph.t) =
  Graph.fold (fun k l graph ->
      let graph, l =
        Extlib.fold_map (fun graph e ->
            let m = Graph.singleton (Edge.succ e) [] in
            let e = match e.edge_trans with
              | Assign (_, _,_,_)
              | Assume (_, _,_)
              | EnterScope _
              | LeaveScope _
              | CallDeclared (_,_,_)
              | Msg _ -> e
              | Loop (stmt,s,g) ->
                let n = e.edge_dst in
                let g = Graph.shape (complete_graph (Graph.from_shape_id g)) in
                { edge_dst = n; edge_trans = Loop(stmt,s,g) }
            in
            Graph.join graph m, e)
          graph l
      in
      Graph.join graph (Graph.singleton k l)
    ) graph Graph.empty


module Internal = struct
  type nonrec state = state
  type value = Cvalue.V.t
  type location = Precise_locs.precise_location

  include (Traces: sig
             include Datatype.S_with_collections with type t = state
             include Abstract_domain.Lattice with type state := state
           end)

  let log_category = Value_parameters.register_category "d-traces"

  type origin = unit

  module Transfer (Valuation: Abstract_domain.Valuation
                   with type value = value
                    and type origin = origin
                    and type loc = Precise_locs.precise_location)
    : Abstract_domain.Transfer
      with type state = state
       and type value = Cvalue.V.t
       and type location = Precise_locs.precise_location
       and type valuation = Valuation.t
  = struct
    type value = Cvalue.V.t
    type state = t
    type location = Precise_locs.precise_location
    type valuation = Valuation.t

    let assign ki lv e _v _valuation state =
      let trans = Assign (ki, lv.Eval.lval, lv.Eval.ltyp, e) in
      `Value (Traces.add_trans state trans)

    let assume stmt e pos _valuation state =
      let trans = Assume (stmt, e, pos) in
      `Value (Traces.add_trans state trans)

    let start_call stmt call _valuation state =
      let kf = call.Eval.kf in
      if Kernel_function.is_definition kf then
        let msg = Format.asprintf "start_call: %s (%b)" (Kernel_function.get_name call.Eval.kf)
            (Kernel_function.is_definition call.Eval.kf) in
        let state = Traces.add_trans state (Msg msg) in
        let formals = List.map (fun arg -> arg.Eval.formal) call.Eval.arguments in
        let state = Traces.add_trans state (EnterScope (kf, formals)) in
        let state = List.fold_left (fun state arg ->
            Traces.add_trans state
              (Assign (Kstmt stmt, Cil.var arg.Eval.formal,
                       arg.Eval.formal.Cil_types.vtype,
                       arg.Eval.concrete))) state call.Eval.arguments in
        `Value state
      else
        (** enter the scope of the dumb result variable *)
        let var = call.Eval.return in
        let state = match var with
          | Some var -> Traces.add_trans state (EnterScope (kf, [var]))
          | None -> state in
        let exps = List.map (fun arg -> arg.Eval.concrete) call.Eval.arguments in
        let state = Traces.add_trans state
            (CallDeclared (call.Eval.kf, exps, Extlib.opt_map Cil.var var))
        in `Value {state with call_declared_function = true}

    let finalize_call _stmt call ~pre:_ ~post =
      if post.call_declared_function
      then `Value {post with call_declared_function = false}
      else
        let msg = Format.asprintf "finalize_call: %s" (Kernel_function.get_name call.Eval.kf) in
        let state = Traces.add_trans post (Msg msg) in
        `Value state

    let update _valuation state = `Value state

    let show_expr _valuation state fmt _expr = Traces.pretty fmt state
  end

  (* Memexec *)
  (* This domains infers no relation between variables. *)
  let relate _kf _bases _state = Base.SetLattice.bottom
  (* Do not filter the state: the memexec cache will be applied only on function
     calls for which the entry states are equal. This almost completely
     disable memexec, but is always sound. *)
  let filter _kf _kind _bases state = state
  (* As memexec cache is only applied on equal entry states, the previous
     output state is a correct output for the current input state. *)
  let reuse _kf _bases ~current_input:_ ~previous_output:state = state

  let empty () = Traces.empty
  let introduce_globals vars state =
    {state with globals = vars @ state.globals}
  let initialize_variable lv _ ~initialized:_ _ state =
    Traces.add_trans state (Msg(Format.asprintf "initialize variable: %a" Printer.pp_lval lv ))
  let initialize_variable_using_type init_kind varinfo state =
    let state =
      match init_kind with
      | Abstract_domain.Main_Formal -> {state with main_formals = varinfo::state.main_formals}
      | _ -> state
    in
    let msg = Format.asprintf "initialize@ variable@ using@ type@ %a@ %a"
        (fun fmt init_kind ->
           match init_kind with
           | Abstract_domain.Main_Formal -> Format.pp_print_string fmt "Main_Formal"
           | Abstract_domain.Library_Global -> Format.pp_print_string fmt "Library_Global"
           | Abstract_domain.Spec_Return kf -> Format.fprintf fmt "Spec_Return(%s)" (Kernel_function.get_name kf))
        init_kind
        Varinfo.pretty varinfo
    in
    Traces.add_trans state (Msg msg)

  (* TODO *)
  let logic_assign _assign _location ~pre:_ state =
    Traces.add_trans state (Msg "logic assign")

  (* Logic *)
  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ state _ _ = `Value state

  let storage () = true

  let top_query = `Value (Cvalue.V.top, ()), Alarmset.all

  let extract_expr _oracle _state _expr = top_query
  let extract_lval _oracle _state _lv _typ _locs = top_query

  let backward_location _state _lval _typ loc value =
    `Value (loc, value)

  let enter_loop stmt state =
    let state = Traces.add_trans state (Msg "enter_loop") in
    let state = if not (Value_parameters.TracesUnrollLoop.get ())
      then Traces.add_loop stmt state
      else { state with current = UnrollLoop(stmt,state.current) } in
    state

  let incr_loop_counter _ state =
    match state.current with
    | Base _ -> assert false
    | UnrollLoop(_,_) -> state
    | OpenLoop(stmt,s,last,_,g,l) ->
      let last = Graph.join last g in
      let last = if Value_parameters.TracesUnifyLoop.get () then
          let s',old_last = Stmt.Hashtbl.find state.all_loop_start stmt in
          let last = Graph.join last old_last in
          assert (Node.equal s s');
          Stmt.Hashtbl.add state.all_loop_start stmt (s,last);
          last
        else last
      in
      let current = OpenLoop(stmt,s,last,s,Graph.empty,l) in
      let state = { state with current } in
      (* Traces.add_trans state (Msg("incr_loop_counter")) *)
      state

  let leave_loop stmt' state =
    match state.current with
    | Base _ -> assert false (* absurd: we are in at least a loop *)
    | UnrollLoop(_,l) -> { state with current = l }
    | OpenLoop(stmt,s,last,old_current_node,g,current) ->
      assert (Stmt.equal stmt stmt');
      let state = { state with current } in
      let last = if Value_parameters.TracesUnifyLoop.get () then
          let s',old_last = Stmt.Hashtbl.find state.all_loop_start stmt in
          let last = Graph.join last old_last in
          assert (Node.equal s s');
          Stmt.Hashtbl.add state.all_loop_start stmt (s,last);
          last
        else last
      in
      let state = if Graph.is_empty last then state
        else Traces.add_trans state (Loop(stmt,s,Graph.shape last)) in
      let state = Traces.copy_edges s old_current_node g state in
      Traces.add_trans state (Msg "leave_loop")


  let enter_scope kf vars state = Traces.add_trans state (EnterScope (kf, vars))
  let leave_scope kf vars state = Traces.add_trans state (LeaveScope (kf, vars))

  let reduce_further _state _expr _value = [] (*Nothing intelligent to suggest*)

end

let dummy_loc = Location.unknown

let subst_in_full var_mapping =
  let visit = Visitor_behavior.copy (Project.current ()) in
  visit, object
    inherit Cil.genericCilVisitor (visit)
    method! vvrbl vi =
      match Varinfo.Map.find vi var_mapping with
      | exception Not_found -> Cil.DoChildren
      | v -> Cil.ChangeTo v
    method! vlogic_var_use lv =
      match lv.Cil_types.lv_origin with
      | None -> Cil.DoChildren
      | Some vi ->
        match Varinfo.Map.find vi var_mapping with
        | exception Not_found -> Cil.DoChildren
        | v -> Cil.ChangeTo (Cil.cvar_to_lvar v)
  end

let subst_in var_mapping = (snd (subst_in_full var_mapping))

let sanitize_name s =
  String.map
    (fun c ->
       if
         ('0' <= c && c <= '9') ||
         ('a' <= c && c <= 'z') ||
         ('A' <= c && c <= 'Z')
       then c else '_') s

let subst_in_exp var_map exp = Cil.visitCilExpr (subst_in var_map) exp
let subst_in_lval var_map exp = Cil.visitCilLval (subst_in var_map) exp
let subst_in_varinfo var_map v =
  match Varinfo.Map.find v var_map with
  | exception Not_found -> v
  | v -> v

let fresh_varinfo var_map v =
  let v' = Cil.copyVarinfo v (sanitize_name v.Cil_types.vname) in
  v'.Cil_types.vdefined <- false;
  Varinfo.Map.add v v' var_map

let valid_sid = true

let rec stmts_of_cfg cfg current var_map locals return_exp acc =
  match Graph.find current cfg with
  | exception Not_found ->
    begin match return_exp with
      | None -> List.rev acc
      | Some (var,exp) ->
        let exp = subst_in_exp var_map exp in
        let return_stmt = Cil.mkStmtOneInstr ~valid_sid (Cil_types.Set(Cil.var var,exp,dummy_loc)) in
        List.rev (return_stmt::acc)
    end
  | [] -> assert false
  | [a] -> begin
      let n = a.edge_dst in
      match a.edge_trans with

      | Assign (_, lval,_typ,exp) ->
        let exp = subst_in_exp var_map exp in
        let lval = subst_in_lval var_map lval in
        let stmt = Cil.mkStmtOneInstr ~valid_sid (Cil_types.Set(lval,exp,dummy_loc)) in
        stmts_of_cfg cfg n var_map locals return_exp (stmt::acc)

      | Assume (_, exp,b) ->
        let exp = subst_in_exp var_map exp in
        let predicate = (Logic_utils.expr_to_predicate ~cast:true exp).Cil_types.ip_content in
        let predicate = if b then predicate else Logic_const.pnot predicate in
        let code_annot = Logic_const.new_code_annotation(Cil_types.AAssert([],Assert,predicate)) in
        let stmt = Cil.mkStmtOneInstr ~valid_sid (Cil_types.Code_annot(code_annot,dummy_loc)) in
        stmts_of_cfg cfg n var_map locals return_exp (stmt::acc)

      | EnterScope (_, vs) ->
        (** all our variables are assigned, not defined *)
        let var_map = List.fold_left fresh_varinfo var_map vs in
        let vs = List.map (subst_in_varinfo var_map) vs in
        locals := vs @ !locals;
        let block = { Cil_types.battrs = [];
                      bscoping = true;
                      blocals = vs;
                      bstatics = [];
                      bstmts = stmts_of_cfg cfg n var_map locals return_exp [] } in
        let stmt = Cil.mkStmt ~valid_sid (Cil_types.Block(block)) in
        List.rev (stmt::acc)

      | LeaveScope _ -> stmts_of_cfg cfg n var_map locals return_exp acc

      | CallDeclared (kf,exps,lval) ->
        let exps = List.map (subst_in_exp var_map) exps in
        let lval = Extlib.opt_map (subst_in_lval var_map) lval in
        let call = Cil.evar ~loc:dummy_loc (subst_in_varinfo var_map (Kernel_function.get_vi kf)) in
        let stmt = Cil.mkStmtOneInstr ~valid_sid (Cil_types.Call(lval,call,exps,dummy_loc)) in
        stmts_of_cfg cfg n var_map locals return_exp (stmt::acc)

      | Msg _ -> stmts_of_cfg cfg n var_map locals return_exp acc
      | Loop (_,s,g) ->
        let g = Graph.from_shape (fun _ v -> v) g in
        let is_while =
          match Graph.succs s g, Graph.succs n cfg with
          | [{ edge_dst = n1'; edge_trans = Assume(_,exp1,b1) }],
            [{ edge_dst = n2'; edge_trans = Assume(_,exp2,b2) }]
            when ExpStructEq.equal exp1 exp2 && b1 != b2 ->
            Some (exp1, n1', b1, n2')
          | _ -> None in
        match is_while with
        | None -> Value_parameters.not_yet_implemented "Traces_domain: Loop without condition"
        | Some(exp,nloop,bloop,n2) ->
          let exp = subst_in_exp var_map exp in
          let exp = if bloop then exp else Cil.new_exp ~loc:dummy_loc (UnOp(LNot,exp,Cil.intType)) in
          let body = stmts_of_cfg g nloop var_map locals None [] in
          let acc = (List.rev (Cil.mkLoop ?sattr:None ~guard:exp ~body)) @ acc in
          stmts_of_cfg cfg n2 var_map locals return_exp acc
    end
  | l ->
    let is_if = match l with
      | [] | [_] -> assert false (* absurd *)
      | [{ edge_dst = n1'; edge_trans = Assume(_,exp1,b1) } ;
         { edge_dst = n2'; edge_trans = Assume(_,exp2,b2) }]
        when ExpStructEq.equal exp1 exp2 && b1 != b2 ->
        if b1 then Some (exp1, n1', n2') else Some (exp1,n2',n1')
      | _ -> None in
    let stmt =
      match is_if with
      | None -> Value_parameters.not_yet_implemented "Traces_domain: switch at node(%a)" Node.pretty current
      | Some(exp,n1,n2) ->
        let exp = subst_in_exp var_map exp in
        let block1 = Cil.mkBlock (stmts_of_cfg cfg n1 var_map locals return_exp []) in
        let block2 = Cil.mkBlock (stmts_of_cfg cfg n2 var_map locals return_exp []) in
        Cil.mkStmt ~valid_sid (Cil_types.If(exp,block1,block2,dummy_loc)) in
    List.rev (stmt::acc)

let project_of_cfg vreturn s =
  let main = Kernel_function.get_vi (fst (Globals.entry_point ())) in

  let visit project =
    let visitor =
      object (self)
        inherit Visitor.frama_c_copy project
        method! vglob_aux global =
          match global with
          | Cil_types.GFun(fundec,_) when Varinfo.equal fundec.svar main ->
            Cil.DoChildren
          | Cil_types.GFun _ -> Cil.ChangeTo([])
          | _ -> Cil.JustCopy
        method! vfunc fundec =
          if Varinfo.equal (Visitor_behavior.Get_orig.varinfo self#behavior fundec.Cil_types.svar) main then begin
            (** copy of the fundec structure has already been done *)
            fundec.slocals <- [];
            let var_map = Varinfo.Map.empty in
            let return_stmt, return_equal, blocals = match vreturn with
              | None -> Cil.mkStmt ~valid_sid (Cil_types.Return(None,dummy_loc)), None, []
              | Some exp ->
                let var = Cil.makeVarinfo false false "__traces_domain_return" (Cil.typeOf exp) in
                Cil.mkStmt ~valid_sid (Cil_types.Return(Some (Cil.evar var),dummy_loc)),
                Some (var,exp), [var]
            in
            let locals = ref [] in
            let graph = match s.current with | Base (_,g) -> g | _ ->
              Value_parameters.fatal "Traces.project_of_cfg used with open loops" in
            let stmts = stmts_of_cfg graph s.start var_map locals return_equal [] in
            let sbody = Cil.mkBlock (stmts@[return_stmt])  in
            sbody.Cil_types.blocals <- blocals;
            fundec.sbody <- sbody;
            fundec.slocals <- blocals @ !locals @ fundec.slocals;
            Cil.setMaxId fundec;
            let fundec = {fundec with sbody} in
            Cil.ChangeDoChildrenPost(fundec,(fun x -> x))
          end
          else
            Cil.JustCopy
      end
    in
    visitor
  in

  let _project = Frama_c_File.create_project_from_visitor "Eva.Traces_domain" visit in
  ()
(* let selection = *)
(*   State_selection.diff *)
(*     State_selection.full *)
(*     (State_selection.list_union *)
(*        (List.map State_selection.with_dependencies *)
(*           [Cil.Builtin_functions.self; *)
(*            Ast.self; *)
(*            Frama_c_File.files_pre_register_state])) *)
(* in *)
(* let project = Project.create_by_copy ~selection ~last:true "Eva.Traces_domain" in *)
(* let fundecls = *)
(*   let l = ref [] in *)
(*   Globals.Functions.iter (fun kf -> *)
(*       if not (Kernel_function.is_definition kf) then *)
(*         l := (kf.Cil_types.spec, Kernel_function.get_vi kf)::!l *)
(*     ); *)
(*   !l in *)
(* Project.on project (fun () -> *)

(*     let var_map = Varinfo.Map.empty in *)
(*     let var_map = List.fold_left fresh_varinfo var_map s.globals in *)
(*     let var_map = List.fold_left fresh_varinfo var_map s.main_formals in *)
(*     let fundecls, var_map = List.fold_left (fun (fundecls,var_map) (funspec,v) -> *)
(*         let fundecl = Cil_types.GFunDecl(funspec,v,dummy_loc) in *)
(*         let behavior,visitor = subst_in_full var_map in *)
(*         let fundecl = Cil.visitCilGlobal visitor fundecl in *)
(*         let v' = Cil.get_varinfo behavior v in *)
(*         (fundecl @ fundecls), Varinfo.Map.add v v' var_map *)
(*         (\* (fundecl :: fundecls, var_map) *\) *)
(*       ) ([],var_map) fundecls in *)
(*     let globals = [] in *)
(*     (\** main function *\) *)
(*     let var_map = fresh_varinfo var_map main in *)
(*     let main = subst_in_varinfo var_map main in *)
(*     let fundec = Cil.emptyFunctionFromVI main in *)
(*     fundec.Cil_types.sformals <- List.map (subst_in_varinfo var_map) s.main_formals; *)
(*     let stmts = Cil.mkBlock (stmts_of_cfg s.graph s.start var_map vreturn []) in *)
(*     fundec.Cil_types.sbody <- stmts; *)
(*     let globals = Cil_types.GFun(fundec,dummy_loc) :: globals in *)
(*     (\* declared functions *\) *)
(*     let globals = fundecls @ globals in *)
(*     (\* globals *\) *)
(*     let globals = (List.map (fun v -> Cil_types.GVarDecl(subst_in_varinfo var_map v,dummy_loc)) s.globals) @ globals in *)
(*     let file = { Cil_types.fileName = "Traces_domain"; *)
(*                  globals; *)
(*                  globinit = None; *)
(*                  globinitcalled = false; } in *)
(*     Globals.set_entry_point (main.Cil_types.vname) false; *)
(*     Format.printf "@[<2>@[file1:@] %a@]@." Printer.pp_file file; *)
(*     (\* let file = Cil.visitCilFileCopy (new Cil.genericCilVisitor (Cil.refresh_visit project)) file in *\) *)
(*     Format.printf "@[<2>@[file2:@] %a@]@." Printer.pp_file file; *)
(*     Ast.set_file file; *)
(*     Format.printf "@[<2>@[file3:@] %a@]@." Printer.pp_file file; *)
(*   ) () *)


let output_dot filename state =
  let out = open_out filename in
  Value_parameters.feedback ~dkey:Internal.log_category "@[Output dot produced to %s.@]" filename;
  (** *)
  GraphDot.output_graph out (complete_graph (snd (Traces.get_current state)));
  close_out out

module D = struct
  include Domain_builder.Complete (Internal)

  let post_analysis state =
    let return_stmt = Kernel_function.find_return (fst (Globals.entry_point ())) in
    let return_exp = match return_stmt.Cil_types.skind with
      | Cil_types.Return (oexp,_) -> oexp
      | _ -> assert false in
    let header fmt = Format.fprintf fmt "Trace domains:" in
    let body = Bottom.pretty Traces.pretty in
    Value_parameters.printf ~dkey:Internal.log_category ~header " @[%a@]" body state;
    if Value_parameters.TracesProject.get () ||
       not (Value_parameters.TracesDot.is_default ()) then
      match state with
      | `Bottom ->
        Value_parameters.failure "The trace is Bottom can't generate code"
      | `Value state when state ==Traces.top ->
        Value_parameters.failure "The trace is TOP can't generate code"
      | `Value state ->
        if not (Value_parameters.TracesDot.is_default ())
        then output_dot (Value_parameters.TracesDot.get ()) state;
        if Value_parameters.TracesProject.get ()
        then project_of_cfg return_exp state
end


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
