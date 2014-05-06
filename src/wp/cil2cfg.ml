(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

(** Build a CFG of a function keeping some information of the initial structure.
 **)

open Cil_types

let dkey = Wp_parameters.register_category "cil2cfg" (* debugging key *)

let debug fmt = Wp_parameters.debug ~dkey fmt
let debug2 fmt = Wp_parameters.debug ~dkey ~level:2 fmt

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Nodes} *)

(** Be careful that only Bstmt are real Block statements *)
type block_type =
  Bstmt of stmt | Bthen of stmt | Belse of stmt | Bloop of stmt | Bfct
  (* added to identify 2 blocks for tests, else there are mixed up because same
  * sid *)

type call_type =
  | Dynamic of exp
  | Static of kernel_function

let pp_call_type fmt = function
  | Dynamic _ -> Format.pp_print_string fmt "dynamic"
  | Static kf -> Kernel_function.pretty fmt kf

type node_type =
  | Vstart | Vend | Vexit
  | VfctIn | VfctOut (* TODO : not useful anymore -> Bfct *)
  | VblkIn of block_type * block
  | VblkOut of block_type * block
  | Vstmt of stmt
  | Vcall of stmt * lval option * call_type * exp list
  | Vtest of bool * stmt * exp (** bool=true for In and false for Out *)
  | Vswitch of stmt * exp
  | Vloop of bool option * stmt 
          (** boolean is is_natural. None means the node has not been detected
            * as a loop *)
  | Vloop2 of bool * int

type node_info = { kind : node_type ; mutable reachable : bool }

type node = node_info

let node_type n = n.kind

let bkind_stmt bk = match bk with
  | Bfct -> None
  | Bstmt s | Bthen s | Belse s | Bloop s -> Some s

let _bkind_sid bk = match bk with
  | Bfct -> 0
  | Bstmt s | Bthen s | Belse s | Bloop s -> s.sid

type node_id = int * int

(** gives a identifier to each CFG node in order to hash them *)
let node_type_id t : node_id = match t with
    | Vstart -> (0, 0)
    | VfctIn -> (0, 1)
    | VfctOut -> (0, 2)
    | Vexit -> (0, 3)
    | Vend -> (0, 4)
    | Vstmt s | Vtest (true, s, _) | Vswitch (s,_) | Vcall (s, _, _, _) -> 
        (1, s.sid)
    | Vloop (_, s) -> (2, s.sid)
    | Vloop2 (_, n) -> (3, n)
    | VblkIn (Bfct, _) -> (4, 0)
    | VblkIn (Bstmt s,_) -> (5, s.sid)
    | VblkIn (Bthen s,_) -> (6, s.sid)
    | VblkIn (Belse s,_) -> (7, s.sid)
    | VblkIn (Bloop s,_) -> (8, s.sid)
    | VblkOut (Bfct, _) -> (9, 0)
    | VblkOut (Bstmt s,_) -> (10, s.sid)
    | VblkOut (Bthen s,_) -> (11, s.sid)
    | VblkOut (Belse s,_) -> (12, s.sid)
    | VblkOut (Bloop s,_) -> (13, s.sid)
    | Vtest (false, s, _) -> (14, s.sid)

let node_id n = node_type_id (node_type n)

let pp_bkind fmt bk = match bk with
  | Bfct -> Format.fprintf fmt "fct"
  | Bstmt s -> Format.fprintf fmt "stmt:%d" s.sid
  | Bthen s  -> Format.fprintf fmt "then:%d" s.sid
  | Belse s -> Format.fprintf fmt "else:%d" s.sid
  | Bloop s -> Format.fprintf fmt "loop:%d" s.sid

let pp_node_type fmt n = match n with
  | Vstart -> Format.fprintf fmt "<start>"
  | VfctIn -> Format.fprintf fmt "<fctIn>"
  | VfctOut -> Format.fprintf fmt "<fctOut>"
  | Vend -> Format.fprintf fmt "<end>"
  | Vexit -> Format.fprintf fmt "<exit>"
  | VblkIn (bk,_) -> Format.fprintf fmt "<blkIn-%a>" pp_bkind bk
  | VblkOut (bk,_) -> Format.fprintf fmt "<blkOut-%a>" pp_bkind bk
  | Vcall (s, _, _, _) -> Format.fprintf fmt "<callIn-%d>" s.sid
  | Vstmt s -> Format.fprintf fmt "<stmt-%d>" s.sid
  | Vtest (b, s, _) ->
      Format.fprintf fmt "<test%s-%d>" (if b then "In" else "Out") s.sid
  | Vswitch (s,_) -> Format.fprintf fmt "<switch-%d>" s.sid
  | Vloop (_, s) -> Format.fprintf fmt "<loop-%d>" s.sid
  | Vloop2 (_, n) -> Format.fprintf fmt "<loop-n%d>" n

let same_node v v' =
    (node_id v) = (node_id v')

(** the CFG nodes *)
module VL = 
struct
  type t = node

  let hash v = let (a,b) = (node_id v) in b*17 + a

  let equal v v' = same_node v v'

  let compare v v' = Extlib.compare_basic (node_id v) (node_id v')

  let pretty fmt v = pp_node_type fmt (node_type v)
end

let pp_node fmt v = VL.pretty fmt v

let start_stmt_of_node v = match node_type v with
    | Vstart | Vtest (false, _, _) | VblkOut _
    | VfctIn | VfctOut | Vend | Vexit | Vloop2 _ -> None
    | VblkIn (bk, _) -> bkind_stmt bk
    | Vstmt s | Vtest (true, s, _) | Vloop (_, s) | Vswitch (s,_)
    | Vcall (s, _, _, _)
      -> Some s

let node_stmt_opt v = match node_type v with
    | Vstart | Vtest (false, _, _)
    | VfctIn | VfctOut | Vend | Vexit | Vloop2 _ -> None
    | VblkIn (bk, _) | VblkOut (bk, _) -> bkind_stmt bk
    | Vstmt s | Vtest (true, s, _) | Vloop (_, s) | Vswitch (s,_)
    | Vcall (s, _, _, _)
      -> Some s

let node_stmt_exn v =
  match node_stmt_opt v with None -> raise Not_found | Some s -> s

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Edge labels} *)

type edge_type =
  | Enone  (** normal edge *)
  | Ethen  (** then branch : edge source is a Vtest *)
  | Eelse  (** else branch : edge source is a Vtest *)
  | Eback  (** back edge to a loop : the edge destination is a Vloop *)
  | EbackThen  (** Eback + Ethen *)
  | EbackElse  (** Eback + Eelse *)
  | Ecase of (exp list) (** switch branch : edge source is a Vswitch.
                              Ecase [] for default case *)
  | Enext (** not really a edge : gives the next node of a complex stmt *)

(** the CFG edges *)
module EL = struct

  let compare_edge_type e1 e2 =
    if e1 == e2 then 0
    else match e1, e2 with
      | Enone, Enone | Ethen, Ethen | Eelse, Eelse | Eback, Eback
      | EbackThen, EbackThen | EbackElse, EbackElse | Enext, Enext -> 0

      | Ecase l1, Ecase l2 -> Extlib.list_compare Cil_datatype.Exp.compare l1 l2

      | Enone, (Ethen | Eelse | Eback | EbackThen | EbackElse | Ecase _ | Enext)
      | Ethen, (Eelse | Eback | EbackThen | EbackElse | Ecase _ | Enext)
      | Eelse, (Eback | EbackThen | EbackElse | Ecase _ | Enext)
      | Eback, (EbackThen | EbackElse | Ecase _ | Enext)
      | EbackThen, (EbackElse | Ecase _ | Enext)
      | EbackElse, (Ecase _ | Enext)
      | Ecase _, Enext
          -> -1

      | Enext, (Ecase _ | EbackElse | EbackThen | Eback | Eelse | Ethen | Enone)
      | Ecase _, (EbackElse | EbackThen | Eback | Eelse | Ethen | Enone)
      | EbackElse, (EbackThen | Eback | Eelse | Ethen | Enone)
      | EbackThen, (Eback | Eelse | Ethen | Enone)
      | Eback, (Eelse | Ethen | Enone)
      | Eelse, (Ethen | Enone)
      | Ethen, Enone -> 1

  type t = edge_type ref

  let compare (e1 : t) (e2 : t) = compare_edge_type !e1 !e2
  let default = ref Enone
  let pretty fmt e =
    let txt = match e with
      | Enone -> "----" | Ethen -> "then" | Eelse -> "else"
      | Eback -> "back" | EbackThen -> "then-back" | EbackElse -> "else-back"
      | Ecase [] -> "default"
      | Ecase l -> Pretty_utils.sfprintf "case(%a)"
                     (Pretty_utils.pp_list ~sep:", " Printer.pp_exp) l
      | Enext -> "(next)"
    in Format.fprintf fmt "%s" txt
end

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Graph} *)

module PMAP(X: Graph.Sig.COMPARABLE) = struct

  module M = FCMap.Make(X)
  type 'a t = 'a M.t ref
  type key = X.t
  type 'a return = unit

  let empty = ()
    (* never called and not visible for the user thanks to signature
       constraints *)

  let create ?size () = ignore size ; ref M.empty

  let create_from h = ignore h ; ref M.empty

  let is_empty h = M.is_empty !h

  let clear h = h := M.empty

  let add k v h = h := M.add k v !h ; h
  let remove k h = h := M.remove k !h ; h
  let find k h = M.find k !h
  let mem k h = M.mem k !h

  let find_and_raise k t s = try find k t with Not_found -> invalid_arg s

  let fold f h init = M.fold f !h init

  let map f h = 
    ref (M.fold (fun k v m -> let (k,v) = f k v in M.add k v m) !h M.empty)

  let iter f h = M.iter f !h

  let copy h = ref !h

end

(** the CFG is an ocamlgraph, but be careful to use it through the cfg function
 * because some edges don't have the same meaning as some others... *)
module MyGraph = Graph.Blocks.Make(PMAP)
module CFG: 
  Graph.Sig.I 
  with type V.t = VL.t
  and  type V.label = VL.t
  and  type E.t = VL.t * EL.t * VL.t
  and  type E.label = EL.t
  = 
  struct
    include MyGraph.Digraph.ConcreteBidirectionalLabeled(VL)(EL)
    let add_vertex g v = ignore (add_vertex g v)
    let add_edge g v1 v2 = ignore (add_edge g v1 v2)
    let remove_edge g v1 v2 = ignore (remove_edge g v1 v2)
    let remove_edge_e g e = ignore (remove_edge_e g e)
    let add_edge_e g e = ignore (add_edge_e g e)
    let remove_vertex g v =
      if HM.mem v g then begin
        ignore (HM.remove v g);
        let remove v = S.filter (fun (v2,_) -> not (V.equal v v2)) in
        HM.iter (fun k (s1, s2) ->
                   ignore (HM.add k (remove v s1, remove v s2) g)) g
      end
  end

(** Set of edges. *)
module Eset = FCSet.Make (CFG.E)

(** Set of nodes. *)
module Nset = FCSet.Make (CFG.V)

(** Hashtbl of node *)
module Ntbl = Hashtbl.Make (CFG.V)

(** The final CFG is composed of the graph, but also :
  * the function that it represents,
  * an hashtable to find a CFG node knowing its hashcode *)
type t = {
  kernel_function : kernel_function;
  graph : CFG.t;
  spec_only : bool;
  stmt_node : ((int*int), CFG.V.t) Hashtbl.t;
  unreachables : node_type list;
  loop_nodes : (node list) option;
  mutable loop_cpt : int;
}

let new_cfg_env spec_only kf = {
  kernel_function = kf;
  spec_only = spec_only ;
  graph = CFG.create ();
  stmt_node = Hashtbl.create 97;
  unreachables = [];
  loop_nodes = None;
  loop_cpt = 0;
}

let cfg_kf cfg = cfg.kernel_function
let cfg_graph cfg = cfg.graph
let cfg_spec_only cfg = cfg.spec_only

let unreachable_nodes cfg = cfg.unreachables

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 CFG edges} *)

type edge = CFG.E.t

let edge_type e = !(CFG.E.label e)
let edge_src e = CFG.E.src e
let edge_dst e = CFG.E.dst e

let pp_edge fmt e =
  Format.fprintf fmt "%a -%a-> %a"
    pp_node (CFG.E.src e) EL.pretty (edge_type e) pp_node (CFG.E.dst e)

let is_back_edge e = match (edge_type e) with
  | Eback | EbackThen | EbackElse -> true
  | Enone | Ethen | Eelse | Ecase _ | Enext -> false

let is_next_edge e = match (edge_type e) with
  | Enext -> true
  | Eback | EbackThen | EbackElse | Enone | Ethen | Eelse | Ecase _ -> false

let pred_e cfg n =
  try
  let edges = CFG.pred_e cfg.graph n in
    List.filter (fun e -> not (is_next_edge e)) edges
  with Invalid_argument _ ->
    (Wp_parameters.warning "[cfg.pred_e] pb with node %a" pp_node n; [])

let succ_e cfg n =
  try
    let edges = CFG.succ_e cfg.graph n in
      List.filter (fun e -> not (is_next_edge e)) edges
  with Invalid_argument _ ->
    (Wp_parameters.warning "[cfg.succ_e] pb with node %a" pp_node n; [])

type edge_key = int * int * int * int

let edge_key e : edge_key =
  let a,b = node_id (edge_src e) in
  let c,d = node_id (edge_dst e) in
  a,b,c,d

let same_edge e1 e2 = (edge_key e1 = edge_key e2)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Iterators} ignoring the [Enext] edges *)

let iter_nodes f cfg = CFG.iter_vertex f (cfg.graph)
let fold_nodes f cfg acc = CFG.fold_vertex f (cfg.graph) acc

let iter_edges f cfg =
  let f e = if is_next_edge e then () else f e in
  CFG.iter_edges_e f (cfg.graph)

let iter_succ f cfg n =
  let f e = if is_next_edge e then () else f (CFG.E.dst e)
  in try CFG.iter_succ_e f (cfg.graph) n
  with Invalid_argument _ -> 
    (Wp_parameters.warning "[cfg.iter_succ] pb with node %a" pp_node n)

let fold_succ f cfg n acc =
  let f e acc = if is_next_edge e then acc else f (CFG.E.dst e) acc
  in try CFG.fold_succ_e f (cfg.graph) n acc
  with Invalid_argument _ -> 
    (Wp_parameters.warning "[cfg.fold_succ] pb with node %a" pp_node n; acc)

let fold_pred f cfg n acc =
  let f e acc = if is_next_edge e then acc else f (CFG.E.src e) acc in
  try CFG.fold_pred_e f (cfg.graph) n acc
  with Invalid_argument s -> 
    (Wp_parameters.warning "[cfg.fold_pred] pb with node %a: %s" pp_node n s; acc)

let _iter_succ_e f cfg n =
  let f e = if is_next_edge e then () else f e
  in try CFG.iter_succ_e f (cfg.graph) n
  with Invalid_argument _ -> 
    (Wp_parameters.warning "[cfg.iter_succ_e] pb with node %a" pp_node n)

let iter_pred_e f cfg n =
  let f e = if is_next_edge e then () else f e
  in try CFG.iter_pred_e f (cfg.graph) n
  with Invalid_argument _ -> 
    (Wp_parameters.warning "[cfg.iter_pred_e] pb with node %a" pp_node n)

let fold_pred_e f cfg n acc =
  let f e acc = if is_next_edge e then acc else f e acc
  in try CFG.fold_pred_e f (cfg.graph) n acc
  with Invalid_argument _ -> 
    (Wp_parameters.warning "[cfg.fold_pred_e] pb with node %a" pp_node n; acc)

let fold_succ_e f cfg n acc =
  let f e acc = if is_next_edge e then acc else f e acc
  in try CFG.fold_succ_e f (cfg.graph) n acc
  with Invalid_argument _ -> 
    (Wp_parameters.warning "[cfg.fold_succ_e] pb with node %a" pp_node n; acc)


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Getting information} *)

let cfg_start cfg = Hashtbl.find cfg.stmt_node (node_type_id Vstart)

let start_edge cfg = match succ_e cfg (cfg_start cfg) with [e] -> e
  | _ -> Wp_parameters.fatal "[cfg] should have exactly ONE starting edge !"

exception Found of node
let _find_stmt_node cfg stmt =
  let find n = match node_stmt_opt n with None -> ()
    | Some s -> if s.sid = stmt.sid then raise (Found n)
  in
    try (iter_nodes find cfg; raise Not_found)
    with Found n -> n

(** Get the edges going out a test node with the then branch first *)
let get_test_edges cfg v =
  match succ_e cfg v with
    | [e1; e2] ->
        begin match (edge_type e1), (edge_type e2) with
          | (Ethen|EbackThen), (Eelse|EbackElse) -> e1, e2
          | (Eelse|EbackElse), (Ethen|EbackThen) -> e2, e1
          | _, (Eelse|EbackElse) -> 
              Wp_parameters.fatal "[cfg] test node with invalid edges %a" 
                pp_edge e1
          | _, _ -> 
              Wp_parameters.fatal "[cfg] test node with invalid edges %a" 
                pp_edge e2
        end
    | _ -> raise (Invalid_argument "[cfg:get_test_edges] not a test")

let get_switch_edges cfg v =
  match node_type v with
  | Vswitch _ ->
      begin
      let get_case (cl, dl) e = match (edge_type e) with
        | Ecase [] -> cl, e::dl
        | Ecase c -> (c, e)::cl, dl
        | _ ->  Wp_parameters.fatal ("[cfg] switch node with invalid edges")
      in match List.fold_left get_case ([],[]) (succ_e cfg v) with
        | cl, [d] -> cl, d
        | _ ->
            Wp_parameters.fatal ("[cfg] switch node with several 'default' ?")
      end
  | _ -> raise (Invalid_argument "[cfg:get_switch_edges] not a switch")

let get_call_out_edges cfg v =
  let e1, e2 = match succ_e cfg v with
    | [e1;e2] -> e1, e2
    |  _ -> assert false
  in
  let en, ee = match node_type (edge_dst e1) ,
                     node_type (edge_dst e2) with
    | _,  Vexit -> e1, e2
    | Vexit, _  -> e2, e1
    | _, _ -> assert false
  in en, ee

let get_edge_labels e =
  let v_after = edge_dst e in
  let l = match node_type v_after with
    | Vstart -> assert false
    | VfctIn -> []
    | Vexit | VfctOut -> [Clabels.Post]
    | VblkIn (Bstmt s, _) -> [Clabels.mk_stmt_label s]
    | Vtest (false, _, _) | VblkIn _ | VblkOut _ | Vend -> []
    | Vcall (s,_,_,_) ->
        [Clabels.CallAt s.sid; Clabels.mk_stmt_label s]
    | Vstmt s | Vtest (true, s, _) | Vswitch (s,_) ->
        [Clabels.mk_stmt_label s]
    | Vloop2 _ -> []
    | Vloop (_,s) ->
        if is_back_edge e then []
        else [Clabels.mk_stmt_label s]
  in
  let v_before =  edge_src e in
  match node_type v_before with
    | VfctIn -> Clabels.Pre::l
    | Vloop (_, s) -> (Clabels.mk_loop_label s)::l
    | _ -> l

let next_edge cfg n =
  let edges = match node_type n with
    | VblkIn _ | Vswitch _ | Vtest _ | Vloop _ ->
        let edges = CFG.succ_e cfg.graph n in
          List.filter is_next_edge edges
    | Vcall _ ->
        let en, _ee = get_call_out_edges cfg n in [en]
    | Vstmt _ ->
        let edges = match CFG.succ_e cfg.graph n with
          | (([] | _::[]) as edges) -> edges
          | edges -> (* this case may happen in case of a loop
                        which is not really a loop : it is then a Vstmt,
                        and the Enext is not the succ_e. *)
              List.filter is_next_edge edges
        in edges
    | _ ->
        debug "[next_edge] not found for %a@." pp_node n;
        raise Not_found (* No Enext information on this node *)
  in
    match edges with
      | [] -> (* can append when nodes have been removed *) raise Not_found
      | [e] -> e
      | _ -> Wp_parameters.fatal "several (%d) Enext edges to node %a" 
               (List.length edges) pp_node n

(** Find the node that follows the input node statement.
* The statement postcondition can then be stored to the edges before that node.
* @raise Not_found when the node after has been removed (unreachable) *)
let node_after cfg n = edge_dst (next_edge cfg n)

let get_pre_edges cfg n = pred_e cfg n

let get_post_edges cfg v = 
  try let v' = node_after cfg v in pred_e cfg v'
  with Not_found -> []

let get_exit_edges cfg src =
  debug "[get_exit_edges] of %a@." pp_node src;
  let do_node n acc =
    debug "[get_exit_edges] look at %a@." pp_node n;
    let add_exit e acc =
      let dst = edge_dst e in
      match node_type dst with
      | Vexit ->
          debug
            "[get_exit_edges] add %a@." pp_edge e;
          (* (succ_e cfg dst) @ acc *)
          e :: acc
      | _ -> acc
    in match node_type n with
      | Vstart -> (* In it is a problem a domination which is not solved here *)
	  Wp_parameters.warning "[cfg] Forget exits clause of node %a" pp_node src;
	  raise Exit
      | _ -> fold_succ_e add_exit cfg n acc
  in
  let rec do_node_and_preds n (seen, edges as acc) =
    if Nset.mem n seen then acc (* Don't loop over the same node. *)
    else begin
      let edges = do_node n edges in
      if CFG.V.compare src n = 0 then (seen, edges)
      else do_preds n (Nset.add n seen, edges)
    end
  and do_preds n acc =
    fold_pred do_node_and_preds cfg n acc
  in
  let edges =
    try 
      let edge = next_edge cfg src in
	if false || is_next_edge edge then
	  (* needs to look at all node between the next node and the source *)
 	  snd (do_preds (edge_dst edge) (Nset.empty, []))
	else do_node src [] 
    with Exit -> []
  in
    if edges = [] then
      debug "[get_exit_edges] -> empty";
    edges

let add_edges_before cfg src set e_after =
  let rec add_preds set e =
    let e_src = edge_src e in
    if CFG.V.compare src e_src = 0 then set
    else
      let add_edge_and_preds e set =
        if Eset.mem e set then set
        else add_preds (Eset.add e set) e
      in fold_pred_e add_edge_and_preds cfg e_src set
  in add_preds set e_after

let get_internal_edges cfg n =
  let edges = try pred_e cfg (node_after cfg n) with Not_found -> [] in
  let set = Eset.empty in
  let set = List.fold_left (add_edges_before cfg n) set edges in
    edges, set

let rec get_edge_next_stmt cfg e =
  let v_after = edge_dst e in
  let get_next v = match succ_e cfg v with
    | [e] -> get_edge_next_stmt cfg e
    | [] | _ :: _ -> None (* nodes without statement should have one succ,
                             except the last one *)
  in
  match node_type v_after with
  | VblkOut _ | VblkIn ((Bthen _|Belse _|Bloop _|Bfct),_) -> get_next v_after
  | _ ->
    match node_stmt_opt v_after with
    | Some s -> Some s
    | None -> get_next v_after

let get_post_logic_label cfg v =
  match get_post_edges cfg v with [] -> None
  | e::_ -> (* TODO: is this ok to consider only one edge ? *)
      match get_edge_next_stmt cfg e with
        | None -> None
        | Some s ->  Some (Clabels.mk_logic_label s)

let blocks_closed_by_edge cfg e =
  debug "[blocks_closed_by_edge] for %a...@." pp_edge e;
  let v_before = edge_src e in
  let blocks = match node_type v_before with
    | Vstmt s | Vtest (true, s, _) | Vloop (_, s) | Vswitch (s,_) ->
	ignore (Ast.get ()); (* Since CIL Cfg computation is required and
				Ast.get () have to do this well. *) 
      begin match s.succs with
      | [s'] -> (try Kernel_function.blocks_closed_by_edge s s'
	with Not_found as e -> debug "[blocks_closed_by_edge] not found sid:%d -> sid:%d@."
            s.sid s'.sid;
	  raise e)
      | [] | _ :: _ ->
        let s' = get_edge_next_stmt cfg e in
        match s' with
        | None -> []
        | Some s' ->
          debug
            "[blocks_closed_by_edge] found sid:%d -> sid:%d@."
            s.sid s'.sid;
            try Kernel_function.blocks_closed_by_edge s s'
	    with Invalid_argument _ -> []
      end
    | _ -> (* TODO ? *) []
  in
  let v_after = edge_dst e in
  let blocks = match node_type v_after with
    | VblkOut (Bfct, b) -> b::blocks
    | _ -> blocks
  in blocks

let has_exit cfg =
  try
    let node = Hashtbl.find cfg.stmt_node (node_type_id Vexit) in
    match pred_e cfg node with
      | [] -> false
      | _ -> true
  with Not_found | Invalid_argument _ -> false

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Generic table to store things on edges} *)

module type HEsig = sig
  type ti
  type t
  val create : int -> t
  val find : t -> edge -> ti
  val find_all : t -> edge -> ti list
  val add : t -> edge -> ti -> unit
  val replace : t -> edge -> ti -> unit
  val remove : t -> edge -> unit
  val clear : t -> unit
end

module HE (I : sig type t end) = struct
  type ti = I.t
  type t = (edge_key, ti) Hashtbl.t
  let create n = Hashtbl.create n
  let find info e = Hashtbl.find info (edge_key e)
  let find_all info e = Hashtbl.find_all info (edge_key e)
  let add info e i = Hashtbl.add info (edge_key e) i
  let replace info e i = Hashtbl.replace info (edge_key e) i
  let remove info e = Hashtbl.remove info (edge_key e)
  let clear info = Hashtbl.clear info
end

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {2 Building the CFG} *)

let add_node env t =
  let id = node_type_id t in
  let n = {kind = t ; reachable = false } in
  debug "add node : %a@." VL.pretty n;
  let n = CFG.V.create n in
  Hashtbl.add env.stmt_node id n;
  n

let change_node_kind env n t =
  let id = node_id n in
  let id' = node_type_id t in
  let n' = { n with kind = t } in
  debug "change node kind from %a to %a" VL.pretty n VL.pretty n';
  let n' = CFG.V.create n' in
  Hashtbl.remove env.stmt_node id;
  Hashtbl.add env.stmt_node id' n';
  let preds = CFG.fold_pred_e (fun e acc -> e::acc) env.graph n [] in
  let succs = CFG.fold_succ_e (fun e acc -> e::acc) env.graph n [] in
  CFG.remove_vertex env.graph n;
  List.iter
    (fun e ->
       let e' = CFG.E.create (CFG.E.src e) (CFG.E.label e) n' in
       debug "replace edge %a %a %a"
         VL.pretty (CFG.E.src e) EL.pretty !(CFG.E.label e) VL.pretty n';
       CFG.add_edge_e env.graph e') preds;
  List.iter
    (fun e ->
       let e' = CFG.E.create n' (CFG.E.label e) (CFG.E.dst e) in
       debug "replace edge %a %a %a"
         VL.pretty n' EL.pretty !(CFG.E.label e) VL.pretty (CFG.E.dst e) ;
       CFG.add_edge_e env.graph e') succs;
  n'

let add_edge env n1 edge_type n2 =
  let e = CFG.E.create n1 (ref edge_type) n2 in
    debug "add edge : %a@." pp_edge e;
    CFG.add_edge_e env.graph e

let remove_edge env e =
  debug "remove edge : %a@." pp_edge e;
  CFG.remove_edge_e env.graph e

let insert_loop_node env loop_head loop_kind =
  let n_loop = add_node env loop_kind in
  let mv_pred_edge e =
    add_edge env (edge_src e) (edge_type e) n_loop;
    remove_edge env e
  in iter_pred_e mv_pred_edge env loop_head;
     add_edge env n_loop Enone loop_head;
     n_loop

let init_cfg spec_only kf =
  let env = new_cfg_env spec_only kf in
  let start =  add_node env (Vstart) in
  let fct_in =  add_node env (VfctIn) in
  let _ = add_edge env start Enone fct_in in
  let fct_out =  add_node env (VfctOut) in
  let nexit =  add_node env (Vexit) in
  let nend =  add_node env (Vend) in
  let _ = add_edge env fct_out Enone nend in
  let _ = add_edge env nexit Enone nend in
    env, fct_in, fct_out

let get_node env t =
  let id = node_type_id t in
  debug "get_node: %a --> id:%d,%d"
    pp_node_type t (fst id) (snd id);
  try Hashtbl.find env.stmt_node id
  with Not_found -> add_node env t

(** Setup the preconditions at all the call points of [e_kf], when possible *)
let setup_preconditions_proxies e_kf =
  match e_kf.enode with
    | Lval (Var vkf, NoOffset) ->
        let kf = Globals.Functions.get vkf in
        Statuses_by_call.setup_all_preconditions_proxies kf
    | _ -> () (* call through function pointer *)

let get_call_type fct =
  match Kernel_function.get_called fct with
    | None -> Dynamic fct
    | Some kf -> Static kf

(** In some cases (goto for instance) we have to create a node before having
* processed if through [cfg_stmt]. It is important that the created node
* is the same than while the 'normal' processing ! That is why
* this pattern matching might seem redondant with the other one. *)
let get_stmt_node env s = match s.skind with
  | Instr (Call (res, fct, args, _)) ->
      get_node env (Vcall (s, res, get_call_type fct, args))
  | Block b -> get_node env (VblkIn (Bstmt s,b))
  | UnspecifiedSequence seq ->
      let b = Cil.block_from_unspecified_sequence seq in
        get_node env (VblkIn (Bstmt s,b))
  | If (e, _, _, _) -> get_node env (Vtest (true, s, e))
  | Loop _ ->  get_node env (Vloop (None, s))
  | Break _ | Continue _ | Goto _
  | Instr _  | Return _ ->  get_node env (Vstmt s)
  | Switch (e, _, _, _) -> get_node env (Vswitch (s, e))
  | TryExcept _ | TryFinally _ ->
      Wp_parameters.not_yet_implemented "[cfg] exception handling"


(** build the nodes for the [stmts], connect the last one with [next],
* and return the node of the first stmt. *)
let rec cfg_stmts env stmts next = match stmts with
| [] -> next
| [s] -> cfg_stmt env s next
| s::tl ->
    let next = cfg_stmts env tl next in
    let ns = cfg_stmt env s next in
    ns

and cfg_block env bkind b next =
  (*
  match b.bstmts with
    | [] -> next
    | _ ->
        *)
    let in_blk = get_node env (VblkIn (bkind, b)) in
    let _ = add_edge env in_blk Enext next in
    let out_blk = get_node env (VblkOut (bkind, b)) in
    let _ = add_edge env out_blk Enone next in
    let first_in_blk = cfg_stmts env b.bstmts out_blk in
    let _ = add_edge env in_blk Enone first_in_blk in
    in_blk

and cfg_switch env switch_stmt switch_exp blk case_stmts next =
  let n_switch = get_node env (Vswitch (switch_stmt, switch_exp)) in
    add_edge env n_switch Enext next;
  let _first = cfg_stmts env blk.bstmts next in
  let branch with_def s =
    let n = get_stmt_node env s in
    let rec find_case l = match l with
    | [] -> false, []
    | Case (e, _)::tl ->
        let r = match find_case tl with
        | true, [] -> true, []
        | true, _ -> assert false
        | false, l -> false, e::l
        in r
    | Default _ :: _ ->
        (* we don't check if we have several Default because it is impossible:
         * CIL gives an error *)
        true, []
    | _::tl -> find_case tl
    in
    let def, case = find_case s.labels in
    if case = [] && not def then
      Wp_parameters.fatal "[cfg] switch branch without label";
    add_edge env n_switch (Ecase case) n;
    if def then true else with_def
  in
  let with_def = List.fold_left branch false case_stmts in
  let _ = if not with_def then add_edge env n_switch (Ecase []) next in
  n_switch

and cfg_stmt env s next =
  !Db.progress ();
  match s.skind with
  | Instr (Call (_, f, _, _)) ->
      setup_preconditions_proxies f;
      let in_call = get_stmt_node env s in
      add_edge env in_call Enone next;
      let exit_node = get_node env (Vexit) in
      add_edge env in_call Enone exit_node;
      in_call
  | Instr _  | Return _ ->
      let n = get_stmt_node env s in
      add_edge env n Enone next;
      n
  | Block b ->
      cfg_block env (Bstmt s) b next
  | UnspecifiedSequence seq ->
      let b = Cil.block_from_unspecified_sequence seq in
      cfg_block env (Bstmt s) b next
  | If (e, b1, b2, _) ->
      begin
        let n_in = get_stmt_node env s (*get_node env (Vtest (true, s, e))*) in
        let n_out = get_node env (Vtest (false, s, e)) in
          (* this node is to ensure that there is only one edge before
          * the [next] node of a if to put post properties about the IF. *)
          add_edge env n_out Enone next;
        let in_b1 = cfg_block env (Bthen s) b1 n_out in
        let in_b2 = cfg_block env (Belse s) b2 n_out in
          add_edge env n_in Ethen in_b1;
          add_edge env n_in Eelse in_b2;
          add_edge env n_in Enext next;
          n_in
      end
  | Loop(_, b, _, _, _) ->
      let loop = get_stmt_node env s in
        add_edge env loop Enext next;
      let in_b = cfg_block env (Bloop s) b loop in
      add_edge env loop Enone in_b;
      loop
  | Break _ | Continue _ | Goto _ ->
      let n = get_stmt_node env s in
      let _ = match s.succs with
      | [s'] -> add_edge env n Enone (get_stmt_node env s')
      | _ -> Wp_parameters.fatal "[cfg] jump with more than one successor ?"
      in n
  | Switch (e, b, lstmts, _) ->
      cfg_switch env s e b lstmts next
  | TryExcept _ | TryFinally _ ->
      Wp_parameters.not_yet_implemented "[cfg] exception handling"

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {3 Cleaning} remove node and edges that are unreachable *)

let clean_graph cfg =
  let graph = cfg_graph cfg in
  let rec reach n =
    if n.reachable then ()
    else (n.reachable <- true; iter_succ reach cfg n)
  in reach (cfg_start cfg);
  let clean n acc =
    if n.reachable then acc
    else begin
      debug "remove unreachable node %a@." VL.pretty n;
      let v = node_type n in
       CFG.remove_vertex graph n;
       Hashtbl.remove cfg.stmt_node (node_type_id v);
       v::acc
    end
  in
  let unreach = fold_nodes clean cfg [] in
    { cfg with unreachables = unreach }


(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {3 About loops}
* Let's first remind some definitions about loops :
* - {b back edge} : edge n->h such that h dominates n.
* - {b natural loop} : defined by a back edge n->h
*   * h is called the {b loop header},
*   * the body of the loop is the set of nodes n that are "between" h and n,
*     ie all n predecessors until h.
*   Because h dominates n, every backward path from n go through h.
*   Notice that each node in the loop body is dominated by h.
*
* A loop is not a natural loop if it has several entries (no loop header),
* or if it has some irreducible region (no back edge).
*
* Below, we use an algorithm from the paper :
 * "A New Algorithm for Identifying Loops in Decompilation"
 * of Tao Wei, Jian Mao, Wei Zou, and Yu Chen,
 * to gather information about the loops in the builted CFG.
 *)

module type WeiMaoZouChenInput = sig
  type graph
  type node
  type tenv

  (** build a new env from a graph,
  * and also return the entry point of the graph which has to be unique. *)
  val init : graph -> tenv * node

  (** apply the function on the node successors *)
  val fold_succ : (tenv -> node -> tenv) -> tenv -> node -> tenv

  val eq_nodes : node -> node -> bool

  (** store the position for the node and also the fact that the node has
  * been seen *)
  val set_pos : tenv -> node -> int -> tenv

  (** reset the position (set the position to 0), but should keep the
  * information that the node has been seen already. *)
  val reset_pos : tenv -> node -> tenv

 (** get the previously stored position of the node or 0 if nothing has been
 * stored *)
  val get_pos : tenv -> node -> int

 (** get the previously stored position of the node if any, or None
 * if [set_pos] hasn't been called already for this node. *)
  val get_pos_if_traversed : tenv -> node -> int option

  (** [set_iloop_header env b h] store h as the innermost loop header for b.
  * Beware that this function can be called several times for the same b
  * with different values of h during the computation. Only the last one
  * will give the correct information.
  * *)
  val set_iloop_header : tenv -> node ->  node -> tenv

  (** get the node innermost loop header if any *)
  val get_iloop_header : tenv -> node ->  node option

  (** store the node as a loop header. *)
  val add_loop_header : tenv -> node -> tenv

  (** store the node as an irreducible loop header. *)
  val add_irreducible : tenv -> node -> tenv

  (** store the edge between the two nodes (n1, n2) as a reentry edge.
  * n2 is the reentry point which means that it is in a loop,
  * but it is not the loop header, and n1 is not in the loop. *)
  val add_reentry_edge : tenv -> node -> node -> tenv

  (* val pretty_node : Format.formatter -> node -> unit *)
end

(** Implementation of
 * "A New Algorithm for Identifying Loops in Decompilation" *)
module WeiMaoZouChen (G : WeiMaoZouChenInput) : sig
  val identify_loops : G.graph -> G.tenv
end = struct

  let tag_lhead env b h =
    match h with
    | None -> env
    | Some h ->
        if G.eq_nodes h b then (* already done *) env
        else
        let rec do_cur env cur_b cur_h =
          match G.get_iloop_header env cur_b with
          | None -> G.set_iloop_header env cur_b cur_h
          | Some hb when G.eq_nodes hb cur_h -> (* nothing to do *) env
          | Some hb ->
              if (G.get_pos env hb) < (G.get_pos env cur_h) then
                let env = G.set_iloop_header env cur_b cur_h in
                  do_cur env cur_h hb
              else do_cur env hb cur_h
        in do_cur env b h

  (** @return innermost loop header of b0 (None if b0 is not in a loop) *)
  let rec trav_loops_DFS env b0 pos =
    let env = G.set_pos env b0 pos in
    let do_b env b =
      match G.get_pos_if_traversed env b with
        | None -> (* case A : b is not traversed already *)
            let env, nh = trav_loops_DFS env b (pos + 1) in
              tag_lhead env b0 nh
        | Some b_pos when (b_pos > 0) ->
            begin (* case B : b already in path -> it is a loop *)
              let env = G.add_loop_header env b in
              tag_lhead env b0 (Some b)
            end
        | Some 0 ->
            begin
              match G.get_iloop_header env b with
              | None -> (* case C : do nothing *) env
              | Some h when (G.get_pos env h > 0) ->
                  (* case D  : b not in path, but h is *)
                  tag_lhead env b0 (Some h)
              | Some h -> (* h not in path *)
                  begin (* case E : reentry *)
                    assert (G.get_pos env h = 0);
                    let env = G.add_irreducible env h in
                    let env = G.add_reentry_edge env b0 b in
                    let rec f env h = match G.get_iloop_header env h with
                      | Some h when (G.get_pos env h > 0) ->
                          tag_lhead env b0 (Some h)
                      | Some h ->
                          let env = G.add_irreducible env h in
                            f env h
                      | None -> env
                    in f env h
                  end
            end
        | _ -> assert false (* b_pos cannot be < 0 *)
    in
    let env = G.fold_succ do_b env b0 in
    let env = G.reset_pos env b0 in
    let h0 = G.get_iloop_header env b0 in
    env, h0

  let identify_loops g =
    let env, start = G.init g in
    let env, _ = trav_loops_DFS env start 1 in
      env

end

(** To use WeiMaoZouChen algorithm,
  * we need to define how to interact with our CFG graph *)
module LoopInfo = struct
  type node = CFG.V.t
  type graph = t
  type tenv = { graph : t ;
                dfsp : int Ntbl.t;
                iloop_header : node Ntbl.t;
                loop_headers : node list ;
                irreducible : node list ;
                unstruct_coef : int }

  let init cfg =
    let env = { graph = cfg ;
                dfsp = Ntbl.create 97; iloop_header =  Ntbl.create 7;
                loop_headers = []; irreducible = []; unstruct_coef = 0 } in
      env, cfg_start cfg

  let eq_nodes = CFG.V.equal

  let set_pos env n pos = Ntbl.add env.dfsp n pos; env
  let reset_pos env n = Ntbl.replace env.dfsp n 0; env
  let get_pos env n = try Ntbl.find env.dfsp n with Not_found -> 0
  let get_pos_if_traversed env n =
    try Some (Ntbl.find env.dfsp n) with Not_found -> None

  let set_iloop_header env b h = Ntbl.add env.iloop_header b h; env
  let get_iloop_header env b =
    try Some (Ntbl.find env.iloop_header b) with Not_found -> None

  let add_loop_header env h = { env with loop_headers = h :: env.loop_headers}
  let add_irreducible env h = { env with irreducible = h :: env.irreducible}
  let add_reentry_edge env _ _ = (* TODO *) env

  let is_irreducible env h = List.exists (eq_nodes h) env.irreducible

  let fold_succ f env n = fold_succ (fun v env -> f env v) env.graph n env

  let unstructuredness env =
    let k = float_of_int env.unstruct_coef in
    let k = k /. (float_of_int (CFG.nb_edges (cfg_graph env.graph))) in
    let k = 1. +. k in
      k

end

module Mloop = WeiMaoZouChen (LoopInfo)

module HEloop = HE (struct type t = Nset.t end)

let set_back_edge e =
  let info = CFG.E.label e in
    match !info with
      | Eback | EbackThen | EbackElse -> ()
      | Enone -> info := Eback
      | Ethen -> info := EbackThen
      | Eelse -> info := EbackElse
      | Ecase _ | Enext -> assert false

let mark_loops cfg =
  let env = Mloop.identify_loops cfg in
  let mark_loop_back_edge h = match node_stmt_opt h with
    | None -> (* Because we use !Db.Dominators that work on statements,
               we don't know how to detect back edge here.
               TODO: compute dominators on our cfg ? *) false
    | Some h_stmt ->
        let mark_back_edge e =
          let n = edge_src e in
          let is_back_edge =
            try
              let n_stmt = node_stmt_exn n in
                Dominators.dominates h_stmt n_stmt
            with Not_found -> false (* pred of h is not a stmt *)
          in
            if is_back_edge then set_back_edge e;
            debug "to loop edge %a@." pp_edge e
        in iter_pred_e mark_back_edge cfg h; true
  in
  let mark_loop loops h =
    debug "loop head in %a@." VL.pretty h;
    let is_natural =
      if (LoopInfo.is_irreducible env h) then
        (debug "irreducible loop detected in %a@." VL.pretty h; false)
      else true
    in let back_edges_ok = 
      if is_natural then mark_loop_back_edge h else true 
  in 
    let loop = match node_type h with
    | Vloop (_, h_stmt) ->
        assert (back_edges_ok);
        change_node_kind cfg h (Vloop (Some is_natural, h_stmt))
    | _ -> match node_stmt_opt h with
        | Some h_stmt when back_edges_ok ->
            insert_loop_node cfg h (Vloop (Some is_natural, h_stmt))
        | None when back_edges_ok ->
            let n = cfg.loop_cpt in cfg.loop_cpt <- n + 1;
            insert_loop_node cfg h (Vloop2 (is_natural, n))
        | _ -> (* consider it has non-natural. *)
            let n = cfg.loop_cpt in cfg.loop_cpt <- n + 1;
            insert_loop_node cfg h (Vloop2 (false, n))
    in loop::loops
  in
  let loops = List.fold_left mark_loop [] env.LoopInfo.loop_headers in
    debug2 "unstructuredness coef = %f@." (LoopInfo.unstructuredness env);
    { cfg with loop_nodes = Some loops }

let loop_nodes cfg = match cfg.loop_nodes with Some l -> l
  | None -> Wp_parameters.fatal
              "Cannot use the loop nodes before having computed them"

let strange_loops cfg = 
  let strange n = match node_type n with
    | Vloop (Some is_natural, _) when is_natural -> false
    | _ -> true
  in let loops = loop_nodes cfg in
  let strange_loops = List.filter strange loops in
    debug "%d/%d strange loops" 
      (List.length strange_loops) (List.length loops);
    strange_loops

let very_strange_loops cfg = 
  let strange n = match node_type n with
    | Vloop (Some _, _) | Vloop2 _ -> false
    | _ -> true
  in let loops = loop_nodes cfg in
  let strange_loops = List.filter strange loops in
    debug "%d/%d very strange loops" 
      (List.length strange_loops) (List.length loops);
    strange_loops

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)
(** {3 Create CFG} *)

let cfg_from_definition kf f =
  let kf_name = Kernel_function.get_name kf in
  let cfg, fct_in, fct_out = init_cfg false kf in
  let in_b = cfg_block cfg Bfct f.sbody fct_out in
  let _ = add_edge cfg fct_in Enone in_b in
  let graph = cfg_graph cfg in
    debug "for function '%s': %d vertex - %d edges@."
      kf_name (CFG.nb_edges graph) (CFG.nb_vertex graph);
    debug
      "start removing unreachable in %s@." kf_name;
  !Db.progress ();
  let cfg = clean_graph cfg in
    debug "for function '%s': %d vertex - %d edges@."
      kf_name (CFG.nb_edges graph) (CFG.nb_vertex graph);
  !Db.progress ();
    debug
      "start loop analysis for %s@." kf_name;
  let cfg = mark_loops cfg in
    cfg

let cfg_from_proto kf =
  let cfg, fct_in, fct_out = init_cfg true kf in
  let _ = add_edge cfg fct_in Enone fct_out in
  let cfg = { cfg with loop_nodes = Some [] } in
    cfg

(* ------------------------------------------------------------------------ *)
(** {2 Export dot graph} *)

(** {3 Printer for ocamlgraph} *)

module Printer (PE : sig val edge_txt : edge -> string end) = struct
  type t = CFG.t * (edge -> string)
  module V = CFG.V
  module E = CFG.E
  let iter_edges_e f (g, _f) = CFG.iter_edges_e f g
  let iter_vertex f (g, _) = CFG.iter_vertex f g

  let graph_attributes _t = []

  let pretty_raw_stmt s =
    let s = Pretty_utils.sfprintf "%a" Printer.pp_stmt s in
    let s' = if String.length s >= 50 then (String.sub s 0 49) ^ "..." else s in
    String.escaped s'

  let vertex_name v = 
    let a,b = node_id v in
    Printf.sprintf "%d.%d" a b

  let vertex_attributes v =
    let n = V.label v in
    let label = match node_type n with
      | Vstart -> "Start" | Vend -> "End" | Vexit -> "Exit"
      | VfctIn -> "FctIn" | VfctOut -> "FctOut"
      | VblkIn (bk,_) -> Pretty_utils.sfprintf "BLOCKin <%a>" pp_bkind bk
      | VblkOut (bk,_) -> Pretty_utils.sfprintf "BLOCKout <%a>" pp_bkind bk
      | Vcall _ -> Format.sprintf "CALL"
      | Vtest (true, s, e) ->
           Pretty_utils.sfprintf "IF <%d>\n%a" s.sid Printer.pp_exp e
      | Vtest (false, s, _e) -> Pretty_utils.sfprintf "IFout <%d>" s.sid
      | Vstmt s | Vloop (_, s) | Vswitch (s, _) ->
          begin match s.skind with
       | Instr _ -> Format.sprintf "INSTR <%d>\n%s" s.sid (pretty_raw_stmt s)
       | If _ -> "invalid IF ?"
       | Return _ -> Format.sprintf "RETURN <%d>" s.sid
       | Goto _ -> Format.sprintf "%s <%d>" (pretty_raw_stmt s) s.sid
       | Break _ -> Format.sprintf "BREAK <%d>" s.sid
       | Continue _ -> Format.sprintf "CONTINUE <%d>" s.sid
       | Switch _ ->  Format.sprintf "SWITCH <%d>" s.sid
       | Loop _ ->  Format.sprintf "WHILE(1) <%d>" s.sid
       | Block _ ->  Format.sprintf "BLOCK??? <%d>" s.sid
       | TryExcept _ ->  Format.sprintf "TRY EXCEPT <%d>" s.sid
       | TryFinally _ ->  Format.sprintf "TRY FINALLY <%d>" s.sid
       | UnspecifiedSequence _ ->  Format.sprintf "UnspecifiedSeq <%d>" s.sid
          end
      | Vloop2 (_, n) -> Format.sprintf "Loop-%d" n
    in
    let attr = match node_type n with
      | Vstart | Vend | Vexit -> [`Color 0x0000FF; `Shape `Doublecircle]
      | VfctIn | VfctOut -> [`Color 0x0000FF; `Shape `Box]
      | VblkIn _ | VblkOut _ -> [`Shape `Box]
      | Vloop _ | Vloop2 _ -> [`Color 0xFF0000; `Style [`Filled]]
      | Vtest _ | Vswitch _ ->
        [`Color 0x00FF00; `Style [`Filled]; `Shape `Diamond]
      | Vcall _ | Vstmt _ -> []
    in (`Label (String.escaped label))::attr

  let default_vertex_attributes _v = []

  let edge_attributes e =
    let attr = [] in
    let attr = (`Label (String.escaped (PE.edge_txt e)))::attr in
    let attr =
      if is_back_edge e then (`Constraint false)::(`Style [`Bold])::attr
      else attr
    in
    let attr = match (edge_type e) with
      | Ethen | EbackThen -> (`Color 0x00FF00)::attr
      | Eelse | EbackElse -> (`Color 0xFF0000)::attr
      | Ecase [] -> (`Color 0x0000FF)::(`Style [`Dashed])::attr
      | Ecase _ -> (`Color 0x0000FF)::attr
      | Enext -> (`Style [`Dotted])::attr
      | Eback -> attr (* see is_back_edge above *)
      | Enone -> attr
    in
      attr

  let default_edge_attributes _ = []

  let get_subgraph v =
     let mk_subgraph name attrib =
      let attrib = (`Style [`Filled]) :: attrib in
          Some { Graph.Graphviz.DotAttributes.sg_name= name;
                 sg_parent = None;
                 sg_attributes = attrib }
    in
       match node_type (V.label v) with
         | Vcall (s,_,_,_) ->
             let name = Format.sprintf "Call_%d" s.sid in
             let call_txt = pretty_raw_stmt s in
             let label = Format.sprintf "Call <%d> : %s" s.sid call_txt in
             let attrib = [(`Label label)] in
             let attrib = (`Fillcolor 0xB38B4D) :: attrib in
               mk_subgraph name attrib
         | _ -> None

end

(* ---------------------------------- *)
(** {3 Export to dot file} *)

type pp_edge_fun = Format.formatter -> edge -> unit

let export ~file ?pp_edge_fun cfg =
  Kernel.Unicode.without_unicode
    (fun () ->
      let edge_txt = match pp_edge_fun with
        | None ->
          (fun e -> match  (edge_type e) with
            | Ecase (_::_) -> Pretty_utils.sfprintf "%a" EL.pretty (edge_type e)
            | _ -> ""
          )
        | Some pp -> (fun e -> Pretty_utils.sfprintf "%a" pp e)
      in
      let module P = Printer (struct let edge_txt = edge_txt end) in
      let module GPrint = Graph.Graphviz.Dot(P) in
      (* [JS 2011/03/11] open_out and output_graph (and close_out?) may raise
      exception. Should be caught. *)
      let oc = open_out file in
      GPrint.output_graph oc (cfg_graph cfg, edge_txt);
      close_out oc
    ) ()

(* ------------------------------------------------------------------------ *)
(** {2 CFG management} *)

let create kf =
  let kf_name = Kernel_function.get_name kf in
  debug "create cfg for function '%s'@." kf_name;
  let cfg =
    try
      let f = Kernel_function.get_definition kf in
        cfg_from_definition kf f
    with Kernel_function.No_Definition ->
      cfg_from_proto kf
  in debug "done for %s@." kf_name;
     !Db.progress ();
     cfg

module KfCfg =
  Kernel_function.Make_Table
    (Datatype.Make
       (struct
          include Datatype.Undefined 
          type tt = t
          type t = tt
          let name = "WpCfg"
          let mem_project = Datatype.never_any_project
         let reprs =
           List.map
             (fun kf ->
                { kernel_function = kf;
                  spec_only = true;
                  graph = CFG.create ();
                  stmt_node = Hashtbl.create 0;
                  unreachables = [];
                  loop_nodes = None;
                  loop_cpt = 0;
                }
               )
             Kernel_function.reprs
         let equal t1 t2 =
           Kernel_function.equal t1.kernel_function t2.kernel_function
         let hash t = Kernel_function.hash t.kernel_function
         let compare t1 t2 =
           Kernel_function.compare t1.kernel_function t2.kernel_function
        end))
    (struct let name = "KfCfg"
            let dependencies = [Ast.self]
            let size = 17
     end)

let get kf = KfCfg.memo create kf

(* ------------------------------------------------------------------------ *)
