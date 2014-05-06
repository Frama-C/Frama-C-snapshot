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

open Cil_types
open Cil
open Cil_datatype

(* This is a reimplementation of ocamlgraph Path.Check. Instead of using
   an hashtbl containing couples of stmts, we use an association map
   to hptmap from stmts to bool. This enforces a lot of sharing, which
   is very useful when stmt_can_reach is called on a lot of pairs *)
module PathChecker =
struct

  module HV = Hashtbl.Make(Stmt)
  module HptmapStmtBool = Hptmap.Make
    (Cil_datatype.Stmt_Id)
    (Datatype.Bool)
    (Hptmap.Comp_unused)
    (struct let v = [ [] ] end)
    (struct let l = [ Ast.self ] end)
    (* Clear the (non-project compliant) internal caches each time the ast
       changes, which includes every time we switch project. *)
  let () = Ast.add_hook_on_update (fun _ -> HptmapStmtBool.clear_caches ())

  module HashStmtHptmapStmtBool = Stmt.Hashtbl.Make(HptmapStmtBool)

  (* this a cache containing the path tests already computed *)
  type path_checker = HptmapStmtBool.t Stmt.Hashtbl.t

  let create () : path_checker = Stmt.Hashtbl.create 17

  let find_assoc_with_default (pc : path_checker) (v: stmt) =
    try Stmt.Hashtbl.find pc v with Not_found -> HptmapStmtBool.empty

  let add_to_cache pc v1 v2 b =
    let assoc = find_assoc_with_default pc v1 in
    let assoc' = HptmapStmtBool.add v2 b assoc in
    Stmt.Hashtbl.replace pc v1 assoc'

  let check_path_using_filter filterfunc pc v1 v2 =
    let assoc = find_assoc_with_default pc v1 in
    try HptmapStmtBool.find v2 assoc
    with Not_found -> 
      (* the path is not in cache; we check it with Dijkstra *)
      let visited = HV.create 97 in
      let q = Queue.create () in
      let rec loop () =
	if Queue.is_empty q then begin
          add_to_cache pc v1 v2 false;
	  false
	end else begin
	  let v = Queue.pop q in
          add_to_cache  pc v1 v true;
	  if Stmt.equal v v2 then
	    true
	  else begin
	    if not (HV.mem visited v) then begin
	      HV.add visited v ();
	      List.iter (fun v' -> if filterfunc v' then Queue.add v' q) v.succs
	    end;
	    loop ()
	  end
	end
      in
      Queue.add v1 q;
      loop ()

  let check_path = check_path_using_filter (fun _ -> true)
end

(* The kf is no longer useful, but we need to do a partial application anyway *)
let stmt_can_reach _kf =
  let cache = PathChecker.create () in
  let check = PathChecker.check_path cache in
  fun s1 s2 ->
    (*Kernel.debug ~level:4 "CHECK PATH %d->%d@\n" s1.sid s2.sid;*)
    check s1 s2

(* Cached versions of [Stmts_graph.stmt_can_reach] *)

module StmtCanReachCache =
  Kernel_function.Make_Table
    (Datatype.Function
       (struct include Cil_datatype.Stmt let label = None end)
       (Datatype.Function
          (struct include Cil_datatype.Stmt let label = None end)
          (Datatype.Bool)))
    (struct
      let name = "Eval_funs.StmtCanReachCache"
      let size = 17
      let dependencies = [ Ast.self ]
     end)

let stmt_can_reach = StmtCanReachCache.memo stmt_can_reach


let stmt_can_reach_filtered filterfunc =
  let cache = PathChecker.create () in
  let check = PathChecker.check_path_using_filter filterfunc cache in
  fun s1 s2 ->
    (*Kernel.debug ~level:4 "CHECK PATH WITH FUNC %d->%d@\n" s1.sid s2.sid;*)
    check s1 s2

let stmt_is_in_cycle_filtered filterfunc stmt =
  let reachable = stmt_can_reach_filtered filterfunc in
  List.exists (fun s -> filterfunc s && reachable stmt s) stmt.preds

let stmt_is_in_cycle = stmt_is_in_cycle_filtered (fun _ -> true)

module SG = Graph.Imperative.Digraph.Concrete(Stmt)

module TP = struct
  include SG
  let graph_attributes _ = []

  let pretty_raw_stmt s =
    let s = Pretty_utils.sfprintf "%a" Printer.pp_stmt s in
    if String.length s >= 50 then (String.sub s 0 49) ^ "..." else s

  let vertex_name s =
    Format.sprintf "%S"
      (match s.skind with
       | Instr _ -> Format.sprintf "INSTR <%d>\n%s" s.sid (pretty_raw_stmt s)
       | Return _ -> Format.sprintf "RETURN <%d>" s.sid
       | Goto _ -> Format.sprintf "%s <%d>\n" (pretty_raw_stmt s) s.sid
       | Break _ -> Format.sprintf "BREAK <%d>" s.sid
       | Continue _ -> Format.sprintf "CONTINUE <%d>" s.sid
       | If(e,_,_,_) ->
	 Pretty_utils.sfprintf "IF <%d>\n%a" s.sid Printer.pp_exp e
       | Switch _ ->  Format.sprintf "SWITCH <%d>" s.sid
       | Loop _ ->  Format.sprintf "WHILE(1) <%d>" s.sid
       | Block _ ->  Format.sprintf "BLOCK <%d>" s.sid
       | TryExcept _ ->  Format.sprintf "TRY EXCEPT <%d>" s.sid
       | TryFinally _ ->  Format.sprintf "TRY FINALLY <%d>" s.sid
       | UnspecifiedSequence _ ->
         Format.sprintf "UnspecifiedSequence <%d>" s.sid)

  let vertex_attributes s =
    match s.skind with
    | Loop _ -> [`Color 0xFF0000; `Style [`Filled]]
    | If _ -> [`Color 0x00FF00; `Style [`Filled]; `Shape `Diamond]
    | Return _ -> [`Color 0x0000FF; `Style [`Filled]]
    | Block _ -> [`Shape `Box; `Fontsize 8]
    | Goto _ -> [`Shape `Diamond; `Color 0x00FFFF ; `Style [`Filled]]
    | Instr (Skip _) -> [`Color 0x00FFFF ; `Style [`Filled]]
    | _ -> []
  let default_vertex_attributes _ = []

  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
end

module GPrint = Graph.Graphviz.Dot(TP)

class stmt_graph_builder = object
  inherit nopCilVisitor
  val graph = SG.create ()
  method result = graph
  method! vstmt s =
    SG.add_vertex graph s; (* required for function with exactly one stmt *)
    List.iter (SG.add_edge graph s) s.succs;
    (* preds will be set latter while being visited *)
    DoChildren
end

let compute_stmtgraph_func func =
  let o = new stmt_graph_builder in
  ignore (visitCilFunction (o:>cilVisitor) func);
  if Kernel.debug_atleast 1 then
    begin
      Kernel.debug
        "Function %s: Nb vertex: %d Nb edges:%d See file '%s_cfg.dot'.@\n"
        func.svar.vname
        (SG.nb_edges o#result)
        (SG.nb_vertex o#result)
        func.svar.vname;
      let oc = open_out (func.svar.vname^"_cfg.dot") in
      GPrint.output_graph oc o#result;
      close_out oc;
    end;
  (* Classic.add_transitive_closure ~reflexive:true o#result*)
  o#result

module StmtsGraphTbl=
  State_builder.Hashtbl
    (Kernel_function.Hashtbl)
    (Datatype.Make
       (struct
         include Datatype.Serializable_undefined
         type t = SG.t
         let name = "Stmts_Graph.SG.t"
         let reprs = [ SG.create () ]
         let mem_project = Datatype.never_any_project
        end))
    (struct
      let name = "StmtsGraphTbl"
      let size = 17
      let dependencies = [ Ast.self ]
     end)

let get_graph kf =
  StmtsGraphTbl.memo
    (fun kf -> match kf.fundec with
    | Definition (f,_) ->
      compute_stmtgraph_func f
    | Declaration _ -> assert false)
    kf



module Reachable_Stmts =
  Cil_state_builder.Stmt_hashtbl
    (Stmt)
    (struct
       let name = "reachable_stmts"
       let size = 97
       let dependencies = [ Ast.self ]
     end)

let reachable_stmts kf s =
  let g = get_graph kf in
  let rec apply s =
    if Reachable_Stmts.mem s then
      Reachable_Stmts.find_all s
    else begin
      SG.iter_succ
        (fun s' ->
           Reachable_Stmts.add s s';
           List.iter (Reachable_Stmts.add s) (apply s'))
        g
        s;
      Reachable_Stmts.find_all s
    end
  in
  apply s

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(** Store for each statement, the set of the statements it is composed of.
    For a simple statement (not containing blocks), it is only the statement
    itself. *)
module StmtStmts =
  Cil_state_builder.Stmt_hashtbl
    (Stmt.Set)
    (struct
      let name = "StmtStmts"
      let size = 142
      let dependencies = [ Ast.self ]
     end)

let rec get_block_stmts blk =
  let add stmts s = Stmt.Set.union (get_stmt_stmts s) stmts in
  List.fold_left add Stmt.Set.empty blk.bstmts

and get_stmt_stmts s =
  let compute_stmt_stmts s = match s.skind with
    | Instr _ | Return _ -> Stmt.Set.singleton s
    | Continue _ | Break _ | Goto _ -> Stmt.Set.singleton s
    | Block b | Switch (_, b, _, _) | Loop (_, b, _, _, _) ->
        Stmt.Set.add s (get_block_stmts b)
    | UnspecifiedSequence seq ->
        let b = Cil.block_from_unspecified_sequence seq in
          Stmt.Set.add s (get_block_stmts b)
    | If (_, b1, b2, _) ->
        let stmts =
          Stmt.Set.union (get_block_stmts b1)(get_block_stmts b2)
        in Stmt.Set.add s stmts
    | TryExcept (_, _, _, _) | TryFinally (_, _, _) ->
        Kernel.not_yet_implemented "exception handling"
  in
  StmtStmts.memo compute_stmt_stmts s

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

module EdgeDatatype = Datatype.Pair (Stmt)(Stmt)
module EdgesDatatype = Datatype.List (EdgeDatatype)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(** Store for each statement [s], the elements in its statements that
    are ways out of [s], split by termination kind :
    [Normal | Breaks | Continues | Returns + Goto]
    Notice that [Exits] is not here since it cannot be determined directly :
    every call possibly have an [Exits] termination. *)

type waysout = { normal : EdgesDatatype.t ;
                 breaks : EdgesDatatype.t ;
                 continues : EdgesDatatype.t ;
                 returns : EdgesDatatype.t ;
                 gotos : EdgesDatatype.t ;
}
let empty_waysout = { normal = []; breaks = []; continues = [];
                      returns = []; gotos = [] }

module WaysOutDatatype =
  Datatype.Make
    (struct
       include Datatype.Undefined (* TODO: unmarshal ? *)
       type t = waysout
       let reprs = [ empty_waysout ]
       let name = "WaysOut"
       let mem_project = Datatype.never_any_project
     end)

module StmtWaysOut =
  Cil_state_builder.Stmt_hashtbl (WaysOutDatatype)
    (struct
      let name = "StmtWaysOut"
      let size = 142
      let dependencies = [ StmtStmts.self ]
     end)

let compute_stmts_out_edges stmts =
  let do_s s waysout =
    (* if [s] has a successor [s'] which is not in [stmt] statements,
    * add [s,s'] *)
    let add s acc =
      let do_succ acc s' =
        if Stmt.Set.mem s' stmts then acc
        else (s, s')::acc
      in List.fold_left do_succ acc s.succs
    in match s.skind with
          | Continue _ -> { waysout with continues = add s waysout.continues }
          | Break _ -> { waysout with breaks = add s waysout.breaks }
          | Return _ -> { waysout with returns = add s waysout.returns }
          | Goto _ ->
              begin
                match s.succs with
                  | { skind = Return _ }::[] ->
                      { waysout with returns = add s waysout.returns }
                  | _ -> { waysout with gotos = add s waysout.gotos }
              end
           | _ -> { waysout with normal = add s waysout.normal }
  in
    Stmt.Set.fold do_s stmts empty_waysout

let merge_waysout waysout =
    waysout.normal @ waysout.breaks @ waysout.continues @
    waysout.returns @ waysout.gotos

let select_waysout termination_kind waysout =
  match termination_kind with
  | Some Normal -> waysout.normal
  | Some Breaks -> waysout.breaks
  | Some Continues -> waysout.continues
  | Some Returns -> waysout.returns
  | None (* Goto *) -> waysout.gotos
  | Some Exits ->
    invalid_arg "[get_stmt_out_edges] doesn't handle [Exits] termination_kind"

let compute_stmt_out_edges stmt =
  compute_stmts_out_edges (get_stmt_stmts stmt)

let get_stmt_out_edges termination_kind stmt =
  let waysout = StmtWaysOut.memo compute_stmt_out_edges stmt in
  select_waysout termination_kind waysout

let get_all_stmt_out_edges s =
  let waysout = StmtWaysOut.memo compute_stmt_out_edges s in
  merge_waysout waysout

let compute_block_out_edges blk =
  compute_stmts_out_edges (get_block_stmts blk)

let get_all_block_out_edges blk =
  let waysout = compute_block_out_edges blk in
  merge_waysout waysout

let get_block_out_edges termination_kind blk =
  let waysout = compute_block_out_edges blk in
    select_waysout termination_kind waysout

let get_all_stmt_last_stmts s =
  List.map fst (get_all_stmt_out_edges s)

let get_all_block_last_stmts b =
  List.map fst (get_all_block_out_edges b)

let get_stmt_last_stmts tk s =
  List.map fst (get_stmt_out_edges tk s)

let get_block_last_stmts tk b =
  List.map fst (get_block_out_edges tk b)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
module StmtWaysIn =
  Cil_state_builder.Stmt_hashtbl
    (Datatype.List (EdgeDatatype))
    (struct
      let name = "StmtWaysIn"
      let size = 142
      let dependencies = [ StmtStmts.self ]
     end)

let compute_stmts_in_edges stmts =
  let add s acc =
    let do_pred acc s' =
      if (Stmt.Set.mem s' stmts) then acc else (s',s)::acc
    in List.fold_left do_pred acc s.preds
  in Stmt.Set.fold add stmts []

let compute_stmt_entry_stmts stmt =
  compute_stmts_in_edges (get_stmt_stmts stmt)

let get_stmt_in_edges s =
    StmtWaysIn.memo compute_stmt_entry_stmts s

let get_block_in_edges blk =
  compute_stmts_in_edges (get_block_stmts blk)

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let loop_preds s = match s.skind with
  | Loop _ ->
    let loop_stmts = get_stmt_stmts s in
    let back_edges, entry =
      List.partition (fun s -> Stmt.Set.mem s loop_stmts) s.preds
    in
    entry, back_edges
  | _ ->
    invalid_arg "[loop_preds] not a loop"

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
