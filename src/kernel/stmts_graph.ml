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

(* $Id: stmts_graph.ml,v 1.23 2008-10-13 12:56:47 uid530 Exp $ *)

open Cil_types
open Cil
open Db_types
open Cilutil

module SG = Graph.Imperative.Digraph.Concrete(StmtComparable)

module PathChecker = Graph.Path.Check(SG)

(* For transitive closure
module Classic = Graph.Oper.I(StmtsGraph)
*)

module TP = struct
  include SG
  let graph_attributes _ = []

  let pretty_raw_stmt s =
    let s = Pretty_utils.sfprintf "%a" !Ast_printer.d_stmt s in
    if String.length s >= 50 then (String.sub s 0 49) ^ "..." else s

  let vertex_name s =
    Format.sprintf "%S"
      (match s.skind with
       | Instr _ -> Format.sprintf "INSTR <%d>\n%s" s.sid (pretty_raw_stmt s)
       | Return _ -> Format.sprintf "RETURN <%d>" s.sid
       | Goto _ -> Format.sprintf "%s <%d>\n" (pretty_raw_stmt s) s.sid
       | Break _ -> Format.sprintf "BREAK <%d>" s.sid
       | Continue _ -> Format.sprintf "CONTINUE <%d>" s.sid
       | If(e,_,_,_) -> Pretty_utils.sfprintf "IF <%d>\n%a" s.sid !Ast_printer.d_exp e
       | Switch _ ->  Format.sprintf "SWITCH <%d>" s.sid
       | Loop _ ->  Format.sprintf "WHILE(1) <%d>" s.sid
       | Block _ ->  Format.sprintf "BLOCK <%d>" s.sid
       | TryExcept _ ->  Format.sprintf "TRY EXCEPT <%d>" s.sid
       | TryFinally _ ->  Format.sprintf "TRY FINALLY <%d>" s.sid
       | UnspecifiedSequence _ -> Format.sprintf "UnspecifiedSequence <%d>" s.sid)

  let vertex_attributes s =
    match s.skind with
    | Loop _ -> [`Color 0xFF0000; `Style `Filled]
    | If _ -> [`Color 0x00FF00; `Style `Filled; `Shape `Diamond]
    | Return _ -> [`Color 0x0000FF; `Style `Filled]
    | Block _ -> [`Shape `Box; `Fontsize 8]
    | Goto _ -> [`Shape `Diamond; `Color 0x00FFFF ; `Style `Filled]
    | Instr (Skip _) -> [`Color 0x00FFFF ; `Style `Filled]
    | _ -> []
  let default_vertex_attributes _ = []

  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
end

module GPrint = Graph.Graphviz.Dot(TP)

class stmt_graph_builder nb_stmt = object
  inherit nopCilVisitor
  val graph = SG.create ()
  method result = graph
  method vstmt s =
    SG.add_vertex graph s; (* required for function with exactly one stmt *)
    List.iter (SG.add_edge graph s) s.succs;
    (* preds will be set latter while being visited *)
    DoChildren
end

let compute_stmtgraph_func func =
  let nb_stmt = List.length func.sallstmts in
  let o = new stmt_graph_builder nb_stmt in
  ignore (visitCilFunction (o:>cilVisitor) func);
  if Kernel.debug_atleast 1 then
    begin
      Kernel.debug
	"Function %a: Nb vertex: %d Nb edges:%d See file '%s_cfg.dot'.@\n"
	!Ast_printer.d_ident func.svar.vname
	(SG.nb_edges o#result)
	(SG.nb_vertex o#result)
	func.svar.vname;
      let oc = open_out (func.svar.vname^"_cfg.dot") in
      GPrint.output_graph oc o#result;
      close_out oc;
    end;
  (* Classic.add_transitive_closure ~reflexive:true o#result*)
  o#result

let compute_stmtgraph kf =
  match kf.fundec with
  | Definition (f,_) ->
      (match kf.stmts_graph with
       | None ->
           let g = compute_stmtgraph_func f in
           kf.stmts_graph <- Some g
       | Some _ -> ())
  | Declaration _ -> ()

let check_path g =
  let internal = PathChecker.create g in
  PathChecker.check_path internal

let get_graph kf = match kf.stmts_graph with
  | None ->
      compute_stmtgraph kf;
      (match kf.stmts_graph with None -> assert false | Some f -> f);
  | Some f -> 
      f

let stmt_can_reach kf s1 s2 =
  if Kernel.debug_atleast 1 then
    Kernel.debug "CHECK PATH %d->%d@\n" s1.sid s2.sid;
  check_path (get_graph kf) s1 s2

module Reachable_Stmts = 
  Cil_computation.StmtHashtbl
    (Cil_datatype.Stmt)
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
