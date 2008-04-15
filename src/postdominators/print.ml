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

open Cil_types
open Cilutil

let debug = false

let pretty_stmt fmt s =
  let key = PdgIndex.Key.stmt_key s in !Db.Pdg.pretty_key fmt key

module Printer = struct 
  type t = string * (StmtSet.t option InstrHashtbl.t)
  module V = struct
    type t = Cil_types.stmt * bool
    let pretty fmt v = pretty_stmt fmt v
  end
  module E = struct
    type t = (V.t * V.t)
    let src e = fst e
    let dst e = snd e
  end

  let iter_vertex f (_, graph) = 
    let do_s ki postdom = 
      let s = match ki with Kstmt s -> s | _  -> assert false in
      if debug then Cil.log "iter_vertex %d : %a\n" s.sid V.pretty s; 
      let has_postdom = match postdom with None -> false | _ -> true in
      f (s, has_postdom)
    in 
    InstrHashtbl.iter do_s graph

  let iter_edges_e f (_, graph) =
    let do_s ki postdom = 
      let s = match ki with Kstmt s -> s | _  -> assert false in
      match postdom with None -> ()
      | Some postdom ->
        let do_edge p = f ((s, true), (p, true)) in
          StmtSet.iter do_edge postdom
    in InstrHashtbl.iter do_s graph


  let vertex_name (s, _) = string_of_int s.sid

  let graph_attributes (title, _) = [`Label title]

  let default_vertex_attributes _g = [`Style `Filled]
  let default_edge_attributes _g = []

  let vertex_attributes (s, has_postdom) = 
    let attrib = [] in
    let txt = Cil.fprintf_to_string "%a" V.pretty s in
    let attrib = (`Label txt) :: attrib in
    let color = if has_postdom then 0x7FFFD4 else 0xFF0000 in
    let attrib = (`Shape `Box) :: attrib in 
    let attrib = (`Fillcolor color) :: attrib in 
      attrib

  let edge_attributes _s = []

  let get_subgraph _v = None
end

module PostdomGraph = Graph.Graphviz.Dot(Printer)

let get_postdom kf graph s =
  try 
    match InstrHashtbl.find graph (Kstmt s) with 
    | None -> StmtSet.empty 
    | Some l -> l
  with Not_found -> 
    try 
      let postdom = !Db.Postdominators.stmt_postdominators kf s in
      let postdom = StmtSet.remove s postdom in
      if debug then 
	Cil.log "postdom for %d:%a = %a\n" 
          s.sid pretty_stmt s StmtSet.pretty postdom;
      InstrHashtbl.add graph (Kstmt s) (Some postdom); postdom
    with Db.Postdominators.Top ->
      InstrHashtbl.add graph (Kstmt s) None;
      raise Db.Postdominators.Top

(** [s_postdom] are [s] postdominators, including [s].
* We don't have to represent the relation between s and s.
* And because the postdom relation is transitive, if [p] is in [s_postdom],
* we can remove [p_postdom] from [s_postdom] in order to have a clearer graph.
*)
let reduce kf graph s =
  let remove p s_postdom =
    if StmtSet.mem p s_postdom 
    then 
      try
        let p_postdom = get_postdom kf graph p in
        let s_postdom = StmtSet.diff s_postdom p_postdom
        in s_postdom
      with Db.Postdominators.Top -> assert false 
                                   (* p postdom s -> cannot be top *)
    else s_postdom (* p has already been removed from s_postdom *)
  in 
  try
    let postdom = get_postdom kf graph s in
    let postdom = StmtSet.fold remove postdom postdom in
      if debug then 
	Cil.log "new postdom for %d:%a = %a\n" 
          s.sid pretty_stmt s StmtSet.pretty postdom;
    InstrHashtbl.replace graph (Kstmt s) (Some postdom)
  with Db.Postdominators.Top -> 
    ()

let rec build_reduced_graph kf graph stmts = 
  List.iter (reduce kf graph) stmts

let build_dot filename kf =
  let stmts = match kf.Db_types.fundec with
    | Db_types.Definition (fct, _) -> fct.sallstmts
    | Db_types.Declaration _ -> invalid_arg
                         "[postdominators] cannot compute for a leaf function"
  in
  let graph = InstrHashtbl.create (List.length stmts) in
  let _ = build_reduced_graph kf graph stmts in
  let name = Kernel_function.get_name kf in 
  let title = "Postdominators for function " ^ name in
  let file = open_out filename in
  PostdomGraph.output_graph file (title, graph);
  close_out file

