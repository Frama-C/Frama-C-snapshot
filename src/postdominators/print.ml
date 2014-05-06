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
open Cil_datatype

let pretty_stmt fmt s =
  let key = PdgIndex.Key.stmt_key s in !Db.Pdg.pretty_key fmt key

module Printer = struct

  type t = string * (Stmt.Hptset.t option Kinstr.Hashtbl.t)
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
      Postdominators_parameters.debug "iter_vertex %d : %a\n" s.sid V.pretty s;
      let has_postdom = match postdom with None -> false | _ -> true in
      f (s, has_postdom)
    in
    Kinstr.Hashtbl.iter do_s graph

  let iter_edges_e f (_, graph) =
    let do_s ki postdom =
      let s = match ki with Kstmt s -> s | _  -> assert false in
      match postdom with None -> ()
      | Some postdom ->
        let do_edge p = f ((s, true), (p, true)) in
        Stmt.Hptset.iter do_edge postdom
    in
    Kinstr.Hashtbl.iter do_s graph


  let vertex_name (s, _) = string_of_int s.sid

  let graph_attributes (title, _) = [`Label title]

  let default_vertex_attributes _g = [`Style [`Filled]]
  let default_edge_attributes _g = []

  let vertex_attributes (s, has_postdom) =
    let attrib = [] in
    let txt = Pretty_utils.sfprintf "%a" V.pretty s in
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
    match Kinstr.Hashtbl.find graph (Kstmt s) with
    | None -> Stmt.Hptset.empty
    | Some l -> l
  with Not_found ->
    try
      let postdom = !Db.Postdominators.stmt_postdominators kf s in
      let postdom = Stmt.Hptset.remove s postdom in
      Postdominators_parameters.debug "postdom for %d:%a = %a\n"
        s.sid pretty_stmt s Stmt.Hptset.pretty postdom;
      Kinstr.Hashtbl.add graph (Kstmt s) (Some postdom); postdom
    with Db.PostdominatorsTypes.Top ->
      Kinstr.Hashtbl.add graph (Kstmt s) None;
      raise Db.PostdominatorsTypes.Top

(** [s_postdom] are [s] postdominators, including [s].
* We don't have to represent the relation between s and s.
* And because the postdom relation is transitive, if [p] is in [s_postdom],
* we can remove [p_postdom] from [s_postdom] in order to have a clearer graph.
*)
let reduce kf graph s =
  let remove p s_postdom =
    if Stmt.Hptset.mem p s_postdom
    then
      try
        let p_postdom = get_postdom kf graph p in
        let s_postdom = Stmt.Hptset.diff s_postdom p_postdom
        in s_postdom
      with Db.PostdominatorsTypes.Top -> assert false
                                   (* p postdom s -> cannot be top *)
    else s_postdom (* p has already been removed from s_postdom *)
  in
  try
    let postdom = get_postdom kf graph s in
    let postdom = Stmt.Hptset.fold remove postdom postdom in
    Postdominators_parameters.debug "new postdom for %d:%a = %a\n"
      s.sid pretty_stmt s Stmt.Hptset.pretty postdom;
    Kinstr.Hashtbl.replace graph (Kstmt s) (Some postdom)
  with Db.PostdominatorsTypes.Top ->
    ()

let build_reduced_graph kf graph stmts =
  List.iter (reduce kf graph) stmts

let build_dot filename kf =
  match kf.fundec with
    | Definition (fct, _) ->
      let stmts = fct.sallstmts in
      let graph = Kinstr.Hashtbl.create (List.length stmts) in
      let _ = build_reduced_graph kf graph stmts in
      let name = Kernel_function.get_name kf in
      let title = "Postdominators for function " ^ name in
      let file = open_out filename in
      PostdomGraph.output_graph file (title, graph);
      close_out file
    | Declaration _ ->
        Kernel.error "cannot compute for a function without body %a"
          Kernel_function.pretty kf

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
