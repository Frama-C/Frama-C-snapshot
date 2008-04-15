(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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

module K = PdgIndex.Key
module S = PdgIndex.Signature

module N = PdgTypes.Node
module G = PdgTypes.G
module Dpd = PdgTypes.Dpd

let pretty_key = K.pretty

let pretty_node fmt n =
    let id = N.elem_id n in
    Format.fprintf fmt "[Elem] %d : " id;
    let key = N.elem_key n in pretty_key fmt key

let pretty_nodes fmt nodes =
  let pretty_node n = Format.fprintf fmt "%a@." pretty_node n
  in List.iter pretty_node nodes

let pretty_pdg_graph ?(bw=false) fmt graph =
  let iter = if bw then G.iter_pred_e else G.iter_succ_e in
  let print_node n =  Format.fprintf fmt "%a@." pretty_node n in
  let print_dpd d =
    let dpd_kind = G.E.label d in
    if bw then Format.fprintf fmt "  <-%a- %d@." G.pretty_edge_label dpd_kind
                 (N.elem_id (G.E.src d))
    else Format.fprintf fmt "  -%a-> %d@." G.pretty_edge_label dpd_kind
                 (N.elem_id (G.E.dst d))
  in
  let print_node_and_dpds n =
    print_node n ; iter print_dpd graph n
  in
    G.iter_vertex print_node_and_dpds graph

let pretty_pdg ?(bw=false) fmt pdg =
  try
    let graph = PdgTypes.InternalPdg.get_graph pdg in
    pretty_pdg_graph ~bw fmt graph;
  with PdgTypes.Pdg.Top -> Format.fprintf fmt "Top PDG@."
    |  PdgTypes.Pdg.Bottom -> Format.fprintf fmt "Bottom PDG@."

(*-----------------------------------------------------------------------*)
module Printer = struct
  type t = PdgTypes.Pdg.t
  module V = G.V
  module E = struct
    type t = G.E.t * bool (** boolean to say that the edge is dynamic *)
    let src (e, _dyn) = G.E.src e
    let dst (e, _dyn) = G.E.dst e
    end

  let iter_vertex f pdg =
    try
      let graph = PdgTypes.InternalPdg.get_graph pdg in
        G.iter_vertex f graph
    with PdgTypes.Pdg.Top | PdgTypes.Pdg.Bottom -> ()

  let iter_edges_e f pdg =
    try
      let graph = PdgTypes.InternalPdg.get_graph pdg in
      let f_static e = f (e, false) in
        G.iter_edges_e f_static graph;
    with PdgTypes.Pdg.Top | PdgTypes.Pdg.Bottom -> ()

  let graph_attributes _ = [`Rankdir `TopToBottom ]

  let default_vertex_attributes _ = [`Style `Filled]
  let vertex_name v = string_of_int (N.elem_id v)

  let vertex_attributes v =
    let color_in = (`Fillcolor 0x6495ED) in
    let color_out = (`Fillcolor 0x90EE90) in
    let color_decl = (`Fillcolor 0xFFEFD5) in
    let color_stmt = (`Fillcolor 0xCCCCCC) in
    (* let color_annot = (`Fillcolor 0x999999) in *)
    let color_call = (`Fillcolor 0xFF8A0F) in
    let color_elem_call = (`Fillcolor 0xFFCA6E) in
    let sh_box = (`Shape `Box) in
    let key = N.elem_key v in
    let sh, col, txt = match key with
        | K.VarDecl v ->
            let txt =
              Pretty_utils.sfprintf "@[Decl %a@]" !Ast_printer.d_ident v.vname
            in
              (`Shape `Box) , color_decl , txt
        | K.SigKey k ->
            let txt = Cil.fprintf_to_string "%a" S.pretty_key k in
            let color = match k with | S.Out _ -> color_out | _ ->  color_in in
              (`Shape `Box), color, txt
        | K.Stmt s ->
            let sh, txt = match s.skind with
              | Switch (exp,_,_,_) | If (exp,_,_,_) ->
                  let txt = Cil.fprintf_to_string "%a" !Ast_printer.d_exp exp in
                    (`Shape `Diamond), txt
              | Loop _ ->
                  (`Shape `Doublecircle), "while"
              | Block _ | UnspecifiedSequence _ ->
                  (`Shape `Doublecircle), "{}"
              | Goto _ | Break _ | Continue _ ->
                  let txt = Cil.fprintf_to_string "%a"
                              (Cil.defaultCilPrinter#pStmtKind s) s.skind
                  in (`Shape `Doublecircle), txt
              | Return _ | Instr _ ->
                  let txt = Cil.fprintf_to_string "%a"
                              (Cil.defaultCilPrinter#pStmtKind s) s.skind in
                    sh_box, txt
              | _ -> sh_box, "???"
            in sh, color_stmt, txt
        (* | K.Annot _ ->
            let txt = Cil.fprintf_to_string "%a" pretty_key key in
            (`Shape `Doublecircle), color_annot, txt *)
        | K.CallStmt call ->
            let call_stmt = K.call_from_id call in
            let txt = Cil.fprintf_to_string "%a"
                        (Cil.defaultCilPrinter#pStmtKind call_stmt)
                        call_stmt.skind
            in sh_box, color_call, txt
        | K.SigCallKey (_call, sgn) ->
            let txt = Cil.fprintf_to_string "%a" S.pretty_key sgn in
              sh_box, color_elem_call, txt
        | K.Label _ ->
            let txt = Cil.fprintf_to_string "%a" pretty_key key in
              sh_box, color_stmt, txt
    in sh :: col :: [`Label ( String.escaped txt)]

  let default_edge_attributes _ = []

  let edge_attributes (e, dynamic) =
    let d, z = G.edge_dpd e in
    let attrib = [] in
    let attrib = match z with 
      | None -> attrib
      | Some z ->
          let txt = 
            Cil.fprintf_to_string "@[<h 1>%a@]" Locations.Zone.pretty z in
          (`Label txt) :: attrib
    in
    let attrib =
      let color =
        if Dpd.is_data d then (if dynamic then 0xFF00FF else 0x0000FF)
        else  (if dynamic then 0xFF0000 else 0x000000)
      in (`Color color) :: attrib
    in
    let attrib =
      if Dpd.is_ctrl d then (`Arrowhead `Odot)::attrib else attrib
    in
    let attrib =
      if Dpd.is_addr d then (`Style `Dotted)::attrib else attrib
    in attrib

  let get_subgraph v =
    let mk_subgraph name attrib =
      let attrib = (`Style `Filled) :: attrib in
          Some { Graph.Graphviz.DotAttributes.sg_name= name;
                 Graph.Graphviz.DotAttributes.sg_attributes = attrib }
    in
    match N.elem_key v with
      | K.CallStmt call | K.SigCallKey (call, _) ->
          let call_stmt = K.call_from_id call in
          let name = "Call"^(string_of_int call_stmt.sid) in
          let call_txt =
            Cil.fprintf_to_string "%a"
              (fun fmt ->
                 Cil.defaultCilPrinter#pStmtKind call_stmt fmt
              )
                        call_stmt.skind  in
          let call_txt = String.escaped call_txt in
          let attrib = [(`Label (name^" : "^call_txt))] in
          let attrib = (`Fillcolor 0xB38B4D) :: attrib in
            mk_subgraph name attrib
      | K.SigKey k ->
          let pack_inputs_outputs = false in
          if pack_inputs_outputs then
            begin
              let is_in =  match k with S.In _ -> true | _ -> false in
              let name = if is_in then "Inputs" else "Outputs" in
              let color = if is_in then 0x90EE90 else 0x6495ED in
              let attrib = [] in
              let attrib = (`Fillcolor color) :: attrib in
                mk_subgraph name attrib
            end
          else
            None
      | _ -> None
end

(** @see <http://www.lri.fr/~filliatr/ocamlgraph/doc/Graphviz.html>
*        Graph.Graphviz *)
module PrintG = Graph.Graphviz.Dot(Printer)

(*-----------------------------------------------------------------------*)
(** build the PDG .dot file and put it in [filename].  *)
let build_dot filename pdg =
  let file = open_out filename in
  PrintG.output_graph file pdg;
    close_out file

(** build the .dot file and put it in [pdg function name.dot]  *)
let build_dot_file pdg =
  let kf = PdgTypes.Pdg.get_kf pdg in
  let fct_name = Kernel_function.get_name kf in
  let filename = (fct_name ^ ".dot") in
    build_dot filename pdg;
    filename

    (*
  let build_fct_pdg_dot_file proj kf  =
    let pdg = get_pdg proj kf in
    let dot_filename = Pdg.build_dot_file pdg in
      Format.printf "[pdg] dot file generated in %s@." dot_filename;
      dot_filename

  let show_fct_pdg_dot_file proj kf =
    let filename = build_fct_pdg_dot_file proj kf in
    let cmd = "zgrviewer -Pdot" in
      ignore (Sys.command (cmd^" $PWD/" ^ filename))
*)

(*-----------------------------------------------------------------------*)
