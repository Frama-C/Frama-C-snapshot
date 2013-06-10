(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

module NS = PdgTypes.NodeSet


(** Why is a node impacted. The reasons will be given as [n is impacted
    by the effect of [n'], and the impact is of type reason]. *)
type reason_type =
  | Intraprocedural of PdgTypes.Dpd.t
      (** The effect of [n'] in [f] impact [n], which is also in [f]. *)

  | InterproceduralDownward (** the effect of [n'] in [f] has an effect on a
      callee [f'] of [f], in which [n] is located. *)

  | InterproceduralUpward  (** the effect of [n'] in [f] has an effect on a
      caller [f'] of [f] (once the call to [f] has ended), [n] being in [f']. *)

module ReasonType = Datatype.Make(
  struct
    type t = reason_type
    let name = "Impact.Reason_graph.reason_type"
    let reprs = [InterproceduralDownward]
    include Datatype.Serializable_undefined
    let compare (v1: t) (v2: t) = Extlib.compare_basic v1 v2
    let hash (v: t) = Hashtbl.hash v
    let equal (v1: t) (v2: t) = v1 == v2
  end)

(** Reasons for impact are expressed as sets [(n', n, reason)] *)
module Reason =
  Datatype.Triple_with_collections(PdgTypes.Node)(PdgTypes.Node)(ReasonType)
    (struct let module_name = "Impact.Reason_graph.Reason.t" end)

type reason = Reason.Set.t

let empty = Reason.Set.empty

module Printer = struct

  type t = Reason.Set.t
  module V = struct
    type t = PdgTypes.Node.t
    let pretty fmt n = PdgIndex.Key.pretty fmt (PdgTypes.Node.elem_key n)
  end
  module E = struct
    type t = V.t * V.t * reason_type
    let src (e, _, _) = e
    let dst (_, e, _) = e
  end

  let iter_vertex f graph =
    let all =
      Reason.Set.fold
        (fun (src, dst, _) acc -> NS.add src (NS.add dst acc)) graph NS.empty
    in
    NS.iter f all

  let iter_edges_e f graph = Reason.Set.iter f graph

  let vertex_name n = Format.sprintf "n%d" (PdgTypes.Node.id n)

  let graph_attributes _ = [`Label "Impact graph"]

  let default_vertex_attributes _g = [`Style `Filled]
  let default_edge_attributes _g = []

  let vertex_attributes v =
    let txt = Pretty_utils.to_string V.pretty v in
    let txt = if String.length txt > 100 then String.sub txt 0 100 else txt in
    let txt = Pretty_utils.sfprintf "%S" txt in
    let txt = String.sub txt 1 (String.length txt - 2) in
    [`Label txt]

  let edge_attributes (_, _, reason) =
    let color = match reason with
      | Intraprocedural _ -> 0x2F9F9F
      | InterproceduralUpward -> 0x9F2F9F
      | InterproceduralDownward -> 0x9F9F2F
    in
    let attribs = [`Color color] in
    match reason with
      | Intraprocedural dpd ->
          `Label (Pretty_utils.to_string PdgTypes.Dpd.pretty dpd)  :: attribs
      | _ -> attribs


  let get_subgraph n = match PdgIndex.Key.stmt (PdgTypes.Node.elem_key n) with
    | None -> None
    | Some stmt ->
        let kf = Kernel_function.find_englobing_kf stmt in
        let name = Kernel_function.get_name kf in
        let attrs = {
          Graph.Graphviz.DotAttributes.sg_name = name;
          sg_attributes = [`Label name];
        } in
        Some attrs
        
end

module Dot = Graph.Graphviz.Dot(Printer)

(* May raise [Sys_error] *)
let to_dot_file ~temp reason =
  let dot_file =
    try
      let f name ext =
        if temp
        then Extlib.temp_file_cleanup_at_exit name ext
        else Filename.temp_file name ext
      in
      f "impact_reason" ".dot"
    with Extlib.Temp_file_error s ->
      Options.abort "cannot create temporary file: %s" s
  in
  let cout = open_out dot_file in
  Kernel.Unicode.without_unicode (Dot.output_graph cout) reason;
  close_out cout;
  dot_file

let print_dot_graph reason =
  try
    let dot_file = to_dot_file ~temp:false reason in
    Options.result "Graph output in file '%s'" dot_file
  with Sys_error _ as exn ->
    Options.error "Could not generate impact graph: %s"
      (Printexc.to_string exn)



(* Very basic textual debugging function *)
let print_reason reason =
  let pp_node = !Db.Pdg.pretty_node false in
  let pp fmt (nsrc, ndst, reason) =
    Format.fprintf fmt "@[<v 2>%a -> %a (%s)@]"
      pp_node nsrc pp_node ndst
      (match reason with
         | Intraprocedural dpd ->
             Pretty_utils.sfprintf "intra %a" PdgTypes.Dpd.pretty dpd
         | InterproceduralDownward -> "downward"
         | InterproceduralUpward -> "upward"
      )
  in
  Options.result "Impact graph:@.%a"
    (Pretty_utils.pp_iter ~pre:"@[<v>" ~sep:"@ " ~suf:"@]" Reason.Set.iter pp)
    reason
