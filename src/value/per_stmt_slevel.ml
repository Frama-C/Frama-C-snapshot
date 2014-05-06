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

module G = struct
  type t = kernel_function
  let is_directed = true

  module V = Cil_datatype.Stmt

  let fold_vertex f kf acc =
    f (Kernel_function.find_first_stmt kf) acc
  let iter_vertex f kf = f (Kernel_function.find_first_stmt kf)
  let succs s = s.succs
  let iter_succ f _ v = List.iter f (succs v)
  let fold_succ f _ v acc = List.fold_right f (succs v) acc
end

module Dfs = Graph.Traverse.Dfs(G)

module LocalSlevelId = State_builder.SharedCounter(struct
  let name = "Local_slevel.LocalSlevelId"
end)

module LocalSlevelAnnots =
  State_builder.Int_hashtbl
    (Datatype.Option(Datatype.Int))
    (struct
      let name = "Local_slevel.LocalSlevelAnnots"
      let dependencies = [Ast.self]
      let size = 17
     end)
(* Link this state to the AST. Otherwise, it gets removed when parsing hooks
   (such as loop unrolling) are used. *)
let () = Ast.add_linked_state LocalSlevelAnnots.self
(* TODO: it would be great to be able to add annotations dynamically during
   analysis. We must find to allow the user to fill this table. *)

let retrieve_annot i =
  try LocalSlevelAnnots.find i
  with Not_found -> assert false (*This table is always filled at parsing time*)


let () = Logic_typing.register_behavior_extension "slevel"
  (fun ~typing_context:_ ~loc bhv args ->
    let id = LocalSlevelId.next () in
    let open Logic_ptree in
    bhv.b_extended <- ("slevel", id, []) :: bhv.b_extended;
    match args with
    | [{lexpr_node = PLvar "default"}] ->
      LocalSlevelAnnots.add id None;
    | [{lexpr_node = PLconstant (IntConstant i)}] ->
      LocalSlevelAnnots.add id (Some (int_of_string i))
    | _ -> Value_parameters.abort ~source:(fst loc)
      "Invalid slevel directive"
  )

let () = Cil_printer.register_behavior_extension "slevel"
  (fun _pp fmt (i, _) ->
    match retrieve_annot i with
    | None -> Format.pp_print_string fmt "default"
    | Some i -> Format.pp_print_int fmt i
  )

type slevel =
| Global of int
| PerStmt of (stmt -> int)

module DatatypeSlevel = Datatype.Make(struct
  include Datatype.Undefined
  type t = slevel
  let reprs = [Global 0]
  let name = "Value.Local_slevel.Datatype"
  let mem_project = Datatype.never_any_project
end)

let extract_slevel_directive s =
  let rec find_one l =
    match l with
    | [] -> None
    | {annot_content =
        AStmtSpec (_, { spec_behavior = [{b_extended = ["slevel", i, _]}]})}
      :: _ -> Some (retrieve_annot i)
    | _ :: q -> find_one q
  in
  find_one (Annotations.code_annot s)

let kf_contains_slevel_directive kf =
  List.exists
    (fun stmt -> extract_slevel_directive stmt <> None)
    (Kernel_function.get_definition kf).sallstmts

let for_kf kf =
  let default_slevel = Value_util.get_slevel kf in
  if not (kf_contains_slevel_directive kf) then
    Global default_slevel (* No slevel directive *)
  else
    let h = Cil_datatype.Stmt.Hashtbl.create 17 in
    let local_slevel = Stack.create () in
    Stack.push default_slevel local_slevel;
    let debug = false in
    (* Before visiting the successors of the statement: push or pop according
       to directive *)
    let pre s =
      match extract_slevel_directive s with
      | None ->
        Cil_datatype.Stmt.Hashtbl.add h s (Stack.top local_slevel)
      | Some (Some i) ->
        if debug then Format.printf "Vising split %d, pushing %d@." s.sid i;
        Cil_datatype.Stmt.Hashtbl.add h s i;
        Stack.push i local_slevel;
      | Some None ->
        let top = Stack.pop local_slevel in
        if debug then
          Format.printf "Visiting merge %d, poping (prev %d)@." s.sid top;
        (* Store top, ie. the slevel value above s, in h. We will use this
           value in the post function *)
        Cil_datatype.Stmt.Hashtbl.add h s top
    (* after the visit of a statement and its successors. Do the converse
       operation of pre *)
    and post s =
      match extract_slevel_directive s with
      | None -> ()
      | Some (Some _) ->
        if debug then Format.printf "Leaving split %d, poping@." s.sid;
        ignore (Stack.pop local_slevel);
      | Some None ->
        (* slevel on nodes above s *)
        let above = Cil_datatype.Stmt.Hashtbl.find h s in
        (* slevel on s and on the nodes below *)
        let cur = Stack.top local_slevel in
        if debug then
          Format.printf "Leaving merge %d, restoring %d@." s.sid above;
        Stack.push above local_slevel;
        Cil_datatype.Stmt.Hashtbl.replace h s cur
    in
    try
      Dfs.iter ~pre ~post kf;
      PerStmt
        (fun s ->
          try Cil_datatype.Stmt.Hashtbl.find h s
          with Not_found -> assert false (* all statements have been visited*))
    with Stack.Empty ->
      Value_parameters.abort
        "Incorrectly nested slevel directives in function %a"
        Kernel_function.pretty kf


module ForKf = Kernel_function.Make_Table
  (DatatypeSlevel)
  (struct
    let size = 17
    let dependencies =
      [LocalSlevelAnnots.self;
       Value_parameters.SemanticUnrollingLevel.self;
       Value_parameters.ShowSlevel.self]
    let name = "Value.Local_slevel.ForKf"
   end)

let for_kf = ForKf.memo for_kf
