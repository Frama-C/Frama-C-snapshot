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
open Callgraph
open Options

let entry_point_ref = ref None

module Service =
  Service_graph.Make
    (struct
       let datatype_name = name
       type t = Callgraph.callgraph
       module V = struct
         type t = callnode
         let id v = v.cnid
         let name v = nodeName v.cnInfo
         let attributes v =
           [ match v.cnInfo with
             | NIVar (_,b) when not !b -> `Style [`Dotted]
             | _ -> `Style [`Bold] ]
         let equal v1 v2 = id v1 = id v2
	 let compare v1 v2 = 
	   let i1 = id v1 in
	   let i2 = id v2 in
	   if i1 < i2 then -1 else if i1 > i2 then 1 else 0
         let hash = id
         let entry_point () = !entry_point_ref
       end
       let iter_vertex f = Hashtbl.iter (fun _ -> f)
       let iter_succ f _g v = Datatype.Int.Hashtbl.iter (fun _ -> f) v.cnCallees
       let iter_pred f _g v = Datatype.Int.Hashtbl.iter (fun _ -> f) v.cnCallers
       let fold_pred f _g v = Datatype.Int.Hashtbl.fold (fun _ -> f) v.cnCallers
     end)

module CG =
  State_builder.Option_ref
    (Service.CallG.Datatype)
    (struct
       let name = name
       let dependencies = [ Ast.self ]
     end)

let get_init_funcs main_name cg =
  match main_name with
  | None -> InitFunc.get ()
  | Some s ->
    (* the entry point is always a root *)
    let init_funcs = Datatype.String.Set.add s (InitFunc.get ()) in
    (* Add the callees of entry point as roots *)
    Datatype.String.Set.union
      (try
         let callees = (Hashtbl.find cg s).Callgraph.cnCallees in
         Datatype.Int.Hashtbl.fold
           (fun _ v acc -> match v.Callgraph.cnInfo with
           | Callgraph.NIVar ({vname=n},_) -> Datatype.String.Set.add n acc
           | _ -> acc)
           callees
           Datatype.String.Set.empty
       with Not_found ->
         Datatype.String.Set.empty)
      init_funcs

let compute () =
  feedback "beginning analysis";
  let p = Ast.get () in
  let cg = computeGraph p in
  let main = Kernel.MainFunction.get () in
  let main_name =
    try
      entry_point_ref := Some (Hashtbl.find cg main);
      Some main
    with Not_found ->
      warning "no entry point available: services could be less precise. \
Use option `-main' to improve them.";
      entry_point_ref := None;
      None
  in
  let init_funcs = get_init_funcs main_name cg in
  let cg = Service.compute cg init_funcs in
  CG.mark_as_computed ();
  feedback "analysis done";
  cg

let get () = CG.memo compute

let dump () =
  let output =
    let cg = get () in
    fun o -> 
      Service_graph.frama_c_display false;
      Service.output_graph o cg
  in
  let file = Filename.get () in
  feedback ~level:2 "dumping the graph into file %s" file;
  try
    let o = open_out file in
    output o;
    close_out o
  with e ->
    error
      "error while dumping the syntactic callgraph: %s"
      (Printexc.to_string e)

let () =
  Db.register_guarded_compute
    "Syntactic_Callgraph.dump"
    (fun () -> Filename.get () = "" || CG.is_computed ())
    Db.Syntactic_Callgraph.dump
    dump

let () =
  (* Do not directly use [dump]: function in [Db] is guarded and apply only if
     required. *)
  Db.Main.extend (fun _fmt -> !Db.Syntactic_Callgraph.dump ())

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
