(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
             | NIVar (_,b) when not !b -> `Style `Dotted
             | _ -> `Style `Bold ]
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

let get_init_funcs main cg =
  match main with
  | None -> InitFunc.get ()
  | Some kf ->
    (* the entry point is always a root *)
    let init_funcs = Kernel_function.Set.add kf (InitFunc.get ()) in
    (* Add the callees of entry point as roots *)
    Kernel_function.Set.union
      (try
         let kf_name = Kernel_function.get_name kf in
         let callees = (Hashtbl.find cg kf_name).Callgraph.cnCallees in
         Datatype.Int.Hashtbl.fold
           (fun _ v acc -> match v.Callgraph.cnInfo with
           | Callgraph.NIVar (vi,_) ->
             let kf =
               try Globals.Functions.get vi
               with Not_found -> assert false
             in
             Kernel_function.Set.add kf acc
           | _ -> acc)
           callees
           Kernel_function.Set.empty
       with Not_found ->
         Kernel_function.Set.empty)
      init_funcs

let compute () =
  feedback "beginning analysis";
  let p = Ast.get () in
  let cg = computeGraph p in
  let main, _ = Globals.entry_point () in
  let main =
    try
      let name = Kernel_function.get_name main in
      entry_point_ref := Some (Hashtbl.find cg name);
      Some main
    with Not_found ->
      warning "no entry point available: services could be less precise. \
Use option `-main' to improve them.";
      entry_point_ref := None;
      None
  in
  let init_funcs = get_init_funcs main cg in
  let init_funcs =
    Kernel_function.Set.fold
      (fun kf acc -> Datatype.String.Set.add (Kernel_function.get_name kf) acc)
      init_funcs
      Datatype.String.Set.empty
  in
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
