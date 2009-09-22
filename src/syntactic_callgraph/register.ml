(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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
open Cil
open Callgraph
open Options

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
	 let hash = id
       end
       let iter_vertex f = Hashtbl.iter (fun _ -> f)
       let iter_succ f _g v = Inthash.iter (fun _ -> f) v.cnCallees
       let iter_pred f _g v = Inthash.iter (fun _ -> f) v.cnCallers
       let fold_pred f _g v = Inthash.fold (fun _ -> f) v.cnCallers
       let in_degree g v = fold_pred (fun _ -> succ) g v 0
     end)

module CG =
  Computation.OptionRef
    (Service.CallG.Datatype)
    (struct
       let name = name
       let dependencies = [ Ast.self ]
     end)

let get_init_funcs cg =
  let entry_point_name = Parameters.MainFunction.get () in
  let init_funcs = (* entry point is always a root *)
    Cilutil.StringSet.add entry_point_name (InitFunc.get ()) 
  in
  (* Add the callees of entry point as roots *)
  Cilutil.StringSet.union 
    (try 
       let callees = 
	 (Hashtbl.find cg entry_point_name).Callgraph.cnCallees
       in
       Inthash.fold (fun _ v acc -> match v.Callgraph.cnInfo with
		     | Callgraph.NIVar ({vname=n},_) -> 
			 Cilutil.StringSet.add n acc
		     | _ -> acc)
	 callees Cilutil.StringSet.empty
     with Not_found -> Cilutil.StringSet.empty)
    init_funcs

let compute () =
  feedback "beginning analysis";
  let p = Ast.get () in 
  let cg = computeGraph p in
  let init_funcs = get_init_funcs cg in
  let cg = Service.compute cg init_funcs in
  CG.mark_as_computed ();
  feedback "analysis done";
  cg

let get () = CG.memo compute
(*
module SG =
  Computation.OptionRef
    (Service.SG.Datatype)
    (struct
       let name = name ^ " (service only)"
       let dependencies = [ CG.self; ServicesOnly.self ]
     end)

let get_services () = SG.memo (fun () -> Service.compute_services (get ()))
*)
let dump () = 
  let output =
(*    if ServicesOnly.get () then
      let sg = get_services () in
      fun o -> Service.output_services o sg
    else *)
      let cg = get () in
      fun o -> Service.output_graph o cg
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
compile-command: "LC_ALL=C make -C ../.."
End:
*)
