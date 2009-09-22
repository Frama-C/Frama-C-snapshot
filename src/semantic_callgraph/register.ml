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

(* $Id: register.ml,v 1.11 2008-11-06 13:03:28 uid568 Exp $ *)

open Db_types
open Db
open Options

module SGraph =
  Graph.Imperative.Digraph.ConcreteLabeled
    (Kernel_function)
    (struct include Cilutil.StmtComparable let default = Cil.dummyStmt end)

module SGState = 
  Computation.OptionRef
    (Project.Datatype.Imperative(struct include SGraph let name="sgraph" end))
    (struct let name = "SGState" let dependencies = [Value.self] end)

module SCQueue =
  Computation.Queue
    (Kernel_function.Datatype)
    (struct let name = "SCQueue" let dependencies = [ SGState.self ] end)

let callgraph () =
  SGState.memo
    (fun () -> 
       let g = SGraph.create () in
       !Value.compute ();
       Globals.Functions.iter
         (fun kf ->
            if !Value.is_called kf then SGraph.add_vertex g kf;
            List.iter
	      (fun (caller,call_sites) ->
	         List.iter
	           (fun call_site -> SGraph.add_edge_e g (kf,call_site,caller))
	           call_sites)
	      (!Value.callers kf));
       g)
    
module Service =
  Service_graph.Make
    (struct
       let datatype_name = name
       type t = SGraph.t
       module V = struct
         type t = Kernel_function.t
         let id v = (Kernel_function.get_vi v).Cil_types.vid
         let name = Kernel_function.get_name
         let attributes v =
           [ `Style 
               (if Kernel_function.is_definition v then `Bold
                else `Dotted) ]
	 let equal = Kernel_function.equal
	 let hash = Kernel_function.hash
       end
       let iter_vertex = SGraph.iter_vertex
       let iter_succ = SGraph.iter_succ
       let iter_pred = SGraph.iter_pred
       let fold_pred = SGraph.fold_pred
       let in_degree = SGraph.in_degree
     end)

module ServiceState = 
  Computation.OptionRef
    (Service.CallG.Datatype)
    (struct 
       let name = "SemanticsServicestate"
       let dependencies = 
         [ SGState.self; Parameters.MainFunction.self; InitFunc.self ]
     end)

let get_init_funcs () =
  let entry_point_name = Parameters.MainFunction.get () in
  let init_funcs = 
    (* entry point is always a root *)
    Cilutil.StringSet.add entry_point_name (InitFunc.get ()) 
  in
  (* Add the callees of entry point as roots *)
  let callees =
    let kf = fst (Globals.entry_point ()) in
    !Db.Users.get kf 
  in
  Kernel_function.Set.fold
    (fun kf acc -> Cilutil.StringSet.add (Kernel_function.get_name kf) acc)
    callees
    init_funcs

let compute () = 
  feedback "beginning analysis";
  let cg = 
    Service.compute
      (callgraph ())
      (get_init_funcs ())
  in
  feedback "analysis done";
  ServiceState.mark_as_computed ();
  cg

let get () = ServiceState.memo compute

let dump () =
  let cg = get () in
  let file = Filename.get () in
  feedback ~level:2 "dumping the graph into file %s" file;
  try
    let o = open_out file in
    Service.output_graph o cg;
    close_out o
  with e ->
    error
      "error while dumping the semantic callgraph: %s"
      (Printexc.to_string e)
    
let () = 
  Db.register_guarded_compute
    "Semantic_Callgraph.dump"
    (fun () -> Filename.get () = "" || ServiceState.is_computed ())
    Db.Semantic_Callgraph.dump
    dump

let () = 
  (* Do not directly use [dump]: function in [Db] is guarded and apply only if
     required. *)
  Db.Main.extend (fun _fmt -> !Db.Semantic_Callgraph.dump ())

let topologically_iter_on_functions =
  let module T = Graph.Topological.Make(SGraph) in
  fun f ->
    (* compute on need *)
    if SCQueue.is_empty () then T.iter SCQueue.add (callgraph ());
    SCQueue.iter f

let () =
  Db.Semantic_Callgraph.topologically_iter_on_functions :=
    topologically_iter_on_functions

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
