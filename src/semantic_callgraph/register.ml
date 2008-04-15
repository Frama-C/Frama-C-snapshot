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

(* $Id: register.ml,v 1.11 2008/11/06 13:03:28 uid568 Exp $ *)

open Db_types
open Db


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
  SGState.memo (fun () -> 
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
       let name = "semantic callgraph"
       type t = SGraph.t
         module V = struct
           type t = Kernel_function.t
           let id v = (Kernel_function.get_vi v).Cil_types.vid
           let name = Kernel_function.get_name
           let attributes v =
             [ `Style 
                 (if Kernel_function.is_definition v then `Bold
                  else `Dotted) ]
       end
       let iter_vertex = SGraph.iter_vertex
       let callees = SGraph.pred
       let callers = SGraph.succ
     end)

module ServiceState = 
  Computation.OptionRef
    (Service.CallG.Datatype)
    (struct 
       let name = "SemanticsServicestate"
       let dependencies = 
         [SGState.self;
          Cmdline.MainFunction.self;
          Cmdline.CallgraphInitFunc.self]
     end)

let service () = 
  ServiceState.memo 
    (fun () -> Service.compute
       (callgraph ())
       (Cilutil.StringSet.add (Cmdline.MainFunction.get ()) (Cmdline.CallgraphInitFunc.get ())))

let dump () = 
  Service.output_graph stdout (service ())

let () = 
  Db.Main.extend 
    (fun _fmt -> if Cmdline.Semantic_Callgraph.Dump.get () then dump ())

let topologically_iter_on_functions =
  let module T = Graph.Topological.Make(SGraph) in
  fun f ->
    (* compute on need *)
    if SCQueue.is_empty () then T.iter SCQueue.add (callgraph ());
    SCQueue.iter f

let () =
  Options.add_plugin ~name:"Semantic callgraph" 
    ["-scg-dump",
     Arg.Unit Cmdline.Semantic_Callgraph.Dump.on,
     ": dump the semantic call graph to stdout"]
    ~descr:"Compute a semantic callgraph including function pointers";
  Db.Semantic_Callgraph.topologically_iter_on_functions :=
    topologically_iter_on_functions

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
