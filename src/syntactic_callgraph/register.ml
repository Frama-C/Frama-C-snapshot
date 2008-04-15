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
open Cil
open Callgraph

let name = "syntactic callgraph"

module CG =
  Computation.OptionRef
    (Project.Datatype.Imperative
       (struct
	  type t = Callgraph.callgraph
	  let copy _cg = assert false (* TODO *)
	  let name = name
	end))
    (struct
       let name = name
       let dependencies = [ Cil_state.self ]
     end)

let get () = CG.memo (fun () -> let p = Cil_state.file () in computeGraph p)

module Service =
  Service_graph.Make
    (struct
       let name = "syntactic callgraph"
       type t = Callgraph.callgraph
       module V = struct
         type t = callnode
         let id v = v.cnid
         let name v = nodeName v.cnInfo
         let attributes v =
           [ match v.cnInfo with
             | NIVar (_,b) when not !b -> `Style `Dotted
             | _ -> `Style `Bold ]
       end
       let iter_vertex f = Hashtbl.iter (fun _ -> f)
       let callees _g v = 
	 Inthash.fold (fun _ v' acc -> v' :: acc) v.cnCallees []
       let callers _g v = 
	 Inthash.fold (fun _ v' acc -> v' :: acc) v.cnCallers []
     end)

let dump () = 
  try 
    let file = Cmdline.CallgraphFilename.get () in
    let cg = get () in
    let init_funcs = (* entry point is always a root *)
      Cilutil.StringSet.add 
	(Cmdline.MainFunction.get ()) (Cmdline.CallgraphInitFunc.get ()) 
    in
    let strat_cg = Service.compute cg init_funcs in
    let o = open_out file in
    Service.output_graph o strat_cg;
    close_out o
  with e ->
    Format.eprintf "dump_callgraph: %s@." (Printexc.to_string e)
    
let () = 
  Db.register_guarded_compute
    "Syntactic_callgraph.dump"
    (fun () -> Cmdline.CallgraphFilename.get () = "")
    Db.Syntactic_callgraph.dump
    dump

let () = 
  (* Do not directly use [dump]: function in [Db] is guarded and apply only if
     required. *)
  Db.Main.extend (fun _fmt -> !Db.Syntactic_callgraph.dump ())

let () =
  Options.add_plugin 
    ~name:"callgraph" ~shortname:"cg"
    ~descr:"syntactic stratified callgraph"
    [ "-cg",
      Arg.String Cmdline.CallgraphFilename.set,
      "FILENAME: dump a stratified call graph to FILENAME in dot format";
      "-cg-init-func",
      Arg.String Cmdline.CallgraphInitFunc.add,
      "FUNCTION: use this function as a root service (you can add as many functions as you want; if no function is declared, then root services are initialized with functions with no callers)" ;
    ]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
