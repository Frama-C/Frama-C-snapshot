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

(* $Id: boot.ml,v 1.19 2008/06/02 14:54:23 uid528 Exp $ *)

(** Frama-C Entry Point (last linked module).
    @plugin developer guide *)

(** Register options for debugging project *)

let dump_dependencies () =
  let except =
    Project.Selection.remove Cmdline.MainFunction.self
      (Project.Selection.remove Cmdline.LibEntry.self
	 (Cmdline.get_selection ()))
  in
  Project.Computation.dump_dependencies ~except "computation_dependencies.dot";
  Project.Datatype.dump_dependencies "datatype_dependencies.dot"

let project_debug =
    [ "-debug", 
      Arg.Int Project.set_debug_level,      
      "Level of debug for project";
      
      "-dump", Arg.Unit dump_dependencies, "Dump dependencies" ]

let () = Options.add_cmdline ~name:"project" ~debug:project_debug []

(** Boot Frama-C *)

(* Additional internal state dependencies *)
let () =
  Project.Computation.add_dependency Alarms.self Db.Value.self;
  Binary_cache.MemoryFootprint.depend Cmdline.MemoryFootprint.self;
  Buckx.MemoryFootprint.depend Cmdline.MemoryFootprint.self

(* Main: let's go! *)
let () =
  Kind.version := Version.version;
  Sys.catch_break true;
  Options.parse_cmdline ();
  Options.initialize_toplevels ()

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
