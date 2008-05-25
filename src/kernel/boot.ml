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

(* $Id: boot.ml,v 1.17 2008/04/01 09:25:20 uid568 Exp $ *)

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

(* CIL initialization and customization *)
let boot_cil () =
  Cil.initCIL ();
  Cabs2cil.forceRLArgEval := false;
  Cil.lineDirectiveStyle := None;
  (*  Cil.lineDirectiveStyle := Some LinePreprocessorInput;*)
  Cil.printCilAsIs := Cmdline.Debug.get () > 0;
  Mergecil.ignore_merge_conflicts := true;
  Cil.useLogicalOperators := false; (* do not use lazy LAND and LOR *)
  Pretty.flushOften := true

(* Additional internal state dependencies *)
let () =
  Project.Computation.add_dependency Alarms.self Db.Value.self;
  Binary_cache.MemoryFootprint.depend Cmdline.MemoryFootprint.self;
  Buckx.MemoryFootprint.depend Cmdline.MemoryFootprint.self

(* Main: let's go! *)
let () =
  Kind.version := Version.version;
  boot_cil ();
  Sys.catch_break true;
  Options.parse_cmdline ();
  Options.initialize_toplevels ()

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
