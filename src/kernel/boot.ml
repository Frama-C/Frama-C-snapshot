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

(* $Id: boot.ml,v 1.33 2008/11/18 12:13:41 uid568 Exp $ *)

(** Frama-C Entry Point (last linked module).
    @plugin development guide *)

(* ************************************************************************* *)
(** Registering options for debugging project *)
(* ************************************************************************* *)

let dump_dependencies () =
  let _except =
    Project.Selection.remove Cmdline.Machdep.self
      (Project.Selection.remove Cmdline.MainFunction.self
	 (Project.Selection.remove Cmdline.LibEntry.self
	    (Cmdline.get_selection ())))
  in
  Project.Computation.dump_dependencies (*~except*) 
    "computation_dependencies.dot"

let project_debug =
    [ "-debug", 
      Arg.Int Project.set_debug_level,      
      "Level of debug for project";
      
      "-dump", Arg.Unit dump_dependencies, "Dump dependencies" ]

let () = Options.add_cmdline ~name:"project" ~debug:project_debug []

(* Journal options *)
let () = 
  Options.add_cmdline 
    ~name:"journalization"
    [ "-journal-disable",
      Arg.Unit Cmdline.Journal.Disable.on,
      ": do not output any journal";
      "-journal-name",
      Arg.String Cmdline.Journal.Name.set,
      ": set the filename of the journal (do not write any extension)";
    ]

(* ************************************************************************* *)
(** Adding some internal state dependencies *)
(* ************************************************************************* *)

let () =
  Project.Computation.add_dependency Alarms.self Db.Value.self;
  Binary_cache.MemoryFootprint.depend Cmdline.MemoryFootprint.self;
  Buckx.MemoryFootprint.depend Cmdline.MemoryFootprint.self;
  List.iter 
    Cil_state.depend 
    [ Cmdline.SimplifyCfg.self;
      Cmdline.KeepSwitch.self;
      Cmdline.UnrollingLevel.self;
      Cmdline.Constfold.self;
      Cmdline.ReadAnnot.self;
      Cmdline.PreprocessAnnot.self ]

(* ************************************************************************* *)
(** Booting Frama-C *)
(* ************************************************************************* *)

(* Customisation of non-projectified CIL parameters.
   (projectified CIL parameters must be initialised with {!Cil.initCIL}). *)
let boot_cil () =
  Cabs2cil.forceRLArgEval := false;
  Cil.miscState.Cil.lineDirectiveStyle <- None;
  (*  Cil.lineDirectiveStyle := Some LinePreprocessorInput;*)
  Cil.miscState.Cil.printCilAsIs <- Cmdline.Debug.get () > 0;
  Mergecil.ignore_merge_conflicts := true;
  Pretty.flushOften := true

(* Main: let's go! *)
let () =
  File.cxx_suffixes := Db.Cxx.suffixes;
  Kind.version := Version.version;
  boot_cil ();
  Sys.catch_break true;
  !Dynamic.include_all_module ();
  Journal.start ();
  at_exit Journal.write;
  Options.parse_cmdline ();
  if Cmdline.Journal.Disable.get () then begin
    Journal.clear ~restart:false ();
    Journal.stop ()
  end;
  if Cmdline.Journal.Name.is_set () then 
    Journal.set_name (Cmdline.Journal.Name.get ());
  Options.initialize_toplevels ()

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
