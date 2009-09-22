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

(* $Id: boot.ml,v 1.33 2008-11-18 12:13:41 uid568 Exp $ *)

(** Frama-C Entry Point (last linked module).
    @plugin development guide *)

let obfuscate_code code_fmt ast =
  let dictionary = Obfuscate.obfuscate ast in
  Format.fprintf code_fmt "// Start of dictionary for obfuscation:@\n";
  Hashtbl.iter
    (fun k v -> Format.fprintf code_fmt "#define %s %s@\n" k v)
    dictionary;
  Format.fprintf code_fmt "// End of dictionary for obfuscation.@\n";
  Format.fprintf code_fmt "@[%a@]" (Cil.d_file (new Printer.print())) ast

let run_plugins () =
  if Parameters.TypeCheck.get () then Ast.compute ();
  if Parameters.Obfuscate.get () then 
    begin
      Parameters.CodeOutput.output "%a" obfuscate_code (Ast.get ()) ;
      raise Cmdline.Exit
    end;
  (* Printing files before anything else (in debug mode only) *)
  if Parameters.Debug.get () > 0 then File.pretty ();
  (* Syntactic constant folding before analysing files if required *)
  if Parameters.Constfold.get () then
    Cil.visitCilFileSameGlobals (Cil.constFoldVisitor true) (Ast.get ());
  try
    Dynamic.Main.apply (); (* for Helium-compatibility purpose only *)
    Db.Main.apply ();
    (* Printing code if required, have to be done at end *)
    if Parameters.PrintCode.get () then File.pretty ();
  with Globals.No_such_entry_point msg ->
    Kernel.error "%s" msg

let () = Db.Main.play := run_plugins

(* ************************************************************************* *)
(** Booting Frama-C *)
(* ************************************************************************* *)

(* Customisation of non-projectified CIL parameters.
   (projectified CIL parameters must be initialised with {!Cil.initCIL}). *)
let boot_cil () =
  Cabs2cil.forceRLArgEval := false;
  Cil.miscState.Cil.lineDirectiveStyle <- None;
  (*  Cil.lineDirectiveStyle := Some LinePreprocessorInput;*)
  Cil.miscState.Cil.printCilAsIs <- Parameters.Debug.get () > 0;
  Mergecil.ignore_merge_conflicts := true

(* Main: let's go! *)
let () =
  Kind.version := Config.version;
  boot_cil ();
  Sys.catch_break true;
  Cmdline.catch_toplevel_run
    (fun () ->
       Journal.set_name (Parameters.Journal.Name.get ());
       ignore (Project.create "default");
       Cmdline.parse_and_boot (fun () -> !Db.Toplevel.run) run_plugins)
    (fun () ->
       Plugin.run_normal_exit_hook ();
       exit 0 (* ensure that nothing occurs after booting: no other file can be
		 linked after boot.ml *))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
