(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
  if Parameters.TypeCheck.get () || Parameters.Files.Copy.get () then
    Ast.compute ();
  if Parameters.Obfuscate.get () then
    begin
      Parameters.CodeOutput.output "%a" obfuscate_code (Ast.get ()) ;
      raise Cmdline.Exit
    end;
  (* Printing files before anything else (in debug mode only) *)
  if Kernel.debug_atleast 1 then File.pretty ();
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
  Cil.miscState.Cil.printCilAsIs <- Kernel.debug_atleast 1;
  Mergecil.ignore_merge_conflicts := true

(* Main: let's go! *)
let () =
  Kind.version := Config.version;
  boot_cil ();
  Sys.catch_break true;
  Cmdline.catch_toplevel_run
    ~f:(fun () ->
	  Journal.set_name (Parameters.Journal.Name.get ());
	  ignore (Project.create "default");
	  Cmdline.parse_and_boot (fun () -> !Db.Toplevel.run) run_plugins)
    ~at_normal_exit:Cmdline.run_normal_exit_hook
    ~quit:true
    ~on_error:Cmdline.run_error_exit_hook

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
