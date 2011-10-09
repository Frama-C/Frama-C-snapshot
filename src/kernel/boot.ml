(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

let run_plugins () =
  if Kernel.TypeCheck.get () then
    Ast.compute ();
  (* Printing files before anything else (in debug mode only) *)
  if Kernel.debug_atleast 1 then File.pretty_ast ();
  (* Syntactic constant folding before analysing files if required *)
  if Kernel.Constfold.get () then
    Cil.visitCilFileSameGlobals (Cil.constFoldVisitor true) (Ast.get ());
  try
    Dynamic.Main.apply (); (* for Helium-compatibility purpose only *)
    Db.Main.apply ();
    (* Printing code if required, have to be done at end *)
    if Kernel.PrintCode.get () then File.pretty_ast ();
  with Globals.No_such_entry_point msg ->
    Kernel.error "%s" msg

let on_from_name name f = match name with
  | None -> f ()
  | Some s ->
    try Project.on (Project.from_unique_name s) f ()
    with Not_found -> Kernel.abort "no project %S." s

let () = Db.Main.play := run_plugins

(* ************************************************************************* *)
(** Booting Frama-C *)
(* ************************************************************************* *)

(* Customisation of non-projectified CIL parameters.
   (projectified CIL parameters must be initialised with {!Cil.initCIL}). *)
let boot_cil () =
  Cil.miscState.Cil.lineDirectiveStyle <- None;
  Cil.miscState.Cil.printCilAsIs <- Kernel.debug_atleast 1;
  Mergecil.ignore_merge_conflicts := true;;

(* Main: let's go! *)
let () =
  boot_cil ();
  Sys.catch_break true;
  Cmdline.catch_toplevel_run
    ~f:(fun () ->
          Journal.set_name (Kernel.Journal.Name.get ());
          ignore (Project.create "default");
          Cmdline.parse_and_boot
            on_from_name (fun () -> !Db.Toplevel.run) run_plugins)
    ~at_normal_exit:Cmdline.run_normal_exit_hook
    ~quit:true
    ~on_error:Cmdline.run_error_exit_hook

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
