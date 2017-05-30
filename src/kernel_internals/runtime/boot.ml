(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

let play_analysis () =
  if Kernel.TypeCheck.get () then begin
    if Kernel.Files.get () <> [] || Kernel.TypeCheck.is_set () then begin
      Ast.compute ();
      (* Printing files before anything else (in debug mode only) *)
      if Kernel.debug_atleast 1 &&
        Kernel.Debug_category.exists (fun s -> s = "ast")
      then File.pretty_ast ()
    end
  end;
  try
    Db.Main.apply ();
    (* Printing code if required, have to be done at end *)
    if Kernel.PrintCode.get () then File.pretty_ast ();
    (* Easier to handle option -set-project-as-default at the last moment:
       no need to worry about nested [Project.on] *)
    Project.set_keep_current (Kernel.Set_project_as_default.get ());
    (* unset Kernel.Set_project_as_default, but only if it set.
       This avoids disturbing the "set by user" flag. *)
    if Kernel.Set_project_as_default.get () then
      Kernel.Set_project_as_default.off ()
  with Globals.No_such_entry_point msg ->
    Kernel.abort "%s" msg

let on_from_name name f =
  try Project.on (Project.from_unique_name name) f ()
  with Project.Unknown_project -> Kernel.abort "no project `%s'." name

let () = Db.Main.play := play_analysis

(* ************************************************************************* *)
(** Booting Frama-C *)
(* ************************************************************************* *)

(* Main: let's go! *)
let () =
  Cil_printer.state.Printer_api.print_cil_as_is <- Kernel.debug_atleast 1;
  Sys.catch_break true;
  let f () =
    ignore (Project.create "default");
    let on_from_name = { Cmdline.on_from_name } in
    Cmdline.parse_and_boot
      ~on_from_name
      ~get_toplevel:(fun () -> !Db.Toplevel.run)
      ~play_analysis
  in
  Cmdline.catch_toplevel_run
    ~f
    ~at_normal_exit:Cmdline.run_normal_exit_hook
    ~on_error:Cmdline.run_error_exit_hook;

(* Implicit exit 0 if we haven't exited yet *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
