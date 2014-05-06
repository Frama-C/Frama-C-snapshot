(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
  with Globals.No_such_entry_point msg ->
    Kernel.abort "%s" msg

let on_from_name name f = match name with
  | None -> f ()
  | Some s ->
    try Project.on (Project.from_unique_name s) f ()
    with Not_found -> Kernel.abort "no project %S." s

let () = Db.Main.play := run_plugins

(* ************************************************************************* *)
(** Booting Frama-C *)
(* ************************************************************************* *)

(* Customisation of pretty-printers' parameters. *)
let boot_cil () =
  Cil.miscState.Cil.lineDirectiveStyle <- None;
  Cil.miscState.Cil.printCilAsIs <- Kernel.debug_atleast 1;
  Cil_printer.state.Printer_api.line_directive_style <- None;
  Cil_printer.state.Printer_api.print_cil_as_is <- Kernel.debug_atleast 1

(* Main: let's go! *)
let () =
  boot_cil ();
  Sys.catch_break true;
  Cmdline.catch_toplevel_run
    ~f:(fun () ->
          ignore (Project.create "default");
          Cmdline.parse_and_boot
            on_from_name (fun () -> !Db.Toplevel.run) run_plugins)
    ~at_normal_exit:Cmdline.run_normal_exit_hook
    ~quit:true
    ~on_error:Cmdline.run_error_exit_hook;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
