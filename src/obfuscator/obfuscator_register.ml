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

let disable_other_analyzers () =
  if Options.Run.get () then
    let selection =
      State_selection.Static.diff
        (Parameter_state.get_selection ())
        (State_selection.Static.union
           (State_selection.of_list
	      (Kernel.CodeOutput.self :: Options.states))
           (* The command-line options that govern the creation of the AST
              must be preserved *)
           (State_selection.Static.with_codependencies Ast.self))
    in
    Project.clear ~selection ()

let () = Cmdline.run_after_configuring_stage disable_other_analyzers

let force_run () =
  if not (Dictionary.is_computed ()) then begin
    let old_printer = Printer.printer () in
    Obfuscate.obfuscate ();
    if Options.Dictionary.is_default () then Log.print_delayed Dictionary.pretty
    else begin
      let file = Options.Dictionary.get () in
      try
	let cout = open_out file in
	let fmt = Format.formatter_of_out_channel cout in
	Dictionary.pretty fmt
      with Sys_error _ as exn ->
	Options.error
	  "@[cannot generate the dictionary into file `%s':@ %s@]" 
	  file
	  (Printexc.to_string exn)
    end;
    File.pretty_ast ();
    Printer.change_printer (fun () -> old_printer)
  end

let force_run =
  Dynamic.register
    ~plugin:"Obfuscator"
    "force_run"
    (Datatype.func Datatype.unit Datatype.unit)
    ~journalize:true
    force_run

let run () = 
  if Options.Run.get () then begin 
    force_run ();
    Options.Run.off () (* de-activate yourself to allow the other analyzers to
			  run from now *)
  end

let () = Db.Main.extend run

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
