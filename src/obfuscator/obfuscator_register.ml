(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

include Plugin.Register
  (struct
     let name = "obfuscator"
     let shortname = "obfuscator"
     let help = "objuscator for confidential code"
   end)

module Run =
  False
    (struct
       let option_name = "-obfuscate"
       let help = "print an obfuscated version of the input files and exit.\n\
Disable any other Frama-C analysis."
     end)

let disable_other_analyzers () =
  if Run.get () then
    let selection =
      State_selection.Static.diff
        (Plugin.get_selection ())
        (State_selection.Static.union
           (State_selection.singleton Run.self)
           (* The command-line options that govern the creation of the AST
              must be preserved *)
           (State_selection.Static.with_codependencies Ast.self))
    in
    Project.clear ~selection ()

let () = Cmdline.run_after_configuring_stage disable_other_analyzers

let obfuscate_code code_fmt =
  let ast = Ast.get () in
  let dictionary = Obfuscate.obfuscate ast in
  Format.fprintf code_fmt "// Start of dictionary for obfuscation:@\n";
  let sorted_dictionary = Hashtbl.fold Datatype.String.Map.add
    dictionary Datatype.String.Map.empty in
  Datatype.String.Map.iter
    (fun k v -> Format.fprintf code_fmt "#define %s %s@\n" k v)
    sorted_dictionary;
  Format.fprintf code_fmt "// End of dictionary for obfuscation.@\n";
  Format.fprintf code_fmt "@[%a@]" Printer.pp_file ast

let force_run () = Kernel.CodeOutput.output obfuscate_code

let force_run =
  Dynamic.register
    ~plugin:"Obfuscator"
    "force_run"
    (Datatype.func Datatype.unit Datatype.unit)
    ~journalize:true
    force_run

let run () =
  if Run.get () then begin
    force_run ();
    raise Cmdline.Exit
  end

let () = Db.Main.extend run

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
