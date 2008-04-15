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

(** Frama-C Main.
    @plugin development guide *)

let load_dynamic_plugins () =
  Dynamic.trace ();
  Cmdline.Dynamic.AddPath.iter Dynamic.add_path ;
  if Cmdline.Dynamic.AddPath.is_set () then !Dynamic.include_all_module ();
  Cmdline.Dynamic.LoadModule.iter Dynamic.include_module

let obfuscate_code code_fmt files =
  let dictionary = Obfuscate.obfuscate files in
  Format.fprintf code_fmt "// Start of dictionary for obfuscation:@\n";
  Hashtbl.iter
    (fun k v -> Format.fprintf code_fmt "#define %s %s@\n" k v)
    dictionary;
  Format.fprintf code_fmt "// End of dictionary for obfuscation.@\n";
  Format.fprintf code_fmt "@[%a@]" (Cil.d_file (new Printer.print())) files
   
let all_plugins fmt =
  let files = Cil_state.file () in
  let code_fmt = Cmdline.CodeOutput.get_fmt () in
  load_dynamic_plugins ();
  if Cmdline.Obfuscate.get () then begin
    obfuscate_code code_fmt files;
    exit 0
  end;
  (* Printing files before anything else (in debug mode only) *)
  if Cmdline.Debug.get () > 0 then
    Format.fprintf code_fmt "@[%a@]" (Cil.d_file (new Printer.print())) files;
  (* Syntactic constant folding before analysing files if required *)
  if Cmdline.Constfold.get () then 
    Cil.visitCilFileSameGlobals (Cil.constFoldVisitor true) files;
  try 
    Dynamic.Main.apply (); (* for Helium-compatibility purpose only *)
    Db.Main.apply fmt;
    (* Printing code if required, have to be done at end *)
    if Cmdline.PrintCode.get () then
      Format.fprintf code_fmt "@[%a@]" 
	(Cil.d_file (new Printer.print ())) files;
    Cilutil.flush_all ();
  with Globals.No_such_entry_point msg -> (* Db.entry_point doesn't work *)
    Format.eprintf "%s@." msg

let global_toplevel_startup_hook () =
  try
    let ignore_files = Options.init_from_options () in
    if not ignore_files then begin
      try Cil_state.compute ()
      with Cil_state.Bad_Initialisation _ -> assert false
    end;
    all_plugins Format.std_formatter
  with Failure "" (*Sys.Break*) as e ->
    Cil.warn "user interrupted computations. \
Dumping whatever we may have at this point.";
    if Cmdline.ForceValues.get ()
      || Cmdline.ForceDeps.get ()
      || Cmdline.ForceOut.get () then
	Globals.Functions.iter
          (fun kf -> 
	     if Kernel_function.is_definition kf then begin
	       if Cmdline.ForceOut.get () then
		 !Db.Outputs.display Format.std_formatter kf;
	       if Cmdline.ForceValues.get () then
		 Db.Value.display Format.std_formatter kf;
             end);
    raise e

let () =
  Options.add_plugin
    ~name:"Command Line Interface"
    ~descr:"used for batch processing"
    ~toplevel_init:global_toplevel_startup_hook
    [];
  Db.Toplevel.replay := global_toplevel_startup_hook;
  Db.Toplevel.run_all_plugins := all_plugins;

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
