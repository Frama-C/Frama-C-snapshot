(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(**************************************************************************)

(* $Id: register.ml,v 1.59 2008/06/02 06:15:11 uid570 Exp $ *)

(* Import from Cil *)
open Cil_types
open Cil
open Cilutil
open Extlib

(* Import from Why *)
open Jc
open Jc_ast
open Jc_env
open Jc_fenv
open Jc_pervasives

(* Utility functions *)
open Common


let prolog_h_name = 
  Filename.concat Version.dataroot 
    (Filename.concat "jessie" "prolog.h")

let prolog_c_name = 
  Filename.concat Version.dataroot 
    (Filename.concat "jessie" "prolog.c")

let jessie_cpp_extra = 
  "-include " ^ prolog_h_name

let treat_jessie_prolog () =
  Cmdline.CppExtraArgs.set jessie_cpp_extra;
  File.pre_register (File.NoCPP prolog_c_name)

let options =
  [ "-jessie-analysis",
    Arg.Tuple [
      Arg.Unit Cmdline.Jessie.Analysis.on;
      Arg.Unit Cmdline.SimplifyCfg.on;
      Arg.Unit Cmdline.KeepSwitch.on;
      Arg.Unit Cmdline.Constfold.on;
      Arg.Unit Cmdline.PreprocessAnnot.on;
      Arg.Unit Cabs2cil.setDoTransformWhile;
      Arg.Unit Cabs2cil.setDoAlternateConditional;
      Arg.Unit Cabs2cil.setDoAlternateAssign;
      Arg.Unit treat_jessie_prolog;
    ],
    ": perform C to Jessie translation";

    "-jessie-project-name",
    Arg.String Cmdline.Jessie.ProjectName.set,
    "<s> : specify project name for Jessie analysis"; 

    "-jessie-gui",
    Arg.Unit Cmdline.Jessie.Gui.on,
    ": call graphical interface after Jessie analysis";

    "-jessie-int-model",
    Arg.String Cmdline.Jessie.IntModel.set,
    "<s> : set the model for integer arithmetic (exact, bounded or modulo)";

    "-jessie-gen-only",
    Arg.Unit Cmdline.Jessie.GenOnly.on,
    ": only generates jessie code (for developer use)";

    "-jc-opt",
    Arg.String Cmdline.Jessie.JcOpt.add,
    "<s> : give an option to Jc (e.g., -separation)"; 

    "-why-opt",
    Arg.String Cmdline.Jessie.WhyOpt.add,
    "<s> : give an option to Why (e.g., -fast-wp)"; 
  ]

let run () =

  (* Work in our own project, initialized by a copy of the main one. *)
  let prj = Project.create "jessie" in
  File.init_project_from_visitor prj (fun prj -> new Visitor.frama_c_copy prj);
  Project.copy ~only:(Cmdline.get_selection ()) prj;
  Project.set_current prj;

  let file = Cil_state.file () in

  (* Phase 1: various preprocessing steps before C to Jessie translation *)

  (* If main function present, remove unused parts *)
  (*
    Should be done elsewhere, so that global tables are updated accordingly.
    Should also take into account logic annotations as possible uses.

    if Globals.has_entry_point () then
      Rmtmps.removeUnusedTemps ~isRoot:Rmtmps.isCompleteProgramRoot file;
  *)

  (* Stub missing parts of the specification. Must occur before any 
   * transformation, so that stub annotations go through the same rewriting 
   * as user annotations.
   *)
  Globals.Functions.iter (ignore $ Kernel_function.get_spec);

  (* Rewrite ranges in logic annotations by comprehesion *)
  !Db.Properties.Interp.from_range_to_comprehension 
    (Cil.inplace_visit ()) (Project.current ()) file;
  if checking then check_types file;

  (* Phase 2: C-level rewriting to facilitate analysis *)

  Rewrite.rewrite file;
  if checking then check_types file;

  if Cmdline.Debug.get () >= 1 then print_to_stdout file;

  (* Phase 3: Caduceus-like normalization that rewrites C-level AST into
   * Jessie-like AST, still in Cil (in order to use Cil visitors)
   *)

  Norm.normalize file;
  Retype.retype file;
  if checking then check_types file;

  (* Phase 4: various postprocessing steps, still on Cil AST *)

  (* Rewrite ranges back in logic annotations *)
  !Db.Properties.Interp.from_comprehension_to_range
    (Cil.inplace_visit ()) (Project.current ()) file;

  if Cmdline.Debug.get () >= 1 then print_to_stdout file;

  (* Phase 5: C to Jessie translation, should be quite straighforward at this
   * stage (after normalization)
   *)

  let pragmas = Interp.pragmas file in
  let pfile = Interp.file file in

  (* Phase 6: pretty-printing of Jessie program *)

  let sys_command cmd = 
    if Sys.command cmd <> 0 then
      begin
	Format.eprintf "Failed to run: %s@." cmd;
	exit 1
      end
  in

  let default_project_name () =
    let filename = File.name (last (File.get_all ())) in
    Filename.chop_extension (Filename.basename filename)
  in

  let projname = Cmdline.Jessie.ProjectName.get () in
  let projname = if projname = "" then default_project_name () else projname in

  let filename = projname ^ ".jc" in
  let fmt =
    let c = open_out filename in
    let fmt = Format.formatter_of_out_channel c in
    at_exit (fun () -> Format.pp_print_flush fmt (); close_out c);
    fmt
  in
  Jc_output.print_decls fmt pragmas;
  Jc_poutput.pdecls fmt pfile;

  (* Phase 7: produce source-to-source correspondance file *)

  let locname = projname ^ ".cloc" in
  Pp.print_in_file Output.print_locs locname;

  if Cmdline.Jessie.GenOnly.get () then () else

    (* Phase 8: call Jessie to Why translation *)

    let why_opt =
      StringSet.fold (fun opt acc -> " -why-opt " ^ opt ^ " " ^ acc) 
	(Cmdline.Jessie.WhyOpt.get ()) ""
    in
    let jc_opt =
      StringSet.fold (fun opt acc -> " " ^ opt ^ " " ^ acc) 
	(Cmdline.Jessie.JcOpt.get ()) ""
    in
    let debug_opt = if Cmdline.Debug.get () >= 1 then " -d " else " " in
    sys_command (
      "jessie -why-opt -split-user-conj " 
      ^ why_opt ^ jc_opt ^ debug_opt ^ " -locs " ^ locname ^ " " ^ filename);
    
    (* Phase 9: call Why to VP translation *)

    let makefile = projname ^ ".makefile" in
    if Cmdline.Jessie.Gui.get () then
      sys_command ("make -f " ^ makefile ^ " gui")

let run_and_catch_error () =
  try run () with Errormsg.Error -> ()

let () =
  Db.Jessie.run_analysis := run_and_catch_error;
  Options.add_plugin
    ~name:"C to Jessie (experimental)"
    ~descr:"translation to Jessie"
    options

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
