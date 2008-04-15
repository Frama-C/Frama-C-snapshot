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

(* $Id: register.ml,v 1.97 2008/11/24 13:01:41 uid570 Exp $ *)

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


let std_include = Filename.concat Version.dataroot "jessie"

let prolog_h_name = Filename.concat std_include "jessie_prolog.h"

let treat_jessie_prolog () =
  Cmdline.CppExtraArgs.add ("-include " ^ prolog_h_name)

let treat_jessie_std_headers () =
  Cmdline.CppExtraArgs.add ("-I " ^ std_include)

let treat_integer_model () =
  if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
    Cmdline.CppExtraArgs.add ("-D JESSIE_EXACT_INT_MODEL")

let treat_jessie_no_prolog () =
  Cmdline.CppExtraArgs.add ("-D JESSIE_NO_PROLOG")

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
    Arg.Tuple [
      Arg.String Cmdline.Jessie.IntModel.set;
      Arg.Unit treat_integer_model;
    ],
    "<s> : set the model for integer arithmetic (exact, bounded or modulo)";

    "-jessie-behavior",
    Arg.String Cmdline.Jessie.Behavior.set,
    "<s> : restrict verification to a specific behavior \
(safety, default or a user-defined behavior)";

    "-jessie-gen-only",
    Arg.Unit Cmdline.Jessie.GenOnly.on,
    ": only generates jessie code (for developer use)";

    "-jessie-gen-goals",
    Arg.Unit Cmdline.Jessie.GenGoals.on,
    ": generates WHY goals instead of calling an automatic prover";

    "-jessie-no-regions",
    Arg.Unit Cmdline.Jessie.SepRegions.off,
    ": do not separate memory into regions (for developer use)";

    "-jessie-no-prolog",
    Arg.Unit treat_jessie_no_prolog,
    ": do not include Jessie prolog (for developer use)";

    "-jessie-std-stubs",
    Arg.Tuple [
      Arg.Unit Cmdline.Jessie.StdStubs.on;
      Arg.Unit treat_jessie_std_headers;
    ],
    ": use annotated standard headers";

    "-jessie-hint-level",
    Arg.Int Cmdline.Jessie.HintLevel.set,
    "<i>: level of hints, i.e. assertions to help the proof (e.g. for string usage)";

    "-jessie-infer-annot",
    Arg.String Cmdline.Jessie.InferAnnot.set,
    "<s> : infer function annotations (inv, pre, spre, wpre)";

    "-jessie-abstract-domain",
    Arg.String Cmdline.Jessie.AbsDomain.set,
    "<s> : use specified abstract domain (box, oct or poly)";

    "-jessie-atp",
    Arg.String Cmdline.Jessie.Atp.set,
    "<s> : use specified automated theorem prover \
(alt-ergo, cvcl, harvey, simplify, yices, z3, zenon)";

    "-jessie-cpu-limit",
    Arg.Int Cmdline.Jessie.CpuLimit.set,
    "<i> : set the time limit in sec. for the analysis";

    "-jc-opt",
    Arg.String Cmdline.Jessie.JcOpt.add,
    "<s> : give an option to Jc (e.g., -trust-ai)";

    "-why-opt",
    Arg.String Cmdline.Jessie.WhyOpt.add,
    "<s> : give an option to Why (e.g., -fast-wp)";
  ]


let apply_if_dir_exist name f =
  try
    let d = Unix.opendir name in
    Unix.closedir d;
    f name
  with Unix.Unix_error (Unix.ENOENT, "opendir",_) -> ()

(** Environment variables for Jessie and Why when using the Jessie plugin *)
let () =
  let whylib = String.escaped (Filename.concat Version.dataroot "why") in
  apply_if_dir_exist whylib (Unix.putenv "WHYLIB")


let run () =
  Format.printf "Starting Jessie translation@.";
  (* Work in our own project, initialized by a copy of the main one. *)
  let prj = Project.create "jessie" in
  File.init_project_from_visitor prj (fun prj -> new Visitor.frama_c_copy prj);
  Project.copy ~only:(Cmdline.get_selection ()) prj;
  Project.set_current prj;

  let file = Cil_state.file () in

  try
  if file.globals = [] then
    (Format.eprintf
       "Jessie: nothing to process. There was probably an error before.@.";
    raise Exit);
  (* Phase 1: various preprocessing steps before C to Jessie translation *)

  (* Enforce the prototype of malloc to exist before visiting anything.
   * It might be useful for allocation pointers from arrays
   *)
  ignore (Common.malloc_function ());
  ignore (Common.free_function ());

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
      (Format.eprintf "Jessie subprocess failed: %s@." cmd; raise Exit)
  in

  let projname = Cmdline.Jessie.ProjectName.get () in
  let projname =
    if projname <> "" then projname else
      match Cmdline.Files.get() with
	| [f] ->
	    (try
	       Filename.chop_extension f
	     with Invalid_argument _ -> f)
	| _ ->
	    "wholeprogram"
  in
  (* if called on 'path/file.c', projname is 'path/file' *)
  (* jessie_subdir is 'path/file.jessie' *)
  let jessie_subdir = projname ^ ".jessie" in
  Lib.mkdir_p jessie_subdir;
  Format.eprintf "Producing Jessie files in subdir %s@." jessie_subdir;

  (* basename is 'file' *)
  let basename = Filename.basename projname in

  (* filename is 'file.jc' *)
  let filename = basename ^ ".jc" in
  let () = Pp.print_in_file
    (fun fmt ->
       Jc_output.print_decls fmt pragmas;
       Format.fprintf fmt "%a@." Jc_poutput.pdecls pfile)
    (Filename.concat jessie_subdir filename)
  in
  Format.eprintf "File %s/%s written.@." jessie_subdir filename;

  (* Phase 7: produce source-to-source correspondance file *)

  (* locname is 'file.cloc' *)
  let locname = basename ^ ".cloc" in
  Pp.print_in_file Output.print_pos (Filename.concat jessie_subdir locname);
  Format.eprintf "File %s/%s written.@." jessie_subdir locname;

  if Cmdline.Jessie.GenOnly.get () then () else

    (* Phase 8: call Jessie to Why translation *)

    let why_opt =
      Cmdline.Jessie.WhyOpt.fold
	(fun opt acc -> " -why-opt " ^ opt ^ " " ^ acc) ""
    in
    let jc_opt =
      StringSet.fold (fun opt acc -> " " ^ opt ^ " " ^ acc)
	(Cmdline.Jessie.JcOpt.get ()) ""
    in
    let debug_opt = if Cmdline.Debug.get () >= 1 then " -d " else " " in
    let behav_opt =
      if Cmdline.Jessie.Behavior.get () <> "" then
	"-behavior " ^ Cmdline.Jessie.Behavior.get ()
      else ""
    in
    let verbose_opt = if Cmdline.Quiet.get () then " " else " -v " in
    let env_opt =
      if Cmdline.Debug.get () >= 1 then
	"OCAMLRUNPARAM=bt"
      else ""
    in
    let jessie_cmd =
      if Cmdline.Debug.get () >= 1 then
	" OCAMLRUNPARAM=bt jessie.byte "
      else
	" jessie "
    in
    let cpulimit_opt =
      if Cmdline.Jessie.CpuLimit.get () <> 0 then
	"why-cpulimit " ^ (string_of_int (Cmdline.Jessie.CpuLimit.get ()))
      else ""
    in
    let rec make_command = function
      | [] -> ""
      | [ a ] -> a
      | a::cmd -> a ^ " " ^ make_command cmd
    in
    Format.eprintf "Calling Jessie tool in subdir %s@." jessie_subdir;
    Sys.chdir jessie_subdir;


    let cmd =
      make_command
	[ env_opt; cpulimit_opt; jessie_cmd; "-why-opt -split-user-conj";
	  verbose_opt; why_opt; jc_opt; debug_opt; behav_opt;
	  "-locs"; locname; filename ]
    in
(*     Format.eprintf "%s@." cmd; *)
    sys_command cmd;

    (* Phase 9: call Why to VP translation *)

    let makefile = basename ^ ".makefile" in

    (* temporarily, we launch proving tools of the Why platform,
       either graphic or script-based
    *)

    Format.eprintf "Calling VCs generator.@.";
    if Cmdline.Jessie.Gui.get () then
      sys_command ("make -f " ^ makefile ^ " gui")
    else if Cmdline.Jessie.GenGoals.get () then
      sys_command ("make -f " ^ makefile ^ " goals")
    else
       sys_command ("make -f " ^ makefile ^ " " ^ (Cmdline.Jessie.Atp.get ()));

    flush_all ()(*;
    (* TODO: we should not exit now but give
       the control back to Frama-C kernel *)
    exit 0*)
  with Exit -> ()

let run_and_catch_error () =
  try run () with Errormsg.Error -> ()

let main _fmt =
  if Cmdline.Jessie.Analysis.get () then !Db.Jessie.run_analysis ()

let () = Db.Main.extend main
let () =
  Db.Jessie.run_analysis := run_and_catch_error;
  Options.add_plugin
    ~name:"C to Jessie (experimental)"
    ~descr:"translation to Jessie"
    options

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j bin/toplevel.byte"
End:
*)
