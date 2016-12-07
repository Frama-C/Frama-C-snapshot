(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

module Fc_config = Config
module STRING = String
let () = Plugin.is_share_visible ()
include Plugin.Register
    (struct
      let name = "WP"
      let shortname = "wp"
      let help = "Proof by Weakest Precondition Calculus"
    end)

(* localize all warnings inside WP *)

let warning ?current = match current with
  | None -> warning ~current:true
  | Some b -> warning ~current:b

let resetdemon = ref []
let on_reset f = resetdemon := f :: !resetdemon
let reset () = List.iter (fun f -> f ()) !resetdemon
let has_dkey k = Datatype.String.Set.mem k (Debug_category.get())

(* ------------------------------------------------------------------------ *)
(* ---  WP Generation                                                   --- *)
(* ------------------------------------------------------------------------ *)

let wp_generation = add_group "Goal Selection"

let () = Parameter_customize.set_group wp_generation
let () = Parameter_customize.do_not_save ()
module WP =
  Action(struct
    let option_name = "-wp"
    let help = "Generate proof obligations for all (selected) properties."
  end)
let () = on_reset WP.clear

let () = Parameter_customize.set_group wp_generation
let () = Parameter_customize.do_not_save ()
module Functions =
  Kernel_function_set
    (struct
      let option_name = "-wp-fct"
      let arg_name = "f,..."
      let help = "Select properties of given functions (defaults to all functions)."
    end)
let () = on_reset Functions.clear

let () = Parameter_customize.set_group wp_generation
let () = Parameter_customize.do_not_save ()
module SkipFunctions =
  Kernel_function_set
    (struct
      let option_name = "-wp-skip-fct"
      let arg_name = "f,..."
      let help = "Skip the specified functions (defaults to none)."
    end)
let () = on_reset SkipFunctions.clear

let () = Parameter_customize.set_group wp_generation
let () = Parameter_customize.do_not_save ()
module Behaviors =
  String_list
    (struct
      let option_name = "-wp-bhv"
      let arg_name = "b,..."
      let help = "Select properties of the given behaviors (defaults to all behaviors) of the selected functions."
    end)
let () = on_reset Behaviors.clear

let () = Parameter_customize.set_group wp_generation
let () = Parameter_customize.do_not_save ()
let () = Parameter_customize.no_category ()
module Properties =
  String_list
    (struct
      let option_name = "-wp-prop"
      let arg_name = "p,..."
      let help = "Select properties having the one of the given tagnames (defaults to all properties).\n\
                  You may also replace the tagname by '@category' for the selection of all properties of the given category.\n\
                  Accepted categories are: lemmas, requires, assigns, ensures, exits, complete_behaviors, disjoint_behaviors assert, invariant, variant, breaks, continues, returns.\n\
                  Starts by a minus character to remove properties from the selection."
    end)
let () = on_reset Properties.clear

type job =
  | WP_None
  | WP_All
  | WP_SkipFct of Cil_datatype.Kf.Set.t
  | WP_Fct of Cil_datatype.Kf.Set.t

let job () =
  if WP.get () || not (Functions.is_empty()) ||
     not (Behaviors.is_empty()) || not (Properties.is_empty())
  then
    if Functions.is_empty() then
      if SkipFunctions.is_empty () then WP_All
      else WP_SkipFct (SkipFunctions.get())
    else
      WP_Fct (Cil_datatype.Kf.Set.diff (Functions.get()) (SkipFunctions.get()))
  else WP_None

let () = Parameter_customize.set_group wp_generation
module StatusAll =
  False(struct
    let option_name = "-wp-status-all"
    let help = "Select properties with any status."
  end)

let () = Parameter_customize.set_group wp_generation
module StatusTrue =
  False(struct
    let option_name = "-wp-status-valid"
    let help = "Select properties with status 'Valid'."
  end)

let () = Parameter_customize.set_group wp_generation
module StatusFalse =
  False(struct
    let option_name = "-wp-status-invalid"
    let help = "Select properties with status 'Invalid'."
  end)

let () = Parameter_customize.set_group wp_generation
module StatusMaybe =
  True(struct
    let option_name = "-wp-status-maybe"
    let help = "Select properties with status 'Maybe'."
  end)

(* ------------------------------------------------------------------------ *)
(* ---  Memory Models                                                   --- *)
(* ------------------------------------------------------------------------ *)

let wp_model = add_group "Model Selection"

let () = Parameter_customize.set_group wp_model
module Model =
  String_list
    (struct
      let option_name = "-wp-model"
      let arg_name = "model+..."
      let help = "Memory model selection. Available selectors:\n \
                  * 'Hoare' logic variables only\n \
                  * 'Typed' typed pointers only\n \
                  * '+nocast' no pointer cast\n \
                  * '+cast' unsafe pointer casts\n \
                  * '+raw' no logic variable\n \
                  * '+ref' by-reference-style pointers detection\n \
                  * '+nat/+rg/+int' natural, no-range or machine-integers arithmetics\n \
                  * '+real/+float' real or IEEE floatting point arithmetics"
    end)

let () = Parameter_customize.set_group wp_model
module ByValue =
  String_set
    (struct
      let option_name = "-wp-unalias-vars"
      let arg_name = "var,..."
      let help = "Consider variable names non-aliased."
    end)

let () = Parameter_customize.set_group wp_model
module ByRef =
  String_set
    (struct
      let option_name = "-wp-ref-vars"
      let arg_name = "var,..."
      let help = "Consider variable names by reference."
    end)

let () = Parameter_customize.set_group wp_model
module InHeap =
  String_set
    (struct
      let option_name = "-wp-alias-vars"
      let arg_name = "var,..."
      let help = "Consider variable names aliased."
    end)

let () = Parameter_customize.set_group wp_model
module InCtxt =
  String_set
    (struct
      let option_name = "-wp-context-vars"
      let arg_name = "var,..."
      let help = "Consider variable names in isolated context."
    end)

let () = Parameter_customize.set_group wp_model
module ExternArrays =
  False(struct
    let option_name = "-wp-extern-arrays"
    let help = "Put some default size for extern arrays."
  end)

let () = Parameter_customize.set_group wp_model
module ExtEqual =
  False(struct
    let option_name = "-wp-extensional"
    let help = "Use extensional equality on compounds (hypotheses only)."
  end)

let () = Parameter_customize.set_group wp_model
module Literals =
  False(struct
    let option_name = "-wp-literals"
    let help = "Export content of string literals."
  end)

(* ------------------------------------------------------------------------ *)
(* ---  WP Strategy                                                     --- *)
(* ------------------------------------------------------------------------ *)

let wp_strategy = add_group "Computation Strategies"

let () = Parameter_customize.set_group wp_strategy
module Init =
  False(struct
    let option_name = "-wp-init-const"
    let help = "Use initializers for global const variables."
  end)

let () = Parameter_customize.set_group wp_strategy
module CalleePreCond =
  True(struct
    let option_name = "-wp-callee-precond"
    let help = "Use pre-conditions of callee."
  end)

let () = Parameter_customize.set_group wp_strategy
module RTE =
  False(struct
    let option_name = "-wp-rte"
    let help = "Generate RTE guards before WP."
  end)

let () = Parameter_customize.set_group wp_strategy
module Split =
  False(struct
    let option_name = "-wp-split"
    let help = "Split conjunctions into sub-goals."
  end)

let () = Parameter_customize.set_group wp_strategy
module Invariants =
  False(struct
    let option_name = "-wp-invariants"
    let help = "Handle generalized invariants inside loops."
  end)

let () = Parameter_customize.set_group wp_strategy
module DynCall =
  False(struct
    let option_name = "-wp-dynamic"
    let help = "Handle dynamic calls with specific annotations."
  end)

(* ------------------------------------------------------------------------ *)
(* ---  Qed Simplifications                                             --- *)
(* ------------------------------------------------------------------------ *)

let wp_simplifier = add_group "Qed Simplifications"

let () = Parameter_customize.set_group wp_simplifier
module Simpl =
  True(struct
    let option_name = "-wp-simpl"
    let help = "Enable Qed Simplifications."
  end)

let () = Parameter_customize.set_group wp_simplifier
module Let =
  True(struct
    let option_name = "-wp-let"
    let help = "Use variable elimination."
  end)

let () = Parameter_customize.set_group wp_simplifier
module Core =
  True(struct
    let option_name = "-wp-core"
    let help = "Lift core facts through branches."
  end)

let () = Parameter_customize.set_group wp_simplifier
module Prune =
  True(struct
    let option_name = "-wp-pruning"
    let help = "Prune trivial branches."
  end)

let () = Parameter_customize.set_group wp_simplifier
module Clean =
  True(struct
    let option_name = "-wp-clean"
    let help = "Use a simple cleaning in case of -wp-no-let."
  end)

let () = Parameter_customize.set_group wp_simplifier
module Filter =
  True(struct
    let option_name = "-wp-filter"
    let help = "Use variable filtering."
  end)

let () = Parameter_customize.set_group wp_simplifier
module Bits =
  True(struct
    let option_name = "-wp-bits"
    let help = "Use bit-test simplifications."
  end)

let () = Parameter_customize.set_group wp_simplifier
module SimplifyIsCint =
  True(struct
    let option_name = "-wp-simplify-is-cint"
    let help = "Remove redondant machine integer range hypothesis."
  end)

let () = Parameter_customize.set_group wp_simplifier
module SimplifyForall =
  False(struct
    let option_name = "-wp-simplify-forall"
    let help = "Remove machine integer ranges in quantifiers."
  end)

let () = Parameter_customize.set_group wp_simplifier
module SimplifyType =
  False(struct
    let option_name = "-wp-simplify-type"
    let help = "Remove all `Type` constraints."
  end)

let () = Parameter_customize.set_group wp_simplifier
module QedChecks =
  String_set(struct
    let option_name = "-wp-qed-checks"
    let arg_name = "qed-key,..."
    let help = "Check internal simplifications."
  end)

let () = Parameter_customize.set_group wp_simplifier
module InitWithForall =
  True(struct
    let option_name = "-wp-init-summarize-array"
    let help = "Summarize contiguous initializers with quantifiers."
  end)

let () = Parameter_customize.set_group wp_simplifier
module BoundForallUnfolding =
  Int(struct
    let option_name = "-wp-bound-forall-unfolding"
    let help = "Instanciate up to <n> forall-integers hypotheses."
    let arg_name="n"
    let default = 1000
  end)

(* ------------------------------------------------------------------------ *)
(* ---  Prover Interface                                                --- *)
(* ------------------------------------------------------------------------ *)

let wp_prover = add_group "Prover Interface"

let () = Parameter_customize.set_group wp_prover
module Provers = String_list
    (struct
      let option_name = "-wp-prover"
      let arg_name = "dp,..."
      let help =
        "Submit proof obligations to external prover(s):\n\
         - 'none' to skip provers\n\
         Directly supported provers:\n\
         - 'alt-ergo' (default)\n\
         - 'altgr-ergo' (gui)\n\
         - 'coq', 'coqide' (see also -wp-script)\n\
         - 'why3:<dp>' or '<dp>' (why3 prover, see -wp-detect)\n\
         - 'why3ide' (why3 gui)"
    end)

let () = Provers.add_aliases [ "-wp-proof" ] (* Deprecated *)

let () = Parameter_customize.set_group wp_prover
module Generate = False
    (struct
      let option_name = "-wp-gen"
      let help = "Only generate prover files (default: no)."
    end)
let () = on_reset Generate.clear

let () = Parameter_customize.set_group wp_prover
module Detect = Action
    (struct
      let option_name = "-wp-detect"
      let help = "List installed provers."
    end)
let () = on_reset Detect.clear

let () = Parameter_customize.set_group wp_prover
module Drivers =
  String_list
    (struct
      let option_name = "-wp-driver"
      let arg_name = "file,..."
      let help = "Load drivers for linking to external libraries"
    end)

let () = Parameter_customize.set_group wp_prover
module Depth =
  Int(struct
    let option_name = "-wp-depth"
    let default = 0
    let arg_name = "p"
    let help = "Set depth of exploration for provers."
  end)

let () = Parameter_customize.set_group wp_prover
module Steps =
  Int(struct
    let option_name = "-wp-steps"
    let default = 0
    let arg_name = "n"
    let help = "Set number of steps for provers."
  end)

let () = Parameter_customize.set_group wp_prover
module Timeout =
  Int(struct
    let option_name = "-wp-timeout"
    let default = 10
    let arg_name = "n"
    let help =
      Printf.sprintf
        "Set the timeout (in seconds) for provers (default: %d)." default
  end)

let () = Parameter_customize.set_group wp_prover
module Procs =
  Int(struct
    let option_name = "-wp-par"
    let arg_name = "p"
    let default = 4
    let help =
      Printf.sprintf
        "Number of parallel proof process (default: %d)" default
  end)

let () = Parameter_customize.set_group wp_prover
module ProofTrace =
  False
    (struct
      let option_name = "-wp-proof-trace"
      let help = "Keeps output of provers for valid POs (default: no)"
    end)

(* ------------------------------------------------------------------------ *)
(* ---  Prover Options                                                  --- *)
(* ------------------------------------------------------------------------ *)

let wp_prover_options = add_group "Prover Options"

let () = Parameter_customize.set_group wp_prover_options
module Script =
  String(struct
    let option_name = "-wp-script"
    let arg_name = "f.script"
    let default = ""
    let help = "Set user's file for Coq proofs."
  end)

let () = Parameter_customize.set_group wp_prover_options
module UpdateScript =
  True(struct
    let option_name = "-wp-update-script"
    let help = "If turned off, do not save or modify user's proofs."
  end)

let () = Parameter_customize.set_group wp_prover_options
module CoqTimeout =
  Int(struct
    let option_name = "-wp-coq-timeout"
    let default = 30
    let arg_name = "n"
    let help =
      Printf.sprintf
        "Set the timeout (in seconds) for Coq (default: %d)." default
  end)

let () = Parameter_customize.set_group wp_prover_options
module CoqCompiler =
  String(struct
    let option_name = "-wp-coqc"
    let default = "coqc"
    let arg_name = "cmd"
    let help =
      Printf.sprintf
        "Set the command line to run Coq Compiler (default 'coqc')."
  end)

let () = Parameter_customize.set_group wp_prover_options
module CoqIde =
  String(struct
    let option_name = "-wp-coqide"
    let default = "coqide"
    let arg_name = "cmd"
    let help =
      Printf.sprintf
        "Set the command line to run CoqIde (default 'coqide')\n\
         If the command-line contains 'emacs' (case insentive),\n\
         a coq-project file is used instead of coq options."
  end)

let () = Parameter_customize.set_group wp_prover_options
module CoqProject =
  String(struct
    let option_name = "-wp-coq-project"
    let default = "_CoqProject"
    let arg_name = "file"
    let help =
      Printf.sprintf
        "Set the Coq-Project file to used with Proof General (default '_CoqProject')"
  end)

let () = Parameter_customize.set_group wp_prover_options
module CoqTactic =
  String
    (struct
      let option_name = "-wp-tactic"
      let arg_name = "proof"
      let default = "auto with zarith"
      let help = "Default tactic for Coq"
    end)

let () = Parameter_customize.set_group wp_prover_options
module TryHints =
  False
    (struct
      let option_name = "-wp-tryhints"
      let help = "Try scripts from other goals (see also -wp-hints)"
    end)

let () = Parameter_customize.set_group wp_prover_options
module Hints =
  Int
    (struct
      let option_name = "-wp-hints"
      let arg_name = "n"
      let default = 3
      let help = "Maximum number of proposed Coq scripts (default 3)"
    end)

let () = Parameter_customize.set_group wp_prover_options
module Includes =
  String_list
    (struct
      let option_name = "-wp-include"
      let arg_name = "dir,...,++sharedir"
      let help = "Directory where to find libraries and drivers for provers"
    end)

let () = Parameter_customize.set_group wp_prover_options
module CoqLibs =
  String_list
    (struct
      let option_name = "-wp-coq-lib"
      let arg_name = "*.v,*.vo"
      let help = "Additional libraries for Coq"
    end)

let () = Parameter_customize.set_group wp_prover_options
module Why3 =
  String(struct
    let option_name = "-wp-why3"
    let default = "why3" 
    let arg_name = "cmd"
    let help = "Command to run Why-3 (default: 'why3')"
  end)

let () = Parameter_customize.set_group wp_prover_options
module WhyLibs =
  String_list
    (struct
      let option_name = "-wp-why-lib"
      let arg_name = "*.why"
      let help = "Additional libraries for Why"
    end)

let () = Parameter_customize.set_group wp_prover_options
let () = Parameter_customize.no_category ()
module WhyFlags =
  String_list
    (struct
      let option_name = "-wp-why-opt"
      let arg_name = "option,..."
      let help = "Additional options for Why3"
    end)

let () = Parameter_customize.set_group wp_prover_options
module AltErgo =
  String(struct
    let option_name = "-wp-alt-ergo"
    let default = "alt-ergo" 
    let arg_name = "<cmd>"
    let help = "Command to run alt-ergo (default: 'alt-ergo')"
  end)

let () = Parameter_customize.set_group wp_prover_options
module AltGrErgo =
  String(struct
    let option_name = "-wp-altgr-ergo"
    let default = "altgr-ergo" 
    let arg_name = "<cmd>"
    let help = "Command to run alt-ergo user interface (default: 'altgr-ergo')"
  end)

let () = Parameter_customize.set_group wp_prover_options
module AltErgoLibs =
  String_list
    (struct
      let option_name = "-wp-alt-ergo-lib"
      let arg_name = "*.mlw"
      let help = "Additional library file for Alt-Ergo"
    end)

let () = Parameter_customize.set_group wp_prover_options
let () = Parameter_customize.no_category ()
module AltErgoFlags =
  String_list
    (struct
      let option_name = "-wp-alt-ergo-opt"
      let arg_name = "option,..."
      let help = "Additional options for Alt-Ergo"
    end)

(* ------------------------------------------------------------------------ *)
(* --- PO Management                                                    --- *)
(* ------------------------------------------------------------------------ *)

let wp_po = add_group "Proof Obligations"

let () = Parameter_customize.set_group wp_po
module TruncPropIdFileName =
  Int(struct
    let option_name = "-wp-filename-truncation"
    let default = 60
    let arg_name = "n"
    let help = "Truncate basename of proof obligation files after <n> characters. Since numbers can be added as suffixes to make theses names unique, filename lengths can be highter to <n>. No truncation is performed when the value equals to zero (defaut: 60)."
  end)


let () = Parameter_customize.set_group wp_po
let () = Parameter_customize.do_not_save ()
module Print =
  Action(struct
    let option_name = "-wp-print"
    let help = "Pretty-prints proof obligations on standard output."
  end)
let () = on_reset Print.clear

let () = Parameter_customize.set_group wp_po
let () = Parameter_customize.do_not_save ()
module Report =
  String_list
    (struct
      let option_name = "-wp-report"
      let arg_name = "report,..."
      let help = "Report specification file(s)"
    end)

let () = Parameter_customize.set_group wp_po
let () = Parameter_customize.do_not_save ()
module ReportName =
  String(struct
    let option_name = "-wp-report-basename"
    let arg_name = "file"
    let default = "wp-report"
    let help = Printf.sprintf "Basename of generated reports (default %S)" default
  end)

let () = Parameter_customize.set_group wp_po
let () = Parameter_customize.do_not_save ()
module Separation =
  False
    (struct
      let option_name = "-wp-print-separation"
      let help = "Print Separation Hypotheses"
    end)

let () = Parameter_customize.set_group wp_po
module OutputDir =
  String(struct
    let option_name = "-wp-out"
    let arg_name = "dir"
    let default = ""
    let help = "Set working directory for generated files.\n\
                Defaults to some temporary directory."
  end)

let () = Parameter_customize.set_group wp_po
let () = Parameter_customize.do_not_save ()
module Check =
  Action(struct
    let option_name = "-wp-check"
    let help =
      "Check the syntax and type of the produced file, instead of proving."
  end)
let () = on_reset Print.clear

(* -------------------------------------------------------------------------- *)
(* --- OS environment variables                                           --- *)
(* -------------------------------------------------------------------------- *)

let dkey = register_category "env"

let get_env ?default var =
  try
    let varval = Sys.getenv var in
    debug ~dkey "ENV %s=%S" var varval ; varval
  with Not_found ->
    debug ~dkey "ENV %s not set." var ;
    match default with
    | Some varval ->
        debug ~dkey "ENV %s default(%S)" var varval ; varval
    | None ->
        debug ~dkey "ENV %s undefined." var ;
        raise Not_found

let dkey = register_category "prover"

let is_out () = !Fc_config.is_gui || OutputDir.get() <> ""

let make_output_dir dir =
  if Sys.file_exists dir then
    begin
      if not (Sys.is_directory dir) then
        abort "File '%s' is not a directory (WP aborted)" dir ;
    end
  else
    begin
      try
        Unix.mkdir dir 0o770 ;
        debug ~dkey "Created output directory '%s'" dir
      with Unix.Unix_error (err,_,_) ->
        let msg = Unix.error_message err in
        abort
          "System Error (%s)@\nCan not create output directory '%s'"
          msg dir
    end

(*[LC] Do not projectify this reference : it is common to all projects *)
let unique_tmp = ref None
let make_tmp_dir () =
  match !unique_tmp with
  | None ->
      let tmp =
        try Extlib.temp_dir_cleanup_at_exit "wp"
        with Extlib.Temp_file_error s ->
          abort "Cannot create temporary file: %s" s
      in
      unique_tmp := Some tmp ;
      debug ~dkey "Created temporary directory '%s'" tmp ;
      tmp
  | Some tmp -> tmp

let make_gui_dir () =
  try
    let home =
      try Sys.getenv "USERPROFILE" (*Win32*) with Not_found ->
        try Sys.getenv "HOME" (*Unix like*) with Not_found ->
          "." in
    let dir = home ^ "/" ^ ".frama-c-wp" in
    if Sys.file_exists dir && Sys.is_directory dir then
      Extlib.safe_remove_dir dir;
    make_output_dir dir ; dir
  with _ ->
    make_tmp_dir ()

(** call the construction of the directory only once *)
let base_output = ref None
let base_output () =
  match !base_output with
  | None -> let output =
              match OutputDir.get () with
              | "" ->
                  if !Fc_config.is_gui
                  then make_gui_dir ()
                  else make_tmp_dir ()
              | dir ->
                  make_output_dir dir ; dir in
      base_output := Some output;
      output
  | Some output -> output

let get_output () =
  let base = base_output () in
  let project = Project.current () in
  let name = Project.get_unique_name project in
  if name = "default" then base
  else
    let dir = base ^ "/" ^ name in
    make_output_dir dir ; dir

let get_output_dir d =
  let base = get_output () in
  let path = Printf.sprintf "%s/%s" base d in
  make_output_dir path ; path

let get_includes () =
  List.map
    (fun d ->
       if STRING.get d 0 = '+' then
         Printf.sprintf "%s/%s"
           (Kernel.Share.dir ())
           (STRING.sub d 1 (STRING.length d - 1))
       else d)
    (Includes.get ())

let cat_print_generated = register_category "print-generated"

let has_print_generated () = has_dkey "print-generated"

let print_generated file =
  debug ~dkey:cat_print_generated
    "%a@."
    (fun fmt file ->
       Command.read_lines file (fun s ->
           Format.pp_print_string fmt s;
           Format.pp_print_newline fmt ()))
    file;
