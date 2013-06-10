(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

module STRING = String
let () = Plugin.is_share_visible ()
include Plugin.Register
  (struct
     let name = "WP"
     let shortname = "wp"
     let help = "Weakest Preconditions Calculus\n\
WP 0.7 for " ^ Config.version
   end)

(* localize all warnings inside WP *)

let warning ?current = match current with
  | None -> warning ~current:true
  | Some b -> warning ~current:b

let resetdemon = ref []
let on_reset f = resetdemon := f :: !resetdemon
let reset () = List.iter (fun f -> f ()) !resetdemon

module Log = 
  StringSet
    (struct
      let option_name = "-wp-log"
      let arg_name = "..."
      let help = "Log Specific informations"
     end)

let has_dkey k = 
  Datatype.String.Set.mem k (Log.get()) ||
    Datatype.String.Set.mem k (Debug_category.get())

(* ------------------------------------------------------------------------ *)
(* ---  WP Generation                                                   --- *)
(* ------------------------------------------------------------------------ *)

let wp_generation = add_group "Goal Selection"

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module WP =
  Action(struct
           let option_name = "-wp"
           let help = "Generates proof obligations for all (selected) properties."
         end)
let () = on_reset WP.clear

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module Functions =
  StringList
    (struct
      let option_name = "-wp-fct"
      let arg_name = "f,..."
      let help = "selects properties of given functions (defaults to all functions)"
     end)
let () = on_reset Functions.clear

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module SkipFunctions =
  StringList
    (struct
      let option_name = "-wp-skip-fct"
      let arg_name = "f,..."
      let help = "skip the specified functions (defaults to none)"
     end)
let () = on_reset SkipFunctions.clear

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module Behaviors =
  StringList
    (struct
      let option_name = "-wp-bhv"
      let arg_name = "b,..."
      let help = "selects properties of the given behaviors (defaults to all behaviors) of the selected functions."
     end)
let () = on_reset Behaviors.clear

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module Properties =
  StringList
    (struct
      let option_name = "-wp-prop"
      let arg_name = "p,..."
      let help = "selects properties having the one of the given tagnames (defaults to all properties).\n\
You may also replace the tagname by '@category' for the selection of all properties of the given category.\n\
Accepted categories are: lemmas, requires, assigns, ensures, exits, complete_behaviors, disjoint_behaviors assert, invariant, variant, breaks, continues, returns.\n\
Starts by a minus character to remove properties from the selection."
     end)
let () = on_reset Properties.clear

type job =
  | WP_None
  | WP_All
  | WP_SkipFct of string list
  | WP_Fct of string list

let job () =
  let nonempty p = p () <> [] in
  if WP.get () || nonempty Functions.get || nonempty Behaviors.get || nonempty Properties.get 
  then
    let fct = Functions.get () in
    let skp = SkipFunctions.get () in
    match fct , skp with
      | [] , [] -> WP_All
      | _ , [] -> WP_Fct fct
      | [] , _ -> WP_SkipFct skp
      | _ , _ -> WP_Fct (List.filter (fun f -> not (List.mem f skp)) fct)
  else WP_None

let () = Plugin.set_group wp_generation
module StatusAll =
  False(struct
          let option_name = "-wp-status-all"
          let help = "Select properties with any status (default: no)"
       end)

let () = Plugin.set_group wp_generation
module StatusTrue =
  False(struct
          let option_name = "-wp-status-valid"
          let help = "Select properties with status 'Valid' (default: no)"
       end)

let () = Plugin.set_group wp_generation
module StatusFalse =
  False(struct
          let option_name = "-wp-status-invalid"
          let help = "Select properties with status 'Invalid' (default: no)"
        end)

let () = Plugin.set_group wp_generation
module StatusMaybe =
  True(struct
         let option_name = "-wp-status-maybe"
         let help = "Select properties with status 'Maybe' (default: yes)"
       end)

(* ------------------------------------------------------------------------ *)
(* --- Froms                                                            --- *)
(* ------------------------------------------------------------------------ *)

let () = Plugin.set_group wp_generation
module Froms =
  False(struct
          let option_name = "-wp-froms"
          let help = "Undocumented (dot not use)."
        end)

(* ------------------------------------------------------------------------ *)
(* ---  Memory Models                                                   --- *)
(* ------------------------------------------------------------------------ *)

let wp_model = add_group "Model Selection"

let () = Plugin.set_group wp_model
module Model =
  StringList
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
                    * '+nat/+cint' natural or machine integers arithmetics\n \
                    * '+real/+float' real or IEEE floatting point arithmetics"
     end)
    
let () = Plugin.set_group wp_model
module ExternArrays =
  False(struct
	  let option_name = "-wp-extern-arrays"
	  let help = "Put some default size for extern arrays"
	end)

let () = Plugin.set_group wp_model
module ExtEqual =
  False(struct
	  let option_name = "-wp-extensional"
	  let help = "Use extensional equality on compounds (hypotheses only)"
	end)

let () = Plugin.set_group wp_model
module Literals =
  False(struct
	  let option_name = "-wp-literals"
	  let help = "Export content of string literals (not by default)"
	end)

(* ------------------------------------------------------------------------ *)
(* ---  WP Strategy                                                     --- *)
(* ------------------------------------------------------------------------ *)

let wp_strategy = add_group "Computation Strategies"

let () = Plugin.set_group wp_strategy
module RTE =
  False(struct
          let option_name = "-wp-rte"
          let help = "Generates RTE guards before WP"
        end)

let () = Plugin.set_group wp_strategy
module Simpl =
  True(struct
        let option_name = "-wp-simpl"
        let help = "Simplify constant terms and predicates."
       end)

let () = Plugin.set_group wp_strategy
module Let =
  True(struct
         let option_name = "-wp-let"
         let help = "Use variable elimination (by default)."
       end)

let () = Plugin.set_group wp_strategy
module Prune =
  True(struct
         let option_name = "-wp-pruning"
         let help = "Prune trivial branches (by default)."
       end)

let () = Plugin.set_group wp_strategy
module Clean =
  True(struct
          let option_name = "-wp-clean"
          let help = "Use variable filtering (by default)."
	end)

let () = Plugin.set_group wp_strategy
module Invariants =
  False(struct
          let option_name = "-wp-invariants"
          let help = "Handle generalized invariants inside loops."
        end)

let () = Plugin.set_group wp_strategy
module Split =
  False(struct
          let option_name = "-wp-split"
          let help = "Split conjunctions into sub-goals."
        end)

(* ------------------------------------------------------------------------ *)
(* ---  Prover Interface                                                --- *)
(* ------------------------------------------------------------------------ *)

let wp_prover = add_group "Prover Interface"

let () = Plugin.set_group wp_prover
module Provers = StringList
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

let () = Plugin.set_group wp_prover
module Generate = False
  (struct
     let option_name = "-wp-gen"
     let help = "Only generate prover files (default: no)."
   end)
let () = on_reset Generate.clear 

let () = Plugin.set_group wp_prover
module Detect = Action
  (struct
     let option_name = "-wp-detect"
     let help = "List installed provers."
   end)
let () = on_reset Detect.clear

let () = Plugin.set_group wp_prover
module Drivers =
  StringList
    (struct
       let option_name = "-wp-driver"
       let arg_name = "file,..."
       let help = "Load drivers for linking to external libraries"
     end)

let () = Plugin.set_group wp_prover
module Depth =
  Int(struct
	let option_name = "-wp-depth"
	let default = 0
	let arg_name = "p"
	let help = "Set depth of exploration for provers."
      end)

let () = Plugin.set_group wp_prover
module Steps =
  Int(struct
	let option_name = "-wp-steps"
	let default = 0
	let arg_name = "n"
	let help = "Set number of steps for provers."
      end)

let () = Plugin.set_group wp_prover
module Timeout =
  Int(struct
        let option_name = "-wp-timeout"
        let default = 10
        let arg_name = "n"
        let help =
          Printf.sprintf
            "Set the timeout (in seconds) for provers (default: %d)." default
      end)

let () = Plugin.set_group wp_prover
module CoqTimeout =
  Int(struct
        let option_name = "-wp-coq-timeout"
        let default = 30
        let arg_name = "n"
        let help =
          Printf.sprintf
            "Set the timeout (in seconds) for Coq (default: %d)." default
      end)

let () = Plugin.set_group wp_prover
module Procs =
  Int(struct
        let option_name = "-wp-par"
        let arg_name = "p"
        let default = 4
        let help =
          Printf.sprintf
            "Number of parallel proof process (default: %d)" default
      end)

let () = Plugin.set_group wp_prover
module ProofTrace =
  False
    (struct
        let option_name = "-wp-proof-trace"
        let help = "Keeps output of provers for valid POs (default: no)"
     end)

let () = Plugin.set_group wp_prover
module UnsatModel =
  False
    (struct
       let option_name = "-wp-unsat-model"
       let help = "Keeps output of provers for unknown POs (default: no)"
     end)

(* ------------------------------------------------------------------------ *)
(* ---  Prover Libraries                                                --- *)
(* ------------------------------------------------------------------------ *)

let wp_proverlibs = add_group "Prover Libraries"

let () = Plugin.set_group wp_proverlibs
module Script =
  String(struct
           let option_name = "-wp-script"
           let arg_name = "f.script"
           let default = ""
           let help = "Set user's file for Coq proofs."
         end)

let () = Plugin.set_group wp_proverlibs
module UpdateScript =
  True(struct
         let option_name = "-wp-update-script"
         let help = "If turned off, do not save or modify user's proofs."
       end)

let () = Plugin.set_group wp_proverlibs
module CoqTactic =
  String
    (struct
       let option_name = "-wp-tactic"
       let arg_name = "proof"
       let default = "auto with zarith"
       let help = "Default tactic for Coq"
     end)

let () = Plugin.set_group wp_proverlibs
module TryHints =
  False
    (struct
       let option_name = "-wp-tryhints"
       let help = "Try scripts from other goals (see also -wp-hints)"
     end)

let () = Plugin.set_group wp_proverlibs
module Hints =
  Int
    (struct
       let option_name = "-wp-hints"
       let arg_name = "n"
       let default = 3
       let help = "Maximum number of proposed Coq scripts (default 3)"
     end)

let () = Plugin.set_group wp_proverlibs
module Includes =
  StringList
    (struct
       let option_name = "-wp-include"
       let arg_name = "dir,..."
       let help = "Directory where to find extensions for provers"
     end)

let () = Plugin.set_group wp_proverlibs
module CoqLibs =
  StringList
    (struct
       let option_name = "-wp-coq-lib"
       let arg_name = "*.v,*.vo"
       let help = "Additional libraries for Coq."
     end)

let () = Plugin.set_group wp_proverlibs
module WhyLibs =
  StringList
    (struct
       let option_name = "-wp-why-lib"
       let arg_name = "*.why"
       let help = "Additional libraries for Why"
     end)

let () = Plugin.set_group wp_proverlibs
module WhyFlags =
  StringList
    (struct
       let option_name = "-wp-why-opt"
       let arg_name = "option,..."
       let help = "Additional options for Why3"
     end)

let () = Plugin.set_group wp_proverlibs
module AltErgoLibs =
  StringList
    (struct
       let option_name = "-wp-alt-ergo-lib"
       let arg_name = "*.mlw"
       let help = "Additional library file for Alt-Ergo"
     end)

let () = Plugin.set_group wp_proverlibs
module AltErgoFlags =
  StringList
    (struct
       let option_name = "-wp-alt-ergo-opt"
       let arg_name = "option,..."
       let help = "Additional options for Alt-Ergo"
     end)

let () = Plugin.set_group wp_proverlibs
module AltErgoLightInt =
  False
    (struct
       let option_name = "-wp-alt-ergo-lightint"
       let help = "Use lightweight machine integer library for Alt-Ergo"
     end)

(* ------------------------------------------------------------------------ *)
(* --- PO Management                                                    --- *)
(* ------------------------------------------------------------------------ *)

let wp_po = add_group "Proof Obligations"

let () = Plugin.set_group wp_po
let () = Plugin.do_not_save ()
module Print =
  Action(struct
           let option_name = "-wp-print"
           let help = "Pretty-prints proof obligations on standard output."
         end)
let () = on_reset Print.clear

let () = Plugin.set_group wp_po
let () = Plugin.do_not_save ()
module Report =
  StringList(struct
	       let option_name = "-wp-report"
	       let arg_name = "report,..."
	       let help = "Report specification file(s)"
	     end)

let () = Plugin.set_group wp_po
let () = Plugin.do_not_save ()
module ReportName =
  String(struct
	   let option_name = "-wp-report-basename"
	   let arg_name = "file"
	   let default = "wp-report"
	   let help = Printf.sprintf "Basename of generated reports (default %S)" default
	 end)

let () = Plugin.set_group wp_po
module OutputDir =
  String(struct
           let option_name = "-wp-out"
           let arg_name = "dir"
           let default = ""
           let help = "Set working directory for generated files.\n\
                       Defaults to some temporary directory."
         end)

let () = Plugin.set_group wp_po
let () = Plugin.do_not_save ()
module Check =
  Action(struct
           let option_name = "-wp-check"
           let help =
        "check the syntax and type of the produced file against \n\
         alt-ergo, why3 and coq when the environnement variable WPCHECK=YES."
         end)
let () = on_reset Print.clear

let wpcheck () =
  Check.get () &&
    try
      Sys.getenv "WPCHECK" = "YES"
    with Not_found -> false

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

let dkey = register_category "out"

let is_out () = !Config.is_gui || OutputDir.get() <> ""

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
      with e ->
        debug ~dkey "System error '%s'" (Printexc.to_string e) ;
        abort "Can not create output directory '%s'" dir
    end
      
(*[LC] Do not projectify this reference : it is common to all projects *)
let unique_tmp = ref None
let make_tmp_dir () =
  match !unique_tmp with
    | None ->
	let tmp =
	  try Extlib.temp_dir_cleanup_at_exit "wp"
	  with Extlib.Temp_file_error s ->
	    abort "cannot create temporary file: %s" s
	in unique_tmp := Some tmp ; tmp
    | Some tmp -> tmp

let make_gui_dir () =
  try
    let home = 
      try Sys.getenv "USERPROFILE" (*Win32*) with Not_found ->
        try Sys.getenv "HOME" (*Unix like*) with Not_found ->
          "." in
    let dir = Filename.concat home ".frama-c-wp" in
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
                if !Config.is_gui
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
    let dir = Filename.concat base name in
    make_output_dir dir ; dir
    
let get_output_dir d =
  let base = get_output () in
  let path = Printf.sprintf "%s/%s" base d in
  make_output_dir path ; path

let dkey = register_category "includes"
let find_db = ref false
let find_lib file =
  if Sys.file_exists file then file else 
    let rec lookup file = function
      | [] -> abort "File '%s' not found (see -wp-include)" file
      | dir::dirs ->
	  let path = Printf.sprintf "%s/%s" dir file in
	  if Sys.file_exists path then path else lookup file dirs
    in
    let includes = 
      List.map
	(fun d ->
	   if STRING.get d 0 = '+' then
	     Printf.sprintf "%s/%s"
	       (Kernel.Share.dir ()) 
	       (STRING.sub d 1 (STRING.length d - 1))
	   else d)
	(Includes.get ())
    in
    let shared = 
      try [Share.dir ~error:false ()] 
      with Share.No_dir -> [] in
    let drivers = List.map Filename.dirname (Drivers.get ()) in
    let directories = includes @ drivers @ shared in
    if not !find_db && has_dkey "includes" then
      begin
	find_db := true ;
	debug ~dkey "Included directories:%t"
	  (fun fmt ->
	     List.iter 
	       (fun d -> Format.fprintf fmt "@\n - '%s'" d)
	       directories
	  )
      end ;
    lookup file directories
