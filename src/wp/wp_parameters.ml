(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

let () = Plugin.is_share_visible ()
include Plugin.Register
  (struct
     let name = "WP"
     let shortname = "wp"
     let help = "Weakest Preconditions Calculus\n\
WP 0.6 for " ^ Config.version
   end)

(* localize all warnings inside WP *)

let warning ?current = match current with
  | None -> warning ~current:true
  | Some b -> warning ~current:b

let resetdemon = ref []
let on_reset f = resetdemon := f :: !resetdemon
let reset () = List.iter (fun f -> f ()) !resetdemon

(* implemented as an alias for -wp-debug-category. *)
let () = Debug_category.add_aliases [ "-wp-log" ]
let has_dkey k = List.mem k (Debug_category.get())

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
  | WP_Select of string list

type assigns_method =
  | NoAssigns
  | NormalAssigns
  | EffectAssigns

let job () =
  let nonempty p = p () <> [] in
  if WP.get () || nonempty Functions.get || nonempty Behaviors.get || nonempty Properties.get then
    let fct = Functions.get () in
    if fct <> [] then WP_Select fct else WP_All
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

type model_kind =
  | M_Q of string
  | M_Hoare
  | M_Logic
  | M_Store
  | M_Runtime

let model_of_name = function
  | "Runtime" -> M_Runtime
  | "Store" -> M_Store
  | "Logic" -> M_Logic
  | "Hoare" -> M_Hoare
  | m -> M_Q m

let () = Plugin.set_group wp_model
module Model =
  String(struct
           let option_name = "-wp-model"
           let arg_name = "model"
           let help =
             "Memory model selection:\n\
              - 'Hoare':  no indirect access to the memory\n\
              - 'Store':  no heterogeneous casts (default)\n\
              - 'Logic':  Store with logic variables\n\
              - 'Runtime': low-level model"
           let default = "Store"
         end)

let get_model () =
  let m = Model.get () in
  try model_of_name m
  with Not_found -> abort "Unknown model '%s'" m

let () = Plugin.set_group wp_model
module LogicVar =
  True(struct
         let option_name = "-wp-logicvar"
         let help = "Apply Hoare model for variables when possible."
       end)

let () = Plugin.set_group wp_model
module RefVar = 
  False(struct
         let option_name = "-wp-byreference"
         let help = "Apply Hoare model for arguments passed by reference."
       end)

let () = Plugin.set_group wp_model
module Fits = 
  False(struct
         let option_name = "-wp-fits"
         let help = "Accept casts among fitting pointers in Typed model."
       end)

let () = Plugin.set_group wp_model
module Natural = 
  False(struct
         let option_name = "-wp-untyped"
         let help = "No hypothesis on type."
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
module Assigns =
  String(struct
           let option_name = "-wp-assigns"
           let arg_name = "mth"
           let help = "Method for proving assigns clauses:\n\
                       - 'effect' one sub-goal per assignment (default)\n\
                       - 'memory' strong proof (incompatible with Hoare)\n\
                       - 'none'   skip assigns clause (default with Hoare)"
           let default = "effect"
         end)
let () = Assigns.set_possible_values [ "effect" ; "memory" ; "none" ]
(* no reset for GUI *)

let get_assigns_method () =
  match Assigns.get () with
    | "effect" -> EffectAssigns
    | "memory" -> NormalAssigns
    | "none"   -> NoAssigns
    | m -> abort "Unknown assigns method '%s'" m

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

module Qed =
  False(struct
          let option_name = "-wp-qed"
          let help = "Use Qed Simplifier (not by default)."
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

let () = Plugin.set_group wp_strategy
module SplitDim =
  Int(struct
        let option_name = "-wp-split-dim"
        let arg_name = "n"
        let default = 6
        let help =
          Printf.sprintf
            "Bounds the number of splited sub-goals to 2**n (default 2**%d)" default
      end)

let () = Plugin.set_group wp_strategy
module Norm =
  String(struct
           let option_name = "-wp-norm"
           let arg_name = "norm"
           let help =
             "Predicate normalization for Coq and Alt-Ergo provers:\n\
              - Eqs: replace let-bindings by equalities (default).\n\
              - Let: preserve let-bindings.\n\
              - Exp: let-expansion.\n\
              - Cc:  generates local functions by closure-conversion"
           let default = "Eqs"
         end)

let () = Norm.set_possible_values [ "Let";"Exp";"Cc";"Eqs" ]

type norm = Let | Exp | Eqs | Cc

let get_norm () =
  match Norm.get () with
  | "Let" -> Let
  | "Exp" -> Exp
  | "Eqs" -> Eqs
  | "Cc" -> Cc
  | m -> abort "Unknown normalization '%s'" m

let () = Plugin.set_group wp_strategy
module Huge =
  Int(struct
        let option_name = "-wp-huge"
        let default = 30
        let arg_name = "s"
        let help =
          Printf.sprintf
            "Limits the size for generated proof obligation.\n\
             Proof terms of size exceeding 2^s are not generated.\n\
             (default: 2^%d)"
            default
      end)

(* ------------------------------------------------------------------------ *)
(* ---  Prover Interface                                                --- *)
(* ------------------------------------------------------------------------ *)

let wp_prover = add_group "Prover Interface"

let () = Plugin.set_group wp_prover
module Provers = FilledStringSet
  (struct
     let option_name = "-wp-proof"
     let arg_name = "dp,..."
     let help =
       "Submit proof obligations to external prover(s):\n\
         - 'none' to skip proofs\n\
        Directly supported provers:\n\
         - 'alt-ergo' (default)\n\
         - 'altgr-ergo' (gui)\n\
         - 'coq', 'coqide' (see also -wp-script)\n\
        Supported provers via Why:\n\
         - 'simplify', 'vampire', 'yices', 'cvc3', 'z3', 'zenon'"
     let default =
       if !Config.is_gui
       then Datatype.String.Set.singleton "alt-ergo"
       else Datatype.String.Set.empty
   end)

let get_provers () =
  match Datatype.String.Set.elements (Provers.get()) with
    | [] -> ["alt-ergo"]
    | ps -> ps

let _prover_names =
  [ "none" ;
    "coq" ; "coqide" ;
    "alt-ergo"; "altgr-ergo" ;
    "simplify";
    "vampire";
    "yices";
    "cvc3";
    "zenon";
    "z3" ]

let () = Plugin.set_group wp_prover
let () = Plugin.do_not_save ()
module Check =
  String(struct
           let option_name = "-wp-check"
           let default =  "none"
           let arg_name = "dp"
           let help =
             "Typecheck proof obligations for external prover:\n\
               - 'none' to skip checks (default)\n\
               - 'alt-ergo'\n\
               - 'coq'\n\
               - 'why' "
         end)
let () = on_reset Check.clear

let _check_names =
  [ "none" ;
    "coq" ;
    "alt-ergo";
    "why" ]

let () = Plugin.set_group wp_prover
module Depth =
  Int(struct
	let option_name = "-wp-depth"
	let default = 0
	let arg_name = "p"
	let help = "Set depth of exploration for provers."
      end)

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
module Trace =
  False(struct
          let option_name = "-wp-trace"
          let help = "Keep labels in proof obligations (default: no)."
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
module ProverSwitch =
  EmptyString
    (struct
      let option_name = "-wp-prover-switch"
      let arg_name ="switches"
      let help = "Adds extra <switches> to the selected prover"
     end)

let () = Plugin.set_group wp_proverlibs
module CoqLibs =
  StringList
    (struct
       let option_name = "-wp-coq-lib"
       let arg_name = "file,..."
       let help = "Additional library for Coq (either '.v' or '.vo' extension)"
     end)

let () = Plugin.set_group wp_proverlibs
module WhyLibs =
  StringList
    (struct
       let option_name = "-wp-why-lib"
       let arg_name = "file,..."
       let help = "Additional library file for Why"
     end)

let () = Plugin.set_group wp_proverlibs
module AltErgoLibs =
  StringList
    (struct
       let option_name = "-wp-alt-ergo-lib"
       let arg_name = "file,..."
       let help = "Additional library file for Alt-Ergo"
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
	       let arg_name = "<report,...>"
	       let help = "Report specification file(s)"
	     end)

let () = Plugin.set_group wp_po
let () = Plugin.do_not_save ()
module ReportName =
  String(struct
	   let option_name = "-wp-report-basename"
	   let arg_name = "<f>"
	   let default = "wp-report"
	   let help = Printf.sprintf "Basename of generated reports (default %S)" default
	 end)

let () = Plugin.set_group wp_po
let () = Plugin.do_not_save ()
module Dot =
  False(struct
          let option_name = "-wp-dot"
          let help = "Generates dot files for wp computations."
        end)
let () = on_reset Dot.clear

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
module Details =
  False(struct
          let option_name = "-wp-warnings"
          let help = "Print details about warnings for 'stronger' and 'degenerated' goals"
        end)

(* -------------------------------------------------------------------------- *)
(* --- OS environment variables                                           --- *)
(* -------------------------------------------------------------------------- *)

let get_env ?default var =
  try
    let varval = Sys.getenv var in
    debug "ENV %s=%S" var varval ; varval
  with Not_found ->
    debug "ENV %s not set." var ;
    match default with
      | Some varval ->
          debug "ENV %s default(%S)" var varval ; varval
      | None ->
          debug "ENV %s undefined." var ;
          raise Not_found

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
        debug "Created output directory '%s'" dir
      with e ->
        debug "System error '%s'" (Printexc.to_string e) ;
        abort "Can not create output directory '%s'" dir
    end
      
(* Do not projectify this reference : it is common to all projects *)
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
    make_output_dir dir ; dir
  with _ ->
    make_tmp_dir ()

let base_output () =
  match OutputDir.get () with
    | "" ->
        if !Config.is_gui || debug_atleast 1
        then make_gui_dir ()
        else make_tmp_dir ()
    | dir ->
        make_output_dir dir ; dir

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

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
