(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

include Plugin.Register
  (struct
     let name = "WP" (* Format.sprintf "WP %s" Wp_version.version *)
     let shortname = "wp"
     let help = "Weakest Preconditions Calculus" (* v%d Wp_version.version *)
   end)

(* localize all warnings inside WP *)

let warning ?current = match current with
  | None -> warning ~current:true
  | Some b -> warning ~current:b

let resetdemon = ref []
let on_reset f = resetdemon := f :: !resetdemon
let reset () = List.iter (fun f -> f ()) !resetdemon

module DebugKey =
  StringList
    (struct
       let option_name = "-wp-log"
       let arg_name = "..."
       let help = "Undocumented feature"
       let kind = `Irrelevant
     end)
let () = DebugKey.add_set_hook (fun _old  -> set_debug_keys)

(* ------------------------------------------------------------------------ *)
(* ---  WP Generation                                                   --- *)
(* ------------------------------------------------------------------------ *)

let wp_generation = add_group "Goal Selection"

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module WP =
  Action(struct
           let option_name = "-wp"
           let help = "Computes wp on all functions."
           let kind = `Tuning
         end)
let () = on_reset WP.clear

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module Functions =
  StringList
    (struct
      let option_name = "-wp-fct"
      let arg_name = "f,..."
      let help = "Computes wp only for the selected functions."
      let kind = `Tuning
     end)
let () = on_reset Functions.clear

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module Behaviors =
  StringList
    (struct
      let option_name = "-wp-bhv"
      let arg_name = "b,..."
      let help = "Computes wp only for the selected behaviors."
      let kind = `Tuning
     end)
let () = on_reset Behaviors.clear

let () = Plugin.set_group wp_generation
let () = Plugin.do_not_save ()
module Properties =
  (* TODO [LC] : restablish several names *)
  StringList
    (struct
      let option_name = "-wp-prop"
      let arg_name = "p"
      let help = "Computes wp only for the selected properties.\n\
                  Type 'assigns' for all assigns clauses"
      let kind = `Tuning
     end)
let () = on_reset Properties.clear

type job =
  | WP_None
  | WP_All
  | WP_Select of string list

let job () =
  match Functions.get (), Behaviors.get () with
  | [], [] ->
    (if WP.get () then WP_All
     else match Properties.get () with
     | [] -> WP_None
     | _ :: _ -> WP_Select [])
  | (_ :: _ as fct, _) | fct, _ :: _ -> WP_Select fct

let () = Plugin.set_group wp_generation
module StatusAll =
  False(struct
          let option_name = "-wp-status-all"
          let help = "Select properties with any status (default: no)"
          let kind = `Tuning
       end)

let () = Plugin.set_group wp_generation
module StatusTrue =
  False(struct
          let option_name = "-wp-status-valid"
          let help = "Select properties with status 'Valid' (default: no)"
          let kind = `Tuning
       end)

let () = Plugin.set_group wp_generation
module StatusFalse =
  False(struct
          let option_name = "-wp-status-invalid"
          let help = "Select properties with status 'Invalid' (default: no)"
          let kind = `Tuning
        end)

let () = Plugin.set_group wp_generation
module StatusMaybe =
  True(struct
         let option_name = "-wp-status-maybe"
         let help = "Select properties with status 'Maybe' (default: yes)"
         let kind = `Tuning
       end)

(* ------------------------------------------------------------------------ *)
(* --- Froms                                                            --- *)
(* ------------------------------------------------------------------------ *)

let () = Plugin.set_group wp_generation
module Froms =
  False(struct
          let option_name = "-wp-froms"
          let help = "Undocumented (dot not use)."
          let kind = `Tuning
        end)

(* ------------------------------------------------------------------------ *)
(* ---  Memory Models                                                   --- *)
(* ------------------------------------------------------------------------ *)

let wp_model = add_group "Model Selection"

type model_kind =
  | M_Hoare
  | M_Store
  | M_Runtime

let model_names =
  [ "Hoare" ; "Store"; "Runtime" ]

let model_of_name = function
  | "Runtime" -> M_Runtime
  | "Store" -> M_Store
  | "Hoare" -> M_Hoare
  | _ -> raise Not_found

let () = Plugin.set_group wp_model
module Model =
  String(struct
           let option_name = "-wp-model"
           let arg_name = "model"
           let help =
             "Memory model selection:\n\
              - 'Hoare':  no indirect access to the memory\n\
              - 'Store':  no heterogeneous casts (default)\n\
              - 'Runtime': low-level model"
              (* "- 'Runtime': low-level model\n\
              - 'UnsafeCaveat': no alias\n\
              - 'Caveat': no alias with guards" *)
           let default = "Store"
           let kind = `Tuning
         end)
let () = Model.set_possible_values model_names

let get_model () =
  let m = Model.get () in
  try model_of_name m
  with Not_found -> abort "Unknown model '%s'" m

let get_models () = model_names

let () = Plugin.set_group wp_model
module LogicVar =
  True(struct
         let option_name = "-wp-logicvar"
         let help = "Apply Hoare model for variables when possible."
         let kind = `Tuning
       end)

let () = Plugin.set_group wp_model
module RefVar = 
  False(struct
         let option_name = "-wp-byreference"
         let help = "Apply Hoare model for arguments passed by reference."
         let kind = `Tuning
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
           let kind = `Tuning
         end)
let () = Assigns.set_possible_values [ "effect" ; "memory" ; "none" ]
(* no reset for GUI *)

let get_assigns_method () =
  match Assigns.get () with
    | "effect" -> Mcfg.EffectAssigns
     | "memory" -> Mcfg.NormalAssigns
    | "none"   -> Mcfg.NoAssigns
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
          let kind = `Tuning
        end)

let () = Plugin.set_group wp_strategy
module Simpl =
  True(struct
        let option_name = "-wp-simpl"
        let help = "Simplify constant terms and predicates."
        let kind = `Tuning
       end)

let () = Plugin.set_group wp_strategy
module Invariants =
  False(struct
          let option_name = "-wp-invariants"
          let help = "Handle generalized invariants inside loops."
          let kind = `Tuning
        end)

let () = Plugin.set_group wp_strategy
module Split =
  False(struct
          let option_name = "-wp-split"
          let help = "Split conjunctions into sub-goals."
          let kind = `Tuning
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
        let kind = `Tuning
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
           let kind = `Tuning
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
        let kind = `Tuning
      end)

(* ------------------------------------------------------------------------ *)
(* ---  Prover Interface                                                --- *)
(* ------------------------------------------------------------------------ *)

let wp_prover = add_group "Prover Interface"

let () = Plugin.set_group wp_prover
module Prover =
  String(struct
           let option_name = "-wp-proof"
           let default = "alt-ergo"
           let arg_name = "dp"
           let help =
             "Submit proof obligations to external prover:\n\
               - 'none' to skip proofs\n\
              Directly supported provers:\n\
               - 'alt-ergo' (default)\n\
               - 'coq', 'coqide' (see also -wp-script)\n\
              Supported provers via Why:\n\
               - 'simplify', 'vampire', 'yices', 'cvc3', 'z3', 'zenon'"
           let kind = `Tuning
         end)

let prover_names =
  [ "none" ;
    "coq" ; "coqide" ;
    "alt-ergo";
    "simplify";
    "vampire";
    "yices";
    "cvc3";
    "zenon";
    "z3" ]

let () = Prover.set_possible_values prover_names
let get_provers () = prover_names

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
           let kind = `Tuning
         end)
let () = on_reset Check.clear

let check_names =
  [ "none" ;
    "coq" ;
    "alt-ergo";
    "why" ]

let () = Plugin.set_group wp_prover
module Script =
  String(struct
           let option_name = "-wp-script"
           let arg_name = "f.script"
           let default = ""
           let help = "Set user's file for saving Coq proofs."
           let kind = `Tuning
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
       let kind = `Tuning
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
        let kind = `Tuning
      end)

let () = Plugin.set_group wp_prover
module Trace =
  False(struct
          let option_name = "-wp-trace"
          let help = "Keep labels in proof obligations (default: no)."
          let kind = `Tuning
        end)

let () = Plugin.set_group wp_prover
module ShareDir =
  String(struct
           let option_name = "-wp-share"
           let arg_name = "dir"
           let default = ""
           let help = "Directory where model specifications are found.\n\
                       Defaults: installation directory $FRAMAC_SHARE/wp"
           let kind = `Tuning
         end)

let () = Plugin.set_group wp_prover
module ProofTrace =
  False
    (struct
        let option_name = "-wp-proof-trace"
        let help = "Keeps output of provers for valid POs (default: no)"
        let kind = `Tuning
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
           let kind = `Tuning
         end)
let () = on_reset Print.clear

let () = Plugin.set_group wp_po
let () = Plugin.do_not_save ()
module Dot =
  False(struct
          let option_name = "-wp-dot"
          let help = "Generates dot files for wp computations."
          let kind = `Tuning
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
           let kind = `Tuning
         end)

let () = Plugin.set_group wp_po
module Details =
  False(struct
          let option_name = "-wp-warnings"
          let help = "Print details about warnings for 'stronger' and 'degenerated' goals"
          let kind = `Tuning
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

(*TODO: Projectifier cette reference*)
let output_dir = ref None

let make_output_dir dir =
  if Sys.file_exists dir then
    begin
      if not (Sys.is_directory dir) then
        abort "File '%s' is not a directory (WP aborted)" dir ;
      dir
    end
  else
    begin
      try
        Unix.mkdir dir 0o770 ;
        debug "Created output directory '%s'" dir ; dir
      with e ->
        debug "System error '%s'" (Printexc.to_string e) ;
        abort "Can not create output directory '%s'" dir
    end

let make_tmp_dir () =
  begin
    try Extlib.temp_dir_cleanup_at_exit "wp"
    with Extlib.Temp_file_error s ->
      abort "cannot create temporary file: %s" s
  end

let make_gui_dir () =
  try
    let home = try Sys.getenv "HOME" with Not_found -> "." in
    make_output_dir (home ^ "/.frama-c-wp")
  with _ ->
    make_tmp_dir ()

let get_output () =
  match !output_dir with
    | Some dir -> dir
    | None ->
        let dir =
          match OutputDir.get () with
            | "" ->
                if !Config.is_gui
                then make_gui_dir ()
                else make_tmp_dir ()
            | dir ->
                make_output_dir dir
        in
        output_dir := Some dir ; dir

let get_share () =
  match ShareDir.get() with
    | "" -> Config.datadir^"/wp"
    | dir ->
        if Sys.file_exists dir && Sys.is_directory dir then dir
          else abort "'%s': no such directory" dir


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
