(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: coptions.ml,v 1.48 2008/02/18 09:10:04 marche Exp $ i*)

open Format

(*s The log file *)

let c = ref stdout

let log =
  c := open_out "caduceus.log";
  Format.formatter_of_out_channel !c

let lprintf s = Format.fprintf log s

let close_log () =
  Format.fprintf log "End of log.@.";
  close_out !c;
  c := stdout

(*s environment variables *)

let libdir = 
  try
    let v = Sys.getenv "CADUCEUSLIB" in
    Format.fprintf log "CADUCEUSLIB is set to %s@." v;
    v
  with Not_found -> 
    let p = Cversion.libdir in
    Format.fprintf log 
      "CADUCEUSLIB is not set, using %s as default@." p;
    p

let whylib =
  try
    Sys.getenv "WHYLIB"
  with Not_found -> 
    Version.libdir

(*s command-line options *)

let zones = ref false
let show_time = ref false
let parse_only = ref false
let type_only = ref false
let print_norm = ref false
let print_graph = ref false
let cpp_command = ref "gcc -E -C"
let cpp_dump = ref false
let with_cpp = ref true
let debug = ref false
let verbose = ref false
let werror = ref false
let why_opt = ref ""
let coq_tactic = ref "intuition"
let separate = ref false
let no_zone_type = ref false
let closed_program = ref false
let floats = ref true
let local_aliasing = ref false
let arith_memory_model = ref false
let no_alloc_table = ref false 
let abstract_interp = ref false
let gen_invariant = ref false
let absint_as_proof = ref false

type fp_rounding_mode = 
  | RM_nearest_even | RM_to_zero | RM_up | RM_down | RM_nearest_away 
  | RM_dynamic
let fp_rounding_mode = ref RM_nearest_even
let set_fp_rounding_mode = function
  | "nearest_even" -> fp_rounding_mode := RM_nearest_even
  | "to_zero" -> fp_rounding_mode := RM_to_zero
  | "up" -> fp_rounding_mode := RM_up
  | "down" -> fp_rounding_mode := RM_down
  | "nearest_away" -> fp_rounding_mode := RM_nearest_away
  | _ -> 
      eprintf "rounding mode should be nearest_even, to_zero, up, down, or nearest_away@."; exit 1
let fp_overflow_check = ref false

type int_model = IMexact | IMbounded | IMmodulo

let int_model = ref IMexact
let set_int_model = function
  | "exact" -> 
      int_model := IMexact
  | "bounded" -> 
      int_model := IMbounded
  | "modulo" -> 
      int_model := IMmodulo
  | _ -> 
      eprintf 
	"integer arithmetic model should be `exact', `bounded' or `modulo'@."; 
      exit 1

let enum_check = ref false

let char_size_ = ref 8
let short_size_ = ref 16
let int_size_ = ref 32
let long_size_ = ref 32
let long_long_size_ = ref 64

let set_integer_size r s = 
  if s < 1 || s > 64 then begin
    eprintf "invalid integer size: %d (should be with 1..64)@." s; exit 1
  end;
  r := s

let add_why_opt s = why_opt := !why_opt ^ " " ^ s

let files_ = ref []
let add_file f = 
  if not (Sys.file_exists f) then begin
    eprintf "caduceus: %s: no such file@." f;
    exit 1
  end;
  files_ := f :: !files_
let files () = List.rev !files_

let version () = 
  Printf.printf "This is Caduceus version %s, compiled on %s
Copyright (c) 2003-2008 - Jean-Christophe Filliâtre, Thierry Hubert and Claude Marché
This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)
" Cversion.version Cversion.date;
  exit 0

type verification = All | Verify | Assume
let verification = ref All

let comma = Str.regexp "[ \t]*,[ \t]*"
let split = Str.split comma

let functions = Hashtbl.create 97

let verify s =
  separate := true;
  if !verification = Assume then begin
    Printf.eprintf "you cannot use both -verify and -assume\n"; exit 1
  end;
  verification := Verify;
  List.iter (fun f -> Hashtbl.add functions f ()) (split s)

let assume s =
  if !verification = Verify then begin
    eprintf "you cannot use both -verify and -assume\n"; exit 1
  end;
  verification := Assume;
  List.iter (fun f -> Hashtbl.add functions f ()) (split s)

let spec =
  [ "-parse-only", Arg.Set parse_only, 
  "  stops after parsing";
    "-type-only", Arg.Set type_only, 
  "  stops after typing";
    "-print-norm", Arg.Set print_norm, 
  "  stops after normalization and print C tree";
    "-print-call-graph", Arg.Set print_graph, 
  "  stops after call graph and print call graph";
    "-no-cpp", Arg.Clear with_cpp, 
  "  no C preprocessor";
    "-ccp", Arg.String ((:=) cpp_command), 
  " <cmd>  sets the C preprocessor";
    "-E", Arg.Set cpp_dump,
  "  stops after pre-processing and dump pre-processed file";
    "-d", Arg.Set debug,
  "  debugging mode";
    "-why-opt", Arg.String add_why_opt,
  " <why options>  passes options to Why";
    "-coq-tactic", Arg.String ((:=) coq_tactic),
  " <coq tactic>  Coq tactic for new goals";
    "-v", Arg.Set verbose,
  "  verbose mode";
    "-q", Arg.Clear verbose,
  "  quiet mode (default)";
    "--werror", Arg.Set werror,
  "  treats warnings as errors";
    "--version", Arg.Unit version,
  "  prints version and exit";
    "-s", Arg.Set separate,
  "  a separate file for each function";
    "-verify", Arg.String verify,
  " <f,g,h...>  specifies the functions to verify; implies -s";
    "-assume", Arg.String assume,
  " <f,g,h...>  specifies functions not to be verified (i.e. assumed)";
    "-closed", Arg.Set closed_program,
  "  assumes a closed program";
    "-separation", 
  Arg.Unit(fun () -> zones := true; closed_program := true),
  "  separates pointers into several zones (implies -closed)";	
    "-show-time", 
  Arg.Unit(fun () -> show_time := true;),
  " prints execution time ";
    "-no-zone-type", 
  Arg.Set no_zone_type,
  "  zones do not generate different why types";
    "--no-fp", Arg.Clear floats,
  "  do not use floating-point arithmetic";
    "--fp-rounding-mode", Arg.String set_fp_rounding_mode,
  "  set the default FP rounding mode";
    "--fp-overflow", Arg.Set fp_overflow_check,
  "  check for FP overflows";
    "-int-model", Arg.String set_int_model, 
  "  set the model for integer arithmetic (exact, bounded or modulo)";
    "-check-enum", Arg.Set enum_check,
  "  check for enum values";
    "-char-size", Arg.Int (set_integer_size char_size_),
  "<n>  set the size of type `char' (default is 8)";
    "-short-size", Arg.Int (set_integer_size short_size_),
  "  set the size of type `short' (default is 16)";
    "-int-size", Arg.Int (set_integer_size int_size_),
  "  set the size of type `int' (default is 32)";
    "-long-size", Arg.Int (set_integer_size long_size_),
  "  set the size of type `long' (default is 32)";
    "-long-long-size", Arg.Int (set_integer_size long_long_size_),
  "  set the size of type `long long' (default is 64)";
    "--loc-alias", Arg.Set local_aliasing,
  "  local aliasing analysis (experimental)";
    "--arith-mem", Arg.Set arith_memory_model,
  "  alternate arithmetic memory model (experimental)";
    "--abs-int", Arg.Set abstract_interp,
  "  local abstract interpretation over integers (experimental)";
    "--inv-gen", Arg.Set gen_invariant,
  "  invariant generation by local abstract interpretation over integers (experimental)";
    "--abs-int-proof", Arg.Set absint_as_proof,
  "  proof by local abstract interpretation over integers (experimental)";
  ]

let () = Arg.parse spec add_file "caduceus [options] file..."

let () = 
  if !files_ = [] then begin
    Arg.usage spec "usage: caduceus [options] file...\nOptions are:";
    exit 1
  end

let zones = !zones
let show_time = !show_time
let no_zone_type = !no_zone_type
let parse_only = !parse_only
let type_only = !type_only
let print_norm = !print_norm
let print_graph = !print_graph
let debug = !debug
let verbose = !verbose
let werror = !werror
let with_cpp = !with_cpp
let cpp_command = !cpp_command
let cpp_dump = !cpp_dump
let coq_tactic = !coq_tactic
let separate = !separate
let closed_program = !closed_program
let local_aliasing = !local_aliasing
let arith_memory_model = !arith_memory_model
let no_alloc_table = !no_alloc_table
let abstract_interp = !abstract_interp
let gen_invariant = !gen_invariant
let absint_as_proof = !absint_as_proof

let floats = !floats
let fp_overflow_check = !fp_overflow_check
let dft_fp_rounding_mode = !fp_rounding_mode

let char_size = !char_size_
let short_size = !short_size_
let int_size = !int_size_
let long_size = !long_size_
let long_long_size = !long_long_size_

let int_model = !int_model
let machine_ints = int_model = IMbounded || int_model = IMmodulo

let enum_check = !enum_check

let libfile =
  if arith_memory_model then
    "caduceus_arith.why"
  else
    "caduceus.why"

let use_floats = ref false

let why_opt () = 
  let o = !why_opt in
  (*let o = if int_overflow_check then o ^ " --lib-file marith.why" else o in*)
  if floats && !use_floats then o ^ " --fp" else o

let verify f = match !verification with
  | All -> true
  | Verify -> Hashtbl.mem functions f
  | Assume -> not (Hashtbl.mem functions f)

type evaluation_order_t =
    { binary_left_to_right : bool;
      assign_left_to_right : bool;
      call_left_to_right : bool }

let evaluation_order = 
  { binary_left_to_right = true;
    assign_left_to_right = false;
    call_left_to_right = true }

(*
Local Variables: 
compile-command: "make -j -C .. bin/caduceus.byte"
End: 
*)
