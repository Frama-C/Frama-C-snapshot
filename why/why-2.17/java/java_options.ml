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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: java_options.ml,v 1.25 2008/11/05 14:03:15 filliatr Exp $ i*)

open Format

(*s The log file *)

let c = ref stdout

let log =
  c := open_out "krakatoa.log";
  Format.formatter_of_out_channel !c

let lprintf s = Format.fprintf log s

let close_log () =
  lprintf "End of log.@.";
  close_out !c;
  c := stdout

(*s environment variables *)

let libdir = 
  try
    let v = Sys.getenv "KRAKATOALIB" in
    lprintf "KRAKATOALIB is set to %s@." v;
    v
  with Not_found -> 
    let p = Version.libdir in
    lprintf "KRAKATOALIB is not set, using %s as default@." p;
    p

let rec split ch s =
  try
    let i = String.index s ch in
    let h = String.sub s 0 i
    and t = String.sub s (i+1) (String.length s - i - 1)
    in
    h::(split ch t)
  with
    Not_found -> [s]
     

let libfile = "krakatoa.why"

let javacard = ref false

      
(*s command-line options *)
      
let parse_only = ref false
let type_only = ref false
let abstract = ref ""
let print_graph = ref false
let debug = ref false
let verbose = ref false
let werror = ref false
let why_opt = ref ""
let ignore_overflow = ref false
let nonnull_sem = ref Java_env.NonNullNone
let minimal_class_hierarchy = ref false

(* Jessie options *)
let inv_sem = ref Jc_env.InvArguments
let separation_policy = ref Jc_env.SepNone
let annotation_sem = ref Jc_env.AnnotNone
let ai_domain = ref Jc_env.AbsNone

let add_why_opt s = why_opt := !why_opt ^ " " ^ s

let files_ = ref []
let add_file f = files_ := f :: !files_
let files () = List.rev !files_

let version () = 
  Printf.printf "This is Krakatoa version %s, compiled on %s
Copyright (c) 2006-2008 - INRIA team-project ProVal
This is free software with ABSOLUTELY NO WARRANTY (use option -warranty)
" Version.version Version.date;
  exit 0

let usage = "krakatoa [options] files"

let _ = 
  Arg.parse 
      [ "-parse-only", Arg.Set parse_only, 
	  "  stops after parsing";
        "-type-only", Arg.Set type_only, 
	  "  stops after typing";
	"-abstract", Arg.String ((:=) abstract),
	  " <file> stops after typing and output abstract view to <file>" ;
        "-print-call-graph", Arg.Set print_graph, 
	  "  stops after call graph and print call graph";
        "-d", Arg.Set debug,
          "  debugging mode";
        "-why-opt", Arg.String add_why_opt,
	  " <why options>  passes options to Why";
	"-v", Arg.Set verbose,
          "  verbose mode";
	"-q", Arg.Clear verbose,
          "  quiet mode (default)";
	"-werror", Arg.Set werror,
          "  treats warnings as errors";
	"-version", Arg.Unit version,
          "  prints version and exit";
	"-javacard", Arg.Set javacard,
	  "  source is Java Card";
	"-nonnull-sem", Arg.String
	  (function 
	     | "none" -> nonnull_sem := Java_env.NonNullNone
	     | "fields" -> nonnull_sem := Java_env.NonNullFields
	     | "all" -> nonnull_sem := Java_env.NonNullAll
	     | s -> raise (Arg.Bad ("Unknown nonnull_sem: " ^ s))),
	"  <kind> nonnull-by-default semantics: none (default), fields, all";
      ]
      add_file usage

let usage () =
  eprintf "usage: %s@." usage;
  exit 2

let parse_only = !parse_only
let type_only = !type_only
let print_graph = !print_graph
let debug = !debug
let verbose = !verbose
let werror = !werror
let why_opt = !why_opt

let classpath = 
  let p =
    try
      let v = Sys.getenv "KRAKATOACLASSPATH" in
	lprintf "KRAKATOACLASSPATH is set to %s@." v;
	split ':' v
    with Not_found ->      
      let p = Filename.concat libdir 
	(if !javacard then "javacard_api" else "java_api") 
      in
	lprintf "KRAKATOACLASSPATH is not set, using %s as default@." p;
	[p]
  in
    "." :: p 


(*s error handling *)

exception Java_error of Loc.position * string

let parsing_error l f = 
  Format.ksprintf 
    (fun s -> 
       let s = if s="" then s else " ("^s^")" in
       raise (Java_error(l, "syntax error" ^ s))) f


(*
Local Variables: 
compile-command: "make -j -C .. bin/krakatoa.byte"
End: 
*)

