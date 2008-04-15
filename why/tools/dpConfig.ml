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



type prover_id = 
    Simplify | Harvey | Cvcl | Zenon | Rvsat | Yices | Ergo | ErgoSelect
  | Cvc3 | Graph | Z3 | Coq

type prover_data =
  {
    name : string;
    is_interactive : bool;
    mutable version: string;
    version_switch : string;
    version_regexp : string;
    mutable command : string;
    command_switches : string;
    valid_regexp : string;
    mutable valid_cregexp : Str.regexp option;
    undecided_regexp : string;
    mutable undecided_cregexp : Str.regexp option;
  }
    
let alt_ergo =
  {
    name = "Alt-Ergo";
    is_interactive = false;
    version = "";
    version_switch = "-version";
    version_regexp = "Ergo \\([^ ]*\\)";
    command = "alt-ergo";
    command_switches = "";
    valid_regexp = "\\bValid\\b";
    valid_cregexp = None;
    undecided_regexp = "\\bI don't know\\b\\|\\bInvalid\\b";
    undecided_cregexp = None;
  }

let simplify = 
  {
    name = "Simplify";
    is_interactive = false;
    version = "";
    version_switch = "-version";
    version_regexp = "Simplify version \\([^ ,]+\\)";
    command = "Simplify";
    command_switches = "";
    valid_regexp = "\\bValid\\b";
    valid_cregexp = None;
    undecided_regexp = "\\bInvalid\\b";
    undecided_cregexp = None;
  }

let z3 =
  {
    name = "Z3";
    is_interactive = false;
    version = "";
    version_switch = "-version";
    version_regexp = "Z3 version \\([^ \r]+\\)";
    command = "z3";
    command_switches = "-smt";
(*
"wine /home/cmarche/.wine/drive_c/Program\ Files/Microsoft\ Research/Z3-1.3.6/bin/z3.exe";
    
*)
    valid_regexp = "\\bunsat\\b";
    valid_cregexp = None;
    undecided_regexp = "\\bunknown\\b\\|\\bsat\\b";
    undecided_cregexp = None;
  }


let yices =    
  {
    name = "Yices";
    is_interactive = false;
    version = "";
    version_switch = "--version";
    version_regexp = "\\([^ ]+\\)";
    command = "yices";
    command_switches = "-pc 0 -smt < ";
    valid_regexp = "\\bunsat\\b";
    valid_cregexp = None;
    undecided_regexp = "\\bunknown\\b\\|\\bsat\\b\\|feature not supported: non linear problem";
    undecided_cregexp = None;
  }

let cvc3 =    
  {
    name = "CVC3";
    is_interactive = false;
    version = "";
    version_switch = "-version";
    version_regexp = "This is CVC3 version \\([^ ]+\\)";
    command = "cvc3";
    command_switches = "-lang smt < ";
    valid_regexp = "\\bunsat\\b";
    valid_cregexp = None;
    undecided_regexp = "\\bunknown\\b\\|\\bsat\\b";
    undecided_cregexp = None;
  }

let coq =    
  {
    name = "Coq";
    is_interactive = true;
    version = "";
    version_switch = "-v";
    version_regexp = "The Coq Proof Assistant, version \\([^ ]+\\)";
    command = "coqc";
    command_switches = "";
    valid_regexp = "\\bunsat\\b";
    valid_cregexp = None;
    undecided_regexp = "Error while reading";
    undecided_cregexp = None;
  }


let prover_list = 
  [
    Ergo, (alt_ergo, ["alt-ergo" ; "ergo"]) ;
    Simplify, (simplify, ["Simplify" ; "simplify"]) ;
    Z3, (z3, ["z3"]) ;
    Yices, (yices, ["yices"]) ;
    Cvc3, (cvc3, ["cvc3"]) ;
    Coq, (coq, ["coqc"]);
  ] 

let rc_file () =
  let home =
    try Sys.getenv "HOME"
    with Not_found -> 
      (* try windows env var *)
      try Sys.getenv "USERPROFILE"
      with Not_found -> ""
  in
  Filename.concat home ".whyrc"

open Format
  
let load_prover_info p key l =
  List.iter
    (function 
       | ("version",Rc.RCstring s) -> p.version <- s
       | ("command",Rc.RCstring s) -> p.command <- s
       | (field,_) ->
	   printf "Unknown field `%s' in section [%s] of rc file@." field key)
    l
	 
let load_rc_file () = 
  let rc = Rc.from_file (rc_file ()) in
  List.iter
    (fun (key,args) ->
       match key with
	 | "Alt-Ergo" -> load_prover_info alt_ergo key args
	 | "Simplify" -> load_prover_info simplify key args
	 | "Z3" -> load_prover_info z3 key args
	 | "Yices" -> load_prover_info yices key args
	 | "CVC3" -> load_prover_info cvc3 key args
	 | "Coq" -> load_prover_info coq key args
	 | _ -> 
	     printf "Unknown section [%s] in rc file@." key)
    rc
	     

let save_prover_info fmt p =
  fprintf fmt "[%s]@." p.name;
  fprintf fmt "version = \"%s\"@." p.version;
  fprintf fmt "command = \"%s\"@." p.command;
  fprintf fmt "@."

let save_rc_file () =
  let ch = open_out (rc_file ()) in
  let fmt = Format.formatter_of_out_channel ch in
  List.iter (fun (_,(p,_)) -> save_prover_info fmt p) prover_list;
  close_out ch

