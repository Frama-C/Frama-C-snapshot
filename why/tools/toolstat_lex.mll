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


(*i $Id: toolstat_lex.mll,v 1.13 2008/11/25 12:44:22 moy Exp $ i*)

{
  open Toolstat_pars
  open Lexing
  open Format
  open Pp

  let debug = ref false
  let debug_more = ref false
  let no_parsing = ref false

  let extract_pos s c =
    let rec aux s acc i =
      try 
	let n = String.index s c in
	aux (String.sub s (n+1) (String.length s - (n+1))) ((i+n)::acc) (i+n+1)
      with Not_found -> acc
    in
    aux s [] 0

  let detailed_result s n1 n2 n3 n4 n5 =
    let valid_pos = extract_pos s '.' in 
    assert (List.length valid_pos = n1);
    let invalid_pos = extract_pos s '*' in
    assert (List.length invalid_pos = n2);
    let unknown_pos = extract_pos s '?' in
    assert (List.length unknown_pos = n3);
    let timeout_pos = extract_pos s '#' in
    assert (List.length timeout_pos = n4);
    let failure_pos = extract_pos s '!' in
    assert (List.length failure_pos = n5);
    (n1, n2, n3, n4, n5),
    (valid_pos, invalid_pos, unknown_pos, timeout_pos, failure_pos)

  let opt_as_float = function 
    | Some t -> float_of_string t 
    | None -> 0.

  let opt_as_int = function 
    | Some t -> int_of_string t 
    | None -> 0

  let as_int = int_of_string 

  let loc lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let num_eq = ref 0
  let num_diseq = ref 0
  let num_ineq = ref 0
}

let ws = [' ' '\t' '\012' '\r']
let backslash_escapes = ['\\' '"' '\'' 'n' 't' 'b' 'r' 'f' ]
let num = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_' '-']
let alphanum = alpha | num
let id = alphanum*
let filechar = ['.' '/' '\\']
let file = (alphanum | filechar)*
let special = ['.' '*' '?' '#' '!']
let result = special*
let int = num+
let real = num* '.' num* | int

rule token = parse
  | "\nInferring "
      { 
	num_eq := 0;
	num_diseq := 0;
	num_ineq := 0;
	token lexbuf
      }
  | "postcondition" 
      {
	if !debug then printf "post@."; 
	POST
      }
  | "precondition" 
      {
	if !debug then printf "pre@."; 
	PRE
      }
  | "loop invariant" 
      {
	if !debug then printf "loopinv@."; 
	LOOPINV
      }
  | "backward loop invariant" 
      {
	if !debug then printf "bwd_loopinv@."; 
	BWD_LOOPINV
      }
  | "==" | "!=" | "<=" | ">=" | "<" | ">"
      {
	if !debug then printf "relation@."; 
	RELATION
      }
  | "\nDoing " ws* (id as s)
      { 
	newline lexbuf; 
	if !debug then printf "project %s@." s; 
	PROJECT(s)
      }
  | "\nRunning " (id as s) " on proof obligations"
      { 
	newline lexbuf; 
	if !debug then printf "prover %s@." s; 
	PROVER(s)
      }
  | "\n" id "/" (file as f) ("_why." id) ws* ':' ws* 
      { 
	newline lexbuf; 
	if !debug then printf "test %s@." f; 
	TEST(f)
      }
  | "\n" id "/" (file "_po_" int as f) ".why" ws* ':' ws* 
      { 
	newline lexbuf; 
	if !debug then printf "test %s@." f; 
	TEST(f)
      }
  | (result as s) ws*
      '(' (int as n1) '/' (int as n2) 
      '/' (int as n3) '/' (int as n4) '/' (int as n5) ')'  
      { 
	if !debug then printf "result %s@." s; 
	RESULT
	  (detailed_result s 
	     (as_int n1) (as_int n2) (as_int n3) (as_int n4) (as_int n5)) 
      }
  | "\ntotal CPU time" ws* ':' ws*
      ((real as h) " h")* ws* ((real as m) " m")* ws* ((real as s) " sec")* 
      { 
	newline lexbuf; 
	if !debug then printf "time %a h %a m %a s@." (print_option string) h 
	  (print_option string) m (print_option string) s; 
	TIME(opt_as_int h, opt_as_int m, opt_as_float s) 
      }
  | '\n'
      { newline lexbuf; token lexbuf }
  | _                                              
      { 
	if !debug_more then 
	  (printf "other token %s@." (lexeme lexbuf);
	   flush_all ());
	token lexbuf 
      }
  | eof
      { EOF }

{
  let parse lb = 
    try
      if !no_parsing then (Toolstat_pars.all token lb; ([],[]))
      else Toolstat_pars.log token lb
    with Parsing.Parse_error ->
      Format.eprintf "%a@." Loc.gen_report_position (loc lb);
      raise Parsing.Parse_error
}
