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

(*i $Id: simplify_lexer.mll,v 1.12 2008/11/05 14:03:19 filliatr Exp $ i*)

{

  open Lexing 
  open Simplify_ast
  open Simplify_parser

  let atoms = Hashtbl.create 1021
  let () = 
    List.iter (fun (s,a) -> Hashtbl.add atoms s a)
      [
    "DEFPRED", DEFPRED;
    "BG_PUSH", BG_PUSH;
    "@true", AT_TRUE;
    "TRUE", TRUE;
    "FALSE", FALSE;
    "AND", AND;
    "IMPLIES", IMPLIES;
    "IFF", IFF;
    "FORALL", FORALL;
    "EXISTS", EXISTS;
    "MPAT", MPAT;
    "PATS", PATS;
    "AND", AND;
    "OR", OR;
    "NOT", NOT;
    "LBLPOS", LBLPOS;
    "LBLNEG", LBLNEG;
    "DISTINCT", DISTINCT;
    "EQ", EQ;
    "NEQ", NEQ;
    "+", IDENT "+";
    "-", IDENT "-";
    "*", IDENT "*";
    ">", GT;
    ">=", GE;
    "<", LT;
    "<=", LE;
      ]

  let mk_ident s =
    let is_char_ok i = function
      | 'a'..'z' | 'A'..'Z' | '_' -> true
      | '0'..'9' when i > 0 -> true
      | _ -> false
    in
    for i = 0 to String.length s - 1 do
      if not (is_char_ok i s.[i]) then s.[i] <- 'a'
    done;
    if Hashtbl.mem atoms s then begin
      let rec loop n = 
	let sn = s ^ string_of_int n in
	if Hashtbl.mem atoms sn then loop (n+1) else sn
      in
      loop 0
    end else
      s

  let atom s =
    try
      Hashtbl.find atoms s
    with Not_found ->
      let s' = mk_ident (String.copy s) in
      (* Format.eprintf "atom %s -> %s@." s s'; *)
      let a = IDENT s' in 
      Hashtbl.add atoms s a; a

}

let space = [' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z']+

rule token = parse
  | space+
      { token lexbuf }
  | ";" [^'\n']* '\n' 
      { token lexbuf }
  | "(" 
      { LPAR }
  | ")" 
      { RPAR }
  | eof 
      { EOF }
  | ('-'? ['0'-'9']+) as s
      { INTEGER s }
  | '|' ([^ '|']+ as s) '|'
      { atom s }
  | [^' ' '\t' '\n' '\r' ';' '(' ')']+ as s
      { atom s }
  | _ as c 
      { Format.eprintf  "simplify_lexer: illegal character %c@." c; exit 1 }

{

}

