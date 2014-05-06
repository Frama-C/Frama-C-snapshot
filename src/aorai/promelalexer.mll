(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

(* $Id: promelalexer.mll,v 1.2 2008-10-02 13:33:29 uid588 Exp $ *)

(* from http://www.ltl2dstar.de/down/ltl2dstar-0.4.2.zip *)

{
  open Promelaparser
  open Lexing

  exception Error of (Lexing.position * Lexing.position) * string

  let loc lexbuf = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  let raise_located loc e = raise (Error (loc, e))

  let buf = Buffer.create 1024

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let rD =        ['0'-'9']
let rL = ['a'-'z' 'A'-'Z' '_']


rule token = parse
  | "true"                  { PROMELA_TRUE }
  | "never"                 { PROMELA_NEVER }
  | "if"                    { PROMELA_IF }
  | "fi"                    { PROMELA_FI }
  | "goto"                  { PROMELA_GOTO }
  | "skip"                  { PROMELA_SKIP }
  | "::"                    { PROMELA_DOUBLE_COLON }
  | ':'                     { PROMELA_COLON }
  | ';'                     { PROMELA_SEMICOLON }
  | '('                     { PROMELA_LPAREN }
  | ')'                     { PROMELA_RPAREN }
  | '{'                     { PROMELA_LBRACE }
  | '}'                     { PROMELA_RBRACE }
  | "->"                    { PROMELA_RIGHT_ARROW }
  | "false"                 { PROMELA_FALSE }
  | "||"                    { PROMELA_OR }
  | "&&"                    { PROMELA_AND }
  | '!'                     { PROMELA_NOT }
  | [' ' '\t' '\012' '\r']+ { token lexbuf }
  | '\n'                    { newline lexbuf; token lexbuf }
  | "/*"                    { comment lexbuf; token lexbuf }
  | "//" [^ '\n']* '\n'     { newline lexbuf; token lexbuf }

  | "callof_" rL* (rL | rD)*
                            { let s=(lexeme lexbuf) in
                              let s=String.sub s 7 ((String.length s)-7) in
                              PROMELA_CALLOF s }
  | "returnof_" rL* (rL | rD)*
                            { let s=(lexeme lexbuf) in
                              let s=String.sub s 9 ((String.length s)-9) in
                              PROMELA_RETURNOF s }
  | "callorreturnof_" rL* (rL | rD)*
                            { let s=(lexeme lexbuf) in
                              let s=String.sub s 15 ((String.length s)-15) in
                              PROMELA_CALLORRETURNOF s }


  | "callof_"               { raise_located (loc lexbuf) "Illegal fonction name in Promela file." }
  | "returnof_"             { raise_located (loc lexbuf) "Illegal fonction name in Promela file." }
  | "callorreturnof_"       { raise_located (loc lexbuf) "Illegal fonction name in Promela file." }



  | rL (rL | rD)*           { let s = lexeme lexbuf in
                                PROMELA_LABEL s }
  | eof                     { EOF }

  | "1"                     { PROMELA_TRUE }
  | _                       { Aorai_option.error "Illegal_character : '%s'\n" (lexeme lexbuf);
                              raise Parsing.Parse_error}




and comment = parse
  | "*/" { () }
  | eof  {  Aorai_option.error "Unterminated_comment\n"  (*lex_error lexbuf "Unterminated_comment"*) }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }


{
  let parse c =
    let lb = from_channel c in
    try
      Promelaparser.promela token lb
    with
        Parsing.Parse_error
      | Invalid_argument _ ->
          let (a,b)=(loc lb) in
                   Aorai_option.error "Syntax error (l%d c%d -> l%dc%d)" a.pos_lnum (a.pos_cnum-a.pos_bol) b.pos_lnum (b.pos_cnum-b.pos_bol);
(*          Format.print_string "Syntax error (" ;   *)
(*          Format.print_string "l" ;                *)
(*          Format.print_int a.pos_lnum ;            *)
(*          Format.print_string "c" ;                *)
(*          Format.print_int (a.pos_cnum-a.pos_bol) ;*)
(*          Format.print_string " -> l" ;            *)
(*          Format.print_int b.pos_lnum ;            *)
(*          Format.print_string "c" ;                *)
(*          Format.print_int (b.pos_cnum-b.pos_bol) ;*)
(*          Format.print_string ")\n" ;              *)
            raise_located (loc lb) "Syntax error"



}
