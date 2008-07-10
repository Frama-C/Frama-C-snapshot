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

{
  open Lexing 
  open Mix_ast
  open Mix_parser

  let keywords = Hashtbl.create 97
  let () = 
    List.iter 
      (fun (x,y) -> Hashtbl.add keywords x y)
      [ (* loading *)
	"lda", INSTR (Ld A);  "ldx", INSTR (Ld X);
	"ld1", INSTR (Ld I1); "ld2", INSTR (Ld I2); "ld3", INSTR (Ld I3);
	"ld4", INSTR (Ld I4); "ld5", INSTR (Ld I5); "ld6", INSTR (Ld I6);
	"ldan", INSTR (Ldn A);  "ldxn", INSTR (Ldn X);
	"ld1n", INSTR (Ldn I1); "ld2n", INSTR (Ldn I2); "ld3n", INSTR (Ldn I3);
	"ld4n", INSTR (Ldn I4); "ld5n", INSTR (Ldn I5); "ld6n", INSTR (Ldn I6);
	(* storing *)
	"sta", INSTR (St A);  "stx", INSTR (St X);
	"st1", INSTR (St I1); "st2", INSTR (St I2); "st3", INSTR (St I3);
	"st4", INSTR (St I4); "st5", INSTR (St I5); "st6", INSTR (St I6);
	"stj", INSTR Stj;     "stz", INSTR Stz;
	(* arithmetic *)
	"add", INSTR Add; "sub", INSTR Sub; 
	"mul", INSTR Mul; "div", INSTR Div;
	"srb", INSTR Srb;
	(* address transfer *)
	"enta", INSTR (Ent A);  "entx", INSTR (Ent X);
	"ent1", INSTR (Ent I1); "ent2", INSTR (Ent I2); "ent3", INSTR (Ent I3);
	"ent4", INSTR (Ent I4); "ent5", INSTR (Ent I5); "ent6", INSTR (Ent I6);
	"enna", INSTR (Enn A);  "ennx", INSTR (Enn X);
	"enn1", INSTR (Enn I1); "enn2", INSTR (Enn I2); "enn3", INSTR (Enn I3);
	"enn4", INSTR (Enn I4); "enn5", INSTR (Enn I5); "enn6", INSTR (Enn I6);
	"inca", INSTR (Inc A);  "incx", INSTR (Inc X);
	"inc1", INSTR (Inc I1); "inc2", INSTR (Inc I2); "inc3", INSTR (Inc I3);
	"inc4", INSTR (Inc I4); "inc5", INSTR (Inc I5); "inc6", INSTR (Inc I6);
	"deca", INSTR (Dec A);  "decx", INSTR (Dec X);
	"dec1", INSTR (Dec I1); "dec2", INSTR (Dec I2); "dec3", INSTR (Dec I3);
	"dec4", INSTR (Dec I4); "dec5", INSTR (Dec I5); "dec6", INSTR (Dec I6);
        (* comparison *)
	"cmpa", INSTR (Cmp A);  "cmpx", INSTR (Cmp X);
	"cmp1", INSTR (Cmp I1); "cmp2", INSTR (Cmp I2); "cmp3", INSTR (Cmp I3);
	"cmp4", INSTR (Cmp I4); "cmp5", INSTR (Cmp I5); "cmp6", INSTR (Cmp I6);
	(* jumps *)
	"jmp", INSTR Jmp; "jsj", INSTR Jsj; "jl", INSTR Jl;
	"jg", INSTR Jg; "jge", INSTR Jge; "je", INSTR Je; "jne", INSTR Jne;
	"jan", INSTR (Jn A);  "jxn", INSTR (Jn X);
	"j1n", INSTR (Jn I1); "j2n", INSTR (Jn I2); "j3n", INSTR (Jn I3);
	"j4n", INSTR (Jn I4); "j5n", INSTR (Jn I5); "j6n", INSTR (Jn I6);
	"jaz", INSTR (Jz A);  "jxz", INSTR (Jz X);
	"j1z", INSTR (Jz I1); "j2z", INSTR (Jz I2); "j3z", INSTR (Jz I3);
	"j4z", INSTR (Jz I4); "j5z", INSTR (Jz I5); "j6z", INSTR (Jz I6);
	"jap", INSTR (Jp A);  "jxp", INSTR (Jp X);
	"j1p", INSTR (Jp I1); "j2p", INSTR (Jp I2); "j3p", INSTR (Jp I3);
	"j4p", INSTR (Jp I4); "j5p", INSTR (Jp I5); "j6p", INSTR (Jp I6);
	"jann", INSTR (Jnn A);  "jxnn", INSTR (Jnn X);
	"j1nn", INSTR (Jnn I1); "j2nn", INSTR (Jnn I2); "j3nn", INSTR (Jnn I3);
	"j4nn", INSTR (Jnn I4); "j5nn", INSTR (Jnn I5); "j6nn", INSTR (Jnn I6);
	"janz", INSTR (Jnz A);  "jxnz", INSTR (Jnz X);
	"j1nz", INSTR (Jnz I1); "j2nz", INSTR (Jnz I2); "j3nz", INSTR (Jnz I3);
	"j4nz", INSTR (Jnz I4); "j5nz", INSTR (Jnz I5); "j6nz", INSTR (Jnz I6);
	"janp", INSTR (Jnp A);  "jxnp", INSTR (Jnp X);
	"j1np", INSTR (Jnp I1); "j2np", INSTR (Jnp I2); "j3np", INSTR (Jnp I3);
	"j4np", INSTR (Jnp I4); "j5np", INSTR (Jnp I5); "j6np", INSTR (Jnp I6);
	(* other *)
	"nop", INSTR Nop;
	"hlt", INSTR Hlt;
	(* pseudo *)
	"equ", EQU;
	"orig", ORIG;
      ]

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let string_buf = Buffer.create 1024

  let char_for_backslash = function
    | 'n' -> '\n'
    | 't' -> '\t'
    | c -> c

  exception Lexical_error of string

  (* local symbols *)

  let local_symbols = Array.create 10 (0,0)

  let make_local_symbol i n = Printf.sprintf "local_symbol_%d_%d" i n

  let line lexbuf = (lexeme_start_p lexbuf).pos_lnum

  let local_symbol lexbuf i =
    let (n,_) = local_symbols.(i) in
    local_symbols.(i) <- (n+1, line lexbuf);
    make_local_symbol i (n+1)

  let forward_local_symbol _ i =
    let (n,_) = local_symbols.(i) in
    make_local_symbol i (n+1)

  let backward_local_symbol lexbuf i =
    let (n,l) = local_symbols.(i) in
    make_local_symbol i (if line lexbuf > l then n else n-1)

}

let newline = '\n'
let space = [' ' '\t' '\r']
let alpha = ['a'-'z' 'A'-'Z']
let letter = alpha | '_'
let digit = ['0'-'9']
let ident = letter (letter | digit | '\'')*
let integer = digit+

rule token = parse
  | newline 
      { newline lexbuf; token lexbuf }
  | space+  
      { token lexbuf }
  | ident as id  
      { let lid = String.lowercase id in
	try Hashtbl.find keywords lid with Not_found -> IDENT id }
  | ident as id ":"
      { LABEL id }
  | digit as d "H:"
      { LABEL (local_symbol lexbuf (Char.code d - Char.code '0')) }
  | digit as d "F"
      { IDENT (forward_local_symbol lexbuf (Char.code d - Char.code '0')) }
  | digit as d "B"
      { IDENT (backward_local_symbol lexbuf (Char.code d - Char.code '0')) }
  | integer as n
      { INTEGER n }
  | "{{{"
      { Buffer.clear string_buf; verbatim lexbuf }
  | "{{" | "/*i"
      { Buffer.clear string_buf; invariant lexbuf }
  | "{" | "/*a"
      { Buffer.clear string_buf; assertion lexbuf }
  | "/*"
      { comment lexbuf; token lexbuf }
  | "\""      { Buffer.clear string_buf; string lexbuf }
  | ","
      { COMMA }
  | ":"
      { COLON }
  | "("
      { LPAR }
  | ")"
      { RPAR }
  | "+"
      { PLUS }
  | "-"
      { MINUS }
  | "*"
      { STAR }
  | eof 
      { EOF }
  | _ as c
      { raise (Lexical_error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | "*/" 
      { () }
  | "/*" 
      { comment lexbuf; comment lexbuf }
  | newline 
      { newline lexbuf; comment lexbuf }
  | eof
      { raise (Lexical_error "unterminated comment") }
  | _ 
      { comment lexbuf }

and string = parse
  | "\""
      { STRING (Buffer.contents string_buf) }
  | "\\" (_ as c)
      { Buffer.add_char string_buf (char_for_backslash c); string lexbuf }
  | newline 
      { newline lexbuf; Buffer.add_char string_buf '\n'; string lexbuf }
  | eof
      { raise (Lexical_error "unterminated string") }
  | _ as c
      { Buffer.add_char string_buf c; string lexbuf }

and verbatim = parse
  | "}}}" 
      { VERBATIM (Buffer.contents string_buf) }
  | eof
      { raise (Lexical_error "unterminated verbatim") }
  | newline 
      { newline lexbuf; Buffer.add_char string_buf '\n'; verbatim lexbuf }
  | _ as c
      { Buffer.add_char string_buf c; verbatim lexbuf }

and invariant = parse
  | "}}" | "i*/"
      { INVARIANT (Buffer.contents string_buf) }
  | eof
      { raise (Lexical_error "unterminated invariant") }
  | newline 
      { newline lexbuf; Buffer.add_char string_buf '\n'; invariant lexbuf }
  | _ as c
      { Buffer.add_char string_buf c; invariant lexbuf }

and assertion = parse
  | "}" | "a*/"
      { ASSERT (Buffer.contents string_buf) }
  | eof
      { raise (Lexical_error "unterminated assert") }
  | newline 
      { newline lexbuf; Buffer.add_char string_buf '\n'; assertion lexbuf }
  | _ as c
      { Buffer.add_char string_buf c; assertion lexbuf }
