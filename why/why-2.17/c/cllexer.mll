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

(*i $Id: cllexer.mll,v 1.58 2008/11/05 14:03:13 filliatr Exp $ i*)

(* tokens for the C annotations *)

{

  open Clparser
  open Lexing
  open Cerror
  open Clogic

  let loc lexbuf = 
    (Loc.reloc (lexeme_start_p lexbuf), Loc.reloc (lexeme_end_p lexbuf))

  let lex_error lexbuf s =
    Creport.raise_located (loc lexbuf) (AnyMessage ("lexical error: " ^ s))

  let identifier = 
    let h = Hashtbl.create 97 in
    List.iter (fun (i,t) -> Hashtbl.add h i t)
      [
	"if", IF;
	"then", THEN;
	"else", ELSE;
	"invariant", INVARIANT;
	"variant", VARIANT;
	"decreases", DECREASES;
	"for", FOR;
	"assert", ASSERT;
	"assume", ASSUME;
	"label", LABEL;
	"requires", REQUIRES;
	"ensures", ENSURES ;
	"assigns", ASSIGNS;
	"loop_assigns", LOOP_ASSIGNS;
	"reads", READS;
	"logic", LOGIC;
	"predicate", PREDICATE;
	"axiom", AXIOM;
	"int", INT;
	"integer", INTEGER;
	"float", FLOAT;
	"void", VOID;
	"char", CHAR;
	"signed", SIGNED;
	"unsigned", UNSIGNED;
	"short", SHORT;
	"long", LONG;
	"double", DOUBLE;
	"real", REAL;
	"struct", STRUCT;
	"enum", ENUM;
	"union", UNION;
	"ghost", GHOST;
	"set", SET;
	"type", TYPE;
      ];
    fun s -> try Hashtbl.find h s with Not_found -> IDENTIFIER s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with 
	  pos_lnum = pos.pos_lnum + 1; 
	  pos_bol = pos.pos_cnum + !Loc.current_offset }
}

let space = [' ' '\t' '\012' '\r']

let rD =	['0'-'9']
let rL = ['a'-'z' 'A'-'Z' '_']
let rH = ['a'-'f' 'A'-'F' '0'-'9']
let rE = ['E''e']['+''-']? rD+
let rFS	= ('f'|'F'|'l'|'L')
let rIS = ('u'|'U'|'l'|'L')*

rule token = parse
  | '@' | [' ' '\t' '\012' '\r']+ { token lexbuf }
  | '\n' { newline lexbuf; token lexbuf }
  | "//" [^'\n']* ('\n'|eof) { newline lexbuf; token lexbuf }
  | "\\forall"  { FORALL }
  | "\\exists"  { EXISTS }
  | "=>" { IMPLIES }
  | "<=>" { IFF }
  | "&"     { AMP }
  | "&&"     { AND }
  | "|"      { BAR }
  | "||"      { OR }
  | "!"     { NOT }
  | "~"     { TILDE }
  | "\\true"    { TRUE }
  | "\\false"   { FALSE }
  | "\\old"    { OLD }
  | "\\at"    { AT }
  | "\\result" { RESULT }
  | "\\base_addr" { BASE_ADDR }
  | "\\offset" { OFFSET }
  | "\\block_length" { BLOCK_LENGTH }
  | "\\arrlen" { ARRLEN }
  | "\\strlen" { STRLEN }
  | "\\min" { MIN }
  | "\\max" { MAX }
  | "\\min_range" { MININT }
  | "\\max_range" { MAXINT }
  | "\\valid" { VALID }
  | "\\separated" { SEPARATED }
  | "\\bound_separated" { BOUND_SEPARATED }
  | "\\full_separated" { FULL_SEPARATED }
  | "\\fullseparated" { FULLSEPARATED }
  | "\\fresh" { FRESH }
  | "\\valid_index" { VALID_INDEX }
  | "\\valid_range" { VALID_RANGE }
  | "\\nothing"   { NOTHING }
  | "\\null" { NULL }
  | "\\abs" { ABS }
  | "\\sqrt" { SQRT }
  | "\\round_error" { ROUNDERROR }
  | "\\total_error" { TOTALERROR }
  | "\\exact" { EXACT }
  | "\\model" { MODEL }

  (* Why keywords are reserved *)
  | "in" 
      { lex_error lexbuf ("reserved keyword `" ^ lexeme lexbuf ^ "'") }
 
  | rL (rL | rD)*       { let s = lexeme lexbuf in identifier s }

  | '0'['x''X'] rH+ rIS?    { CONSTANT (IntConstant (lexeme lexbuf)) }
  | '0' rD+ rIS?            { CONSTANT (IntConstant (lexeme lexbuf)) }
  | rD+ rIS?                { CONSTANT (IntConstant (lexeme lexbuf)) }
  | 'L'? "'" [^ '\n' '\'']+ "'"     { CONSTANT (IntConstant (lexeme lexbuf)) }

  | rD+ rE rFS?             { CONSTANT (RealConstant (lexeme lexbuf)) }
  | rD* "." rD+ (rE)? rFS?  { CONSTANT (RealConstant (lexeme lexbuf)) }
  | rD+ "." rD* (rE)? rFS?  { CONSTANT (RealConstant (lexeme lexbuf)) }
  (* hack to lex 0..3 as 0 .. 3 and not as 0. .3 *)
  | (rD+ as n) ".."         { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 2;
			      CONSTANT (IntConstant n) }
  | 'L'? '"' [^ '"']* '"'   { STRING_LITERAL (lexeme lexbuf) }

  | "@"                     { AT }
  | ","                     { COMMA }
  | "->"                    { ARROW }
  | "?"                     { QUESTION }
  | ";"                     { SEMICOLON }
  | ":"                     { COLON }
  | "::"                    { COLONCOLON }
  | "."                     { DOT }
  | ".."                    { DOTDOT }
  | "-"                     { MINUS }
  | "+"                     { PLUS }
  | "*"                     { STAR }
  | "&"                     { AMP }
  | "/"                     { SLASH }
  | "%"                     { PERCENT }
  | "<"                     { LT }
  | "<<"                    { LTLT }
  | ">"                     { GT }
  | ">>"                    { GTGT }
  | "<="                    { LE }
  | ">="                    { GE }
  | "=="                    { EQ }
  | "!="                    { NE }
  | "("                     { LPAR }
  | ")"                     { RPAR }
  | "{"                     { LBRACE }
  | "}"                     { RBRACE }
  | ("["|"<:")              { LSQUARE }
  | ("]"|":>")              { RSQUARE }
  | "="                     { EQUAL }
  | "^"                     { HAT }
  | "^^"                    { HATHAT }

  | eof { EOF }
  | '\\' rL (rL | rD)* 
    { lex_error lexbuf ("Illegal escape sequence " ^ lexeme lexbuf) }
  | _   { lex_error lexbuf ("Illegal character " ^ lexeme lexbuf) }
 
{

  let parse_with_offset f (ofs, s) =
    let lb = from_string s in
    Loc.current_offset := ofs.pos_cnum + 3;
    lb.lex_curr_p <- ofs;
    try
      f token lb
    with Parsing.Parse_error ->
      let loc = loc lb in
      Creport.raise_located loc (AnyMessage "Syntax error in annotation")

  let annot = parse_with_offset Clparser.annot

}
