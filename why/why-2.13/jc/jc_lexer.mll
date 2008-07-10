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

(*i $Id: jc_lexer.mll,v 1.60 2008/07/03 09:14:35 marche Exp $ i*)

{
  open Jc_ast
  open Jc_parser
  open Lexing

  type location = Lexing.position * Lexing.position

  let loc lexbuf : location = (lexeme_start_p lexbuf, lexeme_end_p lexbuf)

  exception Lexical_error of location * string
  exception Syntax_error of location

  let lex_error lexbuf s =
    raise (Lexical_error(loc lexbuf,s))

  let buf = Buffer.create 1024

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  (* Update the current location with file name and line number. *)

  let update_loc lexbuf file line absolute chars =
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
      | None -> pos.pos_fname
      | Some s -> s
    in
    lexbuf.lex_curr_p <- 
      { pos with
	  pos_fname = new_file;
	  pos_lnum = if absolute then line else pos.pos_lnum + line;
	  pos_bol = pos.pos_cnum - chars;
      }


  exception Dotdot of string

  let pragma lexbuf id v =
    match id with
      | "InvariantPolicy" ->
	  begin
	    Jc_options.inv_sem :=
	      match v with	  
		| "None" -> Jc_env.InvNone
		| "Arguments" -> Jc_env.InvArguments
		| "Ownership" -> Jc_env.InvOwnership
		| _ -> lex_error lexbuf ("unknown invariant policy " ^ v)
	  end  
      | "SeparationPolicy" ->
	  begin
	    Jc_options.separation_sem :=
	      match v with
		| "None" -> Jc_env.SepNone
		| "Regions" -> Jc_env.SepRegions
		| _ -> lex_error lexbuf ("unknown separation policy " ^ v)
	  end  
      | "AnnotationPolicy" ->
	  begin
	    Jc_options.annotation_sem :=
	      match v with
		| "None" -> Jc_env.AnnotNone
		| "Invariants" -> Jc_env.AnnotInvariants
		| "WeakPre" -> Jc_env.AnnotWeakPre
		| "StrongPre" -> Jc_env.AnnotStrongPre
		| _ -> lex_error lexbuf ("unknown annotation policy " ^ v)
	  end  
      | "AbstractDomain" ->
	  begin
	    Jc_options.ai_domain :=
	      match v with
		| "None" -> Jc_env.AbsNone 
		| "Box" -> Jc_env.AbsBox 
		| "Oct" -> Jc_env.AbsOct 
		| "Pol" -> Jc_env.AbsPol 
		| _ -> lex_error lexbuf ("unknown abstract domain " ^ v)
	  end  
      | "IntModel" ->
	  begin
	    Jc_options.int_model :=
	      match v with
		| "bounded" -> Jc_env.IMbounded
		| "modulo" -> Jc_env.IMmodulo 
		| _ -> lex_error lexbuf ("unknown int model " ^ v)
	  end  
      | _ -> lex_error lexbuf ("unknown pragma " ^ id)
}

let space = [' ' '\t' '\012' '\r']
let backslash_escapes =
  ['\\' '"' '\'' 'n' 't' 'b' 'r' 'f' ]
let rD = ['0'-'9']
let rL = ['a'-'z' 'A'-'Z' '_']
let rH = ['a'-'f' 'A'-'F' '0'-'9']
let rE = ['E''e']['+''-']? rD+
let rFS	= ('f'|'F'|'l'|'L')
let rIS = ('u'|'U'|'l'|'L')*

rule token = parse
  | [' ' '\t' '\012' '\r']+ { token lexbuf }
  | '\n'                    { newline lexbuf; token lexbuf }
  | "/*@" | "//@"           { lex_error lexbuf "annotations should not be in @ comments" }
  | "/*"                    { comment lexbuf; token lexbuf }
  | "//" [^ '\n']* '\n'     { newline lexbuf; token lexbuf }
  | "and"                   { AND }
  | "as"                    { AS }
  | "assert"                { ASSERT }
  | "assigns"               { ASSIGNS }
  | "assumes"               { ASSUMES }
  | "axiom"                 { AXIOM }
  | "behavior"              { BEHAVIOR }
  | "boolean"               { BOOLEAN }
  | "break"                 { BREAK }
  | "case"                  { CASE }
  | "default"               { DEFAULT }
  | "catch"                 { CATCH }
  | "continue"              { CONTINUE }
  | "do"                    { DO }
(*
  | "double"                { DOUBLE }
*)
  | "else"                  { ELSE }
  | "ensures"               { ENSURES }
  | "end"                   { END }
(*
  | "enum"                  { ENUM }
*)
  | "exception"             { EXCEPTION }
  | "false"                 { CONSTANT (JCCboolean false) }
  | "finally"               { FINALLY }
(*
  | "float"                 { FLOAT }
*)
  | "for"                   { FOR }
  | "free"                  { FREE }
  | "goto"                  { GOTO }
  | "if"                    { IF }
  | "in"                    { IN }
  | "integer"               { INTEGER }
  | "invariant"             { INVARIANT }
  | "lemma"                 { LEMMA }
  | "let"                   { LET }
  | "logic"                 { LOGIC }
  | "match"                 { MATCH }
  | "new"                   { NEW }
  | "null"                  { NULL }
  | "of"                    { OF }
  | "pack"                  { PACK }
  | "reads"                 { READS }
  | "real"                  { REAL}
  | "rep"                   { REP }
  | "requires"              { REQUIRES }
  | "return"                { RETURN }
  | "switch"                { SWITCH }
  | "tag"                   { TAG }
  | "then"                  { THEN }
  | "throw"                 { THROW }
  | "throws"                { THROWS }
  | "true"                  { CONSTANT (JCCboolean true) }
  | "try"                   { TRY }
  | "type"                  { TYPE }
  | "unit"                  { UNIT }
  | "unpack"                { UNPACK }
  | "variant"               { VARIANT }
  | "var"                   { VAR }
  | "while"                 { WHILE }
  | "with"                  { WITH }
  | "\\at"                  { BSAT }
  | "\\bottom"              { BSBOTTOM }
  | "\\forall"              { BSFORALL }
  | "\\exists"              { BSEXISTS }
  | "\\mutable"             { BSMUTABLE }
  | "\\nothing"             { BSNOTHING }
  | "\\offset_max"          { BSOFFSET_MAX }
  | "\\offset_min"          { BSOFFSET_MIN }
  | "\\old"                 { BSOLD }
  | "\\result"              { BSRESULT }
  | "\\typeeq"              { BSTYPEEQ }
  | "\\typeof"              { BSTYPEOF }
  | "\\" rL*                { lex_error lexbuf ("Illegal escape sequence " ^ lexeme lexbuf) }
(*
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
        ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
        [^ '\010' '\013'] * '\n'
      { update_loc lexbuf name (int_of_string num) true 0;
        token lexbuf }
  | '#' [^'\n']* '\n'       { newline lexbuf; token lexbuf }
*)
  | '#' space* ((rL | rD)+ as id) space* "=" 
        space* ((rL | rD)+ as v) space* '\n'
      { pragma lexbuf id v; newline lexbuf; token lexbuf } 
  | rL (rL | rD)*           { match lexeme lexbuf with
				| "_" -> UNDERSCORE
				| s -> IDENTIFIER s }

  | '0'['x''X'] rH+ rIS?    { CONSTANT (JCCinteger (lexeme lexbuf)) }
  | '0' rD+ rIS?            { CONSTANT (JCCinteger (lexeme lexbuf)) }
  | rD+ rIS?                { CONSTANT (JCCinteger (lexeme lexbuf)) }
  | 'L'? "'" [^ '\n' '\'']+ "'"     { CONSTANT (JCCinteger (lexeme lexbuf)) }

  | rD+ rE rFS?             { CONSTANT (JCCreal (lexeme lexbuf)) }
  | rD* "." rD+ (rE)? rFS?  { CONSTANT (JCCreal (lexeme lexbuf)) }
  | rD+ "." rD* (rE)? rFS?  { CONSTANT (JCCreal (lexeme lexbuf)) }

      (* trick to deal with intervals like 0..10 *)

  | (rD+ as n) ".."         { raise (Dotdot n) }

      (* character constants *)

  | '"'                     { Buffer.clear buf; STRING_LITERAL(string lexbuf) }

(*
  | ">>="                   { RIGHT_ASSIGN }
  | "<<="                   { LEFT_ASSIGN }
*)
  | "+="                    { PLUSEQ }
  | "-="                    { MINUSEQ }
  | "*="                    { STAREQ }
  | "/="                    { SLASHEQ }
  | "%="                    { PERCENTEQ }
  | "&="                    { AMPEQ }
  | "^="                    { CARETEQ }
  | "|="                    { BAREQ }
  | ">>>"                   { ARSHIFT }
  | ">>"                    { LRSHIFT }
  | "<<"                    { LSHIFT }
  | "++"                    { PLUSPLUS }
  | "--"                    { MINUSMINUS }
  | "&&"                    { AMPAMP }
  | "||"                    { BARBAR }
  | "==>"                   { EQEQGT }
  | "<==>"                  { LTEQEQGT }
  | "<="                    { LTEQ }
  | ">="                    { GTEQ }
  | "=="                    { EQEQ }
  | "!="                    { BANGEQ }
  | ";"                     { SEMICOLON }
  | ";;"                    { SEMISEMI }
  | "{"                     { LBRACE }
  | "}"                     { RBRACE }
  | ","                     { COMMA }
  | ":"                     { COLON }
  | "="                     { EQ }
  | "()"                    { LPARRPAR }
  | "("                     { LPAR }
  | ")"                     { RPAR }
  | "["                     { LSQUARE }
  | "]"                     { RSQUARE }
  | "."                     { DOT }
  | ".."                    { DOTDOT }
  | "..."                   { DOTDOTDOT }
  | "<:"                    { LTCOLON } 
  | ":>"                    { COLONGT } 
  | "&"                     { AMP }
  | "!"                     { EXCLAM }
  | "~"                     { TILDE }
  | "-"                     { MINUS }
  | "+"                     { PLUS }
  | "*"                     { STAR }
  | "/"                     { SLASH }
  | "%"                     { PERCENT }
  | "<"                     { LT }
  | ">"                     { GT }
  | "^"                     { HAT }
  | "|"                     { PIPE }
  | "?"                     { QUESTION }
  | "@"                     { AT }
  | "->"                    { MINUSGT }
  | eof { EOF }
  | '"' { lex_error lexbuf "unterminated string" }
  | _   { lex_error lexbuf ("illegal character " ^ lexeme lexbuf) }

and comment = parse
  | "*/" { () }
  | "/*" { comment lexbuf ; comment lexbuf }
  | eof  { lex_error lexbuf "unterminated comment" }
  | '\n' { newline lexbuf; comment lexbuf }
  | _    { comment lexbuf }

and string = parse
  | '"'
      { Buffer.contents buf }
  | '\\' backslash_escapes
      { Buffer.add_string buf (lexeme lexbuf);
	string lexbuf }
  | '\\' _ 
      { lex_error lexbuf "unknown escape sequence in string" }
  | ['\n' '\r']
      { (* no \n anymore in strings since java 1.4 *)
	lex_error lexbuf "string not terminated"; }
  | [^ '\n' '\\' '"']+ 
      { Buffer.add_string buf (lexeme lexbuf); string lexbuf }
  | eof
      { lex_error lexbuf "string not terminated" }

      
{

let dotdot_mem = ref false
 
let next_token lexbuf =
  if !dotdot_mem then
    begin
      dotdot_mem := false;
      DOTDOT
    end
  else
    begin
      try
	token lexbuf
      with
	Dotdot(n) ->
	  dotdot_mem := true;
	  CONSTANT(JCCinteger n)
    end

  let parse f c =
    let lb = from_channel c in
    lb.lex_curr_p <- { lb.lex_curr_p with pos_fname = f };
    try
      Jc_parser.file next_token lb
    with Parsing.Parse_error ->
      Jc_options.parsing_error (loc lb) ""
	
}


(*
Local Variables: 
compile-command: "make -C .. bin/jessie.byte"
End: 
*)
