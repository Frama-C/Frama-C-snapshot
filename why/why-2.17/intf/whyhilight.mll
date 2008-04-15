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

{
  open Lexing
  open Colors

  exception Eof
  
  let insert_text (tbuf:GText.buffer) ty s = 
    let it = tbuf#end_iter in
    let (fc, bc) = get_color ty in
    let new_tag = tbuf#create_tag [`BACKGROUND bc; `FOREGROUND fc] in
    tbuf#insert ~tags:[new_tag] ~iter:it s 

  let insert_string (tbuf:GText.buffer) s =
    let it = tbuf#end_iter in
    tbuf#insert ~iter:it s 

  let buffer = Buffer.create 1024
}

let keyw = "goal" | "external" | "parameter" | "logic" | "predicate" | "axiom" |
    "let" | "in" | "begin" | "end" | "if" | "then" | "else" | "of" |
    "ref" | "array" | "while" | "do" | "done" | "assert" | "label" | 
    "fun" | "rec" | "forall" | "and" | "->" | "type" | "exception"
let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (alpha | '_' | digit)+

rule scan tbuf = parse
  | "(*"  { let t = Buffer.contents buffer in
	    insert_string tbuf t; 
	    Buffer.clear buffer;
	    insert_text tbuf "comment" "(*";
	    comment tbuf lexbuf; 
	    scan tbuf lexbuf }
  | "{"   { let t = Buffer.contents buffer in
	    insert_string tbuf t; 
	    Buffer.clear buffer;
	    insert_text tbuf "predicate" "{";
	    annotation tbuf lexbuf; 
	    scan tbuf lexbuf }
  | keyw  { insert_text tbuf "keyword" (lexeme lexbuf);
	    scan tbuf lexbuf }
  | eof   { raise Eof }
  | ident { insert_string tbuf (lexeme lexbuf); 
	    scan tbuf lexbuf }
  | _     { insert_string tbuf (lexeme lexbuf); 
	    scan tbuf lexbuf }

and comment tbuf = parse
  | "(*" { insert_text tbuf "comment" "(*"; 
	   comment tbuf lexbuf; 
	   comment tbuf lexbuf }
  | "*)" { insert_text tbuf "comment" "*)" }
  | eof  { () }
  | [^ '*']*
  | _    { insert_text tbuf "comment" (lexeme lexbuf); 
	   comment tbuf lexbuf }

and annotation tbuf = parse
  | "}"  { insert_text tbuf "predicate" "}" }
  | eof  { () }
  | _    { insert_text tbuf "predicate" (lexeme lexbuf); 
	   annotation tbuf lexbuf }
