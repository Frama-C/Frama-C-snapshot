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

  open Printf
  open Lexing 

  let debug = ref true
  let callback = ref (fun f -> assert false : string -> unit)

  (* we put everything not a goal into [buf] *)
  let buf = Buffer.create 8192

  let outc = ref stdout
  let file = ref ""
  let level = ref 0 

  let print s = 
    output_string !outc s

  let end_file file =
    close_out !outc;
    !callback file
    (*if not !debug then Sys.remove file*)

}

let space = [' ' '\t' '\n' '\r']
let ident = ['a'-'z' 'A'-'Z' '0'-'9']+

rule split = parse
  | "(" space* "(theory" space* "(extends)"
      { 
	Buffer.add_string buf (lexeme lexbuf) ;
	split lexbuf 
      }
  | ";;" space*"Why axiom"  
      {
	Buffer.add_string buf ";;"  ;
	splitAxiom lexbuf;
	split lexbuf
      }

  | "))" space* ")" space*";;" space*  "END THEORY" 
      { 
	Buffer.add_string buf (lexeme lexbuf);
	split lexbuf 
      }
  | ";;" space* "Why obligation" 
      {
	file := Filename.temp_file "rv" ".rv"; 
	level := 0 ;
	outc := open_out !file;
	print (Buffer.contents buf);
	print (lexeme lexbuf); 
	query lexbuf;
	end_file !file;
	split lexbuf
      }
  | eof {()}
  |_ 
      {
	Buffer.add_string buf (lexeme lexbuf) ;
       split lexbuf}
      

and splitAxiom = parse
  | "(" { Buffer.add_char buf '('; 
	  level := !level + 1 ; 
	  splitAxiom lexbuf;
	}
  | ")" { Buffer.add_char buf ')'; 
	  level := !level - 1 ;
	  if !level <> 0 then splitAxiom lexbuf 
	}
  | _   { Buffer.add_string buf (lexeme lexbuf); 
	  splitAxiom lexbuf }

and query = parse
  | "(" { print "(";
	  level := !level + 1 ; 
	  query lexbuf;}
  | ")" { print ")" ;
	  level := !level - 1 ;
	  if !level <> 0 then query lexbuf 
	}
  | _   { print (lexeme lexbuf); query lexbuf }

{

  let iter cb f =
    callback := cb;
    Buffer.reset buf;
    let c = open_in f in
    let lb = from_channel c in
    split lb;
    close_in c

}

