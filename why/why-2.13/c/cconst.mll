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

(*i $Id: cconst.mll,v 1.13 2008/02/05 12:10:47 marche Exp $ i*)

(* evaluation of integer literals *)

{

  open Lexing
  open Int64
  open Creport

  module IntMap = Map.Make(struct type t = int64 let compare = compare end)

  exception Constant_too_large

  exception Invalid of string

  let val_char = function
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | 'a' | 'A' -> 10
    | 'b' | 'B' -> 11
    | 'c' | 'C' -> 12
    | 'd' | 'D' -> 13
    | 'e' | 'E' -> 14
    | 'f' | 'F' -> 15
    | _ -> assert false

  let val_char c = Int64.of_int (val_char c)

  let check_bounds loc hexa accu suffix =
    match String.lowercase suffix with
      | "" ->
	  if accu > 0x7FFFFFFFL then 
	     if hexa then warning loc "Constant too large for a int"
                     else raise Constant_too_large
          else
	    if accu > 0x7FFFL then 
	      warning loc
		"this constant overflows if sizeof(int)<=16";
	  accu 
      | "u" ->
	  if accu > 0xFFFFFFFFL then raise Constant_too_large;
	  if accu > 0xFFFFL then 
	    warning loc
	      "this constant overflows if sizeof(int)<=16";
	  accu 
      | "l" ->
	  if accu > 0x7FFFFFFFL then raise Constant_too_large else accu 
      | "ul" | "lu" ->
	  if accu > 0xFFFFFFFFL then raise Constant_too_large else accu 
      | "ll" ->
	  if accu > 0x7FFFFFFFFFFFFFFFL then raise Constant_too_large else accu
      | "ull" | "llu" -> accu 
      | _ ->
	  raise (Invalid ("suffix '" ^ suffix ^ "' on integer constant")) 

}

let rD =	['0'-'9']
let rL = ['a'-'z' 'A'-'Z' '_']
let rH = ['a'-'f' 'A'-'F' '0'-'9']
let rE = ['E''e']['+''-']? rD+
let rFS	= ('f'|'F'|'l'|'L')
let rIS = ('u'|'U'|'l'|'L')

(*
  | '0'['x''X'] rH+ rIS?    { CONSTANT (IntConstant (lexeme lexbuf)) }
  | '0' rD+ rIS?            { CONSTANT (IntConstant (lexeme lexbuf)) }
  | rD+ rIS?                { CONSTANT (IntConstant (lexeme lexbuf)) }
  | 'L'? "'" [^ '\n' '\'']+ "'"     { CONSTANT (IntConstant (lexeme lexbuf)) }
*)

rule eval_int loc = parse

  | '0'['x''X']  { eval_hexa loc Int64.zero lexbuf }
  | "'" { eval_char lexbuf }
  | '0' { eval_octa loc Int64.zero lexbuf}
  | ['1'-'9'] as d { eval_deci loc (val_char d) lexbuf}
  | 'L' { unsupported loc "extended character" } 
  | eof { raise (Invalid "empty literal") }
  | _   { raise (Invalid ("Illegal character " ^ lexeme lexbuf)) }

and eval_hexa loc accu = parse
  | rH  { if accu >= 0x10000000L then raise Constant_too_large;
	  let accu = add (mul 16L accu) (val_char (lexeme_char lexbuf 0)) in 
	  eval_hexa loc accu lexbuf }
  | eof { check_bounds loc true accu "" }
  | rIS+ { check_bounds loc true accu (lexeme lexbuf) }
  | _ { raise (Invalid ("digit '" ^ (lexeme lexbuf) ^ 
			"' in hexadecimal constant")) }

and eval_deci loc accu = parse
  | rD as c 
      { if accu >= 0x10000000L then raise Constant_too_large;
	let accu = add (mul 10L accu) (val_char c) in 
	eval_deci loc accu lexbuf }
  | eof 
      { check_bounds loc false accu "" }
  | rIS+ 
      { check_bounds loc false accu (lexeme lexbuf) }
  | _ 
      { raise (Invalid ("digit '" ^ (lexeme lexbuf) ^ 
			"' in decimal constant")) }

and eval_octa loc accu = parse
  | ['0'-'7']  { if accu >= 0x10000000L
	  then raise Constant_too_large
	  else
	    let accu = add (mul 8L accu) (val_char (lexeme_char lexbuf 0)) in 
	    eval_octa loc  accu lexbuf }
  | eof { check_bounds loc false accu "" }
  | rIS+ { check_bounds loc false accu (lexeme lexbuf) }
  | _ { raise (Invalid ("digit '" ^ (lexeme lexbuf) ^ 
			"' in octal constant")) }

and eval_char = parse
  | "\\n'" { Int64.of_int (Char.code '\n') }
  | "\\t'" { Int64.of_int (Char.code '\t') }
  | "\\v'" { Int64.of_int 11 }
  | "\\a'" { Int64.of_int 7 }
  | "\\b'" { Int64.of_int (Char.code '\b') }
  | "\\r'" { Int64.of_int (Char.code '\r') }
  | "\\f'" { Int64.of_int 12 }
  | "\\\\'" { Int64.of_int (Char.code '\\') }
  | "\\?'" { Int64.of_int (Char.code '?') }
  | "\\''" { Int64.of_int (Char.code '\'') }
  | "\\\"'" { Int64.of_int (Char.code '"') }
  | "\\" (['0'-'7'] ['0'-'7']? ['0'-'7']? as s) "'" 
      { Int64.of_int (int_of_string ("0o" ^ s)) }
  | "\\" (['0'-'9'] ['0'-'9']? ['0'-'9']? as s) "'" 
      { raise (Invalid ("digits '" ^ s ^ "' in octal constant")) }
  | "\\x" (rH rH? as s) "'" { Int64.of_int (int_of_string ("0x" ^ s)) }
  | (_ as c) "'" { Int64.of_int (Char.code c) }


{

  let int loc s =
    try
      let lb = Lexing.from_string s in
      eval_int loc lb
    with
      | Constant_too_large -> error loc "constant too large"
      | Invalid msg -> error loc "invalid constant: %s" msg

}
