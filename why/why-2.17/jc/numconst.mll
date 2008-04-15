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

(*i $Id: numconst.mll,v 1.7 2008/11/05 14:03:16 filliatr Exp $ i*)

(* evaluation of integer literals *)

{

  open Lexing
  open Num

  let zero = num_of_int 0
  let eight = num_of_int 8
  let ten = num_of_int 10
  let sixteen = num_of_int 16

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

  let val_char c = num_of_int (val_char c)

(*
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
*)

}

let rD =	['0'-'9']
let rL = ['a'-'z' 'A'-'Z' '_']
let rH = ['a'-'f' 'A'-'F' '0'-'9']
(*
let rE = ['E''e']['+''-']? rD+
let rFS	= ('f'|'F'|'l'|'L')
*)
let rIS = ('u'|'U'|'l'|'L')

(*
  | '0'['x''X'] rH+ rIS?    { CONSTANT (IntConstant (lexeme lexbuf)) }
  | '0' rD+ rIS?            { CONSTANT (IntConstant (lexeme lexbuf)) }
  | rD+ rIS?                { CONSTANT (IntConstant (lexeme lexbuf)) }
  | 'L'? "'" [^ '\n' '\'']+ "'"     { CONSTANT (IntConstant (lexeme lexbuf)) }
*)

rule eval_int = parse

  | '0'['x''X']  { eval_hexa zero lexbuf }
  | "'" { eval_char lexbuf }
  | '0' { eval_octa zero lexbuf}
  | ['1'-'9'] as d { eval_deci (val_char d) lexbuf}
  | "-" { minus_num(eval_int lexbuf) }
  | 'L' { invalid_arg "extended characters not yet implemented" } 
  | eof { invalid_arg "empty literal" }
  | _   { invalid_arg ("Illegal character " ^ lexeme lexbuf) }

and eval_hexa accu = parse
  | rH as d { (* if accu >= 0x10000000L then raise Constant_too_large; *)
	  let accu = add_num (mult_num sixteen accu) (val_char d) in 
	  eval_hexa accu lexbuf }
  | eof { (* check_bounds loc true *) accu (*""*) }
  | rIS+ as _s { (*check_bounds loc true *) accu (* s *) }
  | _ { invalid_arg ("digit '" ^ (lexeme lexbuf) ^ 
		       "' in hexadecimal constant") }

and eval_deci accu = parse
  | rD as c 
      { (* if accu >= 0x10000000L then raise Constant_too_large; *)
	let accu = add_num (mult_num ten accu) (val_char c) in 
	eval_deci accu lexbuf }
  | eof 
      { (* check_bounds loc false *) accu (* "" *) }
  | rIS+ as _s
      { (* check_bounds loc false *) accu (* s *) }
  | _ 
      { invalid_arg ("digit '" ^ (lexeme lexbuf) ^ 
			"' in decimal constant") }

and eval_octa accu = parse
  | ['0'-'7'] as d
      { (* if accu >= 0x10000000L then raise Constant_too_large; *)
	let accu = add_num (mult_num eight accu) (val_char d) in 
	eval_octa accu lexbuf }
  | eof { (* check_bounds loc false *) accu (* "" *) }
  | rIS+ as _s { (* check_bounds loc false *) accu (* s *) }
  | _ { invalid_arg ("digit '" ^ (lexeme lexbuf) ^ 
			"' in octal constant") }

and eval_char = parse
  | "\\n'" { num_of_int (Char.code '\n') }
  | "\\t'" { num_of_int (Char.code '\t') }
  | "\\v'" { num_of_int 11 }
  | "\\a'" { num_of_int 7 }
  | "\\b'" { num_of_int (Char.code '\b') }
  | "\\r'" { num_of_int (Char.code '\r') }
  | "\\f'" { num_of_int 12 }
  | "\\\\'" { num_of_int (Char.code '\\') }
  | "\\?'" { num_of_int (Char.code '?') }
  | "\\''" { num_of_int (Char.code '\'') }
  | "\\\"'" { num_of_int (Char.code '"') }
  | "\\" (['0'-'7'] ['0'-'7']? ['0'-'7']? as s) "'" 
      { num_of_int (int_of_string ("0o" ^ s)) }
  | "\\" (['0'-'9'] ['0'-'9']? ['0'-'9']? as s) "'" 
      { invalid_arg ("digits '" ^ s ^ "' in octal constant") }
  | "\\x" (rH rH? as s) "'" { num_of_int (int_of_string ("0x" ^ s)) }
  | "\\u" (rH rH rH rH as s) "'" { num_of_int (int_of_string ("0x" ^ s)) }
  | (_ as c) "'" { num_of_int (Char.code c) }
  | [^'\'']* as s "'"  
      { invalid_arg ("cannot evaluate char constant '" ^ s ^"'") }

{

  let integer s =
(*
    try
*)
      let lb = Lexing.from_string s in
      eval_int lb
(*
    with
      | Constant_too_large -> error loc "constant too large"
      | Invalid msg -> error loc "invalid constant: %s" msg
*)

    
}
