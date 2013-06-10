(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
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

{

  open Lexing

  type lexeme =
    | INT     of string
    | REAL    of string
    | STRING  of string
    | IDENT   of string
    | QUOTED  of string
    | KEYWORD of string
    | END
    | EOF

  type keymap = (string,lexeme) Hashtbl.t

  let operators = [
    "(" ; ")" ; "[" ; "]" ; "{" ; "}" ;
    "+" ; "-" ; "*" ; "/" ; "%" ; "." ; "," ; ":" ; ";" ; "?" ; "!" ;
    "=>" ; "<=>" ; "->" ; "<-"; "<->" ; "<=" ; ">=" ; ">" ; "<" ; "=" ; "!=" ; "<>" ; 
    ";;"
  ]

  let keymap keywords = 
    let kmap = Hashtbl.create 31 in
    let add_key k = Hashtbl.add kmap k (KEYWORD k) in
    List.iter add_key operators ;
    List.iter add_key keywords ; 
    kmap

  let extend kmap keywords =
    let kmap = Hashtbl.copy kmap in
    let add_key k = Hashtbl.add kmap k (KEYWORD k) in
    List.iter add_key keywords ;
    kmap
    
  let word keymap m =
    try Hashtbl.find keymap m with Not_found -> IDENT m
}

let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']

rule token keymap = parse

    '\n' { new_line lexbuf ; token keymap lexbuf }
  | '.' [' ' '\t' '\r']* '\n' { Lexing.new_line lexbuf ; END }
  | ['\t' '\r' ' '] { token keymap lexbuf }
  | "(*" { comment 0 lexbuf ; token keymap lexbuf }

  | '"' (([^ '"' '\n']*) as s)'"' { STRING s }

  | "0x"? digit+ 
      { INT (Lexing.lexeme lexbuf) }

  | digit+ ('.' digit+)? (['e' 'E'] ['+' '-']? digit+)? 
      { REAL(Lexing.lexeme lexbuf) }
      
  | letter (letter | digit)* '\''*
      { word keymap (Lexing.lexeme lexbuf) }

  | ['\'' '#'] letter (letter|digit)* { QUOTED(Lexing.lexeme lexbuf) }

  | [ '(' ')' '[' ']' '+' '-' '*' '/' '.' ',' ':' ';' '?' '!' '%' '{' '}' ]
  | "=>" | "<=>" | "->" | "<->" | "<=" | ">=" | ">" | "<" | "=" | "!=" | "<>" | ";;"
	
	{ Hashtbl.find keymap (Lexing.lexeme lexbuf) }

  | eof { EOF }
  | _   { failwith (Printf.sprintf "Unexpected token %S" (Lexing.lexeme lexbuf)) }

and comment n = parse
  | "*)" { if n > 0 then comment (n-1) lexbuf }
  | "(*" { comment (n+1) lexbuf }
  | '\n' { Lexing.new_line lexbuf ; comment n lexbuf }
  | eof  { failwith "unexpected end-of-line inside comments" }
  | _    { comment n lexbuf }

{

  include Input.Make
    (struct
       type token = lexeme
       type langage = keymap
       let eof = EOF
       let create = token
     end)

  let pp_lexeme fmt = function
    | KEYWORD k -> Format.fprintf fmt "keyword %S" k
    | IDENT a   -> Format.fprintf fmt "ident %S" a
    | INT a     -> Format.fprintf fmt "int %S" a
    | REAL a    -> Format.fprintf fmt "real %S" a
    | QUOTED a  -> Format.fprintf fmt "quoted %S" a
    | STRING a  -> Format.fprintf fmt "string %S" a
    | END       -> Format.fprintf fmt "end of sentence"
    | EOF       -> Format.fprintf fmt "eof"

  let skip_pos input =
    let p = position input in
    skip input ; p
    
  let skip_ident input =
    match token input with
      | IDENT x -> skip input ; position input , x
      | a -> error input "Missing identifier (%a)" pp_lexeme a

  let skip_key input k =
    match token input with
      | KEYWORD k0 when k0 = k -> skip input
      | a -> error input "Missing '%s' (%a)" k pp_lexeme a

  let is_key input k =
    match token input with
      | KEYWORD k0 when k0 = k -> skip input ; true
      | _ -> false
	  
  let parse_list ~left ~sep ~right pp input =
    let rec collect xs =
      let x = pp input in
      if is_key input sep then collect (x::xs) else
	if is_key input right then List.rev (x::xs) else
	  error input "Missing ',' or ')'"
    in
    if is_key input left then
      if is_key input right then Some []
      else Some(collect [])
    else None

  let parse_option ~key pp input =
    if is_key input key then Some (pp input) else None

}
