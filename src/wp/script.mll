(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

  type token =
    | Id of string
    | Key of string
    | Proof of string
    | Word
    | Eof

  let fill buffer lexbuf =
    Buffer.add_string buffer (Lexing.lexeme lexbuf)

  open Lexing

  let newline lexbuf =
    lexbuf.lex_curr_p <-
      { lexbuf.lex_curr_p with pos_lnum = succ lexbuf.lex_curr_p.pos_lnum }

}

let space = [' ' '\t' '\r']

rule token = parse
    space+ { token lexbuf }
  | '\n' { newline lexbuf ; token lexbuf }
  | "Proof." space* '\n'
      {
        newline lexbuf ;
        let buffer = Buffer.create 512 in
        proof buffer 0 lexbuf ;
        Proof (Buffer.contents buffer)
      }
  | "Proof." space*
      {
        let buffer = Buffer.create 512 in
        proof buffer 0 lexbuf ;
        Proof (Buffer.contents buffer)
      }
  | [ 'a'-'z' 'A'-'Z' '0'-'9' '_' '-' '*' ]+
      {
        Id (Lexing.lexeme lexbuf)
      }
  | [ '.' ':' ',' ';' ] { Key(Lexing.lexeme lexbuf) }
  | "(*" { comment 0 lexbuf }
  | eof { Eof }
  | _ { Word }

and comment n = parse
    "*)" { if n > 0 then comment (pred n) lexbuf else token lexbuf }
  | "(*" { comment (succ n) lexbuf }
  | eof { failwith "Non-terminated comment" }
  | '\n' { newline lexbuf ; comment n lexbuf }
  | _ { comment n lexbuf }

and proof buffer n = parse
    ( "Qed." | "Save." )
      {
        if n > 0 then proof buffer (pred n) lexbuf
      }
  | "(*@" { skip 0 lexbuf ; proof buffer n lexbuf }
  | "(*"  { fill buffer lexbuf ; proof buffer (succ n) lexbuf }
  | "*)"  { fill buffer lexbuf ;
            if n>0 then proof buffer (pred n) lexbuf
            else failwith "Non-terminated comment (inside proof)" }
  | eof  { failwith "Non-terminated proof" }
  | '\n' { fill buffer lexbuf ; newline lexbuf ; proof buffer n lexbuf }
  | _    { fill buffer lexbuf ; proof buffer n lexbuf }

and skip n = parse
  | "(*" { skip (succ n) lexbuf }
  | "*)" { if n>0 then skip (pred n) lexbuf }
  | eof  { () }
  | "\n" { newline lexbuf ; skip n lexbuf }
  | _ { skip n lexbuf }

{

  let filter key =
    let lexbuf = Lexing.from_string key in
    match token lexbuf with
      | Id a -> Some a
      | _ -> None

  type input = {
    src : string ;
    inc : in_channel ;
    lexbuf : Lexing.lexbuf ;
    mutable token : token ;
    mutable tik : int ;
  }

  let open_file f =
    let inc = open_in f in
    let lex = Lexing.from_channel inc in
    let tok = token lex in
    { src=f ; tik=0 ; inc=inc ; lexbuf=lex ; token=tok }

  let pp_token lexbuf fmt = function
    | Id x -> Format.fprintf fmt "ident '%s'" x
    | Key k -> Format.fprintf fmt "'%s'" k
    | Proof _ -> Format.fprintf fmt "Proof...Qed"
    | Eof -> Format.fprintf fmt "end-of-file"
    | Word -> Format.fprintf fmt "start of '%s'" (Lexing.lexeme lexbuf)

  let skip input =
    if input.token <> Eof then
      ( input.tik <- 0 ; input.token <- token input.lexbuf )
  let token input =
    input.tik <- succ input.tik ;
    if input.tik > 1000 then failwith "Blocked" ;
    input.token
  let close input = close_in input.inc
  let error input text =
    let buffer = Buffer.create 80 in
    let fmt = Format.formatter_of_buffer buffer in
    let line = (Lexing.lexeme_start_p input.lexbuf).Lexing.pos_lnum in
    Format.fprintf fmt "%s:%d: " input.src line ;
    Format.kfprintf
      (fun fmt ->
         Format.fprintf fmt "(at %a)" (pp_token input.lexbuf) input.token ;
         Format.pp_print_flush fmt () ;
         failwith (Buffer.contents buffer)
      ) fmt text

  let key input k =
    match input.token with
      | (Key a) | (Id a) when a=k -> skip input ; true
      | _ -> false

  let eat input k =
    if not (key input k) then error input "Missing '%s'" k

  let ident input =
    match input.token with
      | Id a -> skip input ; a
      | _ -> error input "Missing identifier"

  let rec idents input =
    match input.token with
      | Id a ->
          skip input ;
          if key input "," then a :: idents input else [a]
      | Word ->
          skip input ;
          if key input "," then idents input else []
      | _ -> []

}
