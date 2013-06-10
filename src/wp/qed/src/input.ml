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

(* -------------------------------------------------------------------------- *)
(* --- Lexer Utilities                                                    --- *)
(* -------------------------------------------------------------------------- *)

open Syntax

type 'a lexer = Lexing.lexbuf -> 'a

exception SyntaxError of position * string

let merge p q =
  let r = if p.p_line <= q.p_line then p else q in
  { r with
      p_start = min p.p_start q.p_start ;
      p_stop  = max p.p_stop  q.p_stop  ;
  }

let rec merge_list f p = function
  | [] -> p
  | x::xs -> merge_list f (merge p (f x)) xs

let pp_position fmt p =
  Format.fprintf fmt "File \"%s\", line %d, characters %d-%d"
    p.p_file p.p_line (p.p_start - p.p_bol+1) (p.p_stop - p.p_bol)

let error_at position message =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       raise (SyntaxError(position,Buffer.contents buffer)))
    (Format.formatter_of_buffer buffer)
    message

let string_of_exn = function
  | Failure msg -> msg
  | e -> Printexc.to_string e

let locate position = function
  | SyntaxError _ as e -> e
  | e -> SyntaxError(position,string_of_exn e)

let nowhere = {
  p_file = "qed-prelude" ;
  p_line = 0 ;
  p_bol = 0 ;
  p_start = 0 ;
  p_stop = 0 ;
}
      
module type Lexer =
sig
  type token
  type langage
  val eof : token
  val create : langage -> token lexer
end

module type S =
sig

  type input
  type token
  type langage

  val open_file : langage -> string -> input
  val open_shell : langage -> string -> input
  val close : input -> unit

  val token    : input -> token
  val skip     : input -> unit
  val context  : input -> string -> unit
  val position : input -> position

  val error : input -> ('a,Format.formatter,unit,'b) format4 -> 'a

end

module Make(L : Lexer) =
struct

  open Lexing

  type id = position * string
  type token = L.token
  type langage = L.langage
  type input = {
    lexing : token lexer ;
    lexbuf : Lexing.lexbuf ;
    inc : in_channel option ;
    mutable token : token ;
    mutable lock : int ;
    mutable context : string ;
  }

  let init_pos lex f =
    let pos = {
      pos_fname = f ;
      pos_lnum  = 1 ;
      pos_bol   = 0 ;
      pos_cnum  = 0 ;
    } in
    ( lex.lex_start_p <- pos ; lex.lex_curr_p  <- pos )

  let first_token lexing lexbuf =
    try lexing lexbuf
    with End_of_file -> L.eof

  let open_file lang f =
    let inc = open_in f in
    let lexbuf = from_channel inc in
    let lexing = L.create lang in
    init_pos lexbuf f ;
    {
      inc = Some inc ;
      lexbuf = lexbuf ;
      lexing = lexing ;
      token = first_token lexing lexbuf ;
      context = "file "^f ; lock = 256 ;
    }

  let open_shell lang buffer =
    let lexbuf = Lexing.from_string buffer in
    let lexing = L.create lang in
    init_pos lexbuf "toplevel" ;
    {
      inc = None ;
      lexbuf = lexbuf ;
      lexing = lexing ;
      token = first_token lexing lexbuf ;
      context = "toplevel" ; lock = 256 ;
    }

  let context input s = input.context <- s

  let close input =
    input.token <- L.eof ;
    match input.inc with Some inc -> close_in inc | None -> ()

  let position input =
    let start = input.lexbuf.lex_start_p in
    let stop  = input.lexbuf.lex_curr_p in
    {
      p_file = start.pos_fname ;
      p_line = start.pos_lnum ;
      p_bol  = start.pos_bol ;
      p_start = start.pos_cnum ;
      p_stop  = stop.pos_cnum ;
    }

  let error input message = error_at (position input) message

  let skip input =
    if input.token <> L.eof then
      try
	input.token <- input.lexing input.lexbuf ;
	input.lock <- 256
      with
	| End_of_file -> input.token <- L.eof
	| e ->
	    let msg = Printf.sprintf
	      "Lexical failure (%s)"
	      (string_of_exn e) in
	    raise (locate (position input) (Failure msg))

  let token input =
    if input.lock < 0 then failwith input.context ;
    input.lock <- pred input.lock ;
    input.token

end
