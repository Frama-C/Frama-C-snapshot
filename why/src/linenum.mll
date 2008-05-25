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

(*i $Id: linenum.mll,v 1.7 2008/02/05 12:10:49 marche Exp $ i*)

(* code from Ocaml sources *)

{

  open Lexing

  let bol = ref 0 (* beginning of line, in chars *)
  let line = ref 1 (* current line number *)
  let file = ref ""

}

rule one_line = parse
  | '#' [' ' '\t']* (['0'-'9']+ as l) [' ' '\t']*
    ("\"" ([^ '\n' '\r' '"' (* '"' *) ]* as f) "\"")?
    [^ '\n' '\r']* ('\n' | '\r' | "\r\n")
      { line := int_of_string l;
	begin match f with Some f -> file := f | None -> () end;
	bol := lexeme_start lexbuf;
	lexeme_end lexbuf }
  | [^ '\n' '\r']*
    ('\n' | '\r' | "\r\n")
      { incr line;
        bol := lexeme_start lexbuf;
        lexeme_end lexbuf }
  | [^ '\n' '\r'] * eof
      { incr line;
        bol := lexeme_start lexbuf;
        raise End_of_file }

{

  let from_char f c =
    let cin = open_in_bin f in
    let lb = from_channel cin in
    file := f;
    line := 1;
    bol := 0;
    begin try while one_line lb <= c do () done with End_of_file -> () end;
    close_in cin;
    (!file, !line - 1, c - !bol)

}
