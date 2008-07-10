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

(*i $Id: ergo_split.mll,v 1.4 2008/02/05 12:10:50 marche Exp $ i*)

{

  open Printf
  open Lexing

  let debug = ref false
  let callback = ref (fun f -> assert false)

  (* we put everything not a goal into [buf] *)
  let buf = Buffer.create 8192

  let outc = ref stdout
  let print s = output_string !outc s

  let start_file () =
    let f = Filename.temp_file "ergo" ".why" in
    outc := open_out f;
    print (Buffer.contents buf);
    f

  let end_file file =
    close_out !outc;
    !callback file;
    if not !debug then Sys.remove file

}

let letters = ['a'-'z']+
rule split = parse
  | "goal" 
      { let file = start_file () in print "goal"; goal file lexbuf }
  | (letters | _) as s
      { Buffer.add_string buf s; split lexbuf }
  | eof 
      { () }

(* copy the query up to the semi-colon *)
and goal file = parse
  | "goal" 
      { end_file file; let file = start_file () in 
	print "goal "; goal file lexbuf }
  | ("\"" ([^ '\"'] | "\\" _)* "\"") as s
      { print s; goal file lexbuf }
  | ("type" | "logic" | "predicate" | "axiom") as k
      { end_file file; Buffer.add_string buf k; split lexbuf }
  | (letters | _) as s
      { print s; goal file lexbuf }
  | eof { end_file file }

{

  let iter cb f =
    callback := cb;
    Buffer.reset buf;
    let c = open_in f in
    let lb = from_channel c in
    split lb;
    close_in c

}

