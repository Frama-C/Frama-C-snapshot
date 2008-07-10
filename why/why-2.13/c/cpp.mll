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

(*i $Id: cpp.mll,v 1.20 2008/05/25 11:53:34 stoulsn Exp $ i*)

(* C-preprocessor for Caduceus *)

{

  open Coptions
  open Printf
  open Lexing 

  let channel_out = ref stdout
  let print s = output_string !channel_out s

  let start_of_comment_string = "start_of_comment_string_multiple_lines"
  let start_of_comment_string_one_line = "start_of_comment_string_one_line"
  let end_of_comment_string = "end_of_comment_string"

}

let start_of_comment_string = "start_of_comment_string_multiple_lines"
let start_of_comment_string_one_line = "start_of_comment_string_one_line"
let end_of_comment_string = "end_of_comment_string"

rule before = parse
  | "/*@" { print start_of_comment_string; before lexbuf }
(*  | "*/" { print end_of_comment_string; before lexbuf } *)
  | "//@" { print start_of_comment_string_one_line; before lexbuf }
  | "# 1 \"<" [^'\n''>']* ">\"\n#define" [^'\n']* '\n' { before lexbuf } 
  | "#define __" [^'\n']* '\n' { before lexbuf } 

  | _    { print (lexeme lexbuf); before lexbuf }
  | eof  { () }

and after = parse
  | start_of_comment_string_one_line { print "//@"; after lexbuf }
  | start_of_comment_string { print "/*@"; after lexbuf }
  | end_of_comment_string   { print "*/"; after lexbuf }
  | _    { print (lexeme lexbuf); after lexbuf }
  | eof  { () }

{

  let rec local_temp_file basename suffix =
    let i = Random.int 536870912 in
    let f = basename ^ string_of_int i ^ suffix in
    if Sys.file_exists f then local_temp_file basename suffix else f

  (* [translate_using filter f] translates file [f] using lexer rule [filter]
     and returns the translated file *)
  let translate_using filter f =
    let cin = open_in f in
    let lb = from_channel cin in
    let pf = local_temp_file (Filename.basename f) ".c" in
    let cout = open_out pf in
    fprintf cout "# 1 \"%s\"\n" f;
    channel_out := cout;
    filter lb;
    close_in cin;
    close_out cout;
    pf

  let before_cpp = translate_using before
  let after_cpp = translate_using after

  (* [external_cpp f] runs an external C preprocessor on file [f];
     returns the preprocessed file. *)
  let external_cpp f = 
    let ppf = local_temp_file (Filename.basename f) ".i" in
    let cmd = sprintf "%s %s > %s" cpp_command f ppf in
    ignore (Sys.command cmd);
    ppf

  (* [first_cpp f] runs an external C preprocessor on file [f] and preserves the definitions in trace;
     returns the preprocessed file. *)
  let first_cpp f = 
    let ppf = local_temp_file (Filename.basename f) ".c" in
    ignore (Sys.command (sprintf "%s -dD %s > %s" cpp_command f ppf));
 (*   ignore (Sys.command (sprintf "LANG=en ; %s -dD %s > %s" cpp_command f ppf)); *)
    ppf
 




  (* [translate f] preprocesses file [f]; returns the preprocessed file and a 
     finalizer to be called when the preprocessed file has been used. *)
  let translate f =
    if with_cpp then begin
      let inlinedf = first_cpp f in
      let pf = before_cpp inlinedf in
      let ppf = external_cpp pf in
      Sys.remove inlinedf; 
      Sys.remove pf; 
      let pppf = after_cpp ppf in
      Sys.remove ppf; 
      (* if not debug then *) at_exit (fun () -> Sys.remove pppf);
      pppf
    end else begin
      f
    end

  let dump f =
    let cin = open_in f in
    try
      while true do printf "%s\n" (input_line cin) done
    with End_of_file -> 
      close_in cin

  let cpp f =
    let pf = translate f in
    if cpp_dump then begin dump pf; exit 0 end;
    pf

}

