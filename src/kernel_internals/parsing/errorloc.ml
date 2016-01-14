(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux          *)
(*                        énergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(* Copied and modified from [cil/src/errormsg.ml] *)

(***** Handling parsing errors ********)
type parseinfo = {
  lexbuf : Lexing.lexbuf;
  inchan : in_channel;
  mutable current_working_directory : string option;
}

let dummyinfo = {
  lexbuf    = Lexing.from_string "";
  inchan    = stdin;
  current_working_directory = None;
}

let current = ref dummyinfo

let startParsing fname =
  (* We only support one open file at a time *)
  if !current != dummyinfo then begin
    Kernel.fatal
      "[Errorloc.startParsing] supports only one open file: \
You want to open %S and %S is still open"
      fname (Lexing.lexeme_start_p !current.lexbuf).Lexing.pos_fname
  end;
  let inchan =
    try open_in_bin fname
    with Sys_error s ->
      Kernel.abort "Cannot find input file %S: %s" fname s
  in
  let lexbuf = Lexing.from_channel inchan in
  let filename = Filepath.normalize fname in
  let i = { lexbuf; inchan; current_working_directory = None } in
  (* Initialize lexer buffer. *)
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = filename;
      Lexing.pos_lnum  = 1;
      Lexing.pos_bol   = 0;
      Lexing.pos_cnum  = 0
    };
  current := i;
  lexbuf

let finishParsing () =
  let i = !current in
  assert (i != dummyinfo);
  close_in i.inchan;
  current := dummyinfo


(* Call this function to announce a new line *)
let newline () =
  Lexing.new_line !current.lexbuf

let setCurrentLine (i: int) =
  let pos = !current.lexbuf.Lexing.lex_curr_p in
  !current.lexbuf.Lexing.lex_curr_p <-
    { pos with
      Lexing.pos_lnum = i;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

let setCurrentWorkingDirectory s =
  !current.current_working_directory <- Some s;;

let setCurrentFile ?(normalize=true) (n: string) =
  let n =
    if not normalize then n
    else
      (match !current.current_working_directory with
        | None -> Filepath.normalize n
        | Some(s) -> Sysutil.absolutize_filename s n)
  in
  let pos = !current.lexbuf.Lexing.lex_curr_p in
  !current.lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_fname = n }


let parse_error ?(source=Lexing.lexeme_start_p !current.lexbuf) msg =
  Kernel.abort  ~source msg

(* More parsing support functions: line, file, char count *)
let currentLoc () : Lexing.position * Lexing.position =
  let i = !current in
  Lexing.lexeme_start_p i.lexbuf, Lexing.lexeme_end_p i.lexbuf


(** Handling of errors during parsing *)

let hadErrors = ref false
let had_errors () = !hadErrors
let clear_errors () = hadErrors := false

let set_error (_:Log.event) = hadErrors := true

let () =
  Kernel.register Log.Error set_error;
  Kernel.register Log.Failure set_error
