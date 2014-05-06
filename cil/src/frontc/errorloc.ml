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

(*
 *
 * Copyright (c) 2001-2002,
 *  George C. Necula    <necula@cs.berkeley.edu>
 *  Scott McPeak        <smcpeak@cs.berkeley.edu>
 *  Wes Weimer          <weimer@cs.berkeley.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)

(* Copied and modified from [cil/src/errormsg.ml] *)

(***** Handling parsing errors ********)
type parseinfo =
    { mutable  linenum: int      ; (* Current line *)
      mutable  linestart: int    ; (* The position in the buffer where the
                                    * current line starts *)
      mutable fileName : string   ; (* Current file *)
      lexbuf          : Lexing.lexbuf;
      inchan          : in_channel option; (* None, if from a string *)
      mutable   num_errors : int;  (* Errors so far *)
    }

let dummyinfo =
    { linenum   = 1;
      linestart = 0;
      fileName  = "" ;
      lexbuf    = Lexing.from_string "";
      inchan    = None;
      num_errors = 0;
    }

let current = ref dummyinfo
let current_working_directory = ref None

let readingFromStdin = ref false

let startParsing ?(useBasename=true) fname =
  (* We only support one open file at a time *)
  if !current != dummyinfo then begin
    Kernel.fatal
      "[Errorloc.startParsing] supports only one open file: \
You want to open %S and %S is still open"
      fname !current.fileName;
  end;
  let inchan =
    try
      if fname = "-" then begin
        readingFromStdin := true;
        stdin
      end else begin
        readingFromStdin := false;
        open_in_bin fname
      end
    with Sys_error s ->
      Kernel.abort "Cannot find input file %S: %s" fname s
  in
  let lexbuf = Lexing.from_channel inchan in
  let i =
    { linenum = 1; linestart = 0;
      fileName =
	(if useBasename then Filename.basename fname
         else Filepath.normalize fname);
      lexbuf = lexbuf; inchan = Some inchan;
      num_errors = 0 } in
  (* Initialize lexer buffer. *)
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = i.fileName;
      Lexing.pos_lnum  = 1;
      Lexing.pos_bol   = 0;
      Lexing.pos_cnum  = 0
    };
  current := i;
  current_working_directory  := None;
  lexbuf

let finishParsing () =
  let i = !current in
  (match i.inchan with Some c -> close_in c | _ -> ());
  current := dummyinfo


(* Call this function to announce a new line *)
let newline () =
  (* Update lexer buffer. *)
  let update_newline_loc lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
	Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	Lexing.pos_bol = pos.Lexing.pos_cnum;
      }
  in
  update_newline_loc !current.lexbuf;
  (* Default CIL location update. *)
  let i = !current in
  i.linenum <- 1 + i.linenum;
  i.linestart <- Lexing.lexeme_start i.lexbuf

let setCurrentLine (i: int) =
  (* Update lexer buffer. *)
  let update_line_loc lexbuf line absolute chars =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
	Lexing.pos_lnum = if absolute then line else pos.Lexing.pos_lnum + line;
	Lexing.pos_bol = pos.Lexing.pos_cnum - chars;
      }
  in
  update_line_loc !current.lexbuf i true 0;
  (* Default CIL location update. *)
  !current.linenum <- i

let setCurrentWorkingDirectory s =
  current_working_directory := Some(s);;

let setCurrentFile ?(normalize=true) (n: string) =
  let n =
    if not normalize then n
    else
      (match !current_working_directory with
        | None -> Filepath.normalize n
        | Some(s) -> Sysutil.absolutize_filename s n)
  in
  (* Update lexer buffer. *)
  let update_file_loc lexbuf file =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
	Lexing.pos_fname = file;
      }
  in
  update_file_loc !current.lexbuf n;
  (* Default CIL location update. *)
  !current.fileName <- n


let max_errors = 20  (* Stop after 20 errors *)

let parse_error (msg: string) : 'a =
  let i = !current in
  i.num_errors <- i.num_errors + 1;
  if i.num_errors > max_errors then
    Kernel.abort "Too many errors."
  else
    Kernel.with_error
      (fun _ -> raise Parsing.Parse_error)
      ~source:{Lexing.pos_fname= i.fileName ;
	       pos_lnum = i.linenum;
               pos_bol = i.linestart;
               pos_cnum = 0;}
      "%s" msg

(* More parsing support functions: line, file, char count *)
let getPosition () : Lexing.position * Lexing.position =
  let i = !current in
  Lexing.lexeme_start_p i.lexbuf, Lexing.lexeme_end_p i.lexbuf

(** Type for source-file locations *)
type location =
    { file: string; (** The file name *)
      line: int;    (** The line number *)
    }

let d_loc fmt l = Format.fprintf fmt "%s:%d" l.file l.line

let locUnknown = { file = ""; line = -1 }

