(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2001-2003                                               *)
(*   George C. Necula    <necula@cs.berkeley.edu>                         *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                        *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                         *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                         *)
(*  All rights reserved.                                                  *)
(*                                                                        *)
(*  Redistribution and use in source and binary forms, with or without    *)
(*  modification, are permitted provided that the following conditions    *)
(*  are met:                                                              *)
(*                                                                        *)
(*  1. Redistributions of source code must retain the above copyright     *)
(*  notice, this list of conditions and the following disclaimer.         *)
(*                                                                        *)
(*  2. Redistributions in binary form must reproduce the above copyright  *)
(*  notice, this list of conditions and the following disclaimer in the   *)
(*  documentation and/or other materials provided with the distribution.  *)
(*                                                                        *)
(*  3. The names of the contributors may not be used to endorse or        *)
(*  promote products derived from this software without specific prior    *)
(*  written permission.                                                   *)
(*                                                                        *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,   *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *)
(*  POSSIBILITY OF SUCH DAMAGE.                                           *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(**************************************************************************)

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
      mutable hfile   : string   ; (* High-level file *)
      mutable hline   : int;       (* High-level line *)
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
      hfile     = "";
      hline     = 0;
      num_errors = 0;
    }

let current = ref dummyinfo
let first_filename_encountered = ref None

let setHLine (l: int) : unit =
    !current.hline <- l
let setHFile (f: string) : unit =
    !current.hfile <- f

let rem_quotes str = String.sub str 1 ((String.length str) - 2)

(* Change \ into / in file names. To avoid complications with escapes
   [BM] DO NOT USE this function. It mutates [str] and does not take care of its length. *)
(*let cleanFileName str =
  let str1 =
    if str <> "" && String.get str 0 = '"' (* '"' ( *)
    then rem_quotes str else str in
  let l = String.length str1 in
  let rec loop (copyto: int) (i: int) =
    if i >= l then
      String.sub str1 0 copyto
     else
       let c = String.get str1 i in
       if c <> '\\' then begin
          String.set str1 copyto c; loop (copyto + 1) (i + 1)
       end else begin
          String.set str1 copyto '/';
          if i < l - 2 && String.get str1 (i + 1) = '\\' then
              loop (copyto + 1) (i + 2)
          else
              loop (copyto + 1) (i + 1)
       end
  in
  loop 0 0
*)

let readingFromStdin = ref false

let startParsing ?(useBasename=true) (fname: string) =
  (* We only support one open file at a time *)
  if !current != dummyinfo then begin
     (Cilmsg.abort "Errormsg.startParsing supports only one open file: You want to open %s and %s is still open@\n" fname !current.fileName);
  end;
  let inchan =
    try if fname = "-" then begin
           readingFromStdin := true;
           stdin
        end else begin
           readingFromStdin := false;
          open_in_bin fname
        end
    with e -> (Cilmsg.abort "Cannot find input file %s (exception %s"
                    fname (Printexc.to_string e)) in
  let lexbuf = Lexing.from_channel inchan in
  let i =
    { linenum = 1; linestart = 0;
      fileName =
        (*cleanFileName*) (if useBasename then Filename.basename fname else fname);
      lexbuf = lexbuf; inchan = Some inchan;
      hfile = ""; hline = 0;
      num_errors = 0 } in
  (* Initialize lexer buffer. *)
  lexbuf.Lexing.lex_curr_p <-
    { Lexing.pos_fname = i.fileName;
      Lexing.pos_lnum  = 1;
      Lexing.pos_bol   = 0;
      Lexing.pos_cnum  = 0
    };
  current := i;
  first_filename_encountered := None;
  lexbuf

let startParsingFromString ?(file="<string>") ?(line=1) (str: string) =
  let lexbuf = Lexing.from_string str in
  let i =
    { linenum = line; linestart = line - 1;
      fileName = file;
      hfile = ""; hline = 0;
      lexbuf = lexbuf;
      inchan = None;
      num_errors = 0 }
  in
  current := i;
  first_filename_encountered := None;
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

let newHline () =
  let i = !current in
  i.hline <- 1 + i.hline

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

let setCurrentFile (n: string) =
  (* Update lexer buffer. *)
  let update_file_loc lexbuf file =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
	Lexing.pos_fname = file;
      }
  in
  update_file_loc !current.lexbuf n;
  (if !first_filename_encountered = None
   then first_filename_encountered:=Some n);
  (* Default CIL location update. *)
  !current.fileName <- (*cleanFileName*) n


let max_errors = 20  (* Stop after 20 errors *)

let parse_error (msg: string) : 'a =
  let i = !current in
  i.num_errors <- i.num_errors + 1;
  if i.num_errors > max_errors then
    Cilmsg.abort "Too many errors."
  else
    Cilmsg.with_error
      (fun _ -> raise Parsing.Parse_error)
      ~source:{
	Log.src_file = i.fileName ;
	Log.src_line = i.linenum ;
      } "%s" msg

(* More parsing support functions: line, file, char count *)
let getPosition () : Lexing.position * Lexing.position =
  let i = !current in
  Lexing.lexeme_start_p i.lexbuf, Lexing.lexeme_end_p i.lexbuf

let getHPosition () =
  !current.hline, !current.hfile

(** Type for source-file locations *)
type location =
    { file: string; (** The file name *)
      line: int;    (** The line number *)
      hfile: string; (** The high-level file name, or "" if not present *)
      hline: int;    (** The high-level line number, or 0 if not present *)
    }

let d_loc fmt l = Format.fprintf fmt "%s:%d" l.file l.line

let d_hloc fmt l = 
  Format.fprintf fmt "%s:%d:" l.file l.line ;
  if l.hline > 0 then Format.fprintf fmt " (%s:%d)" l.hfile l.hline

let locUnknown = { file = ""; hfile = ""; line = -1; hline = -1 }

let getLocation () =
  let hl, hf = getHPosition () in
  let pos = getPosition () in
  { hfile = hf; hline = hl;
    file = (fst pos).Lexing.pos_fname; line = (fst pos).Lexing.pos_lnum }
