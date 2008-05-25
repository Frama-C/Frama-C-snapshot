(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(*$Id: logic_preprocess.mll,v 1.12 2008/05/23 12:39:23 uid528 Exp $*)
{
  open Lexing
  type state = NORMAL | SLASH | INCOMMENT
  let debug = false
  let buf = Buffer.create 1024
  let macros = Buffer.create 1024
  let blacklisted_macros = [ "__STDC__"; "__STDC_HOSTED__"]
  let is_newline = ref false
  let preprocess_annot is_oneline cpp outfile =
    let (ppname, ppfile) = Filename.open_temp_file "ppannot" ".c" in
    Buffer.output_buffer ppfile macros;
    Buffer.output_buffer ppfile buf;
    (* cpp complains if the temp file does not end with a newline *)
    if not (!is_newline) then output_char ppfile '\n';
    close_out ppfile;
    let cppname = Filename.temp_file "cppannot" ".c" in
    let res = Sys.command (cpp ppname cppname) in
    if not debug then (try Sys.remove ppname with Sys_error _ -> ());
    output_string outfile (if is_oneline then "//@" else "/*@");
    if res <> 0 then begin
    Printf.eprintf
      "Warning: could not preprocess logical annotation. Keeping as is%!";
    Buffer.output_buffer outfile buf;
    if not debug then (try Sys.remove cppname with Sys_error _ -> ())
    end else begin
    try
      let tmp = open_in cppname in
      let tmp_buf = Buffer.create 1024 in
      Buffer.clear tmp_buf;
      let x = ref (input_char tmp) in
      let state = ref NORMAL in
      (try
         while true do
           (* we have to remove the spurious \n at the end of buffer*)
           let c = input_char tmp in
           (match !x with
                '/' ->
                  (match !state with
                       NORMAL -> state:=SLASH
                     | SLASH ->state:=INCOMMENT
                     | INCOMMENT -> ()
                  )
              | '\n' -> state:=NORMAL
              | _ -> ());
           Buffer.add_char tmp_buf !x;
           x:=c;
         done;
         assert false
       with
           End_of_file ->
             if !is_newline
             then Buffer.add_char tmp_buf !x;
             if !state = INCOMMENT then
             Buffer.add_char tmp_buf '\n';
             Buffer.output_buffer outfile tmp_buf;
             close_in tmp;
             if not debug then Sys.remove cppname)
    with
      | Sys_error _ ->
          if not debug then (try Sys.remove cppname with Sys_error _ -> ());
          Printf.eprintf
            "Warning: could not preprocess logical annotation. \
             Keeping as is%!";
          Buffer.output_buffer outfile buf;

    end;
    output_string outfile (if is_oneline then "\n" else "*/");
    Buffer.clear buf
}

rule main cpp outfile = parse
  | "#define" [' ''\t']* ((['a'-'z''A'-'Z''0'-'9''_'])* as m)
      [^'\n']* '\n'
      {
        if not (List.mem m blacklisted_macros) then
          Buffer.add_string macros (lexeme lexbuf);
	output_char outfile '\n';
        main cpp outfile lexbuf
      }
  | "/*"  ([^ '*' '\n'] as c) {
      if c = !Clexer.annot_char then begin
      is_newline:=false;
      Buffer.clear buf;
      annot cpp outfile lexbuf
      end else begin
      output_string outfile (lexeme lexbuf);
      comment cpp outfile lexbuf;
      end}
  | "//"  ([^ '*' '\n'] as c) {
      if c = !Clexer.annot_char then begin
      Buffer.clear buf;
      is_newline:=false;
      oneline_annot cpp outfile lexbuf
      end else begin
      output_string outfile (lexeme lexbuf);
      oneline_comment cpp outfile lexbuf;
      end}
  | eof  { flush outfile }
  | _ as c { output_char outfile c; main cpp outfile lexbuf }

and annot cpp outfile = parse
    "*/"  { preprocess_annot false cpp outfile; main cpp outfile lexbuf }
  | '\n' { is_newline := true; Buffer.add_char buf '\n';
           annot cpp outfile lexbuf }
  | _ as c { is_newline := false;
             Buffer.add_char buf c; annot cpp outfile lexbuf }

and comment cpp outfile =
parse
    "*/" { output_string outfile (lexeme lexbuf);
           main cpp outfile lexbuf }
  | _ as c { output_char outfile c; comment cpp outfile lexbuf}

and oneline_annot cpp outfile = parse
    "\n"|eof { preprocess_annot true cpp outfile;
               main cpp outfile lexbuf }
  | _ as c { Buffer.add_char buf c; oneline_annot cpp outfile lexbuf }

and oneline_comment cpp outfile =
parse
    "\n"|eof
      { output_string outfile (lexeme lexbuf);
        main cpp outfile lexbuf}
  | _ as c { output_char outfile c; oneline_comment cpp outfile lexbuf}

{
  let file cpp filename =
    let inchan = open_in filename in
    let lex = Lexing.from_channel inchan in
    let (ppname, ppfile) = Filename.open_temp_file
      (Filename.basename filename) ".pp"
    in
    main cpp ppfile lex;
    close_in inchan;
    close_out ppfile;
    ppname
}

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
