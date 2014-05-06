(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

{
  open Lexing
  type state = NORMAL | SLASH | INCOMMENT
  type end_of_buffer = NEWLINE | SPACE | CHAR
  let buf = Buffer.create 1024
  let macros = Buffer.create 1024
  let beg_of_line = Buffer.create 8
  let blacklisted_macros = [ "__STDC__"; "__STDC_HOSTED__"; "assert"]
  let is_newline = ref CHAR
  let curr_file = ref ""
  let curr_line = ref 1
  let is_ghost = ref false
  let begin_annot_line = ref 1

  let reset () =
    Buffer.clear buf;
    Buffer.clear macros;
    Buffer.clear beg_of_line;
    is_newline := CHAR;
    curr_file := "";
    curr_line := 1;
    is_ghost := false;
    begin_annot_line := 1

  let backslash = "__BACKSLASH__"

  let abort_preprocess reason outfile =
    let source = {Lexing.dummy_pos with Lexing.pos_fname = !curr_file;
                  pos_lnum = !curr_line;}
    in
    Kernel.error ~source
      "Can't preprocess annotation: %s\nAnnotation will be kept as is"
      reason;
    Buffer.output_buffer outfile buf

  let preprocess_annot suffix cpp outfile =
    (*Printf.printf "Preprocessing annotation:\n%!";
    Buffer.output_buffer stdout buf;
    print_newline(); *)
    let debug =
      Kernel.debug_atleast 3 ||
      Kernel.Debug_category.exists (fun x -> x = "parser")
    in
    let ppname =
      try Extlib.temp_file_cleanup_at_exit ~debug "ppannot" suffix
      with Extlib.Temp_file_error s ->
        Kernel.abort
          "Could not open temporary file for logic pre-processing: %s" s
    in
    let ppfile = open_out ppname in
    Buffer.output_buffer ppfile macros;
    (* NB: the three extra spaces replace the beginning of the annotation
       in order to keep the columns count accurate (at least until there's
       a macro expansion).
    *)
    Printf.fprintf ppfile "# %d %s \n   " !begin_annot_line !curr_file;
    Buffer.output_buffer ppfile beg_of_line;
    Buffer.output_buffer ppfile buf;
    (* cpp complains if the temp file does not end with a newline *)
    Buffer.clear beg_of_line;
    if not (!is_newline = NEWLINE) then output_char ppfile '\n';
    close_out ppfile;
    let cppname = Extlib.temp_file_cleanup_at_exit ~debug "cppannot" suffix in
    let res = Sys.command (cpp ppname cppname) in
    if not debug then Extlib.safe_remove ppname;
    output_string outfile "/*@";
    if !is_ghost then output_string outfile " ghost\n";
    if res <> 0 then begin
      abort_preprocess "Preprocessor call exited with an error" outfile;
      if not debug then Extlib.safe_remove cppname
    end else begin
    try
      let tmp = open_in_bin cppname in
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
              | _ -> (match !state with
                          SLASH->state:=NORMAL
                        | NORMAL | INCOMMENT -> ())
           );
           Buffer.add_char tmp_buf !x;
           x:=c;
         done;
         assert false
       with
           End_of_file ->
             if !is_newline <> CHAR
             then Buffer.add_char tmp_buf !x;
             (* one-line annotations get a new line anyway. *)
             if !state = INCOMMENT then
             Buffer.add_char tmp_buf '\n';
             let res = Buffer.contents tmp_buf in
             let res =
               Str.global_replace (Str.regexp_string backslash) "\\\\" res
             in
             (* Printf.printf "after preprocessing:\n%s%!" res; *)
             output_string outfile res;
             close_in tmp;
             if not debug then Sys.remove cppname)
    with
      | End_of_file ->
        if not debug then (try Sys.remove cppname with Sys_error _ -> ());
        abort_preprocess "Empty result in annotation pre-processing" outfile
      | Sys_error e ->
          if not debug then (try Sys.remove cppname with Sys_error _ -> ());
          abort_preprocess ("System error: " ^ e) outfile

    end;
    Printf.fprintf outfile "*/\n# %d %s\n%!" !curr_line !curr_file;
    Buffer.clear buf

  let make_newline () =
    incr curr_line;
    Buffer.clear beg_of_line
}

rule main suffix cpp outfile = parse
  | ("#define"|"#undef") [' ''\t']* ((['a'-'z''A'-'Z''0'-'9''_'])* as m)
      [^'\n']* '\n'
      {
        if not (List.mem m blacklisted_macros) then
          Buffer.add_string macros (lexeme lexbuf);
	output_char outfile '\n';
        make_newline ();
        main suffix cpp outfile lexbuf
      }
  | "#"  [' ''\t']* "line"?  [' ''\t']* (['0'-'9']+ as line)
    [' ''\t']* (('"' [^'"']+ '"') as file)  [^'\n']* "\n"
    { (try
        curr_line := (int_of_string line) -1
       with Failure "int_of_string" -> curr_line:= -1);
      if file <> "" then curr_file := file;
      output_string outfile (lexeme lexbuf);
      make_newline();
      main suffix cpp outfile lexbuf
    }
  | "/*"  (_ as c) {
      if c = !Clexer.annot_char then begin
        is_newline:=CHAR;
        begin_annot_line := ! curr_line;
        Buffer.clear buf;
        maybe_ghost suffix cpp outfile lexbuf
      end else begin
        output_string outfile (lexeme lexbuf);
        if c = '\n' then make_newline();
        Buffer.add_string beg_of_line "   ";
        comment suffix cpp outfile c lexbuf;
      end}
  | "//"  (_ as c) {
      if c = !Clexer.annot_char then begin
        Buffer.clear buf;
        begin_annot_line := !curr_line;
        is_newline:=CHAR;
        maybe_oneline_ghost suffix cpp outfile lexbuf
      end
      else if c = '\n' then begin
        make_newline ();
        output_string outfile (lexeme lexbuf);
        main suffix cpp outfile lexbuf
      end
      else begin
        output_string outfile (lexeme lexbuf);
        oneline_comment suffix cpp outfile lexbuf;
      end}
  | eof  { flush outfile }
  | '\n' {
      make_newline ();
      output_char outfile '\n';
      main suffix cpp outfile lexbuf }
  | '"' { 
      Buffer.add_char beg_of_line ' ';
      output_char outfile '"'; 
      c_string suffix cpp outfile lexbuf }
  | "'" { 
      Buffer.add_char beg_of_line ' ';
      output_char outfile '\'';
      c_char suffix cpp outfile lexbuf }
  | _ as c {
      Buffer.add_char beg_of_line ' ';
      output_char outfile c;
      main suffix cpp outfile lexbuf }

and c_string suffix cpp outfile = parse
| "\\\"" { Buffer.add_string beg_of_line "  ";
           output_string outfile (lexeme lexbuf);
           c_string suffix cpp outfile lexbuf }
| "\"" { Buffer.add_char beg_of_line ' '; 
         output_char outfile '"';
         main suffix cpp outfile lexbuf }
| '\n' { make_newline ();
         output_char outfile '\n';
         c_string suffix cpp outfile lexbuf
       }
| "\\\\" { Buffer.add_string beg_of_line "  ";
           output_string outfile (lexeme lexbuf);
           c_string suffix cpp outfile lexbuf }
| _ as c { Buffer.add_char beg_of_line ' ';
           output_char outfile c;
           c_string suffix cpp outfile lexbuf }
(* C syntax allows for multiple char character constants *)
and c_char suffix cpp outfile = parse
| "\\\'" { Buffer.add_string beg_of_line "  ";
           output_string outfile (lexeme lexbuf);
           c_char suffix cpp outfile lexbuf }
| "'" { Buffer.add_char beg_of_line ' '; 
         output_char outfile '\'';
         main suffix cpp outfile lexbuf }
| '\n' { make_newline ();
         output_char outfile '\n';
         c_char suffix cpp outfile lexbuf
       }
| "\\\\" { Buffer.add_string beg_of_line "  ";
           output_string outfile (lexeme lexbuf);
           c_char suffix cpp outfile lexbuf }
| _ as c { Buffer.add_char beg_of_line ' ';
           output_char outfile c;
           c_char suffix cpp outfile lexbuf }

and maybe_ghost suffix cpp outfile = parse
   [' ''\t']+ as space{
     Buffer.add_string buf space;
     maybe_ghost suffix cpp outfile lexbuf }
  | '\n' {
      is_newline := NEWLINE;
      incr curr_line;
      Buffer.add_char buf '\n';
      maybe_ghost suffix cpp outfile lexbuf
    }
  | "ghost"
      { is_ghost := true;
        Buffer.add_string buf "     ";
        annot suffix cpp outfile lexbuf
      }
  (* silently skipping an empty annotation *)
  | "*/" { main suffix cpp outfile lexbuf }
  | _ as c { Buffer.add_char buf c; is_ghost:=false;
             annot suffix cpp outfile lexbuf}
and maybe_oneline_ghost suffix cpp outfile = parse
   [' ''\t']+ as space{
     Buffer.add_string buf space;
     maybe_oneline_ghost suffix cpp outfile lexbuf }
  | '\n' {
      incr curr_line;
      main suffix cpp outfile lexbuf
    }
  | "ghost"
      { is_ghost := true;
        Buffer.add_string buf "     ";
        oneline_annot suffix cpp outfile lexbuf
      }
  | _ as c
      {
        Buffer.add_char buf c;
        is_ghost:=false;
        oneline_annot suffix cpp outfile lexbuf
      }
and annot suffix cpp outfile = parse
    "*/"  { preprocess_annot suffix cpp outfile;
            main suffix cpp outfile lexbuf }
  | '\n' { is_newline := NEWLINE;
           incr curr_line;
           Buffer.add_char buf '\n';
           annot suffix cpp outfile lexbuf }
  | "//" { Buffer.add_string buf "//";
           annot_comment suffix cpp outfile lexbuf }
  | '@' {
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char buf ' ';
      annot suffix cpp outfile lexbuf }
  | ' '  {
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char buf ' ';
      annot suffix cpp outfile lexbuf }
  (* We're not respecting char count here. Maybe using '$' would do it,
     as cpp is likely to count it as part of an identifier, but this would
     imply that we can not speak about $ ident in annotations.
   *)
  | '\\' { Buffer.add_string buf backslash;
           annot suffix cpp outfile lexbuf }
  | '\'' { Buffer.add_char buf '\'';
           char suffix annot cpp outfile lexbuf }
  | '"'  { Buffer.add_char buf '"';
           string suffix annot cpp outfile lexbuf }
  | _ as c { is_newline := CHAR;
             Buffer.add_char buf c;
             annot suffix cpp outfile lexbuf }

and annot_comment suffix cpp outfile = parse
  | '\n' { incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char buf '\n';
           annot suffix cpp outfile lexbuf
         }
  | "*/" { preprocess_annot suffix cpp outfile;
           main suffix cpp outfile lexbuf }
  | eof { abort_preprocess "eof in the middle of a comment" outfile }
  | _ as c {
    Buffer.add_char buf c;
    annot_comment suffix cpp outfile lexbuf }

and char suffix annot cpp outfile = parse

  | '\n' { incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char buf '\n';
           char suffix annot cpp outfile lexbuf
         }
  | '\'' { is_newline:=CHAR;
           Buffer.add_char buf '\'';
           annot suffix cpp outfile lexbuf }
  | "\\'" { is_newline:=CHAR;
            Buffer.add_string buf "\\'";
            char suffix annot cpp outfile lexbuf }
  | "\\\\" { is_newline:=CHAR;
            Buffer.add_string buf "\\\\";
            char suffix annot cpp outfile lexbuf }
  | eof { abort_preprocess "eof while parsing a char literal" outfile }
  | _ as c { is_newline:=CHAR;
             Buffer.add_char buf c;
             char suffix annot cpp outfile lexbuf }

and string suffix annot cpp outfile = parse
  | '\n' { incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char buf '\n'; string suffix annot cpp outfile lexbuf
         }
  | '"' { is_newline:=CHAR; Buffer.add_char buf '"';
          annot suffix cpp outfile lexbuf }
  | "\\\"" { is_newline:=CHAR;
             Buffer.add_string buf "\\\"";
             string suffix annot cpp outfile lexbuf }
  | eof { abort_preprocess "eof while parsing a string literal" outfile }
  | _ as c { is_newline:=CHAR;
             Buffer.add_char buf c;
             string suffix annot cpp outfile lexbuf }

and comment suffix cpp outfile c =
parse
    "/" {
      Buffer.add_char beg_of_line ' ';
      output_string outfile (lexeme lexbuf);
      if c = '*' then
        main suffix cpp outfile lexbuf
      else
        comment suffix cpp outfile '/' lexbuf
      }
  | '\n' { make_newline (); output_char outfile '\n';
           comment suffix cpp outfile '\n' lexbuf }
  | eof { abort_preprocess "eof while parsing C comment" outfile}
  | _ as c {
      Buffer.add_char beg_of_line ' ';
      output_char outfile c;
      comment suffix cpp outfile c lexbuf}

and oneline_annot suffix cpp outfile = parse
    "\n"|eof {
      incr curr_line;
      preprocess_annot suffix cpp outfile;
      main suffix cpp outfile lexbuf }
  | '@'  { Buffer.add_char buf ' ';
           oneline_annot suffix cpp outfile lexbuf
         }
  | '\\' { Buffer.add_string buf backslash;
           oneline_annot suffix cpp outfile lexbuf }
  | '\'' { Buffer.add_char buf '\'';
           char suffix oneline_annot cpp outfile lexbuf }
  | '"'  { Buffer.add_char buf '"';
           string suffix oneline_annot cpp outfile lexbuf }
  | _ as c { Buffer.add_char buf c;
             oneline_annot suffix cpp outfile lexbuf }

and oneline_comment suffix cpp outfile =
parse
    "\n"|eof
      { make_newline();
        output_string outfile (lexeme lexbuf);
        main suffix cpp outfile lexbuf}
  | _ as c { output_char outfile c;
             oneline_comment suffix cpp outfile lexbuf}

{
  let file suffix cpp filename =
    reset ();
    let debug = Kernel.Debug_category.exists (fun x -> x = "parser") in
    let inchan = open_in_bin filename in
    let lex = Lexing.from_channel inchan in
    let ppname =
      Extlib.temp_file_cleanup_at_exit ~debug
        (Filename.basename filename) ".pp"
    in
    let ppfile = open_out ppname in
    main suffix cpp ppfile lex;
    close_in inchan;
    close_out ppfile;
    ppname
}

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
