(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
  type end_of_buffer = NEWLINE | SPACE | CHAR
  let preprocess_buffer = Buffer.create 1024
  let output_buffer = Buffer.create 1024
  let beg_of_line = Buffer.create 8
  (* Standard prohibits the predefined macros to be subject of a #define
     (or #undef) directive. We thus have to filter the definition of these
     macros from gcc's output (gcc emits a warning otherwise).
     The list of predefined macros is taken from C11 standard, in the order
     in which they are defined in Section 6.10.8
   *)
  let blacklisted_macros = [
    (* 6.10.8.1 mandatory macros. *)
    "__DATE__"; "__FILE"; "__LINE__"; "__STDC__"; "__STDC_HOSTED__";
    "__STDC_VERSION__"; "__TIME__";
    (* 6.10.8.2 environment macros *)
    "__STDC_ISO_10646__"; "__STDC_MB_MIGHT_NEQ_WC__";
    "__STDC_UTF_16__"; "__STDC_UTF_32__";
    (* 6.10.8.3 conditional feature macros *)
    "__STDC_ANALYZABLE__"; "__STDC_IEC_559__"; "__STDC_IEC_559_COMPLEX__";
    "__STDC_LIB_EXT1__"; "__STD_NO_ATOMICS__"; "__STD_NO_COMPLEX__";
    "__STDC_NO_THREADS__"; "__STDC_NO_VLA__";
    (* expanding assert, an ACSL keyword, is not a good idea. *)
    "assert"]
  let is_newline = ref CHAR
  let curr_file = ref ""
  let curr_line = ref 1
  let has_annot = ref false

  let reset () =
    Buffer.clear preprocess_buffer;
    Buffer.clear output_buffer;
    Buffer.clear beg_of_line;
    is_newline := CHAR;
    curr_file := "";
    curr_line := 1;
    has_annot := false

  let backslash = "__ANNOT_BACKSLASH__"
  let annot_content = "__ANNOT_CONTENT__"

  let re_backslash = Str.regexp_string backslash
  let re_annot_content = Str.regexp_string annot_content

  (* Delimiters for the various annotations in the preprocessing buffer.
     We have one delimiter for the beginning of an annotation (to discard
     #defines along the way), and three delimiters for the various ways
     an annotation can end:
      - on a normal line
      - with a newline
      - with a newline inside a comment (only for one-line annotations)
     When preprocessed annotations are inserted back in the main file, this will
     result in distinct translation to preserve line numbers while avoiding
     ill-formed annotations.
  *)
  let annot_beg =         "////////////////__ANNOT_BEG__"
  let annot_end =         "////////////////__ANNOT_END__"
  let annot_end_nl  =     "////////////////__ANNOT_END_NL__"
  let annot_end_comment = "////////////////__ANNOT_END_COMMENT__"

  let abort_preprocess reason =
    let source = {Lexing.dummy_pos with Lexing.pos_fname = !curr_file;
                  pos_lnum = !curr_line;}
    in
    Kernel.error ~source
      "Can't preprocess annotation: %s\nSome annotations will be kept as is"
      reason

  let next_preprocessed file =
    let content = Buffer.create 80 in
    let rec ignore_content () =
      let s = input_line file in
      if s <> annot_beg then ignore_content ()
    in
    let rec get_annot first =
      let s = input_line file in
      if s = annot_end then false, Buffer.contents content
      else if s = annot_end_nl then true, Buffer.contents content
      else if s = annot_end_comment then begin
        Buffer.add_char content '\n';
        false, Buffer.contents content
      end else begin
        if not first then Buffer.add_char content '\n';
        Buffer.add_string content s;
        get_annot false
      end
    in
    let replace_backslash s = Str.global_replace re_backslash "\\\\" s in
    try
      ignore_content ();
      ignore (input_line file); (* ignore the #line directive *)
      let with_nl, content = get_annot true in
      with_nl, replace_backslash content
    with End_of_file ->
      Kernel.fatal
        "too few annotations in result file while pre-processing annotations"

  let output_result outfile preprocessed content =
    let rec aux = function
      | [] -> ()
      | [s] -> output_string outfile s
      | content :: rem ->
          output_string outfile content;
          output_string outfile "/*@";
          let with_nl, pp_content = next_preprocessed preprocessed in
          output_string outfile pp_content;
          output_string outfile "*/";
          if with_nl then output_char outfile '\n';
          aux rem
    in aux content

  let preprocess_annots suffix cpp outfile =
    if !has_annot then begin
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
      Buffer.output_buffer ppfile preprocess_buffer;
      close_out ppfile;
      let cppname = Extlib.temp_file_cleanup_at_exit ~debug "cppannot" suffix in
      let res = Sys.command (cpp ppname cppname) in
      let result_file =
        if res <> 0 then begin
          abort_preprocess "Preprocessor call exited with an error";
          if not debug then Extlib.safe_remove cppname;
          ppname
        end else cppname
      in
      let result = open_in result_file in
      let content =
        Str.split_delim re_annot_content (Buffer.contents output_buffer)
      in
      output_result outfile result content;
    end else begin
      Buffer.output_buffer outfile output_buffer
    end;
    flush outfile

  let add_preprocess_line_info () =
    Printf.bprintf
      preprocess_buffer "# %d %s \n%s   "
      !curr_line !curr_file (Buffer.contents beg_of_line);
    Buffer.clear beg_of_line

  let make_newline () =
    incr curr_line;
    Buffer.clear beg_of_line
}

rule main = parse
  | ("#define"|"#undef") [' ''\t']* ((['a'-'z''A'-'Z''0'-'9''_'])* as m)
      {
        let blacklisted = List.mem m blacklisted_macros in
        if not blacklisted then
          Buffer.add_string preprocess_buffer (lexeme lexbuf);
        macro blacklisted lexbuf
      }
  | "#"  [' ''\t']* "line"?  [' ''\t']* (['0'-'9']+ as line)
    [' ''\t']* (('"' [^'"']+ '"') as file)  [^'\n']* "\n"
    { (try
        curr_line := (int_of_string line) -1
       with Failure "int_of_string" -> curr_line:= -1);
      if file <> "" then curr_file := file;
      Buffer.add_string output_buffer (lexeme lexbuf);
      make_newline();
      main lexbuf
    }
  | "/*@" ('{' | '}' as c) { (* Skip special doxygen comments. Use of '@'
                                instead of !Clexer.annot_char is intentional *)
        Buffer.add_string beg_of_line "   ";
        Buffer.add_string output_buffer (lexeme lexbuf);
        comment c lexbuf;}
  | "/*"  (_ as c) {
      if c = !Clexer.annot_char then begin
        is_newline:=CHAR;
        has_annot := true;
        Buffer.add_string output_buffer annot_content;
        Buffer.add_string preprocess_buffer annot_beg;
        Buffer.add_char preprocess_buffer '\n';
        add_preprocess_line_info();
        annot lexbuf
      end else begin
        if c = '\n' then make_newline()
        else Buffer.add_string beg_of_line "   ";
        Buffer.add_string output_buffer (lexeme lexbuf);
        comment c lexbuf;
      end}
  | "//@" ('{' | '}') { (* See comments for "/*@{" above *)
        Buffer.add_string output_buffer (lexeme lexbuf);
        oneline_comment lexbuf;
      } 
  | "//"  (_ as c) {
      if c = !Clexer.annot_char then begin
        is_newline:=CHAR;
        has_annot:=true;
        Buffer.add_string output_buffer annot_content;
        Buffer.add_string preprocess_buffer annot_beg;
        Buffer.add_char preprocess_buffer '\n';
        add_preprocess_line_info();
        oneline_annot lexbuf
      end
      else if c = '\n' then begin
        make_newline ();
        Buffer.add_string output_buffer (lexeme lexbuf);
        main lexbuf
      end
      else begin
        Buffer.add_string output_buffer (lexeme lexbuf);
        oneline_comment lexbuf;
      end}
  | '\n' {
      make_newline (); Buffer.add_char output_buffer '\n'; main lexbuf }
  | eof  { }
  | '"' {
      Buffer.add_char beg_of_line ' ';
      Buffer.add_char output_buffer '"'; 
      c_string lexbuf }
  | "'" { 
      Buffer.add_char beg_of_line ' ';
      Buffer.add_char output_buffer '\'';
      c_char lexbuf }
  | _ as c {
      Buffer.add_char beg_of_line ' ';
      Buffer.add_char output_buffer c;
      main lexbuf }
and macro blacklisted = parse
| "\\\n" {
      make_newline ();
      Buffer.add_char output_buffer '\n';
      macro blacklisted lexbuf
    }
(* we ignore comments in macro definition, as their expansion 
   in ACSL annotations would lead to ill-formed ACSL. *)
| "/*" { macro_comment blacklisted lexbuf }
| '"' { 
  if not blacklisted then
    Buffer.add_char preprocess_buffer '"';
  macro_string blacklisted lexbuf
}
| "'" {
  if not blacklisted then
    Buffer.add_char preprocess_buffer '\'';
  macro_char blacklisted lexbuf
}
| "\n" {
      if not blacklisted then
        Buffer.add_char preprocess_buffer '\n';
      make_newline ();
      Buffer.add_char output_buffer '\n';
      main lexbuf
    }
| _ as c {
           if not blacklisted then
             Buffer.add_char preprocess_buffer c;
           macro blacklisted lexbuf
         }
and macro_comment blacklisted = parse
| '\n' {
      make_newline ();

      macro_comment blacklisted lexbuf
    }
| "*/" { macro blacklisted lexbuf }
| _  { macro_comment blacklisted lexbuf }

and macro_string blacklisted = parse
|  "\\\"" as s {
  if not blacklisted then Buffer.add_string preprocess_buffer s;
  macro_string blacklisted lexbuf
  }
| "\\\n" {
  make_newline();
  Buffer.add_char output_buffer '\n';
  macro_string blacklisted lexbuf
}
| "\n" { abort_preprocess "unterminated string in macro definition" }
| eof { abort_preprocess "unterminated string in macro definition" }
| '"' { if not blacklisted then Buffer.add_char preprocess_buffer '"';
        macro blacklisted lexbuf }
| _ as c { if not blacklisted then Buffer.add_char preprocess_buffer c;
           macro_string blacklisted lexbuf }
and macro_char blacklisted = parse
|  "\\'" as s {
  if not blacklisted then Buffer.add_string preprocess_buffer s;
  macro_char blacklisted lexbuf
  }
| "\\\n" {
  make_newline();
  Buffer.add_char output_buffer '\n';
  macro_char blacklisted lexbuf
}
| "\n" { abort_preprocess "unterminated char in macro definition" }
| eof { abort_preprocess "unterminated char in macro definition" }
| "'" { if not blacklisted then Buffer.add_char preprocess_buffer '\'';
        macro blacklisted lexbuf }
| _ as c { if not blacklisted then Buffer.add_char preprocess_buffer c;
           macro_char blacklisted lexbuf }
and c_string = parse
| "\\\"" { Buffer.add_string beg_of_line "  ";
           Buffer.add_string output_buffer (lexeme lexbuf);
           c_string lexbuf }
| "\"" { Buffer.add_char beg_of_line ' '; 
         Buffer.add_char output_buffer '"';
         main lexbuf }
| '\n' { make_newline ();
         Buffer.add_char output_buffer '\n';
         c_string lexbuf
       }
| "\\\\" { Buffer.add_string beg_of_line "  ";
           Buffer.add_string output_buffer (lexeme lexbuf);
           c_string lexbuf }
| _ as c { Buffer.add_char beg_of_line ' ';
           Buffer.add_char output_buffer c;
           c_string lexbuf }
(* C syntax allows for multiple char character constants *)
and c_char = parse
| "\\\'" { Buffer.add_string beg_of_line "  ";
           Buffer.add_string output_buffer (lexeme lexbuf);
           c_char lexbuf }
| "'" { Buffer.add_char beg_of_line ' '; 
         Buffer.add_char output_buffer '\'';
         main lexbuf }
| '\n' { make_newline ();
         Buffer.add_char output_buffer '\n';
         c_char lexbuf
       }
| "\\\\" { Buffer.add_string beg_of_line "  ";
           Buffer.add_string output_buffer (lexeme lexbuf);
           c_char lexbuf }
| _ as c { Buffer.add_char beg_of_line ' ';
           Buffer.add_char output_buffer c;
           c_char lexbuf }

and annot = parse
    "*/"  {
      if !is_newline = NEWLINE then
        Buffer.add_string preprocess_buffer annot_end_nl
      else begin
        Buffer.add_char preprocess_buffer '\n';
        Buffer.add_string preprocess_buffer annot_end;
      end;
      Buffer.add_char preprocess_buffer '\n';
      main lexbuf }
  | '\n' { is_newline := NEWLINE;
           incr curr_line;
           Buffer.add_char preprocess_buffer '\n';
           annot lexbuf }
  | "//" { Buffer.add_string preprocess_buffer "//";
           annot_comment lexbuf }
  | '@' {
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char preprocess_buffer ' ';
      annot lexbuf }
  | ' '  {
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char preprocess_buffer ' ';
      annot lexbuf }
  (* We're not respecting char count here. Maybe using '$' would do it,
     as cpp is likely to count it as part of an identifier, but this would
     imply that we can not speak about $ ident in annotations.
   *)
  | '\\' { 
        is_newline := CHAR;
        Buffer.add_string preprocess_buffer backslash;
        annot lexbuf }
  | '\'' {
        is_newline := CHAR;
        Buffer.add_char preprocess_buffer '\'';
        char annot lexbuf }
  | '"'  {
        is_newline:=CHAR;
        Buffer.add_char preprocess_buffer '"';
        string annot lexbuf }
  | _ as c { is_newline := CHAR;
             Buffer.add_char preprocess_buffer c;
             annot lexbuf }

and annot_comment = parse
  | '\n' { incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char preprocess_buffer '\n';
           annot lexbuf
         }
  | "*/" {
        Buffer.add_char preprocess_buffer '\n';
        Buffer.add_string preprocess_buffer annot_end;
        Buffer.add_char preprocess_buffer '\n';
        main lexbuf }
  | eof { abort_preprocess "eof in the middle of a comment" }
  | _ as c {
    Buffer.add_char preprocess_buffer c; annot_comment lexbuf }

and char annot = parse

  | '\n' { incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char preprocess_buffer '\n';
           char annot lexbuf
         }
  | '\'' { is_newline:=CHAR;
           Buffer.add_char preprocess_buffer '\'';
           annot lexbuf }
  | "\\'" { is_newline:=CHAR;
            Buffer.add_string preprocess_buffer "\\'";
            char annot lexbuf }
  | "\\\\" { is_newline:=CHAR;
            Buffer.add_string preprocess_buffer "\\\\";
            char annot lexbuf }
  | eof { abort_preprocess "eof while parsing a char literal" }
  | _ as c { is_newline:=CHAR;
             Buffer.add_char preprocess_buffer c;
             char annot lexbuf }

and string annot = parse
  | '\n' { incr curr_line; is_newline:=NEWLINE;
           Buffer.add_char preprocess_buffer '\n'; string annot lexbuf
         }
  | '"' { is_newline:=CHAR;
          Buffer.add_char preprocess_buffer '"'; annot lexbuf }
  | "\\\"" { is_newline:=CHAR;
             Buffer.add_string preprocess_buffer "\\\"";
             string annot lexbuf }
  | eof { abort_preprocess "eof while parsing a string literal" }
  | _ as c { is_newline:=CHAR;
             Buffer.add_char preprocess_buffer c;
             string annot lexbuf }

and comment c =
parse
    "/" {
      Buffer.add_char beg_of_line ' ';
      Buffer.add_char output_buffer  '/';
      if c = '*' then
        main lexbuf
      else
        comment '/' lexbuf
      }
  | '\n' { make_newline (); Buffer.add_char output_buffer '\n';
           comment '\n' lexbuf }
  | eof { abort_preprocess "eof while parsing C comment" }
  | _ as c {
      Buffer.add_char beg_of_line ' ';
      Buffer.add_char output_buffer c;
      comment c lexbuf}

and oneline_annot = parse
    "\n"|eof {
      incr curr_line;
      Buffer.add_char preprocess_buffer '\n';
      Buffer.add_string preprocess_buffer annot_end_nl;
      Buffer.add_char preprocess_buffer '\n';
      main lexbuf }
  | '\\' { Buffer.add_string preprocess_buffer backslash;
           oneline_annot lexbuf }
  | '\'' { Buffer.add_char preprocess_buffer '\'';
           char oneline_annot lexbuf }
  | '"'  { Buffer.add_char preprocess_buffer '"';
           string oneline_annot lexbuf }
  | "//" { Buffer.add_string preprocess_buffer "//";
           oneline_annot_comment lexbuf }
  | _ as c { Buffer.add_char preprocess_buffer c;
             oneline_annot lexbuf }

and oneline_annot_comment = parse
    "\n"|eof {
       incr curr_line;
       Buffer.add_char preprocess_buffer '\n';
       Buffer.add_string preprocess_buffer annot_end_comment;
       Buffer.add_char preprocess_buffer '\n';
       main lexbuf }
  | _ as c { Buffer.add_char preprocess_buffer c;
             oneline_annot_comment lexbuf }

and oneline_comment =
parse
    "\n"|eof
      { make_newline();
        Buffer.add_string output_buffer (lexeme lexbuf);
        main lexbuf}
  | _ as c { Buffer.add_char output_buffer c;
             oneline_comment lexbuf}

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
    main lex;
    preprocess_annots suffix cpp ppfile;
    close_in inchan;
    close_out ppfile;
    ppname
}

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
