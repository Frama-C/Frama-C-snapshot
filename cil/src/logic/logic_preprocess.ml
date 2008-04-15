# 25 "cil/src/logic/logic_preprocess.mll"
 
  open Lexing
  type state = NORMAL | SLASH | INCOMMENT
  type end_of_buffer = NEWLINE | SPACE | CHAR
  let debug = false
  let buf = Buffer.create 1024
  let macros = Buffer.create 1024
  let beg_of_line = Buffer.create 8
  let blacklisted_macros = [ "__STDC__"; "__STDC_HOSTED__"]
  let is_newline = ref CHAR
  let curr_file = ref ""
  let curr_line = ref 1
  let is_ghost = ref false
  let begin_annot_line = ref 1
  let preprocess_annot cpp outfile =
    let (ppname, ppfile) = Filename.open_temp_file "ppannot" ".c" in
    Buffer.output_buffer ppfile macros;
    (* NB: the three extra spaces replace the begining of the annotation
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
    let cppname = Filename.temp_file "cppannot" ".c" in
    let res = Sys.command (cpp ppname cppname) in
    if not debug then (try Sys.remove ppname with Sys_error _ -> ());
    output_string outfile "/*@";
    if !is_ghost then output_string outfile " ghost\n";
    if res <> 0 then begin
    Printf.eprintf
      "Warning: could not preprocess logical annotation. Keeping as is%!";
    Buffer.output_buffer outfile buf;
    if not debug then (try Sys.remove cppname with Sys_error _ -> ())
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
              | _ -> ());
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
    Printf.fprintf outfile "*/\n# %d %s\n" !curr_line !curr_file;
    Buffer.clear buf

  let make_newline () =
    incr curr_line;
    Buffer.clear beg_of_line

# 91 "cil/src/logic/logic_preprocess.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\249\255\250\255\251\255\001\000\002\000\012\000\000\000\
    \040\000\000\000\001\000\001\000\001\000\003\000\089\000\255\255\
    \002\000\211\000\002\000\006\000\076\000\046\000\004\000\047\000\
    \013\000\254\255\001\000\014\000\253\255\252\255\204\000\023\000\
    \005\000\254\255\000\000\002\000\000\000\000\000\239\000\010\000\
    \005\000\006\000\003\000\003\000\208\000\024\000\255\255\170\000\
    \018\000\017\000";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\006\000\006\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\004\000\
    \004\000\255\255\000\000\255\255\255\255\255\255\255\255\003\000\
    \000\000\255\255\255\255\255\255\255\255\004\000\255\255\255\255\
    \255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\016\000\000\000\
    \016\000\016\000\255\255\255\255\255\255\023\000\255\255\023\000\
    \024\000\000\000\029\000\028\000\000\000\000\000\003\000\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\029\000\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\000\000\028\000\
    \028\000\033\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\034\000\002\000\008\000\015\000\022\000\040\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\022\000\000\000\025\000\
    \000\000\000\000\000\000\046\000\046\000\000\000\000\000\000\000\
    \034\000\000\000\008\000\005\000\022\000\040\000\021\000\000\000\
    \000\000\000\000\000\000\027\000\022\000\000\000\021\000\004\000\
    \026\000\008\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\029\000\046\000\
    \008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\024\000\033\000\000\000\000\000\020\000\000\000\000\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\014\000\015\000\000\000\010\000\009\000\011\000\
    \014\000\018\000\012\000\020\000\020\000\035\000\007\000\013\000\
    \019\000\036\000\041\000\037\000\028\000\042\000\043\000\028\000\
    \000\000\014\000\000\000\000\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\000\000\000\000\
    \000\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\000\000\007\000\000\000\000\000\000\000\
    \000\000\000\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\033\000\000\000\000\000\000\000\
    \017\000\000\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\000\000\034\000\033\000\000\000\
    \000\000\046\000\033\000\000\000\000\000\015\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\034\000\000\000\000\000\000\000\
    \029\000\000\000\000\000\000\000\000\000\000\000\031\000\000\000\
    \040\000\033\000\045\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\255\255\255\255\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\255\255\255\255\040\000\
    \028\000\046\000\046\000\000\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\255\255\255\255\
    \000\000\000\000\017\000\032\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\039\000\000\000\
    \000\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\000\000\000\000\000\000\
    \255\255\000\000\000\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    ";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\034\000\000\000\005\000\016\000\022\000\040\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\006\000\255\255\024\000\
    \255\255\255\255\255\255\049\000\048\000\255\255\255\255\255\255\
    \034\000\255\255\005\000\000\000\022\000\040\000\022\000\255\255\
    \255\255\255\255\255\255\004\000\006\000\255\255\006\000\000\000\
    \004\000\008\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\031\000\045\000\
    \008\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \021\000\023\000\048\000\255\255\255\255\020\000\255\255\255\255\
    \008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
    \008\000\008\000\014\000\014\000\255\255\009\000\005\000\010\000\
    \013\000\007\000\011\000\019\000\020\000\032\000\005\000\012\000\
    \018\000\035\000\039\000\036\000\037\000\041\000\042\000\043\000\
    \255\255\014\000\255\255\255\255\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\255\255\255\255\
    \255\255\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\255\255\008\000\255\255\255\255\255\255\
    \255\255\255\255\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\047\000\255\255\255\255\255\255\
    \014\000\255\255\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\255\255\030\000\030\000\255\255\
    \255\255\047\000\044\000\255\255\255\255\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\030\000\255\255\255\255\255\255\
    \044\000\255\255\255\255\255\255\255\255\255\255\030\000\255\255\
    \038\000\038\000\044\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\026\000\016\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\024\000\027\000\038\000\
    \044\000\049\000\048\000\255\255\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\021\000\023\000\
    \255\255\255\255\017\000\030\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\038\000\255\255\
    \255\255\014\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\047\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\030\000\255\255\255\255\255\255\
    \044\000\255\255\255\255\017\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\038\000\
    ";
  Lexing.lex_base_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
    \036\000\000\000\000\000\000\000\000\000\000\000\062\000\030\000\
    \000\000\137\000\000\000\001\000\251\000\000\000\001\000\000\000\
    \000\000\038\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_backtrk_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_default_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000";
  Lexing.lex_trans_code = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\001\000\009\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\009\000\035\000\001\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\009\000\000\000\000\000\001\000\000\000\000\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\001\000\000\000\000\000\022\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\022\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\017\000\001\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\000\000\000\000\000\000\000\000\027\000\000\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\000\000\000\000\000\000\000\000\
    \027\000\000\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\027\000\027\000\027\000\027\000\
    \027\000\027\000\027\000\027\000\001\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000";
  Lexing.lex_check_code = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\005\000\022\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\006\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \005\000\022\000\023\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\006\000\255\255\255\255\008\000\255\255\255\255\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\008\000\255\255\255\255\014\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\008\000\008\000\008\000\008\000\
    \008\000\008\000\008\000\008\000\008\000\008\000\014\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\013\000\019\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\255\255\255\255\255\255\255\255\014\000\255\255\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\255\255\255\255\255\255\255\255\
    \017\000\255\255\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\020\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\020\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\020\000\020\000\020\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255";
  Lexing.lex_code = 
   "\255\004\255\255\006\255\005\255\255\006\255\255\005\255\006\255\
    \255\008\255\007\255\255\007\255\008\255\255\008\255\255\000\007\
    \001\008\255\009\255\255\000\004\001\005\002\006\003\009\255";
}

let rec main cpp outfile lexbuf =
  lexbuf.Lexing.lex_mem <- Array.create 10 (-1) ;   __ocaml_lex_main_rec cpp outfile lexbuf 0
and __ocaml_lex_main_rec cpp outfile lexbuf __ocaml_lex_state =
  match Lexing.new_engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 115 "cil/src/logic/logic_preprocess.mll"
                                                           m
# 413 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1) in
# 117 "cil/src/logic/logic_preprocess.mll"
      (
        if not (List.mem m blacklisted_macros) then
          Buffer.add_string macros (lexeme lexbuf);
	output_char outfile '\n';
        make_newline ();
        main cpp outfile lexbuf
      )
# 423 "cil/src/logic/logic_preprocess.ml"

  | 1 ->
let
# 124 "cil/src/logic/logic_preprocess.mll"
                                                       line
# 429 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(0) lexbuf.Lexing.lex_mem.(1)
and
# 125 "cil/src/logic/logic_preprocess.mll"
                                     file
# 434 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_mem.(2) lexbuf.Lexing.lex_mem.(3) in
# 126 "cil/src/logic/logic_preprocess.mll"
    ( (try
        curr_line := (int_of_string line) -1
       with Failure "int_of_string" -> curr_line:= -1);
      if file <> "" then curr_file := file;
      output_string outfile (lexeme lexbuf);
      make_newline();
      main cpp outfile lexbuf
    )
# 445 "cil/src/logic/logic_preprocess.ml"

  | 2 ->
let
# 134 "cil/src/logic/logic_preprocess.mll"
                c
# 451 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 134 "cil/src/logic/logic_preprocess.mll"
                   (
      if c = !Clexer.annot_char then begin
        is_newline:=CHAR;
        begin_annot_line := ! curr_line;
        Buffer.clear buf;
        maybe_ghost cpp outfile lexbuf
      end else begin
        output_string outfile (lexeme lexbuf);
        if c = '\n' then make_newline();
        Buffer.add_string beg_of_line "   ";
        comment cpp outfile c lexbuf;
      end)
# 466 "cil/src/logic/logic_preprocess.ml"

  | 3 ->
let
# 146 "cil/src/logic/logic_preprocess.mll"
                c
# 472 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
# 146 "cil/src/logic/logic_preprocess.mll"
                   (
      if c = !Clexer.annot_char then begin
        Buffer.clear buf;
        begin_annot_line := !curr_line;
        is_newline:=CHAR;
        maybe_oneline_ghost cpp outfile lexbuf
      end
      else if c = '\n' then begin
        make_newline ();
        output_string outfile (lexeme lexbuf);
        main cpp outfile lexbuf
      end
      else begin
        output_string outfile (lexeme lexbuf);
        oneline_comment cpp outfile lexbuf;
      end)
# 491 "cil/src/logic/logic_preprocess.ml"

  | 4 ->
# 162 "cil/src/logic/logic_preprocess.mll"
         ( flush outfile )
# 496 "cil/src/logic/logic_preprocess.ml"

  | 5 ->
# 163 "cil/src/logic/logic_preprocess.mll"
         (
      make_newline ();
      output_char outfile '\n'; main cpp outfile lexbuf )
# 503 "cil/src/logic/logic_preprocess.ml"

  | 6 ->
let
# 166 "cil/src/logic/logic_preprocess.mll"
         c
# 509 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 166 "cil/src/logic/logic_preprocess.mll"
           (
      Buffer.add_char beg_of_line ' ';
      output_char outfile c; main cpp outfile lexbuf )
# 515 "cil/src/logic/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_main_rec cpp outfile lexbuf __ocaml_lex_state

and maybe_ghost cpp outfile lexbuf =
    __ocaml_lex_maybe_ghost_rec cpp outfile lexbuf 30
and __ocaml_lex_maybe_ghost_rec cpp outfile lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 171 "cil/src/logic/logic_preprocess.mll"
                 space
# 527 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 171 "cil/src/logic/logic_preprocess.mll"
                      (
     Buffer.add_string buf space;
     maybe_ghost cpp outfile lexbuf )
# 533 "cil/src/logic/logic_preprocess.ml"

  | 1 ->
# 174 "cil/src/logic/logic_preprocess.mll"
         (
      is_newline := NEWLINE;
      incr curr_line;
      Buffer.add_char buf '\n';
      maybe_ghost cpp outfile lexbuf
    )
# 543 "cil/src/logic/logic_preprocess.ml"

  | 2 ->
# 181 "cil/src/logic/logic_preprocess.mll"
      ( is_ghost := true;
        Buffer.add_string buf "     ";
        annot cpp outfile lexbuf
      )
# 551 "cil/src/logic/logic_preprocess.ml"

  | 3 ->
# 186 "cil/src/logic/logic_preprocess.mll"
         ( main cpp outfile lexbuf )
# 556 "cil/src/logic/logic_preprocess.ml"

  | 4 ->
let
# 187 "cil/src/logic/logic_preprocess.mll"
         c
# 562 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 187 "cil/src/logic/logic_preprocess.mll"
           ( Buffer.add_char buf c; is_ghost:=false; annot cpp outfile lexbuf)
# 566 "cil/src/logic/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_maybe_ghost_rec cpp outfile lexbuf __ocaml_lex_state

and maybe_oneline_ghost cpp outfile lexbuf =
    __ocaml_lex_maybe_oneline_ghost_rec cpp outfile lexbuf 38
and __ocaml_lex_maybe_oneline_ghost_rec cpp outfile lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 189 "cil/src/logic/logic_preprocess.mll"
                 space
# 578 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 189 "cil/src/logic/logic_preprocess.mll"
                      (
     Buffer.add_string buf space;
     maybe_oneline_ghost cpp outfile lexbuf )
# 584 "cil/src/logic/logic_preprocess.ml"

  | 1 ->
# 192 "cil/src/logic/logic_preprocess.mll"
         (
      incr curr_line;
      main cpp outfile lexbuf
    )
# 592 "cil/src/logic/logic_preprocess.ml"

  | 2 ->
# 197 "cil/src/logic/logic_preprocess.mll"
      ( is_ghost := true;
        Buffer.add_string buf "     ";
        oneline_annot cpp outfile lexbuf
      )
# 600 "cil/src/logic/logic_preprocess.ml"

  | 3 ->
let
# 201 "cil/src/logic/logic_preprocess.mll"
         c
# 606 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 202 "cil/src/logic/logic_preprocess.mll"
      (
        Buffer.add_char buf c;
        is_ghost:=false;
        oneline_annot cpp outfile lexbuf
      )
# 614 "cil/src/logic/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_maybe_oneline_ghost_rec cpp outfile lexbuf __ocaml_lex_state

and annot cpp outfile lexbuf =
    __ocaml_lex_annot_rec cpp outfile lexbuf 44
and __ocaml_lex_annot_rec cpp outfile lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 208 "cil/src/logic/logic_preprocess.mll"
          ( preprocess_annot cpp outfile; main cpp outfile lexbuf )
# 625 "cil/src/logic/logic_preprocess.ml"

  | 1 ->
# 209 "cil/src/logic/logic_preprocess.mll"
         ( is_newline := NEWLINE;
           incr curr_line;
           Buffer.add_char buf '\n';
           annot cpp outfile lexbuf )
# 633 "cil/src/logic/logic_preprocess.ml"

  | 2 ->
# 213 "cil/src/logic/logic_preprocess.mll"
        (
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char buf ' ';
      annot cpp outfile lexbuf )
# 641 "cil/src/logic/logic_preprocess.ml"

  | 3 ->
# 217 "cil/src/logic/logic_preprocess.mll"
         (
      if !is_newline = NEWLINE then is_newline:=SPACE;
      Buffer.add_char buf ' '; annot cpp outfile lexbuf )
# 648 "cil/src/logic/logic_preprocess.ml"

  | 4 ->
let
# 220 "cil/src/logic/logic_preprocess.mll"
         c
# 654 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 220 "cil/src/logic/logic_preprocess.mll"
           ( is_newline := CHAR;
             Buffer.add_char buf c; annot cpp outfile lexbuf )
# 659 "cil/src/logic/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_annot_rec cpp outfile lexbuf __ocaml_lex_state

and comment cpp outfile c lexbuf =
    __ocaml_lex_comment_rec cpp outfile c lexbuf 47
and __ocaml_lex_comment_rec cpp outfile c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 225 "cil/src/logic/logic_preprocess.mll"
        (
      Buffer.add_char beg_of_line ' ';
      output_string outfile (lexeme lexbuf);
      if c = '*' then
        main cpp outfile lexbuf
      else
        comment cpp outfile '/' lexbuf
      )
# 677 "cil/src/logic/logic_preprocess.ml"

  | 1 ->
# 233 "cil/src/logic/logic_preprocess.mll"
         ( make_newline (); output_char outfile '\n';
           comment cpp outfile '\n' lexbuf )
# 683 "cil/src/logic/logic_preprocess.ml"

  | 2 ->
let
# 235 "cil/src/logic/logic_preprocess.mll"
         c
# 689 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 235 "cil/src/logic/logic_preprocess.mll"
           (
      Buffer.add_char beg_of_line ' ';
      output_char outfile c;
      comment cpp outfile c lexbuf)
# 696 "cil/src/logic/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec cpp outfile c lexbuf __ocaml_lex_state

and oneline_annot cpp outfile lexbuf =
    __ocaml_lex_oneline_annot_rec cpp outfile lexbuf 48
and __ocaml_lex_oneline_annot_rec cpp outfile lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 241 "cil/src/logic/logic_preprocess.mll"
             (
      incr curr_line;
      preprocess_annot cpp outfile;
      main cpp outfile lexbuf )
# 710 "cil/src/logic/logic_preprocess.ml"

  | 1 ->
# 245 "cil/src/logic/logic_preprocess.mll"
         ( Buffer.add_char buf ' '; oneline_annot cpp outfile lexbuf )
# 715 "cil/src/logic/logic_preprocess.ml"

  | 2 ->
let
# 246 "cil/src/logic/logic_preprocess.mll"
         c
# 721 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 246 "cil/src/logic/logic_preprocess.mll"
           ( Buffer.add_char buf c; oneline_annot cpp outfile lexbuf )
# 725 "cil/src/logic/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_oneline_annot_rec cpp outfile lexbuf __ocaml_lex_state

and oneline_comment cpp outfile lexbuf =
    __ocaml_lex_oneline_comment_rec cpp outfile lexbuf 49
and __ocaml_lex_oneline_comment_rec cpp outfile lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 251 "cil/src/logic/logic_preprocess.mll"
      ( make_newline();
        output_string outfile (lexeme lexbuf);
        main cpp outfile lexbuf)
# 738 "cil/src/logic/logic_preprocess.ml"

  | 1 ->
let
# 254 "cil/src/logic/logic_preprocess.mll"
         c
# 744 "cil/src/logic/logic_preprocess.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 254 "cil/src/logic/logic_preprocess.mll"
           ( output_char outfile c; oneline_comment cpp outfile lexbuf)
# 748 "cil/src/logic/logic_preprocess.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_oneline_comment_rec cpp outfile lexbuf __ocaml_lex_state

;;

# 256 "cil/src/logic/logic_preprocess.mll"
 
  let file cpp filename =
    let inchan = open_in_bin filename in
    let lex = Lexing.from_channel inchan in
    let (ppname, ppfile) = Filename.open_temp_file
      (Filename.basename filename) ".pp"
    in
    main cpp ppfile lex;
    close_in inchan;
    close_out ppfile;
    ppname

# 767 "cil/src/logic/logic_preprocess.ml"
