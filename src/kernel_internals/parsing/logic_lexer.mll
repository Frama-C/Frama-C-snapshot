(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

  open Logic_parser
  open Lexing
  open Logic_ptree

  type state = Normal | Test

  let state_stack = Stack.create ()

  let () = Stack.push Normal state_stack

  let get_state () = try Stack.top state_stack with Stack.Empty -> Normal

  let pop_state () = try ignore (Stack.pop state_stack) with Stack.Empty -> ()

  exception Error of (int * int) * string

  let loc lexbuf = (lexeme_start lexbuf, lexeme_end lexbuf)

  let lex_error lexbuf s =
    raise (Error (loc lexbuf, "lexical error, " ^ s))

  let find_utf8 =
    let h = Hashtbl.create 97 in
    List.iter (fun (i,t) -> Hashtbl.add h i t)
      [ Utf8_logic.forall, FORALL;
        Utf8_logic.exists, EXISTS;
        Utf8_logic.eq, EQ;
        Utf8_logic.neq, NE;
        Utf8_logic.le, LE;
        Utf8_logic.ge, GE;
        Utf8_logic.implies,IMPLIES;
        Utf8_logic.iff, IFF;
        Utf8_logic.conj, AND;
        Utf8_logic.disj, OR;
        Utf8_logic.neg, NOT;
        Utf8_logic.x_or, HATHAT;
        Utf8_logic.minus, MINUS;
        Utf8_logic.boolean, BOOLEAN;
        Utf8_logic.integer, INTEGER;
        Utf8_logic.real, REAL;
        Utf8_logic.inset, IN;
        Utf8_logic.pi, PI;
      ];

    fun s -> try Hashtbl.find h s
    with Not_found -> IDENTIFIER s

  let identifier, is_acsl_keyword =
    let all_kw = Hashtbl.create 37 in
    let c_kw = Hashtbl.create 37 in
    let type_kw = Hashtbl.create 3 in
    List.iter
      (fun (i,t,flag) ->
         Hashtbl.add all_kw i t;
         if flag then Hashtbl.add c_kw i t
      )
      [
        "allocates", ALLOCATES, false;
        "assert", ASSERT, false;
        "assigns", ASSIGNS, false;
        "assumes", ASSUMES, false;
        "at", EXT_AT, false;(* ACSL extension for external spec file *)
        "axiom", AXIOM, false;
        "axiomatic", AXIOMATIC, false;
        "behavior", BEHAVIOR, false;
        "behaviors", BEHAVIORS, false;
        "_Bool", BOOL, true;
        "breaks", BREAKS, false;
	"case", CASE, true;
        "char", CHAR, true;
        "check", CHECK, false;
        "complete", COMPLETE, false;
        "const", CONST, true;
        "continues", CONTINUES, false;
        "contract", CONTRACT, false;(* ACSL extension for external spec file *)
	"custom", CUSTOM, false; (* ACSL extension for custom annotations *)
        "decreases", DECREASES, false;
        "disjoint", DISJOINT, false;
        "double", DOUBLE, true;
        "else", ELSE, true;
        "ensures", ENSURES, false ;
        "enum", ENUM, true;
        "exits", EXITS, false;
        "frees", FREES, false;
        "function", FUNCTION, false;(* ACSL extension for external spec file *)
        "float", FLOAT, true;
        "for", FOR, true;
        "global",    GLOBAL, false;
        "if", IF, true;
	"impact", IMPACT, false;
	"inductive", INDUCTIVE, false;
	"include", INCLUDE, false;(* ACSL extension for external spec file *)
        "int", INT, true;
        "invariant", INVARIANT, false;
        "label", LABEL, false;
        "lemma", LEMMA, false;
        "let", EXT_LET, false;(* ACSL extension for external spec file *)
        "logic", LOGIC, false;
        "long", LONG, true;
        "loop", LOOP, false;
        "model", MODEL, false;(* ACSL extension for model fields *)
        "module", MODULE, false;(* ACSL extension for external spec file *)
        "pragma", PRAGMA, false;
        "predicate", PREDICATE, false;
        "reads", READS, true; (* treated specifically in the parser to
                                 avoid issue in volatile clause. *)
        "requires", REQUIRES, false;
        "returns", RETURNS, false;
        "short", SHORT, true;
        "signed", SIGNED, true;
        "sizeof", SIZEOF, true;
        "slice", SLICE, false;
        "struct", STRUCT, true;
        "terminates", TERMINATES, false;
        "type", TYPE, false;
        "union", UNION, true;
        "unsigned", UNSIGNED, true;
        "variant", VARIANT, false;
        "void", VOID, true;
        "volatile", VOLATILE, true;
        "writes", WRITES, true; (* treated specifically in the parser to
                                   avoid issue in volatile clause. *)
      ];
    List.iter (fun (x, y) -> Hashtbl.add type_kw x y)
      ["integer", INTEGER; "real", REAL; "boolean", BOOLEAN; ];
    (fun s ->
      try
        Hashtbl.find (if Logic_utils.is_kw_c_mode () then c_kw else all_kw) s
      with Not_found ->
        let res =
          if not (Logic_utils.is_kw_c_mode ()) then begin
            match Logic_env.extension_category s with
            | None -> None
            | Some Cil_types.Ext_contract -> Some (EXT_CONTRACT s)
            | Some Cil_types.Ext_global -> Some (EXT_GLOBAL s)
            | Some Cil_types.Ext_code_annot _ -> Some (EXT_CODE_ANNOT s)
          end
          else None
        in
        match res with
        | None ->
        if Logic_env.typename_status s then TYPENAME s
        else
          (try
             Hashtbl.find type_kw s
           with Not_found ->
             if Logic_utils.is_rt_type_mode () then TYPENAME s
               else IDENTIFIER s)
        | Some lex -> lex
    ),
    (fun s -> Hashtbl.mem all_kw s || Hashtbl.mem type_kw s)

  let bs_identifier =
    let h = Hashtbl.create 97 in
    List.iter (fun (i,t) -> Hashtbl.add h i t)
      [
        "\\allocation", ALLOCATION;
        "\\allocable", ALLOCABLE;
        "\\automatic", AUTOMATIC;
        "\\at", AT;
        "\\base_addr", BASE_ADDR;
        "\\block_length", BLOCK_LENGTH;
        "\\dynamic", DYNAMIC;
        "\\empty", EMPTY;
        "\\exists", EXISTS;
        "\\false", FALSE;
        "\\forall", FORALL;
        "\\freeable", FREEABLE;
        "\\fresh", FRESH;
        "\\from", FROM;
        "\\initialized", INITIALIZED;
        "\\dangling", DANGLING;
        "\\in", IN;
        "\\inter", INTER;
        "\\lambda", LAMBDA;
        "\\let", LET;
        "\\nothing", NOTHING;
        "\\null", NULL;
        "\\offset", OFFSET;
        "\\old", OLD;
        "\\pi", PI;
        "\\register", REGISTER;
        "\\result", RESULT;
        "\\separated", SEPARATED;
        "\\static", STATIC;
        "\\true", TRUE;
        "\\type", BSTYPE;
        "\\typeof", TYPEOF;
        "\\unallocated", UNALLOCATED;
        "\\union", BSUNION;
        "\\valid", VALID;
        "\\valid_read", VALID_READ;
        "\\valid_index", VALID_INDEX;
        "\\valid_range", VALID_RANGE;
        "\\valid_function", VALID_FUNCTION;
        "\\with", WITH;
      ];
    fun lexbuf ->
      let s = lexeme lexbuf in
      try Hashtbl.find h s with Not_found ->         
	if Logic_env.typename_status s then TYPENAME s
        else
	  IDENTIFIER s


  let int_of_digit chr =
    match chr with
        '0'..'9' -> (Char.code chr) - (Char.code '0')
      | 'a'..'f' -> (Char.code chr) - (Char.code 'a') + 10
      | 'A'..'F' -> (Char.code chr) - (Char.code 'A') + 10
      | _ -> assert false

  (* Update lexer buffer. *)
  let update_line_loc lexbuf line =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
	Lexing.pos_lnum = line;
	Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

  let update_newline_loc lexbuf =
    update_line_loc lexbuf (lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum + 1)

  let update_file_loc lexbuf file =
   let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with Lexing.pos_fname = file }

  let accept_c_comments_into_acsl_spec = ref false

}

let space = [' ' '\t' '\012' '\r' '@' ]

let rB = ['0' '1']
let rD = ['0'-'9']
let rO = ['0'-'7']
let rL = ['a'-'z' 'A'-'Z' '_']
let rH = ['a'-'f' 'A'-'F' '0'-'9']
let rE = ['E''e']['+''-']? rD+
let rP = ['P''p']['+''-']? rD+
let rFS	= ('f'|'F'|'l'|'L'|'d'|'D')
let rIS = ('u'|'U'|'l'|'L')*
let comment_line = "//" [^'\n']*

(* Do not forget to update also the corresponding chr rule if you add
   a supported escape sequence here. *)
let escape = '\\'
   ('\'' | '"' | '?' | '\\' | 'a' | 'b' | 'f' | 'n' | 'r'
   | 't' | 'v')

let hex_escape = '\\' ['x' 'X'] rH+
let oct_escape = '\\' rO rO? rO?

let utf8_char = ['\128'-'\254']+

rule token = parse
  | space+ { token lexbuf }
  | '\n' { update_newline_loc lexbuf; token lexbuf }
  | comment_line '\n' { update_newline_loc lexbuf; token lexbuf }
  | comment_line eof { token lexbuf }
  | "*/" { lex_error lexbuf "unexpected block-comment closing" }
  | "/*" { if !accept_c_comments_into_acsl_spec
           then comment lexbuf
           else lex_error lexbuf "unexpected block-comment opening"
         }

  | '\\' rL (rL | rD)* { bs_identifier lexbuf }
  | rL (rL | rD)*       { let s = lexeme lexbuf in identifier s }

  | '0'['x''X'] rH+ rIS?    { CONSTANT (IntConstant (lexeme lexbuf)) }
  | '0'['b''B'] rB+ rIS?    { CONSTANT (IntConstant (lexeme lexbuf)) }
  | '0' rD+ rIS?            { CONSTANT (IntConstant (lexeme lexbuf)) }
  | rD+                     { CONSTANT10 (lexeme lexbuf) }
  | rD+ rIS                 { CONSTANT (IntConstant (lexeme lexbuf)) }
  | ('L'? "'" as prelude) (([^ '\\' '\'' '\n']|("\\"[^ '\n']))+ as content) "'"
      {
        let b = Buffer.create 5 in
        Buffer.add_string b prelude;
        let lbf = Lexing.from_string content in
        CONSTANT (IntConstant (chr b lbf ^ "'"))
      }
(* floating-point literals, both decimal and hexadecimal *)
  | rD+ rE rFS?
  | rD* "." rD+ (rE)? rFS?
  | rD+ "." rD* (rE)? rFS?
  | '0'['x''X'] rH+ '.' rH* rP rFS?
  | '0'['x''X'] rH* '.' rH+ rP rFS?
  | '0'['x''X'] rH+ rP rFS?
      { CONSTANT (FloatConstant (lexeme lexbuf)) }

 (* hack to lex 0..3 as 0 .. 3 and not as 0. .3 *)
  | (rD+ as n) ".."         { lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 2;
                              CONSTANT (IntConstant n) }

  | 'L'? '"' as prelude (([^ '\\' '"' '\n']|("\\"[^ '\n']))* as content) '"'
      { STRING_LITERAL (prelude.[0] = 'L',content) }
  | '#'                     { hash lexbuf }
  | "==>"                   { IMPLIES }
  | "<==>"                  { IFF }
  | "-->"                   { BIMPLIES }
  | "<-->"                  { BIFF }
  | "&&"                    { AND }
  | "||"                    { OR }
  | "!"                     { NOT }
  | "$"                     { DOLLAR }
  | ","                     { COMMA }
  | "->"                    { ARROW }
  | "?"                     { Stack.push Test state_stack; QUESTION }
  | ";"                     { SEMICOLON }
  | ":"                     { match get_state() with
                                  Normal  -> COLON
                                | Test -> pop_state(); COLON2
                            }
  | "::"                    { COLONCOLON }
  | "."                     { DOT }
  | ".."                    { DOTDOT }
  | "..."                   { DOTDOTDOT }
  | "-"                     { MINUS }
  | "+"                     { PLUS }
  | "*"                     { STAR }
  | "*^"                    { STARHAT }
  | "&"                     { AMP }
  | "^^"                    { HATHAT }
  | "^"                     { HAT }
  | "|"                     { PIPE }
  | "~"                     { TILDE }
  | "/"                     { SLASH }
  | "%"                     { PERCENT }
  | "<"                     { LT }
  | ">"                     { GT }
  | "<="                    { LE }
  | ">="                    { GE }
  | "=="                    { EQ }
  | "="                     { EQUAL }
  | "!="                    { NE }
  | "("                     { Stack.push Normal state_stack; LPAR }
  | ")"                     { pop_state(); RPAR }
  | "{"                     { Stack.push Normal state_stack; LBRACE }
  | "}"                     { pop_state(); RBRACE }
  | "["                     { Stack.push Normal state_stack; LSQUARE }
  | "]"                     { pop_state(); RSQUARE }
  | "[|"                    { Stack.push Normal state_stack; LSQUAREPIPE }
  | "|]"                    { pop_state(); RSQUAREPIPE }
  | "<<"                    { LTLT }
  | ">>"                    { GTGT }
  | utf8_char as c          { find_utf8 c }
  | eof                     { EOF }
  | _   { lex_error lexbuf ("illegal character " ^ lexeme lexbuf) }

and chr buffer = parse
  | hex_escape
      { let s = lexeme lexbuf in
        let real_s = String.sub s 2 (String.length s - 2) in
        let rec add_one_char s =
          let l = String.length s in
          if l = 0 then ()
          else
          let h = int_of_digit s.[0] in
          let c,s =
            if l = 1 then (h,"")
            else
              (16*h + int_of_digit s.[1],
               String.sub s 2 (String.length s - 2))
          in
          Buffer.add_char buffer (Char.chr c); add_one_char s
        in add_one_char real_s; chr buffer lexbuf
      }
  | oct_escape
      { let s = lexeme lexbuf in
        let real_s = String.sub s 1 (String.length s - 1) in
        let rec value i s =
          if s = "" then i
          else value (8*i+int_of_digit s.[0])
            (String.sub s 1 (String.length s -1))
        in let c = value 0 real_s in
        Buffer.add_char buffer (Char.chr c); chr buffer lexbuf
      }
  | escape
      { Buffer.add_char buffer
          (match (lexeme lexbuf).[1] with
               'a' -> '\007'
             | 'b' -> '\b'
             | 'f' -> '\012'
             | 'n' -> '\n'
             | 'r' -> '\r'
             | 't' -> '\t'
             | 'v' -> '\011' (* no '\v' in OCaml 😞 *)
             | '\'' -> '\''
             | '"' -> '"'
             | '?' -> '?'
             | '\\' -> '\\'
             | _ -> (* escape regex does not allow anything else *) assert false
          ); chr buffer lexbuf}
  | eof { Buffer.contents buffer }
  | _  { Buffer.add_string buffer (lexeme lexbuf); chr buffer lexbuf }

and hash = parse
  '\n'		{ update_newline_loc lexbuf; token lexbuf}
| [' ''\t']		{ hash lexbuf}
| rD+	        { (* We are seeing a line number. This is the number for the
                   * next line *)
                 let s = Lexing.lexeme lexbuf in
                 let lineno =
                   try
                     int_of_string s
                   with Failure _ ->
                     (* the int is too big. *)
                     Kernel.warning
                       ~source:(Cil_datatype.Position.of_lexing_pos lexbuf.lex_start_p)
                       "Bad line number in preprocessed file: %s"  s;
                     (-1)
                 in
                 update_line_loc lexbuf (lineno - 1);
                  (* A file name may follow *)
		  file lexbuf }
| "line"        { hash lexbuf } (* MSVC line number info *)
| _	        { endline lexbuf}

and file =  parse
        '\n'		        { update_newline_loc lexbuf; token lexbuf}
|	[' ''\t''\r']			{ file lexbuf}
|	'"' [^ '\012' '\t' '"']* '"'
            {
              let n = Lexing.lexeme lexbuf in
              let n1 = String.sub n 1
                ((String.length n) - 2) in
              update_file_loc lexbuf n1;
	      endline lexbuf
            }

|	_			{ endline lexbuf}

and endline = parse
        '\n' 			{ update_newline_loc lexbuf; token lexbuf}
|   eof                         { EOF }
|	_			{ endline lexbuf}

and comment = parse
    '\n' { update_newline_loc lexbuf; comment lexbuf}
  | "*/" { token lexbuf}
  | eof  { lex_error lexbuf "non-terminating block-comment" }
  | _    { comment lexbuf}

{
  let set_initial_location dest_lexbuf src_loc =
    Lexing.(
      dest_lexbuf.lex_curr_p <- 
        { src_loc with
          pos_bol = src_loc.pos_bol - src_loc.pos_cnum;
          pos_cnum = 0; };
    )

  let parse_from_location f (loc, s : Filepath.position * string) =
    let finally _ = Logic_utils.exit_kw_c_mode () in
    let output = Kernel.logwith finally ~wkey:Kernel.wkey_annot_error
    in
    let lb = from_string s in
    set_initial_location lb (Cil_datatype.Position.to_lexing_pos loc);
    try
      let res = f token lb in
      Some (Cil_datatype.Position.of_lexing_pos lb.Lexing.lex_curr_p, res)
    with
      | Failure s -> (* raised by the lexer itself, through [f] *)
          output ~source:(Cil_datatype.Position.of_lexing_pos lb.lex_curr_p) "lexing error: %s" s; None
      | Parsing.Parse_error ->
        output ~source:(Cil_datatype.Position.of_lexing_pos lb.lex_curr_p) "unexpected token '%s'" (Lexing.lexeme lb);
        None
      | Error (_, m) -> output ~source:(Cil_datatype.Position.of_lexing_pos lb.lex_curr_p) "%s" m; None
      | Logic_utils.Not_well_formed (loc, m) ->
        output ~source:(fst loc) "%s" m;
        None
      | Log.FeatureRequest(_,msg) ->
        output ~source:(Cil_datatype.Position.of_lexing_pos lb.lex_curr_p) "unimplemented ACSL feature: %s" msg; None
      | exn ->
        Kernel.fatal ~source:(Cil_datatype.Position.of_lexing_pos lb.lex_curr_p) "Unknown error (%s)"
          (Printexc.to_string exn)

  let lexpr = parse_from_location Logic_parser.lexpr_eof

  let annot = parse_from_location Logic_parser.annot

  let spec = parse_from_location Logic_parser.spec

  let ext_spec lexbuf = try
      accept_c_comments_into_acsl_spec := true ;
      let r = Logic_parser.ext_spec token lexbuf in
      accept_c_comments_into_acsl_spec := false ;
      r
    with exn ->
      accept_c_comments_into_acsl_spec := false ;
      raise exn

  type 'a parse = Filepath.position * string -> (Filepath.position * 'a) option

  let chr lexbuf =
    let buf = Buffer.create 16 in
    chr buf lexbuf
}

(*
Local Variables:
compile-command: "make -C ../../.. byte"
End:
*)
