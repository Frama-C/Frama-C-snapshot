(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
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

(* -------------------------------------------------------------------------- *)
(* --- External Driver                                                    --- *)
(* -------------------------------------------------------------------------- *)

{

  open Qed.Logic
  open Lexing
  open Cil_types
  open LogicBuiltins

  type bal = [ `Default | `Left | `Right | `Nary ]

  type token =
    | EOF
    | KEY of string
    | BOOLEAN
    | INTEGER
    | REAL
    | INT of ikind
    | FLT of fkind
    | KIND of kind
    | ID of string
    | LINK of string
    | RECLINK of (string * (string * bal)) list
    | FIELD of string * string

  let keywords = [ 
    "library" , KEY "library" ;
    "type" , KEY "type" ;
    "ctor" , KEY "ctor" ;
    "logic" , KEY "logic" ;
    "predicate" , KEY "predicate" ;
    "boolean" , BOOLEAN ;
    "integer" , INTEGER ;
    "real" , REAL ;
    "char" , INT IChar ;
    "short" , INT IShort ;
    "int" , INT IInt ;
    "unsigned" , INT IUInt ;
    "float" , FLT FFloat ;
    "float32" , KIND (F Ctypes.Float32) ;
    "float64" , KIND (F Ctypes.Float64) ;
    "double" , FLT FDouble ;
  ]

  let ident x = try List.assoc x keywords with Not_found -> ID x

  let newline lexbuf =
    lexbuf.lex_curr_p <-
      { lexbuf.lex_curr_p with pos_lnum = succ lexbuf.lex_curr_p.pos_lnum }

  let rec conv_bal default (name,bal) =
    match bal with
    | `Default -> conv_bal default (name,default)
    | `Nary  -> Qed.Engine.F_call name
    | `Left  -> Qed.Engine.F_left name
    | `Right -> Qed.Engine.F_right name

}

let blank = [ ' ' '\t' ]
let ident = '\\'? [ 'a'-'z' 'A'-'Z' '_' '0'-'9' ]+

rule tok = parse
    eof { EOF }
  | '\n' { newline lexbuf ; tok lexbuf }
  | blank+ { tok lexbuf }
  | "//" [^ '\n']* '\n' { newline lexbuf ; tok lexbuf }
  | "/*" { comment lexbuf }
  | ident as a { ident a }
  | '"' { LINK (string_val (Buffer.create 10) lexbuf) }
  | '{' { RECLINK(reclink [] lexbuf) }
  | (ident as group) '.' (ident as var) { FIELD(group,var) }
  | _ | ":=" | "+=" { KEY (Lexing.lexeme lexbuf) }

and comment = parse
  | eof { failwith "Unterminated comment" }
  | "*/" { tok lexbuf }
  | '\n' { newline lexbuf ; comment lexbuf }
  | _ { comment lexbuf }

and value = parse
    | '\n' { newline lexbuf ; value lexbuf }
    | blank+ { value lexbuf }
    | ident  as a { a }
    | '"' { string_val (Buffer.create 10) lexbuf }
    | _ { failwith "Ident or String expected" }

and string_val buf = parse
  | '"' { Buffer.contents buf;}
  | [^ '\\' '"'] as c
      { Buffer.add_char buf c;
        string_val buf lexbuf }
  | '\\' (['\\' '"' 'n' 'r' 't'] as c)
      { Buffer.add_char buf
          (match c with 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t' | _ -> c);
        string_val buf lexbuf }
  | '\\' '\n'
      { string_val buf lexbuf }
  | '\\' (_ as c)
      { Buffer.add_char buf '\\';
        Buffer.add_char buf c;
        string_val buf lexbuf }
  | eof
      { failwith "Unterminated string" }

and recstring acc = parse
  | ';' | blank+ { recstring acc lexbuf }
  | '\n' { newline lexbuf ; recstring acc lexbuf }
  | '}'  { acc }
  | ident as field { recstring_bis acc field lexbuf }
  | _ { failwith "Identifier or '}' expected" }
and recstring_bis acc field = parse
  | blank+ { recstring_bis acc field lexbuf }
  | '\n' { newline lexbuf ; recstring_bis acc field lexbuf }
  | '='  { recstring_ter acc field lexbuf }
  | _    { failwith "'=' expected" }
and recstring_ter acc field = parse
  | blank+ { recstring_ter acc field lexbuf }
  | '\n'   { newline lexbuf ; recstring_ter acc field lexbuf }
  | ident as name { recstring ((field,name)::acc) lexbuf }
  | '"'
      { let name = string_val (Buffer.create 10) lexbuf in
        recstring ((field,name)::acc) lexbuf
      }
  | _ { failwith "Identifier or String expected" }

and recorstring = parse
  | '\n'   { newline lexbuf ; recorstring lexbuf }
  | blank+ { recorstring lexbuf }
  | '"'    { `String (string_val (Buffer.create 10) lexbuf) }
  | '{'    { `RecString (recstring [] lexbuf) }
  | _ as c { failwith (Printf.sprintf "found '%c' instead of \" or {" c) }

and reclink acc = parse
  | ';' | blank+ { reclink acc lexbuf }
  | '\n' { newline lexbuf ; reclink acc lexbuf }
  | '}'  { acc }
  | ident as field { reclink_bis acc field lexbuf }
  | _ { failwith "Identifier or '}' expected" }
and reclink_bis acc field = parse
  | blank+ { reclink_bis acc field lexbuf }
  | '\n'   { newline lexbuf ; reclink_bis acc field lexbuf }
  | '='    { reclink_ter acc field lexbuf }
  | _      { failwith "'=' expected" }
and reclink_ter acc field = parse
  | blank+ { reclink_ter acc field lexbuf }
  | '\n'   { newline lexbuf ; reclink_ter acc field lexbuf }
  | ident as name 
      { let link  = name,(bal lexbuf) in
        reclink ((field,link)::acc) lexbuf
      }
  | '"'
      { let name = string_val (Buffer.create 10) lexbuf in
        let link = name,(bal lexbuf) in
        reclink ((field,link)::acc) lexbuf
      }
  | _ { failwith "Identifier or String expected" }

and bal = parse
  | '\n' { newline lexbuf ; bal lexbuf }
  | blank+ { bal lexbuf }
  | ('(' "right" ')') { `Right }
  | ('(' "nary"  ')') { `Nary }
  | ('(' "left"  ')')? as c { if c = "" then `Default else `Left }

{

  let pretty fmt = function
    | EOF -> Format.pp_print_string fmt "<eof>"
    | KEY a | ID a -> Format.fprintf fmt "'%s'" a
    | LINK s -> Format.fprintf fmt "\"%s\"" s
    | BOOLEAN | INTEGER | REAL | INT _ | FLT _  | KIND _ ->
	Format.pp_print_string fmt "<type>"
    | FIELD(group,name) -> Format.fprintf fmt "%s.%s" group name
    | RECLINK _ -> Format.pp_print_string fmt "<reclink>"

  type input = {
    lexbuf : Lexing.lexbuf ;
    mutable current : token ;
  }

  let skip input =
    if input.current <> EOF then input.current <- tok input.lexbuf

  let token input = input.current

  let value input =
    if input.current = EOF then failwith "Value expected"
    else
      let v = value input.lexbuf in
      skip input; v

  let key input a = match token input with
    | KEY b when a=b -> skip input ; true
    | _ -> false

  let skipkey input a = match token input with
    | KEY b when a=b -> skip input
    | _ -> failwith (Printf.sprintf "Missing '%s'" a)

  let noskipkey input a = match token input with
    | KEY b when a=b -> ()
    | _ -> failwith (Printf.sprintf "Missing '%s'" a)


  let ident input = match token input with
    | ID x | LINK x -> skip input ; x
    | _ -> failwith "missing identifier"

  let kind input = 
    let kd = match token input with
      | INTEGER -> Z
      | REAL -> R
      | BOOLEAN -> A
      | INT i -> I (Ctypes.c_int i)
      | FLT f -> F (Ctypes.c_float f)
      | KIND x -> x
      | ID _ -> A
      | _ -> failwith "<type> expected"
    in skip input ; kd

  let parameter input =
    let k = kind input in
    match token input with
      | ID _ -> skip input ; k
      | _ -> k

  let rec parameters input =
    if key input ")" then [] else
      let p = parameter input in
      if key input "," then p :: parameters input else 
	if key input ")" then [p] else
	  failwith "Missing ',' or ')'"

  let signature input =
    if key input "(" then parameters input else []

  let rec depend input =
    match token input with
      | ID a | LINK a ->
	  skip input ;
	  ignore (key input ",") ;
	  a :: depend input
      | _ -> []

  let link def input =
    match token input with
      | LINK f | ID f ->
        let link = conv_bal def (f,(bal input.lexbuf)) in
        skip input; Lang.infoprover link
      | RECLINK l ->
        skip input ;
        begin try
          {Lang.altergo = conv_bal def (List.assoc "alt-ergo" l);
                why3    = conv_bal def (List.assoc "why3" l);
                coq     = conv_bal def (List.assoc "coq" l) }
        with Not_found ->
          failwith "a link must contain an entry for alt-ergo, why3 and coq"
        end
      | _ -> failwith "Missing link symbol"

  let linkstring input =
    match recorstring input.lexbuf with
      | `String f ->
        skip input ; Lang.infoprover f
      | `RecString l ->
        skip input ;
        begin try
          {Lang.altergo = List.assoc "alt-ergo" l;
                why3    = List.assoc "why3" l;
                coq     = List.assoc "coq" l }
        with Not_found ->
          failwith "a link must contain an entry for alt-ergo, why3 and coq"
        end
      | _ -> failwith "Missing link symbol"

  let input_string input =
    match token input with
      | LINK f | ID f ->
        skip input ; f
      | _ -> failwith "String or ident expected"


  let op = {
    inversible = false ;
    associative = false ;
    commutative = false ;
    idempotent = false ;
    neutral = E_none ;
    absorbant = E_none ;
  }

  let op_elt input =
    ignore (key input ":") ;
    let op = input_string input in
    skipkey input ":" ;
    match op with
      | "0" -> E_int 0
      | "1" -> E_int 1
      | "-1" -> E_int (-1)
      | "\\true" -> E_true
      | "\\false" -> E_false
      | _ ->
        match LogicBuiltins.constant op with
        | ACSLDEF -> failwith (Printf.sprintf "Symbol '%s' not found" op)
        | CONST _ ->
          failwith "Invariant broken only true and false can be const"
        | LFUN lfun -> E_const lfun

  let rec op_link op input =
    match token input with
      | LINK _ | RECLINK _ ->
          Operator op, link `Left input
      | ID "associative" -> skip input ; skipkey input ":" ; 
	  op_link { op with associative = true } input
      | ID "commutative" -> skip input ; skipkey input ":" ; 
	  op_link { op with commutative = true } input
      | ID "ac" -> skip input ; skipkey input ":" ;
	  op_link { op with commutative = true ; associative = true } input
      | ID "idempotent" -> skip input ; skipkey input ":" ;
	  op_link { op with idempotent = true } input
      | ID "inversible" -> skip input ; skipkey input ":" ;
	  op_link { op with inversible = true } input
      | ID "neutral" -> 
	  skip input ; let e = op_elt input in
	  op_link { op with neutral = e } input
      | ID "absorbant" ->
	  skip input ; let e = op_elt input in
	  op_link { op with absorbant = e } input
      | ID t -> failwith (Printf.sprintf "Unknown tag '%s'" t)
      | _ -> failwith "Missing <tag> or <link>"

  let logic_link input =
    match token input with
      | LINK _ | RECLINK _ ->
	  Function, link `Nary input
      | ID "constructor" -> 
	  skip input ; skipkey input ":" ; 
	  Constructor, link `Nary input
      | ID "injective" -> 
	  skip input ; skipkey input ":" ; 
	  Injection, link `Nary input
      | _ -> op_link op input

  let rec parse ~driver_dir library input =
    match token input with
      | EOF -> ()
      | KEY "library" ->
          skip input ;
          let name = input_string input in
          ignore (key input ":") ;
          let depends = depend input in
          ignore (key input ";") ;
          add_library name depends ;
          parse ~driver_dir name input
      | KEY "type" ->
	  skip input ;
	  let name = ident input in
	  noskipkey input "=" ;
	  let link = linkstring input in
	  add_type name ~library ~link () ;
	  skipkey input ";" ;
	  parse ~driver_dir library input
      | KEY "ctor" ->
	  skip input ;
	  let name = ident input in
	  let args = signature input in
	  skipkey input "=" ;
	  let link = link `Nary input in
	  add_ctor name args ~library ~link () ;
	  skipkey input ";" ;
	  parse ~driver_dir library input
      | KEY "logic" ->
	  skip input ;
	  let result = kind input in
	  let name = ident input in
	  let args = signature input in
          if key input ":=" then
            begin
              let alias = ident input in
              add_alias name args ~alias () ;
            end
          else
            begin
	      skipkey input "=" ;
              let category,link = logic_link input in
              add_logic result name args ~library ~category ~link () ;
            end ;
          skipkey input ";" ;
	  parse ~driver_dir library input
      | KEY "predicate" ->
	  skip input ;
	  let name = ident input in
	  let args = signature input in
          if key input ":=" then
            begin
              let alias = ident input in
              add_alias name args ~alias () ;
            end
          else
            begin
	      noskipkey input "=" ;
	      let link = linkstring input in
	      add_predicate name args ~library ~link () ;
            end ;
          skipkey input ";" ;
          parse ~driver_dir library input
      | FIELD (group,var) ->
	skip input ;
        begin match token input with
        | KEY ":=" ->
          let v = value input in
          set_option ~driver_dir group var ~library v
        | KEY "+=" ->
          let v = value input in
          add_option ~driver_dir group var ~library v
        | _ -> failwith "Missing ':=' or '+='"
        end;
        skipkey input ";" ;
        parse ~driver_dir library input
      | _ -> failwith "Unexpected entry"

  let load ?(feedback=true) file =
    try
      if feedback then
        Wp_parameters.feedback "Loading driver '%s'" (Filepath.pretty file) ;
      let driver_dir = Filename.dirname file in
      let inc = open_in file in
      let lex = Lexing.from_channel inc in
      lex.Lexing.lex_curr_p <-
        { lex.Lexing.lex_curr_p with Lexing.pos_fname = file } ;
      let input = { current = tok lex ; lexbuf = lex } in
      try
	parse ~driver_dir "qed" input ;
	close_in inc
      with Failure msg ->
	close_in inc ;
	let source = lex.Lexing.lex_start_p in
	Wp_parameters.abort ~current:false
          ~source "(Driver Error) %s (at %a)" msg
          pretty (token input)
    with exn ->
      Wp_parameters.abort
        ~current:false
        "Error in driver '%s': %s" file (Printexc.to_string exn)

  (*TODO[LC] Think about projectification ... *)
  let dkey = Wp_parameters.register_category "includes"

  let loaded : (string list * string list, driver) Hashtbl.t =Hashtbl.create 10
  let load_driver () =
    let drivers = Wp_parameters.Drivers.get () in
    let includes = Wp_parameters.get_includes () in
    let key = (drivers,includes) in
    begin try
        let driver = Hashtbl.find loaded key in
        Context.set LogicBuiltins.driver driver
      with Not_found ->
	let driver_basename file =
	  let base = Filename.basename file in
	  try Filename.chop_extension base
	  with Invalid_argument _ -> base in
	let drvs = List.map driver_basename drivers in
        let id = String.concat "_" drvs in
	let descr = String.concat "," drvs in
        let directories =
          let shared =
            try [Wp_parameters.Share.dir ~error:false ()]
            with Wp_parameters.Share.No_dir -> [] in
          let directories = includes @ shared in
          if Wp_parameters.has_dkey "includes" then
            Wp_parameters.debug ~dkey "Included directories:%t"
              (fun fmt ->
                 List.iter
                   (fun d -> Format.fprintf fmt "@\n - '%s'" d)
                   directories
              );
          directories
        in
        LogicBuiltins.new_driver ~includes:directories ~id ~descr ;
	let drivers =
	  List.map (fun file ->
		      if Sys.file_exists file
		      then Sysutil.absolutize_filename (Sys.getcwd ()) file
		      else LogicBuiltins.find_lib file)
            drivers in
        let default = Wp_parameters.Share.file ~error:true "wp.driver" in
        let feedback = Wp_parameters.Share.Dir_name.is_set () in
        load ~feedback default;
        List.iter load drivers;
        Hashtbl.add loaded key (Context.get LogicBuiltins.driver);
        if Wp_parameters.has_dkey "driver" then LogicBuiltins.dump ()
    end ; Context.get LogicBuiltins.driver

}
