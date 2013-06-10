(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

  type token =
    | EOF
    | KEY of string
    | BOOLEAN
    | INTEGER
    | REAL
    | INT of ikind
    | FLT of fkind
    | ID of string
    | LINK of string

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
    "double" , FLT FDouble ;
  ]
  
  let ident x = try List.assoc x keywords with Not_found -> ID x

  let newline lexbuf =
    lexbuf.lex_curr_p <-
      { lexbuf.lex_curr_p with pos_lnum = succ lexbuf.lex_curr_p.pos_lnum }

}

let blank = [ ' ' '\t' ]
let ident = [ 'a'-'z' 'A'-'Z' '_' '0'-'9' ]+ 

rule tok = parse
    eof { EOF }
  | '\n' { newline lexbuf ; tok lexbuf }
  | blank+ { tok lexbuf }
  | "//" [^ '\n']* '\n' { newline lexbuf ; tok lexbuf }
  | "/*" { comment lexbuf }
  | ident as a { ident a }
  | '"' (ident as a) '"' { LINK a }
  | _ { KEY (Lexing.lexeme lexbuf) }

and comment = parse
  | eof { failwith "Unterminated comment" }
  | "*/" { tok lexbuf }
  | '\n' { newline lexbuf ; comment lexbuf }
  | _ { comment lexbuf }

{

  let pretty fmt = function
    | EOF -> Format.pp_print_string fmt "<eof>"
    | KEY a | ID a -> Format.fprintf fmt "'%s'" a
    | LINK s -> Format.fprintf fmt "\"%s\"" s
    | BOOLEAN | INTEGER | REAL | INT _ | FLT _  -> 
	Format.pp_print_string fmt "<type>"

  type input = {
    lexbuf : Lexing.lexbuf ;
    mutable current : token ;
  }

  let skip input =
    if input.current <> EOF then input.current <- tok input.lexbuf

  let token input = input.current

  let key input a = match token input with
    | KEY b when a=b -> skip input ; true
    | _ -> false

  let skipkey input a = match token input with
    | KEY b when a=b -> skip input
    | _ -> failwith (Printf.sprintf "Missing '%s'" a)

  let ident input = match token input with
    | ID x -> skip input ; x
    | _ -> failwith "missing identifier"

  let kind input = 
    let kd = match token input with
      | INTEGER -> Z
      | REAL -> R
      | BOOLEAN -> A
      | INT i -> I (Ctypes.c_int i)
      | FLT f -> F (Ctypes.c_float f)
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

  let link input =
    match token input with
      | LINK f | ID f -> skip input ; f
      | _ -> failwith "Missing link symbol"

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
    let op = link input in
    skipkey input ":" ;
    match op with
      | "0" -> E_int 0
      | "1" -> E_int 1
      | "-1" -> E_int (-1)
      | _ -> 
	  try E_const (LogicBuiltins.symbol op)
	  with Not_found -> 
	    failwith (Printf.sprintf "Symbol '%s' undefined" op)
    
  let rec op_link bal op input =
    match token input with
      | LINK f ->
	  skip input ;
	  bal,Operator op,f
      | ID "left" -> skip input ; skipkey input ":" ;
	  op_link Lang.Left op input
      | ID "right" -> skip input ; skipkey input ":" ;
	  op_link Lang.Right op input
      | ID "associative" -> skip input ; skipkey input ":" ; 
	  op_link bal { op with associative = true } input
      | ID "commutative" -> skip input ; skipkey input ":" ; 
	  op_link bal { op with commutative = true } input
      | ID "ac" -> skip input ; skipkey input ":" ;
	  op_link bal { op with commutative = true ; associative = true } input
      | ID "idempotent" -> skip input ; skipkey input ":" ;
	  op_link bal { op with idempotent = true } input
      | ID "inversible" -> skip input ; skipkey input ":" ;
	  op_link bal { op with inversible = true } input
      | ID "neutral" -> 
	  skip input ; let e = op_elt input in
	  op_link bal { op with neutral = e } input
      | ID "absorbant" ->
	  skip input ; let e = op_elt input in
	  op_link bal { op with absorbant = e } input
      | ID t -> failwith (Printf.sprintf "Unknown tag '%s'" t)
      | _ -> failwith "Missing <tag> or <link>"

  let logic_link input =
    match token input with
      | LINK f -> 
	  skip input ; 
	  Lang.Nary,Function,f
      | ID "constructor" -> 
	  skip input ; skipkey input ":" ; 
	  Lang.Nary,Function,link input
      | ID "injective" -> 
	  skip input ; skipkey input ":" ; 
	  Lang.Nary,Injection,link input
      | _ -> op_link Lang.Left op input

  let rec parse theory input =
    match token input with
      | EOF -> ()
      | KEY "library" ->
	  skip input ;
	  let name = link input in
	  ignore (key input ":") ;
	  let depends = depend input in
	  ignore (key input ";") ;
	  add_library name depends ;
	  parse name input 
      | KEY "type" ->
	  skip input ;
	  let name = ident input in
	  skipkey input "=" ;
	  let link = link input in
	  add_type name ~theory ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | KEY "ctor" ->
	  skip input ;
	  let name = ident input in
	  let args = signature input in
	  skipkey input "=" ;
	  let link = link input in
	  add_ctor name args ~theory ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | KEY "logic" ->
	  skip input ;
	  let result = kind input in
	  let name = ident input in
	  let args = signature input in
	  skipkey input "=" ;
	  let balance,category,link = logic_link input in
	  add_logic result name args ~theory ~category ~balance ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | KEY "predicate" ->
	  skip input ;
	  let name = ident input in
	  let args = signature input in
	  skipkey input "=" ;
	  let link = link input in
	  add_predicate name args ~theory ~link () ;
	  skipkey input ";" ;
	  parse theory input
      | _ -> failwith "Unexpected entry"

  let load file =
    try
      let inc = open_in file in
      let lex = Lexing.from_channel inc in
      lex.Lexing.lex_curr_p <- { lex.Lexing.lex_curr_p with Lexing.pos_fname = file } ;
      let input = { current = tok lex ; lexbuf = lex } in
      try
	parse "driver" input ;
	close_in inc
      with Failure msg ->
	close_in inc ;
	let source = lex.Lexing.lex_start_p in
	Wp_parameters.error ~source "(Driver Error) %s (at %a)" msg
          pretty (token input)
    with exn ->
      Wp_parameters.error "Error in driver '%s': %s" file (Printexc.to_string exn)

  (*TODO[LC] Think about projectification ... *)
  let loaded = ref false
  let load_drivers () =
    if not !loaded then
      begin
	List.iter
	  (fun file -> 
	     let path = Wp_parameters.find_lib file in
	     let echo = if Wp_parameters.has_dkey "driver" then path else file in
	     Wp_parameters.feedback "Loading driver '%s'" echo ;
	     load path)
	  (Wp_parameters.Drivers.get ()) ;
	loaded := true ;
	if Wp_parameters.has_dkey "driver" then LogicBuiltins.dump () ;
      end

}
