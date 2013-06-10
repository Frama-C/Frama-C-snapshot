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
(* --- PARSER                                                             --- *)
(* -------------------------------------------------------------------------- *)

open Syntax
open Lexer

let keymap = Lexer.keymap [
  "and" ;
  "bool" ;
  "constructor" ;
  "div" ;
  "else" ; "exists" ;
  "false" ; "forall" ;
  "if" ; "iff"; "in" ; "include"; "injective" ; "int" ;
  "let" ;
  "mod" ;
  "not" ;
  "or" ;
  "prop" ;
  "real" ;
  "then" ; "true" ; "with" ;
]

let extend = Lexer.extend keymap

let parse_recursion = ref (fun _ -> assert false)
let parse_radditive = ref (fun _ -> assert false)

(* -------------------------------------------------------------------------- *)
(* --- Types                                                              --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_type input =
  context input "type" ;
  match parse_typelist input with
    | Some [t] ->
	parse_polymorphic t input
    | Some ts ->
	let id = skip_ident input in
	parse_polymorphic (T_SORT(ts,id)) input
    | None ->
	let t = parse_simpletype input in
	parse_polymorphic t input

and parse_simpletype input =
  match token input with
    | KEYWORD "bool" -> skip input ; T_BOOL
    | KEYWORD "prop" -> skip input ; T_PROP
    | KEYWORD "real" -> skip input ; T_REAL
    | KEYWORD "int"  -> skip input ; T_INT
    | QUOTED x -> let p = skip_pos input in T_ALPHA (p,x)
    | IDENT x -> let p = skip_pos input in T_SORT([],(p,x))
    | KEYWORD "{" ->
	skip input ; T_RECORD (parse_record_type input)
    | _ -> error input "missing type"

and parse_record_type input =
  context input "record" ;
  match token input with
    | KEYWORD "}" -> skip input ; []
    | IDENT a -> 
	let p = skip_pos input in
	let f = (p,a) in
	skip_key input ":" ;
	let t = parse_type input in
	begin
	  match token input with
	    | KEYWORD "}" -> skip input ; [f,t]
	    | KEYWORD ";" -> skip input ; (f,t)::parse_record_type input
	    | _ -> error input "missing record field"
	end
    | _ -> error input "missing record field"

and parse_typelist input =
  parse_list ~left:"(" ~sep:"," ~right:")" parse_type input

and parse_polymorphic t input =
  match token input with
    | IDENT x ->
	let id = skip_pos input,x in
	parse_polymorphic (T_SORT([t],id)) input
    | KEYWORD "[" ->
	let ta =
	  skip input ;
	  if is_key input "]" then T_ARRAY(T_INT,t)
	  else
	    let k = parse_type input in
	    skip_key input "]" ; T_ARRAY(k,t)
	in parse_polymorphic ta input
    | _ -> t

(* -------------------------------------------------------------------------- *)
(* --- Access                                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_access atom input =
  match token input with
    | KEYWORD "[" ->
	skip input ;
	let key = !parse_radditive input in
	let arr =
	  if is_key input "->" then
	    let value = !parse_radditive input in
	    A_SET( atom , key , value )
	  else
	    A_GET( atom , key )
	in skip_key input "]" ;
	parse_access arr input
    | KEYWORD "." ->
	skip input ;
	let field = skip_ident input in
	let acc = E_GETFIELD(atom,field,Ast.fresh()) in
	parse_access acc input
    | _ -> atom

(* -------------------------------------------------------------------------- *)
(* --- Records                                                            --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_fields input =
  let f = skip_ident input in
  skip_key input "=" ;
  let v = !parse_recursion input in
  parse_morefields f v input

and parse_morefields f v input =
  let k = Ast.fresh () in
  if is_key input ";" then (f,k,v)::parse_fields input else [f,k,v]

let parse_record parse_arguments input =
  match token input with
    | IDENT x ->
	let id = skip_pos input,x in
	let xs = parse_arguments input in
	if xs <> [] || token input = KEYWORD "with" then
	  begin
	    let r = E_FUN(id,Ast.fresh(),xs) in
	    skip_key input "with" ;
	    let fs = parse_fields input in
	    E_SETFIELD(r,Ast.fresh(),fs)
	  end
	else
	  begin
	    skip_key input "=" ;
	    let v = !parse_recursion input in
	    E_RECORD (fst id,parse_morefields id v input)
	  end
    | KEYWORD "}" ->
	(* not skip! *)
	let pos = position input in
	E_RECORD(pos,[])
    | _ ->
	let r = !parse_recursion input in
	skip_key input "with" ;
	let fs = parse_fields input in
	E_SETFIELD(r,Ast.fresh(),fs)

(* -------------------------------------------------------------------------- *)
(* --- Atoms                                                              --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_leaf input =
  match token input with
    | INT x   -> let p = skip_pos input in Some(E_INT(p,x))
    | REAL x  -> let p = skip_pos input in Some(E_REAL(p,x))
    | IDENT "_" -> let p = skip_pos input in Some(E_ANY p)
    | IDENT x -> let p = skip_pos input in Some(E_FUN((p,x),Ast.fresh(),[]))
    | QUOTED x -> let p = skip_pos input in Some(E_PVAR(p,x))
    | KEYWORD "true" -> let p = skip_pos input in Some(E_TRUE p)
    | KEYWORD "false" -> let p = skip_pos input in Some(E_FALSE p)
    | KEYWORD "(" ->
	skip input ;
	let e = !parse_recursion input in
	skip_key input ")" ; Some e
    | KEYWORD "{" ->
	skip input ; 
	let e = parse_record parse_arguments input in 
	skip_key input "}" ; Some e
    | _ -> None

and parse_arg input =
  match parse_leaf input with
    | Some e -> Some (parse_access e input)
    | None -> None

and parse_arguments input =
  context input "arguments" ;
  match parse_arg input with
    | None -> []
    | Some x -> x :: parse_arguments input

and parse_value input =
  match parse_arg input with
    | Some e -> e
    | None -> error input "Missing expression"

(* -------------------------------------------------------------------------- *)
(* --- Factors                                                            --- *)
(* -------------------------------------------------------------------------- *)

let parse_factor input =
  context input "factor" ;
  match token input with
    | IDENT x ->
	let id = skip_pos input,x in
	let xs = parse_arguments input in
	let value = E_FUN(id,Ast.fresh(),xs) in
	if xs<>[] then value else parse_access value input
    | KEYWORD "-" ->
	let p = skip_pos input in
	let a = parse_value input in
	E_UNA(p,OPP,a)
    | KEYWORD "not" ->
	let p = skip_pos input in
	let a = parse_value input in
	E_UNA(p,NOT,a)
    | _ -> parse_value input

(* -------------------------------------------------------------------------- *)
(* --- Divisions                                                          --- *)
(* -------------------------------------------------------------------------- *)

let parse_division input =
  let a = parse_factor input in
  match token input with
    | KEYWORD "mod" ->
	let p = skip_pos input in
	let b = parse_factor input in
	E_BIN(a,p,MOD,b)
    | KEYWORD "div" ->
	let p = skip_pos input in
	let b = parse_factor input in
	E_BIN(a,p,DIV,b)
    | _ -> a

(* -------------------------------------------------------------------------- *)
(* --- Multiplications                                                    --- *)
(* -------------------------------------------------------------------------- *)

let parse_multiplicative input =
  let rec pp_mult x input =
    match token input with
      | KEYWORD "*" ->
	  let p = skip_pos input in
	  let y = parse_division input in
	  let z = E_BIN(x,p,MUL,y) in
	  pp_mult z input
      | _ -> x
  in
  context input "multiplicative" ;
  pp_mult (parse_division input) input

(* -------------------------------------------------------------------------- *)
(* --- Additions                                                          --- *)
(* -------------------------------------------------------------------------- *)

let parse_additive input =
  let rec pp_add x input =
    match token input with
      | KEYWORD "+" ->
	  let p = skip_pos input in
	  let y = parse_multiplicative input in
	  let z = E_BIN(x,p,ADD,y) in
	  pp_add z input
      | KEYWORD "-" ->
	  let p = skip_pos input in
	  let y = parse_multiplicative input in
	  let z = E_BIN(x,p,SUB,y) in
	  pp_add z input
      | _ -> x
  in
  context input "additive" ;
  pp_add (parse_multiplicative input) input

(* -------------------------------------------------------------------------- *)
(* --- Relations                                                          --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_ascending x input =
  match token input with
    | KEYWORD "<" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,LT,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,AND,w)
    | KEYWORD "<=" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,LEQ,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,AND,w)
    | _ -> x

let rec parse_descending x input =
  match token input with
    | KEYWORD ">" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,GT,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,ADD,w)
    | KEYWORD ">=" ->
	let p = skip_pos input in
	let y = parse_additive input in
	let t = E_BIN(x,p,GEQ,y) in
	let w = parse_ascending y input in
	if w == y then t else E_BIN(t,p,AND,w)
    | _ -> x

let parse_relation input =
  context input "relation" ;
  let x = parse_additive input in
  match token input with
    | KEYWORD ("<" | "<=") -> (* no skip *) parse_ascending x input
    | KEYWORD (">" | ">=") -> (* no skip *) parse_descending x input
    | KEYWORD "=" ->
	let p = skip_pos input in
	let y = parse_additive input in
	E_BIN(x,p,EQ,y)
    | KEYWORD "!=" ->
	let p = skip_pos input in
	let y = parse_additive input in
	E_BIN(x,p,NEQ,y)
    | _ -> x

(* -------------------------------------------------------------------------- *)
(* --- Logical                                                            --- *)
(* -------------------------------------------------------------------------- *)

let parse_logical input =
  let rec pp_log x input =
    match token input with
      | KEYWORD "and" ->
	  let p = skip_pos input in
	  let y = parse_relation input in
	  let z = E_BIN(x,p,AND,y) in
	  pp_log z input
      | KEYWORD "or" ->
	  let p = skip_pos input in
	  let y = parse_relation input in
	  let z = E_BIN(x,p,OR,y) in
	  pp_log z input
      | _ -> x
  in context input "logical" ;
  let x = parse_relation input in
  pp_log x input

let rec parse_idents input =
  let x = skip_ident input in
  if is_key input "," then x :: parse_idents input else [x]

let rec parse_bindings binder input =
  context input "bindings" ;
  let xs = parse_idents input in
  let t = if is_key input ":" then Some(parse_type input) else None in
  let p =
    match token input with
      | KEYWORD "." ->
	  skip input ;
	  !parse_recursion input
      | KEYWORD "," ->
	  skip input ;
	  parse_bindings binder input
      | _ ->
	  error input "expected '.' or ','"
  in
  List.fold_right (fun x p -> binder x t p) xs p

(* -------------------------------------------------------------------------- *)
(* --- Left-to-right (deductive and bindings)                             --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_deductive input =
  context input "deductive" ;
  match token input with
    | KEYWORD "let" ->
	skip input ;
	let id = skip_ident input in
	let t = if is_key input ":" then Some(parse_type input) else None in
	skip_key input "=" ;
	let a = parse_deductive input in
	skip_key input "in" ;
	let b = parse_deductive input in
	E_LET(id,Ast.fresh(),t,a,b)
    | KEYWORD "if" ->
	skip input ;
	let e = parse_deductive input in
	skip_key input "then" ;
	let a = parse_deductive input in
	skip_key input "else" ;
	let b = parse_deductive input in
	E_IF(e,Ast.fresh(),a,b)
    | KEYWORD "forall" ->
	skip input ;
	parse_bindings (fun x t p -> E_FORALL(x,Ast.fresh(),t,[],p)) input
    | KEYWORD "exists" ->
	skip input ;
	parse_bindings (fun x t p -> E_EXISTS(x,Ast.fresh(),t,[],p)) input
    | _ ->
	let x = parse_logical input in
	match token input with
	  | KEYWORD ("=>"|"->") ->
	      let p = skip_pos input in
	      let y = parse_deductive input in
	      E_BIN(x,p,IMPLY,y)
	  | KEYWORD ("<=>"|"<->") ->
	      let p = skip_pos input in
	      let y = parse_logical input in
	      E_BIN(x,p,EQUIV,y)
	  | _ -> x

(* -------------------------------------------------------------------------- *)
(* --- Root Expressions                                                   --- *)
(* -------------------------------------------------------------------------- *)

let parse_expr = parse_deductive
let () = 
  begin
    parse_recursion := parse_deductive ;
    parse_radditive := parse_additive ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Declarations                                                       --- *)
(* -------------------------------------------------------------------------- *)

let rec parse_quoted xs input =
  match token input with
    | QUOTED a -> let p = skip_pos input in parse_quoted ((p,a)::xs) input
    | IDENT a -> let p = skip_pos input in List.rev xs , (p,a)
    | _ -> error input "expected type name or polymorphic type variable"

let parse_typedef input = parse_quoted [] input

let rec parse_fsig ts input =
  let tr = parse_type input in
  if is_key input "->" then
    parse_fsig (tr::ts) input
  else
    List.rev ts , tr

let parse_signature input = parse_fsig [] input

let rec parse_args input =
  match token input with
    | IDENT _ ->
	let x = skip_ident input in
	( x , Ast.fresh () , None ) :: parse_args input
    | KEYWORD "(" ->
	skip input ;
	let x = skip_ident input in
	skip_key input ":" ;
	let t = parse_type input in
	skip_key input ")" ;
	( x , Ast.fresh () , Some t ) :: parse_args input
    | _ -> []

let parse_category input =
  match token input with
    | KEYWORD "constructor" -> skip input ; Logic.Constructor
    | KEYWORD "injective" -> skip input ; Logic.Injection
    | _ -> Logic.Function

(* -------------------------------------------------------------------------- *)
