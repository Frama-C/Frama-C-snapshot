(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(* ========================================================================= *)
(* Simple algebraic expression example from the introductory chapter.        *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

type expression =
   Var of string
 | Const of int
 | Add of expression * expression
 | Mul of expression * expression;;

(* ------------------------------------------------------------------------- *)
(* Trivial example of using the type constructors.                           *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
Add(Mul(Const 2,Var "x"),Var "y");;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Simplification example.                                                   *)
(* ------------------------------------------------------------------------- *)

let simplify1 expr =
  match expr with
    Add(Const(m),Const(n)) -> Const(m + n)
  | Mul(Const(m),Const(n)) -> Const(m * n)
  | Add(Const(0),x) -> x
  | Add(x,Const(0)) -> x
  | Mul(Const(0),x) -> Const(0)
  | Mul(x,Const(0)) -> Const(0)
  | Mul(Const(1),x) -> x
  | Mul(x,Const(1)) -> x
  | _ -> expr;;

let rec simplify expr =
  match expr with
    Add(e1,e2) -> simplify1(Add(simplify e1,simplify e2))
  | Mul(e1,e2) -> simplify1(Mul(simplify e1,simplify e2))
  | _ -> expr;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)
START_INTERACTIVE;;
let e = Add(Mul(Add(Mul(Const(0),Var "x"),Const(1)),Const(3)),
            Const(12));;
simplify e;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Lexical analysis.                                                         *)
(* ------------------------------------------------------------------------- *)

let matches s = let chars = explode s in fun c -> mem c chars;;

let space = matches " \t\n"
and punctuation = matches "()[]{},"
and symbolic = matches "~`!@#$%^&*-+=|\\:;<>.?/"
and numeric = matches "0123456789"
and alphanumeric = matches
  "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;

let rec lexwhile prop inp =
  match inp with
    [] -> "",[]
  | c::cs ->
        if prop c then let tok,rest = lexwhile prop cs in c^tok,rest
        else "",inp;;

let rec lex inp =
  let _,inp1 = lexwhile space inp in
  match inp1 with
    [] -> []
  | c::cs -> let prop =
               if alphanumeric(c) then alphanumeric
               else if symbolic(c) then symbolic
               else if punctuation(c) then (fun c -> false)
               else failwith "Unknown character in input" in
             let toktl,rest = lexwhile prop cs in
             (c^toktl)::lex rest;;

START_INTERACTIVE;;
lex(explode "2*((var_1 + x') + 11)");;
lex(explode "if (*p1-- == *p2++) then f() else g()");;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Parsing.                                                                  *)
(* ------------------------------------------------------------------------- *)

let rec parse_expression inp =
  let e1,inp1 = parse_product inp in
  if inp1 <> [] & hd inp1 = "+" then
     let e2,inp2 = parse_expression (tl inp1) in Add(e1,e2),inp2
  else e1,inp1

and parse_product inp =
  let e1,inp1 = parse_atom inp in
  if inp1 <> [] & hd inp1 = "*" then
     let e2,inp2 = parse_product (tl inp1) in Mul(e1,e2),inp2
  else e1,inp1

and parse_atom inp =
  match inp with
    [] -> failwith "Expected an expression at end of input"
  | tok::toks ->
        if tok = "(" then
           let e,inp1 = parse_expression toks in
           if inp1 <> [] & hd inp1 = ")" then e,tl inp1
           else failwith "Expected closing bracket"
        else if forall numeric (explode tok) then
           Const(int_of_string tok),toks
        else Var(tok),toks;;

(* ------------------------------------------------------------------------- *)
(* Generic function to impose lexing and exhaustion checking on a parser.    *)
(* ------------------------------------------------------------------------- *)

let make_parser pfn s =
  let expr,rest = pfn (lex(explode s)) in
  if rest = [] then expr else failwith "Unparsed input";;

(* ------------------------------------------------------------------------- *)
(* Our parser.                                                               *)
(* ------------------------------------------------------------------------- *)

let parsee = make_parser parse_expression;;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
parsee "x + 1";;

parsee "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)";;

END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Conservatively bracketing first attempt at printer.                       *)
(* ------------------------------------------------------------------------- *)

let rec string_of_exp e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) -> "("^(string_of_exp e1)^" + "^(string_of_exp e2)^")"
  | Mul(e1,e2) -> "("^(string_of_exp e1)^" * "^(string_of_exp e2)^")";;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
string_of_exp(parsee "x + 3 * y");;
let e = parsee "3 * (y + z) + 7 * 4";;
parsee(string_of_exp e) = e;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Somewhat better attempt.                                                  *)
(* ------------------------------------------------------------------------- *)

let rec string_of_exp pr e =
  match e with
    Var s -> s
  | Const n -> string_of_int n
  | Add(e1,e2) ->
        let s = (string_of_exp 3 e1)^" + "^(string_of_exp 2 e2) in
        if pr > 2 then "("^s^")" else s
  | Mul(e1,e2) ->
        let s = (string_of_exp 5 e1)^" * "^(string_of_exp 4 e2) in
        if pr > 4 then "("^s^")" else s;;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
string_of_exp 0 (parsee "x + 3 * y");;
string_of_exp 0 (parsee "(x + 3) * y");;
string_of_exp 0 (parsee "1 + 2 + 3");;
string_of_exp 0 (parsee "((1 + 2) + 3) + 4");;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Example shows the problem.                                                *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let e = parsee "(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) *
                (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)";;

string_of_exp 0 e;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Real printer with proper line breaks.                                     *)
(* ------------------------------------------------------------------------- *)

let rec print_exp pr e =
  match e with
    Var s -> print_string s
  | Const n -> print_int n
  | Add(e1,e2) ->
        if pr > 2 then (print_string "("; open_box 0) else ();
        print_exp 3 e1;
        print_string " +"; print_space();
        print_exp 2 e2;
        if pr > 2 then (close_box(); print_string ")") else ()
  | Mul(e1,e2) ->
        if pr > 4 then (print_string "("; open_box 0) else ();
        print_exp 5 e1;
        print_string " *"; print_space();
        print_exp 4 e2;
        if pr > 4 then (close_box(); print_string ")") else ();;

let print_expression e =
  open_box 0; print_string "<<";
              open_box 0; print_exp 0 e; close_box();
  print_string ">>"; close_box();;

(* ------------------------------------------------------------------------- *)
(* Also set up parsing of quotations.                                        *)
(* ------------------------------------------------------------------------- *)

let default_parser = parsee;;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
print_expression(Mul(Const 3,Add(Mul(e,e),Mul(e,e))));;

#install_printer print_expression;;

parsee "3 + x * y";;

<<3 + x * y>>;;

END_INTERACTIVE;;
