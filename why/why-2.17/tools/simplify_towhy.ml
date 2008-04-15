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
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: simplify_towhy.ml,v 1.11 2008/11/05 14:03:19 filliatr Exp $ i*)

open Format
open Pp
open Simplify_ast

let funs : (string, int) Hashtbl.t = Hashtbl.create 97
let declare_fun f n = 
  try assert (Hashtbl.find funs f = n)
  with Not_found -> Hashtbl.add funs f n
let reset () = 
  Hashtbl.clear funs;
  declare_fun "at_true" 0

let declare_funs =
  let rec term = function
    | Tconst _ 
    | Tapp (("+"|"*"|"-"), _) -> ()
    | Tapp (f, l) -> declare_fun f (List.length l); List.iter term l
  in
  let rec predicate = function
    | Ptrue | Pfalse -> ()
    | Prel (t1, _, t2) -> term t1; term t2
    | Pnot p | Plblpos (_, p) | Plblneg (_, p) -> predicate p
    | Pand l | Por l -> List.iter predicate l
    | Pimplies (p1, p2) | Piff (p1, p2) -> predicate p1; predicate p2
    | Pdistinct l -> List.iter term l
    | Pforall (_, tl, p) | Pexists (_, tl, p) ->
	List.iter (List.iter term) tl; predicate p
  in
  let decl = function
    | Axiom p | Goal p -> predicate p
    | Defpred (f, l, p) -> declare_fun f (List.length l); predicate p
  in
  List.iter decl

let new_goal = let r = ref 0 in fun () -> incr r; "goal_" ^ string_of_int !r
let new_axiom = let r = ref 0 in fun () -> incr r; "axiom_" ^ string_of_int !r

let is_why_keyword =
  let ht = Hashtbl.create 50  in
  List.iter (fun kw -> Hashtbl.add ht kw ()) 
    [ "absurd";      "and";      "array";      "as";      "assert";
      "axiom";      "begin";      "bool";      "do";      "done";
      "else";      "end";      "exception";      "exists";      "external";
      "false";      "for";      "forall";      "fun";      "function";
      "goal";      "if";      "in";      "int";      "invariant";
      "let";      "logic";      "not";      "of";      "or";
      "parameter";      "predicate";      "prop";      "raise";      "raises";
      "reads";      "real";      "rec";      "ref";      "returns";
      "then";      "true";      "try";      "type";      "unit";
      "variant";      "void";      "while";      "with";      "writes"; ];
  Hashtbl.mem ht

let ident fmt id = 
  if is_why_keyword id then fprintf fmt "why__%s" id else fprintf fmt "%s" id

let rec print_term fmt = function
  | Tconst n -> 
      fprintf fmt "%s" n
  | Tapp (id, []) -> 
      fprintf fmt "%a" ident id
  | Tapp ("+", [t1; t2]) ->
      fprintf fmt "(%a + %a)" print_term t1 print_term t2
  | Tapp ("-", [t1; t2]) ->
      fprintf fmt "(%a - %a)" print_term t1 print_term t2
  | Tapp ("*", [t1; t2]) ->
      fprintf fmt "(%a * %a)" print_term t1 print_term t2
  | Tapp ("-", [t1]) ->
      fprintf fmt "(-%a)" print_term t1
  | Tapp (id, tl) -> 
      fprintf fmt "%a(%a)" ident id (print_list comma print_term) tl

let triggers fmt = function
  | [] -> 
      ()
  | tl -> 
      fprintf fmt " @[[%a]@]" (print_list alt (print_list comma print_term)) tl

let rec print_predicate fmt = function
  | Prel (t1, Eq, t2) ->
      fprintf fmt "(%a = %a)" print_term t1 print_term t2
  | Prel (t1, Neq, t2) ->
      fprintf fmt "(%a <> %a)" print_term t1 print_term t2
  | Prel (t1, Lt, t2) ->
      fprintf fmt "(%a < %a)" print_term t1 print_term t2
  | Prel (t1, Le, t2) ->
      fprintf fmt "(%a <= %a)" print_term t1 print_term t2
  | Prel (t1, Gt, t2) ->
      fprintf fmt "(%a > %a)" print_term t1 print_term t2
  | Prel (t1, Ge, t2) ->
      fprintf fmt "(%a >= %a)" print_term t1 print_term t2
  | Ptrue ->
      fprintf fmt "true"
  | Pfalse ->
      fprintf fmt "false"
  | Pimplies (a, b) -> 
      fprintf fmt "@[(%a ->@ %a)@]" print_predicate a print_predicate b
  | Piff (a, b) -> 
      fprintf fmt "@[(%a <->@ %a)@]" print_predicate a print_predicate b
  | Pand l ->
      let sep fmt () = fprintf fmt " and@ " in
      fprintf fmt "@[%a@]" (print_list sep print_predicate) l
  | Por l ->
      let sep fmt () = fprintf fmt " or@ " in
      fprintf fmt "@[%a@]" (print_list sep print_predicate) l
  | Pnot a ->
      fprintf fmt "@[(not@ %a)@]" print_predicate a
  | Pforall (vl,tl,p) ->
      fprintf fmt "@[<hov 2>("; 
      print_quantifier fmt "forall" vl tl p; 
      fprintf fmt ")@]"
  | Pexists (vl,tl,p) ->
      fprintf fmt "@[<hov 2>("; 
      print_quantifier fmt "exists" vl tl p; 
      fprintf fmt ")@]"
  | Plblneg (_, p)
  | Plblpos (_, p) -> 
      print_predicate fmt p (* TODO *)
  | Pdistinct l ->
      fprintf fmt "@[distinct(%a)@]" (print_list comma print_term) l

and print_quantifier fmt q vl tl p =
  let rec mk_quant = function
    | [] -> 
	assert false
    | [x] -> 
	fprintf fmt "%s %a:int%a.@ %a" q ident x triggers tl print_predicate p
    | x :: xl -> 
	fprintf fmt "%s %a:int.@ " q ident x; mk_quant xl
  in
  mk_quant vl

let print_binder fmt x = fprintf fmt "%a:int" ident x
let print_var fmt x = fprintf fmt "%a" ident x

let print_decl fmt = function
  | Axiom p -> 
      let id = new_axiom () in
      fprintf fmt "@[<hov 2>axiom %s:@ %a@]@\n@\n" id print_predicate p 
  | Goal p ->
      let id = new_goal () in
      fprintf fmt "@[<hov 2>goal %s:@ %a@]@\n@\n" id print_predicate p 
  | Defpred (id, bl, p) ->
      let a = new_axiom () in
      fprintf fmt 
	"@[<hov 2>axiom %s: forall %a:int. (%a(%a) = at_true) <-> %a@]@\n@\n" 
	a (print_list comma print_var) bl 
	ident id (print_list comma print_var) bl print_predicate p

let print_fun fmt f n =
  fprintf fmt "@[logic %a: " ident f;
  for i = 1 to n do fprintf fmt "int"; if i < n then fprintf fmt "," done;
  fprintf fmt " -> int@]@\n"

let report_error_and_exit f lb e =
  let loc = Lexing.lexeme_start lb in
  eprintf "File \"%s\", character %d:@\n" f loc;
  eprintf "%s@." (Printexc.to_string e);
  exit 1

let translate_file f =
  reset ();
  let c = open_in f in
  let lb = Lexing.from_channel c in
  let s = 
    try Simplify_parser.start Simplify_lexer.token lb 
    with e -> report_error_and_exit f lb e
  in
  close_in c;
  declare_funs s;
  let whyf = f ^ ".why" in
  let c = open_out whyf in
  let fmt = formatter_of_out_channel c in
  Hashtbl.iter (print_fun fmt) funs;
  List.iter (print_decl fmt) s;
  fprintf fmt "@.";
  close_out c

let files = Queue.create ()
let () = Arg.parse [] (fun f -> Queue.add f files) ""
let () = Queue.iter translate_file files


