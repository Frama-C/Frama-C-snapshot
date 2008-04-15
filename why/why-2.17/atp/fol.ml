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

(* ========================================================================= *)
(* Basic stuff for first order logic.                                        *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Terms.                                                                    *)
(* ------------------------------------------------------------------------- *)

type term = Var of string
          | Fn of string * term list;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
Fn("sqrt",[Fn("-",[Fn("1",[]);
                   Fn("cos",[Fn("power",[Fn("+",[Var "x"; Var "y"]);
                                        Fn("2",[])])])])]);;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Abbreviation for FOL formula.                                             *)
(* ------------------------------------------------------------------------- *)

type fol = R of string * term list;;

(* ------------------------------------------------------------------------- *)
(* Trivial example of "x + y < z".                                           *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
Atom(R("<",[Fn("+",[Var "x"; Var "y"]); Var "z"]));;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Parsing of terms.                                                         *)
(* ------------------------------------------------------------------------- *)

let is_const s = forall numeric (explode s) or s = "nil";;

let rec parse_atomic_term vs inp =
  match inp with
    [] -> failwith "term expected"
  | "("::rest -> parse_bracketed (parse_term vs) ")" rest
  | f::"("::")"::rest -> Fn(f,[]),rest
  | f::"("::rest ->
      papply (fun args -> Fn(f,args))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
  | a::rest ->
      (if is_const a & not(mem a vs) then Fn(a,[]) else Var a),rest

and parse_term vs inp =
  parse_right_infix "::" (fun (e1,e2) -> Fn("::",[e1;e2]))
    (parse_right_infix "+" (fun (e1,e2) -> Fn("+",[e1;e2]))
       (parse_left_infix "-" (fun (e1,e2) -> Fn("-",[e1;e2]))
           (parse_right_infix "*" (fun (e1,e2) -> Fn("*",[e1;e2]))
                (parse_left_infix "^" (fun (e1,e2) -> Fn("^",[e1;e2]))
                   (parse_atomic_term vs))))) inp;;

let parset = make_parser (parse_term []);;

(* ------------------------------------------------------------------------- *)
(* Parsing of formulas.                                                      *)
(* ------------------------------------------------------------------------- *)

let parse_atom vs inp =
  try let tm,rest = parse_term vs inp in
      if exists (nextin rest) ["="; "<"; "<="; ">"; ">="] then
            papply (fun tm' -> Atom(R(hd rest,[tm;tm'])))
                   (parse_term vs (tl rest))
      else failwith ""
  with Failure _ ->
  match inp with
  | p::"("::")"::rest -> Atom(R(p,[])),rest
  | p::"("::rest ->
      papply (fun args -> Atom(R(p,args)))
             (parse_bracketed (parse_list "," (parse_term vs)) ")" rest)
  | p::rest when p <> "(" -> Atom(R(p,[])),rest
  | _ -> failwith "parse_atom";;

let parse = make_parser (parse_formula parse_atom []);;

(* ------------------------------------------------------------------------- *)
(* Set up parsing of quotations.                                             *)
(* ------------------------------------------------------------------------- *)

let default_parser = parse;;

let secondary_parser = parset;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
<<(forall x. x < 2 ==> 2 * x <= 3) \/ false>>;;

<<|2 * x|>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Printing of terms.                                                        *)
(* ------------------------------------------------------------------------- *)

let rec print_term prec fm =
  match fm with
    Var x -> print_string x
  | Fn("^",[tm1;tm2]) -> print_infix_term true prec 22 "^" tm1 tm2
  | Fn("*",[tm1;tm2]) -> print_infix_term false prec 20 "*" tm1 tm2
  | Fn("-",[tm1;tm2]) -> print_infix_term true prec 18 "-" tm1 tm2
  | Fn("+",[tm1;tm2]) -> print_infix_term false prec 16 "+" tm1 tm2
  | Fn("::",[tm1;tm2]) -> print_infix_term false prec 14 "::" tm1 tm2
  | Fn(f,args) -> print_fargs f args

and print_fargs f args =
  print_string f;
  if args = [] then () else
   (print_string "(";
    open_box 0;
    print_term 0 (hd args); print_break 0 0;
    do_list (fun t -> print_string ","; print_break 0 0; print_term 0 t)
            (tl args);
    close_box();
    print_string ")")

and print_infix_term isleft oldprec newprec sym p q =
  if oldprec > newprec then (print_string "("; open_box 0) else ();
  print_term (if isleft then newprec else newprec+1) p;
  print_string(" "^sym); print_space();
  print_term (if isleft then newprec+1 else newprec) q;
  if oldprec > newprec then (close_box(); print_string ")") else ();;

let printert fm = open_box 0; print_term 0 fm; close_box();;

START_INTERACTIVE;;
#install_printer printert;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Printing of formulas.                                                     *)
(* ------------------------------------------------------------------------- *)

let print_atom prec (R(p,args)) =
  if mem p ["="; "<"; "<="; ">"; ">="] & length args = 2
  then print_infix_term false 12 12 p (el 0 args) (el 1 args)
  else print_fargs p args;;

let printer = formula_printer print_atom;;

START_INTERACTIVE;;
#install_printer printer;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Examples in the main text.                                                *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
<<forall x y. exists z. x < z /\ y < z>>;;

<<~(forall x. P(x)) <=> exists y. ~P(y)>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Model-theoretic notions, but here restricted to finite interpretations.   *)
(* ------------------------------------------------------------------------- *)

type ('a)interpretation =
  Interp of ('a)list *
            (string -> ('a)list -> 'a) *
            (string -> ('a)list -> bool);;

let domain(Interp(d,funs,preds)) = d
and func(Interp(d,funs,preds)) = funs
and predicate(Interp(d,funs,preds)) = preds;;

(* ------------------------------------------------------------------------- *)
(* Semantics.                                                                *)
(* ------------------------------------------------------------------------- *)

let rec termval md v tm =
  match tm with
    Var(x) -> apply v x
  | Fn(f,args) -> func(md) f (map (termval md v) args);;

let rec holds md v fm =
  match fm with
    False -> false
  | True -> true
  | Atom(R(r,args)) -> predicate(md) r (map (termval md v) args)
  | Not(p) -> not(holds md v p)
  | And(p,q) -> (holds md v p) & (holds md v q)
  | Or(p,q) -> (holds md v p) or (holds md v q)
  | Imp(p,q) -> not(holds md v p) or (holds md v q)
  | Iff(p,q) -> (holds md v p = holds md v q)
  | Forall(x,p) ->
        forall (fun a -> holds md ((x |-> a) v) p) (domain md)
  | Exists(x,p) ->
        exists (fun a -> holds md ((x |-> a) v) p) (domain md);;

(* ------------------------------------------------------------------------- *)
(* Examples of particular interpretations.                                   *)
(* ------------------------------------------------------------------------- *)

let bool_interp =
  let fns f args =
    match (f,args) with
      ("0",[]) -> false
    | ("1",[]) -> true
    | ("+",[x;y]) -> not(x = y)
    | ("*",[x;y]) -> x & y
    | _ -> failwith "uninterpreted function"
  and prs p args =
    match (p,args) with
      ("=",[x;y]) -> x = y
    | _ -> failwith "uninterpreted predicate" in
  Interp([false; true],fns,prs);;

let mod_interp n =
  let fns f args =
    match (f,args) with
      ("0",[]) -> 0
    | ("1",[]) -> 1 mod n
    | ("+",[x;y]) -> (x + y) mod n
    | ("*",[x;y]) -> (x * y) mod n
    | _ -> failwith "uninterpreted function"
  and prs p args =
    match (p,args) with
      ("=",[x;y]) -> x = y
    | _ -> failwith "uninterpreted predicate" in
  Interp(0 -- (n - 1),fns,prs);;

START_INTERACTIVE;;
let fm1 = <<forall x. (x = 0) \/ (x = 1)>>;;

holds bool_interp undefined fm1;;

holds (mod_interp 2) undefined fm1;;

holds (mod_interp 3) undefined fm1;;

let fm2 = <<forall x. ~(x = 0) ==> exists y. x * y = 1>>;;

holds bool_interp undefined fm2;;

holds (mod_interp 2) undefined fm2;;

holds (mod_interp 3) undefined fm2;;

holds (mod_interp 4) undefined fm2;;

holds (mod_interp 31) undefined fm2;;

holds (mod_interp 33) undefined fm2;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Free variables in terms and formulas.                                     *)
(* ------------------------------------------------------------------------- *)

let rec fvt tm =
  match tm with
    Var x -> [x]
  | Fn(f,args) -> itlist (union ** fvt) args [];;

let rec fv fm =
  match fm with
    False -> []
  | True -> []
  | Atom(R(p,args)) -> itlist (union ** fvt) args []
  | Not(p) -> fv p
  | And(p,q) -> union (fv p) (fv q)
  | Or(p,q) -> union (fv p) (fv q)
  | Imp(p,q) -> union (fv p) (fv q)
  | Iff(p,q) -> union (fv p) (fv q)
  | Forall(x,p) -> subtract (fv p) [x]
  | Exists(x,p) -> subtract (fv p) [x];;

(* ------------------------------------------------------------------------- *)
(* Substitution within terms.                                                *)
(* ------------------------------------------------------------------------- *)

let instantiate vlist tlist =
  itlist2 (fun x t -> x |-> t) vlist tlist undefined;;

let rec termsubst sfn tm =
  match tm with
    Var x -> tryapplyd sfn x tm
  | Fn(f,args) -> Fn(f,map (termsubst sfn) args);;

(* ------------------------------------------------------------------------- *)
(* Incorrect substitution in formulas, and example showing why it's wrong.   *)
(* ------------------------------------------------------------------------- *)

let rec formsubst subfn fm =    (* WRONG! *)
  match fm with
    False -> False
  | True -> True
  | Atom(R(p,args)) -> Atom(R(p,map (termsubst subfn) args))
  | Not(p) -> Not(formsubst subfn p)
  | And(p,q) -> And(formsubst subfn p,formsubst subfn q)
  | Or(p,q) -> Or(formsubst subfn p,formsubst subfn q)
  | Imp(p,q) -> Imp(formsubst subfn p,formsubst subfn q)
  | Iff(p,q) -> Iff(formsubst subfn p,formsubst subfn q)
  | Forall(x,p) -> Forall(x,formsubst (undefine x subfn) p)
  | Exists(x,p) -> Exists(x,formsubst (undefine x subfn) p);;

START_INTERACTIVE;;
formsubst ("y" := Var "x") <<forall x. x = y>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Variant function and examples.                                            *)
(* ------------------------------------------------------------------------- *)

let rec variant x vars =
  if mem x vars then variant (x^"'") vars else x;;

START_INTERACTIVE;;
variant "x" ["y"; "z"];;

variant "x" ["x"; "y"];;

variant "x" ["x"; "x'"];;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* The correct version.                                                      *)
(* ------------------------------------------------------------------------- *)

let rec formsubst subfn fm =
  match fm with
    False -> False
  | True -> True
  | Atom(R(p,args)) -> Atom(R(p,map (termsubst subfn) args))
  | Not(p) -> Not(formsubst subfn p)
  | And(p,q) -> And(formsubst subfn p,formsubst subfn q)
  | Or(p,q) -> Or(formsubst subfn p,formsubst subfn q)
  | Imp(p,q) -> Imp(formsubst subfn p,formsubst subfn q)
  | Iff(p,q) -> Iff(formsubst subfn p,formsubst subfn q)
  | Forall(x,p) -> formsubstq subfn (fun (x,p) -> Forall(x,p)) (x,p)
  | Exists(x,p) -> formsubstq subfn (fun (x,p) -> Exists(x,p)) (x,p)

and formsubstq subfn quant (x,p) =
  let subfn' = undefine x subfn in
  let x' = if exists
             (fun y -> mem x (fvt(tryapplyd subfn' y (Var y))))
             (subtract (fv p) [x])
           then variant x (fv(formsubst subfn' p)) else x in
  quant(x',formsubst ((x |-> Var x') subfn) p);;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
formsubst ("y" := Var "x") <<forall x. x = y>>;;

formsubst ("y" := Var "x") <<forall x x'. x = y ==> x = x'>>;;
END_INTERACTIVE;;
