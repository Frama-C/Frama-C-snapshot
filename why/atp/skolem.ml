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
(* Prenex and Skolem normal forms.                                           *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Routine simplification. Like "psimplify" but with quantifier clauses.     *)
(* ------------------------------------------------------------------------- *)

let simplify1 fm =
  match fm with
    Forall(x,p) -> if mem x (fv p) then fm else p
  | Exists(x,p) -> if mem x (fv p) then fm else p
  | _ -> psimplify1 fm;;

let rec simplify fm =
  match fm with
    Not p -> simplify1 (Not(simplify p))
  | And(p,q) -> simplify1 (And(simplify p,simplify q))
  | Or(p,q) -> simplify1 (Or(simplify p,simplify q))
  | Imp(p,q) -> simplify1 (Imp(simplify p,simplify q))
  | Iff(p,q) -> simplify1 (Iff(simplify p,simplify q))
  | Forall(x,p) -> simplify1(Forall(x,simplify p))
  | Exists(x,p) -> simplify1(Exists(x,simplify p))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
simplify <<(forall x y. P(x) \/ (P(y) /\ false)) ==> exists z. P(z)>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Negation normal form.                                                     *)
(* ------------------------------------------------------------------------- *)

let rec nnf fm =
  match fm with
    And(p,q) -> And(nnf p,nnf q)
  | Or(p,q) -> Or(nnf p,nnf q)
  | Imp(p,q) -> Or(nnf(Not p),nnf q)
  | Iff(p,q) -> Or(And(nnf p,nnf q),And(nnf(Not p),nnf(Not q)))
  | Not(Not p) -> nnf p
  | Not(And(p,q)) -> Or(nnf(Not p),nnf(Not q))
  | Not(Or(p,q)) -> And(nnf(Not p),nnf(Not q))
  | Not(Imp(p,q)) -> And(nnf p,nnf(Not q))
  | Not(Iff(p,q)) -> Or(And(nnf p,nnf(Not q)),And(nnf(Not p),nnf q))
  | Forall(x,p) -> Forall(x,nnf p)
  | Exists(x,p) -> Exists(x,nnf p)
  | Not(Forall(x,p)) -> Exists(x,nnf(Not p))
  | Not(Exists(x,p)) -> Forall(x,nnf(Not p))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Example of NNF function in action.                                        *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<(forall x. P(x))
           ==> ((exists y. Q(y)) <=> exists z. P(z) /\ Q(z))>> in
nnf fm;;

let andrews =
 <<((exists x. forall y. P(x) <=> P(y)) <=>
    ((exists x. Q(x)) <=> (forall y. Q(y)))) <=>
   ((exists x. forall y. Q(x) <=> Q(y)) <=>
    ((exists x. P(x)) <=> (forall y. P(y))))>> in
 nnf andrews;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Prenex normal form.                                                       *)
(* ------------------------------------------------------------------------- *)

let mk_all x p = Forall(x,p) and mk_ex x p = Exists(x,p);;
let mk_and p q = And(p,q) and mk_or p q = Or(p,q);;

let rec pullquants fm =
  match fm with
    And(Forall(x,p),Forall(y,q)) -> pullquant_2 fm mk_all mk_and x y p q
  | Or(Exists(x,p),Exists(y,q)) -> pullquant_2 fm mk_ex mk_or x y p q
  | And(Forall(x,p),q) -> pullquant_l fm mk_all mk_and x p q
  | And(p,Forall(x,q)) -> pullquant_r fm mk_all mk_and x p q
  | Or(Forall(x,p),q) -> pullquant_l fm mk_all mk_or x p q
  | Or(p,Forall(x,q)) -> pullquant_r fm mk_all mk_or x p q
  | And(Exists(x,p),q) -> pullquant_l fm mk_ex mk_and x p q
  | And(p,Exists(x,q)) -> pullquant_r fm mk_ex mk_and x p q
  | Or(Exists(x,p),q) -> pullquant_l fm mk_ex mk_or x p q
  | Or(p,Exists(x,q)) -> pullquant_r fm mk_ex mk_or x p q
  | _ -> fm

and pullquant_l fm quant op x p q =
  let x' = variant x (fv fm) in
  quant x' (pullquants(op (formsubst (x := Var x') p) q))

and pullquant_r fm quant op x p q =
  let x' = variant x (fv fm) in
  quant x' (pullquants(op p (formsubst (x := Var x') q)))

and pullquant_2 fm quant op x y p q =
  let x' = variant x (fv fm) in
  quant x' (pullquants(op (formsubst (x := Var x') p)
                          (formsubst (y := Var x') q)));;

let rec prenex fm =
  match fm with
    Forall(x,p) -> Forall(x,prenex p)
  | Exists(x,p) -> Exists(x,prenex p)
  | And(p,q) -> pullquants(And(prenex p,prenex q))
  | Or(p,q) -> pullquants(Or(prenex p,prenex q))
  | _ -> fm;;

let pnf fm = prenex(nnf(simplify fm));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
pnf <<forall x. P(x) ==> exists y z. Q(y) \/ ~(exists z. P(z) /\ Q(z))>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Get the functions in a term and formula.                                  *)
(* ------------------------------------------------------------------------- *)

let rec funcs tm = match tm with
    Var x -> []
  | Fn(f,args) -> itlist (union ** funcs) args [f,length args];;

let functions fm =
  atom_union (fun (R(p,a)) -> itlist (union ** funcs) a []) fm;;

(* ------------------------------------------------------------------------- *)
(* Core Skolemization function.                                              *)
(* ------------------------------------------------------------------------- *)

let rec skolem fm corr =
  match fm with
    Exists(y,p) ->
        let xs = fv(fm)
        and fns = map (fun (Fn(f,args),def) -> f) corr in
        let f = variant (if xs = [] then "c_"^y else "f_"^y) fns in
        let fx = Fn(f,map (fun x -> Var x) xs) in
        skolem (formsubst (y := fx) p) ((fx,fm)::corr)
  | Forall(x,p) -> let p',corr' = skolem p corr in Forall(x,p'),corr'
  | And(p,q) -> skolem2 (fun (p,q) -> And(p,q)) (p,q) corr
  | Or(p,q) -> skolem2 (fun (p,q) -> Or(p,q)) (p,q) corr
  | _ -> fm,corr

and skolem2 cons (p,q) corr =
  let p',corr' = skolem p corr in
  let q',corr'' = skolem q corr' in
  cons(p',q'),corr'';;

(* ------------------------------------------------------------------------- *)
(* Overall Skolemization function.                                           *)
(* ------------------------------------------------------------------------- *)

let askolemize fm =
  let fm1 = nnf(simplify fm) in
  let corr = map (fun (n,a) -> Fn(n,[]),False) (functions fm1) in
  fst(skolem fm1 corr);;

let rec specialize fm =
  match fm with
    Forall(x,p) -> specialize p
  | _ -> fm;;

let skolemize fm = specialize(pnf(askolemize fm));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
skolemize <<exists y. x < y ==> forall u. exists v. x * u < y * v>>;;

skolemize
 <<forall x. P(x)
             ==> (exists y z. Q(y) \/ ~(exists z. P(z) /\ Q(z)))>>;;
END_INTERACTIVE;;
