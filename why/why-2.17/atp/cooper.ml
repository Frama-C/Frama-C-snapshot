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
(* Cooper's algorithm for Presburger arithmetic.                             *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Lift operations up to numerals.                                           *)
(* ------------------------------------------------------------------------- *)

let mk_numeral n = Fn(string_of_num n,[]);;

let dest_numeral =
  function (Fn(ns,[])) -> num_of_string ns
         | _ -> failwith "dest_numeral";;

let is_numeral = can dest_numeral;;

let numeral1 fn n = mk_numeral(fn(dest_numeral n));;

let numeral2 fn m n = mk_numeral(fn (dest_numeral m) (dest_numeral n));;

(* ------------------------------------------------------------------------- *)
(* Operations on canonical linear terms c1 * x1 + ... + cn * xn + k          *)
(*                                                                           *)
(* Note that we're quite strict: the ci must be present even if 1            *)
(* (but if 0 we expect the monomial to be omitted) and k must be there       *)
(* even if it's zero. Thus, it's a constant iff not an addition term.        *)
(* ------------------------------------------------------------------------- *)

let rec linear_cmul n tm =
  if n =/ Int 0 then Fn("0",[]) else
  match tm with
    Fn("+",[Fn("*",[c1; x1]); rest]) ->
        Fn("+",[Fn("*",[numeral1 (( */ ) n) c1; x1]);
                        linear_cmul n rest])
  | k -> numeral1 (( */ ) n) k;;

let earlierv vars (Var x) (Var y) = earlier vars x y;;

let rec linear_add vars tm1 tm2 =
  match (tm1,tm2) with
   (Fn("+",[Fn("*",[c1; x1]); rest1]),
    Fn("+",[Fn("*",[c2; x2]); rest2])) ->
        if x1 = x2 then
          let c = numeral2 (+/) c1 c2 in
          if c = Fn("0",[]) then linear_add vars rest1 rest2
          else Fn("+",[Fn("*",[c; x1]); linear_add vars rest1 rest2])
        else if earlierv vars x1 x2 then
          Fn("+",[Fn("*",[c1; x1]); linear_add vars rest1 tm2])
        else
          Fn("+",[Fn("*",[c2; x2]); linear_add vars tm1 rest2])
  | (Fn("+",[Fn("*",[c1; x1]); rest1]),_) ->
        Fn("+",[Fn("*",[c1; x1]); linear_add vars rest1 tm2])
  | (_,Fn("+",[Fn("*",[c2; x2]); rest2])) ->
        Fn("+",[Fn("*",[c2; x2]); linear_add vars tm1 rest2])
  | _ -> numeral2 (+/) tm1 tm2;;

let linear_neg tm = linear_cmul (Int(-1)) tm;;

let linear_sub vars tm1 tm2 = linear_add vars tm1 (linear_neg tm2);;

(* ------------------------------------------------------------------------- *)
(* Linearize a term.                                                         *)
(* ------------------------------------------------------------------------- *)

let rec lint vars tm =
  match tm with
    Var x -> Fn("+",[Fn("*",[Fn("1",[]); tm]); Fn("0",[])])
  | Fn("-",[t]) -> linear_neg (lint vars t)
  | Fn("+",[s;t]) -> linear_add vars (lint vars s) (lint vars t)
  | Fn("-",[s;t]) -> linear_sub vars (lint vars s) (lint vars t)
  | Fn("*",[s;t]) ->
        let s' = lint vars s and t' = lint vars t in
        if is_numeral s' then linear_cmul (dest_numeral s') t'
        else if is_numeral t' then linear_cmul (dest_numeral t') s'
        else failwith "lint: apparent nonlinearity"
  | _ -> if is_numeral tm then tm else failwith "lint: unknown term";;

(* ------------------------------------------------------------------------- *)
(* Linearize the atoms in a formula, and eliminate non-strict inequalities.  *)
(* ------------------------------------------------------------------------- *)

let mkatom vars p t = Atom(R(p,[Fn("0",[]);lint vars t]));;

let linform vars fm =
  match fm with
    Atom(R("divides",[c;t])) ->
        let c' = mk_numeral(abs_num(dest_numeral c)) in
        Atom(R("divides",[c';lint vars t]))
  | Atom(R("=",[s;t])) -> mkatom vars "=" (Fn("-",[t;s]))
  | Atom(R("<",[s;t])) -> mkatom vars "<" (Fn("-",[t;s]))
  | Atom(R(">",[s;t])) -> mkatom vars "<" (Fn("-",[s;t]))
  | Atom(R("<=",[s;t])) ->
        mkatom vars "<" (Fn("-",[Fn("+",[t;Fn("1",[])]);s]))
  | Atom(R(">=",[s;t])) ->
        mkatom vars "<" (Fn("-",[Fn("+",[s;Fn("1",[])]);t]))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Post-NNF transformation eliminating negated inequalities.                 *)
(* ------------------------------------------------------------------------- *)

let rec posineq fm =
  match fm with
  | Not(Atom(R("<",[Fn("0",[]); t]))) ->
        Atom(R("<",[Fn("0",[]); linear_sub [] (Fn("1",[])) t]))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Find the LCM of the coefficients of x.                                    *)
(* ------------------------------------------------------------------------- *)

let rec formlcm x fm =
  match fm with
    Atom(R(p,[_;Fn("+",[Fn("*",[c;y]);z])])) when y = x ->
        abs_num(dest_numeral c)
  | Not(p) -> formlcm x p
  | And(p,q) -> lcm_num (formlcm x p) (formlcm x q)
  | Or(p,q) -> lcm_num (formlcm x p) (formlcm x q)
  | _ -> Int 1;;

(* ------------------------------------------------------------------------- *)
(* Adjust all coefficients of x in formula; fold in reduction to +/- 1.      *)
(* ------------------------------------------------------------------------- *)

let rec adjustcoeff x l fm =
  match fm with
    Atom(R(p,[d; Fn("+",[Fn("*",[c;y]);z])])) when y = x ->
        let m = l // dest_numeral c in
        let n = if p = "<" then abs_num(m) else m in
        let xtm = Fn("*",[mk_numeral(m // n); x]) in
        Atom(R(p,[linear_cmul (abs_num m) d;
                Fn("+",[xtm; linear_cmul n z])]))
  | Not(p) -> Not(adjustcoeff x l p)
  | And(p,q) -> And(adjustcoeff x l p,adjustcoeff x l q)
  | Or(p,q) -> Or(adjustcoeff x l p,adjustcoeff x l q)
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Hence make coefficient of x one in existential formula.                   *)
(* ------------------------------------------------------------------------- *)

let unitycoeff x fm =
  let l = formlcm x fm in
  let fm' = adjustcoeff x l fm in
  if l =/ Int 1 then fm' else
  let xp = Fn("+",[Fn("*",[Fn("1",[]);x]); Fn("0",[])]) in
  And(Atom(R("divides",[mk_numeral l; xp])),adjustcoeff x l fm);;

(* ------------------------------------------------------------------------- *)
(* The "minus infinity" version.                                             *)
(* ------------------------------------------------------------------------- *)

let rec minusinf x fm =
  match fm with
    Atom(R("=",[Fn("0",[]); Fn("+",[Fn("*",[Fn("1",[]);y]);z])]))
        when y = x -> False
  | Atom(R("<",[Fn("0",[]); Fn("+",[Fn("*",[pm1;y]);z])])) when y = x ->
        if pm1 = Fn("1",[]) then False else True
  | Not(p) -> Not(minusinf x p)
  | And(p,q) -> And(minusinf x p,minusinf x q)
  | Or(p,q) -> Or(minusinf x p,minusinf x q)
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* The LCM of all the divisors that involve x.                               *)
(* ------------------------------------------------------------------------- *)

let rec divlcm x fm =
  match fm with
    Atom(R("divides",[d;Fn("+",[Fn("*",[c;y]);z])])) when y = x ->
        dest_numeral d
  | Not(p) -> divlcm x p
  | And(p,q) -> lcm_num (divlcm x p) (divlcm x q)
  | Or(p,q) -> lcm_num (divlcm x p) (divlcm x q)
  | _ -> Int 1;;

(* ------------------------------------------------------------------------- *)
(* Construct the B-set.                                                      *)
(* ------------------------------------------------------------------------- *)

let rec bset x fm =
  match fm with
    Not(Atom(R("=",[Fn("0",[]); Fn("+",[Fn("*",[Fn("1",[]);y]);a])])))
    when y = x -> [linear_neg a]
  | Atom(R("=",[Fn("0",[]); Fn("+",[Fn("*",[Fn("1",[]);y]);a])]))
    when y = x -> [linear_neg(linear_add [] a (Fn("1",[])))]
  | Atom(R("<",[Fn("0",[]); Fn("+",[Fn("*",[Fn("1",[]);y]);a])]))
    when y = x -> [linear_neg a]
  | Not(p) -> bset x p
  | And(p,q) -> union (bset x p) (bset x q)
  | Or(p,q) -> union (bset x p) (bset x q)
  | _ -> [];;

(* ------------------------------------------------------------------------- *)
(* Replace top variable with another linear form, retaining canonicality.    *)
(* ------------------------------------------------------------------------- *)

let rec linrep vars x t fm =
  match fm with
    Atom(R(p,[d; Fn("+",[Fn("*",[c;y]);z])])) when y = x ->
        let ct = linear_cmul (dest_numeral c) t in
        Atom(R(p,[d; linear_add vars ct z]))
  | Not(p) -> Not(linrep vars x t p)
  | And(p,q) -> And(linrep vars x t p,linrep vars x t q)
  | Or(p,q) -> Or(linrep vars x t p,linrep vars x t q)
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Evaluation of constant expressions.                                       *)
(* ------------------------------------------------------------------------- *)

let operations =
  ["=",(=/); "<",(</); ">",(>/); "<=",(<=/); ">=",(>=/);
   "divides",(fun x y -> mod_num y x =/ Int 0)];;

let evalc_atom at =
  match at with
    R(p,[s;t]) ->
        (try if assoc p operations (dest_numeral s) (dest_numeral t)
             then True else False
         with Failure _ -> Atom at)
  | _ -> Atom at;;

let evalc = onatoms evalc_atom;;

(* ------------------------------------------------------------------------- *)
(* Hence the core quantifier elimination procedure.                          *)
(* ------------------------------------------------------------------------- *)

let cooper vars fm =
  match fm with
   Exists(x0,p0) ->
        let x = Var x0 in
        let p = unitycoeff x p0 in
        let p_inf = simplify(minusinf x p) and bs = bset x p
        and js = Int 1 --- divlcm x p in
        let p_element j b =
          linrep vars x (linear_add vars b (mk_numeral j)) p in
        let stage j = list_disj
           (linrep vars x (mk_numeral j) p_inf ::
            map (p_element j) bs) in
        list_disj (map stage js)
  | _ -> failwith "cooper: not an existential formula";;

(* ------------------------------------------------------------------------- *)
(* Overall function.                                                         *)
(* ------------------------------------------------------------------------- *)

let integer_qelim =
  simplify ** evalc **
  lift_qelim linform (cnnf posineq ** evalc) cooper;;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
integer_qelim <<forall x y. x < y ==> 2 * x + 1 < 2 * y>>;;

integer_qelim <<forall x y. ~(2 * x + 1 = 2 * y)>>;;

integer_qelim <<exists x y. x > 0 /\ y >= 0 /\ 3 * x - 5 * y = 1>>;;

integer_qelim <<exists x y z. 4 * x - 6 * y = 1>>;;

integer_qelim <<forall x. b < x ==> a <= x>>;;

integer_qelim <<forall x. a < 3 * x ==> b < 3 * x>>;;

time integer_qelim <<forall x y. x <= y ==> 2 * x + 1 < 2 * y>>;;

time integer_qelim <<(exists d. y = 65 * d) ==> (exists d. y = 5 * d)>>;;

time integer_qelim
  <<forall y. (exists d. y = 65 * d) ==> (exists d. y = 5 * d)>>;;

time integer_qelim <<forall x y. ~(2 * x + 1 = 2 * y)>>;;

time integer_qelim
  <<forall x y z. (2 * x + 1 = 2 * y) ==> x + y + z > 129>>;;

time integer_qelim <<forall x. a < x ==> b < x>>;;

time integer_qelim <<forall x. a <= x ==> b < x>>;;

(* ------------------------------------------------------------------------- *)
(* Formula examples from Cooper's paper.                                     *)
(* ------------------------------------------------------------------------- *)

time integer_qelim <<forall a b. exists x. a < 20 * x /\ 20 * x < b>>;;

time integer_qelim <<exists x. a < 20 * x /\ 20 * x < b>>;;

time integer_qelim <<forall b. exists x. a < 20 * x /\ 20 * x < b>>;;

time integer_qelim
  <<forall a. exists b. a < 4 * b + 3 * a \/ (~(a < b) /\ a > b + 1)>>;;

time integer_qelim
  <<exists y. forall x. x + 5 * y > 1 /\ 13 * x - y > 1 /\ x + 2 < 0>>;;

(* ------------------------------------------------------------------------- *)
(* Some more.                                                                *)
(* ------------------------------------------------------------------------- *)

time integer_qelim <<forall x y. x >= 0 /\ y >= 0
                  ==> 12 * x - 8 * y < 0 \/ 12 * x - 8 * y > 2>>;;

time integer_qelim <<exists x y. 5 * x + 3 * y = 1>>;;

time integer_qelim <<exists x y. 5 * x + 10 * y = 1>>;;

time integer_qelim <<exists x y. x >= 0 /\ y >= 0 /\ 5 * x - 6 * y = 1>>;;


time integer_qelim <<exists w x y z. 2 * w + 3 * x + 4 * y + 5 * z = 1>>;;

time integer_qelim <<exists x y. x >= 0 /\ y >= 0 /\ 5 * x - 3 * y = 1>>;;

time integer_qelim <<exists x y. x >= 0 /\ y >= 0 /\ 3 * x - 5 * y = 1>>;;

time integer_qelim <<exists x y. x >= 0 /\ y >= 0 /\ 6 * x - 3 * y = 1>>;;

time integer_qelim
  <<forall x y. ~(x = 0) ==> 5 * y < 6 * x \/ 5 * y > 6 * x>>;;

time integer_qelim
  <<forall x y. ~divides(5,x) /\ ~divides(6,y) ==> ~(6 * x = 5 * y)>>;;

time integer_qelim <<forall x y. ~divides(5,x) ==> ~(6 * x = 5 * y)>>;;

time integer_qelim <<forall x y. ~(6 * x = 5 * y)>>;;

time integer_qelim <<forall x y. 6 * x = 5 * y ==> exists d. y = 3 * d>>;;

time integer_qelim <<6 * x = 5 * y ==> exists d. y = 3 * d>>;;

(* ------------------------------------------------------------------------- *)
(* Positive variant of the Bezout theorem.                                   *)
(* ------------------------------------------------------------------------- *)

time integer_qelim
  <<forall z. z > 7 ==> exists x y. x >= 0 /\ y >= 0 /\ 3 * x + 5 * y = z>>;;

time integer_qelim
  <<forall z. z > 2 ==> exists x y. x >= 0 /\ y >= 0 /\ 3 * x + 5 * y = z>>;;

time integer_qelim
  <<forall z.
        z <= 7
        ==> ((exists x y. x >= 0 /\ y >= 0 /\ 3 * x + 5 * y = z) <=>
             ~(exists x y. x >= 0 /\ y >= 0 /\ 3 * x + 5 * y = 7 - z))>>;;

(* ------------------------------------------------------------------------- *)
(* Basic result about congruences.                                           *)
(* ------------------------------------------------------------------------- *)

time integer_qelim
  <<forall x. ~divides(2,x) /\ divides(3,x-1) <=>
              divides(12,x-1) \/ divides(12,x-7)>>;;

time integer_qelim
  <<forall x. ~(exists m. x = 2 * m) /\ (exists m. x = 3 * m + 1) <=>
              (exists m. x = 12 * m + 1) \/ (exists m. x = 12 * m + 7)>>;;

(* ------------------------------------------------------------------------- *)
(* Something else.                                                           *)
(* ------------------------------------------------------------------------- *)

time integer_qelim
 <<forall x.
        ~(divides(2,x))
        ==> divides(4,x-1) \/
            divides(8,x-1) \/
            divides(8,x-3) \/
            divides(6,x-1) \/
            divides(14,x-1) \/
            divides(14,x-9) \/
            divides(14,x-11) \/
            divides(24,x-5) \/
            divides(24,x-11)>>;;

(* ------------------------------------------------------------------------- *)
(* Testing fix for an earlier version.                                       *)
(* ------------------------------------------------------------------------- *)

(integer_qelim ** generalize)
  <<a + 2 = b /\ v_3 = b - a + 1 /\ v_2 = b - 2 /\ v_1 = 3 ==> false>>;;

(* ------------------------------------------------------------------------- *)
(* Inspired by the Collatz conjecture.                                       *)
(* ------------------------------------------------------------------------- *)

integer_qelim
  <<exists a b. ~(a = 1) /\ ((2 * b = a) \/ (2 * b = 3 * a + 1)) /\ (a = b)>>;;

integer_qelim
 <<exists a b. a > 1 /\ b > 1 /\
               ((2 * b = a) \/ (2 * b = 3 * a + 1)) /\
               (a = b)>>;;

END_INTERACTIVE;;
