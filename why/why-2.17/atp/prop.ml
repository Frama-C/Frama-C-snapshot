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
(* Basic stuff for propositional logic: datatype, parsing and printing.      *)
(*                                                                           *)
(* Copyright (c) 2003, John Harrison. (See "LICENSE.txt" for details.)       *)
(* ========================================================================= *)

type prop = P of string;;

let pname(P s) = s;;

(* ------------------------------------------------------------------------- *)
(* Parsing of propositional formulas.                                        *)
(* ------------------------------------------------------------------------- *)

let parse_propvar vs inp =
  match inp with
    p::oinp when p <> "(" -> Atom(P(p)),oinp
  | _ -> failwith "parse_propvar";;

let parsep = make_parser (parse_formula parse_propvar []);;

(* ------------------------------------------------------------------------- *)
(* Set this up as default for quotations.                                    *)
(* ------------------------------------------------------------------------- *)

let default_parser = parsep;;

(* ------------------------------------------------------------------------- *)
(* Test of the parser.                                                       *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = parsep "p ==> q <=> r /\\ s \\/ (t <=> ~ ~u /\\ v)";;

let fm = <<p ==> q <=> r /\ s \/ (t <=> ~ ~u /\ v)>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Printer.                                                                  *)
(* ------------------------------------------------------------------------- *)

let print_propvar prec p = print_string(pname p);;

let pr = formula_printer print_propvar;;

START_INTERACTIVE;;
#install_printer pr;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Testing the printer.                                                      *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
And(fm,fm);;

And(Or(fm,fm),fm);;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Interpretation of formulas.                                               *)
(* ------------------------------------------------------------------------- *)

let rec eval fm v =
  match fm with
    False -> false
  | True -> true
  | Atom(x) -> v(x)
  | Not(p) -> not(eval p v)
  | And(p,q) -> (eval p v) & (eval q v)
  | Or(p,q) -> (eval p v) or (eval q v)
  | Imp(p,q) -> not(eval p v) or (eval q v)
  | Iff(p,q) -> (eval p v) = (eval q v);;

(* ------------------------------------------------------------------------- *)
(* Example of how we could define connective interpretations ourselves.      *)
(* ------------------------------------------------------------------------- *)

let (-->) p q = match (p,q) with (true,false) -> false | _ -> true;;

let rec eval fm v =
  match fm with
    False -> false
  | True -> true
  | Atom(x) -> v(x)
  | Not(p) -> not(eval p v)
  | And(p,q) -> (eval p v) & (eval q v)
  | Or(p,q) -> (eval p v) or (eval q v)
  | Imp(p,q) -> eval p v --> eval q v
  | Iff(p,q) -> (eval p v) = (eval q v);;

(* ------------------------------------------------------------------------- *)
(* Example of use, showing the "partial" evaluation.                         *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<p /\ q ==> q /\ r>>;;

let fm_interp = eval fm;;

eval fm (function P"p" -> true | P"q" -> false | P"r" -> true);;

eval fm (function P"p" -> true | P"q" -> true | P"r" -> false);;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Return the set of propositional variables in a formula.                   *)
(* ------------------------------------------------------------------------- *)

let atoms fm = atom_union (fun a -> [a]) fm;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
atoms <<p /\ q \/ s ==> ~p \/ (r <=> s)>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Code to print out truth tables.                                           *)
(* ------------------------------------------------------------------------- *)

let rec onallvaluations subfn v pvs =
  match pvs with
    [] -> subfn v
  | p::ps -> let v' t q = if q = p then t else v(q) in
             onallvaluations subfn (v' false) ps &
             onallvaluations subfn (v' true) ps;;

let print_truthtable fm =
  let pvs = atoms fm in
  let width = itlist (max ** String.length ** pname) pvs 5 + 1 in
  let fixw s = s^String.make(width - String.length s) ' ' in
  let truthstring p = fixw (if p then "true" else "false") in
  let mk_row v =
     let lis = map (fun x -> truthstring(v x)) pvs
     and ans = truthstring(eval fm v) in
     print_string(itlist (^) lis ("| "^ans)); print_newline();
     true in
  let separator = String.make (width * length pvs + 9) '-' in
  print_string(itlist (fun s t -> fixw(pname s) ^ t) pvs "| formula");
  print_newline(); print_string separator; print_newline();
  onallvaluations mk_row (fun x -> false) pvs;
  print_string separator; print_newline();;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<p /\ q ==> q /\ r>>;;

print_truthtable fm;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Additional examples illustrating formula classes.                         *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
print_truthtable <<((p ==> q) ==> p) ==> p>>;;

print_truthtable <<p /\ ~p>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Recognizing tautologies.                                                  *)
(* ------------------------------------------------------------------------- *)

let tautology fm =
  onallvaluations (eval fm) (fun s -> false) (atoms fm);;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
tautology <<p \/ ~p>>;;
tautology <<p \/ q ==> p>>;;
tautology <<p \/ q ==> q \/ (p <=> q)>>;;
tautology <<(p \/ q) /\ ~(p /\ q) ==> (~p <=> q)>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Related concepts.                                                         *)
(* ------------------------------------------------------------------------- *)

let unsatisfiable fm = tautology(Not fm);;

let satisfiable fm = not(unsatisfiable fm);;

(* ------------------------------------------------------------------------- *)
(* Substitution operation.                                                   *)
(* ------------------------------------------------------------------------- *)

let propsubst subfn = onatoms (fun p -> tryapplyd subfn p (Atom p));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let pandq = <<p /\ q>>;;

propsubst (P"p" := pandq) pandq;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Surprising tautologies including Dijkstra's "Golden rule".                *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
tautology <<(p ==> q) \/ (q ==> p)>>;;

tautology <<p \/ (q <=> r) <=> (p \/ q <=> p \/ r)>>;;

tautology <<p /\ q <=> ((p <=> q) <=> p \/ q)>>;;

(* ------------------------------------------------------------------------- *)
(* Some logical equivalences allowing elimination of connectives.            *)
(* ------------------------------------------------------------------------- *)

tautology <<false <=> p /\ ~p>>;;
tautology <<true <=> ~(p /\ ~p)>>;;
tautology <<p \/ q <=> ~(~p /\ ~q)>>;;
tautology <<p ==> q <=> ~(p /\ ~q)>>;;
tautology <<(p <=> q) <=> ~(p /\ ~q) /\ ~(~p /\ q)>>;;

tautology <<true <=> false ==> false>>;;
tautology <<~p <=> p ==> false>>;;
tautology <<p /\ q <=> (p ==> q ==> false) ==> false>>;;
tautology <<p \/ q <=> (p ==> false) ==> q>>;;
tautology(parsep
  "(p <=> q) <=> ((p ==> q) ==> (q ==> p) ==> false) ==> false");;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Dualization.                                                              *)
(* ------------------------------------------------------------------------- *)

let rec subdualize fm =
  match fm with
    False -> True
  | True -> False
  | Atom(p) -> fm
  | Not(p) -> Not(subdualize p)
  | And(p,q) -> Or(subdualize p,subdualize q)
  | Or(p,q) -> And(subdualize p,subdualize q)
  | _ -> failwith "Formula involves connectives ==> and <=>";;

let dualize fm = Not(subdualize fm);;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
dualize <<p \/ ~p>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Routine simplification.                                                   *)
(* ------------------------------------------------------------------------- *)

let psimplify1 fm =
  match fm with
    Not False -> True
  | Not True -> False
  | And(False,q) -> False
  | And(p,False) -> False
  | And(True,q) -> q
  | And(p,True) -> p
  | Or(False,q) -> q
  | Or(p,False) -> p
  | Or(True,q) -> True
  | Or(p,True) -> True
  | Imp(False,q) -> True
  | Imp(True,q) -> q
  | Imp(p,True) -> True
  | Imp(p,False) -> Not p
  | Iff(True,q) -> q
  | Iff(p,True) -> p
  | Iff(False,q) -> Not q
  | Iff(p,False) -> Not p
  | _ -> fm;;

let rec psimplify fm =
  match fm with
  | Not p -> psimplify1 (Not(psimplify p))
  | And(p,q) -> psimplify1 (And(psimplify p,psimplify q))
  | Or(p,q) -> psimplify1 (Or(psimplify p,psimplify q))
  | Imp(p,q) -> psimplify1 (Imp(psimplify p,psimplify q))
  | Iff(p,q) -> psimplify1 (Iff(psimplify p,psimplify q))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
psimplify <<(true ==> (x <=> false)) ==> ~(y \/ false /\ z)>>;;

psimplify <<((x ==> y) ==> true) \/ ~false>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Negation normal form.                                                     *)
(* ------------------------------------------------------------------------- *)

let rec nnf fm =
  match fm with
  | And(p,q) -> And(nnf p,nnf q)
  | Or(p,q) -> Or(nnf p,nnf q)
  | Imp(p,q) -> Or(nnf(Not p),nnf q)
  | Iff(p,q) -> Or(And(nnf p,nnf q),And(nnf(Not p),nnf(Not q)))
  | Not(Not p) -> nnf p
  | Not(And(p,q)) -> Or(nnf(Not p),nnf(Not q))
  | Not(Or(p,q)) -> And(nnf(Not p),nnf(Not q))
  | Not(Imp(p,q)) -> And(nnf p,nnf(Not q))
  | Not(Iff(p,q)) -> Or(And(nnf p,nnf(Not q)),And(nnf(Not p),nnf q))
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Side remark on possible alternative tautology.                            *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
tautology <<(p <=> q) <=> (p \/ ~q) /\ (~p \/ q)>>;;

(* ------------------------------------------------------------------------- *)
(* Example of NNF function in action.                                        *)
(* ------------------------------------------------------------------------- *)

let fm = <<(p <=> q) <=> ~(r ==> s)>>;;

let fm' = nnf fm;;

tautology(Iff(fm,fm'));;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* More efficient version.                                                   *)
(* ------------------------------------------------------------------------- *)

let rec nnfp fm =
  match fm with
  | Not(p) ->
        let p',p'' = nnfp p in
        p'',p'
  | And(p,q) ->
        let p',p'' = nnfp p and q',q'' = nnfp q in
        And(p',q'),Or(p'',q'')
  | Or(p,q) ->
        let p',p'' = nnfp p and q',q'' = nnfp q in
        Or(p',q'),And(p'',q'')
  | Imp(p,q) ->
        let p',p'' = nnfp p and q',q'' = nnfp q in
        Or(p'',q'),And(p',q'')
  | Iff(p,q) ->
        let p',p'' = nnfp p and q',q'' = nnfp q in
        Or(And(p',q'),And(p'',q'')),Or(And(p',q''),And(p'',q'))
  | _ -> fm,Not fm;;

let nnf fm = fst(nnfp(psimplify fm));;

(* ------------------------------------------------------------------------- *)
(* Some tautologies remarked on.                                             *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
tautology
 <<(p ==> p') /\ (q ==> q') ==> (p \/ q ==> p' \/ q')>>;;

tautology
 <<(p ==> p') /\ (q ==> q') ==> (p /\ q ==> p' /\ q')>>;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Tracking positive and negative occurrences.                               *)
(* ------------------------------------------------------------------------- *)

let rec occurrences x fm =
  match fm with
    Atom(y) ->
        (x = y,false)
  | Not(p) ->
        let pos,neg = occurrences x p in neg,pos
  | And(p,q) ->
        let pos1,neg1 = occurrences x p
        and pos2,neg2 = occurrences x q in
        (pos1 or pos2,neg1 or neg2)
  | Or(p,q) ->
        let pos1,neg1 = occurrences x p
        and pos2,neg2 = occurrences x q in
        (pos1 or pos2,neg1 or neg2)
  | Imp(p,q) ->
        let pos1,neg1 = occurrences x p
        and pos2,neg2 = occurrences x q in
        (neg1 or pos2,pos1 or neg2)
  | Iff(p,q) ->
        let pos1,neg1 = occurrences x p
        and pos2,neg2 = occurrences x q in
        if pos1 or pos2 or neg1 or neg2 then (true,true)
        else (false,false)
  | _ -> (false,false);;

(* ------------------------------------------------------------------------- *)
(* Disjunctive normal form (DNF) via truth tables.                           *)
(* ------------------------------------------------------------------------- *)

let list_conj l =
  if l = [] then True else end_itlist (fun p q -> And(p,q)) l;;

let list_disj l =
  if l = [] then False else end_itlist (fun p q -> Or(p,q)) l;;

let mk_lits pvs v =
  list_conj (map (fun p -> if eval p v then p else Not p) pvs);;

let rec allsatvaluations subfn v pvs =
  match pvs with
    [] -> if subfn v then [v] else []
  | p::ps -> let v' t q = if q = p then t else v(q) in
             allsatvaluations subfn (v' false) ps @
             allsatvaluations subfn (v' true) ps;;

let dnf fm =
  let pvs = atoms fm in
  let satvals = allsatvaluations (eval fm) (fun s -> false) pvs in
  list_disj (map (mk_lits (map (fun p -> Atom p) pvs)) satvals);;

(* ------------------------------------------------------------------------- *)
(* Examples.                                                                 *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<(p /\ (q \/ r /\ s)) /\ (~p \/ ~q \/ ~s)>>;;

dnf fm;;

print_truthtable fm;;

let fm = <<p /\ q /\ r /\ s /\ t /\ u \/ u /\ v>>;;

dnf fm;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* DNF via distribution.                                                     *)
(* ------------------------------------------------------------------------- *)

let rec distrib fm =
  match fm with
    And(p,(Or(q,r))) -> Or(distrib(And(p,q)),distrib(And(p,r)))
  | And(Or(p,q),r) -> Or(distrib(And(p,r)),distrib(And(q,r)))
  | _ -> fm;;

let rec rawdnf fm =
  match fm with
    And(p,q) -> distrib(And(rawdnf p,rawdnf q))
  | Or(p,q) -> Or(rawdnf p,rawdnf q)
  | _ -> fm;;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
let fm = <<(p /\ (q \/ r /\ s)) /\ (~p \/ ~q \/ ~s)>>;;

rawdnf fm;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* A version using a list representation.                                    *)
(* ------------------------------------------------------------------------- *)

let distrib s1 s2 = allpairs union s1 s2;;      (** Value restriction hell!  *)
                                                (** Need it for FOL formulas *)
let rec purednf fm =
  match fm with
    And(p,q) -> distrib (purednf p) (purednf q)
  | Or(p,q) -> union (purednf p) (purednf q)
  | _ -> [[fm]];;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
purednf fm;;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Filtering out noncontradictory disjuncts only.                            *)
(* ------------------------------------------------------------------------- *)

let negative = function (Not p) -> true | _ -> false;;

let positive lit = not(negative lit);;

let negate = function (Not p) -> p | p -> Not p;;

let contradictory lits =
  let pos,neg = partition positive lits in
  intersect pos (map negate neg) <> [];;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
filter (non contradictory) (purednf fm);;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* With subsumption checking, done very naively (quadratic).                 *)
(* ------------------------------------------------------------------------- *)

let subsumes s1 s2 = psubset s2 s1;;

let subsume cls =
  filter (fun cl -> not(exists (subsumes cl) cls)) cls;;

let simpdnf fm =
  if fm = False then []
  else if fm = True then [[]]
  else subsume (filter (non contradictory) (purednf(nnf fm)));;

(* ------------------------------------------------------------------------- *)
(* Mapping back to a formula.                                                *)
(* ------------------------------------------------------------------------- *)

let dnf fm = list_disj(map list_conj (simpdnf fm));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
dnf fm;;

tautology(Iff(fm,dnf fm));;
END_INTERACTIVE;;

(* ------------------------------------------------------------------------- *)
(* Conjunctive normal form (CNF) by duality.                                 *)
(* ------------------------------------------------------------------------- *)

let purecnf fm = smap (smap negate) (purednf(nnf(Not fm)));;

let simpcnf fm = subsume (filter (non contradictory) (purecnf fm));;

let cnf fm = list_conj(map list_disj (simpcnf fm));;

(* ------------------------------------------------------------------------- *)
(* Example.                                                                  *)
(* ------------------------------------------------------------------------- *)

START_INTERACTIVE;;
cnf fm;;

cnf(Iff(fm,cnf fm));;
END_INTERACTIVE;;
