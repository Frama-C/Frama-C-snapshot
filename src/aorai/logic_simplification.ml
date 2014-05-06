(**************************************************************************)
(*                                                                        *)
(*  This file is part of Aorai plug-in of Frama-C.                        *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
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

open Cil_types
open Promelaast

let pretty_clause fmt l =
  Format.fprintf fmt "@[<2>[%a@]]@\n"
    (Pretty_utils.pp_list ~sep:",@ " Promelaoutput.print_condition) l

let pretty_dnf fmt l =
  Format.fprintf fmt "@[<2>[%a@]]@\n"
    (Pretty_utils.pp_list pretty_clause) l

let opposite_rel = 
  function
    | Rlt -> Rge
    | Rgt -> Rle
    | Rge -> Rlt
    | Rle -> Rgt
    | Req -> Rneq
    | Rneq -> Req

let rec condToDNF cond = 
  (*Typage : condition --> liste de liste de termes (disjonction de conjonction de termes)
    DNF(terme)   = {{terme}}
    DNF(a or b)  = DNF(a) \/ DNF(b) 
    DNF(a and b) = Composition (DNF(a),DNF(b)) 
    DNF(not a)   = tmp = DNF(a) 
                   composition (tmp) 
                   negation de chaque terme 
  *)
  match cond with
    | TOr  (c1, c2) -> (condToDNF c1)@(condToDNF c2)
    | TAnd (c1, c2) -> 
	let d1,d2=(condToDNF c1), (condToDNF c2) in
	List.fold_left 
	  (fun lclause clauses2 -> 
	     (List.map (fun clauses1 -> clauses1@clauses2) d1) @ lclause
	  )
	  [] d2
    | TNot (c) -> 
	begin
	  match c with
	    | TOr  (c1, c2) -> condToDNF (TAnd(TNot(c1),TNot(c2)))
	    | TAnd (c1, c2) -> condToDNF (TOr (TNot(c1),TNot(c2)))
	    | TNot (c1) -> condToDNF c1
            | TTrue -> condToDNF TFalse
            | TFalse -> condToDNF TTrue
            | TRel(rel,t1,t2) -> [[TRel(opposite_rel rel,t1,t2)]]
	    | _ as t -> [[TNot(t)]]
	end
    | TTrue -> [[TTrue]]
    | TFalse -> []
    | _ as t -> [[t]]

let removeTerm term lterm = 
  List.fold_left
    (fun treated t -> 
	match term,t with 
	  | TCall (kf1,None), TCall (kf2,_) 
	  | TReturn kf1, TReturn kf2
            when Kernel_function.equal kf1 kf2 -> treated
          | TCall(kf1,Some b1), TCall(kf2, Some b2)
            when Kernel_function.equal kf1 kf2 && 
              Datatype.String.equal b1.b_name b2.b_name -> treated
	  | _  -> t::treated)
    []
    lterm

(** Given a list of terms (representing a conjunction), 
    if a positive call or return is present, 
    then all negative ones are obvious and removed *)
let positiveCallOrRet clause =
  try
    (* Step 1: find a positive information TCall or TReturn. *)
    let positive, computePositive=
      List.fold_left
        (fun (positive,treated as res) term ->
	  match term with
	    | TCall (kf1,None) -> 
	      begin match positive with
		| None -> (Some term, term::treated)
		| Some (TCall (kf2,None)) ->
		    if Kernel_function.equal kf1 kf2 then res else raise Exit
		| Some (TReturn _) -> raise Exit
                | Some(TCall (kf2,Some _) as term2) ->
                  if Kernel_function.equal kf1 kf2 then
                    Some term, term :: removeTerm term2 treated
                  else raise Exit
		| _ -> 
                  Aorai_option.fatal
                    "inconsistent environment in positiveCallOrRet"
	      end
            | TCall (kf1, Some b1) ->
              begin match positive with
                | None -> (Some term, term::treated)
		| Some (TCall (kf2,None)) ->
		    if Kernel_function.equal kf1 kf2 then res else raise Exit
		| Some (TReturn _) -> raise Exit
                | Some(TCall (kf2,Some b2)) ->
                  if Kernel_function.equal kf1 kf2 then
                    if Datatype.String.equal b1.b_name b2.b_name then
                      res
                    else
                      positive, term :: treated
                  else raise Exit
		| _ -> 
                  Aorai_option.fatal
                    "inconsistent environment in positiveCallOrRet"
	      end
	    | TReturn kf1 -> 
	      begin match positive with
		| None -> (Some term, term::treated)
		| Some (TReturn kf2) ->
		  if Kernel_function.equal kf1 kf2 then res else raise Exit
		| Some (TCall _) -> raise Exit
                | _ ->
                  Aorai_option.fatal
                    "inconsistent environment in positiveCallOrRet"
	      end
	    | _  -> positive, term::treated
        )
        (None, [])
        clause
    in
    (* Step 2 : Remove negatives not enough expressive *)
    match positive with 
      | None -> computePositive
      | Some (TCall (kf1,None)) ->
	List.fold_left
	  (fun treated term -> 
	    match term with 
	      | TNot(TCall (kf2,_)) ->
		if Kernel_function.equal kf1 kf2 then raise Exit
		(* Positive information more specific than negative *)
		else treated
	      | TNot(TReturn _) -> treated
	      | _  -> term::treated
	  )
	  [] computePositive
      | Some (TCall (kf1, Some b1)) ->
	List.fold_left
	  (fun treated term -> 
	    match term with 
	      | TNot(TCall (kf2,None)) ->
		if Kernel_function.equal kf1 kf2 then raise Exit
		(* Positive information more specific than negative *)
		else treated
              | TNot(TCall(kf2, Some b2)) ->
                if Kernel_function.equal kf1 kf2 then
                  if Datatype.String.equal b1.b_name b2.b_name then raise Exit
                  else term :: treated
                else treated
	      | TNot(TReturn _) -> treated
	      | _  -> term::treated
	  )
	  [] computePositive
        
      | Some (TReturn kf1) ->
	List.fold_left
	  (fun treated term -> 
	    match term with 
	      | TNot(TCall _) -> treated
	      | TNot(TReturn kf2) -> 
		(* Two opposite information *) 
		if Kernel_function.equal kf1 kf2 then raise Exit else treated
	      | _ -> term::treated
	  )
	  [] computePositive
      | _ -> 
        Aorai_option.fatal "inconsistent environment in positiveCallOrRet"
  with Exit -> [TFalse] (* contradictory requirements for current event. *)

let rel_are_equals (rel1,t11,t12) (rel2,t21,t22) =
  rel1 = rel2 
  && Logic_utils.is_same_term t11 t21 
  && Logic_utils.is_same_term t12 t22

let swap_rel (rel,t1,t2) =
  let rel = match rel with
    | Rlt -> Rgt
    | Rle -> Rge
    | Rge -> Rle
    | Rgt -> Rlt
    | Req -> Req
    | Rneq -> Rneq
  in (rel,t2,t1)

let contradict_rel r1 (rel2,t21,t22) =
  rel_are_equals r1 (opposite_rel rel2, t21,t22)
  || rel_are_equals (swap_rel r1) (opposite_rel rel2, t21, t22)

(** Simplify redundant relations. *)
let simplify clause =
  try
    List.fold_left
      (fun clause term -> 
        match term with
	  | TTrue | TNot(TFalse) -> clause
	  | TFalse | TNot(TTrue) -> raise Exit
          | TRel(rel1,t11,t12) ->
	    if 
	      List.exists
		(fun term -> 
		  match term with
                    | TRel(rel2,t21,t22) 
                        when contradict_rel (rel1,t11,t12) (rel2, t21,t22) ->
                      raise Exit
		    | TRel(rel2,t21,t22) ->
                      rel_are_equals (rel1,t11,t12) (rel2,t21,t22)
                    | TNot(TRel(rel2,t21,t22))
                        when (rel_are_equals (rel1,t11,t12) (rel2,t21,t22)) -> 
                      raise Exit
                    | TNot(TRel(rel2,t21,t22)) ->
                      contradict_rel (rel1,t11,t12) (rel2,t21,t22)
		    | _ -> false)
		clause
	    then clause
            else term::clause
	 | TNot(TRel(rel1,t11,t12)) ->
	    if 
	      List.exists
		(fun term -> 
		  match term with
                    | TNot(TRel(rel2,t21,t22))
                        when contradict_rel (rel1,t11,t12) (rel2, t21,t22) ->
                      raise Exit
		    | TNot(TRel(rel2,t21,t22)) ->
                      rel_are_equals (rel1,t11,t12) (rel2,t21,t22)
		    | TRel(rel2,t21,t22)
                        when (rel_are_equals (rel1,t11,t12) (rel2,t21,t22)) -> 
                      raise Exit
                    | TRel(rel2,t21,t22) ->
                      contradict_rel (rel1,t11,t12) (rel2,t21,t22)
		    | _ -> false)
		clause
	    then clause
            else term::clause
	 | _ -> term :: clause)
      [] clause
  with Exit -> [TFalse]

let rec termsAreEqual term1 term2 =
  match term1,term2 with
    | TTrue,TTrue
    | TFalse,TFalse -> true
    | TCall (a,None), TCall (b,None)
    | TReturn a, TReturn b -> Kernel_function.equal a b
    | TCall (f1,Some b1), TCall(f2, Some b2) ->
      Kernel_function.equal f1 f2 && Datatype.String.equal b1.b_name b2.b_name
    | TNot(TRel(rel1,t11,t12)), TRel(rel2,t21,t22)
    | TRel(rel1,t11,t12), TNot(TRel(rel2,t21,t22)) ->
      contradict_rel (rel1,t11,t12) (rel2,t21,t22)
    | TNot(a),TNot(b) -> termsAreEqual a b
    | TRel(rel1,t11,t12), TRel(rel2,t21,t22) -> 
      rel_are_equals (rel1,t11,t12) (rel2,t21,t22)
    | _  -> false

(** true iff clause1  <: clause2*)
let clausesAreSubSetEq clause1 clause2 = 
  (List.for_all 
     (fun t1 ->List.exists ( fun t2 -> termsAreEqual t1 t2) clause2)
    clause1)


(** true iff clause1  <: clause2 and clause2  <: clause1 *)
let clausesAreEqual clause1 clause2 = 
  clausesAreSubSetEq clause1 clause2 && clausesAreSubSetEq clause2 clause1

(** return the clauses list named lclauses without any clause c such as  cl <: c *)
let removeClause lclauses cl =
  List.filter (fun c -> not (clausesAreSubSetEq cl c)) lclauses

(* Obvious version. *)
let negativeClause clause = 
  List.map
    (fun term -> 
      match term with 
	| TNot(c) -> c
	| TCall _ | TReturn _ | TRel _ -> TNot term
	| TTrue -> TFalse
	| TFalse -> TTrue
	| TAnd (_,_)
	| TOr (_,_) -> Aorai_option.fatal "not a DNF clause"
    ) clause

let simplifyClauses clauses =
  try
    List.fold_left 
      (fun acc c -> 
       (* If 2 clauses are C and not C then their disjunction implies true *)
        if List.exists (clausesAreEqual (negativeClause c)) acc then
          raise Exit
       (* If an observed clause c2 is included inside the current clause 
          then the current is not added *)
       else if (List.exists (fun c2 -> clausesAreSubSetEq c2 c) acc) then
         acc
       (* If the current clause is included inside an observed clause 
          c2 then the current is add and c2 is removed *)
       else if (List.exists (fun c2 -> clausesAreSubSetEq c c2) acc) then
         c::(removeClause acc c)
       (* If no simplification then c is add to the list *)
       else c::acc
      )
      [] clauses
  with Exit -> [[]]

let tor t1 t2 =
  match t1,t2 with
      TTrue,_ | _,TTrue -> TTrue
    | TFalse,t | t,TFalse -> t
    | _,_ -> TOr(t1,t2)

let tand t1 t2 =
  match t1,t2 with
      TTrue,t | t,TTrue -> t
    | TFalse,_ | _,TFalse -> TFalse
    | _,_ -> TAnd(t1,t2)

let tnot t =
  match t with
      TTrue -> TFalse
    | TFalse -> TTrue
    | TNot t -> t
    | TRel(rel,t1,t2) -> TRel(opposite_rel rel, t1, t2)
    | _ -> TNot t

let tands l = List.fold_left tand TTrue l

let tors l = List.fold_left tor TFalse l

(** Given a DNF condition, it returns a condition in Promelaast.condition form. 
    WARNING : empty lists not supported
*)
let dnfToCond d = tors (List.map tands d)

let simplClause dnf clause =
  match clause with
    | [] | [TTrue] | [TNot TFalse]-> [[]]
    | [TFalse] | [TNot TTrue] -> dnf
    | _ -> clause :: dnf

(** Given a condition, this function does some logical simplifications. 
    It returns both the simplified condition and a disjunction of 
    conjunctions of parametrized call or return.
*)
let simplifyCond condition =
  Aorai_option.debug 
    "initial condition: %a" Promelaoutput.print_condition condition;
  (* Step 1 : Condition is translate into Disjunctive Normal Form *)
  let res1 = condToDNF condition in 
  Aorai_option.debug "initial dnf: %a" pretty_dnf res1;
  (* Step 2 : Positive Call/Ret are used to simplify negative ones *)
  let res = 
    List.fold_left 
      (fun lclauses clause -> simplClause lclauses (positiveCallOrRet clause))
      [] res1
  in
  Aorai_option.debug "after step 2: %a" pretty_dnf res;
  (* Step 3 : simplification between exprs inside a clause *)
  let res = 
    List.fold_left 
      (fun lclauses clause -> simplClause lclauses (simplify clause)) [] res
  in
  Aorai_option.debug "after step 3: %a" pretty_dnf res;
  
  (* Step 4 : simplification between clauses *)
  let res = simplifyClauses res in 
  Aorai_option.debug "after step 4: %a" pretty_dnf res;
  ((dnfToCond res), res)

(** Given a list of transitions, this function returns the same list of
   transition with simplifyCond done on its cross condition *) 
let simplifyTrans transl =
  List.fold_left 
    (fun (ltr,lpcond) tr -> 
      let (crossCond , pcond ) = simplifyCond (tr.cross) in
       (* pcond stands for parametrized condition : 
          disjunction of conjunctions of parametrized call/return *)
      let tr'={ start = tr.start ;
		stop  = tr.stop  ;
		cross = crossCond ;
		numt  = tr.numt
	      }
      in
      Aorai_option.debug "condition is %a, dnf is %a" 
        Promelaoutput.print_condition crossCond pretty_dnf pcond;
      if tr'.cross <> TFalse then (tr'::ltr,pcond::lpcond) else (ltr,lpcond)
    ) 
    ([],[]) 
    (List.rev transl)

(** Given a DNF condition, it returns the same condition simplified according 
    to the context (function name and status). Hence, the returned condition 
    is without any Call/Return stmts. 
*)
let simplifyDNFwrtCtx dnf kf1 status =
  Aorai_option.debug "Before simplification: %a" pretty_dnf dnf;
  let rec simplCondition c =
    match c with
      | TCall (kf2, None) -> 
        if  Kernel_function.equal kf1 kf2 && status = Promelaast.Call then 
          TTrue 
        else TFalse
      | TCall (kf2, Some _) ->
        if Kernel_function.equal kf1 kf2 && status = Promelaast.Call then
          c
        else TFalse
      | TReturn kf2 ->
        if Kernel_function.equal kf1 kf2 && status = Promelaast.Return then 
          TTrue
        else TFalse
      | TNot c -> tnot (simplCondition c)
      | TAnd(c1,c2) -> tand (simplCondition c1) (simplCondition c2)
      | TOr (c1,c2) -> tor (simplCondition c1) (simplCondition c2)
      | TTrue | TFalse | TRel _ -> c
  in
  let simplCNFwrtCtx cnf =
    tands (List.map simplCondition cnf)
  in
  let res = tors (List.map simplCNFwrtCtx dnf) in
  Aorai_option.debug 
    "After simplification: %a" Promelaoutput.print_condition res; res

(*
Tests : 

Marchent :
==========
simplifyCond(PAnd(POr(PTrue,PIndexedExp("a")),PNot(PAnd(PFalse,PIndexedExp("b")))));;
- : condition = PTrue

simplifyCond(POr(PAnd(PNot(PIndexedExp("b")),POr(PTrue,PIndexedExp("a"))),PAnd(PIndexedExp("a"),PNot(PFalse))));;
- : condition = POr (PIndexedExp "a", PNot (PIndexedExp "b"))


simplifyCond(PAnd(PAnd(PCall("a"),PIndexedExp "a"),PAnd(PNot(PCall("a")),PNot(PIndexedExp "a"))));;
- : condition = PFalse


simplifyCond(PAnd(PIndexedExp "a",PNot(PIndexedExp "a")));;
- : condition = PFalse


simplifyCond(PAnd(PCall("a"),PCall("a")));;
- : condition = PCall "a"

simplifyCond(PAnd(PIndexedExp("a"),PNot(PIndexedExp("a"))));;
- : condition = PFalse


simplifyCond(POr(PCall("a"),PNot(PCall("a"))));;
- : condition = PTrue

simplifyCond(PAnd(POr(PCall("a"),PCall("b")),POr(PNot(PCall("a")),PCall("b")))) ;;
- : condition = PCall "b"

simplifyCond(POr (PCall "b", PCall "b"));;
- : condition = PCall "b"




Simplifications a faire :
=========================



*)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)


