(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
(*    INSA  (Institut National des Sciences Appliquees)                   *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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

open Promelaast


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
    | POr  (c1, c2) -> (condToDNF c1)@(condToDNF c2)
    | PAnd (c1, c2) -> 
	let d1,d2=(condToDNF c1), (condToDNF c2) in
	List.fold_left 
	  (fun lclause clauses2 -> 
	     (List.map 
		(fun clauses1 -> 
		   clauses1@clauses2
		)
		d1)@lclause
	  ) 
	  [] d2
    | PNot (c) -> 
	begin
	  match c with
	    | POr  (c1, c2) -> condToDNF (PAnd(PNot(c1),PNot(c2)))
	    | PAnd (c1, c2) -> condToDNF (POr (PNot(c1),PNot(c2)))
	    | PNot (c1) -> condToDNF c1
	    | _ as t -> [[PNot(t)]]
	end
	  
    | _ as t -> [[t]]
	




let removeTerm term lterm = 
  List.fold_left 
    (fun treated t -> 
	match term,t with 
	  | PCall (s1),PCall (s2) 
	  | PReturn (s1),PReturn (s2)
	  | PCallOrReturn (s1),PCallOrReturn (s2) when (String.compare s1 s2)=0 -> treated
	  | _ as o -> (snd o)::treated
    )
    []
    lterm


(** Given a list of termes, if a positive call or return is present, then all negative ones are obvious and removed *)
let positiveCallOrRet clause = 
  let positive = ref None in 
  let isFalse = ref false in
  let computePositive=
    List.fold_left
      (fun treated term -> 
	 match term with 
	   | PCall (s)  as t -> 
	       begin match !positive with
		 | None -> 
		     positive:= Some(t) ; 
		     t::treated
		       
		 | Some(PCall (a))
		 | Some(PFuncParam (_,a,_)) ->
		     (* Two positives on two different functions *) 
		     if (String.compare a s)<>0 then isFalse:=true;
		     (* Already present. Removed from list *)
		     treated 
			 
		 | Some(PReturn (_))
		 | Some(PFuncReturn (_,_)) -> 
		     (* Two positives on two different function or Call and Ret on the same function. Always False  *)
		     isFalse:=true;
		     []
		       
		 | Some(PCallOrReturn (a)) -> 
		     (* Two positives on two different functions *) 
		     if (String.compare a s)<>0 then isFalse:=true;
		     (* More specific information *)
		     positive:= Some(t) ; 
		     t::(removeTerm (PCallOrReturn (a)) treated)
			 
		 | _ -> assert false (* This Variable has to contain only positive call, ret or call/ret conditions *)
	       end
		 
	   | PReturn (s) as t ->
	       begin match !positive with
		 | None -> 
		     positive:= Some(t) ; 
		     t::treated
		       
		 | Some(PReturn (a))
		 | Some(PFuncReturn (_,a)) -> 
		     (* Two positives on two different functions *) 
		     if (String.compare a s)<>0 then isFalse:=true;
		     (* Already present. Removed from list *)
		     treated 
			 
			 
		 | Some(PCall (_))
		 | Some(PFuncParam (_,_,_)) ->
		     (* Two positives on two different function or Call and Ret on the same function. Always False  *)
		     isFalse:=true;
		     []

			 
		 | Some(PCallOrReturn (a)) -> 
		     (* Two positives on two different functions *) 
		     if (String.compare a s)<>0 then isFalse:=true;
		     (* More specific information *)
		     positive:= Some(t) ; 
		     t::(removeTerm (PCallOrReturn (a)) treated)
			 
		 | _ -> assert false (* This Variable has to contain only positive call, ret or call/ret conditions *)
	       end
		 
		 
	   | PCallOrReturn(s) as t -> 
	       begin match !positive with
		 | None -> 
		     positive:= Some(t) ; 
		     t::treated
		       
		 | Some(PReturn (a))
		 | Some(PFuncReturn (_,a)) -> 
		     (* Two positives on two different functions *) 
		     if (String.compare a s)<>0 then isFalse:=true;
		     (* More specific information already present *)
		     treated 
			 
			 
		 | Some(PCall (a))
		 | Some(PFuncParam (_,a,_)) ->
		     (* Two positives on two different functions *) 
		     if (String.compare a s)<>0 then isFalse:=true;
		     (* More specific information already present *)
		     treated 
			 
			 
		 | Some(PCallOrReturn (a)) -> 
		     (* Two positives on two different functions *) 
		     if (String.compare a s)<>0 then isFalse:=true;
		     (* Already present. Removed from list *)
		     treated 
			 
			 
		 | _ -> assert false (* This Variable has to contain only positive call, ret or call/ret conditions *)
	       end
		 
		 
		 
	     | _ as t -> t::treated
      )
      []
      clause
  in
  (* Step 2 : Remove negatives not enough expressive *)
  if !isFalse then 
    [] 
  else
    let res =
      match !positive with 
	| None -> computePositive
	| Some(PCall(name)) -> 
	    List.fold_left
	      (fun treated term -> 
		match term with 
		  | PNot(PCall (s)) -> 
		      (* Two opposite information *) 
		      if (String.compare name s)=0 then isFalse:=true;
		      (* Positive information more specific than negative one *)
		      treated 

		  | PNot(PReturn (_)) -> 
		      (* Positive information more specific than negative one *)
		      treated 

		  | PNot(PCallOrReturn (s)) -> 
		      (* Two opposite information *) 
		      if (String.compare name s)=0 then isFalse:=true;
		      (* Positive information more specific than negative one *)
		      treated 

		  | _ as t -> t::treated
	      )
	      []
	      computePositive

	| Some(PReturn (name))->
	    List.fold_left
	      (fun treated term -> 
		match term with 
		  | PNot(PCall (_)) -> 
		      (* Positive information more specific than negative one *)
		      treated 

		  | PNot(PReturn (s)) -> 
		      (* Two opposite information *) 
		      if (String.compare name s)=0 then isFalse:=true;
		      (* Positive information more specific than negative one *)
		      treated 

		  | PNot(PCallOrReturn (s)) -> 
		      (* Two opposite information *) 
		      if (String.compare name s)=0 then isFalse:=true;
		      (* Positive information more specific than negative one *)
		      treated 

		  | _ as t -> t::treated
	      )
	      []
	      computePositive


	| Some(PCallOrReturn (name)) -> 
	    List.fold_left
	      (fun treated term -> 
		match term with 
		  | PNot(PCall (s)) -> 
		      if (String.compare name s)=0 then 
			(PReturn(s))::(removeTerm (PCallOrReturn (name)) treated)
		      else
			(* Positive information more specific than negative one *)
			treated 
			
		  | PNot(PReturn (s)) -> 
		      if (String.compare name s)=0 then 
			(PCall(s))::(removeTerm (PCallOrReturn (name)) treated)
		      else
			(* Positive information more specific than negative one *)
			treated 

		  | PNot(PCallOrReturn (s)) -> 
		      (* Two opposite information *) 
		      if (String.compare name s)=0 then isFalse:=true;
		      (* Positive information more specific than negative one *)
		      treated 

		  | _ as t -> t::treated
	      )
	      []
	      computePositive



	      
	| _ -> assert false (* This Variable has to contain only positive call, ret or call/ret conditions *)
    in
    if !isFalse then 
      [] 
    else
      res




(* !!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* Copy from Data_for_aorai, in order to remove forward reference. *)
let ltl_exps = ref (Hashtbl.create 1)
let setLtl_expressions exps = 
  ltl_exps:=exps
let get_str_exp_from_tmpident var =
  try
    let (_,str,_) = (Hashtbl.find !ltl_exps var) in
      str
  with
    | _ ->
	Aorai_option.fatal "Aorai_acsl plugin internal error. Status : TMP Variable (%s) not declared in hashtbl. \n" var;;
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(* !!!!!!!!!!!!!!!!!!!!!!!!!!!! *)




let expAreEqual e1 e2 = 
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* Ici, pour les tests on lit s1 et s2, amis il faudrait dereferencer comme suit : 
   		       PIndexedExp (s) -> Data_for_aorai.get_str_exp_from_tmpident s
		Ou bien 
		       PIndexedExp (s) -> Data_for_aorai.get_exp_from_tmpident s
	     *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
	     (* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *)
(*   (String.compare e1 e2)=0 *)
  let s1,s2 = (get_str_exp_from_tmpident e1),(get_str_exp_from_tmpident e2) in
  (String.compare s1 s2)=0




(** Given a list of termes, if a positive call or return is present, then all negative ones are obvious and removed *)
let simplify clause = 
  let isFalse = ref false in
  let result  = ref [] in
  List.iter
    (fun term -> 
       match term with
	 | PTrue 
	 | PNot(PFalse) -> ()
	 | PFalse 
	 | PNot(PTrue) -> isFalse:=true
	 | PIndexedExp(s1) as t -> 
	     if 
	       List.fold_left
		 (fun toKeep term -> 
		    match term with
		      | PIndexedExp(s2) -> toKeep && (not (expAreEqual s1 s2))
		      | PNot(PIndexedExp(s2)) when (expAreEqual s1 s2) -> isFalse:=true;false
		      | _ -> toKeep
		 )
		 true
		 !result
	     then
	       result:=t::!result
		 
	 | PNot(PIndexedExp(s1)) as t -> 
	     if 
	       List.fold_left
		 (fun toKeep term -> 
		    match term with
		      | PNot(PIndexedExp(s2))  -> toKeep && (not (expAreEqual s1 s2))
		      | PIndexedExp(s2)  when (expAreEqual s1 s2) -> isFalse:=true;false
		      | _ -> toKeep
		 )
		 true
		 !result
	     then
	       result:=t::!result
		 
	 | _ as t -> 
	     result:=t::!result
    )   
    clause ;
  if !isFalse then [] 
  else if !result=[] then [PTrue] 
  else !result
  



let rec termsAreEqual term1 term2 = 
  match term1,term2 with
    | PTrue,PTrue
    | PFalse,PFalse -> true

    | PCall(a),PCall(b)
    | PReturn(a),PReturn(b) 
    | PCallOrReturn(a),PCallOrReturn(b) -> (String.compare a b)=0 

    | PIndexedExp(a),PIndexedExp(b) -> 
	expAreEqual a b 


    | PNot(a),PNot(b) -> termsAreEqual a b
	
    | PFuncReturn (h1, f1), PFuncReturn (h2, f2) -> (String.compare f1 f2)=0 && (expAreEqual h1 h2)
    | PFuncParam (h1, f1, p1), PFuncParam (h2, f2, p2) -> (String.compare f1 f2)=0 && (expAreEqual h1 h2) && p1=p2
    | _  -> false


(** true iff clause1  <: clause2*)
let clausesAreSubSetEq clause1 clause2 = 
  (List.for_all 
     (fun t1 ->List.exists ( fun t2 -> termsAreEqual t1 t2) clause2)
    clause1)


(** true iff clause1  <: clause2 and clause2  <: clause1 *)
let clausesAreEqual clause1 clause2 = 
  (List.for_all 
     (fun t1 ->List.exists ( fun t2 -> termsAreEqual t1 t2) clause2)
    clause1)
  &&
    (List.for_all
       (fun t1 ->List.exists ( fun t2 -> termsAreEqual t2 t1) clause1)
       clause2)

(** return the clauses list named lclauses without any clause c such as  cl <: c *)
let removeClause lclauses cl =
  List.filter (fun c -> not (clausesAreSubSetEq cl c)) lclauses

    

(* Obvious version. *)
let negativeClause clause = 
  List.map(fun term -> 
	     match term with 
		 | PNot(c) -> c
		 | PCall(s) -> PNot(PCall(s))
		 | PReturn(s) -> PNot(PReturn(s))
		 | PCallOrReturn(s) -> PNot(PCallOrReturn(s))
		 | PIndexedExp(s) -> PNot(PIndexedExp(s))
		 | PTrue -> PFalse
		 | PFalse -> PTrue
		 | PFuncReturn (hash, f) -> PNot(PFuncReturn (hash, f))
		 | PFuncParam (hash, f, p) ->  PNot(PFuncParam (hash, f, p))
		 | PAnd (_,_)
		 | POr (_,_) -> assert false 
	  ) clause



let simplifyClauses clauses =
  let result= ref [] in
  List.iter 
    (fun c -> 
       (* If 2 clauses are C and not C then theire disjunction implies true *)
       if List.exists (clausesAreEqual (negativeClause c)) !result then result:=[PTrue]::!result
        
       (* If an observed clause c2 is include inside the current clause then the current is not add *)
       else if (List.exists (fun c2 -> clausesAreSubSetEq c2 c) !result) then () 

       (* If the current clause is include inside an observed clause c2 then the current is add and c2 is removed *)
       else if (List.exists (fun c2 -> clausesAreSubSetEq c c2) !result) then  result:=c::(removeClause !result c)
	 
       (* If no simplification then c is add to the list *)
       else result:=c::!result 
    ) 
    clauses;
  !result

	
let dnfToCond d = 
  let isTrue =ref false in
  
  let clauseToCond c = 
    if c=[PTrue] then isTrue:=true;
    if List.length c =1 then 
      (List.hd c)
    else
      List.fold_left (fun c1 c2 -> PAnd(c1,c2)) (List.hd c) (List.tl c)
  in
  let res = 
    if List.length d=1 then 
      clauseToCond (List.hd d)
    else
      List.fold_left (fun d1 d2 -> POr(d1,(clauseToCond d2))) (clauseToCond (List.hd d)) (List.tl d)
  in
  if !isTrue then PTrue else res
      
      
(** Given a condition, this function does some logical simplifications. *)
let simplifyCond condition = 
  (* Step 1 : Condition is translate into Disjunctive Normal Form *)
  let res = condToDNF condition in 
  
  (* Step 2 : Positive Call/Ret are used to simplify negative ones *)
  let res = List.fold_left (fun lclauses clause -> 
			      let c=(positiveCallOrRet clause) in
			      if c=[] then lclauses
			      else c::lclauses 
			   ) [] res in


  (* Step 3 : simplification between exprs inside a clause *)
  let res = List.fold_left (fun lclauses clause -> 
			      let c=(simplify clause) in
			      if c=[] then lclauses
			      else c::lclauses 
			   ) [] res in
  
  
  (* Step 4 : simplification between clauses *)
  let res = simplifyClauses res in

  (* Last step : list of list translate back into condition type. *)
  if res=[] then PFalse
  else (dnfToCond res)
    
    

(** Given a transition, this function returns the same transition with simplifyCond done on its cross condition *)
let simplifyTrans transl =
  List.fold_left (fun ltr tr -> 
		    let tr'={ start = tr.start ;
			      stop  = tr.stop  ;
			      cross = simplifyCond (tr.cross) ;
			      numt  = tr.numt
			    }
		    in
		    if tr'.cross <> PFalse then tr'::ltr else ltr
		 ) [] (List.rev transl)



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


