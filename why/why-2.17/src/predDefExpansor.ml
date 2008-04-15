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

open Env
open Ident
open Misc
open Logic 
open Cc 
open Options
open Logic_decl

let pred_def = Hashtbl.create 97

let rec subst_in_pure_type st = function
  | PTvar ({ type_val = None } as v) as t ->
      (try Vmap.find v st with Not_found -> t)
  | PTvar { type_val = Some t } ->
      subst_in_pure_type st t
  | PTexternal(l, id) ->
      PTexternal(List.map (subst_in_pure_type st) l, id)
  | PTint | PTreal | PTbool | PTunit as t -> 
      t

let subst_in_instance st = List.map (subst_in_pure_type st)

let rec subst_in_term s st = function
  | Tvar x as t -> 
      (try Idmap.find x s with Not_found -> t)
  | Tapp (x,l,i) -> 
      Tapp (x, List.map (subst_in_term s st) l, subst_in_instance st i)
  | Tconst _ | Tderef _ as t -> 
      t
  | Tnamed(lab,t) -> Tnamed(lab,subst_in_term s st t)

let rec subst_in_predicate s st = function
  | Papp (id, l, i) -> 
      Papp (id, List.map (subst_in_term s st) l, subst_in_instance st i)
  | Pif (a, b ,c) -> 
      Pif (subst_in_term s st a, 
	   subst_in_predicate s st b, 
	   subst_in_predicate s st c)
  | Pfpi (t, f1, f2) -> 
      Pfpi (subst_in_term s st t, f1, f2)
  | Forall (w, id, b, v, tl, p) -> 
      Forall (w, id, b, subst_in_pure_type st v, 
	      List.map (List.map (subst_in_pattern s st)) tl,
	      subst_in_predicate s st p)
  | Exists (id, b, v, p) ->
      Exists (id, b, subst_in_pure_type st v, subst_in_predicate s st p)
  | p -> 
      map_predicate (subst_in_predicate s st) p

and subst_in_pattern s st = function
  | TPat t -> TPat (subst_in_term s st t)
  | PPat p -> PPat (subst_in_predicate s st p)

let rec expand = function
  | Papp (id, tl, i) as p ->
      begin 
	try
	  let tvars,argl,p = Hashtbl.find pred_def id in
	  let st = subst_many argl tl in
	  let sty = List.fold_left2 (fun s v t -> Vmap.add v t s) Vmap.empty tvars i in
	  subst_in_predicate st sty p
	with Not_found ->
	  p
      end
  | p -> 
      map_predicate expand p

let expand_inductive_def (t,l) = (t,List.map (fun (id,p) -> (id,expand p)) l)

let expand_sequent (ctx,p) =
  let expand_hyp = function
    | Svar _ as h -> h
    | Spred (id, p) -> Spred (id, 
			      if Options.defExpanding = All then 
				expand p else p)  in
  (List.map expand_hyp ctx, expand p)


let push decl = 
  match decl with
  | Dpredicate_def (loc, ident, def) ->
      let argl,p = def.scheme_type in
      (*let rootexp = (Papp (Ident.create ident, List.map (fun (i,_) -> Tvar i) argl, [])) in*)
      let p = expand p  in
      let vars = Vset.fold (fun v l -> v :: l) def.scheme_vars [] in
      Hashtbl.add pred_def ident (vars, List.map fst argl, p);
      if Options.defExpanding = All then 
	Dlogic (loc, Ident.string ident, Env.generalize_logic_type (Predicate (List.map snd argl)))
      else
	decl
  | Dinductive_def(loc, ident, def) when Options.defExpanding = All -> 
      Dinductive_def(loc,ident,
		     Env.generalize_inductive_def 
		       (expand_inductive_def def.scheme_type))		       
  | Daxiom (loc, ident, ps) -> 
      Daxiom (loc, ident, Env.generalize_predicate (
			if Options.defExpanding = All then 
			  expand ps.scheme_type else 
			    ps.scheme_type ))
  | Dgoal (loc, expl, ident, ps) ->
      Dgoal (loc, expl, ident, 
	     Env.generalize_sequent (expand_sequent ps.scheme_type)) 
  | Dfunction_def _ 
  | Dinductive_def _ 
  | Dtype _ 
  | Dlogic _ -> decl

(***************
 inductive definitions
**************)

(*

inductive id : t1 -> .. -> tn -> prop {

  c_1 : forall x_1,..,x_k : H1 -> .. Hi -> id(e_1,..,e_n)

| ...

| c_j

}

gives (j+1) axioms : j direct axioms

a_1 : forall x_1,..,x_k : H1 -> .. Hi -> id(e_1,..,e_n)
..
a_j : ...

and one inversion axiom :

forall y_1:t_1,..,y_n:t_n, id(y_1,..,y_n) ->

  exists x_1,..,x_k: H1 /\ ... /\ Hi /\ y_1 = e1 /\ .. /\ y_n = e_n

\/ 
 
  ... 

*)


let inductive_inverse_body id params cases =
  let yvars = List.map (fun t -> (fresh_var(),t)) params in
  let rec invert = function
    | Forall(w,id,n,t,trig,p) -> Exists(id,n,t,invert p)
    | Pimplies(w,h,p) -> Misc.pand h (invert p)
    | Papp(f,l,_) ->
	assert (f == id); (* ill-formed, should have been catch by typing *)
	List.fold_right2 
	  (fun (y,_) e acc -> Misc.pand (Papp(t_eq,[Tvar y;e],[])) acc)
	  yvars l Ptrue	
    | _ -> assert false (* ill-formed, should have been catch by typing *)
  in
  let rec fold acc cases =
    match cases with
      | [] -> acc
      | (id,c)::rem -> fold (Misc.por (invert c) acc) rem
  in
  let body = 
    List.fold_right
      (fun (id,c) acc -> Misc.por (invert c) acc)
      cases Pfalse 
  in
  yvars,body


let inversion_axiom id params cases =
  let yvars,body = inductive_inverse_body id params cases in
  let ytvars = List.map (fun (y,_) -> Tvar y) yvars in
  let body = Pimplies(false,Papp(id,ytvars,[]),body) in
  List.fold_right (fun (y,t) acc -> Forall(false,y,y,t,[],acc)) yvars body
  
    
  

let inductive_def loc id d =
  let (vars,(bl,cases)) = Env.specialize_inductive_def d in
  let t = Env.generalize_logic_type (Predicate bl) in
  let name = Ident.string id in
  Dlogic(loc,name,t)::
    (Daxiom(loc,name ^ "_inversion",
	    Env.generalize_predicate (inversion_axiom id bl cases)))::
    (List.map 
       (fun (id,p) -> 
	  let p = Env.generalize_predicate p in
	  Daxiom(loc,Ident.string id,p)) cases)
