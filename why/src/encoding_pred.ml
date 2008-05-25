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

(*i $Id: encoding_pred.ml,v 1.11 2008/02/05 12:10:49 marche Exp $ i*)

open Cc
open Logic
open Logic_decl

let loc = Loc.dummy_floc

let prefix = "c_"
let suffix = "_c"
let var = "x"
let tvar = "t"
let cpt = ref 0
let axiom c = c ^ "_to_" ^ (c^suffix)
let def c = "def_"^c

(* The unique type for all terms *)
let ut = PTexternal ([], Ident.create (prefix^"unique"))

let unify ptl = List.map (fun _ -> ut) ptl
    
let prelude =
  (Dtype (loc, [], prefix^"unique"))::	(* The unique sort *)
  (Dlogic (loc, prefix^"sort", 
	   Env.empty_scheme (Predicate ([ut; ut]))))::	(* The "sort" predicate*)
  (Dlogic (loc, prefix^"int",
	   Env.empty_scheme (Function ([], ut)))):: (* One synbol for each prefedined type *)
  (Dlogic (loc, prefix^"bool", 
	   Env.empty_scheme (Function ([], ut))))::
  (Dlogic (loc, prefix^"real",
	   Env.empty_scheme (Function ([], ut))))::
  (Dlogic (loc, prefix^"unit", 
	   Env.empty_scheme (Function ([], ut))))::
  []
    
(* Function that creates the predicate sort(pt,term) *)
(* Uses an assoc. list of tags -> idents for type variables *)
let plunge fv term pt =
  let rec leftt pt =
    match pt with
      PTint -> Tapp (Ident.create (prefix^"int"), [], [])
    | PTbool -> Tapp (Ident.create (prefix^"bool"), [], [])
    | PTreal -> Tapp (Ident.create (prefix^"real"), [], [])
    | PTunit -> Tapp (Ident.create (prefix^"unit"), [], [])
    | PTvar ({type_val = None} as var) -> 
	let t = try (List.assoc var.tag fv) 
	with _ -> 
	  let s = string_of_int var.tag in 
	  (print_endline ("unknown vartype : "^s); s)
	in
	Tapp (Ident.create t, [], [])
    | PTvar {type_val = Some pt} -> leftt pt
    | PTexternal (ptl, id) -> Tapp (id, List.map (fun pt -> leftt pt) ptl, [])
  in
  Papp (Ident.create (prefix^"sort"), [leftt pt; term], [])

(* Generalizing a predicate scheme with foralls (can add a trigger) *)
let rec lifted  l p t =
  match l with [] -> p
  | a::[] -> Forall(false, a, a, ut, t, p)
  | a::q -> Forall(false, a, a, ut, [], lifted q p t)

(* The core *)

let queue = Queue.create ()

let reset () = 
  Queue.clear queue
      
let rec push d = 
  match d with
(* A type declaration is translated as new logical function, the arity of *)
(* which depends on the number of type variables in the declaration *)
  | Dtype (loc, vars, ident) ->
      Queue.add (Dlogic (loc, ident, 
			 Env.empty_scheme (Function (unify vars, ut)))) queue
(* In the case of a declaration of a function or predicate symbol, the symbol *)
(* is redefined with 'unified' arity, and in the case of a function, an axiom *)
(* is added to type the result w.r.t the arguments *)
  | Dlogic (loc, ident, arity) -> 
      let cpt = ref 0 in
      let fv = Env.Vset.fold
	  (fun tv acc -> cpt := !cpt + 1; (tv.tag, tvar^(string_of_int !cpt))::acc)
	  (arity.Env.scheme_vars) [] in
      (match arity.Env.scheme_type with 
      | Predicate ptl ->
	  Queue.add (Dlogic (loc, ident,
			     Env.empty_scheme (Predicate (unify ptl)))) queue
      | Function (ptl, rt) -> 
	  let args = 
	    List.map
	      (fun t -> 
		Ident.create (let _ = cpt := !cpt + 1 in var^(string_of_int !cpt)), t)
	      ptl in
	  let rec mult_conjunct l p =
	    match l with [] -> p
	    | a::q -> mult_conjunct q (Pand(false, false, a, p)) in
	  let prem = 
	    mult_conjunct (List.map (fun (id,t) -> plunge fv (Tvar id) t) args) Ptrue in
	  let pattern = (Tapp (Ident.create ident, List.map (fun (t, _) -> Tvar t) args, [])) in
	  let ccl = plunge fv pattern rt in
	  let ax = Env.empty_scheme 
	      (lifted 
		 ((fst (List.split args))@(List.map (fun i -> Ident.create i) (snd (List.split fv))))
		 (Pimplies (false, prem, ccl)) []) in
	  (Queue.add (Dlogic (loc, ident,
			      Env.empty_scheme (Function (unify ptl, ut)))) queue;
	   Queue.add (Daxiom (loc, axiom ident, ax)) queue))
(* A predicate definition can be handled as a predicate logic definition + an axiom *)
  | Dpredicate_def (loc, ident, pred_def_sch) ->
      let p = pred_def_sch.Env.scheme_type in
      let rec lifted_t l p =
	match l with [] -> p
	| (a,t)::q -> lifted_t q (Forall(false, a, a, t, [], p)) in
      push (Dlogic (loc, ident,
		    (Env.generalize_logic_type (Predicate (snd (List.split (fst p)))))));
      push (Daxiom (loc, def ident,
		    (Env.generalize_predicate
		       (lifted_t (fst p)
			  (Piff ((Papp (Ident.create ident, List.map (fun (i,_) -> Tvar i) (fst p), [])),
			         (snd p)))))))
(* A function definition can be handled as a function logic definition + an axiom *)
  | Dfunction_def (loc, ident, fun_def_sch) ->
      let f = fun_def_sch.Env.scheme_type in
      let rec lifted_t l p =
	match l with [] -> p
	| (a,t)::q -> lifted_t q (Forall(false, a, a, t, [], p)) in
      let (ptl, rt, t) = f in
      push (Dlogic (loc, ident,
		    (Env.generalize_logic_type (Function (snd (List.split ptl), rt)))));
      push (Daxiom (loc, def ident,
		    (Env.generalize_predicate
		       (lifted_t ptl
			  (Papp (Ident.t_eq,
				 [(Tapp (Ident.create ident, List.map (fun (i,_) -> Tvar i) ptl, []));
				  t], []))))))
(* Goals and axioms are just translated straightworfardly *)
  | Daxiom (loc, ident, pred_sch) ->
      let cpt = ref 0 in
      let fv = Env.Vset.fold
	  (fun tv acc -> cpt := !cpt + 1; (tv.tag, tvar^(string_of_int !cpt))::acc)
	  (pred_sch.Env.scheme_vars) [] in
      let rec translate_eq = function
	| Pimplies (iswp, p1, p2) ->
	    Pimplies (iswp, translate_eq p1, translate_eq p2)
	| Pif (t, p1, p2) ->
	    Pif (t, translate_eq p1, translate_eq p2)
	| Pand (iswp, issym, p1, p2) ->
	    Pand (iswp, issym, translate_eq p1, translate_eq p2)
	| Por (p1, p2) ->
	    Por (translate_eq p1, translate_eq p2)
	| Piff (p1, p2) ->
	    Piff (translate_eq p1, translate_eq p2)
	| Pnot p ->
	    Pnot (translate_eq p)
	| Forall (iswp, id, n, pt, tl, p) ->
	    Forall (iswp, id, n, ut, tl,
		    Pimplies(false, plunge fv (Tvar n) pt, translate_eq p))
	| Forallb (iswp, p1, p2) ->
	    Forallb (iswp, translate_eq p1, translate_eq p2)
	| Exists (id, n, pt, p) ->
	    Exists (id, n, ut, Pand (false, false, plunge fv (Tvar n) pt, translate_eq p))
	| Pnamed (s, p) ->
	    Pnamed (s, translate_eq p)
	| p -> p in
      let rec lifted  l p =
	match l with [] -> p
	| (_,a)::q -> 
	    lifted q (Forall(false, Ident.create a, Ident.create a, ut, [], p))
      in
      Queue.add (Daxiom (loc, ident,
			 Env.empty_scheme
 			   (lifted fv (translate_eq pred_sch.Env.scheme_type)))) queue
  | Dgoal (loc, expl, ident, s_sch) ->
      let cpt = ref 0 in
      let (cel, pred) = s_sch.Env.scheme_type in
      let fv = Env.Vset.fold
	  (fun tv acc -> cpt := !cpt + 1; (tv.tag, tvar^(string_of_int !cpt))::acc)
	  (s_sch.Env.scheme_vars) [] in
      let rec translate_eq = function
	| Pimplies (iswp, p1, p2) ->
	    Pimplies (iswp, translate_eq p1, translate_eq p2)
	| Pif (t, p1, p2) ->
	    Pif (t, translate_eq p1, translate_eq p2)
	| Pand (iswp, issym, p1, p2) ->
	    Pand (iswp, issym, translate_eq p1, translate_eq p2)
	| Por (p1, p2) ->
	    Por (translate_eq p1, translate_eq p2)
	| Piff (p1, p2) ->
	    Piff (translate_eq p1, translate_eq p2)
	| Pnot p ->
	    Pnot (translate_eq p)
	| Forall (iswp, id, n, pt, tl, p) ->
	    Forall (iswp, id, n, ut, tl,
		    Pimplies(false, plunge fv (Tvar n) pt, translate_eq p))
	| Forallb (iswp, p1, p2) ->
	    Forallb (iswp, translate_eq p1, translate_eq p2)
	| Exists (id, n, pt, p) ->
	    Exists (id, n, ut, Pand (false, false, plunge fv (Tvar n) pt, translate_eq p))
	| Pnamed (s, p) ->
	    Pnamed (s, translate_eq p) 
	| p -> p in
      let rec lifted  l p =
	match l with [] -> p
	| (_,a)::q -> 
	    lifted q (Forall(false, Ident.create a, Ident.create a, ut, [], p))
      in
      Queue.add (Dgoal
		   (loc, expl, ident,
		    Env.empty_scheme
		      (List.map
			 (fun s -> match s with
			   Spred (id, p) -> Spred (id, translate_eq p)
			 | s -> s) cel,
		       translate_eq pred))) queue
	
let iter f =
  (* first the prelude *)
  List.iter f prelude;
  (* then the queue *)
  Queue.iter f queue
