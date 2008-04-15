(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-Fran�ois COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLI�TRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCH�                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann R�GIS-GIANAS                                                   *)
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

(*i $Id: encoding_rec.ml,v 1.16 2008/11/05 14:03:17 filliatr Exp $ i*)

open Options
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
	   Env.empty_scheme (Function ([ut; ut], ut))))::	(* The "sort" symbol *)
  (Dlogic (loc, prefix^"int",
	   Env.empty_scheme (Function ([], ut)))):: (* One synbol for each prefedined type *)
  (Dlogic (loc, prefix^"bool", 
	   Env.empty_scheme (Function ([], ut))))::
  (Dlogic (loc, prefix^"real", 
	   Env.empty_scheme (Function ([], ut))))::
  (Dlogic (loc, prefix^"unit", 
	   Env.empty_scheme (Function ([], ut))))::
  (Dlogic (loc, "equal"^suffix,
	   Env.empty_scheme (Predicate ([ut; ut])))):: (* One 'untyped' predicate of equality *)
(*   (Daxiom (loc, axiom "equal", *)
(* 	   let t = Ident.create "t" *)
(* 	   and x = Ident.create "x" *)
(* 	   and y = Ident.create "y" in *)
(* 	   Env.empty_scheme  *)
(* 	     (Forall (false, t, t, ut, *)
(* 	      Forall (false, x, x, ut, *)
(* 	      Forall (false, y, y, ut, *)
(* 	      (Pimplies  *)
(* 		 (false, *)
(* 		  (Papp (Ident.create ("equal"^suffix),  *)
(* 			 [Tapp (Ident.create (prefix^"sort"), *)
(* 				[Tvar t; Tvar x], []); *)
(* 			  Tapp (Ident.create (prefix^"sort"),  *)
(* 				[Tvar t; Tvar y], [])], [])), *)
(* 		  (Papp (Ident.t_eq, [Tvar x; Tvar y], [])))) *)
(* 		     )))))):: (\* and the corresponding axiom to link it with the built-in = predicate *\) *)
  []

(* A list of polymorphic constants *)
let poly_consts = ref []

let type_vars t =
  let rec aux acc t =
    match t with
    | PTvar ({type_val = None} as var) -> var::acc
    | PTvar {type_val = Some pt} -> aux acc pt
    | PTexternal (ptl, id) -> List.fold_left aux acc ptl
    | _ -> [] in
  aux [] t

let is_poly_cons ptl rt =
  let largs = List.fold_left List.append [] (List.map type_vars ptl)
  and lres = type_vars rt in
  List.exists (fun t -> not (List.mem t largs)) lres

let i_ex id = (* Big big HACK... *)
  match Ident.string id with
    "nil" -> Ident.create "list"
  | "null" -> Ident.create "pointer"
  | "pset_empty" -> Ident.create "pset"
  | _ -> Ident.create "unknown poly-const"
    
(* Function that plunges a term under its type information.  *)
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
	  (if debug then print_endline ("unknown vartype : "^s); s)
	in
	Tapp (Ident.create t, [], [])
    | PTvar {type_val = Some pt} -> leftt pt
    | PTexternal (ptl, id) -> Tapp (id, List.map (fun pt -> leftt pt) ptl, [])
  in
  Tapp (Ident.create (prefix^"sort"),
	[leftt pt; term],
	[])

(* The core *)

let queue = Queue.create ()

let reset () = 
  poly_consts := [];
  Queue.clear queue
      
let rec push d = 
  match d with
(* Dans le cas type on déclare la fonction correspondante, *)
(* d'arité correspondant à la taille du schéma de ce type  *)
  | Dtype (loc, vars, ident) ->
      Queue.add (Dlogic (loc, ident, 
			 Env.empty_scheme (Function (unify vars, ut)))) queue
(* Dans le cas logique, on redéfinit le prédicat/la fonction  *)
(* avec le type u, et on fait un prédicat/une fonction qui  *)
(* l'appelle avec les informations de type *)
  | Dlogic (loc, ident, arity) -> 
      let cpt = ref 0 in
      let fv = Env.Vset.fold
	  (fun tv acc -> cpt := !cpt + 1; (tv.tag, tvar^(string_of_int !cpt))::acc)
	  (arity.Env.scheme_vars) [] in
      (match arity.Env.scheme_type with 
      | Predicate ptl ->
	  let args = 
	    List.map
	      (fun t -> 
		Ident.create (let _ = cpt := !cpt + 1 in var^(string_of_int !cpt)), t)
	      ptl in
	  let terml = 
	    Papp (Ident.create (ident^suffix),
		  List.map (fun (id, t) -> plunge fv (Tvar id) t) args,
		  []) 
	  and termr =
	    Papp (Ident.create ident, List.map (fun (t, _) -> Tvar t) args, []) in
	  let rec lifted  l p =
	    match l with [] -> p
	    | a::q -> lifted q (Forall(false, a, a, ut, [], p)) in
	  let ax = Env.empty_scheme 
	      (lifted 
		 ((fst (List.split args))@(List.map (fun i -> Ident.create i) (snd (List.split fv))))
		 (Piff (terml, termr))) in
	  (Queue.add (Dlogic (loc, ident^suffix,
			      Env.empty_scheme (Predicate (unify ptl)))) queue;
	   Queue.add (Dlogic (loc, ident,
			      Env.empty_scheme (Predicate (unify ptl)))) queue;
	   Queue.add (Daxiom (loc, axiom ident, ax)) queue)
      | Function (ptl, rt) -> 
	  let _ =
	    if is_poly_cons ptl rt then
	      (if debug then
		 print_endline ("Constante polymorphe détectée : "^ident);
	       poly_consts := ident :: !poly_consts)
	    else () in
	  let args = 
	    List.map
	      (fun t -> 
		Ident.create (let _ = cpt := !cpt + 1 in var^(string_of_int !cpt)), t)
	      ptl in
	  let terml = 
	    Tapp (Ident.create (ident^suffix),
		  List.map (fun (id, t) -> plunge fv (Tvar id) t) args,
		  []) 
	  and termr =
	    plunge fv 
	      (Tapp (Ident.create ident, List.map (fun (t, _) -> Tvar t) args, []))
	      rt in
	  let rec lifted  l p =
	    match l with [] -> p
	    | a::q -> lifted q (Forall(false, a, a, ut, [], p)) in
	  let ax = Env.empty_scheme 
	      (lifted 
		 ((fst (List.split args))@(List.map (fun i -> Ident.create i) (snd (List.split fv))))
		 (Papp (Ident.t_eq, [terml;termr], []))) in
	  (Queue.add (Dlogic (loc, ident^suffix, 
			      Env.empty_scheme (Function (unify ptl, ut)))) queue;
	   Queue.add (Dlogic (loc, ident,
			      Env.empty_scheme (Function (unify ptl, ut)))) queue;
	   Queue.add (Daxiom (loc, axiom ident, ax)) queue))
(* A predicate definition can be handled as a predicate logic definition + an axiom *)
  | Dpredicate_def (loc, ident, pred_def_sch) ->
      let p = pred_def_sch.Env.scheme_type in
      let rec lifted_t l p =
	match l with [] -> p
	  | (a,t)::q -> lifted_t q (Forall(false, a, a, t, [], p)) 
      in
      let name = Ident.string ident in
      push (Dlogic (loc, name, 
		    (Env.generalize_logic_type 
		       (Predicate (snd (List.split (fst p)))))));
      push (Daxiom 
	      (loc, def name,
	       (Env.generalize_predicate 
		  (lifted_t (fst p) 
		     (Piff ((Papp (ident, 
				   List.map (fun (i,_) -> Tvar i) (fst p),
				   [])),
			    (snd p)))))))
  | Dinductive_def _ ->
      failwith "encoding rec: inductive def not yet supported"
(* A function definition can be handled as a function logic definition + an axiom *)
  | Dfunction_def (loc, ident, fun_def_sch) -> 
      let f = fun_def_sch.Env.scheme_type in
      let rec lifted_t l p =
	match l with 
	  | [] -> p
	  | (a,t)::q -> lifted_t q (Forall(false, a, a, t, [], p)) 
      in
      let (ptl, rt, t) = f in
      let name = Ident.string ident in
      push (Dlogic (loc, name, 
		    (Env.generalize_logic_type (Function (snd (List.split ptl), rt)))));
      push (Daxiom 
	      (loc, def name,
	       (Env.generalize_predicate
		  (lifted_t ptl
		     (Papp (Ident.t_eq,
			    [(Tapp (ident, 
				    List.map (fun (i,_) -> Tvar i) ptl, 
				    []));
				  t], []))))))
  | Daxiom (loc, ident, pred_sch) -> 
      let cpt = ref 0 in
      let fv = Env.Vset.fold
	  (fun tv acc -> cpt := !cpt + 1; (tv.tag, tvar^(string_of_int !cpt))::acc)
	  (pred_sch.Env.scheme_vars) [] in
      let rec translate_eq lv = function
	| Papp (id, tl, inst) when Ident.is_eq id ->
	    Papp (id, List.map (translate_term lv) tl, inst)
(* Papp (Ident.create (prefix^"equal"), List.map (translate_term lv) tl, inst) *)
	| Papp (id, tl, inst) ->
	    Papp (Ident.create (Ident.string id^suffix), List.map (translate_term lv) tl, inst)
	| Pimplies (iswp, p1, p2) ->
	    Pimplies (iswp, translate_eq lv p1, translate_eq lv p2)
	| Pif (t, p1, p2) ->
	    Pif (translate_term lv t, translate_eq lv p1, translate_eq lv p2)
	| Pand (iswp, issym, p1, p2) ->
	    Pand (iswp, issym, translate_eq lv p1, translate_eq lv p2)
	| Por (p1, p2) ->
	    Por (translate_eq lv p1, translate_eq lv p2)
	| Piff (p1, p2) ->
	    Piff (translate_eq lv p1, translate_eq lv p2)
	| Pnot p ->
	    Pnot (translate_eq lv p)
	| Forall (iswp, id, n, pt, tl, p) ->
	    let lv' = (n,pt)::lv in
	    let tl' = List.map (List.map (translate_pattern lv')) tl in
	    Forall (iswp, id, n, ut, tl', translate_eq lv' p)
	| Forallb (iswp, p1, p2) ->
	    Forallb (iswp, translate_eq lv p1, translate_eq lv p2)
	| Exists (id, n, pt, p) ->
	    Exists (id, n, ut, translate_eq ((n,pt)::lv) p)
	| Pnamed (s, p) ->
	    Pnamed (s, translate_eq lv p)
	| _ as d ->
	    d
      and translate_pattern lv = function
	| TPat t -> TPat (translate_term lv t)
	| PPat p -> PPat (translate_eq lv p)
      and translate_term lv = function
	| Tvar id -> plunge fv (Tvar id) (List.assoc id lv)
	| Tapp (id, tl, inst) when List.mem (Ident.string id) !poly_consts ->
	    if inst = [] then 
	      (print_string "probleme probleme"; 
	       Tapp (Ident.create (Ident.string id ^suffix), 
		     List.map (translate_term lv) tl, inst))
	    else
	      plunge fv (Tapp (id, List.map (translate_term lv) tl, inst))
		(PTexternal (inst, i_ex id)) (* HACK !! *)
	| Tapp (id, tl, inst) -> Tapp (Ident.create (Ident.string id ^suffix),
				       List.map (translate_term lv) tl, inst)
	| Tconst (ConstInt _) as t -> plunge fv t PTint
	| Tconst (ConstBool _) as t -> plunge fv t PTbool
	| Tconst (ConstUnit) as t -> plunge fv t PTunit
	| Tconst (ConstFloat _) as t -> plunge fv t PTreal
	| _ as t -> t in
      let rec lifted  l p =
	match l with [] -> p
	| (_,a)::q -> 
	    lifted q (Forall(false, Ident.create a, Ident.create a, ut, [], p))
      in
      Queue.add (Daxiom (loc, ident,
			 Env.empty_scheme 
 			   (lifted fv (translate_eq [] pred_sch.Env.scheme_type)))) queue
  | Dgoal (loc, expl, ident, s_sch) ->
      let cpt = ref 0 in
      let (cel, pred) = s_sch.Env.scheme_type in
      let fv = Env.Vset.fold
	  (fun tv acc -> cpt := !cpt + 1; (tv.tag, tvar^(string_of_int !cpt))::acc)
	  (s_sch.Env.scheme_vars) [] in
      let lookup id =
	let rec aux = function
	  | [] -> raise Not_found
	  | (Svar (i, pt))::q when id = i -> pt
	  | (Spred (_, _))::q | (Svar (_,_))::q -> aux q in
	aux cel in
      let rec translate_eq lv = function
	| Papp (id, tl, inst) when Ident.is_eq id ->
 	    Papp (id, List.map (translate_term lv) tl, inst)
(* 	    Papp (Ident.create (prefix^"equal"), List.map (translate_term lv) tl, inst) *)
	| Papp (id, tl, inst) ->
	    Papp (Ident.create (Ident.string id^suffix), List.map (translate_term lv) tl, inst)
	| Pimplies (iswp, p1, p2) ->
	    Pimplies (iswp, translate_eq lv p1, translate_eq lv p2)
	| Pif (t, p1, p2) ->
	    Pif (translate_term lv t, translate_eq lv p1, translate_eq lv p2)
	| Pand (iswp, issym, p1, p2) ->
	    Pand (iswp, issym, translate_eq lv p1, translate_eq lv p2)
	| Por (p1, p2) ->
	    Por (translate_eq lv p1, translate_eq lv p2)
	| Piff (p1, p2) ->
	    Piff (translate_eq lv p1, translate_eq lv p2)
	| Pnot p ->
	    Pnot (translate_eq lv p)
	| Forall (iswp, id, n, pt, tl, p) ->
	    let lv' = (n,pt)::lv in
	    let tl' = List.map (List.map (translate_pattern lv')) tl in
	    Forall (iswp, id, n, ut, tl', translate_eq lv' p)
	| Forallb (iswp, p1, p2) ->
	    Forallb (iswp, translate_eq lv p1, translate_eq lv p2)
	| Exists (id, n, pt, p) ->
	    Exists (id, n, ut, translate_eq ((n,pt)::lv) p)
	| Pnamed (s, p) ->
	    Pnamed (s, translate_eq lv p)
	| _ as d ->
	    d 
      and translate_pattern lv = function
	| TPat t -> TPat (translate_term lv t)
	| PPat p -> PPat (translate_eq lv p)
      and translate_term lv = function
	| Tvar id -> 
	    (try 
	      (plunge fv (Tvar id) (List.assoc id lv))
	    with 
	      Not_found -> plunge fv (Tvar id) (lookup id))
	| Tapp (id, tl, inst) when List.mem (Ident.string id) !poly_consts ->
	    if inst = [] then 
	      (print_string "probleme probleme"; 
	       Tapp (Ident.create (Ident.string id ^suffix), 
		     List.map (translate_term lv) tl, inst))
	    else
	      plunge fv (Tapp (id, List.map (translate_term lv) tl, inst)) 
		(PTexternal (inst, i_ex id)) (* HACK !! *)
	| Tapp (id, tl, inst) -> Tapp (Ident.create (Ident.string id ^suffix), 
				       List.map (translate_term lv) tl, inst)
	| Tconst (ConstInt _) as t -> plunge fv t PTint
	| Tconst (ConstBool _) as t -> plunge fv t PTbool
	| Tconst (ConstUnit) as t -> plunge fv t PTunit
	| Tconst (ConstFloat _) as t -> plunge fv t PTreal
	| _ as t -> t in
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
			   Spred (id, p) -> Spred (id, translate_eq [] p)
			 | s -> s) cel, 
		       translate_eq [] pred))) queue
	
let iter f =
  (* first the prelude *)
  List.iter f prelude;
  (* then the queue *)
  Queue.iter f queue
