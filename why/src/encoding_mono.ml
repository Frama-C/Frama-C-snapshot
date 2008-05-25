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

open Cc
open Logic
open Logic_decl

let loc = Loc.dummy_floc
let debug = false
let f2s (s,l,b,e) = Printf.sprintf "File %s, line %d, characters %d-%d" s l b e

let prefix = fun s -> "c_"^s
(* let suffix = "_c" *)
(* let var = "x" *)
let tvar = "t"
(* let cpt = ref 0 *)
let injec c = c^"_injective"
(* let axiom c = c ^ "_to_" ^ (c^suffix) *)
let def c = "def_"^c

(* The names for the new function symbols *)
let sort =  prefix "sort"

let cname t =  match Misc.normalize_pure_type t with
  | PTint -> "int"
  | PTbool -> "bool"
  | PTreal -> "real"
  | PTunit -> "unit"
      (** TODO : better error handling here *)
  | _ -> failwith "this function should only be called on constant types"
let c2u t = (cname t)^"2u"
let s2c t = "s2"^(cname t)

(* This is the list of constant types which is handled as such *)
let htypes = [PTint; PTbool; PTreal; PTunit]

(* The monosorted stratified encoding introduces
   three new external types : a sort for syntactically 
   unsorted terms, one for sorted terms and one for 
   syntactic types *)
let utname = prefix "unsorted"
let stname = prefix "sorted"
let ttname = prefix "type"
let ut = PTexternal ([], Ident.create utname)
let st = PTexternal ([], Ident.create stname)
let tt = PTexternal ([], Ident.create ttname)

(* The prelude is a set of declarations that must be added
   to the context every time the encoding is used :*)
let eq_arity = (Ident.string Ident.t_eq, Env.empty_scheme (Predicate [st; st]))
let neq_arity = (Ident.string Ident.t_neq, Env.empty_scheme (Predicate [st; st]))
let arities = ref [eq_arity; neq_arity]

let prelude = [
  (* first the three new types *)
  (Dtype (loc, [], utname));
  (Dtype (loc, [], stname));
  (Dtype (loc, [], ttname))]
  (* the function symbols representing constant types *)
  @
 (List.map (fun t -> (Dlogic (loc, prefix (cname t),
			       Env.empty_scheme (Function ([], tt)))))
     htypes)
  (* the sorting and conversion functions *)
  @
  [(Dlogic (loc, sort, Env.empty_scheme (Function ([tt; ut], st))))]
  @
  (List.map (fun t ->
	       (Dlogic (loc, c2u t, 
			Env.empty_scheme (Function ([t], ut)))))
     htypes)
  @
  (List.map (fun t ->
	       (Dlogic (loc, s2c t,
			Env.empty_scheme (Function ([st], t)))))
     htypes)
  (* and the conversion axioms *)
  @
  (** \forall x: T .  s2T(sort(T, T2u(x))) = x  **)
  (List.map (fun t -> 
	       (Daxiom (loc, (s2c t)^"_inv_"^(c2u t),
	let x = Ident.create "x" in 
	let t_term = Tapp (Ident.create (prefix (cname t)), [], []) in
	let t2u_x = Tapp (Ident.create (c2u t), [Tvar x], []) in 
	let sort_t_t2u_x = Tapp (Ident.create sort, [t_term; t2u_x], []) in
	let lhs = Tapp (Ident.create (s2c t), [sort_t_t2u_x], []) in
	let peq = Papp (Ident.t_eq,[lhs;Tvar x], []) in
	let pat = [[TPat sort_t_t2u_x]] in
          Env.empty_scheme (Forall (false, x, x, t, pat, peq)))))
     htypes)
  @
  (** \forall x: U .  T2u(s2T(sort(T, x))) = x  **)
  (List.map (fun t -> 
	       (Daxiom (loc, (c2u t)^"_inv_"^(s2c t),
	let x = Ident.create "x" in 
	let t_term = Tapp (Ident.create (prefix (cname t)), [], []) in
	let sort_t_x = Tapp (Ident.create sort, [t_term; Tvar x], []) in
	let s2t_sort_t_x = Tapp (Ident.create (s2c t), [sort_t_x], []) in 
	let lhs = Tapp (Ident.create (c2u t), [s2t_sort_t_x], []) in
	let peq = Papp (Ident.t_eq,[lhs;Tvar x], []) in
          Env.empty_scheme (Forall (false, x, x, ut, [[TPat lhs]], peq)))))
     htypes)

(* Functions that replace polymorphic types by S,T,U and constants *)
let typify ptl = List.map (fun _ -> tt) ptl

let sortify t pt = 
  match pt with
    | PTint | PTbool | PTreal | PTunit -> pt
    | PTvar _ -> t
    | PTexternal (_,_) -> t

let monoify = List.map (sortify st)

let sort_of_c = function
  | ConstInt _ -> PTint
  | ConstBool _ -> PTbool
  | ConstUnit -> PTunit
  | ConstFloat _ -> PTreal

let is_const t = (* match Misc.normalize_pure_type t with *) match t with
  | PTint | PTbool | PTreal | PTunit -> true
  | _ -> false

(* Function that plunges a term under its type information. *)
(* Uses an assoc. list of tags -> idents for type variables *)
let plunge fv term pt =
  let rec leftt pt =
    match pt with
	PTint | PTbool | PTreal | PTunit ->
	  Tapp (Ident.create (prefix (cname pt)), [], [])
      | PTvar ({type_val = None} as var) -> 
	  let t = 
	    try List.assoc var.tag fv
	    with Not_found ->
	      let s = string_of_int var.tag in
		(print_endline ("[plunge] unknown vartype : "^s); 
		 Format.eprintf "term = %a@." Util.print_term term;
		 s)
	  in
	    Tvar (Ident.create t)
      | PTvar {type_val = Some pt} -> leftt pt
      | PTexternal (ptl, id) -> Tapp (id, List.map (fun pt -> leftt pt) ptl, [])
  in
    Tapp (Ident.create sort,[leftt pt; term],[])

let get_arity id =
  let arity =
    try List.assoc (Ident.string id) !arities
    with e -> (print_endline ("unknown arity :"^(Ident.string id))); raise e in
    match arity.Env.scheme_type with
	Function (ptl, rt) -> ptl, rt
      | Predicate ptl -> ptl, PTbool (* ce PTbool est arbitraire et inutilisÃ© *)

(* Ground instanciation of an arity (to be plunged under) *)
let instantiate_arity id inst =
  let arity =
    try List.assoc (Ident.string id) !arities
    with e -> (print_endline ("unknown arity :"^(Ident.string id))); raise e in
  let (vs, log_type) = Env.specialize_logic_type arity in
    if debug then 
      (print_string "\t{";
       Env.Vmap.iter (fun _ v -> Printf.printf "%d" v.tag) vs;
       print_endline "}");
    match log_type with
	Function (ptl, rt) ->
	  if debug then Printf.printf "Instantiate : %d vars - %d types\n"
	    (Env.Vmap.fold (fun _ _ n -> n + 1) vs 0) (List.length inst);
	  ignore 
	    (Env.Vmap.fold (fun _ v l -> 
			      (match l with [] -> []
				 | a::q -> (v.type_val <- Some a; q)))
	       vs (List.rev inst));
	  rt
      | Predicate ptl ->
	  ignore 
	    (Env.Vmap.fold (fun _ v l -> 
			      (match l with [] -> []
				 | a::q -> (v.type_val <- Some a; q)))
	       vs (List.rev inst));
	  PTbool

(* Translation of a term *)
(* [fv] is a map for type variables, [lv] for term variables,
   and [rt] is the type of this term in the arity of its
   direct superterm. *)
let rec translate_term fv lv rt = function
  | Tvar id as t ->
      let pt = try List.assoc id lv with e -> raise e in
	(match is_const rt, is_const pt with
	     true, true -> t
	   | true, false -> Tapp (Ident.create (s2c rt), [plunge fv t pt], [])
	   | false, true -> plunge [] (Tapp (Ident.create (c2u pt), [t], [])) pt
	   | false, false -> plunge fv t pt
	)
  | Tapp (id, tl, inst) ->
      let ptl, pt = get_arity id in
      let trans_term = Tapp (id, List.map2 (translate_term fv lv) ptl tl, []) in
      let ground_type = instantiate_arity id inst in
	(match is_const rt, is_const pt with
	     true, true -> trans_term
	   | true, false -> Tapp (Ident.create (s2c rt), 
				  [plunge fv trans_term rt], [])
	   | false, true -> plunge [] 
	       (Tapp (Ident.create (c2u pt), [trans_term], [])) pt
	   | false, false -> plunge fv trans_term ground_type
	)
  | Tconst (_) as t when is_const rt -> t
  | Tconst (c) as t -> 
      let pt = sort_of_c c in
	plunge fv (Tapp (Ident.create (c2u pt), [t], [])) (sort_of_c c)
  | Tderef id as t -> print_endline ("id in Tderef : "^(Ident.string id)); t
  | Tnamed(_,t) -> translate_term fv lv rt t

(* Generalizing a predicate scheme with foralls (can add a trigger) *)
(* This function is used to explicitely quantify over syntactic type 
   variables that were implicitely quantified at first. *)
let rec lifted l p t =
  match l with [] -> p
    | (_, s)::[] ->
	Forall(false, Ident.create s, Ident.create s, tt, t, p)
    | (_, s)::q ->
	Forall(false, Ident.create s, Ident.create s, tt, [], lifted q p t)
	
let rec lifted_t l p tr =
  match l with [] -> p
    | (a,t)::[] -> (Forall(false, a, a, t, tr, p))
    | (a,t)::q ->  (Forall(false, a, a, t, [], lifted_t q p tr))

let rec lifted_ctxt l cel =
  (List.map (fun (_,s) -> Svar(Ident.create s, tt)) l)@cel

(* Translation of a predicate *)
let rec translate_pred fv lv = function
(*   | Papp (id, [a; b], [t]) when Ident.is_eq id && is_const t -> *)
(*       Papp (id, [translate_term fv lv t a; translate_term fv lv t b], []) *)
  | Papp (id, tl, inst) ->
      let _ = instantiate_arity id inst in
      let arity,_ = get_arity id in
	Papp (id, List.map2 (translate_term fv lv) arity tl, [])
  | Pimplies (iswp, p1, p2) ->
      Pimplies (iswp, translate_pred fv lv p1, translate_pred fv lv p2)
  | Pif (t, p1, p2) ->
      Pif (translate_term fv lv PTbool t,
	   translate_pred fv lv p1, translate_pred fv lv p2)
  | Pand (iswp, issym, p1, p2) ->
      Pand (iswp, issym, translate_pred fv lv p1, translate_pred fv lv p2)
  | Por (p1, p2) ->
      Por (translate_pred fv lv p1, translate_pred fv lv p2)
  | Piff (p1, p2) ->
      Piff (translate_pred fv lv p1, translate_pred fv lv p2)
  | Pnot p ->
      Pnot (translate_pred fv lv p)
  | Forall (iswp, id, n, pt, tl, p) ->
      let lv' = (n,pt)::lv in
(*       let tl' = List.map (List.map (translate_pattern fv lv')) tl in *)
	Forall (iswp, id, n, sortify ut pt, [], translate_pred fv lv' p)
  | Forallb (iswp, p1, p2) ->
      Forallb (iswp, translate_pred fv lv p1, translate_pred fv lv p2)
  | Exists (id, n, pt, p) ->
      Exists (id, n, sortify ut pt, translate_pred fv ((n,pt)::lv) p)
  | Pnamed (s, p) ->
      Pnamed (s, translate_pred fv lv p)
  | _ as d -> d

(* and translate_pattern fv lv = function *)
(*   | TPat t -> TPat (translate_term fv lv t) *)
(*   | PPat p -> PPat (translate_pred fv lv p) *)

(* The core *)
let queue = Queue.create ()

let rec push d = 
  try (match d with
(* A type declaration is translated as new logical function, the arity of *)
(* which depends on the number of type variables in the declaration *)
  | Dtype (loc, vars, ident) ->
      Queue.add (Dlogic (loc, ident,
			 Env.empty_scheme (Function (typify vars, tt)))) queue
(* In the case of a logic definition, we redefine the logic symbol  *)
(* with types u and s, and its complete arity is stored for the encoding *)
  | Dlogic (loc, ident, arity) -> 
      arities := (ident, arity)::!arities;
      let newarity = match arity.Env.scheme_type with
	  Predicate ptl -> Predicate (monoify ptl)
	| Function (ptl, pt) -> Function (monoify ptl, sortify ut pt) in
	Queue.add (Dlogic (loc, ident, Env.empty_scheme newarity)) queue
(* A predicate definition can be handled as a predicate logic definition + an axiom *)
  | Dpredicate_def (loc, ident, pred_def_sch) ->
      let (argl, pred) = pred_def_sch.Env.scheme_type in
      let rootexp = (Papp (Ident.create ident, List.map (fun (i,_) -> Tvar i) argl, [])) in
	push (Dlogic (loc, ident, (Env.generalize_logic_type (Predicate (snd (List.split argl))))));
	push (Daxiom (loc, def ident, (Env.generalize_predicate
					 (lifted_t argl (Piff (rootexp, pred)) [[PPat rootexp]]))))
(* A function definition can be handled as a function logic definition + an axiom *)
  | Dfunction_def (loc, ident, fun_def_sch) ->
(*       let _ = print_endline ident in *)
      let (argl, rt, term) = fun_def_sch.Env.scheme_type in
      let rootexp = (Tapp (Ident.create ident, List.map (fun (i,_) -> Tvar i) argl, [])) in
	push (Dlogic (loc, ident, (Env.generalize_logic_type (Function (snd (List.split argl), rt)))));
	push (Daxiom (loc, def ident,
		      (Env.generalize_predicate
			 (lifted_t argl (Papp (Ident.t_eq, [rootexp; term], [])) [[TPat rootexp]]))))
(* Axiom definitions *)
  | Daxiom (loc, ident, pred_sch) ->
      let cpt = ref 0 in
      let fv = Env.Vset.fold
	(fun tv acc -> cpt := !cpt + 1; (tv.tag, tvar^(string_of_int !cpt))::acc)
	(pred_sch.Env.scheme_vars) [] in
      let new_axiom = Env.empty_scheme
	(lifted fv (translate_pred fv [] pred_sch.Env.scheme_type) []) in
	Queue.add (Daxiom (loc, ident, new_axiom)) queue
(* A goal is a sequent : a context and a predicate and both have to be translated *)
  | Dgoal (loc, expl, ident, s_sch) ->
      if debug then Printf.printf "Encoding goal %s, %s...\n" ident (f2s loc);
      let cpt = ref 0 in
      let fv = Env.Vset.fold
	(fun tv acc -> 
	  cpt := !cpt + 1; 
	  (tv.tag, tvar^(string_of_int !cpt))::acc)
	(s_sch.Env.scheme_vars) [] in
	if debug then
	  (Printf.printf "Goal environment :\n";
	   List.iter (fun (n,id) -> Printf.printf "\tIn env : %s tagged %d\n" id n) fv;
	   Printf.printf "=========\n");
      let (context, new_cel) =
	List.fold_left
	  (fun (acc_c, acc_new_cel) s -> 
	     match s with
		 Spred (id, p) -> 
		   (acc_c, (Spred (id, translate_pred fv acc_c p))::acc_new_cel)
	       | Svar (id, t) -> ((id,t)::acc_c, (Svar (id, sortify ut t))::acc_new_cel))
	  ([], [])
	  (fst (s_sch.Env.scheme_type)) in
	if debug then
	  (Printf.printf "Goal context :\n";
	   List.iter (fun ce -> match ce with 
			| Svar (id, pt) -> Printf.printf "\tvar %s : ??\n" (Ident.string id)
			| Spred(id, _) -> Printf.printf "\thyp %s : ...\n" (Ident.string id)
		     ) new_cel;
	   Printf.printf "=========\n");
      let new_sequent =
	Env.empty_scheme
	  (lifted_ctxt fv (List.rev new_cel),
	   translate_pred fv context (snd (s_sch.Env.scheme_type))) in
	Queue.add (Dgoal (loc, expl, ident, new_sequent)) queue)
  with Not_found -> 
    Format.eprintf "Exception caught in : %a\n" Util.print_decl d;
    raise Not_found

let iter f =
  (* first the prelude *)
  List.iter f prelude;
  (* then the queue *)
  Queue.iter f queue

let reset () = 
  arities := [eq_arity; neq_arity];
  Queue.clear queue
