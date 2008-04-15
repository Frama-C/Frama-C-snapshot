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

(* $Id: csymbol.ml,v 1.14 2008/11/05 14:03:14 filliatr Exp $ *)

(* TO DO:

   - implement [rewrite_pred_wrt_var]

*)

open Clogic
open Cabsint
open Pp

let debug = Coptions.debug
let debug_more = false

(* local types describing terms and predicates, sufficient for expressing
   the predicates that result from the integer analysis.
   They depend on a parameter ['v] that describes the type of a variable.
*)

type 'v int_term =
  | ITconstant of constant
  | ITvar of 'v
  | ITunop of term_unop * 'v int_term
  | ITbinop of 'v int_term * term_binop * 'v int_term
  | ITmax of 'v int_term list
  | ITmin of 'v int_term list
    (* used to translate an expression that has no counterpart in the small
       term language presented here. When computing an abstraction for
       a surrounding term, we will translate [ITany] to top. *)
  | ITany
      
type 'v int_predicate =
  | IPfalse
  | IPtrue
  | IPrel of 'v int_term * relation * 'v int_term
  | IPand of 'v int_predicate * 'v int_predicate
  | IPor of 'v int_predicate * 'v int_predicate
  | IPimplies of 'v int_predicate * 'v int_predicate
  | IPiff of 'v int_predicate * 'v int_predicate
  | IPnot of 'v int_predicate
  | IPfull_separated of 'v int_term * 'v int_term
    (* specific predicates to express (non-)nullity of pointer and
       (non-)nullity of char pointer read. These should be translated in some
       more specific predicates depending on the domain. *)
  | IPnull_pointer of 'v int_term
  | IPnot_null_pointer of 'v int_term
  | IPnull_char_pointed of 'v int_term * 'v int_term
  | IPnot_null_char_pointed of 'v int_term * 'v int_term
    (* used to translate an predicate that has no counterpart in the small
       predicate language presented here. When computing an abstraction for
       a surrounding predicate, we will translate [IPany] to top. *)
  | IPany

module type TERM = sig
  type var
  include ELEMENT_OF_CONTAINER with type t = var int_term
  val collect_term_vars : t -> var list
  val translate : (t * t) list -> t -> t
  val sub : t -> t -> t
  val add : t -> t -> t
  val minus : t -> t
end

module type PREDICATE = sig
  type var
  module T : TERM with type var = var
  include ELEMENT_OF_CONTAINER with type t = var int_predicate
  val explicit_pred : t -> t
  val rewrite_pred_wrt_var : t -> var -> int -> t
  val collect_predicate_vars : t -> var list
  val make_conjunct : t list -> t
  val get_conjuncts : t -> t list
  val make_implication : t -> t -> t
  val get_implicants : t -> t * t
  val translate : (T.t * T.t) list -> t -> t
end

module TermOfVariable (V : ELEMENT_OF_CONTAINER) 
  : TERM with type var = V.t =
struct
  
  type var = V.t
  type t = var int_term

  let equal = ( = )
  let compare = Pervasives.compare
  let hash = Hashtbl.hash

  let rec pretty fmt = function
    | ITconstant (IntConstant c | RealConstant c) -> 
	Format.fprintf fmt "%s" c
    | ITvar v            -> Format.fprintf fmt "%a" V.pretty v
    | ITunop (op,t)      -> Format.fprintf fmt "%s %a" 
	(Cprint.term_unop op) pretty t
    | ITbinop (t1,op,t2) ->  Format.fprintf fmt "%a %s %a" 
	pretty t1 (Cprint.term_binop op) pretty t2
    | ITmax tlist ->
	Format.fprintf fmt "max(%a)" (print_list comma pretty) tlist
    | ITmin tlist ->
	Format.fprintf fmt "min(%a)" (print_list comma pretty) tlist
    | ITany              -> Format.fprintf fmt "_"

  (* takes as input a term
     returns a list of variables occurring in this term, in the order 
     they appear, with possible repetitions
  *)
  let rec collect_term_vars t = match t with
    | ITconstant _ -> []
    | ITvar v -> [v]
    | ITunop (_,t1) -> collect_term_vars t1
    | ITbinop (t1,_,t2) -> collect_term_vars t1 @ (collect_term_vars t2)
    | ITmax tlist -> List.flatten (List.map collect_term_vars tlist)
    | ITmin tlist -> List.flatten (List.map collect_term_vars tlist)
    | ITany -> []

  let rec translate transl t = 
    if List.mem_assoc t transl then
      List.assoc t transl
    else
      match t with
	| ITconstant _ | ITvar _ | ITany -> 
	    t
	| ITunop (op,t1) -> 
	    ITunop (op,translate transl t1)
	| ITbinop (t1,op,t2) -> 
	    ITbinop (translate transl t1,op,translate transl t2)
	| ITmax tlist ->
	    ITmax (List.map (translate transl) tlist)
	| ITmin tlist ->
	    ITmin (List.map (translate transl) tlist)

  let rec sub t1 t2 = match t1,t2 with
    | t1,ITconstant (IntConstant "0") -> t1
    | ITconstant (IntConstant "0"),ITunop(Uminus,t2) -> t2
    | ITconstant (IntConstant "0"),t2 -> ITunop(Uminus,t2)
    | t1,ITunop(Uminus,t2) -> add t1 t2
    | _ -> ITbinop(t1,Bsub,t2)

  and add t1 t2 = match t1,t2 with
    | t1,ITconstant (IntConstant "0") -> t1
    | ITconstant (IntConstant "0"),t2 -> t2
    | t1,ITunop(Uminus,t2) -> sub t1 t2
    | _ -> ITbinop(t1,Badd,t2)

  let minus t = match t with
    | ITconstant (IntConstant s) -> 
	ITconstant (IntConstant (string_of_int (-(int_of_string s))))
    | _ -> ITunop(Uminus,t)

end

module PredicateOfVariable (V : ELEMENT_OF_CONTAINER) 
  (T : TERM with type var = V.t)
  : PREDICATE with type var = V.t =
struct
  
  type var = V.t
  type t = var int_predicate

  module T = T

  let equal = ( = )
  let compare = Pervasives.compare
  let hash = Hashtbl.hash

  let rec pretty fmt = function
    | IPfalse -> Format.fprintf fmt "false"
    | IPtrue -> Format.fprintf fmt "true"
    | IPrel (t1,rel,t2) -> Format.fprintf fmt "%a %s %a"
	T.pretty t1 (Cprint.relation rel) T.pretty t2
    | IPand (p1,p2) -> Format.fprintf fmt "%a && %a"
	pretty p1 pretty p2
    | IPor (p1,p2) -> Format.fprintf fmt "%a || %a"
	pretty p1 pretty p2
    | IPimplies (p1,p2) -> Format.fprintf fmt "%a => %a"
	pretty p1 pretty p2
    | IPiff (p1,p2) -> Format.fprintf fmt "%a <=> %a"
	pretty p1 pretty p2
    | IPnot p -> Format.fprintf fmt "! %a" pretty p
    | IPfull_separated (t1,t2) -> Format.fprintf fmt "full_separated(%a,%a)"
	T.pretty t1 T.pretty t2
    | IPnull_pointer t1 -> Format.fprintf fmt "%a == 0"
	T.pretty t1
    | IPnot_null_pointer t1 -> Format.fprintf fmt "%a != 0"
	T.pretty t1
    | IPnull_char_pointed (t1,t2) -> Format.fprintf fmt "%a[%a] == 0"
	T.pretty t1 T.pretty t2
    | IPnot_null_char_pointed (t1,t2) -> Format.fprintf fmt "%a[%a] != 0"
	T.pretty t1 T.pretty t2
    | IPany -> Format.fprintf fmt "_"

  (* explicit the more complex predicates for an easier treatment from
     the integer analysis: 
     - implication and equivalence are translated in more basic
     conjunction and disjunction (using the next item on negation)
     - negation is pushed inside sub-predicates
  *)
  let rec explicit_pred p = match p with
    | IPfalse | IPtrue | IPrel _ | IPany | IPfull_separated _ 
    | IPnull_pointer _ 
    | IPnot_null_pointer _ | IPnull_char_pointed _ | IPnot_null_char_pointed _
	-> p
    | IPand (p1,p2) -> 
	let ep1 = explicit_pred p1 in
	let ep2 = explicit_pred p2 in
	IPand (ep1,ep2)
    | IPor (p1,p2) -> 
	let ep1 = explicit_pred p1 in
	let ep2 = explicit_pred p2 in
	IPor (ep1,ep2)
    | IPimplies (p1,p2) ->
	(* strengthen the formula here, by stating that either [p1] does not
	   hold, or [p1] AND [p2] hold together *)
	explicit_pred (IPor(IPnot p1,IPand(p1,p2)))
    | IPiff (p1,p2) ->
	explicit_pred (IPand(IPimplies(p1,p2),IPimplies(p2,p1)))
    | IPnot p1 ->
	begin match explicit_pred p1 with
          | IPfalse -> IPtrue
          | IPtrue -> IPfalse
          | IPand (p3,p4) ->
	      let ep3 = explicit_pred (IPnot p3) in
	      let ep4 = explicit_pred (IPnot p4) in
	      IPor (ep3,ep4)
          | IPor (p3,p4) ->
	      let ep3 = explicit_pred (IPnot p3) in
	      let ep4 = explicit_pred (IPnot p4) in
	      IPand (ep3,ep4)
	  | IPnot p3 ->
	      p3
	  | IPrel (t3,op,t4) ->
	      let new_op = match op with
		| Lt -> Ge
		| Gt -> Le
		| Le -> Gt
		| Ge -> Lt
		| Eq -> Neq
		| Neq -> Eq
	      in
	      IPrel (t3,new_op,t4)
	  | IPimplies _ | IPiff _ -> 
	      (* those cases should not be returned by a call 
		 to [explicit_pred] *)
	      assert false
	  | IPany -> IPany
	  | IPfull_separated _ as psep -> psep
	  | IPnull_pointer t1 -> IPnot_null_pointer t1
	  | IPnot_null_pointer t1 -> IPnull_pointer t1
	  | IPnull_char_pointed (t1,t2) -> IPnot_null_char_pointed (t1,t2)
	  | IPnot_null_char_pointed (t1,t2) -> IPnull_char_pointed (t1,t2)
	end

  let rewrite_pred_wrt_var p _v _noccur = p	

  let rec collect_predicate_vars p = match p with
    | IPfalse | IPtrue | IPany -> 
	[]
    | IPnull_pointer t1 | IPnot_null_pointer t1 ->
	T.collect_term_vars t1
    | IPrel (t1,_,t2) | IPfull_separated (t1,t2) | IPnull_char_pointed (t1,t2)
    | IPnot_null_char_pointed (t1,t2) ->
	T.collect_term_vars t1 @ (T.collect_term_vars t2)
    | IPand (p1,p2) | IPor (p1,p2) | IPimplies (p1,p2) | IPiff (p1,p2) ->
	collect_predicate_vars p1 @ (collect_predicate_vars p2)
    | IPnot p1 -> 
	collect_predicate_vars p1

  let make_conjunct plist = 
    let rec make_sub p_acc plist = match p_acc,plist with
      | p_acc,[] -> p_acc
      | IPtrue,p :: plist | p,IPtrue :: plist -> make_sub p plist
      | p_acc,p :: plist -> make_sub (IPand (p_acc,p)) plist
    in
    make_sub IPtrue plist 

  let rec get_conjuncts p = match p with
    | IPand (p1,p2) -> get_conjuncts p1 @ (get_conjuncts p2)
    | _ -> [p]

  let make_implication lhs_p rhs_p = IPimplies (lhs_p,rhs_p)

  let get_implicants p = match p with
    | IPimplies (p1,p2) -> p1,p2
    | _ -> failwith "[get_implicants] expecting an implication"

  let rec translate transl p = match p with
    | IPfalse | IPtrue | IPany ->
	p
    | IPrel (t1,rel,t2) ->
	let tt1 = T.translate transl t1 in
	let tt2 = T.translate transl t2 in
	IPrel (tt1,rel,tt2)
    | IPand (p1,p2) ->
	let tp1 = translate transl p1 in
	let tp2 = translate transl p2 in
	IPand (tp1,tp2)
    | IPor (p1,p2) ->
	let tp1 = translate transl p1 in
	let tp2 = translate transl p2 in
	IPor (tp1,tp2)
    | IPimplies (p1,p2) -> 
	let tp1 = translate transl p1 in
	let tp2 = translate transl p2 in
	IPimplies (tp1,tp2)
    | IPiff (p1,p2) -> 
	let tp1 = translate transl p1 in
	let tp2 = translate transl p2 in
	IPiff (tp1,tp2)
    | IPnot p -> 
	let tp = translate transl p in
	IPnot tp
    | IPfull_separated (t1,t2) -> 
	let tt1 = T.translate transl t1 in
	let tt2 = T.translate transl t2 in
	IPfull_separated (tt1,tt2)
    | IPnull_pointer t1 -> 
	let tt1 = T.translate transl t1 in
	IPnull_pointer (tt1)
    | IPnot_null_pointer t1 -> 
	let tt1 = T.translate transl t1 in
	IPnot_null_pointer (tt1)
    | IPnull_char_pointed (t1,t2) -> 
	let tt1 = T.translate transl t1 in
	let tt2 = T.translate transl t2 in
	IPnull_char_pointed (tt1,tt2)
    | IPnot_null_char_pointed (t1,t2) -> 
	let tt1 = T.translate transl t1 in
	let tt2 = T.translate transl t2 in
	IPnot_null_char_pointed (tt1,tt2)
end

(* predicate variable interface. Same as VARIABLE, plus [translate_predicate]
   whose aim is to translate generic predicates such as [IPnull_pointer] in
   more specific ones. *)

module type PVARIABLE = sig
  include VARIABLE

  exception Introduce_variable of t

  (* containers based on [t] *)
  module S : Set.S with type elt = t
  module M : Map.S with type key = t
  module H : Hashtbl.S with type key = t

  (* modules for terms and predicates *)
  module T : TERM with type var = t
  module P : PREDICATE with type var = t

  (* containers based on terms *)
  module TS : Set.S with type elt = T.t
  module TM : Map.S with type key = T.t
  module TH : Hashtbl.S with type key = T.t

  (* containers based on predicates *)
  module PS : Set.S with type elt = P.t
  module PM : Map.S with type key = P.t
  module PH : Hashtbl.S with type key = P.t

  (* list of restrained variables may be used to better translate 
     the predicate *)
  val translate_predicate : t list -> P.t -> P.t
  (* generates a variable from a term *)
  val generate_variable : T.t -> t
end

module NPredicate =
struct

  module Self =  
  struct
    type t = Ctypes.ctype npredicate
    let equal = ( = )
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
  end

  include Self 

  module S = Set.Make (Self)

  let rec get_conjuncts p = match p.npred_node with
    | NPand (p1,p2) -> get_conjuncts p1 @ (get_conjuncts p2)
    | _ -> [p]

  let make_conjunct plist = 
    let rec make_sub p_acc plist = match p_acc,plist with
      | p_acc,[] -> p_acc
      | {npred_node=NPtrue},p :: plist | p,{npred_node=NPtrue} :: plist ->
	  make_sub p plist
      | p_acc,p :: plist -> 
	  make_sub {p_acc with npred_node = NPand (p_acc,p)} plist
    in
    let plist = List.flatten (List.map get_conjuncts plist) in
    let pset = List.fold_right S.add plist S.empty in
    (* list without duplicates *)
    let plist = S.fold (fun e l -> e::l) pset [] in
    match plist with 
      | [] -> failwith "[make_conjunct] expecting non-empty list"
      | p :: _prest as plist -> make_sub {p with npred_node=NPtrue} plist 

  let subtract p1 p2 =
    let p1list = get_conjuncts p1 in
    let p2list = get_conjuncts p2 in
    let plist = List.filter (fun p -> not (List.mem p p2list)) p1list in
    match plist with
      | [] -> { p1 with npred_node = NPtrue }
      | _ -> make_conjunct plist

end

module VarElimination (V : PVARIABLE) =
struct
  exception Not_Representable

  let rec destruct v p = match p with
    | ITconstant _ -> 0,p
    | ITvar v2 -> 
	if V.equal v v2 then 1,ITconstant (IntConstant "0")
	else 0,p
    | ITunop (Uminus,p1) ->
	let fact,term = destruct v p1 in -fact,V.T.minus term
    | ITunop _ ->
	raise Not_Representable
    | ITbinop (p1,Badd,p2) ->
	let fact1,term1 = destruct v p1 in
	let fact2,term2 = destruct v p2 in
	fact1+fact2,V.T.add term1 term2
    | ITbinop (p1,Bsub,p2) ->
	let fact1,term1 = destruct v p1 in
	let fact2,term2 = destruct v p2 in
	fact1-fact2,V.T.sub term1 term2
    | ITbinop _ -> 
	raise Not_Representable
    | ITany ->
	raise Not_Representable
    | ITmin _ | ITmax _ -> 
	raise Not_Representable

  let rec min_max v p = match p with
    | IPand (p1,p2) ->
	let min_p1,max_p1 = min_max v p1 in
	let min_p2,max_p2 = min_max v p2 in
	V.TS.union min_p1 min_p2,V.TS.union max_p1 max_p2
    | IPrel (t1,Le,t2) ->
	begin try
	  let lhs_vfact,lhs_term = destruct v t1 in
	  let rhs_vfact,rhs_term = destruct v t2 in
	  match lhs_vfact - rhs_vfact with
	    | 1 -> 
		V.TS.empty,
		V.TS.singleton (V.T.sub rhs_term lhs_term)
	    | -1 -> 
		V.TS.singleton (V.T.sub lhs_term rhs_term),
		V.TS.empty
	    | _ ->
		V.TS.empty,V.TS.empty
	with Not_Representable -> V.TS.empty,V.TS.empty end
    | IPrel (t1,Ge,t2) ->
	min_max v (IPrel (t2,Le,t1))
    | IPrel (t1,Lt,t2) ->
	(* since we are working with integer variables, t1 < t2 is equivalent
	   to t1 <= t2 - 1 *)
	min_max v 
	  (IPrel (t1,Le,V.T.sub t2 (ITconstant (IntConstant "1"))))
    | IPrel (t1,Gt,t2) ->
	(* since we are working with integer variables, t1 > t2 is equivalent
	   to t2 <= t1 - 1 *)
	min_max v 
	  (IPrel (t2,Le,V.T.sub t1 (ITconstant (IntConstant "1"))))
    | IPrel (t1,Eq,t2) ->
	min_max v (IPand (IPrel (t1,Le,t2), IPrel (t1,Ge,t2)))
    | IPrel (_,Neq,_) ->
	V.TS.empty,V.TS.empty
    | _ -> failwith "[min_max] expecting conjunct"

  let relate_individual_bounds min_t max_t =
    IPrel (min_t,Le,max_t)    

  let relate_bounds min_set max_set =
    V.TS.fold (fun min_t acc_l ->
		 V.TS.fold (fun max_t acc_l -> 
			      relate_individual_bounds min_t max_t :: acc_l
			   ) max_set acc_l
	      ) min_set []

  let transitivity v p = match p with
    | IPimplies (lhs_p,rhs_p) ->
	let min_lhs_tset,max_lhs_tset = min_max v lhs_p in
	let min_rhs_tset,max_rhs_tset = min_max v rhs_p in
	let trans1 =
	  V.TS.fold (fun min_t acc_p ->
		       V.TS.fold (fun max_t acc_p -> 
				    IPand (acc_p,IPrel (min_t,Le,max_t)))
			 max_lhs_tset acc_p 
		    ) min_rhs_tset IPtrue
	in
	let trans2 =
	  V.TS.fold (fun min_t acc_p ->
		       V.TS.fold (fun max_t acc_p -> 
				    IPand (acc_p,IPrel (min_t,Le,max_t)))
			 max_rhs_tset acc_p 
		    ) min_lhs_tset IPtrue
	in
	IPand (trans1,trans2)
    | _ -> failwith "[transitivity] expecting implication"

  let fourier_motzkin v ~full:pf p = 
    let lhs_p,rhs_p = match p with
      | IPimplies (lhs_p,rhs_p) -> lhs_p,rhs_p
      | _ -> failwith "[fourier_motzkin] expecting implication"
    in
    let lhs_full_p = match pf with
      | IPimplies (lhs_p,_) -> lhs_p
      | _ -> failwith "[fourier_motzkin] expecting implication"
    in
    (* min-max in minimized version *)
    let min_lhs_tset,max_lhs_tset = min_max v lhs_p in
    let min_rhs_tset,max_rhs_tset = min_max v rhs_p in
    (* get equalities from full version *)
    let min_lhs_full_tset,max_lhs_full_tset = min_max v lhs_full_p in
    let eq_lhs_tset = V.TS.inter min_lhs_full_tset max_lhs_full_tset in
    (* remove equal terms from min-max sets *)
    let min_lhs_tset = V.TS.diff min_lhs_tset eq_lhs_tset in
    let max_lhs_tset = V.TS.diff max_lhs_tset eq_lhs_tset in
    if not (V.TS.is_empty min_rhs_tset
	    || V.TS.is_empty min_lhs_tset) then
      if V.TS.cardinal min_lhs_tset = 1 then
	let min_plist = relate_bounds min_rhs_tset min_lhs_tset in
	V.P.make_conjunct min_plist
      else
	(* treating implication of the form:
	   a < x && b < x -> c < x
	   that we rewrite into:
	   max(a,b) < x -> c < x
	*)
	let max_t = ITmax (V.TS.fold (fun x l -> x::l) min_lhs_tset []) in
	let max_t = V.TS.add max_t V.TS.empty in
	let min_plist = relate_bounds min_rhs_tset max_t in
	V.P.make_conjunct min_plist
    else if not (V.TS.is_empty max_rhs_tset
		 || V.TS.is_empty max_lhs_tset) then
      if V.TS.cardinal max_lhs_tset = 1 then
	let max_plist = relate_bounds max_lhs_tset max_rhs_tset in
	V.P.make_conjunct max_plist
      else
	(* treating implication of the form:
	   a > x && b > x -> c > x
	   that we rewrite into:
	   min(a,b) > x -> c > x
	*)
	let min_t = ITmin (V.TS.fold (fun x l -> x::l) max_lhs_tset []) in
	let min_t = V.TS.add min_t V.TS.empty in
	let max_plist = relate_bounds min_t max_rhs_tset in
	V.P.make_conjunct max_plist
    else if not (V.TS.is_empty eq_lhs_tset) then
      let eqt = V.TS.min_elt eq_lhs_tset in
      if not (V.TS.is_empty min_rhs_tset) then
	let min_plist = relate_bounds min_rhs_tset (V.TS.singleton eqt) in
	V.P.make_conjunct min_plist
      else if not (V.TS.is_empty max_rhs_tset) then
	let max_plist = relate_bounds (V.TS.singleton eqt) max_rhs_tset in
	V.P.make_conjunct max_plist
      else IPtrue
    else IPtrue
      
end
