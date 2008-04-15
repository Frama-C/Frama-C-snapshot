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

(*i $Id: red.ml,v 1.45 2008/11/05 14:03:18 filliatr Exp $ i*)

open Ast
open Logic
open Ident
open Misc
open Util
open Cc

(*s Reductions of interpretations. *)

(*s We proceed in two phases (simpler without de Bruijn indices).
    (1) first we rename bound variables, so that successive binders have 
        different names;
    (2) then we reduce, performing substitutions without possible captures *)

(*s Phase 1: Renaming of bound variables. Argument [fv] is the set of 
    already traversed binders (a set of identifiers) *)

let rec uniq_list f fv s = function
  | [] -> 
      [], fv, s
  | x :: l -> 
      let x',fv',s' = f fv s x in 
      let l',fv'',s'' = uniq_list f fv' s' l in
      x' :: l', fv'', s''

let rec uniq_tt fv s = function
  | TTarray tt -> 
      TTarray (uniq_tt fv s tt)
  | TTlambda (b, tt) ->
      let b',fv',s' = uniq_binder fv s b in TTlambda (b', uniq_tt fv' s' tt)
  | TTarrow (b, tt) -> 
      let b',fv',s' = uniq_binder fv s b in TTarrow (b', uniq_tt fv' s' tt)
  | TTtuple (bl, p) -> 
      let bl',fv',s' = uniq_binders fv s bl in
      TTtuple (bl', option_app (uniq_tt fv' s') p)
  | TTpred p ->
      TTpred (tsubst_in_predicate s p)
  | TTpure _ | TTSet as t -> 
      t
  | TTapp (tt, l) ->
      TTapp (uniq_tt fv s tt, List.map (uniq_tt fv s) l)
  | TTterm t ->
      TTterm (tsubst_in_term s t)

and uniq_binder fv s (id,b) =
  let b' = match b with 
    | CC_var_binder c -> CC_var_binder (uniq_tt fv s c)
    | CC_pred_binder c -> CC_pred_binder (tsubst_in_predicate s c)
    | CC_untyped_binder -> CC_untyped_binder
  in
  let id' = next_away id fv in
  if id' <> id then 
    (id',b'), Idset.add id' fv, Idmap.add id (Tvar id') s 
  else 
    (id,b'), Idset.add id fv, s

and uniq_binders fv s bl = uniq_list uniq_binder fv s bl

and uniq_pattern fv s = function
  | PPvariable b -> 
      let b',fv',s' = uniq_binder fv s b in PPvariable b',fv',s'
  | PPcons (id, pl) -> 
      let pl',fv',s' = uniq_list uniq_pattern fv s pl in PPcons (id,pl'),fv',s'

let rec uniq_cc fv s = function
  | CC_var x | CC_term (Tvar x) ->
      CC_term (try Idmap.find x s with Not_found -> Tvar x)
  | CC_letin (dep, bl, e1, e2) ->
      let bl',fv',s' = uniq_binders fv s bl in
      CC_letin (dep, bl', uniq_cc fv s e1, uniq_cc fv' s' e2)
  | CC_lam (b, e) ->
      let b',fv',s' = uniq_binder fv s b in
      CC_lam (b', uniq_cc fv' s' e)
  | CC_app (f, a) ->
      CC_app (uniq_cc fv s f, uniq_cc fv s a)
  | CC_if (a,b,c) ->
      CC_if (uniq_cc fv s a, uniq_cc fv s b, uniq_cc fv s c)
  | CC_tuple (al, po) ->
      CC_tuple (List.map (uniq_cc fv s) al, option_app (uniq_tt fv s) po)
  | CC_case (e, pl) ->
      let uniq_branch (p,e) = 
	let p',fv',s' = uniq_pattern fv s p in (p', uniq_cc fv' s' e)
      in
      CC_case (uniq_cc fv s e, List.map uniq_branch pl)
  | CC_term c ->
      CC_term (tsubst_in_term s c)
  | CC_hole (x, ty) ->
      CC_hole (x, tsubst_in_predicate s ty)
  | CC_type t ->
      CC_type (uniq_tt fv s t)
  | CC_any t ->
      CC_any (uniq_tt fv s t)

(*s Phase 2: we reduce. *)

(*s Occurrence in the range of a substitution *)

let in_rng id s =
  try
    Idmap.iter (fun _ t -> if occur_term id t then raise Exit) s; false
  with Exit ->
    true

(*s Traversing binders and substitution within CC types *)

let rec cc_subst_list f s = function
  | [] -> 
      [], s
  | x :: l -> 
      let x',s' = f s x in 
      let l',s'' = cc_subst_list f s' l in
      x' :: l', s''

let rec cc_subst_binder_type s = function
  | CC_var_binder c -> CC_var_binder (cc_type_subst s c)
  | CC_pred_binder c -> CC_pred_binder (tsubst_in_predicate s c)
  | CC_untyped_binder -> CC_untyped_binder

and cc_subst_binder s (id,b) = 
  (id, cc_subst_binder_type s b), Idmap.remove id s

and cc_subst_binders s = cc_subst_list cc_subst_binder s

and cc_type_subst s = function
  | TTarray tt -> 
      TTarray (cc_type_subst s tt)
  | TTlambda (b, tt) ->
      let b',s' = cc_subst_binder s b in TTlambda (b', cc_type_subst s' tt)
  | TTarrow (b, tt) -> 
      let b',s' = cc_subst_binder s b in TTarrow (b', cc_type_subst s' tt)
  | TTtuple (bl, p) -> 
      let bl',s' = cc_subst_binders s bl in
      TTtuple (bl', option_app (cc_type_subst s') p)
  | TTpred p ->
      TTpred (tsubst_in_predicate s p)
  | TTpure _ | TTSet as t -> 
      t
  | TTapp (tt, l) ->
      TTapp (cc_type_subst s tt, List.map (cc_type_subst s) l)
  | TTterm t ->
      TTterm (tsubst_in_term s t)

let rec cc_subst_pat s = function
  | PPvariable b -> 
      let b',s' = cc_subst_binder s b in PPvariable b', s'
  | PPcons (id, pl) -> 
      let pl', s' = cc_subst_list cc_subst_pat s pl in PPcons (id, pl'), s'

(*s copy of term [c] under binders [bl], with the necessary renamings *)

let copy_under_binders bl c =
  let vars = List.map fst bl in
  let fv = List.fold_right Idset.add vars Idset.empty in
  uniq_cc fv Idmap.empty c       

(*s Eta and iota redexes. *)

let is_eta_redex bl al =
  try
    List.for_all2
      (fun (id,_) t -> match t with CC_var id' -> id = id' | _ -> false)
      bl al
  with Invalid_argument "List.for_all2" -> 
    false

let is_term = function CC_term _ -> true | CC_var _ -> true | _ -> false

let is_iota_redex l1 l2 = 
  (List.length l1 = List.length l2) && List.for_all is_term l2

let rec iota_subst s = function
  | [], [] -> s
  | (id,_) :: l1, CC_term t :: l2 -> iota_subst (Idmap.add id t s) (l1, l2)
  | _ -> assert false

(*s Reduction. Substitution is done at the same time for greater efficiency *)

let rm_binders sp = List.fold_left (fun sp (id,_) -> Idmap.remove id sp) sp

let rec rm_pat_binders sp = function
  | PPvariable (id, _) -> Idmap.remove id sp
  | PPcons (_, pl) -> List.fold_left rm_pat_binders sp pl

let rec red sp s cct = 
  match cct with
  | CC_var x | CC_term (Tvar x) ->
      (try Idmap.find x sp 
       with Not_found -> CC_term (try Idmap.find x s with Not_found -> Tvar x))
  | CC_letin (dep, bl, e1, e2) ->
      (match red sp s e1 with
	 (* [let x = t1 in e2] *)
	 | CC_term t1 when List.length bl = 1 ->
	     red sp (Idmap.add (fst (List.hd bl)) t1 s) e2
         (* [let x = [_]_ in e2] *)
	 | CC_lam _ as re1 when List.length bl = 1 ->
	     red (Idmap.add (fst (List.hd bl)) re1 sp) s e2
	 (* [let (x1,...,xn) = (t1,...,tn) in e2] *)
	 | CC_tuple (al,_) when is_iota_redex bl al ->
	     red sp (iota_subst s (bl, al)) e2
	 | re1 ->
	     let bl',s' = cc_subst_binders s bl in
	     let sp' = rm_binders sp bl in
	     (match red sp' s' e2 with
		(* [let (x1,...,xn) = e1 in (x1,...,xn)] *)
		| CC_tuple (al,_) when is_eta_redex bl al ->
		    red sp s e1
		| re2 ->
		    CC_letin (dep, bl', re1, copy_under_binders bl' re2)))
  | CC_lam (b, e) ->
      let b',s' = cc_subst_binder s b in
      let sp' = rm_binders sp [b] in
      let e' = red sp' s' e in
      CC_lam (b', copy_under_binders [b'] e')
  | CC_app (f, a) ->
      (match red sp s f, red sp s a with
	 (* two terms *)
	 | CC_term tf, CC_term ta ->
	     CC_term (applist tf [ta])
	 (* beta-redex *)
	 | CC_lam ((x,_),e), CC_term ta ->
	     red sp (Idmap.add x ta s) e
	 | CC_lam ((x,_),e), ra ->
	     red (Idmap.add x ra sp) s e
	 | rf, ra -> 
	     CC_app (rf, ra))
  | CC_if (a, b, c) ->
      CC_if (red sp s a, red sp s b, red sp s c)
  | CC_case (e, pl) ->
      let red_branch (p,e) = 
	let p',s' = cc_subst_pat s p in 
	let sp' = rm_pat_binders sp p in
	p', red sp' s' e 
      in
      CC_case (red sp s e, List.map red_branch pl)
  | CC_tuple (al, po) ->
      CC_tuple (List.map (red sp s) al,	option_app (cc_type_subst s) po)
  | CC_term c ->
      CC_term (tsubst_in_term s c)
  | CC_hole (x, ty) ->
      CC_hole (x, tsubst_in_predicate s ty)
  | CC_type t ->
      CC_type (cc_type_subst s t)
  | CC_any t ->
      CC_any (cc_type_subst s t)

let red c = 
  let c' = uniq_cc Idset.empty Idmap.empty c in
  red Idmap.empty Idmap.empty c'

