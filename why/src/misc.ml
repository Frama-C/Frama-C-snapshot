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

(*i $Id: misc.ml,v 1.121 2008/04/10 14:43:57 filliatr Exp $ i*)

open Options
open Ident
open Logic
open Types 
open Ast
open Ptree
open Cc


(*s Utility functions. *)

let map_succeed f = 
  let rec map_f = function 
    | [] -> []
    | h :: t -> try let x = f h in x :: map_f t with Failure _ -> map_f t
  in 
  map_f 

let option_app = Option_misc.map

let option_iter = Option_misc.iter

let option_fold = Option_misc.fold

let a_value a = a.a_value
let a_values = List.map a_value

let list_of_some = function None -> [] | Some x -> [x]

let if_labelled f id = if is_at id then f (un_at id)

let difference l1 l2 =
  let rec diff = function
    | [] -> []
    | a::rem -> if List.mem a l2 then diff rem else a::(diff rem)
  in
  diff l1

let rec last = function
  | [] -> invalid_arg "last"
  | [x] -> x
  | _ :: l -> last l

let rec list_combine3 a b c = match a, b, c with
  | [], [], [] -> []
  | xa::ra, xb::rb, xc::rc -> (xa, xb, xc) :: list_combine3 ra rb rc
  | _ -> invalid_arg "list_combine3"

let rec list_first f = function
  | [] -> raise Exit
  | x :: l -> try f x with Exit -> list_first f l

let is_default_post a = match a.a_value with
  | Pvar id when id == Ident.default_post -> true
  | _ -> false

(*s Functions on names *)

type avoid = Ident.set

let renaming_of_ids avoid ids =
  let rec rename avoid = function
    | [] -> 
	[], avoid
    | x :: rem ->
	let al,avoid = rename avoid rem in
	let x' = Ident.next_away x avoid in
	(x,x')::al, Idset.add x' avoid
  in
  rename avoid ids

(*s hypotheses names *)

let next s r = function
  | Anonymous -> incr r; Ident.create (s ^ string_of_int !r)
  | Name id -> id

let reset_names_table = ref []
let reset_names () = List.iter (fun f -> f ()) !reset_names_table

let gen_sym_name s =
  let r = ref 0 in 
  reset_names_table := (fun () -> r := 0) :: !reset_names_table;
  next s r

let gen_sym s = let g = gen_sym_name s in fun () -> g Anonymous

let pre_name = gen_sym_name "Pre"
let post_name = gen_sym "Post"
let inv_name = gen_sym_name "Inv"
let test_name = gen_sym "Test"
let wp_name = gen_sym "WP"
let h_name = gen_sym_name "H_"
let bool_name = gen_sym "Bool"
let variant_name = gen_sym "variant"
let phi_name = gen_sym "rphi"
let for_name = gen_sym "for"
let label_name = let f = gen_sym "_label_" in fun () -> Ident.string (f ())
let fresh_hyp = gen_sym "HW_"
let fresh_axiom = gen_sym "AXIOM_"
let fresh_var = gen_sym "aux_"
let fresh_c_var = gen_sym "c_aux_"
let wf_name = gen_sym "wf"

let id_of_name = function Name id -> id | Anonymous -> default

let is_post id =
  let s = Ident.string id in
  String.length s >= 4 && String.sub s 0 4 = "Post"

let post_name_from = 
  let avoid = ref Idset.empty in
  reset_names_table := (fun () -> avoid := Idset.empty) :: !reset_names_table;
  function
    | Anonymous ->
	post_name ()
    | Name id when is_post id ->
	post_name ()
    | Name id -> 
	let id' = Ident.next_away id !avoid in
	avoid := Idset.add id' !avoid;
	id'

let warning s = Format.eprintf "warning: %s@\n" s
let wprintf loc f = 
  Format.eprintf "%awarning: " Loc.report_position loc; Format.eprintf f
let unlocated_wprintf f = 
  Format.eprintf "warning: "; Format.eprintf f

(*s Various utility functions. *)

let rec is_closed_pure_type = function
  | PTint | PTbool | PTreal | PTunit -> true
  | PTvar {type_val=None} -> false
  | PTvar {type_val=Some t} -> is_closed_pure_type t
  | PTexternal (ptl,_) -> List.for_all is_closed_pure_type ptl

let rec normalize_pure_type = function
  | PTvar { type_val = Some t } -> normalize_pure_type t
  | PTexternal (i, id) -> PTexternal (List.map normalize_pure_type i, id)
  | PTvar _ | PTint | PTbool | PTunit | PTreal as t -> t

let rationalize s =
  let n = String.length s in
  let i = String.index s '.' in
  let d = n - i - 1 in
  String.sub s 0 i ^ String.sub s (succ i) d, "1" ^ String.make d '0'

let is_mutable = function Ref _ -> true | _ -> false
let is_pure = function PureType _ -> true | _ -> false

let asst_app f x = { x with a_value = (f x.a_value); a_proof = None }

let post_app f (q,l) = (f q, List.map (fun (x,a) -> (x, f a)) l)

let optpost_app f = option_app (post_app f)

let asst_fold f x v = f x.a_value v
let post_fold f (q,l) v = 
  List.fold_right (fun (_,p) -> asst_fold f p) l (asst_fold f q v)
let optpost_fold f = option_fold (post_fold f)

let panonymous loc x = 
  { pa_name = Anonymous; pa_value = x; pa_loc = loc }
let anonymous loc x = 
  { a_name = Ident.anonymous; a_value = x; a_loc = loc; a_proof = None }
let wp_named loc x = 
  { a_name = wp_name (); a_value = x; a_loc = loc; a_proof = None }
let pre_named loc x = 
  { a_name = pre_name Anonymous; a_value = x; a_loc = loc; a_proof = None }
let post_named loc x = 
  { a_name = post_name (); a_value = x; a_loc = loc; a_proof = None }

let force_wp_name = option_app (fun a -> { a with a_name = wp_name () })

let force_name f a = { a with a_name = f a.a_name }

(***
    let force_post_name = option_app (fun (q,l) -> (force_name post_name q, l))

    let force_bool_name = 
    let f = function Name id -> id | Anonymous -> bool_name() in
    option_app (fun (q,l) -> (force_name f q, l))
***)

let force_loc l a = { a with a_loc = l }

let force_post_loc l (q,ql) = 
  (force_loc l q, List.map (fun (x,a) -> (x, force_loc l a)) ql)

(***
    let rec force_type_c_loc l c =
    { c with 
    c_result_type = force_type_v_loc l c.c_result_type;
    c_pre = List.map (force_loc l) c.c_pre;
    c_post = option_app (force_post_loc l) c.c_post }

    and force_type_v_loc l = function
    | Arrow (bl, c) -> Arrow (bl, force_type_c_loc l c)
    | (PureType _ | Ref _) as v -> v
***)

let default_post = anonymous Loc.dummy_position (Pvar Ident.default_post)

(* selection of postcondition's parts *)
let post_val = fst
let post_exn x (_,l) = List.assoc x l

let optpost_val = option_app post_val
let optpost_exn x = option_app (post_exn x)

(* substititution within some parts of postconditions *)
let val_app f (x,xl) = (asst_app f x, xl)
let exn_app x f (x,xl) = (x, List.map (fun (x,a) -> x, asst_app f a) xl)

let optval_app f = option_app (val_app f)
let optexn_app x f = option_app (exn_app x f)

let optasst_app f = option_app (asst_app f)

(*s Functions on terms and predicates. *)

let rec applist f l = match (f,l) with
  | f, [] -> f
  | Tvar id, l -> Tapp (id, l, [])
  | Tapp (id, l, il), l' -> Tapp (id, l @ l', il)
  | (Tconst _ | Tderef _), _ -> assert false
  | Tnamed(lab,t),l -> Tnamed(lab,applist t l)

let papplist f l = match (f,l) with
  | f, [] -> f
  | Pvar id, l -> Papp (id, l, [])
  | Papp (id, l, il), l' -> assert (il = []); Papp (id, l @ l', [])
  | _ -> assert false

let rec predicate_of_term = function
  | Tvar x -> Pvar x
  | Tapp (id, l, i) -> Papp (id, l, i)
  | _ -> assert false

let rec collect_term s = function
  | Tvar id | Tderef id -> Idset.add id s
  | Tapp (_, l, _) -> List.fold_left collect_term s l
  | Tconst _ -> s
  | Tnamed(_,t) -> collect_term s t

let rec collect_pred s = function
  | Pvar _ | Ptrue | Pfalse -> s
  | Papp (_, l, _) -> List.fold_left collect_term s l
  | Pimplies (_, a, b) | Pand (_, _, a, b) | Por (a, b) | Piff (a, b)
  | Forallb (_, a, b) -> 
      collect_pred (collect_pred s a) b
  | Pif (a, b, c) -> collect_pred (collect_pred (collect_term s a) b) c
  | Pnot a -> collect_pred s a
  | Forall (_, _, _, _, _, p) -> collect_pred s p
  | Exists (_, _, _, p) -> collect_pred s p
  | Pfpi (t, _, _) -> collect_term s t
  | Pnamed (_, p) -> collect_pred s p

let term_vars = collect_term Idset.empty
let predicate_vars = collect_pred Idset.empty
let assertion_vars a = predicate_vars a.a_value

let gen_post_vars assertion_vars (q,al) = 
  List.fold_left 
    (fun s (_,a) -> Idset.union s (assertion_vars a))
    (assertion_vars q)
    al

let post_vars = gen_post_vars predicate_vars
let apost_vars = gen_post_vars assertion_vars

let rec map_predicate f = function
  | Pimplies (w, a, b) -> Pimplies (w, f a, f b)
  | Pif (a, b, c) -> Pif (a, f b, f c)
  | Pand (w, s, a, b) -> Pand (w, s, f a, f b)
  | Por (a, b) -> Por (f a, f b)
  | Piff (a, b) -> Piff (f a, f b)
  | Pnot a -> Pnot (f a)
  | Forall (w, id, b, v, tl, p) -> Forall (w, id, b, v, tl, f p)
  | Exists (id, b, v, p) -> Exists (id, b, v, f p)
  | Forallb (w, a, b) -> Forallb (w, f a, f b)
  | Pnamed (n, a) -> Pnamed (n, f a)
  | Ptrue | Pfalse | Pvar _ | Papp _ | Pfpi _ as p -> p

let rec tsubst_in_term s = function
  | Tvar x | Tderef x as t -> 
      (try Idmap.find x s with Not_found -> t)
  | Tapp (x,l,i) -> 
      Tapp (x, List.map (tsubst_in_term s) l, i)
  | Tconst _ as t -> 
      t
  | Tnamed(lab,t) -> Tnamed(lab,tsubst_in_term s t)

let rec tsubst_in_predicate s = function
  | Papp (id, l, i) -> Papp (id, List.map (tsubst_in_term s) l, i)
  | Pif (a, b ,c) -> Pif (tsubst_in_term s a, 
			  tsubst_in_predicate s b, 
			  tsubst_in_predicate s c)
  | Pfpi (t, f1, f2) -> Pfpi (tsubst_in_term s t, f1, f2)
  | Forall (w, id, b, v, tl, p) -> 
      Forall (w, id, b, v, List.map (List.map (tsubst_in_pattern s)) tl,
	      tsubst_in_predicate s p)
  | p -> map_predicate (tsubst_in_predicate s) p

and tsubst_in_pattern s = function
  | TPat t -> TPat (tsubst_in_term s t)
  | PPat p -> PPat (tsubst_in_predicate s p)

(***
    let subst_in_term s = 
    tsubst_in_term (Idmap.map (fun id -> Tvar id) s)
***)
let rec subst_in_term s = function
  | Tvar x | Tderef x as t ->
      (try Tvar (Idmap.find x s) with Not_found -> t)
  | Tapp (x,l,i) -> 
      Tapp (x, List.map (subst_in_term s) l, i)
  | Tconst _ as t -> 
      t
  | Tnamed(lab,t) -> Tnamed(lab,subst_in_term s t)
      

(***
    let subst_in_predicate s = 
    tsubst_in_predicate (Idmap.map (fun id -> Tvar id) s)
    let subst_in_pattern s =
    tsubst_in_pattern (Idmap.map (fun id -> Tvar id) s)
***)

let rec subst_in_predicate s = function
  | Papp (id, l, i) -> Papp (id, List.map (subst_in_term s) l, i)
  | Pif (a, b ,c) -> Pif (subst_in_term s a, 
			  subst_in_predicate s b, 
			  subst_in_predicate s c)
  | Pfpi (t, f1, f2) -> Pfpi (subst_in_term s t, f1, f2)
  | Forall (w, id, b, v, tl, p) -> 
      Forall (w, id, b, v, List.map (List.map (subst_in_pattern s)) tl,
	      subst_in_predicate s p)
  | p -> map_predicate (subst_in_predicate s) p

and subst_in_pattern s = function
  | TPat t -> TPat (subst_in_term s t)
  | PPat p -> PPat (subst_in_predicate s p)

let subst_in_triggers s =
  List.map (List.map (subst_in_pattern s))

let subst_in_assertion s = asst_app (subst_in_predicate s)

let subst_one x t = Idmap.add x t Idmap.empty

let subst_onev = subst_one

let rec subst_many vl1 vl2 = match vl1, vl2 with
  | [], [] -> Idmap.empty
  | x1 :: l1, x2 :: l2 -> Idmap.add x1 x2 (subst_many l1 l2)
  | _ -> invalid_arg "subst_many"

let rec subst_manyv  vl1 vl2 = match vl1, vl2 with
  | [], [] -> Idmap.empty
  | x1 :: l1, x2 :: l2 -> Idmap.add x1 x2 (subst_manyv l1 l2)
  | _ -> invalid_arg "subst_manyv"


let rec unref_term = function
  | Tderef id -> Tvar id
  | Tapp (id, tl, i) -> Tapp (id, List.map unref_term tl, i)
  | Tvar _ | Tconst _ as t -> t
  | Tnamed(lab,t) -> Tnamed(lab,unref_term t)

let equals_true = function
  | Tapp (id, _, _) as t when is_relation id -> t
  | t -> Tapp (t_eq, [t; Tconst (ConstBool true)], [])

let negate id =
  if id == t_lt then t_ge
  else if id == t_le then t_gt 
  else if id == t_gt then t_le
  else if id == t_ge then t_lt
  else if id == t_eq then t_neq
  else if id == t_neq then t_eq
  else if id == t_lt_int then t_ge_int
  else if id == t_le_int then t_gt_int
  else if id == t_gt_int then t_le_int
  else if id == t_ge_int then t_lt_int
  else if id == t_eq_int then t_neq_int
  else if id == t_neq_int then t_eq_int
  else if id == t_lt_real then t_ge_real
  else if id == t_le_real then t_gt_real
  else if id == t_gt_real then t_le_real
  else if id == t_ge_real then t_lt_real
  else if id == t_eq_real then t_neq_real
  else if id == t_neq_real then t_eq_real
  else if id == t_eq_bool then t_neq_bool
  else if id == t_neq_bool then t_eq_bool 
  else if id == t_eq_unit then t_neq_unit
  else if id == t_neq_unit then t_eq_unit 
  else assert false

let make_int_relation id =
  if id == t_lt then t_lt_int
  else if id == t_le then t_le_int
  else if id == t_gt then t_gt_int
  else if id == t_ge then t_ge_int
  else id

let equals_false = function
  | Tapp (id, l, i) when is_relation id -> Tapp (negate id, l, i)
  | t -> Tapp (t_eq, [t; Tconst (ConstBool false)], [])

let rec mlize_type = function
  | PureType pt | Ref pt -> pt
  | Arrow _ -> assert false

(*s Substitutions *)

let rec type_c_subst s c =
  let {c_result_name=id; c_result_type=t; c_effect=e; c_pre=p; c_post=q} = c in
  let s' = Idmap.fold (fun x x' -> Idmap.add (at_id x "") (at_id x' "")) s s in
  { c_result_name = id;
    c_result_type = type_v_subst s t;
    c_effect = Effect.subst s e;
    c_pre = List.map (subst_in_predicate s) p;
    c_post = option_app (post_app (subst_in_predicate s')) q }

and type_v_subst s = function
  | Ref _ | PureType _ as v -> v
  | Arrow (bl,c) -> Arrow (List.map (binder_subst s) bl, type_c_subst s c)

and binder_subst s (n,v) = (n, type_v_subst s v)

(*s substitution of term for variables *)

let rec type_c_rsubst s c =
  { c_result_name = c.c_result_name;
    c_result_type = type_v_rsubst s c.c_result_type;
    c_effect = c.c_effect;
    c_pre = List.map (tsubst_in_predicate s) c.c_pre;
    c_post = option_app (post_app (tsubst_in_predicate s)) c.c_post }

and type_v_rsubst s = function
  | Ref _ | PureType _ as v -> v
  | Arrow (bl,c) -> Arrow(List.map (binder_rsubst s) bl, type_c_rsubst s c)

and binder_rsubst s (n,v) = (n, type_v_rsubst s v)

let type_c_of_v v =
  { c_result_name = Ident.result;
    c_result_type = v;
    c_effect = Effect.bottom; c_pre = []; c_post = None }

(* make_arrow bl c = (x1:V1)...(xn:Vn)c *)

let make_arrow bl c = match bl with
  | [] -> 
      invalid_arg "make_arrow: no binder"
  | _ -> 
      let rename (id,v) (bl,s) = 
	let id' = Ident.bound id in 
	(id',v) :: bl, 
	Idmap.add id id' (Idmap.add (at_id id "") (at_id id' "") s)
      in
      let bl',s = List.fold_right rename bl ([], Idmap.empty) in
      Arrow (bl', type_c_subst s c)

let rec make_binders_type_c s c =
  let {c_result_name=id; c_result_type=t; c_effect=e; c_pre=p; c_post=q} = c in
  { c_result_name = id;
    c_result_type = make_binders_type_v s t;
    c_effect = Effect.subst s e;
    c_pre = List.map (subst_in_predicate s) p;
    c_post = option_app (post_app (subst_in_predicate s)) q }

and make_binders_type_v s = function
  | Ref _ | PureType _ as v -> v
  | Arrow (bl,c) -> 
      let rename (id,v) (bl,s) = 
	let id' = Ident.bound id in 
	(id',v) :: bl, 
	Idmap.add id id' (Idmap.add (at_id id "") (at_id id' "") s)
      in
      let bl',s = List.fold_right rename bl ([], s) in
      Arrow (bl', make_binders_type_c s c)

let make_binders_type_v = make_binders_type_v Idmap.empty
let make_binders_type_c = make_binders_type_c Idmap.empty

(*s Smart constructors. *)

let ttrue = Tconst (ConstBool true)
let tfalse = Tconst (ConstBool false)
let tresult = Tvar Ident.result
let tvoid = Tconst ConstUnit

let relation op t1 t2 = Papp (op, [t1; t2], [])
let not_relation op = relation (negate op)
let lt = relation t_lt
let le = relation t_le
let gt = relation t_gt
let ge = relation t_ge
let ge_real = relation t_ge_real
let eq = relation t_eq
let neq = relation t_neq

let array_length id i = Tapp (array_length, [Tderef id], [i])

let lt_int = relation t_lt_int
let le_int = relation t_le_int

let pif a b c =
  if a = ttrue then b else if a = tfalse then c else Pif (a, b ,c)

let pand ?(is_wp=false) ?(is_sym=true) a b = 
  if a = Ptrue then b else if b = Ptrue then a else
    if a = Pfalse || b = Pfalse then Pfalse else
      Pand (is_wp, is_sym, a, b)

let wpand = pand ~is_wp:true

let pands ?(is_wp=false) ?(is_sym=true) = 
  List.fold_left (pand ~is_wp ~is_sym) Ptrue

let wpands = pands ~is_wp:true

let por a b =
  if a = Ptrue || b = Ptrue then Ptrue else
    if a = Pfalse then b else if b = Pfalse then a else
      Por (a, b)

let pors = List.fold_left por Pfalse

let pnot a =
  if a = Ptrue then Pfalse else if a = Pfalse then Ptrue else Pnot a

let pimplies ?(is_wp=false) p1 p2 = 
  if p2 = Ptrue then Ptrue 
  else if p1 = Ptrue then p2 
  else Pimplies (is_wp, p1, p2)

let wpimplies = pimplies ~is_wp:true

(*s [simplify] only performs simplications which are Coq reductions.
  Currently: only [if true] and [if false] *)

let rec simplify = function
  | Pif (Tconst (ConstBool true), a, _) -> simplify a
  | Pif (Tconst (ConstBool false), _, b) -> simplify b
  | p -> map_predicate simplify p

(*s Equalities *)

let rec eq_pure_type t1 t2 = match t1, t2 with
  | PTvar { type_val = Some t1 }, _ ->
      eq_pure_type t1 t2
  | _, PTvar { type_val = Some t2 } ->
      eq_pure_type t1 t2
  | PTexternal (l1, id1), PTexternal (l2, id2) ->
      id1 == id2 && List.for_all2 eq_pure_type l1 l2
  | _ ->
      t1 = t2

let rec eq_term t1 t2 = match t1, t2 with
  | Tapp (id1, tl1, i1), Tapp (id2, tl2, i2) ->
      id1 == id2 && List.for_all2 eq_term tl1 tl2 
	(* && List.for_all2 eq_pure_type i1 i2 ??? *)
  | _ -> 
      t1 = t2

let rec eq_predicate p1 p2 = match p1, p2 with
  | Papp (id1, tl1, i1), Papp (id2, tl2, i2) ->
      id1 == id2 && List.for_all2 eq_term tl1 tl2 
	(* && List.for_all2 eq_pure_type i1 i2 ??? *)
  | Pvar id1, Pvar id2 ->
      id1 == id2
  | Ptrue, Ptrue 
  | Pfalse, Pfalse ->
      true
  | Pand (_, _, a1, b1), Pand (_, _, a2, b2)
  | Por (a1, b1), Por (a2, b2)
  | Piff (a1, b1), Piff (a2, b2)
  | Forallb (_, a1, b1), Forallb (_, a2, b2)
  | Pimplies (_, a1, b1), Pimplies (_, a2, b2) ->
      eq_predicate a1 a2 && eq_predicate b1 b2
  | Pnot a1, Pnot a2 ->
      eq_predicate a1 a2
  | Pif (t1, a1, b1), Pif (t2, a2, b2) ->
      eq_term t1 t2 && eq_predicate a1 a2 && eq_predicate b1 b2
  | Pfpi (t1, a1, b1), Pfpi (t2, a2, b2) ->
      eq_term t1 t2 && a1 = a2 && b1 = b2
	  (* no alpha-equivalence *)
  | (Forall _ | Exists _), _
  | _, (Forall _ | Exists _) ->
      false
	(* equality up to names *)
  | Pnamed (_, p1), _ ->
      eq_predicate p1 p2
  | _, Pnamed (_, p2) ->
      eq_predicate p1 p2
	(* outside of the diagonal -> false *)
  | (Pfpi _ | Forallb _ | Pnot _ | Piff _ | Por _ |
	 Pand _ | Pif _ | Pimplies _ | Papp _ | Pvar _ | Pfalse | Ptrue),
      (Pfpi _ | Forallb _ | Pnot _ | Piff _ | Por _ |
	   Pand _ | Pif _ | Pimplies _ | Papp _ | Pvar _ | Pfalse | Ptrue) ->
      false

(*s Debug functions *)

module Size = struct

  let rec term = function
    | Tconst _ | Tvar _ | Tderef _ -> 1 
    | Tapp (_, tl, _) -> List.fold_left (fun s t -> s + term t) 1 tl
    | Tnamed(_,t) -> term t

  let rec predicate = function
    | Pvar _ | Ptrue | Pfalse -> 1
    | Papp (_, tl, _) -> List.fold_left (fun s t -> s + term t) 1 tl
    | Pand (_, _, p1, p2) 
    | Por (p1, p2) 
    | Piff (p1, p2) 
    | Pimplies (_, p1, p2) -> 1 + predicate p1 + predicate p2
    | Pif (t, p1, p2) -> 1 + term t + predicate p1 + predicate p2
    | Pnot p -> 1 + predicate p
    | Forall (_,_,_,_,_,p) -> 1 + predicate p
    | Exists (_,_,_,p) -> 1 + predicate p
    | Forallb (_,p1,p2) -> 1+ predicate p1 + predicate p2
    | Pnamed (_,p) -> predicate p
    | Pfpi (t,_,_) -> 1 + term t

  let assertion a = predicate a.a_value

  let postcondition (q,ql) = 
    assertion q + List.fold_left (fun s (_,q) -> s + assertion q) 0 ql

  let postcondition_opt = function None -> 0 | Some q -> postcondition q

end

(*s functions over CC terms *)

let cc_var x = CC_var x

let cc_term t = CC_term t

let rec cc_applist f l = match (f, l) with
  | f, [] -> f
  | f, x :: l -> cc_applist (CC_app (f, x)) l

let cc_lam bl = List.fold_right (fun b c -> CC_lam (b, c)) bl

let tt_var x = TTterm (Tvar x)

let tt_arrow = List.fold_right (fun b t -> TTarrow (b, t))

open Format

let file_formatter f cout =
  let fmt = formatter_of_out_channel cout in
  f fmt;
  pp_print_flush fmt ()

let do_not_edit_below ~file ~before ~sep ~after =
  let cout = 
    if not (Sys.file_exists file) then begin
      let cout = open_out file in
      file_formatter before cout;
      output_string cout ("\n" ^ sep ^ "\n\n");
      cout
    end else begin
      let file_bak = file ^ ".bak" in
      Sys.rename file file_bak;
      let cin = open_in file_bak in
      let cout = open_out file in
      begin try 
	while true do 
	  let s = input_line cin in
	  output_string cout (s ^ "\n");
	  if s = sep then raise Exit
	done
      with
	| End_of_file -> output_string cout (sep ^ "\n\n")
	| Exit -> output_string cout "\n"
      end;
      cout
    end
  in
  file_formatter after cout;
  close_out cout

let do_not_edit_above ~file ~before ~sep ~after =
  if not (Sys.file_exists file) then begin
    let cout = open_out file in
    file_formatter before cout;
    output_string cout ("\n" ^ sep ^ "\n\n");
    file_formatter after cout;
    close_out cout
  end else begin
    let file_bak = file ^ ".bak" in
    Sys.rename file file_bak;
    let cout = open_out file in
    file_formatter before cout;
    output_string cout ("\n" ^ sep ^ "\n\n");
    let cin = open_in file_bak in
    begin try 
      while input_line cin <> sep do () done;
      while true do 
	let s = input_line cin in output_string cout (s ^ "\n")
      done
    with End_of_file -> 
      ()
    end;
    close_out cout
  end

