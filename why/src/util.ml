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

(*i $Id: util.ml,v 1.152 2008/04/25 14:38:01 stoulsn Exp $ i*)

open Logic
open Ident
open Misc
open Types
open Cc
open Logic_decl
open Ast
open Env
open Rename
open Options



(*s References mentioned by a predicate *)

let is_reference env id =
  (is_in_env env id) && (is_mutable (type_in_env env id))

let predicate_now_refs env c =
  Idset.filter (is_reference env) (predicate_vars c)

let term_now_refs env c =
  Idset.filter (is_reference env) (term_vars c)


let labelled_reference env id =
  if is_reference env id then
    id
  else if is_at id then
    let uid,_ = Ident.un_at id in 
    if is_reference env uid then uid else failwith "caught"
  else
    failwith "caught"

let set_map_succeed f s = 
  Idset.fold 
    (fun x e -> try Idset.add (f x) e with Failure _ -> e) 
    s Idset.empty

let predicate_refs env c =
  set_map_succeed (labelled_reference env) (predicate_vars c)

let term_refs env c =
  set_map_succeed (labelled_reference env) (term_vars c)

let post_refs env q =
  set_map_succeed (labelled_reference env) (apost_vars q)

(*s Labels management *)

let gen_change_label f c =
  let ids = Idset.elements (predicate_vars c) in
  let s = 
    List.fold_left 
      (fun s id -> 
	 if is_at id then 
	   try Idmap.add id (f (un_at id)) s with Failure _ -> s
	 else s)
      Idmap.empty ids
  in
  subst_in_predicate s c

let erase_label l c =
  gen_change_label 
    (function (uid,l') when l = l' -> uid | _ -> failwith "caught") c

let change_label l1 l2 c =
  gen_change_label 
    (function (uid,l) when l = l1 -> at_id uid l2 | _ -> failwith "caught") c

let put_label_term env l t =
  let ids = term_refs env t in
  let s = 
    Idset.fold (fun id s -> Idmap.add id (at_id id l) s) ids Idmap.empty 
  in
  subst_in_term s t

let put_label_predicate env l p =
  let ids = predicate_refs env p in
  let s = 
    Idset.fold (fun id s -> Idmap.add id (at_id id l) s) ids Idmap.empty 
  in
  subst_in_predicate s p

let oldify env ef t =
  let ids = term_refs env t in
  let s =
    Idset.fold 
      (fun id s -> 
	 if Effect.is_write ef id then Idmap.add id (at_id id "") s else s)
      ids Idmap.empty
  in
  subst_in_term s t

let type_c_subst_oldify env x t k =
  let s = subst_one x t in 
  let s_old = subst_one x (oldify env k.c_effect t) in
  { c_result_name = k.c_result_name;
    c_result_type = type_v_rsubst s k.c_result_type;
    c_effect = k.c_effect;
    c_pre = List.map (tsubst_in_predicate s) k.c_pre;
    c_post = option_app (post_app (tsubst_in_predicate s_old)) k.c_post }

(*s shortcuts for typing information *)

let effect p = p.info.t_effect
let post p = p.info.t_post
let result_type p = p.info.t_result_type
let result_name p = p.t_result_name

let erase_exns ti = 
  { ti with t_effect = Effect.erase_exns ti.t_effect }

(*s [apply_pre] and [apply_post] instantiate pre- and post- conditions
    according to a given renaming of variables (and a date that means
    `before' in the case of the post-condition). *)

let make_subst before ren env ids =
  Idset.fold
    (fun id s ->
       if is_reference env id then
	 Idmap.add id (current_var ren id) s
       else if is_at id then
	 let uid,d = un_at id in
	 if is_reference env uid then begin
	   let d' = match d, before with
	     | "", None -> assert false
	     | "", Some l -> l
	     | _ -> d
	   in
	   Idmap.add id (var_at_date ren d' uid) s
	 end else
	   s
       else
	 s) 
    ids Idmap.empty

let apply_term ren env t =
  let ids = term_vars t in
  let s = make_subst None ren env ids in
  subst_in_term s t

let apply_assert ren env c =
  let ids = predicate_vars c in
  let s = make_subst None ren env ids in
  subst_in_predicate s c
 
let a_apply_assert ren env c =
  let ids = predicate_vars c.a_value in
  let s = make_subst None ren env ids in
  asst_app (subst_in_predicate s) c
 
let apply_post before ren env q =
  let ids = post_vars q in
  let s = make_subst (Some before) ren env ids in
  post_app (subst_in_predicate s) q
  
let a_apply_post before ren env q =
  let ids = apost_vars q in
  let s = make_subst (Some before) ren env ids in
  post_app (asst_app (subst_in_predicate s)) q
  
(*s [traverse_binder ren env bl] updates renaming [ren] and environment [env]
    as we cross the binders [bl]. *)

let rec traverse_binders env = function
  | [] -> 
      env
  | (id, v) :: rem ->
      traverse_binders (Env.add id v env) rem
	  
let initial_renaming env =
  let ids = Env.fold_all (fun (id,_) l -> id::l) env [] in
  update empty_ren "%init" ids


(*s Occurrences *)

let rec occur_term id = function
  | Tvar id' | Tderef id' -> id = id'
  | Tapp (_, l, _) -> List.exists (occur_term id) l
  | Tconst _ -> false
  | Tnamed(_,t) -> occur_term id t

let rec occur_pattern id = function
  | TPat t -> occur_term id t
  | PPat p -> occur_predicate id p

and occur_trigger id = List.exists (occur_pattern id)
and occur_triggers id = List.exists (occur_trigger id)

and occur_predicate id = function
  | Pvar _ | Ptrue | Pfalse -> false
  | Papp (_, l, _) -> List.exists (occur_term id) l
  | Pif (a, b, c) -> 
      occur_term id a || occur_predicate id b || occur_predicate id c
  | Forallb (_, a, b) 
  | Pimplies (_, a, b) 
  | Pand (_, _, a, b) 
  | Piff (a, b) 
  | Por (a, b) -> occur_predicate id a || occur_predicate id b
  | Pnot a -> occur_predicate id a
  | Forall (_,_,_,_,tl,a) -> occur_triggers id tl || occur_predicate id a
  | Exists (_,_,_,a) -> occur_predicate id a
  | Pfpi (t,_,_) -> occur_term id t
  | Pnamed (_, a) -> occur_predicate id a

let occur_assertion id a = occur_predicate id a.a_value

let gen_occur_post occur_assertion id = function 
  | None -> 
      false 
  | Some (q,l) -> 
      occur_assertion id q || 
      List.exists (fun (_,a) -> occur_assertion id a) l

let occur_post = gen_occur_post occur_assertion

let rec occur_type_v id = function
  | PureType _ | Ref _ -> false
  | Arrow (bl, c) -> occur_arrow id bl c

and occur_type_c id c =
  occur_type_v id c.c_result_type ||
  List.exists (occur_predicate id) c.c_pre ||
  Effect.occur id c.c_effect ||
  gen_occur_post occur_predicate id c.c_post 

and occur_arrow id bl c = match bl with
  | [] -> 
      occur_type_c id c
  | (id', v) :: bl' -> 
      occur_type_v id v || (id <> id' && occur_arrow id bl' c)

let forall ?(is_wp=false) x v ?(triggers=[]) p = match v with
  (* particular case: $\forall b:bool. Q(b) = Q(true) and Q(false)$ *)
  | PureType PTbool when not fast_wp ->
      let ptrue = tsubst_in_predicate (subst_one x ttrue) p in
      let pfalse = tsubst_in_predicate (subst_one x tfalse) p in
      Pand (true, true, simplify ptrue, simplify pfalse)
  | _ ->
      let n = Ident.bound x in
      let s = subst_onev x n in
      let p = subst_in_predicate s p in
      let subst_in_pattern s = function
	| TPat t -> TPat (subst_in_term s t)
	| PPat p -> PPat (subst_in_predicate s p) in
      let triggers = List.map (List.map (subst_in_pattern s)) triggers in
      Forall (is_wp, x, n, mlize_type v, triggers, p)

let pforall ?(is_wp=false) x v p =
  if p = Ptrue then Ptrue else forall ~is_wp x v p

let foralls ?(is_wp=false) =
  List.fold_right
    (fun (x,v) p -> if occur_predicate x p then forall ~is_wp x v p else p)

let foralls_many ?(is_wp=false) bl p =
  let l1,l2 = 
    List.fold_right 
      (fun ((x,v) as xv) (l1,l2) ->
	if occur_predicate x p then 
	  let n = Ident.bound x in xv::l1, n::l2
	else
	  l1,l2)
      bl ([],[])
  in
  let s = subst_manyv (List.map fst l1) l2 in
  let p = subst_in_predicate s p in
  List.fold_right2 
    (fun (x,v) n p -> Forall (is_wp, x, n, mlize_type v, [], p))
    l1 l2 p

let exists x v p =
  let n = Ident.bound x in
  let p = subst_in_predicate (subst_onev x n) p in
  Exists (x, n, mlize_type v, p)

let pexists x v p = 
  if p = Ptrue then Ptrue else exists x v p

let exists x v p = 
  let n = Ident.bound x in
  let p = subst_in_predicate (subst_onev x n) p in
  Exists (x, n, mlize_type v, p)

(* decomposing universal quantifiers, renaming variables on the fly *)

let decomp_forall ?(ids=Idset.empty) p = 
  let rec decomp bv = function
    | Forall (_,id,n,pt,_,p) ->
	decomp ((id,n,pt) :: bv) p
    | p ->
	let s,_,bv = 
	  List.fold_left 
	    (fun (s,ids,bv) (id,n,pt) -> 
	      let id' = next_away id ids in
	      let s = Idmap.add n id' s in
	      let ids = Idset.add id' ids in
	      s, ids, (id',pt) :: bv)
	    (Idmap.empty, Idset.union ids (predicate_vars p), []) bv
	in
	bv, subst_in_predicate s p
  in
  decomp [] p


(* misc. functions *)

let deref_type = function
  | Ref v -> v
  | _ -> invalid_arg "deref_type"

let dearray_type = function
  | PureType (PTexternal ([pt], id)) when id == Ident.farray -> pt
  | _ -> invalid_arg "dearray_type"

let decomp_type_c c = 
  ((c.c_result_name, c.c_result_type), c.c_effect, c.c_pre, c.c_post)

let decomp_kappa c = 
  ((c.t_result_name, c.t_result_type), c.t_effect, c.t_post)

let id_from_name = function Name id -> id | Anonymous -> (Ident.create "X")

(* [decomp_boolean c] returns the specs R and S of a boolean expression *)

let equality t1 t2 = Papp (t_eq, [t1; t2], [])
let nequality t1 t2 = Papp (t_neq, [t1; t2], [])

let tequality v t1 t2 = match v with
  | PureType PTint -> Papp (t_eq_int, [t1; t2], [])
  | PureType PTbool -> Papp (t_eq_bool, [t1; t2], [])
  | PureType PTreal -> Papp (t_eq_real, [t1; t2], [])
  | PureType PTunit -> Papp (t_eq_unit, [t1; t2], [])
  | _ -> Papp (t_eq, [t1; t2], [])

let distinct tl = 
  let rec make acc = function
    | [] | [_] -> 
	acc
    | t :: tl -> 
	let p = List.fold_left (fun p t' -> pand (nequality t t') p) acc tl in
	make p tl
  in
  make Ptrue tl

let decomp_boolean ({ a_value = c }, _) =
  (* q -> if result then q(true) else q(false) *)
  let ctrue = tsubst_in_predicate (subst_one Ident.result ttrue) c in
  let cfalse = tsubst_in_predicate (subst_one Ident.result tfalse) c in
  simplify ctrue, simplify cfalse

(*s [make_access env id c] Access in array id.
    Constructs [t:(array s T)](access_g s T t c ?::(lt c s)). *)

let add_ctx_vars =
  List.fold_left 
    (fun acc -> function Svar (id,_) -> Idset.add id acc | _ -> acc)












(**
   split : split predicate into a list of smaller predicates
   @param ctx : list of already split hypothesis (empty at first call)
   @param pol : polarity of the current predicate (1 at first call)
   @param matched : predicate to split
   @result l : l : list of split predicates 
**)
let rec split_one ctx pol = function
  | Pimplies (i, Por(h1,h2), c) when pol > 0-> (* split(h1 \/ h2 -> c) ~~~> split(h1->c) U split(h2->) *)
      let imp1=(split_one ctx pol (Pimplies (i, h1, c))) in 
      let imp2=(split_one imp1 pol (Pimplies (i, h2, c))) in 
      (*(List.append (List.append imp1 imp2) ctx)*)
      imp2
	
  | Pimplies (i, a, c) when pol > 0->          (* split( h -> c) ~~~> U_c':split(c) {h->c'}  *)
      let lc=split_one [] pol c in
      List.fold_left (fun ct c' -> Pimplies(i,a,c')::ct)  ctx lc
	
  | Pand (_,_, a, b) when pol > 0->            (* split(c1 /\ c2)  ~~~> split(c1) U split(c2)  *)
      let l1 = split_one ctx pol a in   
      (split_one l1 pol b)
	
  | Forall (a, id, n, t, b, c) when pol>0 ->   (* split(forall x.c)  ~~~>  U_c':split(c) {forall x.c'}   *)
      let lc'=split_one [] pol c in
      List.fold_left (fun ct c' -> Forall (a, id, n, t, b, c')::ct)  ctx lc'
	
  | Exists (a, b, t, c) when pol>0 ->          (* split(exists x.c)  ~~~> U_c':split-(c) {exists x.c'} *)
      let lc'=split_one [] (-pol) c in
      List.fold_left (fun ct c' -> Exists (a, b, t, c')::ct)  ctx lc'
	
  | Por (a, b) when pol < 0->                  (* split-(c1 \/ c2)  ~~~> split-(c1) U split-(c2)  *)
      let l1 = split_one ctx pol a in   
      (split_one l1 pol b)
	
  | c ->                                       (* split(c)  ~~~> {c}  *)
      c::ctx
	  

(**
   split : split each predicate of a list into a smaller predicates
   @param ctx : list of already split hypothesis
   @param pol : polarity of the current predicate
   @param matched : list of CC_context predicates to split
   @result l : l : list of split hypothesis 
**)
let rec split ctx my_fresh_hyp =
  function
    | [] -> ctx
    | Spred (h,p):: l -> split (List.fold_left (fun lp p -> Spred(my_fresh_hyp (),p)::lp ) ctx (split_one [] 1 p)) my_fresh_hyp l
    | c :: l -> c :: (split (c::ctx) my_fresh_hyp l)



(**
   intro : split a predicate into a list of hypothesis and a goal
   @param ctx : empty list
   @param p : predicate to split
   @param my_fresh_hyp : function that provide a fresh name of variable
   @result (l,g) : l : list of hypothesis / g : goal
**)
let intros ctx p my_fresh_hyp split_res =   
  (**
     introb : split a predicate into a list of hypothesis and a goal
     @param ctx : list of already split hypothesis
     @param pol : polarity of the current predicate
     @param matched : predicate
     @result (l,g) : l : list of hypothesis / g : goal
  **)
  let rec introb ctx pol = function
    | Forall (_, id, n, t, _, p) when pol>0 ->
	let id' =  Ident.bound id  in
	let sp  =  Misc.subst_onev  n  id' in
	let pp  =  Misc.subst_in_predicate sp p in
	introb (Svar (id', t) :: ctx) pol pp
    | Pimplies (_, a, b) when pol > 0-> 
	let h = my_fresh_hyp () in
	let (l,p) = introb [] (-pol) a in 
	let l' =  Spred(h, p)::ctx in 
	introb (List.append l l') pol b
    | Pand (_,_, a, b) when pol < 0-> 
       let (l1,p1) = introb ctx pol a in
       let h1 = my_fresh_hyp () in
       let l' = Spred(h1, p1)::l1 in
       let (l2,p2) = introb l' pol b in
       l2, p2 
    | Pnamed (_, p) ->
	introb ctx pol p
    | c -> 
	 ctx, c 
  in 
  let l,g = introb ctx 1 p in 
  if split_res then
    (split [] my_fresh_hyp l ), g
  else
    List.rev l, g   



let array_info env id =
  let ty = type_in_env env id in
  let v = dearray_type ty in
  (*i let ty_elem = trad_ml_type_v ren env v in
  let ty_array = trad_imp_type ren env ty in i*)
  v

let make_raw_access env (id,id') c =
  let pt = array_info env id in
  Tapp (Ident.access, [Tvar id'; c], [pt])

let make_pre_access env id c =
  let pt = array_info env id in
  let c = unref_term c in
  Pand (false, true, 
	le_int (Tconst (ConstInt "0")) c, lt_int c (array_length id pt))
      
let make_raw_store env (id,id') c1 c2 =
  let pt = array_info env id in
  Tapp (Ident.store, [Tvar id'; c1; c2], [pt])

(*s to build AST *)

let make_lnode loc p env k = 
  { desc = p; 
    info = { t_loc = loc; t_env = env; t_label = label_name ();
	     t_userlabel = "";
	     t_result_name = k.c_result_name; t_result_type = k.c_result_type;
	     t_effect = k.c_effect; 
	     t_post = optpost_app (anonymous loc) k.c_post } }

let make_var loc x t env =
  make_lnode loc (Expression (Tvar x)) env (type_c_of_v t)

let make_expression loc e t env =
  make_lnode loc (Expression e) env (type_c_of_v t)
    
let make_annot_bool loc b env =
  let k = type_c_of_v (PureType PTbool) in
  let b = Tconst (ConstBool b) in
  let q = equality (Tvar result) b in
  make_lnode loc (Expression b) env { k with c_post = Some (q, []) }

let make_void loc env = 
  make_expression loc (Tconst ConstUnit) (PureType PTunit) env 

let make_raise loc x v env =
  let k = type_c_of_v v in
  let ef = Effect.add_exn exit_exn Effect.bottom in
  make_lnode loc (Raise (x, None)) env { k with c_effect = ef }

let change_desc p d = { p with desc = d }

let force_post env q e = match q with
  | None -> 
      e
  | Some c ->
      let c = force_post_loc e.info.t_loc c in
      let ids = post_refs env c in
      let ef = Effect.add_reads ids e.info.t_effect in
      let i = { e.info with t_post = Some c; t_effect = ef } in
      { e with info = i }

let post_named c = 
  { a_value = c; a_name = post_name (); 
    a_loc = Loc.dummy_position; a_proof = None }

let create_postval c = Some (post_named c)

let create_post c = Some (post_named c, [])

(*s Pretty printers (for debugging purposes) *)

open Format
open Pp

let rec print_pure_type fmt = function
  | PTint -> fprintf fmt "int"
  | PTbool -> fprintf fmt "bool"
  | PTunit -> fprintf fmt "unit"
  | PTreal -> fprintf fmt "real"
  | PTexternal([],id) -> fprintf fmt "%a" Ident.print id
  | PTvar ({user=true; type_val=None} as v) -> 
      fprintf fmt "'%s" (type_var_name v)
  | PTvar {user=true; type_val=Some _} -> assert false
  | PTvar {tag=t; type_val=None} -> fprintf fmt "'a%d" t
  | PTvar {tag=t; type_val=Some pt} -> 
      if debug then
	fprintf fmt "'a%d(=%a)" t print_pure_type pt
      else
	print_pure_type fmt pt
  | PTexternal([t],id) -> 
      fprintf fmt "%a %a" print_pure_type t Ident.print id
  | PTexternal(l,id) -> fprintf fmt "(%a) %a" 
      (print_list comma print_pure_type) l
      Ident.print id

let rec print_logic_type fmt lt =
  let print_args = print_list comma print_pure_type in
  match lt with
    | Predicate l -> fprintf fmt "%a -> prop" print_args l
    | Function (l, pt) -> 
	fprintf fmt "%a -> %a" print_args l print_pure_type pt

let rec print_term fmt = function
  | Tconst (ConstInt n) -> 
      fprintf fmt "%s" n
  | Tconst (ConstBool b) -> 
      fprintf fmt "%b" b
  | Tconst ConstUnit -> 
      fprintf fmt "void" 
  | Tconst (ConstFloat (i,f,e)) -> 
      fprintf fmt "%s.%se%s" i f e
  | Tvar id -> 
      (if debug then Ident.dbprint else Ident.print) fmt id
  | Tderef id ->
      fprintf fmt "!%a" Ident.lprint id
  | Tapp (id, tl, []) -> 
      fprintf fmt "%s(%a)" (Ident.string id) (print_list comma print_term) tl
  | Tapp (id, tl, i) -> 
      fprintf fmt "%s(%a)[%a]" 
	(Ident.string id) (print_list comma print_term) tl
	(print_list comma print_pure_type) i
  | Tnamed(User lab,t) ->
      fprintf fmt "(%s : %a)" lab print_term t
  | Tnamed(Internal n,t) ->
      fprintf fmt "((%d) : %a)" n print_term t

let relation_string id =
  if id == t_lt || id == t_lt_int || id == t_lt_real then "<" 
  else if id == t_le || id == t_le_int || id == t_le_real then "<="
  else if id == t_gt || id == t_gt_int || id == t_gt_real then ">"
  else if id == t_ge || id == t_ge_int || id == t_ge_real then ">="
  else if is_eq id then "="
  else if is_neq id then "<>"
  else assert false


let rec print_pattern fmt = function
  | TPat t -> print_term fmt t
  | PPat p -> print_predicate fmt p

and print_triggers fmt = function
  | [] -> ()
  | tl -> fprintf fmt " [%a]" (print_list alt (print_list comma print_pattern))tl

and print_predicate fmt = function
  | Pvar id -> 
      (if debug then Ident.dbprint else Ident.print) fmt id
(*
  | Papp (id, [t1; t2]) when is_relation id ->
      fprintf fmt "%a %s %a" print_term t1 (relation_string id) print_term t2
*)
  | Papp (id, l, []) ->
      fprintf fmt "%s(%a)" (Ident.string id) (print_list comma print_term) l
  | Papp (id, l, i) ->
      fprintf fmt "%s(%a)[%a]" 
	(Ident.string id) (print_list comma print_term) l
	(print_list comma print_pure_type) i
  | Ptrue ->
      fprintf fmt "true"
  | Pfalse ->
      fprintf fmt "false"
  | Pimplies (_, a, b) -> 
      fprintf fmt "(@[%a ->@ %a@])" print_predicate a print_predicate b
  | Piff (a, b) -> 
      fprintf fmt "(@[%a <->@ %a@])" print_predicate a print_predicate b
  | Pif (a, b, c) -> 
      fprintf fmt "(@[if %a then@ %a else@ %a@])" 
	print_term a print_predicate b print_predicate c
  | Pand (_, _, a, b) ->
      fprintf fmt "(@[%a and@ %a@])" print_predicate a print_predicate b
  | Forallb (_, ptrue, pfalse) ->
      fprintf fmt "(@[forallb(%a,@ %a)@])" 
	print_predicate ptrue print_predicate pfalse
  | Por (a, b) ->
      fprintf fmt "(@[%a or@ %a@])" print_predicate a print_predicate b
  | Pnot a ->
      fprintf fmt "(not %a)" print_predicate a
  | Forall (_,_,b,v,tl,p) ->
      fprintf fmt "@[<hov 2>(forall %a:%a%a.@ %a)@]"
	(if debug then Ident.dbprint else Ident.print) b 
	print_pure_type v print_triggers tl print_predicate p
  | Exists (_,b,v,p) ->
      fprintf fmt "@[<hov 2>(exists %a:%a @ %a)@]" 
	(if debug then Ident.dbprint else Ident.print) b 
	print_pure_type v print_predicate p
  | Pfpi (t, (i1,f1,e1), (i2,f2,e2)) ->
      fprintf fmt "@[<hov 2>fpi(%a,@ %s.%se%s,@ %s.%se%s)@]" 
	print_term t i1 f1 e1 i2 f2 e2
  | Pnamed (User n, p) ->
      fprintf fmt "@[%s:@ %a@]" n print_predicate p
  | Pnamed (Internal n, p) ->
      fprintf fmt "@[(%d):@ %a@]" n print_predicate p

let print_assertion fmt a = 
  fprintf fmt "%a: %a" Ident.print a.a_name print_predicate a.a_value

let print_wp fmt = function
  | None -> fprintf fmt "<no weakest precondition>"
  | Some {a_value=p} -> print_predicate fmt p

let print_pre print_assertion fmt l = 
  if l <> [] then begin
    fprintf fmt "@[ ";
    print_list semi print_assertion fmt l;
    fprintf fmt " @]"
  end

let print_assertion fmt a = print_predicate fmt a.a_value

let print_exn print_assertion fmt (x,c) = 
  fprintf fmt "| %a => @[%a@]@," Ident.print x print_assertion c

let print_post print_assertion fmt = function
  | None -> 
      ()
  | Some (c,[]) -> 
      fprintf fmt "@[ %a @]" print_assertion c
  | Some (c, l) -> 
      fprintf fmt "@[ %a@ %a @]" print_assertion c 
	(print_list space (print_exn print_assertion)) l

let rec print_type_v fmt = function
  | Arrow (b,c) ->
      fprintf fmt "@[<hov 2>%a ->@ %a@]" 
	(print_list arrow pp_binder) b print_type_c c
  | v -> 
      print_type_v2 fmt v

and pp_binder fmt = function
  | id, v when id == Ident.anonymous -> 
      print_type_v2 fmt v
  | id, v ->
      fprintf fmt "@[%a:%a@]" Ident.print id print_type_v v

and print_type_v2 fmt = function
  | Ref v -> 
      fprintf fmt "@[%a@ ref@]" print_pure_type v
  | PureType pt -> 
      print_pure_type fmt pt
  | Arrow _ as v ->
      fprintf fmt "(%a)" print_type_v v

and print_type_c fmt c =
  let id = c.c_result_name in
  let v = c.c_result_type in
  let p = c.c_pre in
  let q = c.c_post in
  let e = c.c_effect in
  if e = Effect.bottom && p = [] && q = None then
    print_type_v fmt v
  else
    fprintf fmt "@[{%a}@ returns %a: %a@ %a@,{%a}@]" 
      (print_pre print_predicate) p
      Ident.print id 
      print_type_v v 
      Effect.print e
      (print_post print_predicate) q

let print_typing_info fmt c =
  let id = c.t_result_name in
  let v = c.t_result_type in
  let q = c.t_post in
  let e = c.t_effect in
  if e = Effect.bottom && q = None then
    print_type_v fmt v
  else
    fprintf fmt "@[{}@ returns %a: %a@ %a@,{%a}@]" 
      Ident.print id 
      print_type_v v 
      Effect.print e
      (print_post print_assertion) q

(*s Pretty-print of typed programs *)

let rec print_prog fmt (pre,p) = 
  let i = p.info in
  fprintf fmt "@[<hv>%s:@,@[{%a}@]@ @[%a@]@ @[{%a}@]@]" 
    i.t_label (print_pre print_assertion) pre 
    print_desc p.desc (print_post print_assertion) i.t_post

and print_expr fmt p =
  let i = p.info in
  if i.t_post = None then
    fprintf fmt "@[%s:@,%a@]" i.t_label print_desc p.desc
  else
    fprintf fmt "@[<hv>%s:@,{}@ @[%a@]@ @[{%a}@]@]" i.t_label
      print_desc p.desc (print_post print_assertion) i.t_post

and print_desc fmt = function
  | Var id -> 
      Ident.print fmt id
  | Seq (e1, e2) -> 
      fprintf fmt "@[%a@ %a@]" print_expr e1 print_expr e2
  | Loop (i, var, e) ->
      fprintf fmt 
	"loop @\n { invariant @[%a@] variant _ }@\n  @[%a@]@\ndone" 
	(print_option print_assertion) i print_expr e
  | If (p1, p2, p3) ->
      fprintf fmt "@[if %a then@ %a else@ %a@]" 
	print_expr p1 print_expr p2 print_expr p3
  | Lam (bl, p, e) -> 
      fprintf fmt "@[fun <bl> ->@ @[%a@]@\n  %a@]" 
	(print_pre print_assertion) p print_expr e
  | AppRef (p, x, k) -> 
      fprintf fmt "(@[%a %a ::@ %a@])" 
	print_expr p Ident.print x print_typing_info k
  | AppTerm (p, t, k) -> 
      fprintf fmt "(@[%a %a ::@ %a@])" 
	print_expr p print_term t print_typing_info k
  | LetRef (id, p1, p2) ->
      fprintf fmt "@[<hv>@[<hv 2>let %a =@ ref %a@ in@]@ %a@]" 
	Ident.print id print_expr p1 print_expr p2
  | LetIn (id, p1, p2) ->
      fprintf fmt "@[<hv>@[<hv 2>let %a =@ %a in@]@ %a@]" 
	Ident.print id print_expr p1 print_expr p2
  | Rec (id, bl, v, var, p, e) ->
      fprintf fmt "rec %a : <bl> %a { variant _ } =@ %a@\n%a" 
	Ident.print id print_type_v v 
	(print_pre print_assertion) p print_expr e
  | Raise (id, None) ->
      fprintf fmt "raise %a" Ident.print id
  | Raise (id, Some p) ->
      fprintf fmt "@[<hv>raise@ (@[<hv>%a@ %a@])@]" 
	Ident.print id print_expr p
  | Try (p, hl) ->
      fprintf fmt "@[<hv>try@ %a@ with %a@ end@]" print_expr p 
	(print_list alt print_handler) hl
  | Expression t -> 
      print_term fmt t
  | Absurd ->
      fprintf fmt "absurd"
  | Any k ->
      fprintf fmt "[%a]" print_type_c k
  | Assertion (b, l, p) ->
      fprintf fmt "@[{%a}@ %a@]" 
	(print_pre print_assertion) l print_expr p
  | Post (e, q, Transparent) ->
      fprintf fmt "@[(%a@ { %a })@]" 
	print_expr e (print_post print_assertion) (Some q)
  | Post (e, q, Opaque) ->
      fprintf fmt "@[(%a@ {{ %a }})@]" 
	print_expr e (print_post print_assertion) (Some q)
  | Label (s, e) ->
      fprintf fmt "@[%s:@ %a@]" s print_expr e

and print_handler fmt ((id, a), e) = match a with
  | None -> 
      fprintf fmt "%a ->@ %a" Ident.print id print_expr e
  | Some a -> 
      fprintf fmt "%a %a ->@ %a" Ident.print id Ident.print a print_expr e

and print_cast fmt = function
  | None -> ()
  | Some v -> fprintf fmt ": %a" print_type_v v

(*s Pretty-print of cc-terms (intermediate terms) *)

let print_pred_binders = ref true
let print_var_binders = ref false

let rec print_cc_type fmt = function
  | TTpure pt -> 
      print_pure_type fmt pt
  | TTarray t -> 
      fprintf fmt "(array %a)" print_cc_type t
  | TTlambda (b, t) ->
      fprintf fmt "[%a]%a" print_binder b print_cc_type t
  | TTarrow (b, t) -> 
      fprintf fmt "(%a)%a" print_binder b print_cc_type t
  | TTtuple (bl, None) -> 
      fprintf fmt "{%a}" print_tuple bl
  | TTtuple (bl, Some q) -> 
      fprintf fmt "{%a | %a}" print_tuple bl print_cc_type q
  | TTpred p ->
      print_predicate fmt p
  | TTapp (tt, l) ->
      fprintf fmt "(%a %a)" print_cc_type tt (print_list space print_cc_type) l
  | TTterm t ->
      print_term fmt t
  | TTSet -> 
      fprintf fmt "Set"

and print_tuple fmt = print_list comma print_binder fmt

and print_binder fmt (id,b) = 
  Ident.print fmt id;
  match b with
    | CC_pred_binder p -> 
	if !print_pred_binders then fprintf fmt ": %a" print_predicate p
    | CC_var_binder t -> 
	if !print_var_binders then fprintf fmt ": %a" print_cc_type t
    | CC_untyped_binder -> 
	()

let rec print_cc_pattern fmt = function
  | PPvariable (id, _) -> 
      Ident.print fmt id
  | PPcons (id, pl) -> 
      fprintf fmt "(%a %a)" 
	Ident.print id (print_list space print_cc_pattern) pl

let print_case_pred fmt (x,_) = Ident.print fmt x

let rec print_cc_term fmt = function
  | CC_var id -> 
      fprintf fmt "%s" (Ident.string id)
  | CC_letin (_,bl,c,c1) ->
      fprintf fmt "@[@[<hov 2>let %a =@ %a in@]@\n%a@]"
      (print_list comma print_binder) bl
      print_cc_term c print_cc_term c1
  | CC_lam (b,c) ->
      fprintf fmt "@[<hov 2>[%a]@,%a@]" print_binder b print_cc_term c
  | CC_app (f,a) ->
      fprintf fmt "@[<hov 2>(%a@ %a)@]" print_cc_term f print_cc_term a
  | CC_tuple (cl,_) ->
      fprintf fmt "@[<hov 2>(%a)@]" (print_list comma print_cc_term) cl
  | CC_if (b,e1,e2) ->
      fprintf fmt "@[if "; print_cc_term fmt b; fprintf fmt " then@\n  ";
      hov 0 fmt (print_cc_term fmt) e1;
      fprintf fmt "@\nelse@\n  ";
      hov 0 fmt (print_cc_term fmt) e2;
      fprintf fmt "@]"
  | CC_case (e,pl) ->
      fprintf fmt "@[<v>match %a with@\n  @[%a@]@\nend@]" 
	print_cc_term e (print_list newline print_case) pl
  | CC_term c ->
      fprintf fmt "@["; print_term fmt c; fprintf fmt "@]"
  | CC_hole (_, c) ->
      fprintf fmt "@[(?:@ "; print_predicate fmt c; fprintf fmt ")@]"
  | CC_type t ->
      print_cc_type fmt t
  | CC_any t ->
      fprintf fmt "@[(any(%a))@]" print_cc_type t

and print_binders fmt bl =
  print_list nothing (fun fmt b -> fprintf fmt "[%a]" print_binder b) fmt bl

and print_case fmt (p,e) =
  fprintf fmt "@[<hov 2>| %a =>@ %a@]" print_cc_pattern p print_cc_term e

let print_subst fmt =
  Idmap.iter
    (fun id t -> fprintf fmt "%a -> %a@\n" Ident.print id print_term t)

let print_cc_subst fmt =
  Idmap.iter
    (fun id t -> fprintf fmt "%a -> %a@\n" Ident.print id print_cc_term t)


let print_env fmt e =
  fold_all (fun (id, v) () -> fprintf fmt "%a:%a, " Ident.dbprint id 
		print_type_v v) e ()

(*s For debugging purposes *)

open Ptree

let rec print_ptree fmt p = match p.pdesc with
  | Svar x -> Ident.print fmt x
  | Sderef x -> fprintf fmt "!%a" Ident.print x
  | Sseq (p1, p2) ->
      fprintf fmt "@[%a;@ %a@]" print_ptree p1 print_ptree p2
  | Sloop ( _, _, p2) ->
      fprintf fmt "loop@\n  @[%a@]@\ndone" print_ptree p2
  | Sif (p1, p2, p3) ->
      fprintf fmt "@[<hv 2>if %a then@ %a else@ %a@]" 
	print_ptree p1 print_ptree p2 print_ptree p3
  | Slam (bl, pre, p) ->
      fprintf fmt "@[<hov 2>fun <...> ->@ %a@]" print_ptree p
  | Sapp (p, a) ->
      fprintf fmt "(%a %a)" print_ptree p print_ptree a
  | Sletref (id, e1, e2) -> 
      fprintf fmt "@[let %a = ref %a in@ %a@]" 
	Ident.print id print_ptree e1 print_ptree e2
  | Sletin (id, e1, e2) ->
      fprintf fmt "@[let %a = %a in@ %a@]"
	Ident.print id print_ptree e1 print_ptree e2
  | Srec _ ->
      fprintf fmt "<Srec>"
  | Sraise (x, None, _) -> 
      fprintf fmt "raise %a" Ident.print x
  | Sraise (x, Some e, _) -> 
      fprintf fmt "raise (%a %a)" Ident.print x print_ptree e
  | Stry (e, hl) ->
      fprintf fmt "@[<v>try %a with@\n  @[%a@]@\nend@]" 
	print_ptree e (print_list newline print_phandler) hl
  | Sconst c -> print_term fmt (Tconst c)
  | Sabsurd _ -> fprintf fmt "<Sabsurd>"
  | Sany _ -> fprintf fmt "<Sany>"
  | Spost (e,q,Transparent) -> fprintf fmt "@[%a@ {...}@]" print_ptree e
  | Spost (e,q,Opaque) -> fprintf fmt "@[%a@ {{...}}@]" print_ptree e
  | Sassert (p,e) -> fprintf fmt "@[{...}@ %a@]" print_ptree e
  | Slabel (l,e) -> fprintf fmt "@[%s: %a@]" l print_ptree e

and print_phandler fmt = function
  | (x, None), e -> 
      fprintf fmt "| %a => %a" Ident.print x print_ptree e
  | (x, Some y), e -> 
      fprintf fmt "| %a %a => %a" Ident.print x Ident.print y print_ptree e

let print_external fmt b = if b then fprintf fmt "external "

let print_decl fmt = function
  | Program (_,id, p) -> fprintf fmt "let %a = %a" Ident.print id print_ptree p
  | Parameter (_, e, ids, v) -> 
      fprintf fmt "%aparameter <...>" print_external e
  | Exception (_, id, pto) -> fprintf fmt "exception %a <...>" Ident.print id
  | Logic (_, e, ids, lt) -> 
      fprintf fmt "%alogic %a : <...>" print_external e 
	(print_list comma Ident.print) ids
  | Axiom (_, id, p) ->
      fprintf fmt "axiom %a : <...>" Ident.print id
  | Goal (_, id, p) ->
      fprintf fmt "assert %a : <...>" Ident.print id
  | Predicate_def (_, id, _, _) ->
      fprintf fmt "predicate %a <...>" Ident.print id
  | Function_def (_, id, _, _, _) ->
      fprintf fmt "function %a <...>" Ident.print id
  | TypeDecl (_, e, _, id) ->
      fprintf fmt "%atype %a" print_external e Ident.print id

let print_pfile = print_list newline print_decl

(*s explanation *)

let program_locs = Hashtbl.create 17

let read_in_file f l b e = 
  let ch = open_in f in
  for i = 2 to l do
    ignore (input_line ch)
  done;
  for i = 1 to b do
    ignore (input_char ch)
  done;
  let buf = Buffer.create 17 in
  for i = b to e-1 do
    Buffer.add_char buf (input_char ch)
  done;
  Buffer.contents buf
 

let loc_of_label n =
  let (f,l,b,e,_) = Hashtbl.find locs_table n in
  (f,l,b,e)
    
let reloc_xpl_term (loc,t) = 
  match t with
    | Tnamed(User n,p) ->     
	begin
	  try loc_of_label n
	  with Not_found -> Loc.extract loc
	end
    | _ -> Loc.extract loc

let reloc_xpl (loc,p) = 
  match p with
    | Pnamed(User n,p) -> 
	begin
	  try loc_of_label n
	  with Not_found -> Loc.extract loc
	end
    | _ -> Loc.extract loc

let dummy_reloc = Loc.dummy_floc

(* [cook_loop_invariant internal_lab userlab] checks whether the loop invariant
   labelled with [userlab] is associated to a formula in the locs file
   given on the command line. If yes, the formula is returned as a
   string, followed by the location in the source file where the loop
   is.
*)
let cook_loop_invariant internal_lab (userlab : string option) p =
  match userlab with
    | None -> "", reloc_xpl p
    | Some lab ->
	try 
	  let (f,l,b,e,o) = Hashtbl.find locs_table lab in
	  try
	    let k = 
	      match List.assoc "formula" o with
		| Rc.RCident s | Rc.RCstring s -> s
		| _ -> raise Not_found		  
	    in
	    if debug then eprintf "Util: formula for '%s' is '%s'@." lab k;
	    k, (f,l,b,e)
	  with Not_found ->
	    if debug then eprintf "Util: cannot find a formula for '%s'@." lab;
	    "", (f,l,b,e)
	with Not_found -> 
	  if debug then eprintf "Util: cannot find a loc for '%s'@." lab;
	  "", reloc_xpl p

(* [cook_explanation userlab raw_exp] processes the raw explanation
   [raw_exp] for the formula labelled [userlab] generated by the WP
   calculus. 

   Location(s) of [raw_exp] refers to the Why file.  A better
   explanation is cooked, depending on the locs file given on the
   command line and the [userlab]: it is check
   whether this label is present in the locs file, and if it is the
   case, replaces the location in [raw_exp] by the location given
   there, and also if a precise kind of VC is given, produces a
   refined explanation.
*)
let cook_explanation (userlab : string option) e =
  let e,l =
    match e with
      | VCEexternal s -> EKOther s, dummy_reloc 
      | VCEabsurd -> EKAbsurd, dummy_reloc
      | VCEassert p -> EKAssert, (reloc_xpl (List.hd p))
      | VCEpre(lab,loc,p) -> 
	  begin
	    if debug then eprintf "Util.cook_explanation: label,loc for pre = %s,%a@." lab
	      Loc.gen_report_position loc;
	    try 
	      let (f,l,b,e,o) = Hashtbl.find locs_table lab in
	      try
		let k = 
		  match List.assoc "kind" o with
		    | Rc.RCident s | Rc.RCstring s -> s
		    | _ -> raise Not_found		  
		in
		if debug then eprintf "Util: kind for '%s' is '%s'@." lab k;
		EKPre k, (f,l,b,e)
	      with Not_found ->
		if debug then eprintf "Util: cannot find a kind for '%s'@." lab;
		EKPre "", (f,l,b,e)
	    with Not_found -> 
	      if debug then eprintf "Util: cannot find a loc for '%s'@." lab;
	      EKPre "", Loc.extract loc
	  end

      | VCEpost p -> EKPost, (reloc_xpl p)
      | VCEwfrel -> EKWfRel, dummy_reloc 
      | VCEvardecr p -> EKVarDecr, (reloc_xpl_term p)
      | VCEinvinit(internal_lab,p) -> 
	  let s,loc  = cook_loop_invariant internal_lab userlab p in
	  EKLoopInvInit s, loc 
      | VCEinvpreserv(internal_lab,p) -> 
	  let s,loc  = cook_loop_invariant internal_lab userlab p in
	  EKLoopInvPreserv s, loc
  in 
  match e with
    | EKPre _ -> 
	(* for pre-conditions, we want to focus on the call, not an the formula to prove *)
	e,l
    | _ -> e, 
	match userlab with
	  | None -> l
	  | Some lab -> 
	      (try loc_of_label lab with Not_found -> 
		if debug then
		  eprintf "Warning: no loc found for user label %s@." lab;
		l)

    
let explanation_table = Hashtbl.create 97
let explanation_counter = ref 0

let reg_explanation e =
  incr explanation_counter;
  let id = !explanation_counter in
  if debug then Format.eprintf "registering internal explanation '%d'@." id;
  Hashtbl.add explanation_table id e;
  Internal id

(*
let print_loc_predicate fmt (loc,p) =
  match p with
    | Pnamed(User s,p) -> 
	begin
	  try 
	    let (f,l,b,e,o) = Hashtbl.find locs_table s in
	    let s = read_in_file f l b e in
	    fprintf fmt "(relocated from %a) %s"
	      Loc.gen_report_line (f,l,b,e) s 
	  with Not_found ->
	    fprintf fmt "(named %s) %a" s print_predicate p
	end
    | _ -> 
	fprintf fmt "(located %a) %a" 
	  Loc.gen_report_position loc print_predicate p
*)
  

let print_decl fmt = function
    Dtype (_, sl, s) -> 
      fprintf fmt "type %s" s
  | Dlogic (_, s, log_type_sch) -> 
      fprintf fmt "logic %a" print_logic_type log_type_sch.Env.scheme_type
  | Dpredicate_def (_, s, pred_def_sch) ->
      let (args, pred) = pred_def_sch.Env.scheme_type in
      fprintf fmt "defpred %s (%a) = %a" s
	(print_list comma (fun fmt t -> print_pure_type fmt (snd t))) args
	print_predicate pred
  | Dfunction_def (_, s, fun_def_sch) ->
      let (args, rt, exp) = fun_def_sch.Env.scheme_type in
      fprintf fmt "deffun %s (%a) : %a = %a" s
	(print_list comma (fun fmt t -> print_pure_type fmt (snd t))) args
	print_pure_type rt
	print_term exp
  | Daxiom (_, s, pred_sch) ->
      fprintf fmt "axiom %s : %a" s 
	print_predicate pred_sch.Env.scheme_type
  | Dgoal (loc, expl, s, seq_sch) -> 
      let (cel, pred) = seq_sch.Env.scheme_type in
      fprintf fmt "goal %s (%a) : %a" s
	(print_list comma 
	   (fun fmt t -> match t with 
	     Cc.Svar (id, pt) -> 
	       fprintf fmt "%a : %a" Ident.print id print_pure_type pt
	   | Cc.Spred (id, pred) -> 
	       fprintf fmt "%a <=> %a" Ident.print id print_predicate pred)) cel
	print_predicate pred;
      fprintf fmt "(* %a *)@\n" (fun fmt -> Explain.print fmt) ((*loc,*)expl)

(* debug *)

(**
let () = 
  Env.dump_type_var := 
    (fun v -> eprintf "@[type_var %d = %a@]@." v.tag print_pure_type (PTvar v))
**)
