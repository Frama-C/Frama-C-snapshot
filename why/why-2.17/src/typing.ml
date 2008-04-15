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

(*i $Id: typing.ml,v 1.141 2008/11/05 14:03:18 filliatr Exp $ i*)

(*s Typing. *)

open Format
open Options
open Ident
open Misc
open Ltyping
open Util
open Logic
open Rename
open Types
open Ptree
open Error
open Report
open Ast
open Env 
open Effect

(*s Typing of terms (used to type pure expressions). *)

let type_v_int = PureType PTint
let type_v_bool = PureType PTbool
let type_v_unit = PureType PTunit
let type_v_real = PureType PTreal

let typing_const = function
  | ConstInt _ -> type_v_int
  | ConstBool _ -> type_v_bool
  | ConstUnit -> type_v_unit
  | ConstFloat _ -> type_v_real

(*s Utility functions for typing *)

let expected_cmp loc =
  raise_located loc 
    (ExpectedType (fun fmt -> fprintf fmt "unit, bool, int or real"))

let just_reads e = difference (get_reads e) (get_writes e)

let rec unify_type_v v1 v2 = match (v1,v2) with
  | PureType c1, PureType c2 -> 
      Ltyping.unify c1 c2
  | Ref v1, Ref v2 -> 
      Ltyping.unify v1 v2
  | Arrow (bl1, k1), Arrow (bl2, k2) ->
      let rec unify_bl = function
	| [], [] -> unify_type_v k1.c_result_type k2.c_result_type
	| (_,t1)::bl1, (_,t2)::bl2 -> unify_type_v t1 t2 && unify_bl (bl1,bl2)
	| [], bl2 -> unify_type_v k1.c_result_type (Arrow (bl2, k2))
	| bl1, [] -> unify_type_v (Arrow (bl1, k1)) k2.c_result_type
      in
      unify_bl (bl1,bl2)
  | _ -> 
      false

let type_v_sup loc t1 t2 =
  if not(unify_type_v t1 t2) then raise_located loc BranchesSameType;
  t1

let union3effects x y z = Effect.union x (Effect.union y z)

let decomp_fun_type loc = function
  | Arrow ([x, v], k) ->
      x, v, k
  | Arrow ((x, v) :: bl, k) ->
      x, v, type_c_of_v (Arrow (bl, k))
  | Arrow ([], _) ->
      assert false
  | ty -> 
      Format.eprintf "decomp_fun_type=%a@." print_type_v ty;
      raise_located loc AppNonFunction

let expected_type loc t et =
  if not (unify_type_v t et) then 
    raise_located loc 
      (ExpectedType2 
	  ((fun fmt -> print_type_v fmt t), (fun fmt -> print_type_v fmt et)))

let check_for_alias loc id v = 
  if occur_type_v id v then raise_located loc (Alias id)

let check_for_let_ref loc v =
  if not (is_pure v) then raise_located loc Error.LetRef

let check_for_not_mutable loc v = 
  if is_mutable v then raise_located loc CannotBeMutable

let check_unbound_exn loc =
  let check (x,_) =
    if not (Env.is_exception x) then raise_located loc (UnboundException x)
  in
  List.iter check

(*s Instantiation of polymorphic functions *)

let type_prim idint idreal idbool idunit loc = function
  | PureType pt -> begin match normalize_pure_type pt with
      | PTint -> idint
      | PTbool -> idbool
      | PTreal -> idreal
      | PTunit -> idunit
      | _ -> expected_cmp loc
    end
  | _ -> expected_cmp loc

let type_eq = type_prim t_eq_int_ t_eq_real_ t_eq_bool_ t_eq_unit_
let type_neq = type_prim t_neq_int_ t_neq_real_ t_neq_bool_ t_neq_unit_

let type_num idint idreal loc = function
  | PureType pt -> begin match normalize_pure_type pt with
      | PTint -> idint
      | PTreal -> idreal
      | _ -> expected_num loc
    end
  | _ -> expected_num loc

let type_lt = type_num t_lt_int_ t_lt_real_
let type_le = type_num t_le_int_ t_le_real_
let type_gt = type_num t_gt_int_ t_gt_real_
let type_ge = type_num t_ge_int_ t_ge_real_
let type_add = type_num t_add_int t_add_real
let type_sub = type_num t_sub_int t_sub_real
let type_mul = type_num t_mul_int t_mul_real
let type_div = type_num t_div_int_ t_div_real_
let type_neg = type_num t_neg_int t_neg_real

let type_poly id =
  if id == t_eq then type_eq 
  else if id == t_neq then type_neq
  else if id == t_lt then type_lt
  else if id == t_le then type_le 
  else if id == t_gt then type_gt
  else if id == t_ge then type_ge
  else if id == t_add then type_add 
  else if id == t_sub then type_sub
  else if id == t_mul then type_mul
  else if id == t_div then type_div
  else if id == t_neg then type_neg 
  else assert false

let type_un_poly id =
  if id == t_neg then type_neg else assert false

(*s Making nodes *)

let gmake_node loc env userlabel l ?(post=None) p rt e = 
  { desc = p; 
    info = { t_loc = loc; t_env = env; t_label = l; 
	     t_userlabel = userlabel;
	     t_result_name = result; 
	     t_result_type = rt; t_effect = e; t_post = post } }


let bool_constant b loc env =
  gmake_node loc env "" (label_name ()) (Expression (Tconst (ConstBool b)))
    type_v_bool Effect.bottom

let make_arrow_type lab bl k =
  let k = 
    let q = optpost_app (change_label lab "") k.c_post in
    { k with c_post = q }
  in
  make_arrow bl k

let k_add_effects k e = { k with t_effect = Effect.union k.t_effect e }

let type_c_of_typing_info pre ti =
  { c_result_name = ti.t_result_name;
    c_result_type = ti.t_result_type;
    c_effect = ti.t_effect;
    c_pre = a_values pre;
    c_post = optpost_app a_value ti.t_post }

let typing_info_of_type_c loc env l k =
  { t_loc = loc;
    t_env = env;
    t_label = l;
    t_userlabel = "";
    t_result_name = k.c_result_name;
    t_result_type = k.c_result_type;
    t_effect = k.c_effect;
    t_post = optpost_app (post_named loc) k.c_post }

let assume loc env ef p =
  let k = { c_result_name = result; c_result_type = type_v_unit;
	    c_effect = ef; c_pre = []; c_post = Some (p,[]) } in
  gmake_node loc env "" (label_name ()) (Any k) type_v_unit ef

let seq loc env e1 e2 =
  gmake_node loc env "" (label_name ()) (Seq (e1, e2)) (result_type e2)
    (Effect.union (effect e1) (effect e2))

(*s Typing variants. 
    Return the effect i.e. variables appearing in the variant. *)

let state_var loc lab env = function
  | None when termination = Total ->
      raise_located loc 
	(AnyMessage "a variant is required here (since -total is set)")
  | None -> 
      None, Effect.bottom
  | Some (pphi,r) ->
      let phi,tphi = Ltyping.term lab env pphi in
      let ids = term_refs env phi in
      (if termination = Partial then None else Some (pphi.pp_loc,phi,tphi,r)), 
      Effect.add_reads ids Effect.bottom
	
(*s Typing preconditions.
    Return the effect i.e. variables appearing in the precondition. 
    Check existence of labels. *)

let predicates_effect lab env loc pl =
  let state e p =
    let ids = predicate_vars p in
    Idset.fold
      (fun id e ->
	 if is_reference env id then
	   Effect.add_read id e
	 else if is_at id then begin
	   let uid,l = un_at id in
	   if not (Label.mem l lab) then raise_located loc (UnboundLabel l);
	   if is_reference env uid then
	     Effect.add_read uid e
	   else
	     e
	 end else
	   e)
      ids e 
  in
  List.fold_left state Effect.bottom pl

let state_pre lab env loc pl =
  let pl = List.map (type_assert lab env) pl in
  predicates_effect lab env loc (List.map (fun x -> x.a_value) pl), pl

let state_assert lab env loc a =
  let a = type_assert lab env a in
  predicates_effect lab env loc [a.a_value], a

let state_inv lab env loc = function
  | None -> 
      Effect.bottom, None
  | Some i -> 
      let i = type_assert lab env i in
      predicates_effect lab env loc [i.a_value], Some i
	

(*s Typing postconditions.
    Return the effect i.e. variables appearing in the postcondition,
    together with the normalized postcondition (i.e. [x] replaced by [x@]
    whenever [x] is not modified by the program).
    Check existence of labels. *)

let state_post lab env (id,v,ef) loc q =
  check_unbound_exn loc (snd q);
  let q = type_post lab env id v ef q in
  let ids = apost_vars q in
  let ef,q = 
    Idset.fold
      (fun id (e,q) ->
	 if is_reference env id then
	   if is_write ef id then
	     Effect.add_write id e, q
	   else
	     Effect.add_read id e,
	     (let s = subst_in_predicate (subst_onev id (at_id id "")) in
	      post_app (asst_app s) q)
	 else if is_at id then begin
	   let uid,l = un_at id in
	   if l <> "" && not (Label.mem l lab) then 
	     raise_located loc (UnboundLabel l);
	   if is_reference env uid then
	     Effect.add_read uid e, q
	   else
	     raise_located loc (UnboundReference uid)
	 end else
	   e,q)
      ids (Effect.bottom, q)
  in
  ef, q
    
let state_post_option lab env res loc = function
  | None -> 
      Effect.bottom, None
  | Some q ->
      let ef,q = state_post lab env res loc q in
      ef, Some q

(*s Detection of pure functions. *)

let rec is_pure_type_v = function
  | PureType _ -> true
  | Arrow (bl,c) -> List.for_all is_pure_arg bl && is_pure_type_c c
  | Ref _ -> false
and is_pure_arg (_,v) = 
  is_pure_type_v v
and is_pure_type_c c =
  is_pure_type_v c.c_result_type && c.c_effect = Effect.bottom &&
  c.c_pre = [] && c.c_post = None

(*s Types of references and arrays *)

let check_ref_type loc env id =
  try
    deref_type (type_in_env env id)
  with 
    | Not_found -> raise_located loc (UnboundReference id)
    | Invalid_argument _ -> raise_located loc (NotAReference id)
      
let check_array_type loc env id =
  try
    PureType (dearray_type (type_in_env env id))
  with 
    | Not_found -> raise_located loc (UnboundArray id)
    | Invalid_argument _ -> raise_located loc (NotAnArray id)
      
let is_pure_type = function
  | PureType _ -> true
  | _ -> false

let pure_type loc = function
  | PureType pt -> pt
  | _ -> raise_located loc MustBePure

let decompose_app e =
  let rec loop args e = match e.pdesc with
    | Sapp (e1, e2) -> loop (e2 :: args) e1
    | _ -> e, args
  in
  loop [] e

let pdesc loc p = { pdesc = p; ploc = loc }
let sapp loc e1 e2 = { pdesc = Sapp (e1, e2); ploc = loc }
let svar loc v = { pdesc = Svar v; ploc = loc }
let sassume loc p = 
  let a = { pa_name = Anonymous; pa_value = p; pa_loc = loc } in
  let k = { pc_result_name = result; pc_result_type = PVpure PPTunit;
	    pc_effect = { pe_reads = []; pe_writes = []; pe_raises = [] };
	    pc_pre = []; pc_post = Some (a, []) } in
  { pdesc = Sany k; ploc = loc }
let sletin loc v e1 e2 = { pdesc = Sletin (v, e1, e2); ploc = loc }
let sseq loc e1 e2 = { pdesc = Sseq (e1, e2); ploc = loc }

let ppinfix loc l1 i l2 = { pp_loc = loc; pp_desc = PPinfix (l1, i, l2) }
let ppvar loc v = { pp_loc = loc; pp_desc = PPvar v }
let ppconst loc c = { pp_loc = loc; pp_desc = PPconst c }
    
(*s Saturation of postconditions: a postcondition must be set for
    any possibly raised exception *)

let warning_no_post loc x = 
  if not !c_file then begin
    wprintf loc "no postcondition for exception %a; false inserted@\n" 
      Ident.print x;
    if werror then exit 1
  end

let saturation loc e (a,al) =
  let xs = Effect.get_exns e in
  let check (x,_) =
    if not (List.mem x xs) then raise_located loc (CannotBeRaised x);
  in
  List.iter check al;
  let set_post x = 
    try 
      x, List.assoc x al 
    with Not_found -> 
      warning_no_post loc x;
      x, anonymous Loc.dummy_position Pfalse (* default_post *)
  in
  (a, List.map set_post xs)

let conj_assert ({a_value=pa} as a) ({a_value=pb} as b) =
  let loc = Loc.join a.a_loc b.a_loc in
  { a with a_value = pand ~is_wp:true pa pb; a_loc = loc }

let conj q q' = match q, q' with
  | None, _ ->
      q'
  | _, None ->
      q
  | Some (q, ql), Some (q', ql') ->
      assert (List.length ql = List.length ql');
      let conjx (x,a) (x',a') =
	assert (x = x');
	x, 
	if is_default_post a then a' 
	else if is_default_post a' then a 
	else conj_assert a a'
      in
      Some (conj_assert q q', List.map2 conjx ql ql') 

(*s The following flag indicates whether exceptions must be checked 
    as possibly raised in try-with constructs; indeed, this must be disabled
    when computing the effects of a recursive function. *)

let exn_check = ref true
let without_exn_check f x =
  if !exn_check then begin
    exn_check := false; 
    try let y = f x in exn_check := true; y 
    with e -> exn_check := true; raise e
  end else
    f x

(*s Pure expressions. Used in [Slazy_and] and [Slazy_or] to decide
    whether to use [strict_bool_and_] and [strict_bool_or_] or not. *)

let has_no_effect e = 
  let ef = effect e in get_writes ef = [] && get_exns ef = []

let rec is_pure_expr e = 
  has_no_effect e &&
  match e.desc with
  | Var _ | Expression _ -> true
  | If (e1, e2, e3) -> is_pure_expr e1 && is_pure_expr e2 && is_pure_expr e3
  | LetIn (_, e1, e2) -> is_pure_expr e1 && is_pure_expr e2
  | Label (_, e1) | Assertion (_, [], e1) -> is_pure_expr e1
  | AppRef (e1, _, _) | AppTerm (e1, _, _) -> is_pure_expr e1
  | Any _ | Rec _ | Lam _ | Try _ 
  | Raise _ | Post _ | Assertion _ | LetRef _ 
  | Loop _ | Seq _ | Absurd -> false

(*s Typing programs. We infer here the type with effects. 
    [lab] is the set of labels, [env] the environment 
    and [expr] the program. [pre] indicates whether preconditions are true 
    preconditions or obligations *)

let rec typef ?(userlabel="") lab env expr =
  let toplabel = label_name () in
  let loc = expr.ploc in
  let make_node = gmake_node loc env userlabel in
  match expr.pdesc with
  | Sconst c ->
      make_node toplabel (Expression (Tconst c)) (typing_const c) Effect.bottom

  | Svar id ->
      let v = 
	try type_in_env env id 
	with Not_found -> raise_located loc (UnboundVariable id)
      in
      let ef = Effect.bottom in
      if not (is_local env id) &&
	 is_logic_function id && 
	 not (is_pure_type v) 
      then 
	raise_located loc 
	  (AnyMessage "a logic function cannot be partially applied");
      if is_pure_type_v v && not (is_rec id env) then 
	let t,v = 
	  try 
	    let lid = { pp_loc = loc; pp_desc = PPvar id } in
	    let t,v = Ltyping.term lab env lid in
	    t, PureType v
	  with _ -> 
	    Tvar id, v
	in
	make_node toplabel (Expression t) v ef
      else 
	make_node toplabel (Var id) v ef

  | Sderef id ->
      let v = check_ref_type loc env id in
      let ef = Effect.add_read id Effect.bottom in
      make_node toplabel (Expression (Tderef id)) (PureType v) ef

  | Sseq (e1, e2) ->
      let t_e1 = typef lab env e1 in
      expected_type e1.ploc (result_type t_e1) type_v_unit;
      let t_e2 = typef lab env e2 in
      let ef = Effect.union (effect t_e1) (effect t_e2) in
      make_node toplabel (Seq (t_e1, t_e2)) (result_type t_e2) ef
	      
  | Sloop (invopt, var, e) ->
      let var,efphi = state_var loc lab env var in
      let t_e = typef lab env e in
      let efe = t_e.info.t_effect in
      let efinv,invopt = state_inv lab env loc invopt in
      let ef = Effect.union efe (Effect.union efinv efphi) in
      let v = type_v_unit in
      make_node toplabel (Loop (invopt,var,t_e)) v ef
      
  | Slam ([], _, _) ->
      assert false

  | Slam (bl, p, e) ->
      let bl',env' = binders loc lab env bl in
      let (ep,p') = state_pre lab env' loc p in
      let t_e = typef lab env' e in
      check_for_not_mutable e.ploc t_e.info.t_result_type;
      let info = k_add_effects t_e.info ep in
      let t_e = { t_e with info = info } in
      let k = type_c_of_typing_info p' info in
      let v = make_arrow_type t_e.info.t_label bl' k in
      let ef = Effect.bottom in
      make_node toplabel (Lam (bl',p',t_e)) v ef

  | Sapp _ ->
      let f,args = decompose_app expr in
      (* 1. if f is a polymorphic symbol, find its type using first arg. *)
      let f = match f.pdesc with
	| Svar x when is_poly x ->
	    begin match args with
	      | a :: _ ->
		  let t_a = typef lab env a in
		  let eq = type_poly x a.ploc (result_type t_a) in
		  { f with pdesc = Svar eq }
	      | [] -> 
		  assert false 
		  (* the parser ensures the presence of an argument *)
	    end
	| _ -> 
	    f
      in
      (* 2. typing the function f *)
      let t_f, tyf = match f.pdesc with
	| Svar x when is_logic_function x && not (is_local env x) ->
	    begin match find_global_logic x with
	      | vars, Function (tl,tr) ->
		  (* TODO: check number of args *)
		  let v = type_v_of_logic tl tr in
		  let i = instance x vars in
		  make_node 
		    toplabel (Expression (Tapp (x, [], i))) v Effect.bottom, v
	      | _, Predicate _ ->
		  assert false
	    end
	| _ ->
	    let tf = typef lab env f in
	    tf, result_type tf
      in
      (* 3. typing the arguments *)
      let rec loop_args t_f tyf = function
	| [] ->
	    t_f
	| a :: ra ->
	    let x,tx,kapp = decomp_fun_type loc tyf in
	    begin match tx with
   	    (* the function expects a mutable; it must be a variable *)
            | Ref _ -> begin match a.pdesc with
		| Svar r ->
		    let v = 
		      try 
			type_in_env env r 
		      with Not_found -> 
			raise_located a.ploc (UnboundVariable r)
		    in
		    expected_type a.ploc v tx;
		    check_for_alias a.ploc r tyf;
		    let kapp = type_c_subst (subst_onev x r) kapp in
		    let (_,tapp),eapp,papp,_ = decomp_type_c kapp in
		    let kapp = typing_info_of_type_c loc env toplabel kapp in
		    let ef = Effect.union (effect t_f) eapp in
		    let t_f = 
		      let make ?post n = 
			make_node (label_name ()) ?post n tapp ef 
		      in
		      make (Assertion
			      (`PRE, 
			       List.map (pre_named loc) papp,
			       make ~post:kapp.t_post (AppRef (t_f, r, kapp))))
		    in
		    loop_args t_f (result_type t_f) ra
		| _ ->
		    raise_located a.ploc ShouldBeVariable
	      end
	    (* otherwise (the argument is not a reference) *)
	    | _ -> 
		let t_a = typef lab env a in
		expected_type a.ploc (result_type t_a) tx;
		begin match t_a with
		(* argument is pure: it is substituted *)
   	        | { desc = Expression ta } when post t_a = None ->
		    let kapp = type_c_subst_oldify env x ta kapp in
		    let _,_,papp,_ = decomp_type_c kapp in
		    let kapp = typing_info_of_type_c loc env toplabel kapp in
		    let (_,tapp),eapp,_ = decomp_kappa kapp in
		    let ef = union3effects (effect t_a) (effect t_f) eapp in
		    let t_f = match t_f with
		      (* collapse: (term(tf) term(ta)) --> term(tf ta) *)
		      | { desc = Expression tf } when post t_f = None -> 
			  let l = label_name () in
			  make_node l (Expression (applist tf [ta])) tapp ef
		      | _ -> 
			  let make ?post n = 
			    make_node (label_name ()) ?post n tapp ef 
			  in
			  make 
			    (Assertion 
			       (`PRE,
				List.map (pre_named loc) papp,
				make 
				  ~post:kapp.t_post (AppTerm (t_f, ta, kapp))))
		    in
		    loop_args t_f (result_type t_f) ra
 	        (* otherwise we transform into [let v = arg in (f v)] *)
		| _ ->
		    let _,eapp,_,_ = decomp_type_c kapp in
		    let v = fresh_var () in
		    let kapp = type_c_subst (subst_onev x v) kapp in
		    let _,_,papp,_ = decomp_type_c kapp in
		    let label_f_v = label_name () in
		    let kapp = typing_info_of_type_c loc env label_f_v kapp in
		    let env' = Env.add v tx env in
		    let app_f_v = match t_f with
		      | { desc = Expression tf } when post t_f = None -> 
			  Expression (applist tf [Tvar v])
		      | _ -> 
			  AppTerm (t_f, Tvar v, kapp) 
		    in
		    let kfv = k_add_effects kapp (effect t_f) in
		    let app_f_v = 
		      gmake_node loc env' userlabel label_f_v app_f_v  
			~post:kfv.t_post kfv.t_result_type kfv.t_effect
		    in
		    let app = loop_args app_f_v (result_type app_f_v) ra in
		    let tapp = result_type app in
		    if occur_type_v v tapp then
		      raise_located a.ploc TooComplexArgument;
		    let ef = union3effects (effect app) (effect t_f) eapp in
		    let ef' = Effect.union ef (effect t_a) in
		    let make n = make_node (label_name ()) n tapp ef' in
		    make 
		      (LetIn (v, t_a, 
			      let make n = 
				gmake_node loc env' userlabel (label_name ()) n tapp ef
			      in
			      make (Assertion 
				      (`PRE,
				       List.map (pre_named loc) papp, 
				       app))))
		end
	    end
      in
      loop_args t_f tyf args
     
  | Sletref (x, e1, e2) ->
      if is_ref env x then raise_located loc (ClashRef x);
      let t_e1 = typef lab env e1 in
      let ef1 = t_e1.info.t_effect in
      let v1 = pure_type loc t_e1.info.t_result_type in
      let env' = add x (Ref v1) env in
      let t_e2 = typef lab env' e2 in
      let ef2 = t_e2.info.t_effect in
      let v2 = t_e2.info.t_result_type in
      check_for_let_ref loc v2;
      let ef = Effect.union ef1 (Effect.remove x ef2) in
      make_node toplabel (LetRef (x, t_e1, t_e2)) v2 ef
	
  | Sletin (x, e1, e2) ->
      let t_e1 = typef lab env e1 in
      let ef1 = t_e1.info.t_effect in
      let v1 = t_e1.info.t_result_type in
      check_for_not_mutable e1.ploc v1;
      let env' = add ~generalize:true x v1 env in
      let t_e2 = typef lab env' e2 in
      let ef2 = t_e2.info.t_effect in
      let v2 = t_e2.info.t_result_type in
      let ef = Effect.union ef1 ef2 in
      make_node toplabel (LetIn (x, t_e1, t_e2)) v2 ef
	    
  | Sif (b, e1, e2) ->
      let t_b = typef lab env b in
      expected_type b.ploc (result_type t_b) type_v_bool;
      let t_e1 = typef lab env e1
      and t_e2 = typef lab env e2 in
      let t1 = t_e1.info.t_result_type in
      let t2 = t_e2.info.t_result_type in
      let ef = union3effects (effect t_b) (effect t_e1) (effect t_e2) in
      let v = type_v_sup loc t1 t2 in
      make_node toplabel (If (t_b, t_e1, t_e2)) v ef

  | Slazy_and (e1, e2) ->
      let t_e1 = typef lab env e1 in
      expected_type e1.ploc (result_type t_e1) type_v_bool;
      let t_e2 = typef lab env e2 in
      expected_type e2.ploc (result_type t_e2) type_v_bool;
      if not split_bool_op && is_pure_expr t_e2 then
	(* we build strict_bool_and_(e1, e2)
	   TODO:  avoid typing e1 and e2 twice *)
	let d = sapp loc (sapp loc (svar loc strict_bool_and_) e1) e2 in
	(* we build let v = e1 in strict_bool_and_ v1 (assume v1=true; e2)
	let v = fresh_var () in
	let p = 
	  ppinfix loc (ppvar loc v) PPeq (ppconst loc (ConstBool true)) 
	in
	let d = 
	  sletin loc v e1 
	    (sapp loc (sapp loc (svar loc strict_bool_and_) (svar loc v)) 
		(sseq loc (sassume loc p) e2))
	in
	*)
	typef lab env d
      else
	let ef = union (effect t_e1) (effect t_e2) in
	let bool_false = bool_constant false loc env in
	make_node toplabel (If (t_e1, t_e2, bool_false)) type_v_bool ef

  | Slazy_or (e1, e2) ->
      let t_e1 = typef lab env e1 in
      expected_type e1.ploc (result_type t_e1) type_v_bool;
      let t_e2 = typef lab env e2 in
      expected_type e2.ploc (result_type t_e2) type_v_bool;
      if not split_bool_op && is_pure_expr t_e2 then
	(* we build strict_bool_or_(e1, e2)
	   TODO:  avoid typing e1 and e2 twice *)
	let d = sapp loc (sapp loc (svar loc strict_bool_or_) e1) e2 in
	typef lab env d
      else
	let ef = union (effect t_e1) (effect t_e2) in
	let bool_true = bool_constant true loc env in
	make_node toplabel (If (t_e1, bool_true, t_e2)) type_v_bool ef

  | Snot e1 -> 
      let t_e1 = typef lab env e1 in
      expected_type e1.ploc (result_type t_e1) type_v_bool;
      if not split_bool_op then
	let d = sapp loc (svar loc bool_not_) e1 in
	typef lab env d
      else
	let bool_false = bool_constant false loc env in
	let bool_true = bool_constant true loc env in
	let d = If (t_e1, bool_false, bool_true) in
	make_node toplabel d type_v_bool (effect t_e1)

  | Srec (f,bl,v,var,p,e) ->
      let loc_e = e.ploc in
      let bl',env' = binders loc lab env bl in
      let (ep,p') = state_pre lab env' loc p in
      let v = type_v loc lab env' v in
      let var, efvar = state_var loc lab env' var in
      (* e --> let vphi0 = phi in e *)
      let varinfo,env' = match var with
	| None -> 
	    None, env'
	| Some (loc,phi,tphi,r) ->
	    let vphi0 = variant_name () in
	    let tphi = PureType tphi in
	    let env' = Env.add vphi0 tphi env' in
	    let decphi = Papp (r, [phi; Tvar vphi0], []) in
	    Some (vphi0,phi,tphi,decphi), env'
      in
      (* effects for a let/rec construct are computed as a fixpoint *)
      let type_body c =
	let c = match varinfo with
	  | None -> c
	  | Some (_,_,_,decphi) -> { c with c_pre = decphi :: c.c_pre } 
	in
	let tf = make_arrow bl' c in
	let env'' = add_rec f (add f tf env') in
	typef lab env'' e
      in
      let fixpoint_reached c1 c2 =
	c1.c_effect = c2.c_effect && 
        List.length c1.c_pre = List.length c2.c_pre &&
        (match c1.c_post, c2.c_post with 
         | None, None | Some _, Some _ -> true | _ -> false)
      in
      let rec fixpoint c =
	let t_e = type_body c in
	let info = k_add_effects t_e.info ep in
	let k_e = type_c_of_typing_info p' info in
	if fixpoint_reached k_e c then
	  t_e
      	else begin
	  if_debug_3 
	    eprintf "  (rec => %a)@\n@?" print_typing_info t_e.info;
	  fixpoint k_e
      	end
      in 
      let c0 = { c_result_name = result; c_result_type = v;
		 c_effect = efvar; c_pre = []; c_post = None } in
      (* fixpoint, without check *)
      let t_e = without_exn_check fixpoint c0 in
      (* once again, with checks *)
      let info = k_add_effects t_e.info ep in
      let t_e = type_body (type_c_of_typing_info p' info) in
      let tf = make_arrow bl' (type_c_of_typing_info p' t_e.info) in
      let t_e = match varinfo with
	| Some (vphi0,phi,tphi,_) ->
	    let mk_node_e = gmake_node loc_e env' "" in
	    mk_node_e (label_name ()) 
	      (LetIn 
		 (vphi0,
		  mk_node_e (label_name ()) (Expression phi) tphi efvar,
		  t_e))
	      (result_type t_e) (Effect.union efvar (effect t_e))
	| None ->
	    t_e
      in
      make_node toplabel (Rec (f,bl',v,var,p',t_e)) tf Effect.bottom

  | Sraise (id, e, ct) ->
      if not (is_exception id) then raise_located loc (UnboundException id);
      let t_e, ef = match find_exception id , e with
	| None, Some _ -> 
	    raise_located loc (ExceptionArgument (id, false))
	| Some _, None ->
	    raise_located loc (ExceptionArgument (id, true))
	| Some xt, Some e ->
	    let t_e = typef lab env e in
	    expected_type e.ploc (result_type t_e) (PureType xt);
	    Some t_e, effect t_e
	| None, None -> 
	    None, Effect.bottom
      in
      let v = match ct with 
	| None -> PureType (PTvar (new_type_var ()))
	| Some v -> type_v loc lab env v
      in
      make_node toplabel (Raise (id, t_e)) v (Effect.add_exn id ef)

  | Stry (e, hl) ->
      let te = typef lab env e in
      let v = result_type te in
      let ef = effect te in
      let xs = get_exns ef in
      let ef = List.fold_left (fun e ((x,_),_) -> remove_exn x e) ef hl in
      let type_handler ((x,a),h) =
	if not (is_exception x) then raise_located loc (UnboundException x);
	if not (List.mem x xs) && !exn_check then 
	  raise_located e.ploc (CannotBeRaised x);
	let env' = match a, find_exception x with 
	  | None, None -> env 
	  | Some v, Some tv -> Env.add v (PureType tv) env
	  | None, Some _ -> raise_located loc (ExceptionArgument (x, true))
	  | Some _, None -> raise_located loc (ExceptionArgument (x, false))
	in
	let th = typef lab env' h in
	expected_type h.ploc (result_type th) v;
	((x,a), th)
      in
      let thl = List.map type_handler hl in
      let ef = List.fold_left (fun e (_,th) -> union e (effect th)) ef thl in
      make_node toplabel (Try (te, thl)) v ef

  | Sabsurd ct -> 	    
      let v = match ct with 
	| None -> PureType (PTvar (new_type_var ()))
	| Some v -> type_v loc lab env v
      in
      let absurd = make_node (label_name ()) Absurd v Effect.bottom in
      make_node toplabel 
	(Assertion (`ABSURD,[anonymous loc Pfalse], absurd)) v Effect.bottom

  | Sany c ->
      let c = type_c loc lab env c in
      make_node toplabel (Any c) c.c_result_type c.c_effect

  | Spost (e, q, tr) ->
      let t_e = typef lab env e in
      let v = t_e.info.t_result_type in
      let e = t_e.info.t_effect in
      let (eq,q) = state_post lab env (result,v,e) loc q in
      let q = saturation loc e q in
      let e' = Effect.union e eq in
      let d = Post (t_e, q, tr) in
      gmake_node loc env userlabel toplabel d v e' ~post:(Some q)

  | Sassert (p, e) ->
      let ep,p = state_pre lab env loc p in
      let t_e = typef lab env e in
      let ef = Effect.union (effect t_e) ep in
      make_node toplabel (Assertion (`ASSERT,p, t_e)) (result_type t_e) ef

  | Slabel (s, e) ->
      if (Label.mem s lab) then raise_located loc (ReboundLabel s);
      let lab' = Label.add s lab in
      let t_e = typef ~userlabel:s lab' env e in
      make_node toplabel (Label (s, t_e)) (result_type t_e) (effect t_e)

let typef = typef ~userlabel:""
