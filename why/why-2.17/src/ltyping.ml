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

(*i $Id: ltyping.ml,v 1.73 2008/11/05 14:03:17 filliatr Exp $ i*)

(*s Typing on the logical side *)

open Format
open Options
open Ident
open Logic
open Types
open Ptree
open Ast
open Misc
open Util
open Env
open Error
open Report

let expected_num loc =
  raise_located loc (ExpectedType (fun fmt -> fprintf fmt "int or real"))

let expected_type loc et =
  raise_located loc (ExpectedType (fun fmt -> print_type_v fmt et))

let rec pure_type env = function
  | PPTint -> PTint
  | PPTbool -> PTbool
  | PPTreal -> PTreal
  | PPTunit -> PTunit
  | PPTvarid (x, _) -> PTvar (find_type_var x env)
  | PPTexternal (p, id, loc) ->
      if not (is_type id) then raise_located loc (UnboundType id);
      let np = List.length p in
      let a = type_arity id in
      if np <> a then raise_located loc (TypeArity (id, a, np));
      PTexternal (List.map (pure_type env) p, id)

(*s Typing predicates *)

let int_cmp = function
  | PPlt -> t_lt_int
  | PPle -> t_le_int
  | PPgt -> t_gt_int
  | PPge -> t_ge_int
  | PPeq -> t_eq_int
  | PPneq -> t_neq_int
  | _ -> assert false

let real_cmp = function
  | PPlt -> t_lt_real
  | PPle -> t_le_real
  | PPgt -> t_gt_real
  | PPge -> t_ge_real
  | PPeq -> t_eq_real
  | PPneq -> t_neq_real
  | _ -> assert false

let other_cmp = function
  | PTbool, PPeq -> t_eq_bool
  | PTbool, PPneq -> t_neq_bool
  | PTunit, PPeq -> t_eq_unit
  | PTunit, PPneq -> t_neq_unit
  | _, PPeq -> t_eq
  | _, PPneq -> t_neq
  | _ -> assert false

let rec occurs v = function
  | PTvar { type_val = Some t } -> occurs v t
  | PTvar { tag = t; type_val = None } -> v.tag = t
  | PTexternal (l, _) -> List.exists (occurs v) l
  | PTint | PTbool | PTunit | PTreal -> false

(* destructive type unification *)
let rec unify t1 t2 = match (t1,t2) with
  | PTvar { type_val = Some t1 }, _ ->
      unify t1 t2
  | _, PTvar { type_val = Some t2 } ->
      unify t1 t2
  | PTvar v1, PTvar v2 when v1.tag = v2.tag ->
      true
  (* instantiable variables *)
  | (PTvar ({user=false} as v), t)
  | (t, PTvar ({user=false} as v)) ->
      not (occurs v t) && (v.type_val <- Some t; true)
  (* recursive types *)
  | (PTexternal(l1,i1), PTexternal(l2,i2)) ->
      i1 = i2 && List.length l1 = List.length l2 &&
      List.for_all2 unify l1 l2
  | (PTexternal _), _
  | _, (PTexternal _) ->
      false
  (* other cases *)
  | (PTunit | PTreal | PTbool | PTint | PTvar {user=true}), 
    (PTunit | PTreal | PTbool | PTint | PTvar {user=true}) -> 
      t1 = t2

let make_comparison loc (a,ta) r (b,tb) = 
  match normalize_pure_type ta, r, normalize_pure_type tb with
  | PTint, (PPlt|PPle|PPgt|PPge|PPeq|PPneq), PTint ->
      Papp (int_cmp r, [a; b], [])
  | PTreal, (PPlt|PPle|PPgt|PPge|PPeq|PPneq), PTreal ->
      Papp (real_cmp r, [a; b], [])
  | (PTbool | PTunit as ta), (PPeq|PPneq), (PTbool | PTunit) ->
      Papp (other_cmp (ta,r), [a; b], [])
  | ta, (PPeq|PPneq), tb ->
      if unify ta tb then
	match normalize_pure_type ta, normalize_pure_type tb with
	  | PTint, PTint -> Papp (int_cmp r, [a;b], [])
	  | PTreal, PTreal -> Papp (real_cmp r, [a;b], [])
	  | _ -> Papp (other_cmp (ta,r), [a; b], [ta])
      else
	raise_located loc 
	  (ExpectedType2 
	     ((fun f -> Util.print_pure_type f (normalize_pure_type ta)),
	      (fun f -> Util.print_pure_type f (normalize_pure_type tb))))
  | ta, _, _ ->
      raise_located loc (IllegalComparison (fun fmt -> print_pure_type fmt ta))

let int_arith = function
  | PPadd -> t_add_int
  | PPsub -> t_sub_int
  | PPmul -> t_mul_int
  | PPdiv -> t_div_int
  | PPmod -> t_mod_int
  | _ -> assert false

let real_arith = function
  | PPadd -> t_add_real
  | PPsub -> t_sub_real
  | PPmul -> t_mul_real
  | PPdiv -> t_div_real
  | _ -> assert false

let make_arith loc = function
  | (a,t1), (PPadd|PPsub|PPmul|PPdiv|PPmod as r), (b,t2) 
    when unify t1 PTint && unify t2 PTint ->
      Tapp (int_arith r, [a; b], []), PTint
  | (a,t1), (PPadd|PPsub|PPmul|PPdiv as r), (b,t2) 
    when unify t1 PTreal && unify t2 PTreal ->
      Tapp (real_arith r, [a; b], []), PTreal
  | (_,t1),op,(_,t2) ->
      expected_num loc

let predicate_expected loc =
  raise_located loc (AnyMessage "syntax error: predicate expected")

let term_expected loc =
  raise_located loc (AnyMessage "syntax error: term expected")

let instance x i = 
  let l = 
    Vmap.fold 
      (fun _ v l -> 
	 (match v.type_val with 
	    | None -> PTvar v 
	    | Some pt -> normalize_pure_type pt) :: l) 
      i []
  in 
  (*
    eprintf "instance %a[@[%a@]]@." 
    Ident.print x (Pp.print_list Pp.comma print_pure_type) l;
  *)
  l

(* typing predicates *)

let rec predicate lab env p =
  desc_predicate p.pp_loc lab env p.pp_desc

and desc_predicate loc lab env = function
  | PPvar x ->
      type_pvar loc env x
  | PPapp (x, pl) when x == t_distinct ->
      let ty = PTvar (Env.new_type_var ()) in
      let each_term a =
	let t,tt = term lab env a in
	if not (unify tt ty) then expected_type a.pp_loc (PureType ty);
	t,tt
      in
      let tl = List.map each_term pl in
      Papp (x, List.map fst tl, [ty])
  | PPapp (x, pl) ->
      type_papp loc env x (List.map (term lab env) pl)
  | PPtrue ->
      Ptrue
  | PPfalse ->
      Pfalse
  | PPconst _ ->
      predicate_expected loc
  | PPinfix (a, PPand, b) ->
      Pand (false, true, predicate lab env a, predicate lab env b)
  | PPinfix (a, PPiff, b) ->
      Piff (predicate lab env a, predicate lab env b)
  | PPinfix (a, PPor, b) ->
      Por (predicate lab env a, predicate lab env b)
  | PPinfix (a, PPimplies, b) ->
      Pimplies (false, predicate lab env a, predicate lab env b)
  | PPinfix 
      ({pp_desc = PPinfix (_, (PPlt|PPle|PPgt|PPge|PPeq|PPneq), a)} as p, 
       (PPlt | PPle | PPgt | PPge | PPeq | PPneq as r), b) ->
      let q = { pp_desc = PPinfix (a, r, b); pp_loc = loc } in
      Pand (false, true, predicate lab env p, predicate lab env q)
  | PPinfix (a, (PPlt | PPle | PPgt | PPge | PPeq | PPneq as r), b) ->
      make_comparison a.pp_loc (term lab env a) r (term lab env b)
  | PPinfix (_, (PPadd | PPsub | PPmul | PPdiv | PPmod), _) -> 
      predicate_expected loc
  | PPprefix (PPneg, _) ->
      predicate_expected loc
  | PPprefix (PPnot, a) ->
      Pnot (predicate lab env a)
  | PPif (a, b, c) ->
      let ta,tya = term lab env a in
      (match normalize_pure_type tya with
	 | PTbool -> 
	     Pif (ta, predicate lab env b, predicate lab env c)
	 | _ -> 
	     raise_located a.pp_loc ShouldBeBoolean)
  | PPforall (id, pt, tl, a) ->
      let v = pure_type env pt in
      let env' = Env.add_logic id v env in
      let tl' = triggers lab env' tl in
      let p' = predicate lab env' a in
      forall id (PureType v) ~triggers:tl' p'
  | PPexists (id, pt, a) ->
      let v = pure_type env pt in
      let p = predicate lab (Env.add_logic id v env) a in
      exists id (PureType v) p
  | PPfpi (e, f1, f2) ->
      (match term lab env e with
	 | te, PTreal -> Pfpi (te, f1, f2)
	 | _ -> raise_located e.pp_loc 
	         (AnyMessage "this expression should have type real"))
  | PPnamed (n, a) ->
      Pnamed (User n, predicate lab env a)

and type_pvar loc env x =
  if is_at x then 
    raise_located loc (AnyMessage "predicates cannot be labelled");
  try match snd (find_global_logic x) with
    | Predicate [] -> Pvar x
    | Function _ -> predicate_expected loc
    | _ -> raise_located loc PartialApp
  with Not_found -> 
    raise_located loc (UnboundVariable x)

and type_papp loc env x tl =
  try match find_global_logic x with
    | vars, Predicate at -> 
	check_type_args loc at tl; 
	Papp (x, List.map fst tl, instance x vars)
    | _ -> 
	predicate_expected loc
  with Not_found -> 
    raise_located loc (UnboundVariable x)


and term lab env t =
    desc_term t.pp_loc lab env t.pp_desc

and desc_term loc lab env = function
  | PPvar x | PPapp (x, []) ->
      type_tvar loc lab env x
  | PPif (a, b, c) ->
      term lab env { pp_loc = loc; pp_desc = PPapp (if_then_else, [a;b;c]) }
  | PPapp (x, tl) ->
      let tl = List.map (term lab env) tl in
      let ty, i = type_tapp loc env x tl in
      Tapp (x, List.map fst tl, i), ty
  | PPtrue ->
      ttrue, PTbool
  | PPfalse ->
      tfalse, PTbool
  | PPconst c ->
      Tconst c, type_const c
  | PPinfix (a, (PPadd|PPsub|PPmul|PPdiv|PPmod as r), b) ->
	make_arith loc (term lab env a, r, term lab env b)
  | PPinfix (_, (PPand|PPor|PPiff|PPimplies
		|PPlt|PPle|PPgt|PPge|PPeq|PPneq), _) ->
      term_expected loc
  | PPprefix (PPneg, a) ->
      (match term lab env a with
	 | ta, ty when unify ty PTint -> Tapp (t_neg_int, [ta], []), PTint
	 | ta, ty when unify ty PTreal -> Tapp (t_neg_real, [ta], []), PTreal
	 | _ -> expected_num loc)
  | PPnamed(n, t) -> 
      let tt,ty = term lab env t in Tnamed(User n, tt), ty

  | PPprefix (PPnot, _) | PPforall _ | PPexists _ | PPfpi _ ->
      term_expected loc

and type_if lab env a b c =
  match term lab env a, term lab env b, term lab env c with
    | (ta, PTbool), (tb, tyb), (tc, tyc) -> 
	if tyb <> tyc then 
	  raise_located c.pp_loc 
	    (ExpectedType (fun f -> print_pure_type f tyb));
	Tapp (if_then_else, [ta; tb; tc], []), tyb
    | _ -> raise_located a.pp_loc ShouldBeBoolean

and type_tvar loc lab env x = 
  let x,xu = 
    if is_at x then begin
      let xu,l = un_at x in
      if not (Label.mem l lab) then raise_located loc (UnboundLabel l);
      if not (is_reference env xu) then 
	xu,xu
      else 
	x,xu
    end else 
      x,x
  in
  try 
    let t = find_logic xu env in Tvar x, t
    (*
    let vars,t = find_logic xu env in
    if Vmap.is_empty vars then Tvar x, t else Tapp (x, [], instance x vars), t
    *)
  with Not_found -> try
    match find_global_logic xu with
      | vars, Function ([], t) -> Tapp (x, [], instance x vars), t 
      | _ -> raise_located loc MustBePure
  with Not_found -> 
    raise_located loc (UnboundVariable xu)


and type_tapp loc env x tl =
  try match find_global_logic x with
    | vars, Function (at, t) -> 
	check_type_args loc at tl; normalize_pure_type t, instance x vars
    | _ -> 
	raise_located loc AppNonFunction
  with Not_found -> 
    raise_located loc (UnboundVariable x)

and check_type_args loc at tl =
  let rec check_arg = function
    | [], [] -> 
	()
    | a :: al, (tb,b) :: bl ->
	if unify a b then
	  check_arg (al, bl)
	else
	  raise_located loc (TermExpectedType ((fun f -> print_term f tb),
					       fun f -> print_pure_type f a))
    | [], _ ->
	raise_located loc TooManyArguments
    | _, [] ->
	raise_located loc PartialApp
  in
  check_arg (at, tl)

and type_const = function
  | ConstInt _ -> PTint
  | ConstBool _ -> PTbool
  | ConstUnit -> PTunit
  | ConstFloat _ -> PTreal

and pattern lab env t =
  match t.pp_desc with
  | PPapp (x, _) ->
      (try match find_global_logic x with
      | _, Function (_, _) -> 
	  TPat (fst (term lab env t))
      | _ -> 
	  PPat (predicate lab env t)
      with Not_found -> 
	raise_located t.pp_loc (UnboundVariable x))
  | PPvar _ | PPinfix (_, (PPadd|PPsub|PPmul|PPdiv|PPmod), _) ->
      TPat (fst (term lab env t))
  | _ -> 
      Report.raise_located t.pp_loc Error.IllformedPattern
  
and triggers lab env = List.map (List.map (pattern lab env))

(*s Checking types *)

let add_logic_if_pure x v env =  match v with
  | PureType pt | Ref pt -> Env.add_logic x pt env
  | Arrow _ -> env 

let type_assert ?(namer=h_name) lab env a = 
  { a_value = predicate lab env a.pa_value;
    a_name = namer a.pa_name;
    a_loc = a.pa_loc;
    a_proof = None }

let type_post lab env id v ef (a,al) = 
  let lab' = Label.add "" lab in 
  let a' = 
    let env' = add_logic_if_pure id v env in type_assert lab' env' a 
  in
  let xs = Effect.get_exns ef in
  let check_exn (x,a) =
    let loc = a.pa_value.pp_loc in
    if not (is_exception x) then raise_located loc (UnboundException x);
    if not (List.mem x xs) then raise_located loc (CannotBeRaised x)
  in
  List.iter check_exn al;
  let loc = a.pa_value.pp_loc in
  let type_exn_post x =
    try
      let a = List.assoc x al in
      let env' = match find_exception x with
	| None -> env
	| Some pt -> Env.add_logic result pt env
      in
      (x, type_assert lab' env' a)
    with Not_found ->
      wprintf loc "no postcondition for exception %a; false inserted@\n"
	Ident.print x;
      (x, anonymous loc Pfalse)
  in
  (a', List.map type_exn_post xs)

let check_effect loc env e =
  let check_ref id =
    if not (Env.is_ref env id) then raise_located loc (UnboundReference id)
  in
  let check_exn id =
    if not (Env.is_exception id) then raise_located loc (UnboundException id)
  in
  let r,w,x,_ = Effect.get_repr e in
  List.iter check_ref r;
  List.iter check_ref w;
  List.iter check_exn x


(* warns if a ref occuring in a predicate is not mentioned in the effect,
   and adds it as read to the effect *)
let warn_refs loc env p = 
  Idset.fold 
    (fun id ef -> 
       if not (Effect.is_read ef id) then begin
	 wprintf loc "mutable %a is not declared in effect; added as read\n"
	   Ident.print id;
	 if werror then exit 1;
	 Effect.add_read id ef
       end else
	 ef)
    (predicate_refs env p)

let effect e =
  let ef = 
    List.fold_left (fun e x -> Effect.add_write x e) Effect.bottom e.pe_writes 
  in
  let ef = List.fold_left (fun e x -> Effect.add_read x e ) ef e.pe_reads in
  List.fold_left (fun e x -> Effect.add_exn x e) ef e.pe_raises
    

let rec type_v loc lab env = function
  | PVpure pt -> 
      PureType (pure_type env pt)
  | PVref v -> 
      Ref (pure_type env v)
  | PVarrow (bl, c) -> 
      let bl',env' = binders loc lab env bl in 
      Arrow (bl', type_c loc lab env' c)

and type_c loc lab env c =
  let ef = effect c.pc_effect in
  check_effect loc env ef;
  let v = type_v loc lab env c.pc_result_type in
  let id = c.pc_result_name in
  let p = List.map (type_assert lab env) c.pc_pre in
  let q = option_app (type_post lab env id v ef) c.pc_post in
  let ef = List.fold_right (asst_fold (warn_refs loc env)) p ef in
  let ef = optpost_fold (warn_refs loc env) q ef in
  let s = subst_onev id Ident.result in
  let p = List.map (fun a -> a.a_value) p in
  let q = optpost_app (fun a -> subst_in_predicate s a.a_value) q in
  { c_result_name = c.pc_result_name; c_effect = ef;
    c_result_type = v; c_pre = p; c_post = q }

and binders loc lab env = function
  | [] ->
      [], env
  | (id, v) :: bl ->
      let v = type_v loc lab env v in
      let bl',env' = 
	binders loc lab 
	  (Env.add id v (add_logic_if_pure id v env)) bl 
      in
      (id, v) :: bl', env'

let type_v loc lab env v = make_binders_type_v (type_v loc lab env v)

let type_c loc lab env c = make_binders_type_c (type_c loc lab env c)

let logic_type lt = 
  let env = Env.empty_logic () in
  match lt with
  | PPredicate pl -> 
      Predicate (List.map (pure_type env) pl)
  | PFunction (pl, t) -> 
      Function (List.map (pure_type env) pl, pure_type env t)

