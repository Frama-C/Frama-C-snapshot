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

(*i $Id: monad.ml,v 1.80 2008/11/05 14:03:17 filliatr Exp $ i*)

open Format
open Misc
open Ident
open Effect
open Util
open Logic
open Types
open Ast
open Cc
open Rename
open Env
open Typing

(*s [product ren [y1,z1;...;yk,zk] q] constructs the (possibly dependent) 
    tuple type
 
       $z1 \times ... \times zk$ if no postcondition
    or $\exists. y1:z1. ... yk:zk. (Q x1 ... xn)$  otherwise *)

let make_bl = List.map (fun (id,t) -> (id, CC_var_binder t))

let product w q = match q, w with
  | None, [_, v] -> 
      v
  | _ -> 
      TTtuple (make_bl w, q)

(*s [arrow_pred ren v pl] abstracts the term [v] over the pre-condition if any
    i.e. computes
 
      (P1 x1 ... xn) -> ... -> (Pk x1 ... xn) -> v
  
    where the [xi] are given by the renaming [ren]. *)

let arrow_pred ren env v pl = 
  List.fold_right
    (fun p t -> 
       let p = apply_assert ren env p in
       TTarrow ((Ident.anonymous, CC_pred_binder p), t))
    pl v
	
let arrow_vars = 
  List.fold_right (fun (id,v) t -> TTarrow ((id, CC_var_binder v), t))

let lambda_vars =
  List.fold_right (fun (id,v) t -> TTlambda ((id, CC_var_binder v), t))

(* type of the argument of an exception; unit if none *)
let exn_arg_type x =
  TTpure (match find_exception x with None -> PTunit | Some pt -> pt)

(* monadic type for exceptions: (EM E1 (EM E1 ... (EM Ek T))) *)
let trad_exn_type =
  List.fold_right 
    (fun id t -> TTapp (tt_var Ident.exn_type, [exn_arg_type id; t]))

(*s Translation of effects types in CC types.
  
    [trad_type_v] and [trad_type_c] translate types with effects
    into CC types. *)

let rec trad_type_c ren env k = 
  let ((_,v),e,p,q) = decomp_type_c k in
  let i,o,x,_ = get_repr e in
  let before = label_name () in
  let ren' = next (push_date ren before) o in
  let lo = output ren' env (v,o,x) in
  let q = option_app (apply_post before ren' env) q in
  let ty = product lo (make_applied_post ren' env k q) in
  let ty = arrow_pred ren env ty p in
  let li = input ren env i in
  arrow_vars li ty

and trad_type_v ren env = function
  | Ref _ -> 
      assert false
  | Arrow (bl, c) ->  
      let bl',ren',env' =
	List.fold_left
	  (fun (bl,ren,env) b -> match b with
	     | (id, (Ref _ as v)) ->
		 let env' = Env.add id v env in
		 let ren' = initial_renaming env' in
		 (bl, ren', env')
	     | (id, v) -> 
		 let tt = trad_type_v ren env v in
		 let env' = Env.add id v env in
		 let ren' = initial_renaming env' in
		 (id,tt)::bl, ren', env')
 	  ([],ren,env) bl 
      in
      arrow_vars (List.rev bl') (trad_type_c ren' env' c)

  | PureType c ->
      TTpure c

and input ren env i =
  prod ren env i

and output ren env (v,o,x) =
  let tv = trad_type_v ren env v in
  (prod ren env o) @ [result, trad_exn_type x tv]

and prod ren env g = 
  List.map (fun id -> (current_var ren id, trad_type_in_env ren env id)) g

and trad_imp_type ren env = function
  | Ref v -> TTpure v
  | _ -> assert false

and trad_type_in_env ren env id =
  let v = type_in_env env id in 
  trad_imp_type ren env v

(* builds (qcomb [r][Q1] (qcomb [r][Q2] (... (qcomb [r][Qn] [r]Q)))) *)

and make_post ren env res k q =
  let q = post_app (subst_in_predicate (subst_onev result res)) q in
  let abs_pred v c =
    lambda_vars [res, trad_type_v ren env v] (TTpred c)
  in
  List.fold_right 
    (fun x c -> 
       let tx = match find_exception x with None -> PTunit | Some pt -> pt in
       let tx = PureType tx in
       let qx = post_exn x q in
       TTapp (tt_var exn_post, [abs_pred tx qx; c]))
    (get_exns k.c_effect) 
    (abs_pred k.c_result_type (post_val q))

(* same, applied on [result] *)

and make_applied_post ren env k = function
  | None -> 
      None
  | Some (a, []) ->
      Some (TTpred a)
  | Some q ->
      let tt = make_post ren env result k q in
      Some (TTapp (tt, [tt_var result]))

let a_make_post ren env r k q =
  make_post ren env r (type_c_of_typing_info [] k) (post_app a_value q)

(* translation of type scheme *)

let trad_scheme_v ren env s =
  let (l,v) = specialize_type_scheme s in
  Vmap.fold
    (fun _ v cc -> 
       TTarrow((Ident.create ("A"^(string_of_int v.tag)),
		CC_var_binder TTSet),cc))
    l
    (trad_type_v ren env v)

(* builds [Val Ex] *) 

let make_valx x =
  CC_app (CC_term (Tvar Ident.exn_val), CC_type (exn_arg_type x))

(* builds [Val_e1 (Val_e2 ... (Val_en t))] *)

let make_val t xs = 
  List.fold_right (fun x cc -> CC_app (make_valx x, cc)) xs t

(* builds [Val_e1 (Val_e2 ... (Exn_ei t))] *)

let rec make_exn ty x v = function
  | [] -> 
      assert false
  | y :: yl when x = y -> 
      let v = match v with Some v -> v | None -> Tconst ConstUnit in
      CC_app (CC_app (CC_term (Tvar Ident.exn_exn), 
		      CC_type (trad_exn_type yl ty)), CC_term v)
  | y :: yl -> 
      CC_app (make_valx y, make_exn ty x v yl)

(*s The Monadic operators. They are operating on values of the following
    type [interp] (functions producing a [cc_term] when given a renaming
    data structure). *)

type interp = Rename.t -> (Loc.position * predicate) cc_term

type result = 
  | Value of term
  | Exn of Ident.t * term option

let apply_result ren env = function
  | Value t -> Value (apply_term ren env t)
  | Exn (x, Some t) -> Exn (x, Some (apply_term ren env t))
  | Exn (_, None) as e -> e

(*s [unit k t ren env] constructs the tuple 
  
      (y1, ..., yn, t, ?::(q/ren y1 ... yn t))
  
   where the [yi] are the values of the output of [k].
   If there is no [yi] and no postcondition, it is simplified into 
   [t] itself. *)

let result_term ef v = function
  | Value t -> make_val (CC_term t) (Effect.get_exns ef)
  | Exn (x, t) -> make_exn v x t (Effect.get_exns ef)

let unit info r ren = 
  let env = info.t_env in
  let r = apply_result ren env r in
  let v = trad_type_v ren env info.t_result_type in
  let ef = info.t_effect in
  let t = result_term ef v r in
  let q = info.t_post in
  let ids = get_writes ef in
  if ids = [] && q = None then
    t
  else 
    let hole, holet = match q with
      | None -> 
	  [], None
      | Some q -> 
	  (* proof obligation *)
	  let h = 
	    let q = a_apply_post info.t_label ren env q in
	    let a = match r with 
	      | Value _ -> post_val q 
	      | Exn (x,_) -> post_exn x q 
	    in
	    match r with
	      | Value t | Exn (_, Some t) ->
		  a.a_loc, 
		  simplify (tsubst_in_predicate (subst_one result t) a.a_value)
	      | Exn _ ->
		  a.a_loc, a.a_value
	  in
	  (* type of the obligation: [y1]...[yn][res]Q *)
	  let ht = 
	    let _,o,xs,_ = get_repr ef in
	    let ren' = Rename.next ren (result :: o) in
	    let result' = current_var ren' result in
	    let q = a_apply_post info.t_label ren' env q in
	    let c = a_make_post ren' env result' info q in
	    let bl = 
	      List.map (fun id -> (current_var ren' id, 
				trad_type_in_env ren env id)) o
	    in
	    lambda_vars bl c
	  in
	  [ CC_hole h ], Some ht
    in
    CC_tuple (
      (List.map (fun id -> let id' = current_var ren id in CC_var id') ids) @
      t :: hole,
      holet)

(*s \pagebreak Case patterns *)

(* pattern for an exception: (Val (Val ... (Exn v))) *)
let exn_pattern dep x res xs =
  let rec make = function
    | [] -> 
	assert false
    | y :: yl when x = y -> 
	PPcons ((if dep then exn_qexn else exn_exn), res)
    | y :: yl -> 
	PPcons ((if dep then exn_qval else exn_val), [make yl])
  in
  make xs

(* pattern for a value: (Val (Val ... (Val v))) *)
let val_pattern dep res xs =
  let t = if dep then [PPcons (exist, res)] else res in
  let id = if dep then exn_qval else exn_val in
  List.hd (List.fold_right (fun x cc -> [PPcons (id, cc)]) xs t)

(*s [compose k1 e1 e2 ren env] constructs the term
   
        [ let h1 = ?:P1 in ... let hn = ?:Pm in ]
          let y1,y2,...,yn, res1 [,q] = <e1 ren> in
          <e2 res1 ren'>

    where [ren' = next w1 ren] and [y1,...,yn = w1/ren] *)

let binding_of_alist ren env =
  List.map
    (fun (id,id') -> (id', CC_var_binder (trad_type_in_env ren env id)))

let make_pre env ren p = 
  let p = a_apply_assert ren env p in
  p.a_name, (p.a_loc, p.a_value)

let let_pre (id, h) cc = 
  CC_letin (false, [id, CC_pred_binder (snd h)], CC_hole h, cc)

let let_many_pre = List.fold_right let_pre

let decomp x b v = 
  let var id = CC_term (Tvar id) in
  match b with
  | [] -> var v
  | (id,_) :: _ -> CC_app (var (decomp (List.length x)), var id)

(* [isapp] signals an application.
   [handler x res : interp] is the handler for exception [x] with value [res].
*)

let gen_compose isapp handler info1 e1 info2 e2 ren =
  let env = info1.t_env in
  let (res1,v1),ef1,q1 = decomp_kappa info1 in
  let ren = push_date ren info1.t_label in
  let r1,w1,x1,_ = get_repr ef1 in
  let ren' = next ren w1 in
  let res1,ren' = fresh ren' res1 in
  let tv1 = trad_type_v ren env v1 in
  let tres1 = trad_exn_type x1 tv1 in
  let b = [res1, CC_var_binder tres1] in
  let b',dep = match q1 with
    | None -> 
	[], false
    | Some q1 -> 
	if x1 = [] then
	  let (q,_) = a_apply_post info1.t_label ren' env q1 in
	  let hyp = subst_in_predicate (subst_onev result res1) q.a_value in
	  [q.a_name, CC_pred_binder hyp], true 
	else
	  let tt = TTapp (a_make_post ren' env res1 info1 q1, [tt_var res1]) in
	  [post_name (), CC_var_binder tt], true 
  in
  let vo = current_vars ren' w1 in
  let bl = (binding_of_alist ren env vo) @ b @ b' in
  let cc1 = 
    if isapp then 
      let input = List.map (fun (_,id') -> CC_var id') (current_vars ren r1) in
      cc_applist (e1 ren) input
    else
      e1 ren
  in

  let cc2 =
    if x1 = [] then
      (* e1 does not raise any exception *)
      e2 res1 ren'
    else
      (* e1 may raise an exception *)
      let q1 = option_app (a_apply_post info1.t_label ren' env) q1 in
      let q1 = optpost_app (subst_in_assertion (subst_onev result res1)) q1 in
      let pat_post a = 
	PPvariable (a.a_name, CC_pred_binder a.a_value) 
      in
      let exn_branch x =
	let px = 
	  (match find_exception x with 
	     | Some pt1 -> PPvariable (res1, CC_var_binder (TTpure pt1))
	     | None -> PPvariable (anonymous, CC_var_binder (TTpure PTunit))) 
	  ::
	  (match q1 with
	     | Some q1 -> let a = post_exn x q1 in [pat_post a]
	     | None -> [])
	in
	exn_pattern dep x px x1, handler x res1 ren'
      in
      let pres1 = 
	(PPvariable (res1, CC_var_binder tv1)) :: 
	(match q1 with Some (q,_) -> [pat_post q] | None -> [])
      in
      CC_case (decomp x1 b' res1,
	       (val_pattern dep pres1 x1, e2 res1 ren') ::
	       (List.map exn_branch x1))
  in
  CC_letin (dep, bl, cc1, cc2)

(* [compose], [apply] and [handle] are instances of [gen_compose].
   [compose] and [apply] use the default handler [reraise]. *)

let reraise info x res =
  let xt = find_exception x in
  let r = Exn (x, option_app (fun _ -> Tvar res) xt) in
  unit info r

let compose info1 e1 info2 e2 = 
  gen_compose false (reraise info2) info1 e1 info2 e2

let apply info1 e1 info2 e2 = 
  gen_compose true (reraise info2) info1 e1 info2 e2

let lift x = 
  fun _ -> CC_var x

let handle info1 e1 info hl = 
  let handler x res = 
    let rec lookup = function
      | [] -> reraise info x res
      | ((y,_),h) :: _ when x = y -> h res
      | _ :: hl -> lookup hl
    in
    lookup hl
  in
  gen_compose false handler info1 e1 info (fun v -> unit info (Value (Tvar v)))

(*s [exn] is the operator corresponding to [raise]. *)

let exn info id t = unit info (Exn (id, t))

(*s [cross_label] is an operator to be used when a label is encountered *)

let cross_label l e ren = e (push_date ren l)

(*s Inserting assertions *)

let insert_pre env p t ren = 
  let_pre (make_pre env ren p) (t ren)

let insert_many_pre env pl t =
  List.fold_left (fun t h -> insert_pre env h t) t pl

(*s [abstraction env k e] abstracts a term [e] with respect to the 
    list of read variables, to the preconditions/assertions, i.e.
    builds

      [x1]...[xk][h1:P1]...[hn:Pn]let h'1 = ?:P'1 in ... let H'm = ?:P'm in t
    *)

let make_abs bl t = match bl with
  | [] -> t
  | _  -> cc_lam bl t

let bind_pre ren env p =
  p.a_name, CC_pred_binder (a_apply_assert ren env p).a_value

let abs_pre env pl t =
  List.fold_right
    (fun p t ren -> CC_lam (bind_pre ren env p, t ren))
    pl t

let abstraction info pl e ren =
  let env = info.t_env in
  let _,ef,_ = decomp_kappa info in
  let ids = get_reads ef in
  let al = current_vars ren ids in
  let bl = binding_of_alist ren env al in
  let c = abs_pre env pl e ren in
  make_abs bl c

let fresh id e ren =
  let id',ren' = Rename.fresh ren id in
  e id' ren'

(*s Well-founded recursion. 

    \begin{verbatim}
    (well_founded_induction
       A R ?::(well_founded A R)
       [Phi:A] (bl) (x) Phi=phi(x) -> <info>
       [Phi:A][w:(Phi0:A) Phi0<Phi -> (bl) (x) Phi=phi(x) -> <info>]
         [bl][x][eq:Phi=phi(x)][h:<pre info>]

            <body where w <- (w phi(x1) ?::phi(x1)<Phi 
                              argl x1 ?::phi(x1)=phi(x1) ?::<pre info>)>

       phi(x) bl x ?::phi(x)=phi(x) ?::<pre info>)
    \end{verbatim}
*)

let wfrec_with_binders bl (_,phi,a,r) pre info f ren =
  let env = info.t_env in
  let vphi = variant_name () in
  let wr = get_writes info.t_effect in
  let info' = { info with t_effect = keep_writes info.t_effect } in
  let a = TTpure a in
  let w = wf_name () in
  let k = info' in
  let ren' = next ren (get_writes k.t_effect) in 
  let tphi = 
    tt_arrow bl (trad_type_c ren' env (type_c_of_typing_info pre k)) 
  in
  let vphi0 = variant_name () in
  let tphi0 = 
    let k0 = 
      type_c_subst (subst_onev vphi vphi0) (type_c_of_typing_info pre k) 
    in 
    tt_arrow bl (trad_type_c ren' env k0) 
  in
  let input ren =
    let args = List.map (fun (id,_) -> CC_var id) bl in
    let input = List.map (fun (_,id') -> CC_var id') (current_vars ren wr) in
    let pl = 
      (Misc.anonymous info.t_loc (equality phi phi)) :: pre
    in
    let holes = 
      List.map (fun p -> 
		  let p = a_apply_assert ren env p in
		  CC_hole (p.a_loc, p.a_value)) pl 
    in
    args @ input @ holes
  in
  let tw = 
    let r_phi0_phi = Papp (r, [Tvar vphi0; Tvar vphi], []) in
    TTarrow ((vphi0, CC_var_binder a),
	     TTarrow ((pre_name Anonymous, CC_pred_binder r_phi0_phi), tphi0))
  in
  let fw ren = 
    let tphi = apply_term ren env phi in
    let decphi = info.t_loc, Papp (r, [tphi; Tvar vphi], []) in
    cc_applist (CC_var w) ([CC_term tphi; CC_hole decphi] @ input ren) 
  in
  cc_applist (CC_var well_founded_induction)
    ([CC_type a; CC_term (Tvar r);
      CC_hole (info.t_loc, Papp (well_founded, [Tvar r], []));
      CC_type (TTlambda ((vphi, CC_var_binder a), tphi));
      cc_lam 
	([vphi, CC_var_binder a; w, CC_var_binder tw] @ bl)
	(abstraction info' pre (f fw) ren');
      CC_term (apply_term ren env phi)] @
     input ren)

let wfrec = wfrec_with_binders []


(*s Interpretations of recursive functions are stored in the following
    table. *)

let rec_table = Hashtbl.create 97

let is_rec = Hashtbl.mem rec_table
let find_rec = Hashtbl.find rec_table

let with_rec f tf e ren = 
  Hashtbl.add rec_table f tf; 
  let r = e ren in 
  Hashtbl.remove rec_table f; 
  r

