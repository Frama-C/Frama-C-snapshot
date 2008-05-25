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

open Format
open Jc_env
open Jc_envset
open Jc_fenv
open Jc_constructors
open Jc_ast
open Jc_pervasives

module type TAst = sig
  type t
  val subs: t -> t list
end

module type TIterators = sig
  type t

  val subs: t -> t list
  val iter: (t -> unit) -> t -> unit

  (* Parcours en profondeur d'abord, mais l'accumulateur obtenu est ensuite
     passé aussi en largeur. *)
  val fold_left: ('a -> t -> 'a) -> 'a -> t -> 'a
  val fold_right: (t -> 'a -> 'a) -> t -> 'a -> 'a

  (* Parcours en profondeur avec accumulateur (le même pour tous les fils). *)
  val iter_deep_left: ('a -> t -> 'a) -> 'a -> t -> unit
  val iter_deep_right: (t -> 'a -> 'a) -> t -> 'a -> unit
end

module Iterators(X: TAst): TIterators with type t = X.t = struct
  type t = X.t
  let subs = X.subs
  let rec iter f x = f x; List.iter (iter f) (subs x)
  let rec fold_left f a x =
    let a = f a x in
    List.fold_left (fold_left f) a (subs x)
  let rec fold_right f x a =
    let a = f x a in
    List.fold_right (fold_right f) (subs x) a
  let rec iter_deep_left f a x =
    let a = f a x in
    List.iter (iter_deep_left f a) (subs x)
  let rec iter_deep_right f x a =
    let a = f x a in
    List.iter (fun x -> iter_deep_right f x a) (subs x)
end

(*****************************************************************************)
(* General iterators on expressions.                                         *)
(*****************************************************************************)

let replace_sub_expr e el =
  let as1 = function [e1] -> e1 | _ -> assert false in
  let as2 = function [e1;e2] -> e1,e2 | _ -> assert false in
  let as3 = function [e1;e2;e3] -> e1,e2,e3 | _ -> assert false in
  let pop = function e1::r -> e1,r | _ -> assert false in
  let popopt el = function 
    | None -> None,el 
    | Some _ -> let e1,r = pop el in Some e1,r
  in
  let rec popn n el = 
    if n > 0 then 
      let e,r = pop el in
      let el1,el2 = popn (n-1) r in
      e :: el1, el2
    else [],el
  in
  let enode = match e#node with
    | JCEconst _ | JCEvar _ | JCEassert _ | JCEreturn_void as enode -> enode
    | JCEbinary(_e1,bop,_e2) ->
	let e1,e2 = as2 el in JCEbinary(e1,bop,e2) 
    | JCEshift(_e1,_e2) ->
	let e1,e2 = as2 el in JCEshift(e1,e2) 
    | JCEunary(uop,_e1) ->
	let e1 = as1 el in JCEunary(uop,e1)
    | JCEassign_var(vi,_e2) ->
	let e2 = as1 el in JCEassign_var(vi,e2)
    | JCEassign_heap(_e1,fi,_e2) ->
	let e1,e2 = as2 el in JCEassign_heap(e1,fi,e2)
    | JCEderef(_e1,fi) ->
	let e1 = as1 el in JCEderef(e1,fi)
    | JCEapp call ->
	JCEapp { call with jc_call_args = el }
    | JCEoffset(off,_e,st) ->
	let e1 = as1 el in JCEoffset(off,e1,st)
    | JCEinstanceof(_e,st) ->
	let e1 = as1 el in JCEinstanceof(e1,st)
    | JCEcast(_e,st) ->
	let e1 = as1 el in JCEcast(e1,st)
    | JCEreal_cast(_e,st) ->
	let e1 = as1 el in JCEreal_cast(e1,st)
    | JCErange_cast(_e,st) ->
	let e1 = as1 el in JCErange_cast(e1,st)
    | JCEif(_e1,_e2,_e3) ->
	let e1,e2,e3 = as3 el in JCEif(e1,e2,e3)
    | JCEmatch(_e1,ptl) ->
	let e1,el = pop el in
	let ptl = List.map2 (fun (p,_e) e -> p,e) ptl el in
	JCEmatch(e1,ptl)
    | JCElet(vi,eopt,_e) ->
	let eopt,el = popopt el eopt in
	let e1 = as1 el in JCElet(vi,eopt,e1)
    | JCEalloc(_e,name) ->
	let e1 = as1 el in JCEalloc(e1,name)
    | JCEfree _e ->
	let e1 = as1 el in JCEfree e1
    | JCEblock elist ->
	assert (List.length elist = List.length el);
	JCEblock el
    | JCEloop(annot,_body) ->
	let body = as1 el in 
	JCEloop(annot,body)
    | JCEreturn(ty,_e) ->
	let e1 = as1 el in JCEreturn(ty,e1)
    | JCEtry(body,catches,finally) ->
	let body,el = pop el in
	let catches,el = 
	  List.fold_left (fun (acc,el) (id,name,_e) -> 
			    let e1,el = pop el in (id,name,e1)::acc,el
			 ) ([],el) catches
	in
	let catches = List.rev catches in
	let finally = as1 el in
	JCEtry(body,catches,finally)
    | JCEthrow(id,eopt) ->
	let eopt,el = popopt el eopt in
        JCEthrow(id,eopt)
    | JCEpack(st1,_e,st2) ->
	let e1 = as1 el in JCEpack(st1,e1,st2)
    | JCEunpack(st1,_e,st2) ->
	let e1 = as1 el in JCEunpack(st1,e1,st2)
  in
  new expr_with ~node:enode e

module ExprAst = struct
  type t = expr
  let subs e =
    match e#node with
      | JCEconst _
      | JCEvar _
      | JCEassert _
      | JCEreturn_void 
      | JCEthrow(_, None) ->
          []
      | JCEderef(e, _)
      | JCEunary(_, e)
      | JCEassign_var(_, e)
      | JCEinstanceof(e, _)
      | JCEcast(e, _)
      | JCErange_cast(e, _)
      | JCEreal_cast(e, _)
      | JCEoffset(_, e, _)
      | JCEalloc(e, _)
      | JCEfree e
      | JCElet(_, None, e)
      | JCEloop(_, e)
      | JCEreturn(_, e)
      | JCEthrow(_, Some e)
      | JCEpack(_, e, _)
      | JCEunpack(_, e, _) ->
          [e]
      | JCEbinary(e1, _, e2)
      | JCEassign_heap(e1, _, e2)
      | JCElet(_, Some e1, e2)
      | JCEshift(e1, e2) ->
          [e1; e2]
      | JCEif(e1, e2, e3) ->
          [e1; e2; e3]
      | JCEblock el ->
          el
      | JCEapp call ->
	  call.jc_call_args
      | JCEtry(e1, l, e2) ->
          e1 :: List.map (fun (_, _, e) -> e) l @ [ e2 ]
      | JCEmatch(e, pel) ->
          e :: List.map snd pel
end

module IExpr = Iterators(ExprAst)

let rec map_expr ?(before = fun x -> x) ?(after = fun x -> x) e =
  let e = before e in
  let elist = List.map (map_expr ~before ~after) (ExprAst.subs e) in
  after (replace_sub_expr e elist)

module NExprAst = struct
  type t = nexpr
  let subs e =
    match e#node with
      | JCNEconst _
      | JCNEvar _
      | JCNEreturn None
      | JCNEthrow(_, None)
      | JCNEtagequality _
      | JCNErange(None, None) ->
          []
      | JCNElabel(_, e)
      | JCNEderef(e, _)
      | JCNEunary(_, e)
      | JCNEinstanceof(e, _)
      | JCNEcast(e, _)
      | JCNEoffset(_, e)
      | JCNEalloc(e, _)
      | JCNEfree e
      | JCNElet(_, _, None, e)
      | JCNEassert e
      | JCNEreturn(Some e)
      | JCNEthrow(_, Some e)
      | JCNEpack(e, _)
      | JCNEunpack(e, _)
      | JCNEquantifier(_, _, _, e)
      | JCNEold e
      | JCNEat(e, _)
      | JCNEmutable(e, _)
      | JCNErange(Some e, None)
      | JCNErange(None, Some e) ->
          [e]
      | JCNEbinary(e1, _, e2)
      | JCNEassign(e1, e2)
      | JCNElet(_, _, Some e1, e2)
      | JCNEloop(e1, None, e2)
      | JCNErange(Some e1, Some e2) ->
          [e1; e2]
      | JCNEif(e1, e2, e3)
      | JCNEloop(e1, Some e2, e3) ->
          [e1; e2; e3]
      | JCNEapp(_, _, el)
      | JCNEblock el ->
          el
      | JCNEmatch(e, pel) ->
          e :: List.map snd pel
      | JCNEtry(e1, l, e2) ->
          e1 :: List.map (fun (_, _, e) -> e) l @ [e2]
end

module INExpr = Iterators(NExprAst)

(*****************************************************************************)
(* General iterators on parsed expressions.                                  *)
(*****************************************************************************)

let replace_sub_pexpr e el =
  let as1 = function [e1] -> e1 | _ -> assert false in
  let as2 = function [e1;e2] -> e1,e2 | _ -> assert false in
  let as3 = function [e1;e2;e3] -> e1,e2,e3 | _ -> assert false in
  let pop = function e1::r -> e1,r | _ -> assert false in
  let popopt el = function 
    | None -> None,el 
    | Some _ -> let e1,r = pop el in Some e1,r
  in
  let rec popn n el = 
    if n > 0 then 
      let e,r = pop el in
      let el1,el2 = popn (n-1) r in
      e :: el1, el2
    else [],el
  in
  let enode = match e#node with
    | JCPEconst _ | JCPEvar _ | JCPEtagequality _ | JCPEbreak _
    | JCPEcontinue _ | JCPEgoto _ as enode -> enode
    | JCPElabel(lab,_e1) -> 
	let e1 = as1 el in JCPElabel(lab,e1)
    | JCPEbinary(_e1,bop,_e2) ->
	let e1,e2 = as2 el in JCPEbinary(e1,bop,e2) 
    | JCPEunary(uop,_e1) ->
	let e1 = as1 el in JCPEunary(uop,e1)
    | JCPEassign(_e1,_e2) ->
	let e1,e2 = as2 el in JCPEassign(e1,e2)
    | JCPEassign_op(_e1,op,_e2) ->
	let e1,e2 = as2 el in JCPEassign_op(e1,op,e2)
    | JCPEderef(_e1,fi) ->
	let e1 = as1 el in JCPEderef(e1,fi)
    | JCPEapp(fi,labs,_args) ->
	JCPEapp(fi,labs,el)
    | JCPEquantifier(q,ty,labs,_e) ->
	let e1 = as1 el in JCPEquantifier(q,ty,labs,e1)
    | JCPEold _e ->
	let e1 = as1 el in JCPEold(e1)
    | JCPEat(_e,lab) ->
	let e1 = as1 el in JCPEat(e1,lab)
    | JCPEoffset(off,_e) ->
	let e1 = as1 el in JCPEoffset(off,e1)
    | JCPEinstanceof(_e,st) ->
	let e1 = as1 el in JCPEinstanceof(e1,st)
    | JCPEcast(_e,st) ->
	let e1 = as1 el in JCPEcast(e1,st)
    | JCPEif(_e1,_e2,_e3) ->
	let e1,e2,e3 = as3 el in JCPEif(e1,e2,e3)
    | JCPErange(e1opt,e2opt) ->
	let e1opt,el = popopt el e1opt in
	let e2opt,el = popopt el e2opt in
	assert (el = []);
	JCPErange(e1opt,e2opt)
    | JCPEmatch(_e1,ptl) ->
	let e1,el = pop el in
	let ptl = List.map2 (fun (p,_e) e -> p,e) ptl el in
	JCPEmatch(e1,ptl)
    | JCPElet(tyopt,name,eopt,_e) ->
	let eopt,el = popopt el eopt in
	let e1 = as1 el in JCPElet(tyopt,name,eopt,e1)
    | JCPEdecl(ty,name,eopt) ->
	let eopt,el = popopt el eopt in
	assert (el = []);
	JCPEdecl(ty,name,eopt)
    | JCPEalloc(_e,name) ->
	let e1 = as1 el in JCPEalloc(e1,name)
    | JCPEfree _e ->
	let e1 = as1 el in JCPEfree e1
    | JCPEmutable(_e,tag) ->
	let e1 = as1 el in JCPEmutable(e1,tag)
    | JCPEblock elist ->
	assert (List.length elist = List.length el);
	JCPEblock el
    | JCPEassert _e ->
	let e1 = as1 el in JCPEassert e1
    | JCPEwhile(_test,_inv,var,_body) ->
	let test,el = pop el in
	let inv,el = pop el in
	let var,el = popopt el var in
	let body = as1 el in 
	JCPEwhile(test,inv,var,body)
    | JCPEfor(inits,test,updates,inv,var,body) ->
	let inits,el = popn (List.length inits) el in
	let test,el = pop el in
	let inv,el = pop el in
	let updates,el = popn (List.length updates) el in
	let var,el = popopt el var in
	let body = as1 el in 
	JCPEfor(inits,test,updates,inv,var,body)
    | JCPEreturn _ ->
	let e1 = as1 el in JCPEreturn e1
    | JCPEtry(body,catches,finally) ->
	let body,el = pop el in
	let catches,el = 
	  List.fold_left (fun (acc,el) (id,name,_e) -> 
			    let e1,el = pop el in (id,name,e1)::acc,el
			 ) ([],el) catches
	in
	let catches = List.rev catches in
	let finally = as1 el in
	JCPEtry(body,catches,finally)
    | JCPEthrow(id,_) ->
        let e1 = as1 el in JCPEthrow(id,e1)
    | JCPEpack(_e,id) ->
	let e1 = as1 el in JCPEpack(e1,id)
    | JCPEunpack(_e,id) ->
	let e1 = as1 el in JCPEunpack(e1,id)
    | JCPEswitch(_e,cases) ->
	let e1,el = pop el in
	let cases,el = 
	  List.fold_left 
	    (fun (acc,el) (caselist,_e) ->
	       let caselist,el = 
		 List.fold_left 
		   (fun (acc,el) eopt ->
		      let eopt,el = popopt el eopt in
		      eopt::acc,el
		   ) ([],el) caselist
	       in
	       let caselist = List.rev caselist in
	       let e1,el = pop el in
	       (caselist,e1)::acc,el
	    ) ([],el) cases 
	in
	let cases = List.rev cases in	
	assert (el = []);
	JCPEswitch(e1,cases)
  in
  new pexpr_with ~node:enode e

module PExprAst = struct
  type t = pexpr
  let subs e =
    match e#node with
      | JCPEconst _
      | JCPEvar _
      | JCPEtagequality _ 
      | JCPEbreak _
      | JCPEcontinue _ 
      | JCPEgoto _ 
      | JCPErange(None,None)
      | JCPEdecl(_,_,None) ->
          []
      | JCPEassert e
      | JCPElabel(_, e)
      | JCPEderef(e, _)
      | JCPEunary(_, e)
      | JCPEinstanceof(e, _)
      | JCPEcast(e, _)
      | JCPEoffset(_, e)
      | JCPEalloc(e, _)
      | JCPEfree e
      | JCPElet(_, _,None, e)
      | JCPEreturn e
      | JCPEthrow(_, e)
      | JCPEpack(e, _)
      | JCPEunpack(e, _)
      | JCPEquantifier(_,_,_,e)
      | JCPEold e
      | JCPEat(e,_)
      | JCPErange(Some e,None)
      | JCPErange(None,Some e)
      | JCPEdecl(_,_,Some e)
      | JCPEmutable(e,_) ->
          [e]
      | JCPEbinary(e1, _, e2)
      | JCPEassign(e1, e2)
      | JCPEassign_op(e1, _, e2)
      | JCPElet(_, _, Some e1, e2)
      | JCPErange(Some e1,Some e2) ->
          [e1; e2]
      | JCPEif(e1, e2, e3)
      | JCPEwhile(e1,e2,None,e3) ->
          [e1; e2; e3]
      | JCPEwhile(e1,e2,Some e3,e4) ->
          [e1; e2; e3; e4]
      | JCPEblock el
      | JCPEapp(_,_,el) ->
	  el
      | JCPEtry(e1, l, e2) ->
          e1 :: List.map (fun (_, _, e) -> e) l @ [ e2 ]
      | JCPEmatch(e, pel) ->
          e :: List.map snd pel
      | JCPEfor(el1,e1,el2,e3,None,e4) ->
	  el1 @ e1 :: el2 @ [e3; e4]
      | JCPEfor(el1,e1,el2,e3,Some e4,e5) ->
	  el1 @ e1 :: el2 @ [e3; e4; e5]
      | JCPEswitch(e,cases) ->
	  let case c = 
	    List.flatten (List.map (function None -> [] | Some e -> [e]) c)
	  in
	  e :: List.flatten (List.map (fun (cases,e) -> case cases @ [e]) cases)
end

module IPExpr = Iterators(PExprAst)

let rec map_pexpr ?(before = fun x -> x) ?(after = fun x -> x) e =
  let e = before e in
  let elist = List.map (map_pexpr ~before ~after) (PExprAst.subs e) in
  after (replace_sub_pexpr e elist)


(*****************************************************************************)
(* General iterators on terms.                                               *)
(*****************************************************************************)

module TermAst = struct
  type t = term
  let subs t = 
    match t#node with
      | JCTconst _ | JCTvar _ | JCTrange(None,None) -> 
	  []
      | JCTbinary(t1,_,t2) | JCTshift(t1,t2) 
      | JCTrange(Some t1,Some t2) ->
	  [t1;t2]
      | JCTunary(_,t1) | JCTderef(t1,_,_) | JCTold t1 | JCTat(t1,_) 
      | JCToffset(_,t1,_)
      | JCTinstanceof(t1,_,_) | JCTcast(t1,_,_) | JCTrange_cast(t1,_) 
      | JCTreal_cast(t1,_) | JCTrange(Some t1,None)
      | JCTrange(None,Some t1) ->
	  [t1]
      | JCTapp app ->
	  app.jc_app_args 
      | JCTif(t1,t2,t3) ->
	  [t1;t2;t3]
      | JCTmatch(t, ptl) ->
	  t :: List.map snd ptl
end

module ITerm = Iterators(TermAst)

let fold_sub_term it f acc t =
  match t#node with
    | JCTconst _ | JCTvar _ | JCTrange(None,None) -> acc
    | JCTbinary(t1,_,t2) | JCTshift(t1,t2)
    | JCTrange(Some t1,Some t2) ->
	let acc = it f acc t1 in
	it f acc t2
    | JCTunary(_,t1) | JCTderef(t1,_,_) | JCTold t1 | JCToffset(_,t1,_)
    | JCTinstanceof(t1,_,_) | JCTcast(t1,_,_) | JCTrange_cast(t1,_) 
    | JCTreal_cast(t1,_) | JCTrange(Some t1,None)
    | JCTrange(None,Some t1) | JCTat(t1,_) ->
	it f acc t1
    | JCTapp app ->
	let tl = app.jc_app_args in
	List.fold_left (it f) acc tl
    | JCTif(t1,t2,t3) ->
	let acc = it f acc t1 in
	let acc = it f acc t2 in
	it f acc t3
    | JCTmatch(t, ptl) ->
	let acc = it f acc t in
	List.fold_left (fun acc (_, t) -> it f acc t) acc ptl

let rec fold_term f acc t =
  let acc = f acc t in
  fold_sub_term fold_term f acc t

let rec fold_rec_term f acc t =
  let cont,acc = f acc t in
  if cont then fold_sub_term fold_rec_term f acc t else acc

let rec map_term f t =
  let tnode = match t#node with
    | JCTconst _ | JCTvar _ | JCTrange (None, None) as tnode -> tnode
    | JCTbinary(t1,bop,t2) ->
	JCTbinary(map_term f t1,bop,map_term f t2) 
    | JCTunary(uop,t1) ->
	JCTunary(uop,map_term f t1)
    | JCTshift(t1,t2) ->
	JCTshift(map_term f t1,map_term f t2)
    | JCTderef(t1,lab,fi) ->
	JCTderef(map_term f t1,lab,fi)
    | JCTapp app ->
	let tl = app.jc_app_args in
	JCTapp { app with jc_app_args = List.map (map_term f) tl; }
    | JCTold t ->
	JCTold(map_term f t)
    | JCTat(t,lab) ->
	JCTat(map_term f t,lab)
    | JCToffset(off,t,st) ->
	JCToffset(off,map_term f t,st)
    | JCTinstanceof(t,lab,st) ->
	JCTinstanceof(map_term f t,lab,st)
    | JCTcast(t,lab,st) ->
	JCTcast(map_term f t,lab,st)
    | JCTrange_cast(t,ei) ->
	JCTrange_cast(map_term f t,ei)
    | JCTreal_cast(t,rc) ->
	JCTreal_cast(map_term f t,rc)
    | JCTif(t1,t2,t3) ->
	JCTif(map_term f t1,map_term f t2,map_term f t3)
    | JCTrange(Some t1,Some t2) ->
	JCTrange(Some (map_term f t1),Some (map_term f t2))
    | JCTrange(Some t1,None) ->
	JCTrange(Some (map_term f t1),None)
    | JCTrange(None,Some t2) ->
	JCTrange(None,Some (map_term f t2))
    | JCTmatch(t, ptl) ->
	JCTmatch(map_term f t, List.map (fun (p, t) -> p, map_term f t) ptl)
  in
  f (new term_with ~node:tnode t)


(*****************************************************************************)
(* Specific iterators on terms.                                              *)
(*****************************************************************************)

let raw_sub_term subt t =
  fold_term (fun acc t -> acc || raw_term_equal subt t) false t

let raw_strict_sub_term subt t =
  raw_term_compare subt t <> 0 && raw_sub_term subt t


(*****************************************************************************)
(* General iterators on assertions.                                          *)
(*****************************************************************************)

let rec iter_term_and_assertion ft fa a =
  fa a;
  match a#node with
    | JCAtrue | JCAfalse | JCAtagequality _ -> ()
    | JCArelation(t1,_,t2) -> 
	ITerm.iter ft t1;
	ITerm.iter ft t2
    | JCAapp app ->
	List.iter (ITerm.iter ft) app.jc_app_args
    | JCAinstanceof(t1,_,_) | JCAbool_term t1 | JCAmutable(t1,_,_) ->
	ITerm.iter ft t1
    | JCAand al | JCAor al ->
	List.iter (iter_term_and_assertion ft fa) al
    | JCAimplies(a1,a2) | JCAiff(a1,a2) ->
	iter_term_and_assertion ft fa a1;
	iter_term_and_assertion ft fa a2
    | JCAif(t1,a1,a2) ->
	ITerm.iter ft t1;
	iter_term_and_assertion ft fa a1;
	iter_term_and_assertion ft fa a2
    | JCAnot a1 | JCAquantifier(_,_,a1) | JCAold a1 | JCAat(a1,_) ->
	iter_term_and_assertion ft fa a1
    | JCAmatch(t, pal) ->
	ITerm.iter ft t;
	List.iter (fun (_, a) -> iter_term_and_assertion ft fa a) pal

let iter_term_and_assertion_in_loop_annot ft fa la =
  iter_term_and_assertion ft fa la.jc_loop_invariant;
  Option_misc.iter (ITerm.iter ft) la.jc_loop_variant

let iter_term_and_assertion_in_behavior ft fa bv =
  Option_misc.iter (iter_term_and_assertion ft fa) bv.jc_behavior_assumes;
  (* TODO: assigns *)
  iter_term_and_assertion ft fa bv.jc_behavior_ensures

let iter_term_and_assertion_in_fun_spec ft fa spec =
  iter_term_and_assertion ft fa spec.jc_fun_requires;
  List.iter (fun (_,_,bv) -> iter_term_and_assertion_in_behavior ft fa bv)
    spec.jc_fun_behavior

let rec fold_assertion f acc a =
  let acc = f acc a in
  match a#node with
    | JCAtrue | JCAfalse | JCArelation _ | JCAapp _ | JCAtagequality _ 
    | JCAinstanceof _ | JCAbool_term _ | JCAmutable _ -> 
	acc
    | JCAand al | JCAor al ->
	List.fold_left (fold_assertion f) acc al
    | JCAimplies(a1,a2) | JCAiff(a1,a2) | JCAif(_,a1,a2) ->
	let acc = fold_assertion f acc a1 in
	fold_assertion f acc a2
    | JCAnot a1 | JCAquantifier(_,_,a1) | JCAold a1 | JCAat(a1,_) ->
	fold_assertion f acc a1
    | JCAmatch(_, pal) ->
	List.fold_left (fun acc (_, a) -> fold_assertion f acc a) acc pal

let rec fold_term_in_assertion f acc a =
  match a#node with
    | JCAtrue | JCAfalse | JCAtagequality _ -> acc
    | JCArelation(t1,_,t2) -> 
	let acc = fold_term f acc t1 in
	fold_term f acc t2
    | JCAapp app ->
	List.fold_left (fold_term f) acc app.jc_app_args
    | JCAinstanceof(t1,_,_) | JCAbool_term t1 | JCAmutable(t1,_,_) ->
	fold_term f acc t1
    | JCAand al | JCAor al ->
	List.fold_left (fold_term_in_assertion f) acc al
    | JCAimplies(a1,a2) | JCAiff(a1,a2) ->
	let acc = fold_term_in_assertion f acc a1 in
	fold_term_in_assertion f acc a2
    | JCAif(t1,a1,a2) ->
	let acc = fold_term f acc t1 in
	let acc = fold_term_in_assertion f acc a1 in
	fold_term_in_assertion f acc a2
    | JCAnot a1 | JCAquantifier(_,_,a1) | JCAold a1 | JCAat(a1,_) ->
	fold_term_in_assertion f acc a1
    | JCAmatch(t, pal) ->
	let acc = fold_term f acc t in
	List.fold_left (fun acc (_, a) -> fold_term_in_assertion f acc a)
	  acc pal

let rec fold_term_and_assertion ft fa acc a =
  let acc = match a#node with
    | JCAtrue | JCAfalse | JCAtagequality _ -> acc
    | JCArelation(t1,_,t2) -> 
	let acc = fold_term ft acc t1 in
	fold_term ft acc t2
    | JCAapp app ->
	List.fold_left (fold_term ft) acc app.jc_app_args
    | JCAinstanceof(t1,_,_) | JCAbool_term t1 | JCAmutable(t1,_,_) ->
	fold_term ft acc t1
    | JCAand al | JCAor al ->
	List.fold_left (fold_term_and_assertion ft fa) acc al
    | JCAimplies(a1,a2) | JCAiff(a1,a2) ->
	let acc = fold_term_and_assertion ft fa acc a1 in
	fold_term_and_assertion ft fa acc a2
    | JCAif(t1,a1,a2) ->
	let acc = fold_term ft acc t1 in
	let acc = fold_term_and_assertion ft fa acc a1 in
	fold_term_and_assertion ft fa acc a2
    | JCAnot a1 | JCAquantifier(_,_,a1) | JCAold a1 | JCAat(a1,_) ->
	fold_term_and_assertion ft fa acc a1
    | JCAmatch(t, pal) ->
	let acc = fold_term ft acc t in
	List.fold_left (fun acc (_, a) -> fold_term_and_assertion ft fa acc a)
	  acc pal
  in
  fa acc a

let rec map_assertion f a =
  let anode = match a#node with
    | JCAtrue | JCAfalse | JCArelation _ | JCAapp _ | JCAtagequality _ 
    | JCAinstanceof _ | JCAbool_term _ | JCAmutable _ as anode -> 
	anode
    | JCAand al ->
	JCAand(List.map (map_assertion f) al)
    | JCAor al ->
	JCAor(List.map (map_assertion f) al)
    | JCAimplies(a1,a2) ->
	JCAimplies(map_assertion f a1,map_assertion f a2)
    | JCAiff(a1,a2) ->
	JCAiff(map_assertion f a1,map_assertion f a2)
    | JCAif(t,a1,a2) ->
	JCAif(t,map_assertion f a1,map_assertion f a2)
    | JCAnot a1 ->
	JCAnot(map_assertion f a1)
    | JCAquantifier(q,vi,a1) ->
	JCAquantifier(q,vi,map_assertion f a1)
    | JCAold a1 ->
	JCAold(map_assertion f a1)
    | JCAat(a1,lab) ->
	JCAat(map_assertion f a1,lab)
    | JCAmatch(t, pal) ->
	JCAmatch(t, List.map (fun (p, a) -> p, map_assertion f a) pal)
  in
  f (new assertion_with ~node:anode a)

let rec map_term_in_assertion f a =
  let anode = match a#node with
    | JCAtrue | JCAfalse | JCAtagequality _ as anode -> anode
    | JCArelation(t1,op,t2) -> 
	JCArelation(map_term f t1,op,map_term f t2)
    | JCAapp app ->
	JCAapp { app with jc_app_args = List.map (map_term f) app.jc_app_args }
    | JCAinstanceof(t1,lab,st) ->
	JCAinstanceof(map_term f t1,lab,st)
    | JCAbool_term t1 ->
	JCAbool_term(map_term f t1)
    | JCAmutable(t1,st,tag) ->
	JCAmutable(map_term f t1,st,tag)
    | JCAand al ->
	JCAand(List.map (map_term_in_assertion f) al)
    | JCAor al ->
	JCAor(List.map (map_term_in_assertion f) al)
    | JCAimplies(a1,a2) ->
	JCAimplies
	  (map_term_in_assertion f a1,map_term_in_assertion f a2)
    | JCAiff(a1,a2) ->
	JCAiff
	  (map_term_in_assertion f a1,map_term_in_assertion f a2)
    | JCAif(t1,a1,a2) ->
	JCAif(
	  map_term f t1,
	  map_term_in_assertion f a1,
	  map_term_in_assertion f a2)
    | JCAnot a1 ->
	JCAnot(map_term_in_assertion f a1)
    | JCAquantifier(q,vi,a1) ->
	JCAquantifier(q,vi,map_term_in_assertion f a1)
    | JCAold a1 ->
	JCAold(map_term_in_assertion f a1)
    | JCAat(a1,lab) ->
	JCAat(map_term_in_assertion f a1,lab)
    | JCAmatch(t, pal) ->
	JCAmatch(map_term f t,
		 List.map (fun (p, a) -> p, map_term_in_assertion f a) pal)
  in
  new assertion_with ~node:anode a

(* Remarque de Romain :
 * C'est un copier-coller d'au-dessus ?
 * Ca devrait pas plutôt être "map_term_and_assertion" ? *)
let rec map_term_in_assertion f a =
  let anode = match a#node with
    | JCAtrue | JCAfalse | JCAtagequality _ as anode -> anode
    | JCArelation(t1,op,t2) -> 
	JCArelation(map_term f t1,op,map_term f t2)
    | JCAapp app ->
	JCAapp { app with jc_app_args = List.map (map_term f) app.jc_app_args }
    | JCAinstanceof(t1,lab,st) ->
	JCAinstanceof(map_term f t1,lab,st)
    | JCAbool_term t1 ->
	JCAbool_term(map_term f t1)
    | JCAmutable(t1,st,tag) ->
	JCAmutable(map_term f t1,st,tag)
    | JCAand al ->
	JCAand(List.map (map_term_in_assertion f) al)
    | JCAor al ->
	JCAor(List.map (map_term_in_assertion f) al)
    | JCAimplies(a1,a2) ->
	JCAimplies
	  (map_term_in_assertion f a1,map_term_in_assertion f a2)
    | JCAiff(a1,a2) ->
	JCAiff
	  (map_term_in_assertion f a1,map_term_in_assertion f a2)
    | JCAif(t1,a1,a2) ->
	JCAif(
	  map_term f t1,
	  map_term_in_assertion f a1,
	  map_term_in_assertion f a2)
    | JCAnot a1 ->
	JCAnot(map_term_in_assertion f a1)
    | JCAquantifier(q,vi,a1) ->
	JCAquantifier(q,vi,map_term_in_assertion f a1)
    | JCAold a1 ->
	JCAold(map_term_in_assertion f a1)
    | JCAat(a1,lab) ->
	JCAat(map_term_in_assertion f a1,lab)
    | JCAmatch(t, pal) ->
	JCAmatch(map_term f t,
		 List.map (fun (p, a) -> p, map_term_in_assertion f a) pal)
  in
  new assertion_with ~node:anode a

(*****************************************************************************)
(* General iterators on patterns.                                            *)
(*****************************************************************************)

let rec iter_pattern f p =
  f p;
  match p#node with
    | JCPstruct(_, fipl) ->
	List.iter (iter_pattern f) (List.map snd fipl)
    | JCPor(p1, p2) ->
	iter_pattern f p1;
	iter_pattern f p2
    | JCPas(p, _) ->
	iter_pattern f p
    | JCPvar _
    | JCPany
    | JCPconst _ -> ()

let rec fold_pattern f acc p =
  let acc = f acc p in
  match p#node with
    | JCPstruct(_, fipl) ->
	List.fold_left (fold_pattern f) acc (List.rev (List.map snd fipl))
    | JCPor(p1, p2) ->
	let acc = fold_pattern f acc p1 in
	fold_pattern f acc p2
    | JCPas(p, _) ->
	fold_pattern f acc p
    | JCPvar _
    | JCPany
    | JCPconst _ -> ()

(*
Local Variables: 
compile-command: "LC_ALL=C make -C .. bin/jessie.byte"
End: 
*)
