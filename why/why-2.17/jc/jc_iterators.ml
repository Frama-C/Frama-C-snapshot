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

open Format
open Jc_env
open Jc_envset
open Jc_fenv
open Jc_constructors
open Jc_ast
open Jc_fenv
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
  let replace_sub_tag tag el =
    let tag_node,el = match tag#node with
      | JCPTtag _ | JCPTbottom as tag_node -> tag_node,el
      | JCPTtypeof e -> 
	  let e1,el = pop el in
	  JCPTtypeof e1,el
    in
    new ptag_with ~node:tag_node tag,el
  in
  let enode = match e#node with
    | JCPEconst _ | JCPEvar _ | JCPEbreak _
    | JCPEcontinue _ | JCPEgoto _ as enode -> enode
    | JCPEeqtype(tag1,tag2) ->
	let tag1,el = replace_sub_tag tag1 el in
	let tag2,el = replace_sub_tag tag2 el in
	assert (el == []);
	JCPEeqtype(tag1,tag2)
    | JCPEsubtype(tag1,tag2) ->
	let tag1,el = replace_sub_tag tag1 el in
	let tag2,el = replace_sub_tag tag2 el in
	assert (el == []);
	JCPEsubtype(tag1,tag2)
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
    | JCPEaddress(absolute,_e) ->
	let e1 = as1 el in JCPEaddress(absolute,e1)
    | JCPEbase_block(_e) ->
	let e1 = as1 el in JCPEbase_block(e1)
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
    | JCPEassert(behav,asrt,_e) ->
	let e1 = as1 el in JCPEassert(behav,asrt,e1)
    | JCPEcontract(req,dec,behs,_e) ->
	let e1 = as1 el in JCPEcontract(req,dec,behs,e1)
    | JCPEwhile(_test,inv1,var,_body) ->
	let test,el = pop el in
	let inv2,el = popn (List.length inv1) el in
	let inv = List.map2 (fun (behav,_) e -> behav,e) inv1 inv2 in
	let var,el = popopt el var in
	let body = as1 el in 
	JCPEwhile(test,inv,var,body)
    | JCPEfor(inits,test,updates,inv1,var,body) ->
	let inits,el = popn (List.length inits) el in
	let test,el = pop el in
	let updates,el = popn (List.length updates) el in
	let inv2,el = popn (List.length inv1) el in
	let inv = List.map2 (fun (behav,_) e -> behav,e) inv1 inv2 in
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
  let subtags tag = 
    match tag#node with
      | JCPTtag _ | JCPTbottom -> []
      | JCPTtypeof e -> [e]
  let subs e =
    match e#node with
      | JCPEconst _
      | JCPEvar _
      | JCPEbreak _
      | JCPEcontinue _ 
      | JCPEgoto _ 
      | JCPErange(None,None)
      | JCPEdecl(_,_,None) ->
          []
      | JCPEeqtype(tag1,tag2) | JCPEsubtype(tag1,tag2) ->
	  subtags tag1 @ subtags tag2
      | JCPEassert(_,_,e)
      | JCPElabel(_, e)
      | JCPEderef(e, _)
      | JCPEunary(_, e)
      | JCPEinstanceof(e, _)
      | JCPEcast(e, _)
      | JCPEoffset(_, e)
      | JCPEaddress(_,e)
      | JCPEbase_block(e)
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
      | JCPEif(e1, e2, e3) ->
	  [e1; e2; e3]
      | JCPEcontract(_,_,_,e) -> [e]
      | JCPEwhile(e1,inv,None,e3) ->
	  let e2list = List.map (fun (_behav,e) -> e) inv in
          e1 :: e2list @ [e3]
      | JCPEwhile(e1,inv,Some e3,e4) ->
	  let e2list = List.map (fun (_behav,e) -> e) inv in
          e1 :: e2list @ [e3; e4]
      | JCPEfor(el1,e1,el2,inv,None,e4) ->
	  let e2list = List.map (fun (_behav,e) -> e) inv in
	  el1 @ e1 :: el2 @ e2list @ [e4]
      | JCPEfor(el1,e1,el2,inv,Some e4,e5) ->
	  let e2list = List.map (fun (_behav,e) -> e) inv in
	  el1 @ e1 :: el2 @ e2list @ [e4; e5]
      | JCPEblock el
      | JCPEapp(_,_,el) ->
	  el
      | JCPEtry(e1, l, e2) ->
          e1 :: List.map (fun (_, _, e) -> e) l @ [ e2 ]
      | JCPEmatch(e, pel) ->
          e :: List.map snd pel
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
      | JCToffset(_,t1,_) | JCTaddress(_,t1) | JCTbase_block(t1)
      | JCTinstanceof(t1,_,_) | JCTcast(t1,_,_) | JCTbitwise_cast(t1,_,_) 
      | JCTrange_cast(t1,_) 
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
  let it = it f in
  match t#node with
    | JCTconst _ 
    | JCTvar _ -> 
	acc
    | JCTbinary(t1,_,t2)
    | JCTshift(t1,t2) ->
	let acc = it acc t1 in
	it acc t2
    | JCTrange(t1_opt,t2_opt) ->
	let acc = Option_misc.fold_left it acc t1_opt in
	Option_misc.fold_left it acc t2_opt
    | JCTunary(_,t1) 
    | JCTderef(t1,_,_)
    | JCTold t1
    | JCToffset(_,t1,_)
    | JCTaddress(_,t1)
    | JCTbase_block(t1)
    | JCTinstanceof(t1,_,_) 
    | JCTcast(t1,_,_) 
    | JCTbitwise_cast(t1,_,_) 
    | JCTreal_cast(t1,_) 
    | JCTrange_cast(t1,_) 
    | JCTat(t1,_) ->
	it acc t1
    | JCTapp app ->
	let tl = app.jc_app_args in
	List.fold_left it acc tl
    | JCTif(t1,t2,t3) ->
	let acc = it acc t1 in
	let acc = it acc t2 in
	it acc t3
    | JCTmatch(t, ptl) ->
	let acc = it acc t in
	List.fold_left (fun acc (_, t) -> it acc t) acc ptl

let rec fold_term f acc t =
  let acc = f acc t in
  fold_sub_term fold_term f acc t

let rec fold_rec_term f acc t =
  let cont,acc = f acc t in
  if cont then fold_sub_term fold_rec_term f acc t else acc

let iter_term ft t = fold_term (fold_unit ft) () t

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
    | JCTaddress(absolute,t) ->
	JCTaddress (absolute,map_term f t)
    | JCTbase_block(t) ->
	JCTbase_block (map_term f t)
    | JCTinstanceof(t,lab,st) ->
	JCTinstanceof(map_term f t,lab,st)
    | JCTcast(t,lab,st) ->
	JCTcast(map_term f t,lab,st)
    | JCTbitwise_cast(t,lab,st) ->
	JCTbitwise_cast(map_term f t,lab,st)
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
(* General iterators on assertions.                                          *)
(*****************************************************************************)

let rec iter_term_and_assertion ft fa a =
  let iter_tag tag = 
    match tag#node with
      | JCTtag _ | JCTbottom -> ()
      | JCTtypeof(t,_st) -> ITerm.iter ft t
  in
  fa a;
  match a#node with
    | JCAtrue | JCAfalse -> ()
    | JCAeqtype(tag1,tag2,_st) | JCAsubtype(tag1,tag2,_st) ->
	iter_tag tag1; 
	iter_tag tag2
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
  List.iter (fun (_behav,inv) ->
	       iter_term_and_assertion ft fa inv) la.jc_loop_invariant;
  iter_term_and_assertion ft fa la.jc_free_loop_invariant;
  Option_misc.iter (ITerm.iter ft) la.jc_loop_variant

let iter_term_and_assertion_in_behavior ft fa bv =
  Option_misc.iter (iter_term_and_assertion ft fa) bv.jc_behavior_assumes;
  (* TODO: assigns *)
  iter_term_and_assertion ft fa bv.jc_behavior_ensures;
  iter_term_and_assertion ft fa bv.jc_behavior_free_ensures

let iter_term_and_assertion_in_fun_spec ft fa spec =
  iter_term_and_assertion ft fa spec.jc_fun_requires;
  List.iter (fun (_,_,bv) -> iter_term_and_assertion_in_behavior ft fa bv)
    (spec.jc_fun_default_behavior :: spec.jc_fun_behavior)

let rec fold_assertion f acc a =
  let acc = f acc a in
  match a#node with
    | JCAtrue | JCAfalse | JCArelation _ | JCAapp _ | JCAeqtype _ 
    | JCAinstanceof _ | JCAbool_term _ | JCAmutable _ | JCAsubtype _ -> 
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
	  
(* let fold_term_in_tag f acc tag =  *)
(*   match tag#node with *)
(*     | JCTtag _ | JCTbottom -> acc *)
(*     | JCTtypeof(t,_st) -> fold_term f acc t *)

let fold_sub_term_in_tag it f acc tag = 
  match tag#node with
    | JCTtag _ | JCTbottom -> acc
    | JCTtypeof(t,_st) -> it f acc t

let fold_term_in_tag f acc tag =
  fold_sub_term_in_tag fold_term f acc tag

let fold_rec_term_in_tag f acc tag =
  fold_sub_term_in_tag fold_rec_term f acc tag

let rec fold_term_in_assertion f acc a =
  match a#node with
    | JCAtrue | JCAfalse -> acc
    | JCAeqtype(tag1,tag2,_st) | JCAsubtype(tag1,tag2,_st) ->
	let acc = fold_term_in_tag f acc tag1 in
	fold_term_in_tag f acc tag2
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

(* let rec fold_term_and_assertion ft fa acc a = *)
(*   let acc = match a#node with *)
(*     | JCAtrue | JCAfalse -> acc *)
(*     | JCAeqtype(tag1,tag2,_st) | JCAsubtype(tag1,tag2,_st) -> *)
(* 	let acc = fold_term_in_tag ft acc tag1 in *)
(* 	fold_term_in_tag ft acc tag2 *)
(*     | JCArelation(t1,_,t2) ->  *)
(* 	let acc = fold_term ft acc t1 in *)
(* 	fold_term ft acc t2 *)
(*     | JCAapp app -> *)
(* 	List.fold_left (fold_term ft) acc app.jc_app_args *)
(*     | JCAinstanceof(t1,_,_) | JCAbool_term t1 | JCAmutable(t1,_,_) -> *)
(* 	fold_term ft acc t1 *)
(*     | JCAand al | JCAor al -> *)
(* 	List.fold_left (fold_term_and_assertion ft fa) acc al *)
(*     | JCAimplies(a1,a2) | JCAiff(a1,a2) -> *)
(* 	let acc = fold_term_and_assertion ft fa acc a1 in *)
(* 	fold_term_and_assertion ft fa acc a2 *)
(*     | JCAif(t1,a1,a2) -> *)
(* 	let acc = fold_term ft acc t1 in *)
(* 	let acc = fold_term_and_assertion ft fa acc a1 in *)
(* 	fold_term_and_assertion ft fa acc a2 *)
(*     | JCAnot a1 | JCAquantifier(_,_,a1) | JCAold a1 | JCAat(a1,_) -> *)
(* 	fold_term_and_assertion ft fa acc a1 *)
(*     | JCAmatch(t, pal) -> *)
(* 	let acc = fold_term ft acc t in *)
(* 	List.fold_left (fun acc (_, a) -> fold_term_and_assertion ft fa acc a) *)
(* 	  acc pal *)
(*   in *)
(*   fa acc a *)

let rec fold_sub_term_and_assertion itt ita ft fa acc a =
  match a#node with
    | JCAtrue | JCAfalse -> acc
    | JCAeqtype(tag1,tag2,_st) | JCAsubtype(tag1,tag2,_st) ->
	let acc = fold_sub_term_in_tag itt ft acc tag1 in
	fold_sub_term_in_tag itt ft acc tag2
    | JCArelation(t1,_,t2) -> 
	let acc = itt ft acc t1 in
	itt ft acc t2
    | JCAapp app ->
	List.fold_left (itt ft) acc app.jc_app_args
    | JCAinstanceof(t1,_,_) | JCAbool_term t1 | JCAmutable(t1,_,_) ->
	itt ft acc t1
    | JCAand al | JCAor al ->
	List.fold_left (ita ft fa) acc al
    | JCAimplies(a1,a2) | JCAiff(a1,a2) ->
	let acc = ita ft fa acc a1 in
	ita ft fa acc a2
    | JCAif(t1,a1,a2) ->
	let acc = itt ft acc t1 in
	let acc = ita ft fa acc a1 in
	ita ft fa acc a2
    | JCAnot a1 | JCAquantifier(_,_,a1) | JCAold a1 | JCAat(a1,_) ->
	ita ft fa acc a1
    | JCAmatch(t, pal) ->
	let acc = itt ft acc t in
	List.fold_left (fun acc (_, a) -> ita ft fa acc a)
	  acc pal

let rec fold_term_and_assertion ft fa acc a =
  let acc = fa acc a in
  fold_sub_term_and_assertion fold_term fold_term_and_assertion ft fa acc a

let rec fold_rec_term_and_assertion ft fa acc a =
  let cont,acc = fa acc a in
  if cont then 
    fold_sub_term_and_assertion 
      fold_rec_term fold_rec_term_and_assertion ft fa acc a 
  else acc

let iter_term_and_assertion ft fa a = 
  fold_term_and_assertion (fold_unit ft) (fold_unit fa) () a

let fold_sub_location_set itt itls ft fls acc locs =
  let itt = itt ft and itls = itls ft fls in
  match locs#node with
    | JCLSvar _vi ->
	acc
    | JCLSderef(locs,_lab,_fi,_r) ->
	itls acc locs
    | JCLSrange(locs,t1_opt,t2_opt) ->
	let acc = itls acc locs in
	let acc = Option_misc.fold_left itt acc t1_opt in
	Option_misc.fold_left itt acc t2_opt 
   | JCLSrange_term(t0,t1_opt,t2_opt) ->
	let acc = itt acc t0 in
	let acc = Option_misc.fold_left itt acc t1_opt in
	Option_misc.fold_left itt acc t2_opt 

let rec fold_location_set ft fls acc locs =
  let acc = fls acc locs in
  fold_sub_location_set fold_term fold_location_set ft fls acc locs

let rec fold_rec_location_set ft fls acc locs =
  let cont,acc = fls acc locs in
  if cont then 
    fold_sub_location_set fold_rec_term fold_rec_location_set ft fls acc locs 
  else acc
  
let iter_location_set ft fls loc =
  fold_location_set (fold_unit ft) (fold_unit fls) () loc
  
let fold_sub_location itt itl itls ft fl fls acc loc =
  let itt = itt ft and itl = itl ft fl fls and itls = itls ft fls in
  match loc#node with
    | JCLvar _vi ->
	acc
    | JCLderef(locs,_lab,_fi,_r) ->
	itls acc locs
    | JCLderef_term(locs,_fi) ->
	itt acc locs
    | JCLat(loc,_lab) ->
	itl acc loc

let rec fold_location ft fl fls acc loc =
  let acc = fl acc loc in
  fold_sub_location fold_term fold_location fold_location_set ft fl fls acc loc

let rec fold_rec_location ft fl fls acc loc =
  let cont,acc = fl acc loc in
  if cont then 
    fold_sub_location fold_rec_term fold_rec_location fold_rec_location_set
      ft fl fls acc loc 
  else acc

let iter_location ft fl fls loc =
  fold_location (fold_unit ft) (fold_unit fl) (fold_unit fls) () loc
  
let fold_sub_behavior itt ita itl itls ft fa fl fls acc b =
  let ita = ita ft fa and itl = itl ft fl fls in
  let acc = Option_misc.fold_left ita acc b.jc_behavior_assumes in
  let acc =
    Option_misc.fold_left
      (fun acc (_,locs) -> List.fold_left itl acc locs)
      acc b.jc_behavior_assigns
  in
  let acc = ita acc b.jc_behavior_ensures in
  ita acc b.jc_behavior_free_ensures

let rec fold_behavior ft fa fl fls acc e =
  fold_sub_behavior 
    fold_term fold_term_and_assertion fold_location fold_location_set
    ft fa fl fls acc e

let rec fold_rec_behavior ft fa fl fls acc e =
  fold_sub_behavior
    fold_rec_term fold_rec_term_and_assertion fold_rec_location
    fold_rec_location_set
    ft fa fl fls acc e 

let iter_behavior ft fa fl fls b =
  fold_behavior (fold_unit ft) (fold_unit fa) (fold_unit fl) (fold_unit fls) 
    () b

let fold_sub_funspec itb itt ita itl itls ft fa fl fls acc spec =
  let ita = ita ft fa and itb = itb ft fa fl fls in
  let acc = ita acc spec.jc_fun_requires in
  let acc = ita acc spec.jc_fun_free_requires in
  List.fold_left
    (fun acc (_pos,_id,behav) -> itb acc behav)
    acc (spec.jc_fun_default_behavior :: spec.jc_fun_behavior)

let fold_funspec ft fa fl fls acc spec =
  fold_sub_funspec 
    fold_behavior fold_term fold_term_and_assertion fold_location 
    fold_location_set ft fa fl fls acc spec

let rec fold_rec_funspec ft fa fl fls acc spec =
  fold_sub_funspec
    fold_rec_behavior fold_rec_term fold_rec_term_and_assertion 
    fold_rec_location fold_rec_location_set ft fa fl fls acc spec

let iter_funspec ft fa fl fls spec =
  fold_funspec (fold_unit ft) (fold_unit fa) (fold_unit fl) (fold_unit fls) 
    () spec

let rec map_assertion f a =
  let anode = match a#node with
    | JCAtrue | JCAfalse | JCArelation _ | JCAapp _ | JCAeqtype _ 
    | JCAinstanceof _ | JCAbool_term _ | JCAmutable _ 
    | JCAsubtype _ as anode -> 
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

let map_term_in_tag f tag = 
  let tag_node = match tag#node with
    | JCTtag _ | JCTbottom as tag_node -> tag_node
    | JCTtypeof(t,st) -> 
	JCTtypeof(map_term f t,st)
  in
  new tag_with ~node:tag_node tag

let rec map_term_in_assertion f a =
  let anode = match a#node with
    | JCAtrue | JCAfalse as anode -> anode
    | JCAeqtype(tag1,tag2,st) ->
	JCAeqtype(map_term_in_tag f tag1,map_term_in_tag f tag2,st)
    | JCAsubtype(tag1,tag2,st) ->
	JCAsubtype(map_term_in_tag f tag1,map_term_in_tag f tag2,st)
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

let rec map_term_and_assertion fa ft a =
  let anode = match a#node with
    | JCAtrue | JCAfalse as anode -> anode
    | JCAeqtype(tag1,tag2,st) ->
	JCAeqtype(map_term_in_tag ft tag1,map_term_in_tag ft tag2,st)
    | JCAsubtype(tag1,tag2,st) ->
	JCAsubtype(map_term_in_tag ft tag1,map_term_in_tag ft tag2,st)
    | JCArelation(t1,op,t2) -> 
	JCArelation(map_term ft t1,op,map_term ft t2)
    | JCAapp app ->
	JCAapp { app with jc_app_args = List.map (map_term ft) app.jc_app_args }
    | JCAinstanceof(t1,lab,st) ->
	JCAinstanceof(map_term ft t1,lab,st)
    | JCAbool_term t1 ->
	JCAbool_term(map_term ft t1)
    | JCAmutable(t1,st,tag) ->
	JCAmutable(map_term ft t1,st,tag)
    | JCAand al ->
	JCAand(List.map (map_term_and_assertion fa ft) al)
    | JCAor al ->
	JCAor(List.map (map_term_and_assertion fa ft) al)
    | JCAimplies(a1,a2) ->
	JCAimplies
	  (map_term_and_assertion fa ft a1,map_term_and_assertion fa ft a2)
    | JCAiff(a1,a2) ->
	JCAiff
	  (map_term_and_assertion fa ft a1,map_term_and_assertion fa ft a2)
    | JCAif(t1,a1,a2) ->
	JCAif(
	  map_term ft t1,
	  map_term_and_assertion fa ft a1,
	  map_term_and_assertion fa ft a2)
    | JCAnot a1 ->
	JCAnot(map_term_and_assertion fa ft a1)
    | JCAquantifier(q,vi,a1) ->
	JCAquantifier(q,vi,map_term_and_assertion fa ft a1)
    | JCAold a1 ->
	JCAold(map_term_and_assertion fa ft a1)
    | JCAat(a1,lab) ->
	JCAat(map_term_and_assertion fa ft a1,lab)
    | JCAmatch(t, pal) ->
	JCAmatch(map_term ft t,
		 List.map (fun (p, a) -> p, map_term_and_assertion fa ft a) pal)
  in
  fa (new assertion_with ~node:anode a)

(*****************************************************************************)
(* Specific iterators on terms.                                              *)
(*****************************************************************************)

let raw_sub_term subt t =
  fold_term (fun acc t -> acc || TermOrd.equal subt t) false t

let raw_sub_term_in_assertion subt a =
  fold_term_in_assertion (fun acc t -> acc || TermOrd.equal subt t) false a

let raw_strict_sub_term subt t =
  TermOrd.compare subt t <> 0 && raw_sub_term subt t


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
    | JCEcontract(req,dec,vi_result,behs,_e) ->
	let e1 = as1 el in JCEcontract(req,dec,vi_result,behs,e1)
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
    | JCEaddress(absolute,_e) ->
	let e1 = as1 el in JCEaddress(absolute,e1)
    | JCEbase_block(_e) ->
	let e1 = as1 el in JCEbase_block(e1)
    | JCEinstanceof(_e,st) ->
	let e1 = as1 el in JCEinstanceof(e1,st)
    | JCEcast(_e,st) ->
	let e1 = as1 el in JCEcast(e1,st)
    | JCEbitwise_cast(_e,st) ->
	let e1 = as1 el in JCEbitwise_cast(e1,st)
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
      | JCEbitwise_cast(e, _)
      | JCErange_cast(e, _)
      | JCEreal_cast(e, _)
      | JCEoffset(_, e, _)
      | JCEaddress(_,e)
      | JCEbase_block(e)
      | JCEalloc(e, _)
      | JCEfree e
      | JCElet(_, None, e)
      | JCEloop(_, e)
      | JCEreturn(_, e)
      | JCEthrow(_, Some e)
      | JCEpack(_, e, _)
      | JCEunpack(_, e, _) 
      | JCEcontract(_,_,_,_,e) -> [e]
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

let fold_sub_expr_and_term_and_assertion 
    itt ita itl itls ite ft fa fl fls fe acc e =
  let fold_sub_behavior = fold_sub_behavior itt ita itl itls ft fa fl fls in
  let ite = ite ft fa fl fls fe and ita = ita ft fa and itt = itt ft in
  match e#node with
    | JCEconst _ 
    | JCEvar _ 
    | JCEreturn_void ->
       acc
    | JCEthrow(_exc,e1_opt) ->
	Option_misc.fold_left ite acc e1_opt
    | JCEbinary(e1,_,e2) 
    | JCEshift(e1,e2)
    | JCEassign_heap(e1,_,e2) ->
	let acc = ite acc e1 in
	ite acc e2
    | JCEunary(_,e1)
    | JCEderef(e1,_)
    | JCEoffset(_,e1,_)
    | JCEaddress(_,e1)
    | JCEbase_block(e1)
    | JCEcast(e1,_)
    | JCEbitwise_cast(e1,_) 
    | JCErange_cast(e1,_) 
    | JCEinstanceof(e1,_) 
    | JCEreal_cast(e1,_)
    | JCEunpack(_,e1,_)
    | JCEpack(_,e1,_)
    | JCEassign_var(_,e1) 
    | JCEalloc(e1,_) 
    | JCEfree e1 
    | JCEreturn(_,e1) ->
	ite acc e1
    | JCElet(_,e1_opt,e2) ->
	let acc = Option_misc.fold_left ite acc e1_opt in
	ite acc e2
    | JCEapp call ->
	List.fold_left ite acc call.jc_call_args
    | JCEif(e1,e2,e3) ->
	let acc = ite acc e1 in
	let acc = ite acc e2 in
	ite acc e3
    | JCEmatch(e, ptl) ->
	let acc = ite acc e in
	List.fold_left (fun acc (_, e) -> ite acc e) acc ptl
    | JCEblock el ->
	List.fold_left ite acc el
    | JCEtry(e1,catches,finally) ->
	let acc = ite acc e1 in
	let acc = 
	  List.fold_left (fun acc (_exc,_vi_opt,e) -> ite acc e) acc catches
	in
	ite acc finally
    | JCEassert(_behav,_asrt,a) ->
	ita acc a
    | JCEloop(la,e1) ->
	let acc = ite acc e1 in
	let acc = 
	  List.fold_left 
	    (fun acc (_behav,a) -> ita acc a) acc la.jc_loop_invariant
	in
	let acc = ita acc la.jc_free_loop_invariant in
	Option_misc.fold_left itt acc la.jc_loop_variant
    | JCEcontract(a_opt,t_opt,_v,behavs,e) ->
	let acc = Option_misc.fold_left ita acc a_opt in
	let acc = Option_misc.fold_left itt acc t_opt in
	let acc = 
	  List.fold_left 
	    (fun acc (_loc,_name,behav) ->
	       fold_sub_behavior acc behav
	    ) acc behavs
	in
	ite acc e

let rec fold_expr_and_term_and_assertion ft fa fl fls fe acc e =
  let acc = fe acc e in
  fold_sub_expr_and_term_and_assertion 
    fold_term fold_term_and_assertion fold_location fold_location_set
    fold_expr_and_term_and_assertion 
    ft fa fl fls fe acc e

let rec fold_rec_expr_and_term_and_assertion ft fa fl fls fe acc e =
  let cont,acc = fe acc e in
  if cont then 
    fold_sub_expr_and_term_and_assertion
      fold_rec_term fold_rec_term_and_assertion fold_rec_location
      fold_rec_location_set fold_rec_expr_and_term_and_assertion 
      ft fa fl fls fe acc e 
  else acc

let iter_expr_and_term_and_assertion ft fa fl fls fe e =
  fold_expr_and_term_and_assertion (fold_unit ft) (fold_unit fa) 
    (fold_unit fl) (fold_unit fls) (fold_unit fe) () e

module NExprAst = struct
  type t = nexpr
  let subtags tag = 
    match tag#node with
      | JCPTtag _ | JCPTbottom -> []
      | JCPTtypeof e -> [e]
  let subs e =
    match e#node with
      | JCNEconst _
      | JCNEvar _
      | JCNEreturn None
      | JCNEthrow(_, None)
      | JCNErange(None, None) ->
          []
      | JCNEeqtype(tag1,tag2) | JCNEsubtype(tag1,tag2) ->
	  subtags tag1 @ subtags tag2
      | JCNElabel(_, e)
      | JCNEderef(e, _)
      | JCNEunary(_, e)
      | JCNEinstanceof(e, _)
      | JCNEcast(e, _)
      | JCNEoffset(_, e)
      | JCNEaddress(_,e)
      | JCNEbase_block(e)
      | JCNEalloc(e, _)
      | JCNEfree e
      | JCNElet(_, _, None, e)
      | JCNEassert(_,_,e)
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
      | JCNErange(Some e1, Some e2) ->
          [e1; e2]
      | JCNEcontract(_,_,_,e) -> 
	  [e]
      | JCNEif(e1, e2, e3) ->
          [e1; e2; e3]
      | JCNEloop(inv, None, e2) ->
	  let e1list = List.map (fun (_behav,e) -> e) inv in
          e1list @ [e2]
      | JCNEloop(inv, Some e2, e3) ->
	  let e1list = List.map (fun (_behav,e) -> e) inv in
          e1list @ [e2; e3]
      | JCNEapp(_, _, el)
      | JCNEblock el ->
          el
      | JCNEmatch(e, pel) ->
          e :: List.map snd pel
      | JCNEtry(e1, l, e2) ->
          e1 :: List.map (fun (_, _, e) -> e) l @ [e2]
end

module INExpr = Iterators(NExprAst)

(*
Local Variables: 
compile-command: "LC_ALL=C make -C .. bin/jessie.byte"
End: 
*)
