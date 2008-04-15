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

(*i $Id: wp.ml,v 1.115 2008/11/05 14:03:18 filliatr Exp $ i*)

(*s Weakest preconditions *)

open Ident
open Error
open Logic
open Misc
open Types
open Ast
open Util
open Env
open Effect
open Options

(*s to quantify over all the variables read/modified by [p] *)

let input_info info = 
  let w = Effect.get_reads info.t_effect in
  let env = info.t_env in
  List.map (fun id -> (id, type_in_env env id)) w

let input p = input_info p.info

let output_info info = 
  let w = Effect.get_writes info.t_effect in
  let env = info.t_env in
  List.map (fun id -> (id, type_in_env env id)) w

let output p = output_info p.info

(* maximum *)

let sup q q' = match q, q' with
  | None, _ ->
      q'
  | _, None ->
      q
  | Some (q, ql), Some (_, ql') ->
      assert (List.length ql = List.length ql');
      let supx (x,a) (x',a') =
	assert (x = x');
	x, if is_default_post a then a' else a
      in
      Some (q, List.map2 supx ql ql') 

(*s [filter_post k q] removes exc. postconditions from [q] which do not
    appear in typing info [k] *)

let filter_post k =
  let ef = k.t_effect in
  let keep (x,_) = is_exn ef x in
  option_app (fun (q, ql) -> (q, List.filter keep ql))




(*s post-condition for a loop body *)

let default_exns_post e =
  let xs = Effect.get_exns e in
  List.map (fun x -> x, default_post) xs
 
let while_post_block env inv var e = 
  let lab = e.info.t_label in
  let decphi = match var with
    | None -> Ptrue
    | Some (loc,phi,_,r) -> 
	if debug then 
	  Format.eprintf "Loc for variant = %a@." Loc.gen_report_position loc;
	let id = Util.reg_explanation (Cc.VCEvardecr(loc,phi)) in
	Pnamed(id, Papp (r, [phi; put_label_term env lab phi], [])) 
  in
  let ql = default_exns_post (effect e) in
  match inv with
    | None -> 
	anonymous e.info.t_loc decphi, ql
    | Some i -> 
	if debug then 
	  Format.eprintf 
	    "Loc for invariant = %a@." Loc.gen_report_position i.a_loc;
	let id = reg_explanation (Cc.VCEinvpreserv("",(i.a_loc,i.a_value))) in
	(* Claude: pand -> wpand to split the conjunct *)
	{ a_value = wpand (Pnamed(id,i.a_value)) decphi; 
	  a_name = post_name_from (Name i.a_name);
	  a_loc = e.info.t_loc;
	  a_proof = None }, ql

let well_founded_rel = function
  | None -> Ptrue
  | Some (_,_,_,r) -> Papp (well_founded, [Tvar r], [])

(*s [saturate_post k a q] makes a postcondition for a program of type [k]
    out of a normal postcondition [a] and the exc. postconditions from [q] *)

let saturate_post k a q = 
  let ql = match q with Some (_,l) -> l | None -> [] in
  let set_post x = x, try List.assoc x ql with Not_found -> default_post in
  let xs = get_exns k.t_effect in
  let saturate a = (a, List.map set_post xs) in
  option_app saturate a

(*s Misc. test functions *)

let need_a_post p = 
  match p.desc with Lam _ | Rec _ -> false | _ -> true

let is_while p = 
  match p.desc with Loop _ -> true | _ -> false

(*s Weakest precondition of an annotated program:
    \begin{verbatim}
    wp(e{Q'}, Q) = forall y,v. Q' => Q
    \end{verbatim}
    When [e] may raise exceptions:
    \begin{verbatim}
    wp(e{Q'|Q'1|...Q'k}, Q|Q1|...Qk) =
    (forall y,v. Q' => Q) and ... and (forall y,x. Q'k => Qk)
    \end{verbatim} *)


let abstract_wp loc (q',ql') (q,ql) res out =
  assert (List.length ql' = List.length ql);
  let quantify a' a res =
    let vars = match res with Some b -> b :: out | None -> out in
    let a' = (*loc_name loc*) a'.a_value in
    foralls ~is_wp:true vars (Pimplies (true, a', a.a_value)) 
  in
  let quantify_h (x',a') (x,a) =
    assert (x' = x);
    let pt = find_exception x in
    quantify a' a (option_app (fun t -> (result, PureType t)) pt)
  in
  wpands (quantify q' q (Some res) :: List.map2 quantify_h ql' ql)

(* simple version when [q'] is absent *)
let abstract_wp_0 (q,ql) res out =
  let quantify a res =
    let vars = match res with Some b -> b :: out | None -> out in
    foralls ~is_wp:true vars a.a_value
  in
  let quantify_h (x,a) =
    let pt = find_exception x in
    quantify a (option_app (fun t -> (result, PureType t)) pt)
  in
  wpands (quantify q (Some res) :: List.map quantify_h ql)

let opaque_wp q' q info = match q', q with
  | Some q', Some q ->
      let res = (result, info.t_result_type) in
      let p = abstract_wp info.t_loc q' q res (output_info info) in
      let p = erase_label info.t_label (erase_label "" p) in
      Some (wp_named info.t_loc p)
  | None, Some q ->
      let res = (result, info.t_result_type) in
      let p = abstract_wp_0 q res (output_info info) in
      let p = erase_label info.t_label (erase_label "" p) in
      Some (wp_named info.t_loc p)
  | _ ->
      None

let pand_wp info w1 w2 = match w1, w2 with
  | Some w1, Some w2 -> 
      Some (wp_named info.t_loc (wpand w1.a_value w2.a_value))
  | Some _, None -> w1
  | None, Some _ -> w2
  | None, None -> None

(* explanations *)

let explain_assertion e a = 
  let id = reg_explanation e in 
  { a with a_value = Pnamed(id, a.a_value) }

let explain_wp e = Option_misc.map (explain_assertion e)

(* subtyping: a VC expressing that a specification is stronger than another *)

(*** EN COURS NE PAS EFFACER SVP
let rec subtyping env v1 v2 = match v1, v2 with
  | PureType _, PureType _
  | Ref _, Ref _ -> 
      Ptrue
  | Arrow ((x1,t1) :: bl1, k1), Arrow ((x2,t2) :: bl2, k2) ->
      let p = subtyping env (Arrow (bl1,k1)) (Arrow (bl2,k2)) in
      let p = subst_in_predicate (subst_onev x2 x1) p in
      if Typing.is_pure_type t1 then pforall ~is_wp:true x1 t1 p else p
  | Arrow ([], k1), Arrow ([], k2) ->
      subtyping_k env k1 k2
  | Arrow _, Arrow ([], k2) ->
      subtyping_k env (type_c_of_v v1) k2
  | Arrow ([], k1), Arrow _ ->
      subtyping_k env k1 (type_c_of_v v2)
  | _ ->
      assert false

and subtyping_k env k1 k2 =
  let p = wpimplies (wpands k2.c_pre) (wpands k1.c_pre) in
  let res = k1.c_result_name, k1.c_result_type in
  let out = 
    List.map (fun id -> (id, type_in_env env id)) (get_writes k1.c_effect)
  in
  let q = match k1.c_post, k2.c_post with
    | _, None -> Ptrue
    | None, Some q2 -> abstract_wp_0 q2 res out 
    | Some q1, Some q2 -> abstract_wp Loc.dummy q1 q2 res out
  in
  pforall (input k1.c_effect) (wpand p q)
***)

(*s function binders *)

let abstract_binders =
  List.fold_right
    (fun (x,v) p -> match v with
       | PureType _ -> forall ~is_wp:true x v p
       | _ -> p)

(*s Adding precondition and obligations to the wp *)

let add_to_wp loc explain_id al w =
  let al = List.map (fun p -> (*loc_name loc*) p.a_value) al in
  if al = [] then
    w
  else match w with
    | Some w -> 
	Some (asst_app 
		(fun w -> 
		   Pnamed(explain_id,
			  List.fold_right (wpand ~is_sym:false) al w))
		w)
    | None -> 
	Some (wp_named loc (Pnamed(explain_id,wpands ~is_sym:false al)))

(*s WP. [wp p q] computes the weakest precondition [wp(p,q)]
    and gives postcondition [q] to [p] if necessary.

    [wp: typed_program -> postcondition option -> assertion option] *)

let rec wp p q =
  (* Format.eprintf "wp with size(q) = %d@." (Size.postcondition_opt q); *)
  let q = option_app (force_post_loc p.info.t_loc) q in
  let lab = p.info.t_label in
  let q0_ = optpost_app (asst_app (change_label "" lab)) q in
  let d,w = wp_desc p.info p.desc q0_ in
  let p = change_desc p d in
  let w = option_app (asst_app (erase_label lab)) w in
  p, w

and wp_desc info d q = 
  let result = info.t_result_name in
  match d with
    (* TODO: check if likely *)
    | Var x ->
	let w = optpost_val q in
	d, optasst_app (tsubst_in_predicate (subst_one result (Tvar x))) w
    (* $wp(E,q) = q[result \leftarrow E]$ *)
    | Expression t ->
	let w = optpost_val q in
	let t = unref_term t in
	let s = subst_one result t in
	d, optasst_app (fun p -> simplify (tsubst_in_predicate s p)) w
    | If (p1, p2, p3) ->
	let p'2,w2 = wp p2 (filter_post p2.info q) in
	let p'3,w3 = wp p3 (filter_post p3.info q) in
	let q1 = match w2, w3 with
	  | None, None ->
	      None
	  | _ -> 
	      let p_or_true = function Some {a_value=q} -> q | None -> Ptrue in
	      let q2 = p_or_true w2 in
	      let q3 = p_or_true w3 in
	      (* $wp(if p1 then p2 else p3, q) =$ *)
	      (* $wp(p1, if result then wp(p2, q) else wp(p3, q))$ *)
	      let result1 = p1.info.t_result_name in
	      let q1 = create_postval (Pif (Tvar result1, q2, q3)) in
	      let q1 = force_wp_name q1 in
	      saturate_post p1.info q1 q
	in
	let p'1,w1 = wp p1 q1 in
	If (p'1, p'2, p'3), w1
    | AppTerm (p1, t, k) ->
	let p'1,w1 = wp p1 None in
	let wapp = opaque_wp k.t_post q k in
	let w = pand_wp info w1 wapp in
	AppTerm (p'1, t, k), w
    | AppRef (p1, x, k) ->
	let p'1,w1 = wp p1 None in
	let wapp = opaque_wp k.t_post q k in
	let w = pand_wp info w1 wapp in
	AppRef (p'1, x, k), w
    | Lam (bl, p, e) ->
	let e',w = wp_prog (p, e) None in
	let wf = optasst_app (abstract_binders bl) w in
	Lam (bl, p, e'), pand_wp info (optpost_val q) wf
    | LetIn (x, _, _) | LetRef (x, _, _) when occur_post x q ->
        Report.raise_located info.t_loc
	  (AnyMessage ("cannot compute wp due to capture variable;\n" ^
                       "please rename variable " ^ Ident.string x))
    | LetIn (x, e1, e2) ->
	let e'2, w2 = wp e2 (filter_post e2.info q) in
	let q1 = optasst_app (subst_in_predicate (subst_onev x result)) w2 in
	let q1 = saturate_post e1.info q1 q in
	let e'1,w = wp e1 q1 in
	LetIn (x, e'1, e'2), w
    | Seq (e1, e2) -> 
	let e'2, w2 = wp e2 (filter_post e2.info q) in
	let q1 = saturate_post e1.info w2 q in
	let e'1,w = wp e1 q1 in
	Seq (e'1, e'2), w
    | LetRef (x, e1, e2) ->
	(* same as LetIn: correct? *)
	let e'2, w2 = wp e2 (filter_post e2.info q) in
	let q1 = optasst_app (subst_in_predicate (subst_onev x result)) w2 in
	let q1 = saturate_post e1.info q1 q in
	let e'1,w = wp e1 q1 in
	LetRef (x, e'1, e'2), w
    | Rec (f, bl, v, var, p, e) ->
	(* wp = well_founded(R) /\ forall bl. wp(e, True) *)
	let wfr = well_founded_rel var in
	let e',we = wp_prog (p, e) None in
	let w = match we with
	  | None -> wfr
	  | Some {a_value=we} -> wpand wfr (abstract_binders bl we)
	in
	let w = Some (wp_named info.t_loc w) in
	Rec (f, bl, v, var, p, e'), pand_wp info (optpost_val q) w
    | Loop (inv, var, e) ->
        (* wp = well_founded(R) /\ I /\ forall w. I => wp(e, I/\var<var@) *)
	let id = reg_explanation Cc.VCEwfrel in
	let wfr = Pnamed(id, well_founded_rel var) in
	let qe = while_post_block info.t_env inv var e in
	let qe = sup (Some qe) q in (* exc. posts taken from [q] *)
	let e',we = wp e qe in
	let lab = info.t_userlabel in
	let w = match inv, we with
	  | None, None ->
	      wfr
	  | Some i, None ->
	      let id = reg_explanation (Cc.VCEinvinit(lab,(i.a_loc,i.a_value)))
	      in wpand wfr (Pnamed(id,i.a_value))
	  | None, Some {a_value=we} ->
	      let vars = output_info info in
	      wpand wfr (foralls ~is_wp:true vars we)
	  | Some i, Some {a_value=we} ->
	      let vars = output_info info in
	      let id = reg_explanation (Cc.VCEinvinit(lab,(i.a_loc,i.a_value))) 
	      in wpand wfr
		(wpand ~is_sym:true
		   (Pnamed(id,i.a_value))
		   (foralls ~is_wp:true vars (Pimplies (true, i.a_value, we))))
	in
	let w = Some (wp_named info.t_loc w) in
	Loop (inv, var, e'), w
    | Raise (id, None) ->
	(* $wp(raise E, _, R) = R$ *)
	d, option_app (fun (_,ql) -> List.assoc id ql) q
    | Raise (id, Some e) ->
        (* $wp(raise (E e), _, R) = wp(e, R, R)$ *)
	let make_post (_,ql) = let r = List.assoc id ql in (r, ql) in
	let qe = filter_post e.info (option_app make_post q) in
	let e',w = wp e qe in
	Raise (id, Some e'), w

    | Try (e, hl) ->
        (* $wp(try e1 with E -> e2, Q, R) = wp(e1, Q, wp(e2, Q, R))$ *)
	let subst w = function
	  | None -> w
	  | Some x -> optasst_app (subst_in_predicate (subst_onev x result)) w
	in
	let hl' = 
	  List.map (fun ((x,v) as p, h) -> 
		      let h',w = wp h (filter_post h.info q) in
		      (p,h'), (x, subst w v)) hl 
	in
	let hl',hwl = List.split hl' in
	let make_post (q,ql) = 
	  let hpost (x,r) =
	    x, 
	    try (match List.assoc x hwl with None -> r | Some w -> w)
	    with Not_found -> r
	  in
	  (q, List.map hpost ql)
	in
	let q = saturate_post e.info (option_app fst q) q in
	let qe = filter_post e.info (option_app make_post q) in
	let e',w = wp e qe in
	Try (e', hl'), w
    | Assertion (k, l, e) ->
	let e',w = wp e q in
	let expl =  
	  match k with
	    | `ABSURD ->
		Cc.VCEabsurd
	    | `ASSERT ->
		Cc.VCEassert (List.map (fun a -> (a.a_loc,a.a_value)) l) 
	    | `PRE ->
		let lab = info.t_userlabel in
		let loc = info.t_loc in
		if debug then 
		  Format.eprintf 
		    "wp: `PRE explanation: lab=`%s', loc=%a@." lab
		    Loc.gen_report_position loc;
		Cc.VCEpre (lab,loc,List.map (fun a -> (a.a_loc,a.a_value)) l) 
	in
	let id = reg_explanation expl in
	Assertion (k, l, e'), add_to_wp info.t_loc id l w
    | Absurd ->
	Absurd, Some (anonymous info.t_loc Pfalse)
    | Any k as d ->
	let q' = optpost_app (post_named info.t_loc) k.c_post in
	let w = opaque_wp q' q info in
	let pre = List.map (pre_named info.t_loc) k.c_pre in
	let l = List.map (fun a -> (a.a_loc,a.a_value)) pre in
	let lab = info.t_userlabel in
	let loc = info.t_loc in
	let id = reg_explanation (Cc.VCEpre(lab,loc,l)) in
	let w = add_to_wp info.t_loc id pre w in
	d, w
    | Post (e, qe, Transparent) ->
	let lab = e.info.t_label in
	let qe' = Typing.conj (Some qe) q in
	let qe' = 
	  optpost_app (fun a -> 
			 explain_assertion
			   (Cc.VCEpost(a.a_loc,a.a_value))
			   (asst_app (change_label "" lab) a))
	    qe' 
	in
	let e',w = wp e qe' in
	Post (e', qe, Transparent), w
    | Post (e, qe, Opaque) ->
	let lab = e.info.t_label in
	let qe' = Some (post_app (asst_app (change_label "" lab)) qe) in
	let e',we = wp e qe' in
	let w' = opaque_wp qe' q e.info in
	let w = pand_wp e.info we w' in
	Post (e', qe, Opaque), w
    | Label (l, e) ->
	let e,w = wp e q in
	Label (l, e), optasst_app (erase_label l) w

and wp_prog (l,e) q =
  let loc = e.info.t_loc in
  let e,w = wp e q in
  let abstract {a_value=w} =
    let w = 
      List.fold_left (fun w p -> pimplies ~is_wp:true p.a_value w) w l 
    in
    wp_named loc (foralls ~is_wp:true (input e) w)
  in
  e, option_app abstract w

let wp p = wp p None
