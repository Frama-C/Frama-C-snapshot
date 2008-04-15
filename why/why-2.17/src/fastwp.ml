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

(*i $Id: fastwp.ml,v 1.31 2008/11/05 14:03:17 filliatr Exp $ i*)

(*s Fast weakest preconditions *)

open Ident
open Logic
open Types
open Effect
open Misc
open Util
open Ast
open Env

let idmap_union m1 m2 =
  Idmap.fold 
    (fun x2 v m1 -> 
      if Idmap.mem x2 m1 
      then begin assert (Idmap.find x2 m1 = v); m1 end
      else Idmap.add x2 v m1)
    m2 m1

module Subst = struct

  type t = { 
    current : Ident.t Idmap.t; (* current name for each variable *)
    sigma : Ident.t Idmap.t;   (* substitution for all variables *)
    types : pure_type Idmap.t; (* types, for quantifiers *)
    all_vars : Idset.t;        (* all names already used *)
  }

  let empty = 
    { current = Idmap.empty;
      sigma = Idmap.empty; 
      types = Idmap.empty; 
      all_vars = Idset.empty }

  let add x pt s = 
    { current = Idmap.add x x s.sigma;
      sigma = Idmap.add x x s.sigma;
      types = Idmap.add x pt s.types;
      all_vars = Idset.add x s.all_vars }

  let add_aux x pt s = 
    { s with
	types = Idmap.add x pt s.types;
	all_vars = Idset.add x s.all_vars }

  let frame env ef s =
    let r,w,_,_ = Effect.get_repr ef in
    List.fold_left 
      (fun s x -> 
	 try 
	   begin match Env.type_in_env env x with
	     | Ref pt -> add x pt s
	     | _ -> assert false end
	 with Not_found -> assert false) 
      s (r @ w)

  let find x s = Idmap.find x s.current

  let global_names = ref Idset.empty

  let next_away x s =
    let x' = next_away x (Idset.union !global_names s) in
    global_names := Idset.add x' !global_names;
    x'

  let fresh x s =
    assert (Idmap.mem x s.types);
    let x' = next_away x s.all_vars in
    x',
    { current = Idmap.add x x' s.current; 
      sigma = Idmap.add x x' s.sigma; 
      types = Idmap.add x' (Idmap.find x s.types) s.types;
      all_vars = Idset.add x' s.all_vars }

  let fresh_pure x pt s =
    let x' = next_away x s.all_vars in
    x',
    { s with 
      types = Idmap.add x' pt s.types;
      all_vars = Idset.add x' s.all_vars }

  let write x s = let _,s = fresh x s in s
  let writes = List.fold_right write

  let term s = Misc.subst_in_term s.sigma
  let predicate s = Misc.subst_in_predicate s.sigma

  (* we cross the label l => 
     the values at label l are mapped to the current values of references *)
  let label l s =
    { s with sigma =
	Idmap.fold 
	  (fun x x' m -> 
	     if not (is_at x) then Idmap.add (at_id x l) x' m else m)
	  s.current s.sigma }

  let add_vars s1 s2 =
    { s1 with 
	types = idmap_union s1.types s2.types;
	all_vars = Idset.union s1.all_vars s2.all_vars } 

  (* debug *)
  open Format
  let print fmt s =
    let print_map fmt m = 
      Idmap.iter 
	(fun x x' -> fprintf fmt "(%a->%a)" Ident.lprint x Ident.lprint x') m
    in
    let print_keys fmt m =
      Idmap.iter 
	(fun x _ -> fprintf fmt "(%a)" Ident.lprint x) m
    in
    fprintf fmt "@[<hov 2>current=%a,@ sigma=%a,@ types=%a@]" 
      print_map s.current print_map s.sigma print_keys s.types

end
open Subst

let all_quantifiers ((_,s),ee) =
  let s =
    List.fold_left (fun s (_,(_,sx)) -> idmap_union s sx.types) s.types ee 
  in
  let l = Idmap.fold (fun x pt acc -> (x, PureType pt) :: acc) s [] in
  List.rev l

let merge s1 s2 =
  (* d = { x | s1(x) <> s2(x) } *)
  let d = 
    Idmap.fold 
      (fun x x1 d ->
	 try 
	   let x2 = Subst.find x s2 in if x1 != x2 then Idset.add x d else d
	 with Not_found -> 
	   d)
      s1.current Idset.empty
  in
  let s12 = 
    { s1 with 
      types = idmap_union s1.types s2.types;
      all_vars = Idset.union s1.all_vars s2.all_vars } 
  in
  Idset.fold 
    (fun x (s',r1,r2) -> 
       let x',s' = Subst.fresh x s' in
       let ty = PureType (Idmap.find x s'.types) in
       s', 
       wpand r1 (tequality ty (Tvar x') (Tvar (Subst.find x s1))),
       wpand r2 (tequality ty (Tvar x') (Tvar (Subst.find x s2))))
    d (s12, Ptrue, Ptrue)

let wpforall = pforall ~is_wp:true
let wpforalls = foralls_many ~is_wp:true

let ssubst_in_predicate s p = simplify (tsubst_in_predicate s p)

let norm (p,_) = p
let exn x pl s = try List.assoc x pl with Not_found -> Pfalse, s
let exns e ee = List.map (fun x -> x, ee x) (get_exns e.info.t_effect)

let with_exception_type e v f x = match find_exception e, v with
  | None, None -> x
  | Some pt, Some v -> f v pt x
  | _ -> assert false

(* INPUT
   - e : program
   - s : Subst.t
   OUTPUT
   - ok : predicate = correctness of e
   - (n,el) : predicate * (Ident.t * predicate) list, such that
     * if e terminates normally then n holds
     * if e raises exception E then List.assoc E el holds
   - s' : Subst.t
*)

let rec wp e s = 
  let _,(_,ee) as r = wp0 e s in
  assert (List.length ee = List.length (get_exns e.info.t_effect));
  r

and wp0 e s =
  (*Format.eprintf "@[wp avec %a@]@." Subst.print s;*)
  let v = result_type e in
  match e.desc with
  | Expression t ->
      (* OK: true
	 NE: result=t *)
      let t = Subst.term s (unref_term t) in
      let p = match e.info.t_result_type with
	| PureType PTunit -> Ptrue
	| _ -> tequality v tresult t
      in
      Ptrue, ((p, s), [])
  | If (e1, e2, e3) ->
      (* OK: ok(e1) /\ (ne(e1,true) => ok(e2)) /\ (ne(e1,false) => ok(e3))
	 NE: (ne(e1,true) /\ ne(e2,result)) \/ (ne(e1,false) /\ ne(e3,result)) 
      *)
      let ok1,((ne1,s1),ee1) = wp e1 s in
      let ok2,((ne2,s2),ee2) = wp e2 s1 in
      let ok3,((ne3,s3),ee3) = wp e3 s1 in
      let ne1true = ssubst_in_predicate (subst_one result ttrue) ne1 in
      let ne1false = ssubst_in_predicate (subst_one result tfalse) ne1 in
      let ok = wpands [ok1; wpimplies ne1true ok2; wpimplies ne1false ok3] in
      let ne = 
	let s',r2,r3 = merge s2 s3 in
	por (wpands [ne1true; ne2; r2]) (wpands [ne1false; ne3; r3]), s'
      in
      let ee x = 
	let ee2,s2 = exn x ee2 s1 and ee3,s3 = exn x ee3 s1 in
	let s23,r2,r3 = merge s2 s3 in
	let ee1,s1 = exn x ee1 s in
	let s',q1,q23 = merge s1 s23 in
	pors [wpand ee1 q1; 
	      wpands [ne1true;ee2;r2;q23]; 
	      wpands [ne1false;ee3;r3;q23]], s'
      in
      ok, (ne, exns e ee)
  | Seq (e1, e2) ->
      (* OK: ok(e1) /\ (ne(e1,void) => ok(e2))
	 NE: ne(e1,void) /\ ne(e2,result) *)
      let ok1,((ne1,s1),ee1) = wp e1 s in
      let ok2,((ne2,s2),ee2) = wp e2 s1 in
      let ne1void = tsubst_in_predicate (subst_one result tvoid) ne1 in
      let ok = wpand ok1 (wpimplies ne1void ok2) in
      let ne = wpand ne1void ne2 in
      let ee x = 
	let ee1,sx1 = exn x ee1 s and ee2,sx2 = exn x ee2 s1 in
	let s',r1,r2 = merge sx1 sx2 in
	por (wpand ee1 r1) (wpands [ne1void; ee2; r2]), s'
      in
      ok, ((ne, s2), exns e ee)
  | LetIn (x, e1, e2) ->
      let ok1,((ne1,s1),ee1) = wp e1 s in
      let x',s1 = match e1.info.t_result_type with
	| PureType pt -> Subst.fresh_pure x pt s1 
	| _ -> x, s1
      in
      let ok2,((ne2,s2),ee2) = wp e2 s1 in
      begin match e1.info.t_result_type with
	| PureType pt ->
	    let ne1x = subst_in_predicate (subst_onev result x') ne1 in
	    let subst = subst_in_predicate (subst_onev x x') in
	    let ok = wpand ok1 (wpimplies ne1x (subst ok2)) in
	    let ne = wpand ne1x (subst ne2) in
	    let ee x =
	      let ee1,sx1 = exn x ee1 s and ee2,sx2 = exn x ee2 s1 in
	      let s',r1,r2 = merge sx1 sx2 in
	      por (wpand ee1 r1) (wpands [ne1x; ee2; r2]), s'
	    in
	    ok, ((ne, s2), exns e ee)
	| Arrow _ ->
	    assert (not (occur_predicate result ne1));
	    assert (not (occur_predicate x ne2));
	    let ok = wpand ok1 (wpimplies ne1 ok2) in (* ok1 /\ ok2 ? *)
	    let ne = wpand ne1 ne2 in
	    let ee x =
	      let ee1,sx1 = exn x ee1 s and ee2,sx2 = exn x ee2 s1 in
	      let s',r1,r2 = merge sx1 sx2 in
	      por (wpand ee1 r1) (wpands [ne1; ee2; r2]), s'
	    in
	    ok, ((ne, s2), exns e ee)
	| Ref _ -> 
	    assert false
      end
  | LetRef (x, e1, e2) ->
      begin match e1.info.t_result_type with
	| PureType pt ->
	    let ok1,((ne1,s1),ee1) = wp e1 s in
	    let s1 = Subst.add x pt s1 in
	    let ok2,((ne2,s2),ee2) = wp e2 s1 in
	    let ne1x = subst_in_predicate (subst_onev result x) ne1 in
	    let ok = wpand ok1 ((*wpforall x ty1*) (wpimplies ne1x ok2)) in
	    let ne = (*exists x ty1*) (wpand ne1x ne2) in
	    let ee x =
	      let ee1,sx1 = exn x ee1 s and ee2,sx2 = exn x ee2 s1 in
	      let s',r1,r2 = merge sx1 sx2 in
	      por (wpand ee1 r1) (wpands [ne1x; ee2; r2]), s'
	    in
	    let s2 = Subst.add_aux x pt s2 in
	    ok, ((ne, s2), exns e ee)
	| Arrow _ | Ref _ -> 
	    assert false
      end
  | Assertion (k, al, e1) ->
      (* OK: al /\ ok(e1)
	 NE: al /\ ne(e1,result) *)
      let ok,((ne1,s'),ee1) = wp e1 s in
      let pl = List.map (fun a -> subst_in_predicate s.sigma a.a_value) al in
      let ee x = let ee,sx = exn x ee1 s in wpands (pl @ [ee]), sx in
      wpands (pl@[ok]), ((wpands (pl@[ne1]), s'), exns e ee)
  | Post (e1, q, _) ->
      (* TODO: what to do with the transparency here? *)
      let lab = e1.info.t_label in
      let s = Subst.label lab s in
      let ok,((ne1,s'),ee1) = wp e1 s in
      let (q,ql) = post_app (asst_app (change_label "" lab)) q in
      let subst p s = subst_in_predicate s.sigma p.a_value in
      let q = subst q s' in
      let ql = List.map2 (fun (_,(_,sx)) (x,qx) -> x, subst qx sx) ee1 ql in
      let post_exn (x,(ex,_)) (x',qx) =
	assert (x=x'); 
	let p = wpimplies ex qx in
	match find_exception x with
	  | Some pt -> wpforall result (PureType pt) p
	  | None -> p
      in
      let ok = 
	wpands 
	  (ok :: 
	     wpforall result e1.info.t_result_type (wpimplies ne1 q) ::
	     List.map2 post_exn ee1 ql)
      in
      let ne = wpand ne1 q, s' in
      let ee x = let ee,sx = exn x ee1 s in wpand ee (List.assoc x ql), sx in
      ok, (ne, exns e ee)
  | Label (l, e) ->
      wp e (Subst.label l s)
  | Var _ -> 
      (* this must be an impure function, thus OK = NE = true *)
      Ptrue, ((Ptrue, s), [])
  | Absurd -> 
      (* OK = NE = false *)
      Pfalse, ((Pfalse, s), [])
  | Loop (inv, var, e1) ->
      (* OK: I /\ forall w. (I => (ok(e1) /\ (ne(e1,void) => I /\ var<var@)))
	 N : false
	 E : e(e1) *)
      (* TODO: termination *)
      let s0 = Subst.writes (Effect.get_writes e1.info.t_effect) s in
      let ok1,((ne1,s1),ee1) = wp e1 s0 in
      let ne1void = tsubst_in_predicate (subst_one result tvoid) ne1 in
      let subst_inv s = match inv with
	| None -> Ptrue
	| Some {a_value=i} -> Subst.predicate s i
      in
      let i0 = subst_inv s0 in 
      let decphi = match var with
	| None -> Ptrue
	| Some (loc,phi,_,r) -> 
	    let id = Util.reg_explanation (Cc.VCEvardecr(loc,phi)) in
	    Pnamed(id, Papp (r, [Subst.term s1 phi;Subst.term s0 phi], []))
      in
      let ok = 
	wpands
	  [Wp.well_founded_rel var;
	   subst_inv s;
	   wpimplies i0 
	     (wpand ok1 (wpimplies ne1void (wpand (subst_inv s1) decphi)))]
      in
      let ee x =
	let ee,sx = exn x ee1 s0 in wpand i0 ee, sx
      in
      ok, ((Pfalse, s1), exns e ee)
  | Raise (id, None) -> 
      (* OK: true  
	 N : false  
	 E : true *)
      Ptrue, ((Pfalse, s), [id, (Ptrue, s)])
  | Raise (id, Some e1) -> 
      (* OK: ok(e1)
	 N : false
	 E : ne(e1) \/ E(e1) if E=id, E(e1) otherwise *)
      let ok1,((ne1,s1),ee1) = wp e1 s in
      let ee x = 
	if x == id then
	  try let ee1,sx = List.assoc x ee1 in por ne1 ee1, sx
	  with Not_found -> ne1, s1
	else
	  try List.assoc x ee1 with Not_found -> assert false
      in
      ok1, ((Pfalse, s1), exns e ee)
  | Try (e1, hl) ->
      let ok1,((ne1,s1),ee1) = wp e1 s in
      let hl = 
	List.map 
	  (fun ((x,v),ei) -> let _,sx = exn x ee1 s in ((x,v), wp ei sx))
	  hl 
      in
      let bind_result v p = match v with
	| None -> p
	| Some x -> subst_in_predicate (subst_onev result x) p
      in
      let handler_ok ((x,v), (oki,_)) = 
	let e1x,_ = exn x ee1 s in 
	let e1x = bind_result v e1x in
	let p = wpimplies e1x oki in
	with_exception_type x v (fun v pt -> wpforall v (PureType pt)) p
      in
      let ok = wpands (ok1 :: List.map handler_ok hl) in
      let ne =
	List.fold_left
	  (fun (ne,s) ((x,v), (_,((nei,si),_))) ->
	    let e1x,_ = exn x ee1 s in 
	    let e1x = bind_result v e1x in
	    let si = with_exception_type x v Subst.add_aux si in
	    let s',r1,r2 = merge s si in
	    por (wpand ne r1) (wpands [e1x; nei; r2]), s')
	  (ne1,s1) hl
      in
      let ee x = 
	let eex,sx =
	  if List.exists (fun ((xi,_),_) -> x == xi) hl then
 	    Pfalse, s
	  else
 	    exn x ee1 s
	in
	List.fold_left
	  (fun (nex,sx) ((xi,vi),(_,(_,eei))) ->
	    let e1xi,sxi = exn xi ee1 s in
	    let eeix,sxi = exn x eei sxi in
	    let eeix = bind_result vi eeix in
	    let sxi = with_exception_type xi vi Subst.add_aux sxi in
	    let sx,r1,r2 = merge sx sxi in
	    por (wpand nex r1) (wpands [e1xi; eeix; r2]), sx)
	  (eex, sx) hl
      in
      ok, (ne, exns e ee)
  | Lam (bl, pl, e) ->
      (* OK: forall bl. pl => ok(e)
	 NE: forall bl. pl /\ ne(e, result) *)
      let s = Subst.frame e.info.t_env e.info.t_effect s in
      let ok,r = wp e s in
      let qr = all_quantifiers r in
      let pl = List.map (fun a -> subst_in_predicate s.sigma a.a_value) pl in
      let q = List.filter (function (_,PureType _) -> true | _ -> false) bl in
      wpforalls (q @ qr) (wpimplies (wpands pl) ok),
      ((Ptrue, s), [])
  | Rec (f, bl, v, var, pl, e) ->
      (* OK: well_founded(R) /\ forall bl. pl => ok(e)
	 NE: forall bl. pl /\ ne(e, result) *)
      let wfr = Wp.well_founded_rel var in
      let s = Subst.frame e.info.t_env e.info.t_effect s in
      let ok,r = wp e s in
      let qr = all_quantifiers r in
      let pl = List.map (fun a -> subst_in_predicate s.sigma a.a_value) pl in
      let q = List.filter (function (_,PureType _) -> true | _ -> false) bl in
      pand wfr (wpforalls (q @ qr) (wpimplies (wpands pl) ok)),
      ((Ptrue, s), [])
  | AppRef (e1, _, k) 
  | AppTerm (e1, _, k) ->
      let lab = e1.info.t_label in
      let s = Subst.label lab s in
      let q = optpost_app (asst_app (change_label "" lab)) k.t_post in
      let ok,(((ne,s'),ee) as nee) = wp e1 s in
      assert (not (occur_predicate result ne));
      let wr s = Subst.writes (Effect.get_writes k.t_effect) s in
      let nee = match q with
	| Some (q', qe) -> 
	    (let s' = wr s' in
	     wpand ne (Subst.predicate s' q'.a_value), s'),
	    (let ee x = 
	       let q' = List.assoc x qe in
	       let ee,s' = exn x ee s in
	       let s' = wr s' in
	       por ee (wpand ne (Subst.predicate s' q'.a_value)), s'
	     in
	     exns e ee)
	| None -> 
	    nee
      in
      ok, nee
  | Any k ->
      let lab = e.info.t_label in
      let s = Subst.label lab s in
      let q = optpost_app (post_named e.info.t_loc) k.c_post in
      let q = optpost_app (asst_app (change_label "" lab)) q in
      let s' = Subst.writes (Effect.get_writes k.c_effect) s in
      let nee = match q with
	| Some (q', qe) -> 
	    (Subst.predicate s' q'.a_value, s'),
	    (let ee x = 
	       let q' = List.assoc x qe in
	       Subst.predicate s' q'.a_value, s'
	     in
	     exns e ee)
	| None -> 
	    let ee x = Ptrue, s' in
	    (Ptrue, s'), exns e ee
      in
      Ptrue, nee

let wp e =
  let s = Subst.frame e.info.t_env e.info.t_effect Subst.empty in
  let ok,_ = wp e s in
  ok

(*
Local Variables: 
compile-command: "unset LANG; make -C .. byte"
End: 
*)
