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

(*i $Id: mlize.ml,v 1.83 2008/02/05 12:10:49 marche Exp $ i*)

(*s Translation of imperative programs into functional ones. *)

open Ident
open Logic
open Misc
open Types
open Ast
open Cc
open Util
open Rename
open Env

module Make (Monad : MonadSig.S) =
struct

  open Monad

  let new_label info = { info with t_label = label_name () }
    
  let make_info info k = 
    { info with t_label = label_name (); 
	t_post = k.t_post; t_effect = k.t_effect }

      (*s [ren] is the current renamings of variables,
	[e] is the imperative program to translate, annotated with type+effects.
	We return the translated program in type [predicate cc_term] *)

  let rec trad e =
    cross_label e.info.t_label (trad_desc e.info e.desc)

  and trad_desc info d ren = match d with
    | Expression t ->
	Monad.unit info (Value (unref_term t)) ren

    | Var id ->
	assert (not (is_reference info.t_env id));
	if is_rec id then
	  find_rec id ren 
	else
	  CC_var id

    | If (e1, e2, e3) ->
	trad_conditional info 
	  e1.info (trad e1) e2.info (trad e2) e3.info (trad e3) 
	  ren

    | LetIn (x, e1, e2) ->
	let info1 = { e1.info with t_result_name = x } in
	  Monad.compose info1 (trad e1) info
	    (fun v1 ren' ->
	       let te2 = 
		 Monad.compose e2.info (trad e2) info
		   (fun v2 -> Monad.unit info (Value (Tvar v2))) ren' 
	       in
		 if v1 <> x then
		   let ty1 = trad_type_v ren info.t_env (result_type e1) in
		     CC_letin (false, [x, CC_var_binder ty1], CC_var v1, te2)
		 else
		   te2)
	    ren

    | Seq (e1, e2) ->
	Monad.compose e1.info (trad e1) info
	  (fun _ ren' ->
	     Monad.compose e2.info (trad e2) info
	       (fun v2 -> Monad.unit info (Value (Tvar v2))) ren')
	  ren

    | LetRef (x, e1, e2) ->
	let info1 = { e1.info with t_result_name = x } in
	  Monad.compose info1 (trad e1) info
	    (fun v1 ren' ->
	       let t1 = trad_type_v ren info.t_env (result_type e1) in
	       let ren'' = next ren' [x] in
	       let x' = current_var ren'' x in
		 CC_letin (false, [x', CC_var_binder t1], CC_var v1, 
			   Monad.compose e2.info (trad e2) info
			     (fun v2 -> Monad.unit info (Value (Tvar v2))) ren''))
	    ren

    | AppTerm (e1, t, kapp) ->
	let infoapp = make_info info kapp in
	  Monad.compose e1.info (trad e1) info
	    (fun v1 -> 
	       Monad.apply 
		 infoapp (fun ren -> 
			    let t = apply_term ren info.t_env (unref_term t) in
			      CC_app (CC_var v1, CC_term t)) 
		 info (fun v -> Monad.unit info (Value (Tvar v))))
	    ren

    | AppRef (e1, _, kapp) ->
	let infoapp = make_info info kapp in
	  Monad.compose e1.info (trad e1) info
	    (fun v1 -> 
	       Monad.apply infoapp (fun _ -> CC_var v1) info
		 (fun v -> Monad.unit info (Value (Tvar v))))
	    ren

    | Lam (bl, p, e) ->
	let bl',env' = trad_binders ren info.t_env bl in
	let ren' = initial_renaming env' in
	let te = trans (p,e) ren' in
	  cc_lam bl' te

    | Loop (_, None, _) ->
	assert false

    | Loop (inv, Some var, e) ->
	let p = match inv with Some a -> [a] | None -> [] in
	let infoc = new_label info in
	  Monad.wfrec var p info
	    (fun w -> Monad.compose e.info (trad e) infoc (fun _ -> w))
	    ren

    | Rec (_, _, _, None, _, _) ->
	assert false

    | Rec (f, bl, v, Some var, p, e) -> 
	let bl',env' = trad_binders ren info.t_env bl in
	let ren' = push_date (initial_renaming env') e.info.t_label in
	let recf w ren = cc_lam bl' (abstraction e.info p w ren) in
	  cc_lam bl' 
	    (abstraction e.info p
	       (Monad.wfrec_with_binders bl' var p e.info
		  (fun w -> with_rec f (recf w) (trad e)))
	       ren')

    | Raise (id, None) ->
	Monad.exn info id None ren

    | Raise (id, Some e) ->
	Monad.compose e.info (trad e) info
	  (fun v -> Monad.exn info id (Some (Tvar v))) 
	  ren

    | Try (e, hl) ->
	let handler ((x,a) as p, h) =
	  let hi = 
	    Monad.compose h.info (trad h) info 
	      (fun v -> Monad.unit info (Value (Tvar v)))
	  in
	    p, (fun res ren -> match a with
		  | None -> 
		      hi ren
		  | Some a ->
		      let ta = exn_arg_type x in
			CC_letin (false, [a, CC_var_binder ta], CC_var res, hi ren))
	in
	  Monad.handle e.info (trad e) info (List.map handler hl) ren

    | Assertion (k, al, e) ->
	insert_many_pre info.t_env al (trad e) ren

    | Ast.Absurd ->
	let v = info.t_result_type in
	let ainfo = 
	  { info with 
	      t_label = label_name (); t_result_type = v; t_post = None }
	in
	let tv = trad_type_v ren info.t_env v in
	  Monad.compose ainfo 
	    (fun _ -> cc_applist (cc_var false_rec) 
	       [CC_type tv; CC_hole (info.t_loc, Pfalse)])
	    info (fun v -> Monad.unit info (Value (Tvar v)))
	    ren

	    (**
	       | Any c when info.obligations = [] && info.kappa = c ->
	       CC_any (Monad.trad_type_c ren info.env c)
	    **)

    | Any c ->
	let info_c = 
	  Typing.typing_info_of_type_c info.t_loc info.t_env info.t_label c 
	in
	  Monad.compose 
	    info_c (fun ren -> CC_any (Monad.trad_type_c ren info.t_env c)) 
	    info (fun v -> Monad.unit info (Value (Tvar v)))
	    ren

    | Label (s, e) ->
	cross_label s (trad e) ren

    | Post (e, q, _) ->
	(* TODO *)
	trad e ren 

  and trad_binders ren env = function
    | [] -> 
	[], env
    | (id, (Ref _ as v)) :: bl ->
	trad_binders ren (Env.add id v env) bl
    | (id, v) :: bl ->
	let tt =  trad_type_v ren env v in
	let env' = Env.add id v env in
	let bl',env'' = trad_binders ren env' bl in
	  (id, CC_var_binder tt) :: bl', env''

	    (* to be used for both [if] and [while] *)
  and trad_conditional info info1 te1 info2 te2 info3 te3 =
    Monad.compose info1 te1 info
      (fun resb ren' -> 
	 let q1 = 
	   option_app (a_apply_post info1.t_label ren' info.t_env) info1.t_post
	 in
	 let branch infob eb tb = 
	   (***
	       let info = { info with loc = infob.loc;
	       kappa = force_type_c_loc infob.loc info.kappa } in
           ***)
	   let t = 
	     Monad.compose infob eb info
	       (fun r -> Monad.unit info (Value (Tvar r))) ren'
	   in
	     match q1 with
	       | Some (q,_) -> 
		   let n = if is_wp q.a_name then q.a_name else test_name () in
		   let q = tsubst_in_predicate (subst_one result tb) q.a_value in
		     CC_lam ((n, CC_pred_binder (simplify q)), t)
	       | None -> 
		   t
	 in
	   CC_if (CC_var resb,  
		  branch info2 te2 ttrue, branch info3 te3 tfalse))

  and trans (p,e) =
    cross_label e.info.t_label (abstraction e.info p (trad e))

end

module MLwithLogicTerms = Make (Monad)

let trad = MLwithLogicTerms.trad


