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

open Cenv
open Cast
open Clogic
open Info
open Ctypes

let binop = function
  | Cast.Bbw_and -> Bbw_and
  | Cast.Bbw_or -> Bbw_or
  | Cast.Bbw_xor -> Bbw_xor
  | Cast.Bshift_right -> Bshift_right
  | Cast.Bshift_left -> Bshift_left
  | _ -> assert false


let rec term_of_expr (e:texpr)  =
  {
    term_node = 
     begin 
       match e.texpr_node with
	 | TEnop ->  assert false
	 | TEconstant c -> Tconstant c
	 | TEstring_literal s -> assert false (*TODO*)
	 | TEvar (Var_info v) -> 
	     Clogic.Tvar v 
	 | TEvar (Fun_info f) -> assert false (*TODO*)
	 | TEdot (e, v) -> Tdot (term_of_expr e, v)
	 | TEarrow (e, v) -> Tarrow (term_of_expr e, v)
	 | TEarrget (e1, e2) -> Tarrget (term_of_expr e1, term_of_expr e2)
	 | TEseq (e1, e2) -> assert false
	 | TEassign (e1, e2) -> assert false (*TODO*)
	 | TEassign_op (e1, b, e2) -> assert false (*TODO*)
	 | TEunary (Cast.Uplus, e) -> Tunop (Clogic.Uplus, term_of_expr e)
	 | TEunary (Cast.Uminus, e) -> Tunop (Clogic.Uminus, term_of_expr e)
	 | TEunary (Cast.Unot, e) -> Tunop (Clogic.Unot,term_of_expr e) 
	 | TEunary (Cast.Ustar, e) -> Tunop (Clogic.Ustar, term_of_expr e)
	 | TEunary (Cast.Uamp, e) -> Tunop (Clogic.Uamp, term_of_expr e)
	 | TEunary (Cast.Utilde, e) -> Tunop (Utilde, term_of_expr e)
	 | TEunary (Cast.Ufloat_of_int, e) -> 
	     Tunop (Ufloat_of_int, term_of_expr e)
	 | TEunary (Cast.Ufloat_conversion, e) -> 
	     Tunop (Ufloat_conversion, term_of_expr e)
	 | TEunary (Cast.Uint_of_float, e) -> 
	     Tunop (Uint_of_float, term_of_expr e)
	 | TEunary (Cast.Uint_conversion, e) -> 
	     (term_of_expr e).term_node
	 | TEincr (Uprefix_inc,e)| TEincr (Upostfix_inc,e) -> 
	     Tbinop (term_of_expr e, Badd, {
		       term_node = Tconstant (IntConstant "1");
		       term_loc = Loc.dummy_position;
		       term_type = c_int;
		     })	 
	 | TEincr (Uprefix_dec,e) | TEincr (Upostfix_dec,e) -> 
	     Tbinop (term_of_expr e, Bsub, {
		       term_node = Tconstant (IntConstant "1");
		       term_loc = Loc.dummy_position;
		       term_type = c_int;
		     })
	 | TEbinary (e1, Badd_pointer_int, e2) | TEbinary (e1,Badd_float _,e2) 
	 | TEbinary (e1, Badd_int _, e2) | TEbinary (e1, Cast.Badd, e2) -> 
	     Tbinop (term_of_expr e1, Badd, term_of_expr e2)
	 | TEbinary (e1, Bsub_float _, e2)| TEbinary (e1, Bsub_pointer, e2) 
	 | TEbinary (e1, Bsub_int _, e2) | TEbinary (e1, Cast.Bsub, e2) ->
	     Tbinop (term_of_expr e1, Bsub, term_of_expr e2)
	 | TEbinary (e1, Bmul_float _, e2) 
	 | TEbinary (e1, Bmul_int _, e2) | TEbinary (e1, Cast.Bmul, e2) ->
	     Tbinop (term_of_expr e1, Bmul, term_of_expr e2)
	 | TEbinary (e1, Bdiv_int _, e2) | TEbinary (e1, Cast.Bdiv, e2) 
	 | TEbinary (e1, Bdiv_float _, e2) ->
	     Tbinop (term_of_expr e1, Bdiv, term_of_expr e2)
	 | TEbinary (e1, Bmod_int _, e2) | TEbinary (e1, Cast.Bmod, e2) ->
	     Tbinop (term_of_expr e1, Bmod, term_of_expr e2)
	 | TEbinary (e1, (Cast.Bbw_and | Cast.Bbw_or | Cast.Bbw_xor |
			  Cast.Bshift_left | Cast.Bshift_right as op), e2) -> 
	     Tbinop (term_of_expr e1, binop op, term_of_expr e2)
	 | TEbinary (e1, Blt, e2) | TEbinary (e1, Bgt, e2) ->
	     assert false
	 | TEbinary (e1, Ble, e2) | TEbinary (e1, Bge, e2)->
	     assert false 
	 | TEbinary (e1, Beq, e2) | TEbinary (e1, Bneq, e2) ->
	     assert false
	 | TEbinary (e1, Band, e2) | TEbinary (e1, Bor, e2)->
	     assert false 
	 | TEbinary (e1, Blt_int, e2) ->
	     if (Ctyping.eval_const_expr e1 < Ctyping.eval_const_expr e2) 
	     then 
	       Tconstant (IntConstant "0")
	     else 
	       Tconstant (IntConstant "1")
	 | TEbinary (e1, Ble_int, e2)->
	     if (Ctyping.eval_const_expr e1 <= Ctyping.eval_const_expr e2) 
	     then 
	       Tconstant (IntConstant "0")
	     else 
	       Tconstant (IntConstant "1")
	 | TEbinary (e1, Bgt_int, e2) ->
	     if (Ctyping.eval_const_expr e1 < Ctyping.eval_const_expr e2) 
	     then 
	       Tconstant (IntConstant "0")
	     else 
	       Tconstant (IntConstant "1")
	 | TEbinary (e1, Bge_int, e2)->
	     if (Ctyping.eval_const_expr e1 >= Ctyping.eval_const_expr e2) 
	     then 
	       Tconstant (IntConstant "0")
	     else 
	       Tconstant (IntConstant "1")
	 | TEbinary (e1, Beq_int, e2)->
	    if (Ctyping.eval_const_expr e1 = Ctyping.eval_const_expr e2) 
	    then 
	      Tconstant (IntConstant "0")
	    else 
	      Tconstant (IntConstant "1")
	 | TEbinary (e1, Bneq_int, e2)->
	     if (Ctyping.eval_const_expr e1 = Ctyping.eval_const_expr e2) 
	     then 
	       Tconstant (IntConstant "1")
	     else 
	       Tconstant (IntConstant "0")
	 | TEbinary (e1, Blt_float _, e2) | TEbinary (e1, Bgt_float _, e2)->
	     assert false 
	 | TEbinary (e1, Ble_float _, e2) | TEbinary (e1, Bge_float _, e2)->
	     assert false 
	 | TEbinary (e1, Beq_float _, e2) | TEbinary (e1, Bneq_float _, e2) ->
	     assert false
	 | TEbinary (e1, Blt_pointer, e2) | TEbinary (e1, Bgt_pointer, e2) ->
	     assert false
	 | TEbinary (e1, Ble_pointer, e2) | TEbinary (e1, Bge_pointer, e2) ->
	     assert false
	 | TEbinary (e1, Beq_pointer, e2) | TEbinary (e1, Bneq_pointer, e2) ->
	     assert false
	 | TEcall (e, l) -> assert false
	 | TEcond (e1,e2,e3) -> 
	     Tif (term_of_expr e1, term_of_expr e2, term_of_expr e3)
	 | TEcast ({ctype_node = Tint _},
		   ({texpr_type = {ctype_node = Tfloat _}} as e) ) -> 
	     Tunop (Clogic.Uint_of_float,(term_of_expr e))
	 | TEcast ({ctype_node = Tfloat _},
		   ({texpr_type = {ctype_node = Tint _}} as e) ) -> 
	     Tunop (Clogic.Ufloat_of_int,(term_of_expr e))
	 | TEcast (ty,e) -> Tcast (ty, term_of_expr e)
	 | TEsizeof _ -> assert false
	 | TEmalloc _ -> assert false
     end;
    term_loc = e.texpr_loc;
    term_type = e.texpr_type;
  }
     

let in_struct v1 v = 
 	{ term_node = Tdot (v1, v); 
	  term_loc = v1.term_loc;
	  term_type = v.var_type }

let split_decl e ((invs,inits) as acc) = 
  match e.node with 
    | Tinvariant (_,p) -> (p :: invs, inits)
    | Tdecl (t, v, c) ->  (invs, e :: inits)
	(* If a ghost is initialized, then its initialization is took account *)
    | Tghost (vi, Some init) -> (invs, e :: inits) 
    | _ -> acc

let split_decls d = List.fold_right split_decl d ([],[])

let dummy_pred p = { pred_node = p; pred_loc = Loc.dummy_position }

let rec combine_inv = function
  | [] -> dummy_pred Ptrue
  | a::[] -> a
  | a::l -> { a with pred_node = Pand (a, combine_inv l) }


let noattr loc ty e =
  { term_node = e;
    term_type = ty;
    term_loc  = loc
  }

let rec pop_initializer loc t i =
  match i with 
    | [] ->{ term_node = 
	       (match t.ctype_node with
		  | Tint _ | Tenum _-> Tconstant (IntConstant "0")
		  | Tfloat _ -> Tconstant (RealConstant "0.0")
		  | Tpointer _ -> let null = default_var_info "null" in       
		    Cenv.set_var_type (Var_info null) t false;
		    Clogic.Tvar (null)
		  | _ -> assert false);
	     term_type = t;
	     term_loc  = loc
	    },[]
    | (Iexpr e)::l -> 
	let term = term_of_expr e in
	let term =
	  { term with term_node = 
	      if t.ctype_node <> term.term_type.ctype_node then
		Tcast (t,term)
	      else
		term.term_node} in
	term,l
    | (Ilist [])::l -> pop_initializer loc t l
    | (Ilist l)::l' -> 
	let e,r = pop_initializer loc t l in e,r@l'


(* Added by copying pop_initializer in order to take account ghosts initialization.
   It seems that this function can be enhanced.
*)
let rec pop_term_initializer loc t i =
  match i with  
    | [] ->{ term_node = 
	       (match t.ctype_node with
		  | Tint _ | Tenum _-> Tconstant (IntConstant "0")
		  | Tfloat _ -> Tconstant (RealConstant "0.0")
		  | Tpointer _ -> let null = default_var_info "null" in       
		    Cenv.set_var_type (Var_info null) t false;
		    Clogic.Tvar (null)
		  | _ -> assert false);
	     term_type = t;
	     term_loc  = loc
	    },[]
    | (Iexpr e)::l -> 
	let term = e in
	let term =
	  { term with term_node = 
	      if t.ctype_node <> term.term_type.ctype_node then
		Tcast (t,term)
	      else
		term.term_node} in
	term,l
    | (Ilist [])::l -> pop_term_initializer loc t l
    | (Ilist l)::l' -> 
	let e,r = pop_term_initializer loc t l in e,r@l'




let make_and p1 p2 = match p1.pred_node, p2.pred_node with
  | Ptrue, _ -> p2
  | _, Ptrue -> p1
  | _ -> { p1 with pred_node = Pand (p1, p2) }

let prel (t1, r, t2) = dummy_pred (Prel (t1, r, t2))

let make_implies p1 p2 = match p2.pred_node with
  | Ptrue -> { p1 with pred_node = Ptrue }
  | _ -> { p1 with pred_node = Pimplies (p1, p2) }

let make_forall q p = match p.pred_node with
  | Ptrue -> { p with pred_node = Ptrue }
  | _ -> { p with pred_node = Pforall (q, p) }




let rec init_term loc t lvalue initializers =
  match t.ctype_node with
    | Tint _ | Tfloat _ | Tpointer _ | Tenum _ -> 
	let x,l = pop_term_initializer loc t initializers in
	({pred_node = Prel(lvalue,Eq,x);pred_loc = loc}, l)

    | Tstruct n ->
	begin match tag_type_definition n with
	  | TTStructUnion (Tstruct (_), fl) ->
	      List.fold_left 
		(fun (acc,init)  f -> 
		   let block, init' =
		     init_term loc f.var_type 
		       (in_struct lvalue f) init
		   in ({ acc with pred_node = Pand (acc, block)},init'))
		(dummy_pred Ptrue,initializers)  fl
	  | _ ->
	      assert false
	end

    | Tunion n ->
	begin match tag_type_definition n with
	  | TTStructUnion (Tunion (_), f::_) ->
	      let block, init' =
		init_term loc f.var_type 
		  (noattr loc f.var_type (Tarrow(lvalue, f)))
		  initializers
	      in (block,init')
	  | _ ->
	      assert false
	end
    | Tarray (_,ty,Some t) ->
	begin
	  match initializers with
	    | [] ->	
		let i = default_var_info "counter" in
		Cenv.set_var_type (Var_info i) c_int false;
		let vari = { term_node = Clogic.Tvar i; 
			     term_loc = Loc.dummy_position;
			     term_type = c_int;
			   } in
		let ts = Cltyping.int_constant (Int64.to_string t) in
		let ineq = make_and 
			     (prel (Cltyping.zero, Le, vari))
			     (prel (vari, Lt,ts)) in
		let (b,init') = 
		  match ty.ctype_node with 
		    | Tstruct _ |  Tunion _ ->
			init_term loc ty 
			  (noattr loc ty 
			     (Tbinop(lvalue,Badd,vari))) initializers
		    | _ ->
			init_term loc ty 
			  (noattr loc ty (Tarrget(lvalue,vari))) initializers
		  in
		((make_forall [c_int,i] (make_implies ineq b)), init')
	    | _ ->
		let rec init_cells i (block,init) =
		if i >= t then (block,init)
		else
		  let ts = Cltyping.int_constant (Int64.to_string i) in
		  let (b,init') = 
		    match ty.ctype_node with 
		      | Tstruct _ |  Tunion _ ->
			  init_term loc ty 
			    (noattr loc ty 
			       (Tbinop(lvalue,Badd,ts))) init
		      | _ ->
			  init_term loc ty 
			    (noattr loc ty (Tarrget(lvalue,ts))) init
		  in
		  init_cells (Int64.add i Int64.one) 
		    ({ block with pred_node = Pand (block,b)},init')
		in	
		init_cells Int64.zero (dummy_pred Ptrue,initializers)
	end
    | Tarray (_,ty,None) -> assert false
    | Tfun (_, _) -> assert false
    | Tvar _ -> assert false
    | Tvoid -> dummy_pred Ptrue,initializers





let rec init_expr loc t lvalue initializers =
  match t.ctype_node with
    | Tint _ | Tfloat _ | Tpointer _ | Tenum _ -> 
	let x,l = pop_initializer loc t initializers in
	({pred_node = Prel(lvalue,Eq,x);pred_loc = loc}, l)
    | Tstruct n ->
	begin match tag_type_definition n with
	  | TTStructUnion (Tstruct (_), fl) ->
	      List.fold_left 
		(fun (acc,init)  f -> 
		   let block, init' =
		     init_expr loc f.var_type 
		       (in_struct lvalue f) init
		   in ({ acc with pred_node = Pand (acc, block)},init'))
		(dummy_pred Ptrue,initializers)  fl
	  | _ ->
	      assert false
	end
    | Tunion n ->
	begin match tag_type_definition n with
	  | TTStructUnion (Tunion (_), f::_) ->
	      let block, init' =
		init_expr loc f.var_type 
		  (noattr loc f.var_type (Tarrow(lvalue, f)))
		  initializers
	      in (block,init')
	  | _ ->
	      assert false
	end
    | Tarray (_,ty,Some t) ->
	begin
	  match initializers with
	    | [] ->	
		let i = default_var_info "counter" in
		Cenv.set_var_type (Var_info i) c_int false;
		let vari = { term_node = Clogic.Tvar i; 
			     term_loc = Loc.dummy_position;
			     term_type = c_int;
			   } in
		let ts = Cltyping.int_constant (Int64.to_string t) in
		let ineq = make_and 
			     (prel (Cltyping.zero, Le, vari))
			     (prel (vari, Lt,ts)) in
		let (b,init') = 
		  match ty.ctype_node with 
		    | Tstruct _ |  Tunion _ ->
			init_expr loc ty 
			  (noattr loc ty 
			     (Tbinop(lvalue,Badd,vari))) initializers
		    | _ ->
			init_expr loc ty 
			  (noattr loc ty (Tarrget(lvalue,vari))) initializers
		  in
		((make_forall [c_int,i] (make_implies ineq b)), init')
	    | _ ->
		let rec init_cells i (block,init) =
		if i >= t then (block,init)
		else
		  let ts = Cltyping.int_constant (Int64.to_string i) in
		  let (b,init') = 
		    match ty.ctype_node with 
		      | Tstruct _ |  Tunion _ ->
			  init_expr loc ty 
			    (noattr loc ty 
			       (Tbinop(lvalue,Badd,ts))) init
		      | _ ->
			  init_expr loc ty 
			    (noattr loc ty (Tarrget(lvalue,ts))) init
		  in
		  init_cells (Int64.add i Int64.one) 
		    ({ block with pred_node = Pand (block,b)},init')
		in	
		init_cells Int64.zero (dummy_pred Ptrue,initializers)
	end
    | Tarray (_,ty,None) -> assert false
    | Tfun (_, _) -> assert false
    | Tvar _ -> assert false
    | Tvoid -> dummy_pred Ptrue,initializers


let rec assigns decl =
  match decl with
    | [] -> dummy_pred Ptrue
    | {node = Tdecl (t,v,None); loc = l}::decl ->
	Coptions.lprintf "initialization of %s@." v.var_name;
	let declar,_ = 
	  init_expr l t (noattr l t (Clogic.Tvar v)) [] in
	{declar with pred_node = Pand (declar, assigns decl)}

    | {node = Tdecl(t, v, Some c) ; loc = l }:: decl ->
	Coptions.lprintf "initialization of %s@." v.var_name;
	let declar,_ = init_expr l t (noattr l t (Clogic.Tvar v)) [c] in
	{declar with pred_node = Pand (declar, assigns decl) }

    | {node = Tghost (v,None); loc = l}::decl -> (* Added in order to take account ghosts initialization *)
	Coptions.lprintf "initialization of %s@." v.var_name;
	let t = v.var_type in
	let declar,_ = 
	  init_term l t (noattr l t (Clogic.Tvar v)) [] in
	  {declar with pred_node = Pand (declar, assigns decl)}

    | {node = Tghost(v, Some c) ; loc = l }:: decl -> (* Added in order to take account ghosts initialization *)
	let t = v.var_type in
	  Coptions.lprintf "initialization of %s@." v.var_name;
	  let declar,_ = init_term l t (noattr l t (Clogic.Tvar v)) [c] in
	    {declar with pred_node = Pand (declar, assigns decl) }

     | _  -> assert false


let invariants_initially_established_info =
  default_fun_info "invariants_initially_established"

let rec reorder l =
  match l with 
    | { node = Tdecl _ }as e ::l  -> let decl,other = reorder l in
      e::decl,other
    | { node = Tghost _ }as e ::l  -> let decl,other = reorder l in
      e::decl,other
    | e::l -> let decl,other = reorder l in
      decl,e::other
    | [] -> [],[]

let user_invariants = ref false

let add_init l = 
  let (inv,decl) = split_decls l in
  if inv = [] then user_invariants := false
  else user_invariants := true;
  let inv = combine_inv inv in
  let init_fun =
    Tfundef ({requires = Some (assigns decl);
	      assigns = None;
	      ensures = Some inv; 
	      decreases = None},
	     c_void,
	     invariants_initially_established_info,
	     {st_node = TSnop;
	      st_break = false;    
	      st_continue = false; 
	      st_return = false;   
		st_term = true;     
		st_loc = Loc.dummy_position 
	     })
  in
  let decl,other = 
    reorder ({ node = init_fun; loc = Loc.dummy_position } :: l) in
  decl@other
