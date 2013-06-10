(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* --- Qed Typechecker                                                    --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Syntax

module Make(ADT:Data)(Field:Field) =
struct

  module U = Unify.Make(ADT)(Field)

  type t = U.t

  type tau = (Field.t,ADT.t) datatype
  type signature = (Field.t,ADT.t) funtype

  type lookup = {
    make_field : Syntax.id -> sort -> Field.t ;
    lookup_field : Syntax.id -> Field.t -> bool ;
    lookup_typedef : Syntax.id -> tau ;
    lookup_datatype : ADT.t -> tau option ;
    lookup_signature : Syntax.id -> signature ;
  }
		
  type symbol = 
    | Local of U.t
    | Global of signature

  type env = {
    sigma : U.mgu ;
    globals : lookup ;
    mutable params : (string * symbol) list ;
    mutable bind : t Intmap.t ; (* node bindings *)
  }

  let newenv lookup = {
    sigma = U.create lookup.lookup_datatype ;
    params = [] ;
    globals = lookup ;
    bind = Intmap.empty ;
  }
  
  (* -------------------------------------------------------------------------- *)
  (* --- Type Compilation                                                   --- *)
  (* -------------------------------------------------------------------------- *)
  
  let rec cc_type (env:env) = function
    | T_INT -> U.int
    | T_REAL -> U.real
    | T_PROP -> U.bool
    | T_BOOL -> U.prop
    | T_ALPHA a -> U.quoted env.sigma (snd a)
    | T_ARRAY(a,b) -> U.array (cc_type env a) (cc_type env b)
    | T_RECORD fts -> U.record (List.map (cc_field env) fts)
    | T_SORT(ts,x) ->
	try
	  let t = env.globals.lookup_typedef x in
	  let d = Kind.degree_of_tau t in
	  if d<>List.length ts then
	    Input.error_at (fst x) "type '%s' has arity %d" (snd x) d ;
	  let ts = List.map (cc_type env) ts in
	  U.typedef (Array.of_list ts) t
	with Not_found ->
	  Input.error_at (fst x) "unknown type '%s'" (snd x)

  and cc_field (env:env) (f,t) =
    let u = cc_type env t in
    let a = env.globals.make_field f (U.sort env.sigma u) in
    (a,u)

  (* -------------------------------------------------------------------------- *)
  (* --- Typing Environment                                                 --- *)
  (* -------------------------------------------------------------------------- *)

  let lookup env locals x =
    try List.assoc (snd x) locals
    with Not_found -> 
      try List.assoc (snd x) env.params
      with Not_found ->
	try Global(env.globals.lookup_signature x)
	with Not_found ->
	  Input.error_at (fst x) "unknown symbol '%s'" (snd x)

  (* -------------------------------------------------------------------------- *)
  (* --- Merge Utilities                                                    --- *)
  (* -------------------------------------------------------------------------- *)

  let set_expression env e te tr =
    try U.unify env.sigma te tr
    with Failure msg ->
      Ast.error_at e
	"@[Expression has type %a@ but is required to have type %a@ (%s)@]"
	(U.pretty env.sigma) tr (U.pretty env.sigma) te msg

  let set_variable env x tx tv =
    try U.unify env.sigma tx tv
    with Failure msg ->
      Input.error_at (fst x)
	"@[Variable %s is expected to have type %a@ \
           but is here bound to value of type %a@ \
           (%s)@]"
	(snd x) (U.pretty env.sigma) tx (U.pretty env.sigma) tv msg

  let binop_aryth env x tx y ty =
    match U.sort env.sigma tx , U.sort env.sigma ty with
      | Sint , Sint -> U.int
      | (Sreal|Sint) , (Sreal|Sint) -> U.real
      | _ -> 
	  set_expression env x tx U.int ;
	  set_expression env y ty U.int ;
	  U.int

  let unop_aryth env x tx =
    match U.sort env.sigma tx with
      | Sint -> U.int
      | Sreal -> U.real
      | _ -> set_expression env x tx U.int ; U.int

  let binop_rel env pos tx ty =
    try U.unify env.sigma tx ty
    with Failure msg -> 
      Input.error_at pos
	"@[Can not compare expressions with types@ %a and@ %a@ (%s)"
	(U.pretty env.sigma) tx (U.pretty env.sigma) ty msg

  let binop_logic env x tx y ty =
    set_expression env x tx U.bool ;
    set_expression env y ty U.bool ;
    match U.sort env.sigma tx , U.sort env.sigma ty with
      | Sbool , Sbool -> U.bool
      | _ -> U.prop

  let unop_logic env x tx =
    set_expression env x tx U.bool ; tx

  let rec set_params env ts es vs =
    match ts with
      | [] -> ()
      | t::ts ->
	  set_expression env (List.hd es) t (List.hd vs) ;
	  set_params env ts (List.tl es) (List.tl vs)
	  
  let bind env k t = env.bind <- Intmap.add k t env.bind ; t
  let final_degree env = U.final_degree env.sigma
  let final_type env t = U.generalize env.sigma t
  let final_node env k = U.generalize env.sigma (Intmap.find k env.bind)
  let final_fields env k =
    let u = Intmap.find k env.bind in
    List.map fst (U.fields env.sigma u)

  (* -------------------------------------------------------------------------- *)
  (* --- Parameters                                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let cc_arg env x = function
    | None -> U.fresh env.sigma
    | Some t -> 
	try cc_type env t
	with Failure msg ->
	  Input.error_at (fst x) "incorrect type for parameter '%s' (%s)" 
	    (snd x) msg

  let cc_result env = function
    | None -> U.fresh env.sigma
    | Some t -> 
	try cc_type env t
	with Failure msg ->
	  failwith (Printf.sprintf "incorrect return type (%s)" msg)

  let cc_local env x k tx =
    bind env k (cc_arg env x tx)

  let cc_param env (x,k,tx) =
    let u = cc_local env x k tx in
    env.params <- (snd x,Local u)::env.params

  let pvar env x =
    try match List.assoc (snd x) env.params with
      | Local u -> u
      | Global _ ->
	  Input.error_at (fst x)
	    "parameter '%s' is associated to a signature, not a pattern" (snd x)
    with Not_found ->
      let u = U.fresh env.sigma in
      env.params <- (snd x,Local u)::env.params ; u

  let check_local x es =
    if es <> [] then
      Input.error_at 
	(fst x)
	"local variable '%s' is not a function, it can not be applied" 
	(snd x)

  let check_arity x ts es =
    let nt = List.length ts in
    let ne = List.length es in
    if nt<>ne then
      Input.error_at
	(fst x)
	"symbol '%s' has arity %d but is here applied to %d arguments"
	(snd x) nt ne

  let get_field env tr fts f k =
    try
      let _,tf = List.find (fun (fd,_) -> env.globals.lookup_field f fd) fts 
      in bind env k tf
    with Not_found ->
      Input.error_at
	(fst f)
	"field '%s' not defined in %a" (snd f) (U.pretty env.sigma) tr

  (* -------------------------------------------------------------------------- *)
  (* --- Expressions (and Patterns)                                         --- *)
  (* -------------------------------------------------------------------------- *)
	
  type locals = (string * symbol) list

  let rec do_typecheck (env:env) (locals:locals) e =
    try match e with
      | E_ANY _ -> U.fresh env.sigma
      | E_PVAR x -> pvar env x
      | E_INT _ -> U.int
      | E_REAL _ -> U.real
      | E_TRUE _ | E_FALSE _ -> U.bool

      | E_BIN(x,_,(ADD|SUB|MUL|DIV),y) ->
	  let tx = do_typecheck env locals x in
	  let ty = do_typecheck env locals y in
	  binop_aryth env x tx y ty

      | E_BIN(x,_,MOD,y) ->
	  set_expression env x (do_typecheck env locals x) U.int ;
	  set_expression env y (do_typecheck env locals y) U.int ;
	  U.int

      | E_BIN(x,_,(LT|GT|LEQ|GEQ),y) ->
	  let tx = do_typecheck env locals x in
	  let ty = do_typecheck env locals y in
	  ignore (binop_aryth env x tx y ty) ;
	  U.bool

      | E_BIN(x,pos,(EQ|NEQ),y) ->
	  let tx = do_typecheck env locals x in
	  let ty = do_typecheck env locals y in
	  binop_rel env pos tx ty ;
	  U.bool
	    
      | E_UNA(_,NOT,x) ->
	  unop_logic env x (do_typecheck env locals x) 

      | E_BIN(x,_,(AND|OR|IMPLY|EQUIV),y) ->
	  let tx = do_typecheck env locals x in
	  let ty = do_typecheck env locals y in
	  binop_logic env x tx y ty

      | E_UNA(_,OPP,x) ->
	  unop_aryth env x (do_typecheck env locals x)

      | E_FUN(x,k,es) ->
	  begin
	    match lookup env locals x with
	      | Local u -> 
		  check_local x es ; 
		  bind env k u
	      | Global s ->
		  check_arity x es s.Logic.params ;
		  let (tr,ts) = U.of_sig env.sigma s in
		  let vs = List.map (do_typecheck env locals) es in
		  set_params env ts es vs ;
		  bind env k tr
	  end

      | E_IF(c,k,a,b) ->
	  set_expression env c (do_typecheck env locals c) U.bool ;
	  let tr = U.fresh env.sigma in
	  let ta = do_typecheck env locals a in
	  let tb = do_typecheck env locals b in
	  begin
	    try
	      U.unify env.sigma tr ta ;
	      U.unify env.sigma tr tb ;
	      bind env k tr
	    with Failure msg ->
	      Ast.error_at e
		"@[Incompatible types@ %a (then branch)@ %a (else branch)@ (%s)@]"
		(U.pretty env.sigma) ta (U.pretty env.sigma) tb msg
	  end

      | A_GET(m,k) ->
	  let tm = do_typecheck env locals m in
	  let tk = do_typecheck env locals k in
	  let tr = U.fresh env.sigma in
	  let ta = U.array tk tr in
	  begin
	    try
	      U.unify env.sigma tm ta ; tr
	    with Failure msg ->
	      Ast.error_at e
		"@[Incompatible type@ %a with@ %a in array access@ (%s)@]"
		(U.pretty env.sigma) tm (U.pretty env.sigma) ta msg
	  end

      | A_SET(m,k,v) ->
	  let tm = do_typecheck env locals m in
	  let tk = do_typecheck env locals k in
	  let tv = do_typecheck env locals v in
	  let ta = U.array tk tv in
	  begin
	    try
	      U.unify env.sigma tm ta ; ta
	    with Failure msg ->
	      Ast.error_at e
		"@[Incompatible type@ %a with@ %a in array access@ (%s)@]"
		(U.pretty env.sigma) tm (U.pretty env.sigma) ta msg
	  end

      | E_GETFIELD(r,f,k) ->
	  let tr = do_typecheck env locals r in
	  let fts = U.fields env.sigma tr in
	  get_field env tr fts f k

      | E_RECORD(_,fes) ->
	  U.record 
	    (List.map
	       (fun (f,k,e) -> 
		  let te = do_typecheck env locals e in
		  let se = U.sort env.sigma te in
		  env.globals.make_field f se , bind env k te)
	       fes)
	    
      | E_SETFIELD(r,k,fes) ->
	  let tr = do_typecheck env locals r in
	  let fts = U.fields env.sigma tr in
	  List.iter
	    (fun (f,k,e) ->
	       let te = do_typecheck env locals e in
	       let tf = get_field env tr fts f k in
	       U.unify env.sigma te tf)
	    fes ;
	  bind env k tr

      | E_LET(x,k,t,a,b) ->
	  let ta = do_typecheck env locals a in
	  let tx = cc_local env x k t in
	  let locals = (snd x,Local tx)::locals in
	  set_variable env x tx ta ;
	  do_typecheck env locals b
	    
      | E_FORALL(x,k,t,_,p) | E_EXISTS(x,k,t,_,p) ->
	  let tx = cc_local env x k t in
	  let locals = (snd x,Local tx)::locals in
	  let tp = do_typecheck env locals p in
	  set_expression env p tp U.bool ;
	  U.prop

    with err -> Ast.raise_at e err

  let create lookup args =
    let env = newenv lookup in
    List.iter (cc_param env) args ; env

  let typecheck env e tr = 
    let u = do_typecheck env [] e in
    let v = cc_result env tr in
    set_expression env e u v ;
    final_type env u

  let signature lookup ts t =
    let env = create lookup [] in
    let us = List.map (cc_type env) ts in
    let u = cc_type env t in {
      Logic.result = final_type env u ;
      Logic.params = List.map (final_type env) us ;
    }


end
      
(* -------------------------------------------------------------------------- *)
