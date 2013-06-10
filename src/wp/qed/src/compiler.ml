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
(* --- Compiler for Qed Syntax                                            --- *)
(* -------------------------------------------------------------------------- *)

open Logic
open Syntax

module Make(T:Term) =
struct

  module Tc = Typechecker.Make(T.ADT)(T.Field)

  type symbol = 
    | Fun of T.signature * T.Fun.t
    | Val of T.tau * T.term

  type lookup = {
    make_field : Syntax.id -> sort -> T.Field.t ;
    lookup_field : Syntax.id -> T.Field.t -> bool ;
    lookup_typedef : Syntax.id -> T.tau ;
    lookup_datatype : T.ADT.t -> T.tau option ;
    lookup_symbol  : Syntax.id -> symbol ;
  }

  let rec cc_type env alpha = function
    | T_INT -> Int
    | T_REAL -> Real
    | T_BOOL -> Bool
    | T_PROP -> Prop
    | T_ARRAY(k,e) -> Array(cc_type env alpha k,cc_type env alpha e)
    | T_ALPHA x -> 
	begin
	  try List.assoc (snd x) alpha
	  with Not_found -> 
	    Input.error_at (fst x) "unknown type variable %s" (snd x)
	end
    | T_RECORD fts ->
	Record 
	  (List.map
	     (fun (f,t) ->
		let t = cc_type env alpha t in
		let a = env.make_field f (Kind.of_tau t) in
		a , t) 
	     fts)
    | T_SORT(ts,x) ->
	begin
	  try
	    let t = env.lookup_typedef x in
	    let d = Kind.degree_of_tau t in
	    if d<>List.length ts then
	      Input.error_at (fst x) "type '%s' has arity %d" (snd x) d ;
	    let ts = List.map (cc_type env alpha) ts in
	    Kind.tmap (Array.of_list ts) t
	  with Not_found ->
	    Input.error_at (fst x) "unknown type '%s'" (snd x)
	end

  let cc_tau env xs t =
    (* environment for alpha-variables *)
    let rec quoted k = function
      | [] -> []
      | x::xs -> (snd x,Tvar k)::quoted (succ k) xs
    in cc_type env (quoted 1 xs) t

  (* -------------------------------------------------------------------------- *)
  (* --- Compilation of typed terms                                         --- *)
  (* -------------------------------------------------------------------------- *)

  let binop = function
    | ADD -> T.e_add
    | SUB -> T.e_sub
    | MUL -> T.e_mul
    | DIV -> T.e_div
    | MOD -> T.e_mod
    | AND -> (fun x y -> T.e_and [x;y])
    | OR  -> (fun x y -> T.e_or [x;y])
    | IMPLY -> (fun h p -> T.e_imply [h] p)
    | EQUIV -> T.e_equiv
    | LT -> T.e_lt
    | LEQ -> T.e_leq
    | GT -> (fun x y -> T.e_lt y x)
    | GEQ -> (fun x y -> T.e_leq y x)
    | EQ -> T.e_eq
    | NEQ -> T.e_neq
	
  type env = {
    symbol : Syntax.id -> symbol ;
    field : Syntax.id -> sort -> T.Field.t ;
    typing : Tc.env ;
    pool : T.pool ;
    mutable locals : (string * T.term) list ;
  }

  type value = Efun of T.Fun.t | Eval of T.term

  let push (env:env) x k =
    let tau = Tc.final_node env.typing k in
    let var = T.fresh env.pool ~basename:(snd x) tau in
    let term = T.e_var var in
    env.locals <- (snd x,term) :: env.locals ; var

  let pop env x =
    assert (fst (List.hd env.locals) = snd x) ;
    env.locals <- List.tl env.locals

  let lookup env x =
    try Eval(List.assoc (snd x) env.locals)
    with Not_found -> 
      try 
	match env.symbol x with
	  | Fun(_,f) -> Efun f
	  | Val(_,e) -> Eval e
      with Not_found ->
	Input.error_at (fst x) "unknown symbol '%s'" (snd x)

  let make_field env f k =
    let tf = Tc.final_node env.typing k in
    let sf = Kind.of_tau tf in
    env.field f sf

  let rec cc env e =
    try match e with
      | E_ANY pos -> Input.error_at pos "unexpected pattern"
      | E_PVAR x -> Input.error_at (fst x) "unexpected pattern"
      | E_INT(_,z)  -> T.e_zint (Z.of_string z)
      | E_REAL(_,r) -> T.e_real (R.of_string r)
      | E_TRUE _    -> T.e_true
      | E_FALSE _   -> T.e_false
      | E_BIN(x,_,op,y) -> (binop op) (cc env x) (cc env y)
      | E_UNA(_,NOT,x) -> T.e_not (cc env x)
      | E_UNA(_,OPP,x) -> T.e_opp (cc env x)
      | E_FUN(x,_,es) ->
	  begin
	    match lookup env x with
	      | Eval value ->
		  if es <> [] then
		    Input.error_at (fst x)
		      "local variable %s can not be applied" (snd x) ;
		  value
	      | Efun f -> 
		  T.e_fun f (List.map (cc env) es)
	  end
      | E_IF(e,_,a,b) -> T.e_if (cc env e) (cc env a) (cc env b)
      | A_GET(m,k) -> T.e_get (cc env m) (cc env k)
      | A_SET(m,k,v) -> T.e_set (cc env m) (cc env k) (cc env v)
      | E_GETFIELD(r,f,k) -> T.e_getfield (cc env r) (make_field env f k)
      | E_SETFIELD(r,k,fvs) ->
	  let base = cc env r in
	  let fbase = List.map
	    (fun fd -> fd , T.e_getfield base fd)
	    (Tc.final_fields env.typing k) in
	  let fvalues = List.map 
	    (fun (f,k,v) -> make_field env f k,cc env v) 
	    fvs in
	  let rec merge fbase fvalues =
	    match fbase , fvalues with
	      | [] , _ -> fvalues
	      | _ , [] -> fbase
	      | fb::fb_others , gv::gv_others ->
		  let cmp = T.Field.compare (fst fb) (fst gv) in
		  if cmp < 0 then fb :: (merge fb_others fvalues)
		  else if cmp > 0 then gv :: (merge fbase gv_others)
		  else gv :: (merge fb_others gv_others)
	  in
	  T.e_record (merge fbase fvalues)
      | E_RECORD(_,fvs) ->
	  T.e_record 
	    (List.map (fun (f,k,v) -> make_field env f k , cc env v) fvs)
      | E_LET(x,_,_,a,b) ->
	  let va = cc env a in
	  env.locals <- (snd x,va)::env.locals ;
	  let vb = cc env b in
	  env.locals <- List.tl env.locals ;
	  vb
      | E_FORALL(x,k,_,_,p) ->
	  let var = push env x k in
	  let prop = T.e_forall [var] (cc env p) in
	  pop env x ; prop
      | E_EXISTS(x,k,_,_,p) ->
	  let var = push env x k in
	  let prop = T.e_exists [var] (cc env p) in
	  pop env x ; prop
    with err -> Ast.raise_at e err

  (* -------------------------------------------------------------------------- *)
  (* --- Signature & Expression entry points                                --- *)
  (* -------------------------------------------------------------------------- *)

  let tc_lookup global =  {
    Tc.make_field = global.make_field ;
    Tc.lookup_field = global.lookup_field ;
    Tc.lookup_typedef = global.lookup_typedef ;
    Tc.lookup_datatype = global.lookup_datatype ;
    Tc.lookup_signature = 
      (fun x -> 
	 match global.lookup_symbol x with
	   | Fun(s,_) -> s
	   | Val(t,_) -> { result=t ; params=[] }) ;
  } 

  let cc_sig global params result =
    Tc.signature (tc_lookup global) params result

  let cc_def global params result expr =
    let sigma = Tc.create (tc_lookup global) params in
    let tr = Tc.typecheck sigma expr result in 
    let env = { 
      symbol=global.lookup_symbol ; 
      field =global.make_field ;
      typing=sigma ; 
      locals=[] ; 
      pool=T.pool () ;
    } in
    let xs = List.map (fun (x,k,_) -> push env x k) params in
    let value = cc env expr in
    let txs = List.map (fun (_,k,_) -> Tc.final_node sigma k) params in
    { Logic.result=tr ; Logic.params=txs } , T.e_lambda xs value

  let cc_exp global expr =
    let sigma = Tc.create (tc_lookup global) [] in
    let tr = Tc.typecheck sigma expr None in
    let env = { 
      symbol=global.lookup_symbol ;
      field=global.make_field ;
      typing=sigma ; 
      locals=[] ; 
      pool=T.pool() ;
    } in
    let value = cc env expr in
    tr , value

end
