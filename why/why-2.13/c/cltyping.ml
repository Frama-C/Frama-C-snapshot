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

(*i $Id: cltyping.ml,v 1.125 2008/02/05 12:10:47 marche Exp $ i*)

open Coptions
open Format
open Cast
open Clogic
open Creport
open Cerror
open Cenv
open Ctypes

let rec eval_const_term_noerror (e : tterm) = match e.term_node with
  | Clogic.Tconstant (RealConstant c | IntConstant c) -> 
      Cconst.int e.term_loc c
  | Clogic.Tvar v ->
      if e.term_type.Ctypes.ctype_const 
      then v.Info.enum_constant_value
      else invalid_arg "not a const variable"
  | Tunop (Uplus,t) -> eval_const_term_noerror t  
  | Tunop (Uminus,t) -> Int64.neg (eval_const_term_noerror t)
  | Tbinop (t1,Badd,t2) ->  
      Int64.add (eval_const_term_noerror t1)  (eval_const_term_noerror t2)
  | Tbinop (t1,Bsub,t2) ->  
      Int64.sub (eval_const_term_noerror t1)  (eval_const_term_noerror t2)
  | Tbinop (t1,Bmul,t2) ->  
      Int64.mul (eval_const_term_noerror t1)  (eval_const_term_noerror t2)
  | Tbinop (t1,Bdiv,t2) ->  
      Int64.div (eval_const_term_noerror t1)  (eval_const_term_noerror t2)
  | _ -> invalid_arg "not a constant expression"

let option_app f = function Some x -> Some (f x) | None -> None

let sign = function true -> Signed | false -> Unsigned

let retype_typedef = function
  | Tint _ when not machine_ints -> Tint (Signed, ExactInt)
  | tn -> tn

let rec type_logic_type ?(machine_ints=machine_ints) loc env = function
  | LTvoid -> c_void
  | LTchar _ | LTshort _ | LTint _ | LTlong _ | LTlonglong _ 
    when not machine_ints ->
      c_exact_int
  | LTchar s -> noattr (Tint (sign s, Char))
  | LTshort s -> noattr (Tint (sign s, Short))
  | LTint s -> noattr (Tint (sign s, Int))
  | LTlong s -> noattr (Tint (sign s, Long))
  | LTlonglong s -> noattr (Tint (sign s, LongLong))
  | LTinteger -> c_exact_int
  | LTfloat -> use_floats := true; c_float Ctypes.Float
  | LTdouble -> use_floats := true; c_float Ctypes.Double
  | LTlongdouble -> use_floats := true; c_float Ctypes.LongDouble
  | LTreal -> c_real
  | LTarray ty -> 
      c_array Not_valid (type_logic_type ~machine_ints loc env ty)
  | LTpointer ty -> 
      c_pointer Not_valid (type_logic_type ~machine_ints loc env ty)
  | LTvar id ->  
      noattr 
	(try 
	   retype_typedef (find_typedef id).ctype_node 
	 with Not_found -> 
	   if not (Cenv.mem_type id) then error loc "unbound type"; 
	   Tvar id )

(* Typing terms *)

let rec is_null t = match t.term_node with
  | Clogic.Tvar v -> v.Info.var_name = "null"
  | Tconstant (IntConstant s) -> (try int_of_string s = 0 with _ -> false)
  | Tcast (_,t) -> is_null t
  | _ -> false

(*
let compatible t1 t2 = 
  sub_type t1.term_type t2.term_type || 
  sub_type t2.term_type t1.term_type ||
  (pointer_or_array_type t1.term_type && is_null t2) ||
  (pointer_or_array_type t2.term_type && is_null t1)
*)
let compatible_term_type t ty = 
  sub_type t.term_type ty || 
  sub_type ty t.term_type ||
  (pointer_or_array_type ty && is_null t)

let expected_type loc t1 t2 =
  if not (eq_type t1 t2) then raise_located loc (ExpectedType (t1, t2))

let expected_term_type loc t1 t2 =
  if not (compatible_term_type t1 t2) 
  then raise_located loc (ExpectedType (t1.term_type, t2))

let expected_num loc t = match t.term_type.ctype_node with
  | Tenum _ | Tint _ | Tfloat _ -> ()
  | _ -> error loc "invalid operand (expected integer or float)"

let expected_num_pointer loc t = match t.term_type.ctype_node with
  | Tenum _ | Tint _ | Tfloat _
  | Tarray _ | Tpointer _ -> ()
  | _ -> 
      Format.eprintf "type = %a@." print_type t.term_type;
      error loc "invalid operand (expected integer, float or pointer)"

let expected_int loc t = match t.term_type.ctype_node with
  | Tenum _ | Tint _ -> ()
  | _ -> error loc "invalid operand (expected integer)"

let coerce ty e = match e.term_type.ctype_node, ty.ctype_node with
  | (Tint _ | Tenum _), Tfloat _ -> 
      { e with term_node = Tunop (Ufloat_of_int, e); term_type = ty }
  | Tfloat _, (Tint _ | Tenum _) ->
      { e with term_node = Tunop (Uint_of_float, e); term_type = ty }
  | Tfloat fk1, Tfloat fk2 when fk1 <> fk2 ->
      { e with term_node = Tunop (Ufloat_conversion, e); term_type = ty }
  | (Tint _ | Tenum _ as ty1), (Tint _ | Tenum _ as ty2) when ty1 <> ty2 ->
      { e with term_node = Tunop (Uint_conversion, e); term_type = ty }
  | ty1, ty2 when eq_type_node ty1 ty2 ->
      e
  | Tpointer (_,{ ctype_node = Tvoid }), Tpointer _ ->
      e
  | _ ->
      if verbose || debug then eprintf 
	"expected %a, found %a@." print_type ty print_type e.term_type;
      error e.term_loc "incompatible type"

(* convert [t1] and [t2] to the same arithmetic type *)
let arith_conversion t1 t2 =
  let ty1 = t1.term_type in
  let ty2 = t2.term_type in
  match ty1.ctype_node, ty2.ctype_node with
    | (Tint _ | Tenum _), (Tint _ | Tenum _) -> 
	coerce c_exact_int t1, coerce c_exact_int t2, c_exact_int
    | Tfloat _, (Tint _ | Tenum _) 
    | (Tint _ | Tenum _), Tfloat _ 
    | Tfloat _, Tfloat _ ->
	coerce c_real t1, coerce c_real t2, c_real
    | _ -> 
	assert false

(* Typing terms *)

open Info

let set_referenced t = match t.term_node with
  | Clogic.Tvar x -> set_is_referenced x
  | Tdot (_,f) | Tarrow(_,f) -> set_is_referenced f
  | _ -> ()


let rec type_term env t =
  let t', ty = type_term_node t.lexpr_loc env t.lexpr_node in
  { term_node = t'; term_loc = t.lexpr_loc; term_type = ty}

and type_term_node loc env = function
  | PLconstant (IntConstant _ as c) -> 
      Tconstant c, c_exact_int
  | PLconstant (RealConstant _ as c) ->
      use_floats := true;
      Tconstant c, c_real
  | PLvar x ->
      let info = 
	try Env.find x.var_name env with Not_found -> 
	  try (Var_info (find_ghost x.var_name)) with Not_found -> 
	    try find_sym x.var_name with Not_found -> 
              error loc "unbound logic variable %s " x.var_name
      in 
      begin match info with
	| Var_info v -> Clogic.Tvar v, v.var_type
	| Fun_info f -> 
            error loc "variable %s is a function" f.fun_name
      end
  | PLapp (f, [t]) when f.logic_name = "sqrt" ->
      let t = type_real_term env t in
      Tunop (Usqrt_real, t), c_real
  | PLapp (f, tl) ->
      (try 
	 let pl, ty, info = find_logic f.logic_name in
	 let tl = type_terms loc env pl tl in
	 Tapp (info, tl), ty
       with Not_found -> 
	 error loc "unbound function %s" f.logic_name)
  | PLunop (Utilde, t) -> 
      let t = type_int_term env t in
      Tunop (Utilde, t), t.term_type
  | PLunop (Uminus, t) -> 
      let t = type_num_term env t in
      begin match t.term_type.ctype_node with
	| Tenum _ | Tint _ -> Tunop (Uminus, coerce c_exact_int t), c_exact_int
	| Tfloat _ -> Tunop (Uminus, coerce c_real t), c_real
	| _ -> assert false
      end  
  | PLunop (Uplus, t) -> 
      let t = type_num_term env t in
      begin match t.term_type.ctype_node with
	| Tenum _ | Tint _ -> Tunop (Uplus, coerce c_exact_int t), c_exact_int
	| Tfloat _ -> Tunop (Uplus, coerce c_real t), c_real
	| _ -> assert false
      end
  | PLunop (Uabs_real | Usqrt_real as op, t) -> 
      let t = type_real_term env t in
      Tunop (op, t), c_real
  | PLunop (Ustar, t) -> 
      let t = type_term env t in
      begin match t.term_type.ctype_node with
	| Tpointer (_,ty) | Tarray (_,ty,_) -> 
	    Tunop (Ustar, t), ty
	| _ -> error loc "invalid type argument of `unary *'"
      end
  | PLunop (Uamp, t) -> 
      let t = type_term env t in
      set_referenced t;
      Tunop (Uamp, t), noattr (Tpointer(Valid(Int64.zero,Int64.one), t.term_type))   
  | PLnot t -> 
      let t = type_term env t in
      Tunop (Unot, t), t.term_type   
  | PLunop (Uround_error | Utotal_error | Uexact | Umodel as op, t) ->
      let t = type_float_term env t in
      Tunop (op, t), c_real
  | PLunop ((Ufloat_of_int | Uint_of_float | 
	     Ufloat_conversion | Uint_conversion | Unot), _) ->
      assert false
  | PLbinop (t1, Badd, t2) ->
      let t1 = type_term env t1 in
      let ty1 = t1.term_type in
      let t2 = type_term env t2 in
      let ty2 = t2.term_type in
      begin match ty1.ctype_node, ty2.ctype_node with
	| (Tenum _ | Tint _ | Tfloat _), (Tenum _ | Tint _ | Tfloat _) ->
	    let t1,t2,ty = arith_conversion t1 t2 in
	    Tbinop (t1, Badd, t2), ty
	| (Tpointer _ | Tarray _), (Tint _ | Tenum _) -> 
	    Tbinop (t1, Badd, coerce c_exact_int t2), ty1
	| (Tenum _ | Tint _), (Tpointer _ | Tarray _) ->
	    Tbinop (coerce c_exact_int t2, Badd, t1), ty2
	| _ -> 
	    error loc "invalid operands to binary +"
      end
  | PLbinop (t1, Bsub, t2) ->
      let t1 = type_term env t1 in
      let ty1 = t1.term_type in
      let t2 = type_term env t2 in
      let ty2 = t2.term_type in
      begin match ty1.ctype_node, ty2.ctype_node with
	| (Tenum _ | Tint _ | Tfloat _), (Tenum _ | Tint _ | Tfloat _) ->
	    let t1,t2,ty = arith_conversion t1 t2 in
	    Tbinop (t1, Bsub, t2),ty
	| (Tpointer _ | Tarray _), (Tint _ | Tenum _) -> 
	    let mt2 = { term_node = Tunop (Uminus, t2); 
			term_loc = t2.term_loc;
			term_type = ty2} in
	    Tbinop (t1, Badd, mt2), ty1
	| (Tpointer _ | Tarray _), (Tpointer _ | Tarray _) ->
	    Tbinop (t1, Bsub, t2), c_exact_int (* TODO check types *)
	| _ -> error loc "invalid operands to binary -"
      end
  | PLbinop (t1, (Bmul | Bdiv as op), t2) ->
      let t1 = type_num_term env t1 in
      let t2 = type_num_term env t2 in
      let t1,t2,ty = arith_conversion t1 t2 in
      Tbinop (t1, op, t2), ty
  | PLbinop (t1, (Bmod | Bbw_and | Bbw_or | Bbw_xor | 
		  Bshift_right | Bshift_left as op), t2) ->
      let t1 = type_int_term env t1 in
      let t2 = type_int_term env t2 in
      Tbinop (t1, op, t2), c_exact_int
  | PLbinop (t1, Bpow_real, t2) ->
      let t1 = type_real_term env t1 in
      let t2 = type_real_term env t2 in
      Tbinop (t1, Bpow_real, t2), c_real
  | PLdot (t, x) ->
      let t = type_term env t in
      let x = type_of_field loc x t.term_type in
      let t_dot_x = match t.term_node with
(*	| Tunop (Ustar, e) -> 
	    Tarrow (e, x)
	| Tarrget (e1, e2) -> 
	    let a = 
	      { term_node = Tbinop (e1, Badd, e2); 
		term_loc = t.term_loc;
		term_type = e1.term_type}
	    in
	    Tarrow (a, x)*)
	| _ -> 
	    Tdot (t, x)
      in
      t_dot_x, x.var_type
  | PLarrow (t, x) ->
      let t = type_term env t in
      begin match t.term_type.ctype_node with
	| Tpointer(_, ty) -> 
	    let x = type_of_field loc x ty in
	    Tarrow (t, x), x.var_type
	| _ -> 
	    error loc "invalid type argument of `->'"
      end
  | PLarrget (t1, t2) ->
      let t1 = type_term env t1 in
      (match t1.term_type.ctype_node with
	 | Tarray (_,ty,_) | Tpointer(_, ty) ->
	     let t2 = type_int_term env t2 in
	     Tarrget (t1, t2), ty
	 | _ ->
	     error loc "subscripted value must be either array or pointer")
  | PLrange (t1, t2, t3) ->
      let t1 = type_term env t1 in
      (match t1.term_type.ctype_node with
	 | Tarray (_,ty,_) | Tpointer (_,ty) ->
	     let t2 = type_int_term_option env t2 in
	     let t3 = type_int_term_option env t3 in
	     Trange (t1, t2, t3), ty
	 | _ ->
	     error loc "subscripted value must be either array or pointer")
  | PLif (t1, t2, t3) ->
      let tt1 = type_term env t1 in
      expected_num_pointer t1.lexpr_loc tt1;
      let t2 = type_term env t2 in
      let t3 = type_term env t3 in
      expected_term_type loc t3 t2.term_type;
      expected_term_type loc t2 t3.term_type;
      Tif (tt1, t2, t3), t2.term_type
  | PLold t ->
      let t = type_term env t in
      Told t, t.term_type
  | PLat (t, l) ->
      (* TODO check label l *)
      let t = type_term env t in
      Tat (t, l), t.term_type
  | PLbase_addr t ->
      let t = type_term env t in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Tbase_addr t, c_addr
	 | _ -> error loc "base_addr argument must be either array or pointer")
  | PLoffset t ->
      let t = type_term env t in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Toffset t, c_exact_int
	 | _ -> error loc "offset argument must be either array or pointer")
  | PLblock_length t ->
      let t = type_term env t in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Tblock_length t, c_exact_int
	 | _ -> error loc "block_length argument must be either array or pointer")
  | PLarrlen t ->
      let t = type_term env t in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Tarrlen t, c_exact_int
	 | _ -> error loc "arrlen argument must be either array or pointer")
  | PLstrlen t ->
      let t = type_term env t in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Tstrlen t, c_exact_int
	 | _ -> error loc "strlen argument must be either array or pointer")
  | PLmin (t1,t2) ->
      let t1 = type_int_term env t1 in
      let t2 = type_int_term env t2 in
      Tmin (t1,t2), c_exact_int
  | PLmax (t1,t2) ->
      let t1 = type_int_term env t1 in
      let t2 = type_int_term env t2 in
      Tmax (t1,t2), c_exact_int
  | PLminint ty ->
      let ty = type_logic_type ~machine_ints:true loc env ty in
      begin match ty.ctype_node with
	| Tint (_, (Char | Short | Ctypes.Int | Long | LongLong)) -> 
	    Tminint ty, c_exact_int
	| _ -> 
	    error loc "argument must be a C integer type"
      end
  | PLmaxint ty ->
      let ty = type_logic_type ~machine_ints:true loc env ty in
      begin match ty.ctype_node with
	| Tint (_, (Char | Short | Ctypes.Int | Long | LongLong)) -> 
	    Tmaxint ty, c_exact_int
	| _ -> 
	    error loc "argument must be a C integer type"
      end
  | PLresult ->
      (try 
	 let t = Env.find "result" env in 
	 begin match t with
	   | Var_info v -> Clogic.Tvar v, v.var_type
	   | Fun_info f -> 
               error loc ("result is a function")
	 end
       with Not_found -> error loc "\\result meaningless")
  | PLnull ->
      let info = default_var_info "null" in 
      Cenv.set_var_type (Var_info info) (c_void_star Not_valid) false;
      Clogic.Tvar info, (c_void_star Not_valid)
  | PLcast (ty, t) ->
      let t = type_term env t in
      let tt = t.term_type in
      begin match ty, tt.ctype_node with
	| LTvoid, Tvoid -> 
	    t.term_node, tt
	| (LTchar _ | LTshort _ | LTint _ | LTlong _ | LTlonglong _ | LTinteger
	  | LTfloat | LTdouble | LTlongdouble | LTreal), 
	  (Tenum _ | Tint _ | Tfloat _) -> 
	    let t = coerce (type_logic_type loc env ty) t in
	    t.term_node, t.term_type
	| _ -> 
	    warning loc "ignored cast in annotation"; t.term_node, tt
      end
  | PLvalid _ | PLvalid_index _ | PLvalid_range _ | PLfresh _ | PLseparated _ 
  | PLexists _ | PLforall _ | PLimplies _ | PLiff _ | PLfalse
  | PLfullseparated _ | PLor _ | PLand _ | PLrel _ | PLtrue | PLnamed _ 
  | PLbound_separated _ | PLfull_separated _ ->
      raise_located loc (AnyMessage "predicates are not allowed here")

and type_int_term env t =
  let tt = type_term env t in
  expected_int t.lexpr_loc tt;
  coerce c_exact_int tt

and type_real_term env t = 
  let tt = type_num_term env t in
  coerce c_real tt

and type_float_term env t = 
  let tt = type_num_term env t in
  match tt.term_type.ctype_node with
    | Tfloat (Ctypes.Float | Double | LongDouble) -> tt
    | _ -> error t.lexpr_loc "illegal operand (expected float)"

and type_int_term_option env = function
  | None -> None 
  | Some t -> Some (type_int_term env t)

and type_num_term env t =
  let tt = type_term env t in
  expected_num t.lexpr_loc tt;
  tt

and type_num_pointer_term env t =
  let tt = type_term env t in
  expected_num_pointer t.lexpr_loc tt;
  tt

and type_terms loc env at tl =
  let rec type_list = function
    | [], [] -> 
	[]
    | et :: etl, ({lexpr_loc=tloc} as t) :: tl ->
	let t = type_term env t in
	expected_term_type tloc t et;
  	coerce et t :: type_list (etl, tl)
    | [], _ ->
	raise_located loc TooManyArguments
    | _, [] ->
	raise_located loc PartialApp
  in
  type_list (at, tl)


(* ghost *)

let rec type_ghost_lvalue env t =
  let t', ty =   type_ghost_lvalue_node t.lexpr_loc env t.lexpr_node in
  { term_node = t'; term_loc = t.lexpr_loc; term_type = ty}

and type_ghost_lvalue_node loc env t =
  match t with
    | PLvar x ->
	let v = 
	  try find_ghost x.var_name
	  with Not_found -> 
            error loc "unbound ghost variable %s" x.var_name
	in 
	Clogic.Tvar v, v.var_type
    | PLarrget (t1, t2) ->
	let t1 = type_ghost_lvalue env t1 in
	(match t1.term_type.ctype_node with
	   | Tarray (_,ty,_) | Tpointer (_,ty) ->
	       let t2 = type_int_term env t2 in
	       Tarrget (t1, t2), ty
	   | _ ->
	       error loc "subscripted value is neither array nor pointer")
    | _ ->
	error loc "not allowed as ghost left value"




(* Typing logic types *)

let rec type_type env t = 
  { t with ctype_node = type_type_node env t.ctype_node }

and type_type_node env = function
  | Tint _ | Tfloat _ as t -> t
  | Tarray (valid,ty,t) -> Tarray (valid,type_type env ty,t)
  | _ -> assert false


(*
let type_quantifier env (ty, x) = (type_logic_type env ty, x)
let type_quantifiers env = List.map (type_quantifier env)
*)

let add_quantifiers loc q env =
  let (tq,env) =
    List.fold_left
      (fun (tq,env) (ty, x) -> 
	 let i = Info.default_var_info x 
	 and ty = type_logic_type loc env ty in
	 ((ty,i)::tq, Env.add x ty (Var_info i) env))
      ([],env) q
  in
  (List.rev tq,env)


let int_constant n = 
  { term_node = Tconstant (IntConstant n); 
    term_loc = Loc.dummy_position;
    term_type = c_exact_int}

let zero = int_constant "0"

let compat_pointers ty1 ty2 = 
  (ty1.ctype_node = Tvoid) || (ty2.ctype_node = Tvoid) || eq_type ty1 ty2

(* Typing predicates *)

let rec type_term env t =
  let t', ty = type_term_node t.lexpr_loc env t.lexpr_node in
  { term_node = t'; term_loc = t.lexpr_loc; term_type = ty}


let rec type_predicate env p0 = 
  let p' = type_predicate_node env p0 in
  { pred_node = p'; pred_loc = p0.lexpr_loc }

and type_predicate_node env p0 = match p0.lexpr_node with
  | PLfalse -> Pfalse
  | PLtrue -> Ptrue
  | PLrel ({lexpr_node = PLrel (_, _, t2)} as p1, op, t3) ->
      let p1 = type_predicate env p1 in
      let p2 = { lexpr_node = PLrel (t2, op, t3); 
		 lexpr_loc = p0.lexpr_loc } in
      let p2 = type_predicate env p2 in
      Pand (p1, p2)
  | PLrel (t1, (Lt | Le | Gt | Ge as r), t2) -> 
      let loc = Loc.join t1.lexpr_loc t2.lexpr_loc in
      let t1 = type_num_pointer_term env t1 in
      let t2 = type_num_pointer_term env t2 in
       begin match t1.term_type.ctype_node, t2.term_type.ctype_node with
	| (Tint _ | Tenum _ | Tfloat _), (Tint _ | Tenum _ | Tfloat _) ->
	    let t1,t2,_ = arith_conversion t1 t2 in
	    Prel (t1, r, t2)
	| (Tpointer (_,ty1)  | Tarray (_,ty1,_)), 
	  (Tpointer (_,ty2) | Tarray (_,ty2,_)) ->
	    if not (compat_pointers ty1 ty2) then
	      warning loc "comparison of distinct pointer types lacks a cast";
	    Prel (t1, r, t2)
	| (Tpointer _  | Tarray _), (Tint _ | Tenum _ | Tfloat _)
	| (Tint _ | Tenum _ | Tfloat _), (Tpointer _  | Tarray _) ->
	    error loc "comparison between pointer and integer" (* C warning *)
	    (* Prel (t1, r, t2) *)
	| _ ->
	    error loc "invalid operands to comparison"
       end
  | PLrel (t1, (Eq | Neq as r), t2) -> 
      let loc = Loc.join t1.lexpr_loc t2.lexpr_loc in
      let t1 = type_term env t1 in
      let t2 = type_term env t2 in
       begin match t1.term_type.ctype_node, t2.term_type.ctype_node with
	| (Tint _ | Tenum _ | Tfloat _), (Tint _ | Tenum _ | Tfloat _) ->
	    let t1,t2,_ = arith_conversion t1 t2 in
	    Prel (t1, r, t2)
	| (Tpointer (_,ty1)  | Tarray (_,ty1,_)), 
	  (Tpointer (_,ty2) | Tarray (_,ty2,_)) ->
	    if not (compat_pointers ty1 ty2) then
	      warning loc "comparison of distinct pointer types lacks a cast";
	    Prel (t1, r, t2)
	| (Tpointer _  | Tarray _), (Tint _ | Tenum _ | Tfloat _) ->
	    if t2.term_node = Tconstant (IntConstant "0") then
	      let ty1 = Tpointer ( Not_valid, t1.term_type) in
	      let ty1 = 
		{ ctype_node = ty1;
		  ctype_storage = No_storage;
		  ctype_const = false;
		  ctype_volatile = false;
		  ctype_ghost = false;
		}
	      in
	      Prel(t1, r, 
		   { term_node = Tcast(ty1,t2);
		     term_type = ty1;
		     term_loc  = t2.term_loc;})
	    else
	      error loc "comparison between pointer and integer" (* C warning *)
		(* Prel (t1, r, t2) *)
	| (Tint _ | Tenum _ | Tfloat _), (Tpointer _  | Tarray _) ->
	    if t1.term_node = Tconstant (IntConstant "0") then
	      let ty2 = Tpointer ( Not_valid, t2.term_type) in
	      let ty2 = 
		{ ctype_node = ty2;
		  ctype_storage = No_storage;
		  ctype_const = false;
		  ctype_volatile = false;
		  ctype_ghost = false;
		}
	      in
	      Prel({ term_node = Tcast(ty2,t1);
		     term_type = ty2;
		     term_loc  = t1.term_loc;},
		   r, t2)
	    else
	      error loc "comparison between pointer and integer" (* C warning *)
		(* Prel (t1, r, t2) *)
	| Tvar s1, Tvar s2 when s1 = s2 -> Prel (t1, r, t2)
	| _ ->
	    error loc "invalid operands to comparison"
      end
  | PLand (p1, p2) -> 
      Pand (type_predicate env p1, type_predicate env p2)
  | PLor (p1, p2) -> 
      Por (type_predicate env p1, type_predicate env p2)
  | PLimplies (p1, p2) -> 
      Pimplies (type_predicate env p1, type_predicate env p2) 
  | PLiff (p1, p2) -> 
      Piff (type_predicate env p1, type_predicate env p2) 
  | PLnot p -> 
      (match type_predicate env p with
	 | { pred_node = Prel (t, Neq, z) } when z == zero -> 
	     Prel (t, Eq, zero)
	 | p -> Pnot p)
  | PLapp (p, tl) ->
      (try
	 let pl,info = find_pred p.logic_name in
	 let tl = type_terms p0.lexpr_loc env pl tl in
	 Papp (info, tl)
       with Not_found -> 
	 error p0.lexpr_loc "unbound predicate %s" p.logic_name)
  | PLif (t, p1, p2) -> 
      let t = type_int_term env t in
      Pif (t, type_predicate env p1, type_predicate env p2)
  | PLforall (q, p) -> 
      let q, env' = add_quantifiers p0.lexpr_loc q env in
      Pforall (q, type_predicate env' p)
  | PLexists (q, p) -> 
      let q, env' = add_quantifiers p0.lexpr_loc q env in
      Pexists (q, type_predicate env' p)
  | PLfresh (t) ->
      let tloc = t.lexpr_loc in
      let t = type_term env t in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Pfresh(t)
	 | _ -> error tloc "subscripted value is neither array nor pointer")
  | PLseparated (t1,t2) ->
      let t1loc = t1.lexpr_loc in
      let t1 = type_term env t1 in 
      let t2loc = t2.lexpr_loc in
      let t2 = type_term env t2 in
      Pseparated (
      (match t1.term_type.ctype_node with
	 | Tstruct _ | Tarray _ | Tpointer _ -> t1
	 | _ -> error t1loc "subscripted value is neither array nor pointer"),
	(match t2.term_type.ctype_node with
	 | Tstruct _ | Tarray _ | Tpointer _ -> t2
	 | _ -> error t2loc "subscripted value is neither array nor pointer"))
  | PLbound_separated (t1,tu1,t2,tu2) ->
      let t1loc = t1.lexpr_loc in
      let t1 = type_term env t1 in 
      let tu1 = type_int_term env tu1 in
      let t2loc = t2.lexpr_loc in
      let t2 = type_term env t2 in
      let tu2 = type_int_term env tu2 in
      Pbound_separated (
      (match t1.term_type.ctype_node with
	 | Tstruct _ | Tarray _ | Tpointer _ -> t1
	 | _ -> error t1loc "subscripted value is neither array nor pointer"),
	tu1,
	(match t2.term_type.ctype_node with
	 | Tstruct _ | Tarray _ | Tpointer _ -> t2
	 | _ -> error t2loc "subscripted value is neither array nor pointer"),
	tu2)
  | PLfull_separated (t1,t2) ->
      let t1loc = t1.lexpr_loc in
      let t1 = type_term env t1 in 
      let t2loc = t2.lexpr_loc in
      let t2 = type_term env t2 in
      Pfull_separated (
      (match t1.term_type.ctype_node with
	 | Tstruct _ | Tarray _ | Tpointer _ -> t1
	 | _ -> error t1loc "subscripted value is neither array nor pointer"),
	(match t2.term_type.ctype_node with
	 | Tstruct _ | Tarray _ | Tpointer _ -> t2
	 | _ -> error t2loc "subscripted value is neither array nor pointer"))
  | PLfullseparated (t1,t2) ->
      let t1loc = t1.lexpr_loc in
      let t1 = type_term env t1 in 
      let t2loc = t2.lexpr_loc in
      let t2 = type_term env t2 in
      Pfullseparated (
      (match t1.term_type.ctype_node with
	 | Tstruct _ -> t1
	 | _ -> error t1loc "subscripted value is neither array nor pointer"),
	(match t2.term_type.ctype_node with
	 | Tstruct _ -> t2
	 | _ -> error t2loc "subscripted value is neither array nor pointer"))
  | PLvalid (t) ->
      let tloc = t.lexpr_loc in
      let t = type_term env t in
      (match t.term_type.ctype_node with
	 | Tstruct _ | Tunion _
	 | Tarray _ | Tpointer _ -> Pvalid(t)
	 | _ -> error tloc "subscripted value is neither array nor pointer")
  | PLvalid_index (t,a) ->
      let tloc = t.lexpr_loc in
      let t = type_term env t in
      let a = type_int_term env a in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Pvalid_index(t,a)
	 | _ -> error tloc "subscripted value is neither array nor pointer")
  | PLvalid_range (t,a,b) ->
      let tloc = t.lexpr_loc in
      let t = type_term env t in
      let a = type_int_term env a in
      let b = type_int_term env b in
      (match t.term_type.ctype_node with
	 | Tarray _ | Tpointer _ -> Pvalid_range(t,a,b)
	 | _ -> error tloc "subscripted value is neither array nor pointer")
  | PLold p ->
      Pold (type_predicate env p)
  | PLat (p, l) ->
      (* TODO check label l *)
      Pat (type_predicate env p, l)
  | PLcast _ | PLblock_length _ | PLarrlen _ | PLstrlen _ 
  | PLbase_addr _ | PLoffset _ | PLarrget _ | PLarrow _ 
  | PLdot _ | PLbinop _ | PLunop _ | PLconstant _ | PLvar _ | PLnull 
  | PLresult | PLrange _ | PLmin _ | PLmax _ | PLminint _ | PLmaxint _ ->
      (*raise (Stdpp.Exc_located (p0.lexpr_loc, Parsing.Parse_error))*)
      (* interpret term [t] as [t != 0] *)
      let t = type_int_term env p0 in
      Prel (t, Neq, zero)
  | PLnamed (n, p) ->
      Pnamed (n, type_predicate env p)

let type_variant env = function 
  | (t, None) -> (type_int_term env t, None)
  | (t, r) -> (type_term env t, r)

let type_location = type_term

let type_loop_annot env la =
  { invariant = option_app (type_predicate env) la.invariant;
    assume_invariant = option_app (type_predicate env) la.assume_invariant;
    loop_assigns = 
      option_app (fun (loc,l) -> loc, List.map (type_location env) l) 
	la.loop_assigns;
    variant = option_app (type_variant env) la.variant }

let type_spec result env s = 
  let p = option_app (type_predicate env) s.requires in
  let env' = match result with
    | None -> env
    | Some ty ->
	let v = Var_info (Info.default_var_info "result") in
	Cenv.set_var_type v ty true;
	Env.add "result" ty v env
  in
  let q = option_app (type_predicate env') s.ensures in
  let v = option_app (type_variant env) s.decreases in
  let m = option_app (fun (loc,l) -> loc, List.map (type_location env) l) s.assigns in
  { requires = p;
    assigns = m;
    ensures = q;
    decreases = v }
