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

(*i $Id: ctyping.ml,v 1.156 2008/06/05 15:14:26 marche Exp $ i*)

open Format
open Coptions
open Clogic
open Cast
open Cerror
open Cltyping
open Creport
open Info
open Cenv
open Lib
open Ctypes
open Int64


let int_teconstant n = 
  { texpr_node = TEconstant (IntConstant n); 
    texpr_loc = Loc.dummy_position;
    texpr_type = c_exact_int (* ? *) }

let tezero = int_teconstant "0"

let char_size_b = char_size / 8
let short_size_b = short_size / 8
let int_size_b = Coptions.int_size / 8
let long_size_b = long_size / 8
let long_long_size_b = long_long_size / 8

(* evaluation static *)
let rec sizeof loc = 
  let incomplete_type () = 
    error loc "invalid application of `sizeof' to an incomplete type"
  in
  let warned = ref false in
  let architecture () =
    if not !warned then begin 
      warned := true;
      warning loc "compiler/architecture-dependent sizeof"
    end
  in
  let rec sizeof ?(field=false) (ty : tctype) = match ty.ctype_node with
    | Tvoid -> of_int 1
    | Tint (_, Char) -> of_int char_size_b
    | Tint (_, Short) -> of_int short_size_b
    | Tint (_, Int) -> architecture (); of_int int_size_b
    | Tint (_, Long) -> of_int long_size_b
    | Tint (_, LongLong) -> of_int long_long_size_b
    | Tint (_, Bitfield n) -> 
	architecture ();
	let d = div n (of_int 8) in
	if rem n (of_int 8) = zero then d else succ d
    | Tint (_, ExactInt) -> assert false
    | Tfloat Float -> of_int 4
    | Tfloat Double -> of_int 8
    | Tfloat LongDouble -> of_int 12
    | Tfloat Real -> assert false
    | Tvar x -> assert false (* should be expansed *)
    | Tarray (_,ty, Some e) -> 
	mul e (sizeof ty)
    | Tarray (_,ty, None) -> if field then of_int 0 else of_int 1
    | Tpointer _ -> of_int 4
    | Tstruct n ->
	(match tag_type_definition n with
	   | TTIncomplete -> 
	       incomplete_type ()
	   | TTStructUnion (Tstruct _, fl) -> 
	       List.fold_left 
		 (fun s v -> add s (sizeof ~field:true v.var_type)) 
		 (of_int 0) fl
	   | _ -> 
	       assert false)
    | Tunion n ->
	(match tag_type_definition n with
	   | TTIncomplete -> 
	       incomplete_type ()
	   | TTStructUnion (Tunion _, fl) -> 
	       List.fold_left 
		 (fun s v -> max s (sizeof ~field:true v.var_type)) 
		 (of_int 0) fl
	   | _ -> 
	       assert false)
    | Tenum _ -> of_int 4
    | Tfun _ -> of_int 4 (* function pointer? *)
  in
  sizeof

and eval_const_expr_noerror (e : texpr) = match e.texpr_node with
  | TEconstant (IntConstant c) -> Cconst.int e.texpr_loc c
  | TEunary (Uplus, t) -> eval_const_expr_noerror t
  | TEunary (Cast.Uminus, t) -> Int64.neg (eval_const_expr_noerror t)
  | TEbinary (t1, Cast.Badd_int _, t2) -> 
      Int64.add (eval_const_expr_noerror t1)  (eval_const_expr_noerror t2)
  | TEbinary (t1, Cast.Bsub_int _, t2) -> 
      Int64.sub (eval_const_expr_noerror t1)  (eval_const_expr_noerror t2)
  | TEbinary (t1, Cast.Bmul_int _, t2) -> 
      Int64.mul (eval_const_expr_noerror t1)  (eval_const_expr_noerror t2)
  | TEbinary (t1, Cast.Bdiv_int _, t2) -> 
      Int64.div (eval_const_expr_noerror t1)  (eval_const_expr_noerror t2)  
  | TEbinary (t1, Cast.Bdiv_float _, t2) -> 
      invalid_arg "not a constant expression 2"
  | TEcast (_, e) -> eval_const_expr_noerror e
  | TEunary (Uint_conversion, e) -> eval_const_expr_noerror e
  | TEsizeof (t,n) -> n
  | TEvar (Var_info v) ->
      if e.texpr_type.Ctypes.ctype_const 
      then v.enum_constant_value
      else invalid_arg "not a const variable"
  | _ -> invalid_arg "not a constant expression"

and eval_const_expr (e : texpr) = 
  try
    eval_const_expr_noerror e
  with
      Invalid_argument msg -> error e.texpr_loc "%s" msg

(* Typing C programs *)

let located_app f x = { node = f x.node; loc = x.loc }

let type_spec ?result env s = type_spec result env s


(*s Some predefined types, subtype relation, etc. *)

let int_op i = function
  | Badd -> Badd_int i
  | Bsub -> Bsub_int i
  | Bmul -> Bmul_int i
  | Bdiv -> Bdiv_int i
  | Bmod -> Bmod_int i
  | Blt -> Blt_int
  | Ble -> Ble_int
  | Bgt -> Bgt_int
  | Bge -> Bge_int
  | Beq -> Beq_int
  | Bneq -> Bneq_int
  | _ -> assert false

let float_op fk = function
  | Badd -> Badd_float fk
  | Bsub -> Bsub_float fk
  | Bmul -> Bmul_float fk
  | Bdiv -> Bdiv_float fk
  | Blt -> Blt_float fk
  | Ble -> Ble_float fk
  | Bgt -> Bgt_float fk
  | Bge -> Bge_float fk
  | Beq -> Beq_float fk
  | Bneq -> Bneq_float fk
  | _ -> assert false

let pointer_op = function
  | Blt -> Blt_pointer
  | Ble -> Ble_pointer
  | Bgt -> Bgt_pointer
  | Bge -> Bge_pointer
  | Beq -> Beq_pointer
  | Bneq -> Bneq_pointer
  | _ -> assert false

let type_op op ty = match ty.ctype_node with 
  | Tint i -> int_op i op 
  | Tenum _ -> int_op (Signed, Int) op
  | Tfloat fk -> float_op fk op 
  | Tpointer _ | Tarray _ -> pointer_op op
  | _ -> assert false 

let is_bitfield ty = match ty.ctype_node with
  | Tint (_, Bitfield _) -> true
  | _ -> false

let va_list = Ctypes.noattr (Ctypes.Tvar "va_list")
let _ = add_typedef Loc.dummy_position "__builtin_va_list" va_list

let rec is_null e = 
  match e.texpr_node with
    | TEconstant (IntConstant s) -> (try int_of_string s = 0 with _ -> false)
    | TEcast(_,e) -> is_null e
    | _ -> false

(* Coercions (ints to floats, floats to int) *)

let coerce ty e = match e.texpr_type.ctype_node, ty.ctype_node with
  | (Tint _ | Tenum _), Tfloat _ -> 
      { e with texpr_node = TEunary (Ufloat_of_int, e); texpr_type = ty }
  | Tfloat _, (Tint _ | Tenum _) ->
      { e with texpr_node = TEunary (Uint_of_float, e); texpr_type = ty }
  | Tfloat fk1, Tfloat fk2 when fk1 <> fk2 ->
      { e with texpr_node = TEunary (Ufloat_conversion, e); texpr_type = ty }
  | (Tint _ | Tenum _ as tn1), (Tint _ | Tenum _ as tn2) when tn1 <> tn2 ->
      { e with texpr_node = TEunary (Uint_conversion, e); texpr_type = ty }
  | ty1, ty2 when eq_type_node ty1 ty2 ->
      e
  | Tpointer _ , Tpointer (_,{ ctype_node = Tvoid }) ->
      e
  | _, Tpointer (_,ty) when is_null e ->
      let ty' = { ctype_node = Tpointer (Not_valid, ty);
		  ctype_storage = No_storage;
		  ctype_const = false;
		  ctype_volatile = false;
		  ctype_ghost = false } 
      in
      { e with texpr_node = TEcast (ty', tezero); texpr_type = ty' }
  | _ ->
      error e.texpr_loc
	"incompatible type: expected %a, found %a@." 
	print_type ty print_type e.texpr_type 

let compat_pointers ty1 ty2 = 
  (ty1.ctype_node = Tvoid) || (ty2.ctype_node = Tvoid) || eq_type ty1 ty2

let max_float = function
  | Float, Float -> Float
  | (Float | Double), (Float | Double) -> Double
  | (Float | Double | LongDouble), (Float | Double | LongDouble) -> LongDouble
  | _ -> assert false

(****
let rec le_cinteger = function
  | Tint (Signed, i1), Tint (_, i2) 
  | Tint (Unsigned, i1), Tint (Unsigned, i2) ->
      Cenv.int_size i1 <= Cenv.int_size i2
  | Tint (Unsigned, i1), Tint (Signed, i2) ->
      int_size i1 < int_size i2
  (* enum = int TODO: could be unsigned int *)
  | Tenum _, ty2 ->
      le_cinteger (Tint (Signed, Int), ty2)
  | ty1, Tenum _ ->
      le_cinteger (ty1, Tint (Signed, Int))
  | _ -> 
      assert false

let max_int i1 i2 = if le_cinteger (i1, i2) then i2 else i1
****)

let integral_promotion = function
  | Tenum _ | Tint (_, (Char | Short)) -> Tint (Signed, Int)
  | tn -> tn

(* convert [e1] and [e2] to the same arithmetic type *)
let conversion e1 e2 =
  let ty1 = e1.texpr_type in
  let ty2 = e2.texpr_type in
  match ty1.ctype_node, ty2.ctype_node with
    | Tfloat _, (Tint _ | Tenum _) -> e1, coerce ty1 e2, ty1
    | (Tint _ | Tenum _), Tfloat _ -> coerce ty2 e1, e2, ty2
    | Tfloat fk1, Tfloat fk2 -> 
	let ty = noattr (Tfloat (max_float (fk1, fk2))) in
	coerce ty e1, coerce ty e2, ty
    | (Tint _ | Tenum _ as tn1), (Tint _ | Tenum _ as tn2) -> 
	let tn = match integral_promotion tn1, integral_promotion tn2 with
	  | (Tint (Unsigned, LongLong) as tn), _ -> tn
	  | _, (Tint (Unsigned, LongLong) as tn) -> tn
	  | (Tint (Signed, LongLong) as tn), _ -> tn
	  | _, (Tint (Signed, LongLong) as tn) -> tn
	  | (Tint (Unsigned, Long) as tn), _ -> tn
	  | _, (Tint (Unsigned, Long) as tn) -> tn
	  | Tint (Signed, Long), Tint (Unsigned, Int) 
	  | Tint (Unsigned, Int), Tint (Signed, Long)
	    when Coptions.long_size <= Coptions.int_size -> 
	      Tint (Unsigned, Long)
	  | (Tint (Signed, Long) as tn), _ -> tn
	  | _, (Tint (Signed, Long) as tn) -> tn
	  | (Tint (Unsigned, Int) as tn), _ -> tn
	  | _, (Tint (Unsigned, Int) as tn) -> tn
	  | (Tint (Signed, Int) as tn), _ -> tn
	  | _, (Tint (Signed, Int) as tn) -> tn
	  | _ -> assert false
	in
	let ty = noattr tn in
	coerce ty e1, coerce ty e2, ty
    | _ -> assert false

let float_constant_type s =
  let n = String.length s in
  assert (n >= 1);
  match s.[n-1] with
    | 'f' | 'F' -> String.sub s 0 (n-1), Float
    | 'l' | 'L' -> String.sub s 0 (n-1), LongDouble
    | _ -> s, Double

let has_suffix ~suf s =
  let nsuf = String.length suf in
  let n = String.length s  in
  n >= nsuf && String.lowercase (String.sub s (n - nsuf) nsuf) = suf

let int_constant_type s =
  if has_suffix ~suf:"ul" s || has_suffix ~suf:"lu" s then
    Unsigned, Long
  else if has_suffix ~suf:"u" s then
    Unsigned, Int
  else if has_suffix ~suf:"l" s then
    Signed, Long
  else
    (* TODO: cf K&R *)
    Signed, Int

(* type the assignment of [e2] into a left value of type [ty1] *)
let type_assignment loc ty1 e2 =
  let ty2 = e2.texpr_type in
  if (arith_type ty1 && arith_type ty2) || 
     (sub_type ty2 ty1) || 
     (pointer_type ty1 && is_null e2) 
  then
    coerce ty1 e2
  else
    error loc "incompatible types in assignment"

(* warns for assigments over read-only left-value *)

let warn_for_read_only loc e = 
  let pointer_on_read_only ty = match ty.ctype_node with
    | Tpointer (_,ty) -> ty.ctype_const 
    | _ -> false
  in
  match e.texpr_node with
  | TEarrow (_, x) | TEdot (_, x) when e.texpr_type.ctype_const  ->
      warning loc "assignment of read-only member `%s'" x.var_name
  | TEarrow (e1, x) when pointer_on_read_only e1.texpr_type ->
      warning loc "assignment of read-only member `%s'" x.var_name
  | TEdot (e1, x) when e1.texpr_type.ctype_const ->
      warning loc "assignment of read-only member `%s'" x.var_name 
  | TEvar (Var_info x) when e.texpr_type.ctype_const ->
      warning loc "assignment of read-only variable `%s'" x.var_name 
  | TEvar (Var_info x) ->
      Loc.report_position Coptions.log loc;
      fprintf Coptions.log "Variable %s is assigned@." x.var_name;
      set_assigned x
  | TEvar (Fun_info f) ->
      warning loc ("function assignment (ignored)")
  | _ when e.texpr_type.ctype_const ->
      warning loc "assigment of read-only location"
  | _ -> 
      ()

let set_referenced e = match e.texpr_node with
  | TEvar (Var_info x) -> set_is_referenced x
  | TEdot (_,f) | TEarrow(_,f) -> set_is_referenced f
  | _ -> ()

let make_shift e1 e2 valid ty n =
  let is_valid =
      match valid, n with
	| Valid(a,b), Some n ->
	    if e1.texpr_type.ctype_ghost 
	    then Valid(Int64.min_int,Int64.max_int)
	    else
	      begin
	      assert (b = n);
		try
		  let i = eval_const_expr_noerror e2 in
		  Valid(Int64.sub a i, Int64.sub b i)
		with Invalid_argument _ -> Not_valid
	      end;
	| _, _ -> Not_valid
  in
  let ty1 = e1.texpr_type in
  TEbinary (e1, Badd_pointer_int, e2), 
  { ty1 with ctype_node = Tpointer(is_valid,ty) }

let remove_top_conversion e = match e.texpr_node with
  | TEunary (( Uint_conversion | Ufloat_conversion
	     | Uint_of_float | Ufloat_of_int), e) -> e
  | _ -> e

(*s Types *)

let rec type_type ?(ghost=false) ?(parameters=false) loc env ty = 
  { Ctypes.ctype_node = type_type_node ~ghost:ghost ~parameters:parameters loc env 
      ty;
    ctype_storage = ty.Cast.ctype_storage ;
    ctype_const = ty.Cast.ctype_const ;
    ctype_volatile = ty.Cast.ctype_volatile;
    ctype_ghost = false;
  }

and type_type_node ?(ghost=false) ?(parameters=false) loc env ty = 
  match ty.Cast.ctype_node with
  | CTvoid -> Tvoid
  | CTfloat x -> 
      begin match x with Real -> () | _ -> use_floats := true end; Tfloat x
  | CTint (s,i) ->
      Tint (s, type_integer loc env i)
  | CTvar x -> 
      (* TODO: les attributs sont perdus *)
      (* Cas d'erreur associés observés en pratique : 
            -> utilisation d'un type non déclarer (pour déclarer une variable ghost 
               ou la signature d'une fonction logique)
            -> déclaration d'une variable ghost avec un type logique *)
      (try (find_typedef x).ctype_node with Not_found -> assert false)
  | CTarray (tyn, None) ->
      Tarray (Not_valid,type_type loc env tyn, None)
  | CTarray (tyn, Some e) ->
      if ghost 
      then 
	Tarray(Valid(Int64.min_int,Int64.max_int),type_type loc env tyn,None)
      else begin
	  if parameters 
	  then Tarray (Not_valid,type_type loc env tyn, None)
	  else
	    let n = eval_const_expr  (type_int_expr env e) in
	    Tarray (Valid(Int64.zero,n),type_type loc env tyn, Some n)
	end
  | CTpointer tyn -> 
      if ghost then Tpointer (Not_valid,type_type loc env tyn)
      else
	Tpointer (Not_valid,type_type loc env tyn)
  | CTstruct (x,Tag) -> Env.find_tag_type loc env (Tstruct x)  
  | CTunion (x,Tag)  -> Env.find_tag_type loc env (Tunion x)
  | CTenum (x,Tag)  -> Env.find_tag_type loc env (Tenum x)
  | CTstruct (x, Decl fl) ->
      let fl = List.map (type_field x loc env) fl in
      let tyn = 
	Env.set_struct_union_type loc env (Tstruct x) (List.map snd fl) in
      declare_fields tyn fl;
      tyn
  | CTunion (x, Decl fl) ->
      let fl = List.map (type_field x loc env) fl in
      let tyn = Env.set_struct_union_type loc env (Tunion x) (List.map snd fl) in
      declare_fields tyn fl;
      tyn
  | CTenum (x, Decl fl) ->
      let tyn = Env.find_tag_type loc env (Tenum x) in
      let ty = noattr tyn in
      let ty = { ty with ctype_const = true } in
      let rec enum_fields v = function
	| [] -> 
	    []
	| (f, op) :: fl ->
	     let i = default_var_info f in
	     ignore (add_sym loc f ty (Var_info i)); 
	     let v = match op with
	       | None -> 
		   v
	       | Some e -> 
		   let e = type_int_expr env e in
		   eval_const_expr e
	     in
	     set_const_value i v;
	     (i, v) :: enum_fields (Int64.succ v) fl
      in
      let fl = enum_fields Int64.zero fl in
      Env.set_enum_type loc env (Tenum x) fl 
  | CTfun (pl, tyn) ->
      let pl = List.map (fun (ty,x) -> type_type loc env ty) pl in
      let pl = match pl with
	| [{ctype_node = Tvoid}] -> []
	| _ -> pl
      in
      Tfun (pl, type_type loc env tyn)

and type_integer loc env = function
  | Cast.Char -> Char
  | Cast.Short -> Short
  | Cast.Int -> Int 
  | Cast.Long -> Long
  | Cast.LongLong -> LongLong
  | Cast.Bitfield e -> 
      let e = type_int_expr env e in
      Bitfield (eval_const_expr e)

(*s Expressions *)

and type_expr env e = 
  let tn,ty = type_expr_node e.loc env e.node in
  { texpr_node = tn; texpr_type = ty; texpr_loc = e.loc }

and type_expr_node loc env = function
  | CEnop ->
      TEnop, c_void
  | CEconstant (IntConstant s as c) ->
      let ik = int_constant_type s in
      TEconstant c, noattr (Tint ik)
  | CEconstant (RealConstant s) ->
      use_floats := true;
      let s,fk = float_constant_type s in
      TEunary (Ufloat_conversion,
	       { texpr_node = TEconstant (RealConstant s);
		 texpr_loc = loc; texpr_type = c_real }), 
      c_float fk
  | CEstring_literal s ->
      TEstring_literal s, 
      c_string (Valid(Int64.zero,Int64.of_int (1 + String.length s)))
  | CEvar x ->
      let var =
	try Env.find x env with Not_found -> 
	  try find_sym x with Not_found -> 
	    error loc "%s undeclared" x
      in 
      (TEvar var,var_type var)
  | CEdot (e, x) ->
      let te = type_expr env e in
      let x = type_of_field loc x te.texpr_type in
      let te_dot_x = 
(*	match te.texpr_node with
	| TEunary (Ustar, e) -> TEarrow (e, x)
	| TEarrget (e1, e2) -> 
	    let a = 
	      { te with 
		  texpr_node = TEbinary (e1, Badd_pointer_int, e2);
		  texpr_type = e1.texpr_type }
	    in
	    TEarrow (a, x)
  | _ -> *) TEdot (te, x)
      in
      te_dot_x, x.var_type
  | CEarrow (e, x) ->
      let te = type_expr env e in
      begin match te.texpr_type.ctype_node with
	| Tarray (_,ty,_) | Tpointer (_,ty) ->
	    let x = type_of_field loc x ty in
	    TEarrow (te, x), x.var_type
	| _ -> 
	    error loc "invalid type argument of `->'"
      end
  | CEarrget (e1, e2) ->
      let te1 = type_expr env e1 in
      (match te1.texpr_type.ctype_node with
	 | Tarray (_,ty,_) | Tpointer (_,ty) ->
	     let te2 = type_int_expr env e2 in
	     TEarrget (te1, te2), ty
	 | _ ->
	     error loc "subscripted value is neither array nor pointer")
  | CEseq (e1, e2) ->
      let e1 = type_expr env e1 in
      let e2 = type_expr env e2 in
      TEseq (e1, e2), e2.texpr_type
  | CEcond (e1, e2, e3) ->
      let e1 = type_boolean env e1 in
      let e2 = type_expr env e2 in
      let ty2 = e2.texpr_type in
      let e3 = type_expr env e3 in
      let ty3 = e3.texpr_type in
      if sub_type ty2 ty3 then
	TEcond (e1, coerce ty3 e2, e3), ty3
      else if sub_type ty3 ty2 then
	TEcond (e1, e2, coerce ty2 e3), ty2
      else
	error loc "type mismatch in conditional expression"
  | CEassign (e1, e2) ->
      let e1_loc = e1.loc in
      let e1 = type_lvalue env e1 in
      warn_for_read_only e1_loc e1;
      let ty1 = e1.texpr_type in
      let e2 = type_expr env e2 in
      let e2 = type_assignment loc ty1 e2 in
      TEassign (e1, e2), ty1
  | CEassign_op (e1, ( Badd | Bsub | Bmul | Bdiv | Bmod 
		     | Bbw_and | Bbw_xor | Bbw_or 
		     | Bshift_left | Bshift_right as op), e2) ->
      let b = { node = CEbinary (e1, op, e2); loc = loc } in
      let b = type_expr env b in
      (match b.texpr_node with
	 | TEbinary (te1, op', te2) -> 
	     let te1 = remove_top_conversion te1 in
	     check_lvalue e1.loc te1;
	     warn_for_read_only e1.loc te1;
	     let ty1 = te1.texpr_type in
	     let _ = type_assignment loc ty1 b in
	     TEassign_op (te1, op', te2), ty1
	 | _ -> 
	     assert false)
  | CEassign_op _ ->
      assert false
  | CEincr (op, e) ->
      let e_loc = e.loc in
      let e = type_lvalue env e in
      warn_for_read_only e_loc e;
      begin match e.texpr_type.ctype_node with
	| Tenum _ | Tint _ | Tfloat _ -> 
            TEincr (op, e), e.texpr_type
	| Tpointer (_,ty) -> 
            TEincr (op, e), 
	    { e.texpr_type with ctype_node = Tpointer(Not_valid,ty) }
	| _ -> error loc "wrong type to {de,in}crement"
      end
  | CEunary (Unot, e) ->
      let e = type_boolean env e in
      TEunary (Unot, e), c_int
  | CEunary ((Uplus | Uminus as op), e) ->
      let e = type_expr env e in
      begin match e.texpr_type.ctype_node with
	| Tenum _ | Tint _ | Tfloat _ -> TEunary (op, e), e.texpr_type
	| _ -> error loc "wrong type argument to unary plus/minus"
      end
  | CEunary (Uamp, e) ->
      (* TODO: cas où e est une fonction *)
      let e = type_lvalue ~modifiable:false env e in
      let ty = e.texpr_type in
      if is_bitfield ty then error loc "cannot take address of bitfield";
      if ty.ctype_storage = Register then 
	warning loc "address of register requested";
      set_referenced e;
      TEunary (Uamp, e), noattr (Tpointer (Valid(Int64.zero,Int64.one),ty))
  | CEunary (Ustar, e) ->
      let e = type_expr env e in
      begin match e.texpr_type.ctype_node with
	| Tpointer (_,ty) | Tarray (_,ty,_) -> TEunary (Ustar, e), ty
	| _ -> error loc "invalid type argument of `unary *'"
      end
  | CEunary (Utilde, e) ->
      let e = type_int_expr env e in
      TEunary (Utilde, e), e.texpr_type
  (* these other unops cannot be built by the parser *)
  | CEunary (( Uint_of_float | Ufloat_of_int 
	     | Ufloat_conversion | Uint_conversion), _) ->
      assert false
  | CEbinary (e1, (Bmul | Bdiv as op), e2) ->
      let e1 = type_arith_expr env e1 in
      let e2 = type_arith_expr env e2 in
      let e1,e2,ty = conversion e1 e2 in
      let op = type_op op ty in 
      TEbinary (e1, op, e2), ty
  | CEbinary (e1, Bmod, e2) ->
      let e1 = type_int_expr env e1 in
      let e2 = type_int_expr env e2 in
      let e1,e2,ty = conversion e1 e2 in
      let op = type_op Bmod ty in 
      TEbinary (e1, op, e2), ty
  | CEbinary (e1, Badd, e2) ->
      let e1 = type_expr env e1 in
      let ty1 = e1.texpr_type in
      let e2 = type_expr env e2 in
      let ty2 = e2.texpr_type in
      begin match ty1.ctype_node, ty2.ctype_node with
	| (Tenum _ | Tint _ | Tfloat _), (Tenum _ | Tint _ | Tfloat _) ->
	    let e1,e2,ty = conversion e1 e2 in
	    TEbinary (e1, type_op Badd ty, e2), ty
	| (Tpointer (valid,ty)), (Tint _ | Tenum _) ->
	    make_shift e1 e2 valid ty None
	| (Tarray (valid,ty,n)), (Tint _ | Tenum _) ->
	    make_shift e1 e2 valid ty n
	| (Tint _ | Tenum _), (Tpointer(valid,ty)) -> 
	    make_shift e2 e1 valid ty None
	| (Tint _ | Tenum _), (Tarray(valid,ty,n)) -> 
	    make_shift e2 e1 valid ty n
	| _ -> error loc "invalid operands to binary +"
      end
  | CEbinary (e1, Bsub, e2) ->
      let e1 = type_expr env e1 in
      let ty1 = e1.texpr_type in
      let e2 = type_expr env e2 in
      let ty2 = e2.texpr_type in
      begin match ty1.ctype_node, ty2.ctype_node with
	| (Tint _ | Tenum _ | Tfloat _), (Tint _ | Tenum _ | Tfloat _) ->
	    let e1,e2,ty = conversion e1 e2 in
	    TEbinary (e1, type_op Bsub ty, e2), ty
	| Tpointer(valid,ty), (Tint _ | Tenum _) -> 
	    let me2 = { e2 with texpr_node = TEunary (Uminus, e2);
			  texpr_type = ty2 } in
	    make_shift e1 me2 valid ty None
	| Tarray(valid,ty,n), (Tint _ | Tenum _) -> 
	    let me2 = { e2 with texpr_node = TEunary (Uminus, e2);
			  texpr_type = ty2 } in
	    make_shift e1 me2 valid ty n
	| (Tpointer _ | Tarray _), (Tpointer _ | Tarray _) ->
	    TEbinary (e1, Bsub_pointer, e2), c_int (* TODO check types *)
	| _ -> error loc "invalid operands to binary -"
      end
  | CEbinary (e1, (Blt | Bgt | Ble | Bge | Beq | Bneq as op), e2) ->
      let e1 = type_expr env e1 in
      let ty1 = e1.texpr_type in
      let e2 = type_expr env e2 in
      let ty2 = e2.texpr_type in
      begin match ty1.ctype_node, ty2.ctype_node with
	| (Tint _ | Tenum _ | Tfloat _), (Tint _ | Tenum _ | Tfloat _) ->
	    let e1,e2,ty = conversion e1 e2 in
	    TEbinary (e1, type_op op ty, e2), c_int
	| (Tpointer (_,ty1)  | Tarray (_,ty1,_)), 
	  (Tpointer (_,ty2) | Tarray (_,ty2,_)) ->
	    if not (compat_pointers ty1 ty2) then
	      warning loc "comparison of distinct pointer types lacks a cast";
	    TEbinary (e1, pointer_op op, e2), c_int
	| (Tpointer _  | Tarray _), (Tint _ | Tenum _ | Tfloat _) ->
	    if e2.texpr_node = TEconstant (IntConstant "0") then
	      TEbinary(e1, pointer_op op, 
		       { texpr_node = TEcast(
			   { ctype_node = Tpointer ( Not_valid, ty1);
			     ctype_storage = No_storage;
			     ctype_const = false;
			     ctype_volatile = false;
			     ctype_ghost = false;
			   },e2);
			 texpr_type = ty1;
			 texpr_loc  = e2.texpr_loc;}), c_int
	    else
	      error loc "comparison between pointer and integer but not 0"
	| (Tint _ | Tenum _ | Tfloat _), (Tpointer _  | Tarray _) ->
	    if e1.texpr_node = TEconstant (IntConstant "0") then
	      TEbinary({texpr_node = TEcast(
			  { ctype_node = Tpointer ( Not_valid, ty2);
			    ctype_storage = No_storage;
			    ctype_const = false;
			    ctype_volatile = false;
			    ctype_ghost = false;
			  },e1);
			texpr_type = ty1;
			texpr_loc  = e1.texpr_loc;
		       }, pointer_op op,e2), c_int
	    else
	      error loc "comparison between pointer and integer but not 0";
	| Tvar t1, Tvar t2 when t1 = t2 && (op = Beq || op = Bneq) ->
	    TEbinary (e1, op, e2), c_int
	| _ ->
	    error loc "invalid operands to comparison"
      end
  | CEbinary (e1, (Band | Bor as op), e2) ->
      let e1 = type_boolean env e1 in
      let e2 = type_boolean env e2 in
      TEbinary (e1, op, e2), c_int
  | CEbinary (e1, ( Bbw_and | Bbw_xor | Bbw_or 
		  | Bshift_left | Bshift_right as op), e2) ->
      (* TODO: max types pour "&" "|" "^" ? *)
      let e1 = type_int_expr env e1 in
      let e2 = type_int_expr env e2 in
      TEbinary (e1, op, e2), e1.texpr_type
  (* these other binops cannot be built by the parser *)
  | CEbinary (_, (Bdiv_float _|Bmul_float _|Bsub_float _|Badd_float _
		 |Bmod_int _|Bdiv_int _|Bmul_int _|Bsub_int _|Badd_int _
		 |Blt_pointer|Bgt_pointer|Ble_pointer|Bge_pointer
		 |Bneq_pointer|Beq_pointer
		 |Bneq_float _|Beq_float _|Bge_float _|Ble_float _
		 |Bgt_float _|Blt_float _
		 |Bneq_int|Beq_int|Bge_int|Ble_int|Bgt_int|Blt_int
		 |Bsub_pointer|Badd_pointer_int), _) ->
      assert false
  | CEcast (typ, 
	    { node = 
		CEcall 
		  ({ node = CEvar "malloc" },
		   [ { node = 
			 ( CEsizeof ty
			 | CEbinary (_, Bmul, { node = CEsizeof ty })
			 | CEbinary ({ node = CEsizeof ty }, Bmul, _)) as e }
		   ])}) ->
      let ty = type_type loc env ty in
      let typ = type_type loc env typ in
      begin match typ.ctype_node with
	| Tpointer (_, ty') when eq_type ty' ty ->
	    let e = match e with
	      | CEbinary (e, _, {node = CEsizeof _}) 
	      | CEbinary ({node = CEsizeof _}, _, e) -> e
	      | _ -> { node = CEconstant (IntConstant "1"); loc = loc }
	    in
	    let e = type_int_expr env e in
	    let valid =
	      try
		let n = eval_const_expr_noerror e in
		Valid(Int64.zero,n)
	      with Invalid_argument _ -> Not_valid
	    in
	    TEmalloc (ty, e), { typ with ctype_node = Tpointer (valid, ty) }
	| _ -> error loc "incompatible types"
      end
  | CEcast (typ, 
	    { node = 
		CEcall 
		  ({ node = CEvar "calloc" },
		   [ e ; { node = CEsizeof ty }])}) ->
      let ty = type_type loc env ty in
      let typ = type_type loc env typ in
      begin match typ.ctype_node with
	| Tpointer (_, ty') when eq_type ty' ty ->
	    let e = type_int_expr env e in
	    let valid =
	      try
		let n = eval_const_expr_noerror e in
		Valid(Int64.zero,n)
	      with Invalid_argument _ -> Not_valid
	    in
	    TEmalloc (ty, e), { typ with ctype_node = Tpointer (valid, ty) }
	| _ -> 
	    error loc "incompatible types"
      end
  | CEcall (e, el) ->
      let e = type_expr env e in
      let el = List.map (type_expr env) el in
      begin match e.texpr_type.ctype_node with
	| Tfun (tl, ty) ->
	    let rec check_args i el' = function
	      | [], [] -> 
		  TEcall (e, List.rev el'), ty
	      | e :: el, t :: tl 
		  when compatible_type e.texpr_type t 
		    || (pointer_type t && is_null e) ->
		  check_args (i+1) (coerce t e :: el') (el, tl)
	      | e :: _, _ :: _ ->
		  error loc "incompatible type for argument %d" i
	      | [], _ :: _ ->
		  error loc "too few arguments"
	      | el, [] ->
		  (* too many arguments is OK *)
		  TEcall (e, List.rev_append el' el), ty
	    in
	    check_args 1 [] (el, tl)
	| _ ->
	    (* TODO should be: warning: implicit declaration of function *)
	    error loc "not a function"
      end
  | CEcast (ty, e) ->
      let ty = type_type loc env ty in
      let e = type_expr env e in
      TEcast (ty, e), ty
  | CEsizeof_expr e ->
      let e = type_expr env e in
      let n = sizeof e.texpr_loc e.texpr_type in
      TEsizeof (e.texpr_type,n), c_int
  | CEsizeof ty ->
      let ty = type_type loc env ty in
      let n = sizeof loc ty in
      TEsizeof(ty,n), c_int

and type_lvalue ?(modifiable=true) env e = 
  let loc = e.loc in
  let e = type_expr env e in
  check_lvalue ~modifiable loc e;
  e

and check_lvalue ?(modifiable=true) loc e = 
  match e.texpr_node with
    | TEvar v -> 
	begin 
	  match (var_type v).ctype_node with
	    | Tarray (_,_,Some _ ) when modifiable -> error loc "not a lvalue"
	    | _ -> ()
	end
    | TEunary (Ustar, _)
    | TEarrget _ 
    | TEarrow _ 
    | TEdot _ ->
	(* TODO: check that e is not of enum type *)
	()
    | TEcast (_, e) ->
	check_lvalue loc e
    | _ -> 
	error loc "invalid lvalue"

and type_expr_option env eo = Option_misc.map (type_expr env) eo

and type_field n loc env (ty, x, bf) = 
  let ty = type_type loc env ty in
  let bf = type_expr_option env bf in
  let info = find_field n x in
  match bf, ty.ctype_node with
    | _, Tvoid ->
	error loc "field `%s' declared void" x
    | None, _ ->
	(ty, info)
    | Some e, (Tenum _ | Tint _ as tyn) -> 
	let s = match tyn with
	  | Tenum _ -> Unsigned (* TODO: verif assez de bits pour l'enum *)
	  | Tint (s, Int) -> s
	  | Tint (s, _) ->
	      warning loc 
		"bit-field type `%a' invalid in ANSI C"
		Creport.print_type ty; 
	      s
	  | _ -> assert false
	in
	let v = eval_const_expr e in
	({ty with ctype_node = Tint (s, Bitfield v)}, info)
    | Some _, _ -> 
	error loc "bit-field `%s' has invalid type" x

(*s Typing of integers expressions: to be used when coercion is not allowed
    (array subscript, array size, enum value, etc.) *)

and type_int_expr_option env eo = Option_misc.map (type_int_expr env) eo

and type_int_expr env e = match type_expr env e with
  | { texpr_type = { ctype_node = Tint _ | Tenum _ } } as te -> te
  | _ -> error e.loc "invalid operand (expected integer)"

(*s Typing of arithmetic expressions: integers or floats *)

and type_arith_expr env e = match type_expr env e with
  | { texpr_type = { ctype_node = Tint _ | Tenum _ | Tfloat _ } } as te -> 
      te
  | _ -> error e.loc "invalid operand (expected integer or float)"

(*s Typing of ``boolean'' expressions *)

and type_boolean env e = 
  let e' = type_expr env e in
  let ty = e'.texpr_type in
  match ty.ctype_node with
    | Tint _ | Tenum _ | Tfloat _ | Tpointer _ | Tarray _ -> e'
    | _ -> error e.loc "invalid operand (expected arith or pointer)"

let int_to_init loc env ty i=
  (Iexpr 
     (coerce ty 
	(type_expr env 
	   { node = (CEconstant 
		       (IntConstant (sprintf "%d" i)));
	     loc = loc})))


(*s Typing of initializers *)

let rec type_initializer loc env ty = function
  | Iexpr e -> 
      let e = type_expr env e in
      Iexpr (coerce ty e)
  | Ilist el -> 
      (match ty.ctype_node with
	 | Tarray (_,ty,_) ->   
	     Ilist (List.map (type_initializer loc env ty) el)
	 | Tstruct (n) ->
	     (match tag_type_definition n with
		| TTIncomplete -> 
		    error loc "initializer but incomplete type"
		| TTStructUnion (Tstruct n, fl) -> 
		    Ilist (type_struct_initializer loc env fl el)
		| _ -> 
		    assert false)
	 | _ -> 
	     (match el with
		| [] -> error loc "empty initializer"
		| e :: el -> 
		    if el <> [] then 
		      warning loc "excess elements in initializer";
		    Ilist [type_initializer loc env ty e]))

and type_struct_initializer loc env fl el = match fl, el with
  | _, [] -> 
      []
  | v :: fl, e :: el ->
      let e = type_initializer loc env v.var_type e in
      e :: type_struct_initializer loc env fl el
  | [], _ ->
      error loc "excess elements in struct initializer"

and type_initializer_option loc env ty = 
  Option_misc.map (type_initializer loc env ty)

(*
let type_initializer_option loc env ty = function
  | None -> None
  | Some i -> Some (type_initializer loc env ty i)
*)

let array_size_from_initializer loc ty i = match ty.ctype_node, i with
  | Tarray (_,ety, None), Some (Ilist l) -> 
      let s = of_int (List.length l) in
      { ty with ctype_node = Tarray (Valid(Int64.zero,s),ety, Some s) }

  | Tarray (_,ety, None), None -> error loc "array size missing"  

  | Tarray (_,ety, None), Some (Iexpr e) ->
      (* since string literals in initializers are no longer transformed
	 in [Ilist] during typing, we need to add a special case here to
	 compute the length of the array. *)
      begin match e.texpr_node with
	| TEstring_literal s ->
	    let s = of_int (String.length s - 1) in
	    { ty with ctype_node = Tarray (Valid(Int64.zero,s),ety, Some s) }
	| _ -> ty
      end

  | _ -> 
      ty

(*s Statements *)

type status = { 
  break : bool; 
  continue : bool; 
  return : bool;
  term : bool
}

let mt_status = 
  { return = false; break = false; continue = false; term = true }

let or_status s1 s2 =
  { return = s1.return || s2.return;
    break = s1.break || s2.break;
    continue = s1.continue || s2.continue;
    term = s1.term || s2.term }

(* structure of labels in a function body : using Huet's zipper *)

type label_tree =
  | LabelItem of label_info
  | LabelBlock of label_tree list

let rec printf_label_tree fmt lt =
  match lt with 
    | LabelItem s -> fprintf fmt "%s" s.label_info_name
    | LabelBlock l -> 
	fprintf fmt "{ %a }" (Pp.print_list Pp.space printf_label_tree ) l

let rec in_label_tree lab = function
  | LabelItem l -> if l.label_info_name=lab then l else raise Not_found
  | LabelBlock l -> in_label_tree_list lab l

and in_label_tree_list lab = function
  | [] -> raise Not_found
  | h::r -> 
      try in_label_tree lab h
      with Not_found -> in_label_tree_list lab r

let rec in_label_upper_tree_list lab = function
  | [] -> raise Not_found
  | LabelItem l :: _ when l.label_info_name=lab -> l
  | _ :: r -> in_label_upper_tree_list lab r


let rec build_label_tree st acc : label_tree list =
  match st.node with
    | CSnop  
    | CSexpr _ 
    | CSbreak 
    | CScontinue 
    | CSgoto _
    | CSreturn _
    | CSannot _ -> acc
    | CSif (e, s1, s2) ->
	let l1 = build_label_tree s1 [] in
	let l2 = build_label_tree s2 [] in
	(LabelBlock l1) :: (LabelBlock l2) :: acc
    | CSlabel (lab, s) ->
	let info = { label_info_name = lab; times_used = 0 } in
	(LabelItem info) :: (build_label_tree s acc)
    | CSblock (_,sl) ->
	let l = List.fold_right build_label_tree sl [] in
	(LabelBlock l) :: acc
    | CSfor (_, _, _, _, s) 
    | CSdowhile (_, s, _) 
    | CSwhile (_, _, s) 
    | CSswitch (_, s) 
    | CScase (_, s) 
    | CSdefault (s) 
    | CSspec (_, s) ->
	let l = build_label_tree s [] in
	(LabelBlock l) :: acc
	
  

let rec type_statement env et lz s =
  let sn,st,lz = type_statement_node s.loc env et lz s.node in
  { st_node = sn; 
    st_return = st.return; 
    st_break = st.break;
    st_continue = st.continue;
    st_term = st.term;
    st_loc = s.loc }, st, lz

and type_statement_node loc env et lz = function
  | CSnop -> 
      TSnop, mt_status, lz
  | CSexpr e ->
      let e = type_expr env e in
      TSexpr e, mt_status, lz
  | CSif (e, s1, s2) ->
      let e = type_boolean env e in
      let lz1,lz2,lz3 = match lz with
	| before,LabelBlock b1::LabelBlock b2::after ->
	    (before,b1@(LabelBlock b2)::after),
	    (before,b2@(LabelBlock b1)::after),
	    (LabelBlock b1::LabelBlock b2::before,after)
	| _ -> assert false
      in
      let s1,st1,_ = type_statement env et lz1 s1 in
      let s2,st2,_ = type_statement env et lz2 s2 in
      TSif (e, s1, s2), or_status st1 st2, lz3
  | CSbreak ->
      TSbreak, { mt_status with term = false; break = true }, lz
  | CScontinue -> 
     TScontinue, { mt_status with term = false; continue = true }, lz
  | CSlabel (lab, s) ->
      let info,lz1 = match lz with
	| before,LabelItem lab'::after ->
	    assert (lab=lab'.label_info_name);
	    lab',(LabelItem lab'::before,after)
	| _ -> assert false
      in
      let s, st, lz2 = type_statement env et lz1 s in
      TSlabel (info, s), st, lz2
  | CSblock bl ->
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let bl,st,_ = type_block env et lz1 bl in
      TSblock bl, st, lz2
  | CSgoto lab ->
      let before,after = lz in
      let status,info =
	try
	  let info = in_label_tree_list lab before in
	  lprintf "%a: backward goto@." Loc.report_position loc;
	  GotoBackward,info
	with Not_found ->
	  try
	    let info = in_label_upper_tree_list lab after in
	    lprintf "%a: forward outer goto@." Loc.report_position loc;
	    GotoForwardOuter,info
	  with Not_found ->
	    try
	      let info = in_label_tree_list lab after in
	      lprintf "%a: forward inner goto@." Loc.report_position loc;
	      GotoForwardInner,info
	    with Not_found ->
	      error loc "undefined label '%s'" lab
      in
      info.times_used <- info.times_used + 1;
      TSgoto(status,info), mt_status, lz
  | CSfor (an, e1, e2, e3, s) -> 
      let an = type_loop_annot env an in
      let e1 = type_expr env e1 in
      let e2 = type_boolean env e2 in
      let e3 = type_expr env e3 in
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let s,st, _ = type_statement env et lz1 s in
      TSfor (an, e1, e2, e3, s),
      { mt_status with return = st.return }, lz2
  | CSdowhile (an, s, e) ->
      let an = type_loop_annot env an in
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let s, st, _ = type_statement env et lz1 s in
      let e = type_boolean env e in
      TSdowhile (an, s, e), 
      { mt_status with return = st.return }, lz2
  | CSwhile (an, e, s) ->
      let an = type_loop_annot env an in
      let e = type_boolean env e in
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let s, st, _ = type_statement env et lz1 s in
      TSwhile (an, e, s), 
      { mt_status with return = st.return }, lz2
  | CSreturn None ->
      if et <> None then warning loc 
	      "`return' with no value, in function returning non-void";
      TSreturn None,{ mt_status with term = false; return = true }, lz
  | CSreturn (Some e) ->
      let e' = type_expr env e in
      let e' = match et with
	| None -> 
	    warning e.loc "`return' with a value, in function returning void";
	    None
	| Some ty ->
	    Some (coerce ty e')
      in
      TSreturn e', { mt_status with term = false; return = true }, lz
  | CSswitch (e, s) ->
      let e = type_int_expr env e in
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let s,st,_ = type_statement env et lz1 s in
      TSswitch (e, s), { st with break = false ; term = true }, lz2
  | CScase (e, s) ->
      let e = type_int_expr env e in
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let s,st,_ = type_statement env et lz1 s in
      TScase (e, s), st, lz2
  | CSdefault (s) ->
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let s,st,_ = type_statement env et lz1 s in
      TSdefault (s), st, lz2
  | CSannot (Assert p) ->
      let p = type_predicate env p in
      TSassert p, mt_status, lz
  | CSannot (Assume p) ->
      let p = type_predicate env p in
      TSassume p, mt_status, lz
  | CSannot (Label l) ->
      TSlogic_label l, mt_status, lz
  | CSannot (GhostSet(x,t)) ->
      let x = type_ghost_lvalue env x in
      let t = type_term env t in
      TSset (x, Cltyping.coerce x.term_type t), mt_status, lz
  | CSspec (spec, s) ->
      let spec = type_spec env spec in
      let lz1,lz2 = match lz with
	| before,LabelBlock b1::after ->
	    (before,b1@after),
	    (LabelBlock b1::before,after)
	| _ -> assert false
      in
      let s,st,_ = type_statement env et lz1 s in
      TSspec (spec, s), st, lz2

and type_block env et lz (dl,sl) = 
  let rec type_decls vs env = function
    | [] -> 
	[], env
    | { node = Cdecl (ty, x, i) } as d :: dl ->
	if Sset.mem x vs then error d.loc "redeclaration of `%s'" x;
	let ty = type_type d.loc env ty in	
	if eq_type_node ty.ctype_node Tvoid then 
	  error d.loc "variable `%s' declared void" x;
	let i = type_initializer_option d.loc env ty i in
	let ty = array_size_from_initializer d.loc ty i in
	let info = default_var_info x in
	if ty.ctype_storage = Static then set_static info;
	let env' = Env.add x ty (Var_info info) env in
	let dl',env'' = type_decls (Sset.add x vs) env' dl in
	{ d with node = Tdecl (ty, info, i) } :: dl', env''
    | { node = Ctypedecl ty } as d :: dl ->
	let ty' = type_type d.loc env ty in
	let dl',env' = type_decls vs env dl in
	{ d with node = Ttypedecl ty' } :: dl', env'
    | { loc = l } :: _ ->
	error l "unsupported local declaration"
  in
  let dl',env' = type_decls Sset.empty (Env.new_block env) dl in
  let rec type_bl lz = function
    | [] -> 
	[], mt_status, lz
    | [s] ->
	let s',st, lz1 = type_statement env' et lz s in
	[s'], st, lz1
    | s :: bl ->
	let s', st1, lz1 = type_statement env' et lz s in
	let bl', st2, lz2 = type_bl lz1 bl in
	let st = or_status st1 st2 in
	s' :: bl', { st with term = st2.term }, lz2
  in
  let sl', st, lz1 = type_bl lz sl in
  (dl', sl'), st, lz1

let type_parameters loc env pl =
  let type_one (ty,x) pl =
    let ty = type_type loc ~parameters:true env ty in 
    let info = default_var_info x in
    set_formal_param info;
    set_var_type (Var_info info) ty true;
    Coptions.lprintf 
      "Parameter %s added in env with unique name %s@." x info.var_unique_name;
    (ty,info) :: pl 
  in
  let is_void (ty,_) = ty.ctype_node = Tvoid in
  let pl = List.fold_right type_one pl [] in
  match pl with
    | [p] when is_void p -> 
	[]
    | _ -> 
	if List.exists is_void pl then 
	  error loc "`void' in parameter list must be the entire list";
	pl
  
let type_logic_parameters loc env pl = 
  List.fold_right
    (fun (ty,x) (pl,env) ->
       let info = default_var_info x in
       let ty = type_logic_type loc env ty in 
       (info,ty) :: pl, Env.add x ty (Var_info info) env)
    pl 
    ([], env)

let type_spec_decl loc = function
  | LDaxiom (id, p) -> 
      Taxiom (id, type_predicate (Env.empty ()) p)
  | LDinvariant (id, p) -> 
      Tinvariant (id, type_predicate (Env.empty ()) p)
  | LDlogic (id, ty, labels, pl, ll) ->
      let ty = type_logic_type loc (Env.empty ()) ty in
      let pl,env' = type_logic_parameters loc (Env.empty ()) pl in
      id.logic_args <- List.map fst pl;
      id.logic_why_type <- type_type_why ty false;
      let ll = List.map (type_location env') ll in
      Cenv.add_logic id.logic_name (List.map snd pl, ty, id);
      Tlogic (id, Function (pl, ty, ll))
  | LDlogic_def (id, ty, labels, pl, t) ->
      let ty = type_logic_type loc (Env.empty ()) ty in
      let pl,env' = type_logic_parameters loc (Env.empty ()) pl in
      id.logic_args <- List.map fst pl;
      id.logic_why_type <- type_type_why ty false;
      let t = type_term env' t in
      Cenv.add_logic id.logic_name (List.map snd pl, ty, id);
      Tlogic (id, Function_def (pl, ty, t))
  | LDpredicate_reads (id, pl, ll) ->
      let pl,env' = type_logic_parameters loc (Env.empty ()) pl in
      id.logic_args <- List.map fst pl;
      let ll = List.map (type_location env') ll in
      Cenv.add_pred id.logic_name (List.map snd pl,id);
      Tlogic (id, Predicate_reads (pl, ll))
  | LDpredicate_def (id, pl, p) ->
      let pl,env' = type_logic_parameters loc (Env.empty ()) pl in
      id.logic_args <- List.map fst pl;
      let p = type_predicate env' p in
      Cenv.add_pred id.logic_name (List.map snd pl,id);
      Tlogic (id, Predicate_def (pl, p))
  | LDghost (ty,x,cinit) -> 
      let ty = type_type ~ghost:true loc (Env.empty ()) ty in
      let info = add_ghost loc x ty (default_var_info x) in 
      set_static info;
      set_var_type (Var_info info) {ty with Ctypes.ctype_ghost = true} false;
      let cinit = 
	match cinit with
	  | None -> None
	  | Some (Iexpr t) -> Some(Iexpr (type_term (Env.empty()) t))
	  | _ -> assert false(*TODO*)
      in
      Tghost (info, cinit)
  | LDtype (id, loc) ->
      if Cenv.mem_type id then error loc "clash with previous type %s" id;
      Cenv.add_type id;
      Ttype id

(* table storing function specifications *)
let function_specs = Hashtbl.create 97

let empty_spec () = 
  { requires = None; assigns = None; ensures = None; decreases = None } 

let is_empty_spec s = 
  s.requires = None && s.assigns = None && 
  s.ensures = None && s.decreases = None

let function_spec loc f = function
  (* no spec given; we return the current spec if any, or [empty_spec] *)
  | None ->
      (try 
	 Hashtbl.find function_specs f
       with Not_found -> 
	 let s = empty_spec () in Hashtbl.add function_specs f s; s)
  (* a spec is given; we update the current spec only if [empty_spec] *)
  | Some s ->
      (try 
	 let s' = Hashtbl.find function_specs f in
	 if not (is_empty_spec s') then 
	   error loc "already a specification for %s" f;
	 s'.requires <- s.requires;
	 s'.assigns <- s.assigns;
	 s'.ensures <- s.ensures;
	 s'.decreases <- s.decreases;
	 s'
       with Not_found -> 
	 Hashtbl.add function_specs f s; s)

let combine_args loc l1 l2 =
  if l1 = [] then l2 else
    try
      List.map2 (fun x1 x2 ->
		if eq_type x1.var_type x2.var_type then 
		  if x1.var_name = "" then x2 else x1
		else error loc "clash with previous declaration: expected type %a, got %a"
		  print_type x1.var_type print_type x2.var_type
		)
	l1 l2
    with Invalid_argument _ -> 
      error loc "clash with previous declaration: not the same number of arguments"

let type_prototype loc pl ty f = 
  let pl = type_parameters loc (Env.empty ()) pl in
  let ty_res = type_type loc (Env.empty ()) ty in
  let info = default_fun_info f in
  let spl = List.map fst pl in
  let info = 
    match add_sym loc f (noattr (Tfun (spl, ty_res))) 
      (Fun_info info) with
	| Var_info _ -> assert false
	| Fun_info f -> 
	    f.args <- combine_args loc f.args (List.map snd pl);
	    f
  in
  let env = (* we build the env. to type the spec and the body *)
    List.fold_right 
      (fun v env -> Env.add v.var_name v.var_type (Var_info v) env)
      info.args (Env.empty ())
  in
  info,ty_res,env


let type_decl d = match d.node with
  | Cspecdecl s -> 
      type_spec_decl d.loc s
  | Ctypedef (ty, x) -> 
      let ty = type_type d.loc (Env.empty ()) ty in
      add_typedef d.loc x ty;
      Ttypedef (ty, x)
  | Ctypedecl ty -> 
      let ty = type_type d.loc (Env.empty ()) ty in
      Ttypedecl ty
  | Cdecl (ty, x, i) -> 
      begin match ty.Cast.ctype_node with
	| CTfun(pl,ty_res) ->
	    let info,ty_res,_ = type_prototype d.loc pl ty_res x in
	    Tfunspec (function_spec d.loc x None, ty_res, info)
	| _ -> 
	    let ty = type_type d.loc (Env.empty ()) ty in
	    let info = 
	      match add_sym d.loc x ty (Var_info (default_var_info x)) with
		| Var_info v -> v
		| Fun_info _ -> assert false
	    in
	    set_static info;
	    Loc.report_position Coptions.log d.loc;
	    fprintf Coptions.log "Variable %s is assigned@." info.var_name;
	    set_assigned info; (* ????? *)
	    let i = type_initializer_option d.loc (Env.empty ()) ty i in
	    let ty = array_size_from_initializer d.loc ty i in
	    set_var_type (Var_info info) ty false;
	    if ty.ctype_const then begin match ty.ctype_node, i with
	      | (Tint _ | Tfloat _ | Tenum _), Some (Iexpr e) -> 
		  begin
		    try
		      set_const_value info (eval_const_expr_noerror e)
		    with
			Invalid_argument msg -> ()	
		  end
	      | _ ->
		  ()
	    end;
	    Tdecl (ty, info,i)
      end
  | Cfunspec (s, ty, f, pl) ->
      let info,ty,env = type_prototype d.loc pl ty f in
      info.has_assigns <- (s.assigns <> None);
      let s = type_spec ~result:ty env s in
      let s = function_spec d.loc f (Some s) in
      Tfunspec (s, ty, info)
  | Cfundef (s, ty, f, pl, bl) -> 
      let info,ty,env = type_prototype d.loc pl ty f in
      let et = if eq_type ty c_void then None else Some ty in
      info.has_body <- true;
      let s = Option_misc.map (type_spec ~result:ty env) s in
      let s = function_spec d.loc f s in
      info.has_assigns <- (s.assigns <> None);
      let lz = build_label_tree bl [] in
      fprintf Coptions.log "Labels for function %s:@\n" f;
      printf_label_tree Coptions.log (LabelBlock lz);
      fprintf Coptions.log "@.";
      let bl,st,_ = type_statement env et ([],lz) bl in
      if st.term && et <> None then
	warning d.loc "control reaches end of non-void function";
      if st.break then 
	error d.loc "break statement not within a loop or switch";
      if st.continue then 
	error d.loc "continue statement not within a loop";
      Tfundef (s, ty, info, bl)

let type_file = List.map (fun d -> { d with node = type_decl d })

(*
Local Variables: 
compile-command: "unset LANG; make -C .. bin/caduceus.byte"
End: 
*)
