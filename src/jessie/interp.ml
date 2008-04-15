(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(**************************************************************************)

(* $Id: interp.ml,v 1.188 2008/12/09 10:36:46 uid525 Exp $ *)

(* Import from Cil *)
open Cil_types
open Cil
open Cilutil
open Ast_info
open Extlib
open Db_types

(* Import from Why *)
open Jc
open Jc_constructors
open Jc_ast
open Jc_env
open Jc_fenv
open Jc_pervasives

(* Utility functions *)
open Common
open Integer
open Format


(*****************************************************************************)
(* Smart constructors.                                                       *)
(*****************************************************************************)

let mktype tnode = new ptype tnode

let mkexpr enode pos = new pexpr ~pos enode

let void_expr = mkexpr (JCPEconst JCCvoid) Loc.dummy_position
let null_expr = mkexpr (JCPEconst JCCnull) Loc.dummy_position
let true_expr = mkexpr (JCPEconst(JCCboolean true)) Loc.dummy_position
let zero_expr = mkexpr (JCPEconst(JCCinteger "0")) Loc.dummy_position
let one_expr = mkexpr (JCPEconst(JCCinteger "1")) Loc.dummy_position

let mktag tag_node pos = new ptag ~pos tag_node

let mkidentifier name pos = new identifier ~pos name

let rec mkconjunct elist pos =
  match elist with
    | [] -> true_expr
    | [e] -> e
    | e::el -> mkexpr (JCPEbinary(e,`Bland,mkconjunct el pos)) pos

let mkdecl dnode pos = new decl ~pos dnode


(*****************************************************************************)
(* Locate Jessie expressions on source program.                              *)
(*****************************************************************************)

let reg_pos ?id ?kind ?name pos = Output.reg_pos "C" ?id ?kind ?name pos

(* [locate] should be called on every Jessie expression which we would like to
 * locate in the original source program.
 *)
let locate ?alarm ?pos e =
  (* Recursively label conjuncts so that splitting conjuncts in Why still
   * allows to locate the resulting VC.
   *)
  let rec dopos ~toplevel e =
    (* Generate (and store) a label associated to this source location *)
    let pos = match pos with
      | None -> e#pos
      | Some pos ->
	  if is_unknown_location e#pos then pos else e#pos
    in
    let lab = match alarm with
      | None ->
	  reg_pos pos
      | Some Alarms.Division_alarm ->
	  reg_pos ~kind:Output.DivByZero pos
      | Some Alarms.Memory_alarm ->
	  reg_pos ~kind:Output.PointerDeref pos
      | Some Alarms.Shift_alarm ->
	  reg_pos ~kind:Output.ArithOverflow pos
      | Some Alarms.Pointer_compare_alarm
      | Some Alarms.Using_nan_or_infinite_alarm
      | Some Alarms.Result_is_nan_or_infinite_alarm ->
	  reg_pos pos
      | Some Alarms.Separation_alarm -> reg_pos pos
      | Some Alarms.Other_alarm -> reg_pos pos
    in
    let e = match e#node with
      | JCPEbinary(e1,`Bland,e2) ->
	  begin match e1#node,e2#node with
	    | JCPElabel _,JCPElabel _ -> e (* already labelled *)
	    | JCPElabel _,_ -> (* [e1] already labelled *)
		let e2 = dopos ~toplevel:false e2 in
		mkexpr (JCPEbinary(e1,`Bland,e2)) pos
	    | _,JCPElabel _ -> (* [e2] already labelled *)
		let e1 = dopos ~toplevel:false e1 in
		mkexpr (JCPEbinary(e1,`Bland,e2)) pos
	    | _,_ -> (* none already labelled *)
		let e1 = dopos ~toplevel:false e1 in
		let e2 = dopos ~toplevel:false e2 in
		mkexpr (JCPEbinary(e1,`Bland,e2)) pos
	  end
      | _ -> e
    in
    (* Do not generate a label for every intermediate conjunct *)
    match e#node with
      | JCPEbinary(_e1,`Bland,_e2) when not toplevel -> e
      | _ ->
	  (* Label the expression accordingly *)
	  mkexpr (JCPElabel(lab,e)) pos
  in
  dopos ~toplevel:true e


(*****************************************************************************)
(* Cil to Jessie translation of operators                                    *)
(*****************************************************************************)

let unop = function
  | Neg -> `Uminus
  | BNot -> `Ubw_not
  | LNot -> `Unot

let binop = function
  | PlusA -> `Badd
  | PlusPI -> `Badd
  | IndexPI -> `Badd
  | MinusA -> `Bsub
  | MinusPI -> `Bsub
  | MinusPP -> `Bsub
  | Mult -> `Bmul
  | Div -> `Bdiv
  | Mod -> `Bmod
  | Shiftlt -> `Bshift_left
  | Shiftrt -> assert false (* Should be decided at point used *)
  | Lt -> `Blt
  | Gt -> `Bgt
  | Le -> `Ble
  | Ge -> `Bge
  | Eq -> `Beq
  | Ne -> `Bneq
  | BAnd -> `Bbw_and
  | BXor -> `Bbw_xor
  | BOr -> `Bbw_or
  | LAnd -> `Bland
  | LOr -> `Blor

let relation = function
  | Rlt -> `Blt
  | Rgt -> `Bgt
  | Rle -> `Ble
  | Rge -> `Bge
  | Req -> `Beq
  | Rneq -> `Bneq


(*****************************************************************************)
(* Cil to Jessie translation of types                                        *)
(*****************************************************************************)

let type_of_padding = mktype (JCPTidentifier name_of_padding_type)

let type_conversion_table = Hashtbl.create 0

let type_conversion ty1 ty2 =
  let ty1 = typeRemoveAttributes ["const";"volatile"] (unrollType ty1) in
  let ty2 = typeRemoveAttributes ["const";"volatile"] (unrollType ty2) in
  let sig1 = typeSig ty1 and sig2 = typeSig ty2 in
  try
    let _,_,ty1_to_ty2,ty2_to_ty1 =
      Hashtbl.find type_conversion_table (sig1,sig2)
    in
    ty1_to_ty2,ty2_to_ty1
  with Not_found ->
    try
      let _,_,ty2_to_ty1,ty1_to_ty2 =
	Hashtbl.find type_conversion_table (sig2,sig1)
      in
      ty1_to_ty2,ty2_to_ty1
    with Not_found ->
      let n1 = type_name ty1 and n2 = type_name ty2 in
      let ty1_to_ty2 = unique_logic_name (n1 ^ "_to_" ^ n2) in
      let ty2_to_ty1 = unique_logic_name (n2 ^ "_to_" ^ n1) in
      Hashtbl.add
	type_conversion_table (sig1,sig2) (ty1,ty2,ty1_to_ty2,ty2_to_ty1);
      ty1_to_ty2,ty2_to_ty1

type float_model = [ `Real | `Strict | `Full ]

let float_model : float_model ref = ref `Real

let ctype ?bitsize ty =
  let tnode = match unrollType ty with
    | TVoid _attr -> JCPTnative Tunit

    | TInt(_ik,_attr) ->
	if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	  JCPTnative Tinteger
	else
	  JCPTidentifier (name_of_integral_type ?bitsize ty)

    | TFloat(fk,_attr) ->
	begin
	  match !float_model with
	    | `Real -> 
		(* RealMode floats interpreted as reals *) 
		JCPTnative Treal
	    | `Strict | `Full ->
		  begin
		    match fk with
		      | FFloat -> JCPTnative Tfloat
		      | FDouble -> JCPTnative Tdouble
		      | FLongDouble -> failwith "Jessie does not handle long double yet"
		  end
	end
    | TPtr(_elemty,_attr) ->
	if is_reference_type ty then
	  (* Do not use [_elemty] directly but rather [pointed_type ty] in order
	   * to get to the array element in references, i.e. pointers to arrays.
	   *)
	  begin match unrollType (pointed_type ty) with
	    | TComp(compinfo,_attr) ->
		let min_bound = Num.num_of_string "0" in
		let max_bound =
		  Num.num_of_string (Int64.to_string (reference_size ty - 1L))
		in
		JCPTpointer(compinfo.cname,[],Some min_bound,Some max_bound)
	    | _ -> assert false
	  end
	else
	  begin match unrollType (pointed_type ty) with
	    | TComp(compinfo,_attr) ->
		JCPTpointer(compinfo.cname,[],None,None)
	    | _ ->
		Format.eprintf "%a@." !Ast_printer.d_type ty;
		assert false
	  end

    | TArray _ -> assert false (* Removed by translation *)

    | TFun _ ->
	Errormsg.s
	  (Cil.error "Function pointer type %a not allowed" !Ast_printer.d_type ty)

    | TNamed _ -> assert false (* Removed by call to [unrollType] *)

    | TComp(compinfo,_attr) -> JCPTidentifier compinfo.cname

    | TEnum(enuminfo,_attr) -> JCPTidentifier enuminfo.ename

    | TBuiltin_va_list _ ->
	Errormsg.s (Cil.error "Type builtin_va_list not allowed")
  in
  mktype tnode

let ltype = function
  | Ctype ty -> ctype ty
  | Ltype (s,[]) -> mktype (JCPTidentifier s)
  | Linteger -> mktype (JCPTnative Tinteger)
  | Lreal -> mktype (JCPTnative Treal)
  | Ltype(_,_) | Lvar _ | Larrow _ -> assert false (* TODO *)


(*****************************************************************************)
(* Cil to Jessie translation of constants                                    *)
(*****************************************************************************)

let native_type_of_fkind x : Jc_env.native_type =
  match x with
    | FFloat -> Tfloat
    | FDouble -> Tdouble
    | FLongDouble -> failwith "Jessie does not handle long double yet"

let strip_float_suffix s =
  let l = pred(String.length s)  in
    match String.get s l with
      | 'f' | 'F' | 'l' | 'L' -> String.sub s 0 l
      | _ -> s

let rec const ~in_code pos = function
  | CInt64(_i,_ik,Some s) -> JCPEconst(JCCinteger s)
      (* Use the textual representation if available *)

  | CInt64(i,_ik,None) -> JCPEconst(JCCinteger(Int64.to_string i))

  | CStr _ | CWStr _ -> assert false  (* Should have been rewritten *)

  | CChr c -> JCPEconst(JCCinteger(string_of_int (Char.code c)))

  | CReal(_f,fk,Some s) -> 
      (* Use the textual representation if available *)
      let s = strip_float_suffix s in
      begin match in_code,!float_model with
	| false,_ | _,`Real -> JCPEconst(JCCreal s)
	| true, (`Strict | `Full) ->
	    (* add a cast to float or double depending on the value of fk *)
	    JCPEcast(mkexpr (JCPEconst(JCCreal s)) pos, mktype (JCPTnative (native_type_of_fkind fk)))
      end
  | CReal(f,_fk,None) -> 
      (* otherwise use the float value *)
      JCPEconst(JCCreal(string_of_float f))

  | CEnum item ->
      let e = mkexpr (const_of_expr ~in_code pos item.eival) pos in
      JCPEcast(e,ctype (TEnum(item.eihost,[])))

and const_of_expr ~in_code pos e =
  match stripInfo e with Const c -> const ~in_code pos c | _ -> assert false

and boolean_const = function
  | CInt64(i,_ik,_text) ->
      if i = Int64.zero then JCCboolean false else JCCboolean true

  | CStr _ | CWStr _ -> JCCboolean true

  | CChr c ->
      if Char.code c = 0 then JCCboolean false else JCCboolean true

  | CReal(f,_fk,_text) ->
      if f = 0.0 then JCCboolean false else JCCboolean true

  | CEnum {eival = e} -> boolean_const_of_expr e

and boolean_const_of_expr e =
  match stripInfo e with Const c -> boolean_const c | _ -> assert false


(*****************************************************************************)
(* Cil to Jessie translation of logic constructs                             *)
(*****************************************************************************)

let label = function
  | Label(lab,_,_) -> lab
  | Case _ | Default _ -> assert false

let logic_label lab =
  let label_name s =
    LabelName {
      label_info_name = s;
      label_info_final_name = s;
      times_used = 0;
    }
  in
  match lab with
    | LogicLabel s -> label_name s
    | StmtLabel sref ->
	let labels = filter_out is_case_label !sref.labels in
	assert (not (labels = []));
	label_name (label (List.hd labels))

let logic_labels = List.map logic_label

let logic_labels_assoc =
  List.map (fun (_,l) -> logic_label l)

let term_lhost pos = function
  | TVar v -> mkexpr (JCPEvar v.lv_name) pos
  | TResult -> mkexpr (JCPEvar "\\result") pos
  | TMem _ -> assert false (* Should have been rewritten *)

let isLogicConstant t = match t.term_node with
    TConst _ -> true
  | _ -> false

let rec coerce_floats t =
  match !float_model with
    | `Real -> term t
    | `Strict | `Full ->
	if isLogicFloatType t.term_type then 
	  mkexpr (JCPEcast(term t, mktype (JCPTnative Treal))) t.term_loc
	else term t
      
and term t =
  let enode = match t.term_node with
    | TConst c -> const ~in_code:false t.term_loc c

    | TDataCons({ctor_type = Ltype ("boolean",[])} as d,_args) ->
        JCPEconst (JCCboolean (d.ctor_name = "\\true"))

    | TDataCons _ | TUpdate _ ->
	(* TODO, but only when we have real concrete
         * types in the Frama-C annotation grammar
         *)
        assert false

    | TLval lv -> (term_lval t.term_loc lv)#node

    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ ->
	assert false (* Should be removed by constant folding *)

    | TUnOp(op,t) -> JCPEunary(unop op,term t)

    | TBinOp((Lt | Gt | Le | Ge as op),t1,t2)
	when app_term_type isPointerType false t1.term_type ->
	(* Pointer comparison is translated as subtraction *)
	let sube = mkexpr (JCPEbinary(term t1,`Bsub,term t2)) t.term_loc in
	JCPEbinary(sube,binop op,zero_expr)

(*     | TBinOp((Eq | Ne as op),t1,t2)  *)
(* 	when app_term_type isPointerType false t1.term_type *)
(* 	  && (not (is_null_term t1 || is_null_term t2))  *)
(* 	  && (not (is_base_addr t1 || is_base_addr t2)) -> *)
(* 	(\* Pointer (dis-)equality is translated as subtraction *\) *)
(* 	let sube = mkexpr (JCPEbinary(term t1,`Bsub,term t2)) t.term_loc in *)
(* 	JCPEbinary(sube,binop op,zero_expr) *)

    | TBinOp(Shiftrt,t1,t2) ->
	begin match possible_value_of_integral_term t2 with
	  | Some i when i >= 0L && i < 63L ->
	      (* Right shift by constant is division by constant *)
	      let pow = constant_term t2.term_loc (power_of_two i) in
	      JCPEbinary(term t1,`Bdiv,term pow)
	  | _ ->
	      let op = match t1.term_type with
		| Ctype ty1 ->
		    if isSignedInteger ty1 then `Barith_shift_right
		    else `Blogical_shift_right
		| Linteger -> `Barith_shift_right
		| _ -> assert false
	      in
	      JCPEbinary(term t1,op,term t2)
	end

    | TBinOp(Shiftlt as op,t1,t2) ->
	begin match possible_value_of_integral_term t2 with
	  | Some i when i >= 0L && i < 63L ->
	      (* Left shift by constant is multiplication by constant *)
	      let pow = constant_term t2.term_loc (power_of_two i) in
	      JCPEbinary(term t1,`Bmul,term pow)
	  | _ ->
	      JCPEbinary(term t1,binop op,term t2)
	end

    | TBinOp((Lt | Gt | Le | Ge) as op,t1,t2) ->
	JCPEbinary(term t1,binop op,term t2)

    | TBinOp(op,t1,t2) ->
	JCPEbinary(coerce_floats t1,binop op,coerce_floats t2)

    | TCastE(ty,t)
	when isIntegralType ty && isLogicArithmeticType t.term_type ->
	if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	  (term t)#node
	else
	  JCPEcast(term t,ctype ty)

    | TCastE(ty,t)
	when isFloatingType ty && isLogicArithmeticType t.term_type ->
	JCPEcast(term t,ctype ty)

    | TCastE(ty,t)
	when isIntegralType ty && app_term_type isPointerType false t.term_type ->
(* 	  && bits_sizeof ty = force_app_term_type bits_sizeof t.term_type -> *)
(* 	let _,ptr_to_int = force_app_term_type (type_conversion ty) t.term_type in *)
(* 	JCPEapp(ptr_to_int,[],[term t]) *)
	Errormsg.s
	  (Cil.error "Casting from type %a to type %a not allowed"
	     !Ast_printer.d_logic_type t.term_type !Ast_printer.d_type ty)

    | TCastE(ptrty,_t1) when isPointerType ptrty ->
	let t = stripTermCasts t in
	begin match t.term_node with
	  | Tnull ->
	      JCPEconst JCCnull
	  | TConst c
	      when is_integral_const c && value_of_integral_const c = Int64.zero ->
	      JCPEconst JCCnull
	  | _ ->
(* 	      if isLogicIntegralType t.term_type then *)
(* 		let addr = *)
(* 		  mkexpr (JCPEaddress(Addr_absolute,term t)) t.term_loc *)
(* 		in *)
(* 		JCPEcast(addr,ctype ptrty) *)
(* 	      else if force_app_term_type isIntegralType t.term_type *)
(* 		&& *)
(* 		force_app_term_type bits_sizeof t.term_type *)
(* 		= bits_sizeof ptrty *)
(* 	      then *)
(* 		let _,int_to_ptr = *)
(* 		  force_app_term_type (type_conversion ptrty) t.term_type *)
(* 		in *)
(* 		JCPEapp(int_to_ptr,[],[term t]) *)
(* 	       else if force_app_term_type isPointerType t.term_type then *)
(* 		let destty = pointed_type ptrty in *)
(* 		let srcty = force_app_term_type pointed_type t.term_type in *)
(* 		if Retype.subtype srcty destty then *)
(* 		  (term t)#node *)
(* 		else if Retype.TypeUnion.same_class destty srcty then *)
(* 		  JCPEcast(term t,ctype ptrty) *)
(* 		else *)
		  (* bitwise cast *)
(* 		  JCPEcast(term t,ctype ptrty) *)
(* 		  let _,ptr_to_ptr = *)
(* 		    force_app_term_type (type_conversion ptrty) t.term_type *)
(* 		  in *)
(* 		  JCPEapp(ptr_to_ptr,[],[term t]) *)
(* 	      else *)
	      (* Only hierarchical types are available in Jessie. It
	       * should have been encoded as the use of pointer types
	       * on structure type.
	       *)

(* 	      match unrollType ty with *)
(* 		| TComp(compinfo,_) -> *)
(* 		    JCPEcast(term t,compinfo.cname) *)
(* 		| _ -> assert false *)
	      Errormsg.s
		(Cil.error "Casting from type %a to type %a not allowed in logic"
		  !Ast_printer.d_logic_type t.term_type !Ast_printer.d_type ptrty)
	end

    | TCastE(ty,t) ->
	(* TODO: support other casts in Jessie as well, through low-level
	 * memory model
	 *)
	Errormsg.s
	  (Cil.error "Casting from type %a to type %a not allowed"
	    !Ast_printer.d_logic_type t.term_type !Ast_printer.d_type ty)

    | TAddrOf _tlv -> assert false (* Should have been rewritten *)

    | TStartOf tlv -> (term_lval t.term_loc tlv)#node

    | Tapp(linfo,labels,tlist) ->
	let name = linfo.l_name in
	JCPEapp(name,logic_labels_assoc labels,List.map coerce_floats tlist)

    | Tif(t1,t2,t3) -> JCPEif(term t1,term t2,term t3)

    | Told t -> JCPEold(term t)

    | Tat(t,lab) -> JCPEat(term t,logic_label lab)

    | Tbase_addr t -> JCPEbase_block(term t)

    | Tblock_length _t -> assert false (* TODO: memory model for Jessie *)

    | Tnull -> JCPEconst JCCnull

    | TCoerce(_t,_typ) -> assert false (* TODO: see if useful *)

    | TCoerceE(_t1,_t2) -> assert false (* TODO: see if useful *)
    | Tlambda _ -> assert false (* TODO: does not exist in Jessie *)

    | Ttypeof _ | Ttype _ -> assert false (* Should have been treated *)
    | Ttsets _ -> assert false
  in
  mkexpr enode t.term_loc

and tag t =
  let tag_node = match t.term_node with
    | Ttypeof t -> JCPTtypeof (term t)
    | Ttype ty ->
	let id = mkidentifier (get_struct_name (pointed_type ty)) t.term_loc in
	JCPTtag id
    | _ -> assert false (* Not a tag *)
  in
  mktag tag_node t.term_loc

and term_lval pos lv =
  match lv with
    | lhost, TNoOffset -> term_lhost pos lhost

    | (TVar _ | TResult), _off ->
	assert false (* Should have been rewritten *)

    | TMem t, TField(fi,toff) ->
	assert (toff = TNoOffset); (* Others should have been rewritten *)
	let e = term t in
	if not fi.fcomp.cstruct then (* field of union *)
	  mkexpr (JCPEcast(e,ctype fi.ftype)) pos
	else
	  let repfi = Retype.FieldUnion.repr fi in
	  let e,fi =
	    if FieldinfoComparable.equal fi repfi then
	      e,fi
	    else
	      let caste =
		mkexpr (JCPEcast(e,ctype (TPtr(TComp(repfi.fcomp,[]),[])))) pos
	      in
	      caste,repfi
	  in
	  mkexpr (JCPEderef(e,fi.fname)) pos

    | TMem t, TIndex(it,TField(fi,toff)) ->
	assert (toff = TNoOffset); (* Others should have been rewritten *)
	(* Normalization made it equivalent to simple add *)
	let e = mkexpr (JCPEbinary(term t,`Badd,term it)) pos in
	if not fi.fcomp.cstruct then (* field of union *)
	  mkexpr (JCPEcast(e,ctype fi.ftype)) pos
	else
	  let repfi = Retype.FieldUnion.repr fi in
	  let e,fi =
	    if FieldinfoComparable.equal fi repfi then
	      e,fi
	    else
	      let caste =
		mkexpr (JCPEcast(e,ctype (TPtr(TComp(repfi.fcomp,[]),[])))) pos
	      in
	      caste,repfi
	  in
	  mkexpr (JCPEderef(e,fi.fname)) pos

    | TMem _e, TIndex _ ->
	Format.eprintf "%a@." !Ast_printer.d_term_lval lv;
	assert false (* Should have been rewritten *)

let tsets_lhost = function
  | TSVar lv -> mkexpr (JCPEvar lv.lv_name) Loc.dummy_position
  | TSResult -> mkexpr (JCPEvar "\\result") Loc.dummy_position
  | TSMem _ as lhost ->
      Format.eprintf "[Interp.tsets_lhost] unexpected %a@."
	!Ast_printer.d_tsets_lhost lhost;
      assert false (* Should have been rewritten *)

let rec tsets_elem ts =
  let enode = match ts with
    | TSLval lv -> (tsets_lval lv)#node

    | TSStartOf lv -> (tsets_lval lv)#node

    | TSAddrOf _ -> assert false (* Should have been rewritten *)

    | TSConst c -> const ~in_code:false Loc.dummy_position c

    | TSat(ts,lab) -> JCPEat(tsets_elem ts,logic_label lab)

    | TSAdd_index(base,idx) ->
	JCPEbinary(tsets_elem base,`Badd,term idx)

    | TSAdd_range(base,low,high) ->
	let e =
	  mkexpr (JCPErange(opt_map term low,opt_map term high)) Loc.dummy_position
	in
	JCPEbinary(tsets_elem base,`Badd,e)

    | TSCastE(ty,ts)
	when isIntegralType ty && isLogicArithmeticType (typeOfTsetsElem ts) ->
	if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	  (tsets_elem ts)#node
	else
	  JCPEcast(tsets_elem ts,ctype ty)

    | TSCastE(ty,ts)
	when isFloatingType ty && isLogicArithmeticType (typeOfTsetsElem ts) ->
	JCPEcast(tsets_elem ts,ctype ty)

    | TSCastE(ptrty,_ts1) when isPointerType ptrty ->
	begin match stripTsetsCasts ts with
	  | TSConst c
	      when is_integral_const c && value_of_integral_const c = Int64.zero ->
	      JCPEconst JCCnull
	  | ts ->
(* 	      let ety = typeOfTsetsElem ts in *)
(* 	      if isLogicIntegralType ety then *)
(* 		let addr = *)
(* 		  mkexpr (JCPEaddress(Addr_absolute,tsets_elem ts)) *)
(* 		    Loc.dummy_position *)
(* 		in *)
(* 		JCPEcast(addr,ctype ptrty) *)
(* 	      else if force_app_term_type isIntegralType ety then *)
(* 		&& *)
(* 		force_app_term_type bits_sizeof ety *)
(* 		= bits_sizeof ptrty *)
(* 	      then *)
(* 		let _,int_to_ptr = *)
(* 		  force_app_term_type (type_conversion ptrty) ety *)
(* 		in *)
(* 		JCPEapp(int_to_ptr,[],[tsets_elem ts]) *)
(*  	      else if force_app_term_type isPointerType ety then *)
(* 		let destty = pointed_type ptrty in *)
(* 		let srcty = force_app_term_type pointed_type ety in *)
(* 		if Retype.subtype srcty destty then *)
(* 		  (tsets_elem ts)#node *)
(* 		else if Retype.TypeUnion.same_class destty srcty then *)
(* 		  JCPEcast(tsets_elem ts,ctype ptrty) *)
(* 		else *)
(* 		  let _,ptr_to_ptr = *)
(* 		    force_app_term_type (type_conversion ptrty) ety *)
(* 		  in *)
(* 		  JCPEapp(ptr_to_ptr,[],[tsets_elem ts]) *)
(* 	      else *)
	      (* Only hierarchical types are available in Jessie. It
	       * should have been encoded as the use of pointer types
	       * on strucure type.
	       *)
(* 	      match unrollType ty with *)
(* 		| TComp(compinfo,_) -> JCPEcast(tsets_elem ts,compinfo.cname) *)
(* 		| _ -> assert false *)
	      Errormsg.s
		(Cil.error "Casting from type %a to type %a not allowed in logic"
		  !Ast_printer.d_logic_type (typeOfTsetsElem ts) !Ast_printer.d_type ptrty)
	end

    | TSCastE(ty,ts) ->
	(* TODO: support other casts in Jessie as well, through low-level
	 * memory model
	 *)
	Errormsg.s
	  (Cil.error "Casting from type %a to type %a not allowed"
	    !Ast_printer.d_logic_type (typeOfTsetsElem ts) !Ast_printer.d_type ty)
    | TSapp _ -> assert false
  in
  mkexpr enode Loc.dummy_position

and tsets_lval = function
  | lhost, TSNoOffset -> tsets_lhost lhost

  | (TSVar _ | TSResult), _off ->
      assert false (* Should have been rewritten *)

  | TSMem ts, TSField(fi,tsoff) ->
      assert (tsoff = TSNoOffset); (* Others should have been rewritten *)
      let e = tsets_elem ts in
      if not fi.fcomp.cstruct then (* field of union *)
	mkexpr (JCPEcast(e,ctype fi.ftype)) Loc.dummy_position
      else
	let repfi = Retype.FieldUnion.repr fi in
	let e,fi =
	  if FieldinfoComparable.equal fi repfi then
	    e,fi
	  else
	    let caste =
	      mkexpr (JCPEcast(e,ctype (TPtr(TComp(repfi.fcomp,[]),[]))))
		Loc.dummy_position
	    in
	    caste,repfi
	in
	mkexpr (JCPEderef(e,fi.fname)) Loc.dummy_position

  | TSMem ts, TSIndex(it,TSField(fi,tsoff)) ->
      assert (tsoff = TSNoOffset); (* Others should have been rewritten *)
      (* Normalization made it equivalent to simple add *)
      let e =
        mkexpr (JCPEbinary(tsets_elem ts,`Badd,term it)) Loc.dummy_position
      in
      if not fi.fcomp.cstruct then (* field of union *)
	mkexpr (JCPEcast(e,ctype fi.ftype)) Loc.dummy_position
      else
	let repfi = Retype.FieldUnion.repr fi in
	let e,fi =
	  if FieldinfoComparable.equal fi repfi then
	    e,fi
	  else
	    let caste =
	      mkexpr (JCPEcast(e,ctype (TPtr(TComp(repfi.fcomp,[]),[]))))
		Loc.dummy_position
	    in
	    caste,repfi
	in
	mkexpr (JCPEderef(e,fi.fname)) Loc.dummy_position

  | TSMem _ts, TSIndex _ -> assert false (* Should have been rewritten *)

  | TSMem ts, TSRange(low,high,TSField(fi,tsoff)) ->
      assert (tsoff = TSNoOffset); (* Others should have been rewritten *)
      (* Normalization made it equivalent to simple add *)
      let enode = JCPErange(opt_map term low,opt_map term high) in
      let e = mkexpr enode Loc.dummy_position in
      let e =
        mkexpr (JCPEbinary(tsets_elem ts,`Badd,e)) Loc.dummy_position
      in
      if not fi.fcomp.cstruct then (* field of union *)
	mkexpr (JCPEcast(e,ctype fi.ftype)) Loc.dummy_position
      else
	let repfi = Retype.FieldUnion.repr fi in
	let e,fi =
	  if FieldinfoComparable.equal fi repfi then
	    e,fi
	  else
	    let caste =
	      mkexpr (JCPEcast(e,ctype (TPtr(TComp(repfi.fcomp,[]),[]))))
		Loc.dummy_position
	    in
	    caste,repfi
	in
	mkexpr (JCPEderef(e,fi.fname)) Loc.dummy_position

  | TSMem _ts, TSRange _ -> assert false (* Should have been rewritten *)

let rec tsets = function
  | TSSingleton ts -> [tsets_elem ts]
  | TSEmpty -> []
  | TSUnion locs -> List.flatten (List.map tsets locs)
  | TSInter _locs -> assert false (* TODO *)
  | TSComprehension(_t,_q,_p) -> assert false (* TODO *)


let rec pred p =
  let enode = match p.content with
    | Pfalse -> JCPEconst(JCCboolean false)

    | Ptrue -> JCPEconst(JCCboolean true)

    | Papp(pinfo,labels,tl) ->
	JCPEapp(pinfo.l_name,logic_labels_assoc labels,List.map term tl)

    | Prel((Rlt | Rgt | Rle | Rge as rel),t1,t2)
	when app_term_type isPointerType false t1.term_type ->
	(* Pointer comparison is translated as subtraction *)
	let sube = mkexpr (JCPEbinary(term t1,`Bsub,term t2)) p.loc in
	JCPEbinary(sube,relation rel,zero_expr)

(*     | Prel((Req | Rneq as rel),t1,t2)  *)
(* 	when app_term_type isPointerType false t1.term_type *)
(* 	  && (not (is_null_term t1 || is_null_term t2)) *)
(* 	  && (not (is_base_addr t1 || is_base_addr t2)) -> *)
(* 	(\* Pointer (dis-)equality is translated as subtraction *\) *)
(* 	let sube = mkexpr (JCPEbinary(term t1,`Bsub,term t2)) p.loc in *)
(* 	JCPEbinary(sube,relation rel,zero_expr) *)

    | Prel(Req,t1,t2) when isTypeTagType t1.term_type ->
	JCPEeqtype(tag t1,tag t2)

    | Prel(Rneq,t1,t2) when isTypeTagType t1.term_type ->
	let eq = mkexpr (JCPEeqtype(tag t1,tag t2)) p.loc in
	JCPEunary(`Unot,eq)

    | Prel(rel,t1,t2) ->
	JCPEbinary(coerce_floats t1,relation rel,coerce_floats t2)

    | Pand(p1,p2) ->
	JCPEbinary(pred p1,`Bland,pred p2)

    | Por(p1,p2) ->
	JCPEbinary(pred p1,`Blor,pred p2)

    | Pxor(p1,p2) ->
	let notp2 = { p2 with content = Pnot p2; } in
	let p1notp2 = { p with content = Pand(p1,notp2); } in
	let notp1 = { p1 with content = Pnot p1; } in
	let p2notp1 = { p with content = Pand(p2,notp1); } in
	JCPEbinary(pred p1notp2,`Blor,pred p2notp1)

    | Pimplies(p1,p2) ->
	JCPEbinary(pred p1,`Bimplies,pred p2)

    | Piff(p1,p2) ->
	JCPEbinary(pred p1,`Biff,pred p2)

    | Pnot p -> JCPEunary(`Unot,pred p)

    | Pif(t,p1,p2) -> JCPEif(term t,pred p1,pred p2)

    | Plet(_v,_t,_p) -> assert false (* TODO *)

    | Pforall([],p) -> (pred p)#node

    | Pforall([v],p) ->
	JCPEquantifier(Forall,ltype v.lv_type,[v.lv_name],pred p)

    | Pforall(v::q,subp) ->
	let newp = { p with content = Pforall(q,subp) } in
	JCPEquantifier(Forall,ltype v.lv_type,[v.lv_name],pred newp)

    | Pexists([],p) -> (pred p)#node

    | Pexists([v],p) ->
	JCPEquantifier(Exists,ltype v.lv_type,[v.lv_name],pred p)

    | Pexists(v::q,subp) ->
	let newp = { p with content = Pexists(q,subp) } in
	JCPEquantifier(Exists,ltype v.lv_type,[v.lv_name],pred newp)

    | Pold p ->	JCPEold(pred p)

    | Pat(p,lab) -> JCPEat(pred p,logic_label lab)

    | Pvalid_index(t1,t2) ->
	let e1 = term t1 in
	let e2 = term t2 in
	let eoffmin = mkexpr (JCPEoffset(Offset_min,e1)) p.loc in
	let emin = mkexpr (JCPEbinary(eoffmin,`Ble,e2)) p.loc in
	let eoffmax = mkexpr (JCPEoffset(Offset_max,e1)) p.loc in
	let emax = mkexpr (JCPEbinary(eoffmax,`Bge,e2)) p.loc in
	(mkconjunct [emin; emax] p.loc)#node

    | Pvalid(TSSingleton(TSAdd_index(t1,t2))) ->
	let e1 = tsets_elem t1 in
	let e2 = term t2 in
	let eoffmin = mkexpr (JCPEoffset(Offset_min,e1)) p.loc in
	let emin = mkexpr (JCPEbinary(eoffmin,`Ble,e2)) p.loc in
	let eoffmax = mkexpr (JCPEoffset(Offset_max,e1)) p.loc in
	let emax = mkexpr (JCPEbinary(eoffmax,`Bge,e2)) p.loc in
	(mkconjunct [emin; emax] p.loc)#node

    | Pvalid_range(t1,t2,t3) ->
	let e1 = term t1 in
	let e2 = term t2 in
	let e3 = term t3 in
	let eoffmin = mkexpr (JCPEoffset(Offset_min,e1)) p.loc in
	let emin = mkexpr (JCPEbinary(eoffmin,`Ble,e2)) p.loc in
	let eoffmax = mkexpr (JCPEoffset(Offset_max,e1)) p.loc in
	let emax = mkexpr (JCPEbinary(eoffmax,`Bge,e3)) p.loc in
	(mkconjunct [emin; emax] p.loc)#node

    | Pvalid(TSSingleton(TSAdd_range(t1,t2,t3))) ->
	let e1 = tsets_elem t1 in
	begin match t2,t3 with
	  | None,None -> true_expr#node
	  | Some t2,None ->
	      let e2 = term t2 in
	      let eoffmin = mkexpr (JCPEoffset(Offset_min,e1)) p.loc in
	      JCPEbinary(eoffmin,`Ble,e2)
	  | None, Some t3 ->
	      let e3 = term t3 in
	      let eoffmax = mkexpr (JCPEoffset(Offset_max,e1)) p.loc in
	      JCPEbinary(eoffmax,`Bge,e3)
	  | Some t2,Some t3 ->
	      let e2 = term t2 in
	      let e3 = term t3 in
	      let eoffmin = mkexpr (JCPEoffset(Offset_min,e1)) p.loc in
	      let emin = mkexpr (JCPEbinary(eoffmin,`Ble,e2)) p.loc in
	      let eoffmax = mkexpr (JCPEoffset(Offset_max,e1)) p.loc in
	      let emax = mkexpr (JCPEbinary(eoffmax,`Bge,e3)) p.loc in
	      (mkconjunct [emin; emax] p.loc)#node
	end

    | Pvalid t ->
	let elist =
	  List.flatten (List.map (fun e ->
	    let eoffmin = mkexpr (JCPEoffset(Offset_min,e)) p.loc in
	    let emin = mkexpr (JCPEbinary(eoffmin,`Ble,zero_expr)) p.loc in
	    let eoffmax = mkexpr (JCPEoffset(Offset_max,e)) p.loc in
	    let emax = mkexpr (JCPEbinary(eoffmax,`Bge,zero_expr)) p.loc in
	    [emin; emax]
	  ) (tsets t))
	in
	(mkconjunct elist p.loc)#node

    | Pfresh _t -> assert false (* TODO: add to memory model for Jessie *)

    | Psubtype({term_node = Ttypeof t},{term_node = Ttype ty}) ->
	JCPEinstanceof(term t,get_struct_name (pointed_type ty))

    | Psubtype(_t1,_t2) -> assert false (* TODO *)
    | Pseparated(_seps) -> assert false (* TODO *)
  in
  mkexpr enode p.loc

(* Keep names associated to predicate *)
let named_pred p =
  List.fold_right
    (fun lab p -> mkexpr (JCPElabel(lab,p)) p#pos) p.name (pred p)

let pred_has_name p n =
  List.exists (fun n2 -> n = n2) p.name

let remove_pred_name p n =
  { p with name = List.filter (fun n2 -> not (n = n2)) p.name }

let conjunct pos pl =
  mkconjunct (List.map (pred $ Logic_const.pred_of_id_pred) pl) pos

let zone = function
  | Location tset,_from -> tsets tset.its_content
  | Nothing,_from -> []

(* Distinguish between:
 * - no assign, which is the empty list in Cil and None in Jessie
 * - assigns nothing, which is [Nothing] in Cil and the Some[] in Jessie
 *)
let assigns = function
  | [] -> None
  | assign_list ->
      let assign_list =
        List.filter
          (function
               Location out,_ ->
                 not (Logic_const.tsets_is_result out.its_content)
             | Nothing, _ -> false)
          assign_list
      in
      let assign_list = List.flatten (List.map zone assign_list) in
      Some(Loc.dummy_position,assign_list)

let spec funspec =
  let require p =
    JCCrequires(locate (pred (Logic_const.pred_of_id_pred p)))
  in
  let requires = List.map require funspec.spec_requires in
  let behavior b =
    JCCbehavior(
      Loc.dummy_position,
      b.b_name,
      None, (* throws *)
      Some(conjunct Loc.dummy_position b.b_assumes),
      None, (* requires *)
      assigns b.b_assigns,
      locate (conjunct Loc.dummy_position b.b_ensures))
  in
  let behaviors = List.map behavior funspec.spec_behavior in

  if funspec.spec_complete_behaviors <> [] then
    Errormsg.warn "[Jessie plugin] complete behaviors specification ignored";
  if funspec.spec_disjoint_behaviors <> [] then
    Errormsg.warn "[Jessie plugin] disjoint behaviors specification ignored";
  if funspec.spec_variant <> None then
    Errormsg.warn "[Jessie plugin] variant for recursive function ignored";
  if funspec.spec_terminates <> None then
    Errormsg.warn "[Jessie plugin] termination condition ignored";

  (* TODO: translate function spec variant, terminates and complete/disjoint
     behaviors *)
  requires @ behaviors

(* Depending on the argument status, an assertion with this status may
   not generate any PO but still be used as an hypothesis. *)
let assertion = function
  | { status = Checked { valid = True } } -> Aassume
  | _ -> Aassert

let assert_ pos = function
  | WP _ -> []
  | User annot ->
      begin match annot.annot_content with
	| AAssert (behav,p,status) ->
	    let asrt = assertion status in
            [mkexpr (JCPEassert (behav,asrt,locate ~pos (named_pred p))) pos]
	| AInvariant(behav,_,p) ->
	    [mkexpr (JCPEassert
		       (behav,Aassert,locate ~pos (named_pred p))) pos]
	| _ -> assert false
      end
  | AI(alarm,annot) ->
      begin match annot.annot_content with
	| AAssert (behav,p,status) ->
	    let asrt =
	      if pred_has_name p name_of_hint_assertion then Ahint
	      else assertion status
	    in
	    let p = remove_pred_name p name_of_hint_assertion in
	    let behav =
	      if behav = [] then [ name_of_default_behavior ] else behav
	    in
	    [mkexpr (JCPEassert (behav,asrt,locate ~alarm ~pos (named_pred p))) pos]
	| AInvariant(behav,_,p) ->
	    let behav =
	      if behav = [] then [ name_of_default_behavior ] else behav
	    in
	    [mkexpr (JCPEassert
		       (behav,Aassert,locate ~alarm ~pos (named_pred p))) pos]
	| _ -> assert false
      end

let invariant annot =
  match annot.annot_content with
    | AInvariant(behav,_loopinv,p) -> behav, locate (pred p)
    | _ -> assert false

let variant annot =
  match annot.annot_content with
    | AVariant(t,_) -> locate (term t)
    | _ -> assert false


(*****************************************************************************)
(* Cil to Jessie translation of coding constructs                            *)
(*****************************************************************************)

let set_curFundec, get_curFundec =
  let cf = ref None in
  ((fun f -> cf := Some f),
   (fun () ->
      match !cf with
          None ->
            let res = emptyFunction "@empty@" in cf := Some res; res
        | Some res -> res))

let rec expr pos e =
  (* Precise the location if possible *)
  let pos = match e with Info(_,einfo) -> einfo.exp_loc | _ -> pos in

  let expr = expr pos in
  let integral_expr = integral_expr pos in

  let enode = match stripInfo e with
    | Info _ -> assert false

    | Const c -> const ~in_code:true pos c

    | Lval lv -> (lval pos lv)#node

    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
	assert false (* Should be removed by constant folding *)

    | UnOp(_op,_e,ty) when isIntegralType ty ->
	(integral_expr e)#node

    | UnOp(op,e,_ty) ->
	let e =
	  locate (mkexpr (JCPEunary(unop op,expr e)) pos)
	in
	e#node

    | BinOp(_op,_e1,_e2,ty) when isIntegralType ty ->
	(integral_expr e)#node

    | BinOp(op,e1,e2,_ty) ->
	let e =
	  locate (mkexpr (JCPEbinary(expr e1,binop op,expr e2)) pos)
	in
	e#node

    | CastE(ty,e') when isIntegralType ty && isArithmeticType (typeOf e') ->
	(integral_expr e)#node

    | CastE(ty,e) when isFloatingType ty && isArithmeticType (typeOf e) ->
	let e = locate (mkexpr (JCPEcast(expr e,ctype ty)) pos) in
	e#node

    | CastE(ty,e') when isIntegralType ty && isPointerType (typeOf e') ->
(* 	&& bits_sizeof ty = bits_sizeof (typeOf e') -> *)
(* 	let _,ptr_to_int = type_conversion ty (typeOf e') in *)
(* 	JCPEapp(ptr_to_int,[],[expr e']) *)
	Errormsg.s
	  (Cil.error "Casting from type %a to type %a not allowed"
	     !Ast_printer.d_type (typeOf e') !Ast_printer.d_type ty)

    | CastE(ptrty,_e1) when isPointerType ptrty ->
	begin match stripCastsAndInfo e with
	  | Const c
	      when is_integral_const c
		&& value_of_integral_const c = Int64.zero ->
	      JCPEconst JCCnull
	  | e ->
	      let ety = typeOf e in
	      if isIntegralType ety(*  && bits_sizeof ety = bits_sizeof ptrty *) then
(* 		let _,int_to_ptr = type_conversion ptrty ety in *)
(* 		JCPEapp(int_to_ptr,[],[integral_expr e]) *)
		Errormsg.s
		  (Cil.error "Casting from type %a to type %a not allowed"
		     !Ast_printer.d_type (typeOf e) !Ast_printer.d_type ptrty)
	      else if isPointerType ety then
(* 		let destty = pointed_type ptrty in *)
(* 		let srcty = pointed_type ety in *)
(* 		if Retype.subtype srcty destty then *)
(* 		  (expr e)#node *)
(* 		else if Retype.TypeUnion.same_class destty srcty then *)
(* 		  let enode = JCPEcast(expr e,ctype ptrty) in *)
(* 		  let e = locate (mkexpr enode pos) in *)
(* 		  e#node *)
(* 		else *)
		  (* bitwise cast *)
		  let enode = JCPEcast(expr e,ctype ptrty) in
		  let e = locate (mkexpr enode pos) in
		  e#node
(* 		  let _,ptr_to_ptr = type_conversion ptrty ety in *)
(* 		  JCPEapp(ptr_to_ptr,[],[expr e]) *)
	      else
		(* Only hierarchical types are available in Jessie. It
		 * should have been encoded as the use of pointer types
		 * on structure type.
		 *)
(* 	      match unrollType ty with *)
(* 		| TComp(compinfo,_) -> *)
(* 		    JCPEcast(expr (stripCasts e),compinfo.cname) *)
(* 		| _ -> assert false *)
		Errormsg.s
		  (Cil.error "Casting from type %a to type %a not allowed"
		     !Ast_printer.d_type (typeOf e) !Ast_printer.d_type ptrty)
	end

    | CastE(ty,e) as caste ->
	(* TODO: support other casts in Jessie as well, through low-level
	 * memory model
	 *)
	Errormsg.s
	  (Cil.error "Casting from type %a to type %a not allowed in %a with size %Ld and %Ld"
	     !Ast_printer.d_type (typeOf e) !Ast_printer.d_type ty
	     !Ast_printer.d_exp caste
	   ( bits_sizeof ty) ( bits_sizeof (typeOf e))
)

    | AddrOf _lv ->
	Format.printf "%a@." !Ast_printer.d_lval _lv;
assert false (* Should have been rewritten *)

    | StartOf lv -> (lval pos lv)#node
  in
  mkexpr enode pos

(* Function called when expecting a boolean in Jessie, i.e. when translating
   a test or a sub-expression of an "or" or "and".
*)
and boolean_expr pos e =
  (* Precise the posation if possible *)
  let pos = match e with Info(_,einfo) -> einfo.exp_loc | _ -> pos in

  let expr = expr pos in
  let boolean_expr = boolean_expr pos in
  let boolean_node_from_expr ty e =
    if isPointerType ty then JCPEbinary(e,`Bneq,null_expr)
    else if isArithmeticType ty then JCPEbinary (e,`Bneq,zero_expr)
    else assert false
  in

  let enode = match stripInfo e with
    | Info _ -> assert false

    | Const c -> JCPEconst(boolean_const c)

    | UnOp(LNot,e,_typ) -> JCPEunary(unop LNot,boolean_expr e)

    | BinOp((LAnd | LOr) as op,e1,e2,_typ) ->
	JCPEbinary(boolean_expr e1,binop op,boolean_expr e2)

    | BinOp((Eq | Ne) as op,e1,e2,_typ) ->
	JCPEbinary(expr e1,binop op,expr e2)

    | BinOp((Lt | Gt | Le | Ge) as op,e1,e2,_typ) ->
	let ty = typeOf e1 in
	if isArithmeticType ty then
	  JCPEbinary(expr e1,binop op,expr e2)
	else
          (* Pointer comparison is translated as subtraction *)
          let sube = mkexpr (JCPEbinary(expr e1,`Bsub,expr e2)) pos in
          JCPEbinary(sube,binop op,zero_expr)

    | _ -> boolean_node_from_expr (typeOf e) (expr e)
  in
  mkexpr enode pos

(* Function called instead of plain [expr] when the evaluation result should
 * fit in a C integral type.
 *)
and integral_expr pos e =

  let rec int_expr pos e =
    let expr = expr pos in
    let int_expr = int_expr pos in
    let boolean_expr = boolean_expr pos in
    let node_from_boolean_expr e = JCPEif(e,one_expr,zero_expr) in

    let enode = match e with
      | UnOp(LNot,e,_ty) ->
	  let e = mkexpr (JCPEunary(unop LNot,boolean_expr e)) pos in
	  node_from_boolean_expr e

      | UnOp(op,e,_ty) ->
	  let e =
	    locate (mkexpr (JCPEunary(unop op,expr e)) pos)
	  in
	  e#node

      | BinOp((LAnd | LOr) as op,e1,e2,_ty) ->
	  let e =
	    mkexpr (JCPEbinary(boolean_expr e1,binop op,boolean_expr e2)) pos
	  in
	  node_from_boolean_expr e

      | BinOp((Lt | Gt | Le | Ge as op),e1,e2,_ty)
	  when isPointerType (typeOf e1) ->
	  (* Pointer comparison is translated as subtraction *)
	  let sube = mkexpr (JCPEbinary(expr e1,`Bsub,expr e2)) pos in
	  let e = mkexpr (JCPEbinary(sube,binop op,zero_expr)) pos in
	  node_from_boolean_expr e

(*       | BinOp((Eq | Ne as op),e1,e2,_ty) *)
(* 	  when isPointerType (typeOf e1) && *)
(* 	    not (is_null_expr e2 || is_null_expr e1) -> *)
(* 	  (\* Pointer (dis-)equality is translated as subtraction *\) *)
(* 	  let sube = mkexpr (JCPEbinary(expr e1,`Bsub,expr e2)) pos in *)
(* 	  let e = mkexpr (JCPEbinary(sube,binop op,zero_expr)) pos in *)
(* 	  node_from_boolean_expr e *)

      | BinOp((Eq | Ne) as op,e1,e2,_ty) ->
	  let e = mkexpr (JCPEbinary(expr e1,binop op,expr e2)) pos in
	  node_from_boolean_expr e

      | BinOp((Lt | Gt | Le | Ge) as op,e1,e2,_ty) ->
	  let e = mkexpr (JCPEbinary(expr e1,binop op,expr e2)) pos in
	  node_from_boolean_expr e

      | BinOp(Shiftrt,e1,e2,_ty) ->
	  let e = match possible_value_of_integral_expr e2 with
	    | Some i when i >= 0L && i < 63L ->
		(* Right shift by constant is division by constant *)
		let pow = constant_expr (power_of_two i) in
		locate (mkexpr (JCPEbinary(expr e1,`Bdiv,expr pow)) pos)
	    | _ ->
		let op =
		  if isSignedInteger (typeOf e1) then `Barith_shift_right
		  else `Blogical_shift_right
		in
		locate (mkexpr (JCPEbinary(expr e1,op,expr e2)) pos)
	  in
	  e#node

      | BinOp(Shiftlt as op,e1,e2,_ty) ->
	  let e = match possible_value_of_integral_expr e2 with
	    | Some i when i >= 0L && i < 63L ->
		(* Left shift by constant is multiplication by constant *)
		let pow = constant_expr (power_of_two i) in
		locate (mkexpr (JCPEbinary(expr e1,`Bmul,expr pow)) pos)
	    | _ ->
		locate (mkexpr (JCPEbinary(expr e1,binop op,expr e2)) pos)
	  in
	  e#node

      | BinOp(op,e1,e2,_ty) ->
	  let e =
	    locate (mkexpr (JCPEbinary(expr e1,binop op,expr e2)) pos)
	  in
	  e#node

      | CastE(ty,e1) when isFloatingType (typeOf e1) ->
	  let e1' = locate (mkexpr (JCPEcast(expr e1,ltype Linteger)) pos) in
	  if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	    e1'#node
	  else
	    let e2' = locate (mkexpr (JCPEcast(e1',ctype ty)) pos) in
	    e2'#node

      | CastE(ty,e) when isIntegralType (typeOf e) ->
	  if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	    (int_expr e)#node
	  else
	    let e = locate (mkexpr (JCPEcast(int_expr e,ctype ty)) pos) in
	    e#node

      | _ -> (expr e)#node
    in
    mkexpr enode pos
  in

  match e with
    | CastE _ -> int_expr pos e
    | _ -> int_expr pos (CastE(typeOf e,e))

and lval pos = function
  | Var v, NoOffset -> mkexpr (JCPEvar v.vname) pos

  | Var _v, _off -> assert false (* Should have been rewritten *)

  | Mem _e, NoOffset -> assert false (* Should have been rewritten *)

  | Mem e, Field(fi,off) ->
      assert (off = NoOffset); (* Others should have been rewritten *)
      let e = expr pos e in
      if not fi.fcomp.cstruct then (* field of union *)
	locate (mkexpr (JCPEcast(e,ctype fi.ftype)) pos)
      else
	let repfi = Retype.FieldUnion.repr fi in
	let e,fi =
	  if FieldinfoComparable.equal fi repfi then
	    e,fi
	  else
	    let caste =
	      locate
		(mkexpr
		   (JCPEcast(e,ctype (TPtr(TComp(repfi.fcomp,[]),[])))) pos)
	    in
	    caste,repfi
	in
	locate (mkexpr (JCPEderef(e,fi.fname)) pos)

  | Mem e, Index(ie,Field(fi,off)) ->
      assert (off = NoOffset); (* Others should have been rewritten *)
      (* Normalization made it equivalent to simple add *)
      let e = mkexpr (JCPEbinary(expr pos e,`Badd,expr pos ie)) pos in
      if not fi.fcomp.cstruct then (* field of union *)
	locate (mkexpr (JCPEcast(e,ctype fi.ftype)) pos)
      else
	let repfi = Retype.FieldUnion.repr fi in
	let e,fi =
	  if FieldinfoComparable.equal fi repfi then
	    e,fi
	  else
	    let caste =
	      locate
		(mkexpr
		   (JCPEcast(e,ctype (TPtr(TComp(repfi.fcomp,[]),[])))) pos)
	    in
	    caste,repfi
	in
	locate (mkexpr (JCPEderef(e,fi.fname)) pos)

  | Mem _e, Index _ as lv ->
      Format.printf "lval %a@." !Ast_printer.d_lval lv;
      assert false (* Should have been rewritten *)

let keep_only_declared_nb_of_arguments vi l =
  let _,args,is_variadic, _ = splitFunctionTypeVI vi in
  if args=None then
    (warn "skipping all arguments of implicit prototype %s" vi.vname;
     [])
  else if is_variadic then bug "unsupported variadic functions"
  else l

let rec instruction = function
  | Set(lv,e,pos) ->
      let enode = JCPEassign(lval pos lv,expr pos e) in
      (locate (mkexpr enode pos))#node

  | Call(None,Lval(Var v,NoOffset),eargs,pos) ->
      if is_assert_function v then
	JCPEassert([],Aassert,locate (boolean_expr pos (as_singleton eargs)))
      else
	let enode =
	  if is_free_function v then
	    let arg = as_singleton eargs in
	    let subarg = stripCasts arg in
	    let arg = if isPointerType (typeOf subarg) then subarg else arg in
	    JCPEfree(expr pos arg)
	  else
	    JCPEapp(v.vname,[],
		    keep_only_declared_nb_of_arguments
		      v
		      (List.map (expr pos) eargs))
	in
	(locate (mkexpr enode pos))#node

  | Call(Some lv,Lval(Var v,NoOffset),eargs,pos) ->
      let enode =
	if is_malloc_function v || is_realloc_function v then
	  let lvtyp = pointed_type (typeOfLval lv) in
	  let lvsiz = (bits_sizeof lvtyp) lsr 3 in
	  let arg =
	    if is_malloc_function v then as_singleton eargs
	    else (* realloc *)
	      match eargs with [ _; arg ] -> arg | _ -> assert false
	  in
	  let ty,arg = match stripInfo arg with
	    | Info _ -> assert false
	    | Const c as arg when is_integral_const c ->
		let allocsiz = (value_of_integral_expr arg) / lvsiz in
		let siznode = JCPEconst(JCCinteger(Int64.to_string allocsiz)) in
		lvtyp, mkexpr siznode pos
	    | BinOp(Mult,(Const c as arg),nelem,_ty)
	    | BinOp(Mult,nelem,(Const c as arg),_ty) when is_integral_const c ->
		let factor = (value_of_integral_expr arg) / lvsiz in
		let siz =
		  if factor = Int64.one then
		    expr pos nelem
		  else
		    let factor = constant_expr factor in
		    expr pos (BinOp(Mult,nelem,factor,typeOf arg))
		in
		lvtyp, siz
	    | arg ->
		if lvsiz = Int64.one then
		  lvtyp, expr pos arg
		else
		  let esiz = constant_expr lvsiz in
		  lvtyp, expr pos (BinOp(Div,arg,esiz,typeOf arg))
	  in
	  let name_of_type = match unrollType ty with
	    | TComp(compinfo,_) -> compinfo.cname
	    | _ -> assert false
	  in
	  JCPEalloc(arg,name_of_type)
	else if is_calloc_function v then
	  let nelem,elsize = match eargs with
	    | [nelem;elsize] -> nelem,elsize
	    | _ -> assert false
	  in
	  let ty,arg = match stripInfo elsize with
	    | Info _ -> assert false
	    | Const c as arg when is_integral_const c ->
		let lvtyp = pointed_type (typeOfLval lv) in
		let lvsiz = (bits_sizeof lvtyp) lsr 3 in
		let factor = (value_of_integral_expr arg) / lvsiz in
		let siz =
		  if factor = Int64.one then
		    expr pos nelem
		  else
		    let factor = constant_expr factor in
		    expr pos (BinOp(Mult,nelem,factor,typeOf arg))
		in
		lvtyp, siz
	    | arg ->
		let lvtyp = pointed_type (typeOfLval lv) in
		let lvsiz = (bits_sizeof lvtyp) lsr 3 in
		let esiz = constant_expr lvsiz in
		lvtyp, expr pos (BinOp(Div,
				       BinOp(Mult,nelem,elsize,typeOf arg),
				       esiz,typeOf arg))
	  in
	  let name_of_type = match unrollType ty with
	    | TComp(compinfo,_) -> compinfo.cname
	    | _ -> assert false
	  in
	  JCPEalloc(arg,name_of_type)
	else
	  JCPEapp(v.vname,[],
		  keep_only_declared_nb_of_arguments
		    v
		    (List.map (expr pos) eargs))
      in
      let lvty = typeOfLval lv in
      let call = locate (mkexpr enode pos) in
      let enode =
	if TypeComparable.equal lvty (getReturnType v.vtype)
	  || is_malloc_function v
	  || is_realloc_function v
	  || is_calloc_function v
	then
	  JCPEassign(lval pos lv,call)
	else
	  let tmpv = makeTempVar (get_curFundec()) (getReturnType v.vtype) in
	  let tmplv = Var tmpv, NoOffset in
	  let cast = CastE(lvty,Lval tmplv) in
	  let tmpassign = JCPEassign(lval pos lv,expr pos cast) in
	  JCPElet(None,tmpv.vname,Some call,locate (mkexpr tmpassign pos))
      in
      (locate (mkexpr enode pos))#node

  | Call _ -> assert false

  | Asm _ -> assert false

  | Skip _pos -> JCPEconst JCCvoid

  | Code_annot _ -> JCPEconst JCCvoid
      (* Annotations should be retrieved from Db *)

let rec statement s =
  let pos = get_stmtLoc s.skind in

  let assert_list =
    Annotations.get_filter Logic_const.is_assert s
    @ Annotations.get_filter Logic_const.is_stmt_invariant s
  in
  let assert_before,assert_after =
    List.partition (function Before _ -> true | After _ -> false) assert_list
  in
  let assert_before =
    List.flatten (List.map ((assert_ pos) $ before_after_content) assert_before)
  in
  let assert_after =
    List.flatten (List.map ((assert_ pos) $ before_after_content) assert_after)
  in

  let snode = match s.skind with
    | Instr i -> instruction i

    | Return(Some e,pos) ->
	JCPEreturn(expr pos e)

    | Return(None,_pos) ->
	JCPEreturn(mkexpr (JCPEconst JCCvoid) pos)

    | Goto(sref,_pos) ->
	(* Pick the first non-case label in the list of labels associated to
	 * the target statement
	 *)
	let labels = filter_out is_case_label !sref.labels in
	assert (not (labels = []));
	JCPEgoto(label (List.hd labels))

    | Break _pos ->
	assert false (* Should not occur after [prepareCFG] *)

    | Continue _pos ->
	assert false (* Should not occur after [prepareCFG] *)

    | If(e,bl1,bl2,pos) ->
	JCPEif(boolean_expr pos e,block bl1,block bl2)

    | Switch(e,bl,slist,pos) ->
	let case_blocks stat_list case_list =
	  let rec next_case curr_list final_list stat_list case_list =
	    match stat_list,case_list with
	      | curr_stat :: rest_stat, curr_case :: rest_case ->
		  if curr_case.sid = curr_stat.sid then
		    (* Beginning of a new case. Add previous case to list
		       if not empty. *)
		    let add_to_list cond e l = if cond e then e::l else l in
		    let not_empty_list = function [] -> false | _ -> true in
		    let final_list =
		      add_to_list not_empty_list (List.rev curr_list) final_list
		    in
		    let curr_list = [curr_stat] in
		    next_case curr_list final_list rest_stat rest_case
		  else
		    let curr_list = curr_stat :: curr_list in
		    next_case curr_list final_list rest_stat case_list
	      | [], [] ->
		  if List.length curr_list <> 0 then
		    List.rev curr_list :: final_list
		  else
		    final_list
	      | [], _ ->
		  (* More cases than available. *)
		  assert false
	      | stat_list, [] ->
		  (List.rev_append curr_list stat_list) :: final_list
	  in
	  List.rev (next_case [] [] stat_list case_list)
	in
	let switch_label = function
	  | Label _ -> assert false
	  | Case(e,pos) -> Some(expr pos e)
	  | Default _ -> None
	in
	let case = function
	  | [] -> assert false
	  | case_stmt :: _ as slist ->
	      let switch_labels = List.filter is_case_label case_stmt.labels in
	      let labs = List.map switch_label switch_labels in
	      let slist = mkexpr (JCPEblock(statement_list slist)) pos in
	      labs, slist
	in
	let case_list = List.map case (case_blocks bl.bstmts slist) in
	JCPEswitch(expr pos e,case_list)

    | Loop (_,bl,_pos,_continue_stmt,_break_stmt) ->
	let invariant_list =
	  Annotations.get_filter Logic_const.is_invariant s
	in
	let invariant_list =
	  lift_annot_list_func (List.map invariant) invariant_list
	in

	let variant_list = Annotations.get_filter Logic_const.is_variant s in
	let variant_list =
	  lift_annot_list_func (List.map variant) variant_list
	in
	(* At most one variant *)
	let variant = match variant_list with
	  | [] -> None
	  | [e] -> Some e
	  | _ -> assert false
	in
	(* Issue a warning on loop assigns not yet supported *)
	let assigns_list = Annotations.get_filter Logic_const.is_assigns s in
	if assigns_list <> [] then
	  Errormsg.warn "[Jessie plugin] loop assigns are ignored";

	(* Locate the beginning of the loop, to serve as location for generated
	 * invariants and variants.
	 *)
(* 	let lab = reg_pos pos in *)
	(* TODO: add loop-assigns to Jessie *)
	JCPEwhile(true_expr,invariant_list,variant,block bl)

    | Block bl ->
	JCPEblock(statement_list bl.bstmts)

    | UnspecifiedSequence seq ->
        (*[VP]TODO: take into account undefined behavior tied to the
          effects of the statements...
         *)
	JCPEblock(statement_list (List.map (fun (x,_,_) -> x) seq))

    | TryFinally _ | TryExcept _ -> assert false
  in
  (* Prefix statement by all non-case labels *)
  let labels = filter_out is_case_label s.labels in
  let s = mkexpr snode pos in
  let s = match assert_before @ s :: assert_after with
    | [s] -> s
    | slist -> mkexpr (JCPEblock slist) pos
  in
  List.fold_left (fun s lab -> mkexpr (JCPElabel(label lab,s)) pos) s labels

and statement_list slist = List.rev (List.rev_map statement slist)

and block bl =
  match bl.bstmts with
    | [] -> mkexpr (JCPEconst JCCvoid) Loc.dummy_position
    | [s] -> statement s
    | slist -> mkexpr (JCPEblock(statement_list slist)) Loc.dummy_position


(*****************************************************************************)
(* Cil to Jessie translation of global declarations                          *)
(*****************************************************************************)

let logic_variable v =
  let name = opt_app (fun v -> v.vname) v.lv_name v.lv_origin in
  ltype v.lv_type, name

let rec annotation = function
  | Dfun_or_pred info ->
      begin try
	let params = List.map logic_variable info.l_profile in
	let body =
	  match info.l_body with
	    | LBreads reads_tsets ->
	      	let reads = List.flatten (List.map tsets reads_tsets) in
		JCreads reads
	    | LBpred p -> JCexpr(pred p)
	    | LBinductive indcases ->
		let l = List.map
		  (fun (id,labs,_poly,p) ->
		     (new identifier id,logic_labels labs,pred p)) indcases
		in
		JCinductive l
	    | LBterm t -> JCexpr(term t)
	in
	[JCDlogic(Option_misc.map ltype info.l_type,
		  info.l_name,
		  logic_labels info.l_labels,
		  params,body)]
      with Errormsg.Error ->
	Format.printf "Dropping declaration of predicate %s@." info.l_name;
	[]
      end

(*
  | Dpredicate_reads(info,_poly,params,reads_tsets) ->
      begin try
	let params = List.map logic_variable params in
	let reads = List.flatten (List.map tsets reads_tsets) in
	[JCDlogic(None,info.p_name,logic_labels info.p_labels,params,JCreads reads)]
      with Errormsg.Error ->
	Format.printf "Dropping declaration of predicate %s@." info.p_name;
	[]
      end

  | Dpredicate_def(info,_poly,params,def) ->
      begin try
	let params = List.map logic_variable params in
	[JCDlogic(None,info.p_name,logic_labels info.p_labels,params,JCexpr(pred def))]
      with Errormsg.Error ->
	Format.printf "Dropping definition of predicate %s@." info.p_name;
	[]
      end

  | Dinductive_def(info,_poly,params,indcases) ->
      begin try
	let params = List.map logic_variable params in
	let l = List.map
	  (fun (id,p) ->
	     (new identifier id,pred p)) indcases
	in
	[JCDlogic(None,info.p_name,logic_labels info.p_labels,params,JCinductive l)]
      with Errormsg.Error ->
	Format.printf "Dropping definition of predicate %s@." info.p_name;
	[]
      end

  | Dlogic_reads(info,_poly,params,return_type,reads_tsets) ->
      begin try
	let params = List.map logic_variable params in
	let reads = List.flatten (List.map tsets reads_tsets) in
	[JCDlogic(
	   Some(ltype return_type),
           info.l_name,logic_labels info.l_labels,params,JCreads reads)]
      with Errormsg.Error ->
	Format.printf "Dropping declaration of logic function %s@." info.l_name;
	[]
      end

  | Dlogic_def(info,_poly,params,return_type,def) ->
      begin try
	let params = List.map logic_variable params in
	[JCDlogic(
	   Some(ltype return_type),info.l_name,logic_labels info.l_labels,params,
           JCexpr(term def))]
      with Errormsg.Error ->
	Format.printf "Dropping definition of logic function %s@." info.l_name;
	[]
      end

  | Dlogic_axiomatic(info,_poly,params,return_type,axioms) ->
      begin try
	let params = List.map logic_variable params in
	let l = List.map
	  (fun (id,p) ->
	     (new identifier id,pred p)) axioms
	in
	[JCDlogic(
	   Some(ltype return_type),info.l_name,logic_labels info.l_labels,
	   params, JCaxiomatic l)]
      with Errormsg.Error ->
	Format.printf "Dropping definition of logic function %s@." info.l_name;
	[]
      end
*)
  | Dlemma(name,is_axiom,labels,_poly,property) ->
      begin try
	[JCDlemma(name,is_axiom,logic_labels labels,pred property)]
      with Errormsg.Error ->
	Format.printf "Dropping lemma %s@." name;
	[]
      end

  | Dinvariant property ->
      begin try
	[JCDglobal_inv(property.l_name,
                       pred (Logic_const.get_pred_body property))]
      with Errormsg.Error ->
	Format.printf "Dropping invariant %s@." property.l_name;
	[]
      end

  | Dtype_annot annot ->
      begin try
	[JCDlogic(
	   None,annot.l_name, logic_labels annot.l_labels,
	   List.map logic_variable annot.l_profile,
           JCexpr(pred (Logic_const.get_pred_body annot)))]
      with Errormsg.Error ->
	Format.printf "Dropping type invariant %s@." annot.l_name;
	[]
      end

  | Dtype(name,[]) -> [JCDlogic_type name]

  | Dtype(_name,_) -> assert false (* TODO *)

  | Daxiomatic(id,l) ->
(*
      Format.eprintf "Translating axiomatic %s into jessie code@." id;
*)
      let l = List.fold_left (fun acc d -> (annotation d)@acc) [] l in
      [JCDaxiomatic(id,List.map (fun d -> mkdecl  d Loc.dummy_position )
		      (List.rev l))]

let global vardefs g =
  let dnodes = match g with
    | GType _ -> [] (* No typedef in Jessie *)

    | GCompTag(compinfo,pos) when compinfo.cstruct -> (* struct type *)
	let field fi =
	  let this = 
	    false, ctype ?bitsize:fi.fsize_in_bits fi.ftype, 
	    fi.fname, fi.fsize_in_bits 
	  in
	  let padding_size =
	    match fi.fpadding_in_bits with None -> assert false | Some i -> i
	  in
	  if padding_size = 0 then [this] else
	    let padding =
	      false, type_of_padding, unique_name "padding", fi.fpadding_in_bits
	    in
	    [this;padding]
	in
	let fields =
	  List.fold_right (fun fi acc ->
			     let repfi = Retype.FieldUnion.repr fi in
			     if FieldinfoComparable.equal fi repfi then
			       field fi @ acc
			     else acc
			  ) compinfo.cfields []
	in
	let _parent = None in
(* 	  find_or_none (Hashtbl.find Norm.type_to_parent_type) compinfo.cname *)
(* 	in *)
	let ty = TComp(compinfo,[]) in
	begin try
	  let parentty = TypeHashtbl.find Retype.type_to_parent_type ty in
	  let parent = get_struct_name parentty in
	  [
	    JCDtag(compinfo.cname,[],Some (parent,[]),fields,[])
	  ]
	with Not_found ->
	  try
	    ignore(TypeHashtbl.find Norm.generated_union_types ty);
	    [JCDtag(compinfo.cname,[],None,fields,[])]
	  with Not_found ->
	    let id = mkidentifier compinfo.cname pos in
	    [
	      JCDtag(compinfo.cname,[],None,fields,[]);
	      JCDvariant_type(compinfo.cname,[id])
	    ]
	end

    | GCompTag(compinfo,pos) -> (* union type *)
	assert (not compinfo.cstruct);
	let field fi =
	  let ty = pointed_type fi.ftype in
	  mkidentifier (get_struct_name ty) pos
	in
(* 	  match pointed_type fi.ftype with *)
(* 	    | TComp(compinfo,_) -> *)
(* 		let field fi = false, ctype fi.ftype, fi.fname in *)
(* 		let fields = List.map field compinfo.cfields in *)
(* (\* 		let parent = *\) *)
(* (\* 		  find_or_none (Hashtbl.find Norm.type_to_parent_type) *\) *)
(* (\* 		    compinfo.cname *\) *)
(* (\* 		in *\) *)
(* 		mkidentifier fi.fname fi.floc,  *)
(* 		JCDtag(fi.fname,[],None,fields,[]) *)
(* 	    | _ ->  *)
(* 		assert false *)
(* 	in *)
	let union_id = mkidentifier compinfo.cname pos in
	let union_size = match compinfo.cfields with
	  | [] -> 0
	  | fi::_ ->
	      Pervasives.(+) (the fi.fsize_in_bits) (the fi.fpadding_in_bits)
	in
	let padding =
	  if union_size = 0 then [] else
	    [false, type_of_padding, unique_name "padding", Some union_size]
	in
	let union_tag = JCDtag(compinfo.cname,[],None,padding,[]) in
	let fields = List.map field compinfo.cfields in
	let rec has_pointer ty =
	  match unrollType ty with
	    | TComp(compinfo,_attr) ->
		List.exists (fun fi -> has_pointer fi.ftype) compinfo.cfields
	    | TPtr _ ->
		if is_reference_type ty then 
		  (* Possibly skip intermediate array type *)
		  has_pointer (pointed_type ty) 
		else true
	    | TVoid _
	    | TInt _
	    | TFloat _
	    | TEnum _ -> false
	    | TArray _ -> assert false (* Removed by translation *)
	    | TFun _ ->
		Errormsg.s
		  (Cil.error "Function pointer type %a not allowed"
		     !Ast_printer.d_type ty)
	    | TNamed _ -> assert false
	    | TBuiltin_va_list _ -> assert false (* not supported *)
	in
	(* Union type with pointer as sub-field should be used as a
	   discriminated union *)
	let discr = has_pointer (TComp(compinfo,[])) in
	[JCDunion_type(compinfo.cname,discr,union_id :: fields); union_tag]

    | GCompTagDecl _ -> [] (* No struct/union declaration in Jessie *)

    | GEnumTag(enuminfo,_pos) ->
	assert (not (enuminfo.eitems = []));
	let enums =
	  List.map
            (fun {eival = enum} -> value_of_integral_expr enum) enuminfo.eitems
	in
	let emin =
	  List.fold_left (fun acc enum ->
			    if acc < enum then acc else enum) (List.hd enums) enums
	in
	let min = Num.num_of_string (Int64.to_string emin) in
	let emax =
	  List.fold_left (fun acc enum ->
			    if acc > enum then acc else enum) (List.hd enums) enums
	in
	let max = Num.num_of_string (Int64.to_string emax) in
	[JCDenum_type(enuminfo.ename,min,max)]

    | GEnumTagDecl _ -> [] (* No enumeration declaration in Jessie *)

    | GVarDecl(_,v,pos) ->
	(* Keep only declarations for which there is no definition *)
	if List.mem v vardefs
	  || (isFunctionType v.vtype &&
		(v.vname = name_of_assert
		    || v.vname = name_of_free
		    || v.vname = name_of_malloc))
	then []
	else if isFunctionType v.vtype then
	  let rtyp = match unrollType v.vtype with
	    | TFun(rt,_,_,_) -> rt
	    | _ -> assert false
	  in
	  let id = mkidentifier v.vname pos in
	  let kf = Globals.Functions.get v in
	  let funspec = Kernel_function.get_spec kf in
	  let params = Globals.Functions.get_params kf in
	  let formal v = ctype v.vtype, unique_name_if_empty v.vname in
	  let formals = List.map formal params in
          [JCDfun(ctype rtyp,id,formals,spec funspec,None)]
	else
	  [JCDvar(ctype v.vtype,v.vname,None)]

    | GVar(v,{init=None},_pos) ->
	[JCDvar(ctype v.vtype,v.vname,None)]

    | GVar(_v,_iinfo,_pos) ->
	(* Initialization should have been rewritten as code in an
	 * initialization function, that is called by the main function in
	 * global analyses and ignored otherwise.
	 *)
	assert false

    | GFun(f,pos) ->
	set_curFundec f;
	if f.svar.vname = name_of_assert
	  || f.svar.vname = name_of_free then []
	else
	  let rty = match unrollType f.svar.vtype with
	    | TFun(ty,_,_,_) -> ty
	    | _ -> assert false
	  in
	  let formal v = ctype v.vtype, v.vname in
	  let formals = List.map formal f.sformals in
	  let id = mkidentifier f.svar.vname f.svar.vdecl in
	  let funspec =
	    Kernel_function.get_spec (Globals.Functions.get f.svar)
	  in
	  begin try
	    let local v =
	      mkexpr (JCPEdecl(ctype v.vtype,v.vname,None)) v.vdecl
	    in
	    let locals = List.rev (List.rev_map local f.slocals) in
	    let body = mkexpr (JCPEblock(statement_list f.sbody.bstmts)) pos in
	    let body = locals @ [body] in
	    let body = mkexpr (JCPEblock body) pos in
	    ignore
	      (reg_pos ~id:f.svar.vname
		 ~name:("Function " ^ f.svar.vname) f.svar.vdecl);
	    [JCDfun(ctype rty,id,formals,spec funspec,Some body)]
	  with Errormsg.Error ->
	    Format.printf "Dropping definition of function %s@." f.svar.vname;
	    [JCDfun(ctype rty,id,formals,spec funspec,None)]
	  end

    | GAsm _ -> [] (* No assembly in Jessie *)

    | GPragma _ -> [] (* Pragmas treated separately *)

    | GText _ -> [] (* Ignore text in Jessie *)

    | GAnnot(la,_pos) -> annotation la

  in
  let pos = get_globalLoc g in
  List.map (fun dnode -> mkdecl dnode pos) dnodes

let integral_type name ty bitsize =
  let min = Num.num_of_big_int (min_value_of_integral_type ~bitsize ty) in
  let max = Num.num_of_big_int (max_value_of_integral_type ~bitsize ty) in
  mkdecl (JCDenum_type(name,min,max)) Loc.dummy_position

(* let all_integral_kinds = *)
(*   let rec all_ik = function *)
(*     | IBool -> IBool :: (all_ik IChar) *)
(*     | IChar -> IChar :: (all_ik ISChar) *)
(*     | ISChar -> ISChar :: (all_ik IUChar) *)
(*     | IUChar -> IUChar :: (all_ik IInt) *)
(*     | IInt -> IInt :: (all_ik IUInt) *)
(*     | IUInt -> IUInt :: (all_ik IShort) *)
(*     | IShort -> IShort :: (all_ik IUShort) *)
(*     | IUShort -> IUShort :: (all_ik ILong) *)
(*     | ILong -> ILong :: (all_ik IULong) *)
(*     | IULong -> IULong :: (all_ik ILongLong) *)
(*     | ILongLong -> ILongLong :: (all_ik IULongLong) *)
(*     | IULongLong -> IULongLong :: [] *)
(*   in *)
(*   all_ik IBool *)

let integral_types () =
  if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
    []
  else
    Hashtbl.fold
      (fun name (ty,bitsize) acc -> integral_type name ty bitsize :: acc)
      all_integral_types []

let type_conversions () =
  let typconv_axiom ty1 ty1_to_ty2 ty2_to_ty1 =
    let x = PExpr.mkvar ~name:"x" () in
    let app1 = PExpr.mkapp ~fun_name:ty1_to_ty2 ~args:[x] () in
    let app2 = PExpr.mkapp ~fun_name:ty2_to_ty1 ~args:[app1] () in
    let eq = PExpr.mkeq ~expr1:x ~expr2:app2 () in
    let forall = PExpr.mkforall ~typ:(ctype ty1) ~vars:["x"] ~body:eq () in
    PDecl.mklemma_def ~name:(unique_logic_name (ty1_to_ty2 ^ "_axiom")) ~axiom:true
      ~body:forall ()
  in
  Hashtbl.fold
    (fun _ (ty1,ty2,ty1_to_ty2,ty2_to_ty1) acc ->
       [
	 PDecl.mklogic_def ~typ:(ctype ty2) ~name:ty1_to_ty2
	   ~params:[ctype ty1, "x"] ~reads:[] ();
	 PDecl.mklogic_def ~typ:(ctype ty1) ~name:ty2_to_ty1
	   ~params:[ctype ty2, "x"] ~reads:[] ();
	 typconv_axiom ty1 ty1_to_ty2 ty2_to_ty1;
	 typconv_axiom ty2 ty2_to_ty1 ty1_to_ty2
       ] @ acc
    ) type_conversion_table []

let file f =
  let filter_defined = function GFun _ | GVar _ -> true | _ -> false in
  let defined_var =
    function GFun(f,_) -> f.svar | GVar(vi,_,_) -> vi | _ -> assert false
  in
  let globals =
(* AVOID CHECKING THE GLOBAL INITIALIZATION FUNCTION, WHICH IS GUARANTEED *)
(*     if Globals.has_entry_point () then *)
(*       let gif = *)
(* 	Kernel_function.get_definition (Globals.Functions.get_glob_init f) *)
(*       in *)
(*       f.globals @ [GFun(gif,Loc.dummy_position)] *)
(*     else  *)f.globals
  in
  let vardefs =
    List.rev (List.rev_map defined_var (List.filter filter_defined globals))
  in
  (* Compute translation of [globals] before [integral_types] so that types 
     used are known *)
  let globals' = 
    List.flatten (List.rev (List.rev_map (global vardefs) globals))
  in
  mkdecl (JCDaxiomatic("Padding",
		       [mkdecl (JCDlogic_type name_of_padding_type)
			  Loc.dummy_position]))
    Loc.dummy_position
  (* Define all integral types as enumerated types in Jessie *)
  :: integral_types ()
  (* Define conversion functions and identity axiom for back
     and forth conversion *)
  @ type_conversions ()
  @ globals'

(* Translate pragmas separately as their is no declaration for pragmas in
 * the parsed AST of Jessie, only in its types AST.
 *)
let pragma = function
  | GPragma(Attr(name,[AStr arg]),_)
  | GPragma(Attr(name,[ACons(arg,[])]),_) ->
      begin match name with
	| "InvariantPolicy" ->
	    begin match String.lowercase arg with
	      | "none" -> [Jc_output.JCinvariant_policy Jc_env.InvNone]
	      | "arguments" ->
		  [Jc_output.JCinvariant_policy Jc_env.InvArguments]
	      | "ownership" ->
		  [Jc_output.JCinvariant_policy Jc_env.InvOwnership]
	      | _ -> assert false
	    end
	| "SeparationPolicy" ->
	    begin match String.lowercase arg with
	      | "none" -> [Jc_output.JCseparation_policy Jc_env.SepNone]
	      | "regions" -> [Jc_output.JCseparation_policy Jc_env.SepRegions]
	      | _ -> assert false
	    end
	| "AnnotationPolicy" ->
	    begin match String.lowercase arg with
	      | "none" -> [Jc_output.JCannotation_policy Jc_env.AnnotNone]
	      | "invariants" ->
		  [Jc_output.JCannotation_policy Jc_env.AnnotInvariants]
	      | "weakpre" ->
		  [Jc_output.JCannotation_policy Jc_env.AnnotWeakPre]
	      | "strongpre" ->
		  [Jc_output.JCannotation_policy Jc_env.AnnotStrongPre]
	      | _ -> assert false
	    end
	| "AbstractDomain" ->
	    begin match String.lowercase arg with
	      | "none" -> [Jc_output.JCabstract_domain Jc_env.AbsNone]
	      | "box" -> [Jc_output.JCabstract_domain Jc_env.AbsBox]
	      | "oct" -> [Jc_output.JCabstract_domain Jc_env.AbsOct]
	      | "pol" -> [Jc_output.JCabstract_domain Jc_env.AbsPol]
	      | _ -> assert false
	    end
	| "FloatModel" ->
	    begin match String.lowercase arg with
	      | "real" -> float_model := `Real
	      | "strict" -> float_model := `Strict
	      | "full" -> float_model := `Full
	      | s ->
		  Format.eprintf
		    "Warning: pragma %s: identifier %s is not a valid value.\npragma is ignored.@." name s
	    end; []
	| "JessieIntegerModel" ->
	    begin match String.lowercase arg with
	      | "exact" | "math" ->
		  Cmdline.Jessie.IntModel.set "exact"
	      | "strict" ->
		  Cmdline.Jessie.IntModel.set "strict"
	      | "modulo" ->
		  Cmdline.Jessie.IntModel.set "modulo"
	      | s ->
		  Format.eprintf
		    "Warning: pragma %s: identifier %s is not a valid value.\npragma is ignored.@." name s
	    end;
	    []
	| _ ->
	    Format.eprintf
	      "Warning: pragma %s is ignored by Jessie plugin.@." name;
	    []
      end
  | GPragma _ -> []
  | _ -> []

let pragmas f =
  (match Cmdline.Jessie.IntModel.get_val () with
    | Cmdline.Jessie.IMexact -> []
    | Cmdline.Jessie.IMbounded -> [Jc_output.JCint_model Jc_env.IMbounded]
    | Cmdline.Jessie.IMmodulo -> [Jc_output.JCint_model Jc_env.IMmodulo])
  @ Jc_output.JCinvariant_policy Jc_env.InvArguments
  :: (if Cmdline.Jessie.SepRegions.get () then
	Jc_output.JCseparation_policy Jc_env.SepRegions
      else
	Jc_output.JCseparation_policy Jc_env.SepNone)
  :: (match Cmdline.Jessie.InferAnnot.get () with
	| "" -> Jc_output.JCannotation_policy Jc_env.AnnotNone
	| "inv" -> Jc_output.JCannotation_policy Jc_env.AnnotInvariants
	| "pre" -> Jc_output.JCannotation_policy Jc_env.AnnotElimPre
	| "spre" -> Jc_output.JCannotation_policy Jc_env.AnnotStrongPre
	| "wpre" -> Jc_output.JCannotation_policy Jc_env.AnnotWeakPre
	| s ->
	    Format.eprintf "unknown inference technique %s@." s; assert false)
  :: (match Cmdline.Jessie.AbsDomain.get () with
	| "box" -> Jc_output.JCabstract_domain Jc_env.AbsBox
	| "oct" -> Jc_output.JCabstract_domain Jc_env.AbsOct
	| "poly" -> Jc_output.JCabstract_domain Jc_env.AbsPol
	| s -> Format.eprintf "unknown abstract domain %s@." s; assert false)
  :: List.flatten (List.rev (List.rev_map pragma f.globals))


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j bin/toplevel.byte"
End:
*)
