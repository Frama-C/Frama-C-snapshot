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

(* $Id: interp.ml,v 1.121 2008/05/23 16:55:39 uid570 Exp $ *)

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

let mkexpr enode loc = new pexpr ~loc enode

let void_expr = mkexpr (JCPEconst JCCvoid) Loc.dummy_position
let null_expr = mkexpr (JCPEconst JCCnull) Loc.dummy_position
let true_expr = mkexpr (JCPEconst(JCCboolean true)) Loc.dummy_position
let zero_expr = mkexpr (JCPEconst(JCCinteger "0")) Loc.dummy_position
let one_expr = mkexpr (JCPEconst(JCCinteger "1")) Loc.dummy_position

let mkidentifier name loc = new identifier ~loc name

let rec mkconjunct elist loc =
  match elist with
    | [] -> true_expr
    | [e] -> e
    | e::el -> mkexpr (JCPEbinary(e,`Bland,mkconjunct el loc)) loc

let mkdecl dnode loc = new decl ~loc dnode


(*****************************************************************************)
(* Locate Jessie expressions on source program.                              *)
(*****************************************************************************)

let reg_loc ?id ?kind ?name loc = Output.reg_loc "C" ?id ?kind ?name loc

(* [locate] should be called on every Jessie expression which we would like to
 * locate in the original source program.
 *)
let locate ?alarm ?loc e =
  (* Recursively label conjuncts so that splitting conjuncts in Why still
   * allows to locate the resulting VC.
   *)
  let rec doloc ~toplevel e =
    (* Generate (and store) a label associated to this source location *)
    let loc = match loc with
      | None -> e#loc
      | Some loc ->
	  if is_unknown_location e#loc then loc else e#loc
    in
    let lab = match alarm with
      | None ->
	  reg_loc loc
      | Some Alarms.Division_alarm ->
	  reg_loc ~kind:Output.DivByZero loc
      | Some Alarms.Memory_alarm ->
	  reg_loc ~kind:Output.PointerDeref loc
      | Some Alarms.Shift_alarm ->
	  reg_loc ~kind:Output.ArithOverflow loc
      | Some Alarms.Pointer_compare_alarm
      | Some Alarms.Using_nan_or_infinite_alarm
      | Some Alarms.Result_is_nan_or_infinite_alarm ->
	  reg_loc loc
    in
    let e = match e#node with
      | JCPEbinary(e1,`Bland,e2) ->
	  begin match e1#node,e2#node with
	    | JCPElabel _,JCPElabel _ -> e (* already labelled *)
	    | JCPElabel _,_ -> (* [e1] already labelled *)
		let e2 = doloc ~toplevel:false e2 in
		mkexpr (JCPEbinary(e1,`Bland,e2)) loc
	    | _,JCPElabel _ -> (* [e2] already labelled *)
		let e1 = doloc ~toplevel:false e1 in
		mkexpr (JCPEbinary(e1,`Bland,e2)) loc
	    | _,_ -> (* none already labelled *)
		let e1 = doloc ~toplevel:false e1 in
		let e2 = doloc ~toplevel:false e2 in
		mkexpr (JCPEbinary(e1,`Bland,e2)) loc
	  end
      | _ -> e
    in
    (* Do not generate a label for every intermediate conjunct *)
    match e#node with
      | JCPEbinary(_e1,`Bland,_e2) when not toplevel -> e
      | _ ->
	  (* Label the expression accordingly *)
	  mkexpr (JCPElabel(lab,e)) loc
  in
  doloc ~toplevel:true e


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
      let ty1_to_ty2 = unique_name (n1 ^ "_to_" ^ n2) in
      let ty2_to_ty1 = unique_name (n2 ^ "_to_" ^ n1) in
      Hashtbl.add 
	type_conversion_table (sig1,sig2) (ty1,ty2,ty1_to_ty2,ty2_to_ty1);
      ty1_to_ty2,ty2_to_ty1

let ctype ty =
  let tnode = match unrollType ty with
    | TVoid _attr -> JCPTnative Tunit

    | TInt(_ik,_attr) ->
	if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	  JCPTnative Tinteger
	else
	  JCPTidentifier (name_of_integral_type ty)

    | TFloat(_fk,_attr) ->
	(* TODO: use real kind in Jessie when available *)
	JCPTnative Treal

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
		JCPTpointer(compinfo.cname,Some min_bound,Some max_bound)
	    | _ -> assert false
	  end
	else
	  begin match unrollType (pointed_type ty) with
	    | TComp(compinfo,_attr) ->
		JCPTpointer(compinfo.cname,None,None)
	    | _ -> assert false
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

let rec const = function
  | CInt64(_i,_ik,Some s) -> JCCinteger s
      (* Use the textual representation if available *)

  | CInt64(i,_ik,None) -> JCCinteger(Int64.to_string i)

  | CStr _ | CWStr _ -> assert false  (* Should have been rewritten *)

  | CChr c -> JCCinteger(string_of_int (Char.code c))

  | CReal(_f,_fk,Some s) -> JCCreal s
      (* Use the textual representation if available *)

  | CReal(f,_fk,None) -> JCCreal(string_of_float f)

  | CEnum(e,_name,_einfo) -> const_of_expr e

and const_of_expr e =
  match stripInfo e with Const c -> const c | _ -> assert false

and boolean_const = function
  | CInt64(i,_ik,_text) ->
      if i = Int64.zero then JCCboolean false else JCCboolean true

  | CStr _ | CWStr _ -> JCCboolean true

  | CChr c ->
      if Char.code c = 0 then JCCboolean false else JCCboolean true

  | CReal(f,_fk,_text) ->
      if f = 0.0 then JCCboolean false else JCCboolean true

  | CEnum(e,_name,_einfo) -> boolean_const_of_expr e

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

let term_lhost loc = function
  | TVar v -> mkexpr (JCPEvar v.lv_name) loc
  | TResult -> mkexpr (JCPEvar "\\result") loc
  | TMem _ -> assert false (* Should have been rewritten *)

let rec term t =
  let enode = match t.term_node with
    | TConst c -> JCPEconst(const c)

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

    | TBinOp(op,t1,t2) -> JCPEbinary(term t1,binop op,term t2)

    | TCastE(ty,t)
	when isIntegralType ty && isLogicArithmeticType t.term_type ->
	if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	  (term t)#node
	else
	  let tyname = name_of_integral_type ty in
	  JCPEcast(term t,tyname)

    | TCastE(ty,t)
	when isFloatingType ty && isLogicArithmeticType t.term_type ->
	JCPEcast(term t,"real")

    | TCastE(ty,t) 
	when isIntegralType ty && app_term_type isPointerType false t.term_type
	  && bits_sizeof ty = force_app_term_type bits_sizeof t.term_type ->
	let _,ptr_to_int = force_app_term_type (type_conversion ty) t.term_type in 
	JCPEapp(ptr_to_int,[],[term t])

    | TCastE(TPtr(_ty,_attr) as ptrty,_t1) ->
	let t = stripTermCasts t in
	begin match t.term_node with
	  | Tnull ->
	      JCPEconst JCCnull
	  | TConst c
	      when is_integral_const c && value_of_integral_const c = Int64.zero ->
	      JCPEconst JCCnull
	  | _ ->
	      (* Only hierarchical types are available in Jessie. It
	       * should have been encoded as the use of pointer types
	       * on structure type.
	       *)
(* 	      match unrollType ty with *)
(* 		| TComp(compinfo,_) -> *)
(* 		    JCPEcast(term t,compinfo.cname) *)
(* 		| _ -> assert false *)
	      Errormsg.s
		(Cil.error "Casting from type %a to type %a not allowed"
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
	if in_jessie_memory_model name then
	  if name = name_of_offset_min then
	    JCPEoffset(Offset_min,term (as_singleton tlist))
	  else if name = name_of_offset_max then
	    JCPEoffset(Offset_max,term (as_singleton tlist))
	  else assert false
	else JCPEapp(name,logic_labels_assoc labels,List.map term tlist)

    | Tif(t1,t2,t3) -> JCPEif(term t1,term t2,term t3)

    | Told t -> JCPEold(term t)

    | Tat(t,lab) -> JCPEat(term t,logic_label lab)

    | Tbase_addr _t -> assert false (* TODO: memory model for Jessie *)

    | Tblock_length _t -> assert false (* TODO: memory model for Jessie *)

    | Tnull -> JCPEconst JCCnull

    | TCoerce(_t,_typ) -> assert false (* TODO: see if useful *)

    | TCoerceE(_t1,_t2) -> assert false (* TODO: see if useful *)
    | Tlambda _ -> assert false (* TODO: does not exist in Jessie *)
  in
  mkexpr enode t.term_loc

and term_lval loc = function
  | lhost, TNoOffset -> term_lhost loc lhost

  | (TVar _ | TResult), _off ->
      assert false (* Should have been rewritten *)

  | TMem t, TField(fi,toff) ->
      assert (toff = TNoOffset); (* Others should have been rewritten *)
      mkexpr (JCPEderef(term t,fi.fname)) loc

  | TMem t, TIndex(it,TField(fi,toff)) ->
      assert (toff = TNoOffset); (* Others should have been rewritten *)
      (* Normalization made it equivalent to simple add *)
      let adde = mkexpr (JCPEbinary(term t,`Badd,term it)) loc in
      mkexpr (JCPEderef(adde,fi.fname)) loc

  | TMem _e, TIndex _ -> assert false (* Should have been rewritten *)

let tsets_lhost = function
  | TSVar lv -> mkexpr (JCPEvar lv.lv_name) Loc.dummy_position
  | TSResult -> mkexpr (JCPEvar "\\result") Loc.dummy_position
  | TSMem _ -> assert false (* Should have been rewritten *)

let rec tsets_elem ts =
  let enode = match ts with
    | TSLval lv -> (tsets_lval lv)#node

    | TSStartOf lv -> (tsets_lval lv)#node

    | TSConst c -> JCPEconst(const c)

    | TSAt(ts,lab) -> JCPEat(tsets_elem ts,logic_label lab)

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
	  let tyname = name_of_integral_type ty in
	  JCPEcast(tsets_elem ts,tyname)

    | TSCastE(ty,ts)
	when isFloatingType ty && isLogicArithmeticType (typeOfTsetsElem ts) ->
	JCPEcast(tsets_elem ts,"real")

    | TSCastE(TPtr(_ty,_attr) as ptrty,_ts1) ->
	begin match stripTsetsCasts ts with
	  | TSConst c
	      when is_integral_const c && value_of_integral_const c = Int64.zero ->
	      JCPEconst JCCnull
	  | ts ->
	      (* Only hierarchical types are available in Jessie. It
	       * should have been encoded as the use of pointer types
	       * on strucure type.
	       *)
(* 	      match unrollType ty with *)
(* 		| TComp(compinfo,_) -> JCPEcast(tsets_elem ts,compinfo.cname) *)
(* 		| _ -> assert false *)
	      Errormsg.s
		(Cil.error "Casting from type %a to type %a not allowed"
		  !Ast_printer.d_logic_type (typeOfTsetsElem ts) !Ast_printer.d_type ptrty)
	end

    | TSCastE(ty,ts) ->
	(* TODO: support other casts in Jessie as well, through low-level
	 * memory model
	 *)
	Errormsg.s
	  (Cil.error "Casting from type %a to type %a not allowed"
	    !Ast_printer.d_logic_type (typeOfTsetsElem ts) !Ast_printer.d_type ty)
  in
  mkexpr enode Loc.dummy_position

and tsets_lval = function
  | lhost, TSNo_offset -> tsets_lhost lhost

  | (TSVar _ | TSResult), _off ->
      assert false (* Should have been rewritten *)

  | TSMem ts, TSField(fi,tsoff) ->
      assert (tsoff = TSNo_offset); (* Others should have been rewritten *)
      mkexpr (JCPEderef(tsets_elem ts,fi.fname)) Loc.dummy_position

  | TSMem ts, TSIndex(it,TSField(fi,tsoff)) ->
      assert (tsoff = TSNo_offset); (* Others should have been rewritten *)
      (* Normalization made it equivalent to simple add *)
      let adde =
        mkexpr (JCPEbinary(tsets_elem ts,`Badd,term it)) Loc.dummy_position
      in
      mkexpr (JCPEderef(adde,fi.fname)) Loc.dummy_position

  | TSMem _ts, TSIndex _ -> assert false (* Should have been rewritten *)

  | TSMem ts, TSRange(low,high,TSField(fi,tsoff)) ->
      assert (tsoff = TSNo_offset); (* Others should have been rewritten *)
      (* Normalization made it equivalent to simple add *)
      let enode = JCPErange(opt_map term low,opt_map term high) in
      let e = mkexpr enode Loc.dummy_position in
      let adde =
        mkexpr (JCPEbinary(tsets_elem ts,`Badd,e)) Loc.dummy_position
      in
      mkexpr (JCPEderef(adde,fi.fname)) Loc.dummy_position

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
	JCPEapp(pinfo.p_name,logic_labels_assoc labels,List.map term tl)

    | Prel(rel,t1,t2) ->
	JCPEbinary(term t1,relation rel,term t2)

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

    | Pvalid_index(t1,t2) ->
	let e1 = term t1 in
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

    | Pfresh _t -> assert false (* TODO: add to memory model for Jessie *)

    | PInstanceOf(t,TPtr(ty,_attr)) ->
	begin match unrollType ty with
	  | TComp(compinfo,_) -> JCPEinstanceof(term t,compinfo.cname)
	  | _ -> assert false
	end

    | PInstanceOf(_t,_typ) -> assert false (* TODO *)

    | PInstanceOfE(_t1,_t2) -> assert false (* TODO *)
  in
  mkexpr enode p.loc

(* Keep names associated to predicate *)
let named_pred p =
  List.fold_right
    (fun lab p -> mkexpr (JCPElabel(lab,p)) p#loc) p.name (pred p)

let conjunct loc pl =
  mkconjunct (List.map (pred $ Logic_const.pred_of_id_pred) pl) loc

let zone = function
  | Location tset,_from -> tsets tset.its_content
  | Nothing,_from -> []

(* Distinguish between:
 * - no assign, which is the empty list in Cil and None in Jessie
 * - assigns nothing, which is [Nothing] in Cil and the Some[] in Jessie
 *)
let assigns = function
  | [] -> None
  | [Location({ its_content = TSSingleton(TSLval(TSResult,TSNo_offset))}),_from] ->
      None (* FS#249: It is the same to assign \result or nothing *)
  | assign_list ->
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
      unique_name b.b_name,
      None, (* throws *)
      Some(conjunct Loc.dummy_position b.b_assumes),
      None, (* requires *)
      assigns b.b_assigns,
      locate (conjunct Loc.dummy_position b.b_ensures))
  in
  let behaviors = List.map behavior funspec.spec_behavior in

  (* TODO: translate function spec variant, terminates and complete/disjoint
     behaviors *)
  requires @ behaviors

let assert_ loc = function
  | WP _ -> []
  | User annot ->
      begin match annot.annot_content with
	| AAssert p | AInvariant(_,p) ->
	    [mkexpr (JCPEassert (locate ~loc (named_pred p))) loc]
	| _ -> assert false
      end
  | AI(alarm,annot) ->
      begin match annot.annot_content with
	| AAssert p | AInvariant(_,p) ->
	    [mkexpr (JCPEassert (locate ~alarm ~loc (named_pred p))) loc]
	| _ -> assert false
      end

let invariant annot =
  match annot.annot_content with
    | AInvariant(_loopinv,p) -> locate (pred p)
    | _ -> assert false

let variant annot =
  match annot.annot_content with
    | AVariant(t,_) -> locate (term t)
    | _ -> assert false


(*****************************************************************************)
(* Cil to Jessie translation of coding constructs                            *)
(*****************************************************************************)

let rec expr loc e =
  (* Precise the location if possible *)
  let loc = match e with Info(_,einfo) -> einfo.exp_loc | _ -> loc in

  let expr = expr loc in
  let integral_expr = integral_expr loc in

  let enode = match stripInfo e with
    | Info _ -> assert false

    | Const c -> JCPEconst(const c)

    | Lval lv -> (lval loc lv)#node

    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
	assert false (* Should be removed by constant folding *)

    | UnOp(_op,_e,ty) when isIntegralType ty ->
	(integral_expr e)#node

    | UnOp(op,e,_ty) ->
	let e =
	  locate (mkexpr (JCPEunary(unop op,expr e)) loc)
	in
	e#node

    | BinOp(_op,_e1,_e2,ty) when isIntegralType ty ->
	(integral_expr e)#node

    | BinOp(op,e1,e2,_ty) ->
	let e =
	  locate (mkexpr (JCPEbinary(expr e1,binop op,expr e2)) loc)
	in
	e#node

    | CastE(ty,e') when isIntegralType ty && isArithmeticType (typeOf e') ->
	(integral_expr e)#node

    | CastE(ty,e) when isFloatingType ty && isArithmeticType (typeOf e) ->
	let e = locate (mkexpr (JCPEcast(expr e,"real")) loc) in
	e#node

    | CastE(ty,e') when isIntegralType ty && isPointerType (typeOf e') 
	&& bits_sizeof ty = bits_sizeof (typeOf e') ->
	let _,ptr_to_int = type_conversion ty (typeOf e') in 
	JCPEapp(ptr_to_int,[],[expr e'])
	    
    | CastE(TPtr(_ty,_attr) as ptrty,_e1) ->
	begin match stripCastsAndInfo e with
	  | Const c
	      when is_integral_const c && value_of_integral_const c = Int64.zero ->
	      JCPEconst JCCnull
	  | e ->
	      let ety = typeOf e in
	      if isIntegralType ety && bits_sizeof ety = bits_sizeof ptrty then
		let _,int_to_ptr = type_conversion ptrty ety in 
		JCPEapp(int_to_ptr,[],[integral_expr e])
	      else if isPointerType ety then
		let _,ptr_to_ptr = type_conversion ptrty ety in 
		JCPEapp(ptr_to_ptr,[],[expr e])
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

    | StartOf lv -> (lval loc lv)#node
  in
  mkexpr enode loc

(* Function called when expecting a boolean in Jessie, i.e. when translating
   a test or a sub-expression of an "or" or "and".
*)
and boolean_expr loc e =
  (* Precise the location if possible *)
  let loc = match e with Info(_,einfo) -> einfo.exp_loc | _ -> loc in

  let expr = expr loc in
  let boolean_expr = boolean_expr loc in
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
	  if is_null_expr e1 || is_null_expr e2 then
	    (* No rewriting towards integers performed. Nonetheless, we are
	     * here in a case of undecidable behavior according to
	     * the standard, with no good reason to compare a pointer to
	     * zero like this. *)
	    Errormsg.s
	      (Cil.error "Comparing pointer to null with %a not allowed"
		!Ast_printer.d_binop op)
	  else
	    (* Pointer comparison should have been rewritten as pointer
	     * subtraction compared to zero. *)
	    assert false

    | _ -> boolean_node_from_expr (typeOf e) (expr e)
  in
  mkexpr enode loc

(* Function called instead of plain [expr] when the evaluation result should
 * fit in a C integral type.
 *)
and integral_expr loc e =

  let rec int_expr loc e =
    let expr = expr loc in
    let int_expr = int_expr loc in
    let boolean_expr = boolean_expr loc in
    let node_from_boolean_expr e = JCPEif(e,one_expr,zero_expr) in

    let enode = match e with
      | UnOp(LNot,e,_ty) ->
	  let e = mkexpr (JCPEunary(unop LNot,boolean_expr e)) loc in
	  node_from_boolean_expr e

      | UnOp(op,e,_ty) ->
	  let e =
	    locate (mkexpr (JCPEunary(unop op,expr e)) loc)
	  in
	  e#node

      | BinOp((LAnd | LOr) as op,e1,e2,_ty) ->
	  let e =
	    mkexpr (JCPEbinary(boolean_expr e1,binop op,boolean_expr e2)) loc
	  in
	  node_from_boolean_expr e

      | BinOp((Eq | Ne) as op,e1,e2,_ty) ->
	  let e = mkexpr (JCPEbinary(expr e1,binop op,expr e2)) loc in
	  node_from_boolean_expr e

      | BinOp((Lt | Gt | Le | Ge) as op,e1,e2,_ty) ->
	  let ty = typeOf e1 in
	  if isArithmeticType ty then
	    let e =
	      mkexpr
		(JCPEbinary(expr e1,binop op,expr e2))
		loc
	    in
	    node_from_boolean_expr e
	  else
	    if is_null_expr e1 || is_null_expr e2 then
	      (* No rewriting towards integers performed. Nonetheless, we are
	       * here in a case of undecidable behavior according to
	       * the standard, with no good reason to compare a pointer to
	       * zero like this. *)
	      Errormsg.s
		(Cil.error "Comparing pointer to null with %a not allowed"
		  !Ast_printer.d_binop op)
	    else
	      (* Pointer comparison should have been rewritten as pointer
	       * subtraction compared to zero. *)
	      assert false

      | BinOp(Shiftrt,e1,e2,_ty) ->
	  let e = match possible_value_of_integral_expr e2 with
	    | Some i when i >= 0L && i < 63L ->
		(* Right shift by constant is division by constant *)
		let pow = constant_expr (power_of_two i) in
		locate (mkexpr (JCPEbinary(expr e1,`Bdiv,expr pow)) loc)
	    | _ ->
		let op =
		  if isSignedInteger (typeOf e1) then `Barith_shift_right
		  else `Blogical_shift_right
		in
		locate (mkexpr (JCPEbinary(expr e1,op,expr e2)) loc)
	  in
	  e#node

      | BinOp(Shiftlt as op,e1,e2,_ty) ->
	  let e = match possible_value_of_integral_expr e2 with
	    | Some i when i >= 0L && i < 63L ->
		(* Left shift by constant is multiplication by constant *)
		let pow = constant_expr (power_of_two i) in
		locate (mkexpr (JCPEbinary(expr e1,`Bmul,expr pow)) loc)
	    | _ ->
		locate (mkexpr (JCPEbinary(expr e1,binop op,expr e2)) loc)
	  in
	  e#node

      | BinOp(op,e1,e2,_ty) ->
	  let e =
	    locate (mkexpr (JCPEbinary(expr e1,binop op,expr e2)) loc)
	  in
	  e#node

      | CastE(ty,e) when isIntegralType ty && isArithmeticType (typeOf e) ->
	  if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
	    (int_expr e)#node
	  else
	    let tyname = name_of_integral_type ty in
	    let e = locate (mkexpr (JCPEcast(int_expr e,tyname)) loc) in
	    e#node

      | _ -> (expr e)#node
    in
    mkexpr enode loc
  in

  match e with
    | CastE _ -> int_expr loc e
    | _ -> int_expr loc (CastE(typeOf e,e))

and lval loc = function
  | Var v, NoOffset -> mkexpr (JCPEvar v.vname) loc

  | Var _v, _off -> assert false (* Should have been rewritten *)

  | Mem _e, NoOffset -> assert false (* Should have been rewritten *)

  | Mem e, Field(fi,off) ->
      assert (off = NoOffset); (* Others should have been rewritten *)
      locate (mkexpr (JCPEderef(expr loc e,fi.fname)) loc)

  | Mem e, Index(ie,Field(fi,off)) ->
      assert (off = NoOffset); (* Others should have been rewritten *)
      (* Normalization made it equivalent to simple add *)
      let adde = mkexpr (JCPEbinary(expr loc e,`Badd,expr loc ie)) loc in
      locate (mkexpr (JCPEderef(adde,fi.fname)) loc)

  | Mem _e, Index _ as lv -> 
      Format.printf "lval %a@." !Ast_printer.d_lval lv;
      assert false (* Should have been rewritten *)

let rec instruction = function
  | Set(lv,e,loc) ->
      let enode = JCPEassign(lval loc lv,expr loc e) in
      (locate (mkexpr enode loc))#node

  | Call(None,Lval(Var v,NoOffset),eargs,loc) ->
      if is_assert_function v then
	JCPEassert(locate (boolean_expr loc (as_singleton eargs)))
      else
	let enode =
	  if is_free_function v then
	    JCPEfree(expr loc (as_singleton eargs))
	  else
	    JCPEapp(v.vname,[],List.map (expr loc) eargs)
	in
	(locate (mkexpr enode loc))#node

  | Call(Some lv,Lval(Var v,NoOffset),eargs,loc) ->
      let enode =
	if is_malloc_function v then
	  let ty,arg = match stripInfo (as_singleton eargs) with
	    | Info _ -> assert false
	    | Const c as arg when is_integral_const c ->
		let lvtyp = pointed_type (typeOfLval lv) in
		let lvsiz = (bits_sizeof lvtyp) lsr 3 in
		let allocsiz = (value_of_integral_expr arg) / lvsiz in
		let siznode = JCPEconst(JCCinteger(Int64.to_string allocsiz)) in
		lvtyp, mkexpr siznode loc
	    | arg ->
		let lvtyp = pointed_type (typeOfLval lv) in
		let lvsiz = (bits_sizeof lvtyp) lsr 3 in
		let esiz = constant_expr lvsiz in
		lvtyp, expr loc (BinOp(Div,arg,esiz,typeOf arg))
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
		    expr loc nelem
		  else
		    let factor = constant_expr factor in
		    expr loc (BinOp(Mult,nelem,factor,typeOf arg))
		in
		lvtyp, siz
	    | arg ->
		let lvtyp = pointed_type (typeOfLval lv) in
		let lvsiz = (bits_sizeof lvtyp) lsr 3 in
		let esiz = constant_expr lvsiz in
		lvtyp, expr loc (BinOp(Div,
				       BinOp(Mult,nelem,elsize,typeOf arg),
				       esiz,typeOf arg))
	  in
	  let name_of_type = match unrollType ty with
	    | TComp(compinfo,_) -> compinfo.cname
	    | _ -> assert false
	  in
	  JCPEalloc(arg,name_of_type)
	else
	  JCPEapp(v.vname,[],List.map (expr loc) eargs)
      in
      let enode = JCPEassign(lval loc lv,locate (mkexpr enode loc)) in
      (locate (mkexpr enode loc))#node

  | Call _ -> assert false

  | Asm _ -> assert false

  | Skip _loc -> JCPEconst JCCvoid

  | Code_annot _ -> JCPEconst JCCvoid
      (* Annotations should be retrieved from Db *)

let rec statement s =
  let loc = get_stmtLoc s.skind in

  let assert_list =
    Annotations.get_filter Logic_const.is_assert s
    @ Annotations.get_filter Logic_const.is_stmt_invariant s
  in
  let assert_before,assert_after =
    List.partition (function Before _ -> true | After _ -> false) assert_list
  in
  let assert_before =
    List.flatten (List.map ((assert_ loc) $ before_after_content) assert_before)
  in
  let assert_after =
    List.flatten (List.map ((assert_ loc) $ before_after_content) assert_after)
  in

  let snode = match s.skind with
    | Instr i -> instruction i

    | Return(Some e,loc) ->
	JCPEreturn(expr loc e)

    | Return(None,_loc) ->
	JCPEreturn(mkexpr (JCPEconst JCCvoid) loc)

    | Goto(sref,_loc) ->
	(* Pick the first non-case label in the list of labels associated to
	 * the target statement
	 *)
	let labels = filter_out is_case_label !sref.labels in
	assert (not (labels = []));
	JCPEgoto(label (List.hd labels))

    | Break _loc ->
	assert false (* Should not occur after [prepareCFG] *)

    | Continue _loc ->
	assert false (* Should not occur after [prepareCFG] *)

    | If(e,bl1,bl2,loc) ->
	JCPEif(boolean_expr loc e,block bl1,block bl2)

    | Switch(e,bl,slist,loc) ->
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
	  | Case(e,loc) -> Some(expr loc e)
	  | Default _ -> None
	in
	let case = function
	  | [] -> assert false
	  | case_stmt :: _ as slist ->
	      let switch_labels = List.filter is_case_label case_stmt.labels in
	      let labs = List.map switch_label switch_labels in
	      let slist = mkexpr (JCPEblock(statement_list slist)) loc in
	      labs, slist
	in
	let case_list = List.map case (case_blocks bl.bstmts slist) in
	JCPEswitch(expr loc e,case_list)

    | Loop (_,bl,loc,_continue_stmt,_break_stmt) ->

	let invariant_list =
	  Annotations.get_filter Logic_const.is_invariant s
	in
	let invariant_list =
	  lift_annot_list_func (List.map invariant) invariant_list
	in
	let invariant = match invariant_list with
	  | [inv] -> inv
	  | _ -> locate (mkconjunct invariant_list loc)
	in

	let variant_list =
	  Annotations.get_filter Logic_const.is_variant s
	in
	let variant_list =
	  lift_annot_list_func (List.map variant) variant_list
	in
	(* At most one variant *)
	let variant = match variant_list with
	  | [] -> None
	  | [e] -> Some e
	  | _ -> assert false
	in

	(* Locate the beginning of the loop, to serve as location for generated
	 * invariants and variants.
	 *)
(* 	let lab = reg_loc loc in *)
	(* TODO: add loop-assigns to Jessie *)
	JCPEwhile(true_expr,invariant,variant,block bl)

    | Block bl ->
	JCPEblock(statement_list bl.bstmts)

    | UnspecifiedSequence(s1,s2) ->
	let s = block (mkBlock [s1;s2]) in
	s#node

    | TryFinally _ | TryExcept _ -> assert false
  in
  (* Prefix statement by all non-case labels *)
  let labels = filter_out is_case_label s.labels in
  let s = mkexpr snode loc in
  let s = match assert_before @ s :: assert_after with
    | [s] -> s
    | slist -> mkexpr (JCPEblock slist) loc
  in
  List.fold_left (fun s lab -> mkexpr (JCPElabel(label lab,s)) loc) s labels

and statement_list slist = List.rev (List.rev_map statement slist)

and block bl =
  match bl.bstmts with
    | [] -> mkexpr (JCPEconst JCCvoid) Loc.dummy_position
    | [s] -> statement s
    | slist -> mkexpr (JCPEblock(statement_list slist)) Loc.dummy_position


(*****************************************************************************)
(* Cil to Jessie translation of global declarations                          *)
(*****************************************************************************)

let logic_variable v = ltype v.lv_type, v.lv_name

let annotation = function
  | Dpredicate_reads(info,_poly,params,reads_tsets) ->
      if in_jessie_memory_model info.p_name then [] else
	let params = List.map logic_variable params in
	let reads = List.flatten (List.map tsets reads_tsets) in
	[JCDlogic(None,info.p_name,logic_labels info.p_labels,params,JCreads reads)]

  | Dpredicate_def(info,_poly,params,def) ->
      let params = List.map logic_variable params in
      [JCDlogic(None,info.p_name,logic_labels info.p_labels,params,JCexpr(pred def))]

  | Dlogic_reads(info,_poly,params,return_type,reads_tsets) ->
      if in_jessie_memory_model info.l_name then [] else
	let params = List.map logic_variable params in
	let reads = List.flatten (List.map tsets reads_tsets) in
	[JCDlogic(
	  Some(ltype return_type),
           info.l_name,logic_labels info.l_labels,params,JCreads reads)]

  | Dlogic_def(info,_poly,params,return_type,def) ->
      let params = List.map logic_variable params in
      [JCDlogic(
	Some(ltype return_type),info.l_name,logic_labels info.l_labels,params,
         JCexpr(term def))]

  | Dlemma(name,is_axiom,labels,_poly,property) ->
      [JCDlemma(name,is_axiom,logic_labels labels,pred property)]

  | Dinvariant(name,property) -> [JCDglobal_inv(name,pred property)]

  | Dtype_annot annot ->
      [JCDlogic(
	None,annot.inv_name,[(* TODO *) LabelHere],
	[ltype annot.this_type, annot.this_name],JCexpr(pred annot.inv))]

  | Dtype(name,[]) -> [JCDlogic_type name]

  | Dtype(_name,_) -> assert false (* TODO *)

let global vardefs g =
  let dnodes = match g with
    | GType _ -> [] (* No typedef in Jessie *)

    | GCompTag(compinfo,loc) when compinfo.cstruct -> (* struct type *)
	let field fi = false, ctype fi.ftype, fi.fname in
	let fields = List.map field compinfo.cfields in
	let parent =
	  find_or_none (Hashtbl.find Norm.type_to_parent_type) compinfo.cname
	in
	let id = mkidentifier compinfo.cname loc in
	[
	  JCDtag(compinfo.cname,parent,fields,[]);
	  JCDvariant_type(compinfo.cname,[id])
	]

    | GCompTag(compinfo,_loc) -> (* union type *)
(* 	assert (not compinfo.cstruct); *)
(* 	let field fi = *)
(* 	  match fi.ftype with *)
(* 	    | TComp(compinfo,_) -> *)
(* 		let field fi = false, ctype fi.ftype, fi.fname in *)
(* 		let fields = List.map field compinfo.cfields in *)
(* 		let parent = *)
(* 		  find_or_none (Hashtbl.find Norm.type_to_parent_type) *)
(* 		    compinfo.cname *)
(* 		in *)
(* 		mkidentifier fi.fname fi.floc, JCDtag(fi.fname,parent,fields,[]) *)
(* 	    | _ -> assert false *)
(* 	in *)
(* 	let fields,field_tags = List.split (List.map field compinfo.cfields) in *)
(* 	JCDvarianttype(compinfo.cname,fields) :: field_tags *)

	Errormsg.s (Cil.error "Union type %s not allowed" compinfo.cname)

    | GCompTagDecl _ -> [] (* No struct/union declaration in Jessie *)

    | GEnumTag(enuminfo,_loc) ->
	assert (not (enuminfo.eitems = []));
	let enums =
	  List.map (fun (_,enum,_) -> value_of_integral_expr enum) enuminfo.eitems
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

    | GVarDecl(_,v,loc) ->
	(* Keep only declarations for which there is no definition *)
	if List.mem v vardefs
	  || (isFunctionType v.vtype && in_jessie_memory_model v.vname) then []
	else if isFunctionType v.vtype then
	  let rtyp,params = match unrollType v.vtype with
	    | TFun(rt,pt,_,_) -> rt, argsToList pt
	    | _ -> assert false
	  in
	  let formal (s,typ,_) = ctype typ, unique_name_if_empty s in
	  let formals = List.map formal params in
	  let id = mkidentifier v.vname loc in
	  let funspec =
	    Kernel_function.get_spec (Globals.Functions.get v)
	  in
          [JCDfun(ctype rtyp,id,formals,spec funspec,None)]
	else
	  [JCDvar(ctype v.vtype,v.vname,None)]

    | GVar(v,{init=None},_loc) ->
	[JCDvar(ctype v.vtype,v.vname,None)]

    | GVar(_v,_iinfo,_loc) ->
	(* Initialization should have been rewritten as code in an
	 * initialization function, that is called by the main function in
	 * global analyses and ignored otherwise.
	 *)
	assert false

    | GFun(f,loc) ->
	if in_jessie_memory_model f.svar.vname then [] else
	  let rty = match unrollType f.svar.vtype with
	    | TFun(ty,_,_,_) -> ty
	    | _ -> assert false
	  in
	  let formal v = ctype v.vtype, v.vname in
	  let formals = List.map formal f.sformals in
	  let local v =
	    mkexpr (JCPEdecl(ctype v.vtype,v.vname,None)) v.vdecl
	  in
	  let locals = List.rev (List.rev_map local f.slocals) in
	  let body = mkexpr (JCPEblock(statement_list f.sbody.bstmts)) loc in
	  let body = locals @ [body] in
	  let body = mkexpr (JCPEblock body) loc in
	  let id = mkidentifier f.svar.vname f.svar.vdecl in
	  let funspec =
	    Kernel_function.get_spec (Globals.Functions.get f.svar)
	  in
	  ignore
	    (reg_loc ~id:f.svar.vname
	      ~name:("Function " ^ f.svar.vname) f.svar.vdecl);
	  [JCDfun(ctype rty,id,formals,spec funspec,Some body)]

    | GAsm _ -> [] (* No assembly in Jessie *)

    | GPragma _ -> [] (* Pragmas treated separately *)

    | GText _ -> [] (* Ignore text in Jessie *)

    | GAnnot(la,_loc) -> annotation la
  in
  let loc = get_globalLoc g in
  List.map (fun dnode -> mkdecl dnode loc) dnodes

let integral_type name ty =
  let min = Num.num_of_big_int (min_value_of_integral_type ty) in
  let max = Num.num_of_big_int (max_value_of_integral_type ty) in
  mkdecl (JCDenum_type(name,min,max)) Loc.dummy_position

let all_integral_kinds =
  let rec all_ik = function
    | IBool -> IBool :: (all_ik IChar)
    | IChar -> IChar :: (all_ik ISChar)
    | ISChar -> ISChar :: (all_ik IUChar)
    | IUChar -> IUChar :: (all_ik IInt)
    | IInt -> IInt :: (all_ik IUInt)
    | IUInt -> IUInt :: (all_ik IShort)
    | IShort -> IShort :: (all_ik IUShort)
    | IUShort -> IUShort :: (all_ik ILong)
    | ILong -> ILong :: (all_ik IULong)
    | IULong -> IULong :: (all_ik ILongLong)
    | ILongLong -> ILongLong :: (all_ik IULongLong)
    | IULongLong -> IULongLong :: []
  in
  all_ik IBool

let integral_types () =
  if Cmdline.Jessie.IntModel.get_val () = Cmdline.Jessie.IMexact then
    []
  else
    let names_already_seen = ref StringSet.empty in
    List.flatten (List.map (fun ik ->
      let ty = TInt(ik,[]) in
      let name = name_of_integral_type ty in
      if StringSet.mem name !names_already_seen then [] else
	begin
	  names_already_seen := StringSet.add name !names_already_seen;
	  [integral_type name ty]
	end
    ) all_integral_kinds)

let type_conversions () =
  let typconv_axiom ty1 ty1_to_ty2 ty2_to_ty1 =
    let x = PExpr.mkvar ~name:"x" () in
    let app1 = PExpr.mkapp ~fun_name:ty1_to_ty2 ~args:[x] () in
    let app2 = PExpr.mkapp ~fun_name:ty2_to_ty1 ~args:[app1] () in
    let eq = PExpr.mkeq ~expr1:x ~expr2:app2 () in
    let forall = PExpr.mkforall ~typ:(ctype ty1) ~vars:["x"] ~body:eq () in
    PDecl.mklemma_def ~name:(unique_name (ty1_to_ty2 ^ "_axiom")) ~axiom:true
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
    if Globals.has_entry_point () then
      let gif =
	Kernel_function.get_definition (Globals.Functions.get_glob_init f)
      in
      f.globals @ [GFun(gif,Loc.dummy_position)]
    else f.globals
  in
  let vardefs =
    List.rev (List.rev_map defined_var (List.filter filter_defined globals))
  in
  (* Define all integral types as enumerated types in Jessie *)
  integral_types () 
  (* Define conversion functions and identity axiom for back 
     and forth conversion *)
  @ type_conversions ()
  @ List.flatten (List.rev (List.rev_map (global vardefs) globals))

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
	| _ -> []
      end
  | GPragma _ -> []
  | _ -> []

let pragmas f =
  (match Cmdline.Jessie.IntModel.get_val () with
    | Cmdline.Jessie.IMexact -> []
    | Cmdline.Jessie.IMbounded -> [Jc_output.JCint_model Jc_env.IMbounded]
    | Cmdline.Jessie.IMmodulo -> [Jc_output.JCint_model Jc_env.IMmodulo])
  @ Jc_output.JCinvariant_policy Jc_env.InvArguments
  :: List.flatten (List.rev (List.rev_map pragma f.globals))


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
