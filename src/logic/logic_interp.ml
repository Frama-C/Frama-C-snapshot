(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil
open Cil_types
open Cilutil
open Db
open Db_types
open Logic_typing
open Extlib
open Ast_info
open Visitor

exception Error of Cil_types.location * string

let error loc s = raise (Error (loc, s))

let find_var kf stmt file x =
  let vi =
    let lookup = List.find (fun vi -> vi.vname = x) in
    let rec lookup_global = function
      | [] -> raise Not_found
      | (GVar (vi,_,_) | GVarDecl (_,vi,_ )) :: _ when vi.vname = x -> vi
      | _ :: r -> lookup_global r
    in
    let fd = Kernel_function.get_definition kf in
    try
      lookup fd.slocals
    with Not_found -> try
      lookup fd.sformals
    with Not_found -> try
      lookup_global file.globals
    with Not_found ->
      error (loc_stmt stmt) ("Unbound variable " ^ x)
  in
  cvar_to_lvar vi

let code_annot kf stmt ~before:_ s =
  let file = Cil_state.file () in
  let loc = snd (Cabshelper.currentLoc ()) in
  let pa = match Logic_lexer.annot (loc, s) with
    | Logic_ptree.Acode_annot (_,a) ->
	a
    | _ ->
	error (loc_stmt stmt)
	  "Syntax error (expecting a code annotation)"
  in
  let module LT =
    Logic_typing.Make
      (struct
	 let annonCompFieldName = Cabs2cil.annonCompFieldName
	 let conditionalConversion = Cabs2cil.conditionalConversion

	 let find_var x = find_var kf stmt file x

	 let find_enum_tag _ = assert false (*TODO*)

	 let find_comp_type ~kind:_ _s = assert false (*TODO*)

	 let find_type _s = assert false (*TODO*)

	 let find_label s = Kernel_function.find_label kf s
         include Logic_env
       end)
  in
  LT.code_annot pa

let expr kf stmt s =
  let file = Cil_state.file () in
  let pa_expr =
    Logic_lexer.lexpr
      (Lexing.dummy_pos,
       s)
  in
  let module LT =
    Logic_typing.Make
      (struct
	 let annonCompFieldName = Cabs2cil.annonCompFieldName
	 let integralPromotion = Cabs2cil.integralPromotion
	 let arithmeticConversion = Cabs2cil.arithmeticConversion
	 let conditionalConversion = Cabs2cil.conditionalConversion

	 let find_var x = find_var kf stmt file x

	 let find_enum_tag _x = assert false (*TODO*)

	 let find_comp_type ~kind:_ _s = assert false (*TODO*)


	 let find_type _s = assert false (*TODO*)

	 let find_label s = Kernel_function.find_label kf s
         include Logic_env
       end)
  in
  LT.term (Logic_typing.make_here_label ()) pa_expr

let lval kf stmt s =
  match (expr kf stmt s).term_node with
  | TLval lv -> lv
  | _ -> error (loc_stmt stmt) "Syntax error (expecting an lvalue)"

(* may raise [Invalid_argument "not a lvalue"] *)
let error_lval () = raise (Invalid_argument "not a lvalue")

(* Force conversion from terms to expressions by returning, along with
 * the result, a map of the sub-terms that could not be converted as
 * a map from fresh variables to terms or term left-values. *)

let logic_type_to_typ = function
  | Ctype typ -> typ
  | Linteger -> TInt(ILongLong,[]) (*TODO: to have an unlimited integer type
                                    in the logic interpretation*)
  | Lreal -> TFloat(FLongDouble,[]) (* TODO: handle reals, not floats... *)
  | Ltype("boolean",[]) -> TInt(ILongLong,[])
  | Ltype _ | Lvar _ | Larrow _ -> error_lval ()

let empty_term_env = {
  term_lhosts = VarinfoMap.empty;
  terms = VarinfoMap.empty;
  vars = VarinfoMap.empty;
  tsets_elems = VarinfoMap.empty;
  tsets_lhosts = VarinfoMap.empty;
}

let is_empty_term_env env =
  VarinfoMap.is_empty env.term_lhosts
  && VarinfoMap.is_empty env.terms
  && VarinfoMap.is_empty env.vars
  && VarinfoMap.is_empty env.tsets_elems
  && VarinfoMap.is_empty env.tsets_lhosts

let merge_term_env env1 env2 =
  {
    term_lhosts =
      VarinfoMap.fold VarinfoMap.add env1.term_lhosts env2.term_lhosts;
    terms = VarinfoMap.fold VarinfoMap.add env1.terms env2.terms;
    vars = VarinfoMap.fold VarinfoMap.add env1.vars env2.vars;
    tsets_elems =
      VarinfoMap.fold VarinfoMap.add env1.tsets_elems env2.tsets_elems;
    tsets_lhosts =
      VarinfoMap.fold VarinfoMap.add env1.tsets_lhosts env2.tsets_lhosts;
  }

let add_opaque_term t env =
  (* Precise the type when possible *)
  let ty = match t.term_type with Ctype ty -> ty | _ -> voidType in
  let v = makePseudoVar ty in
  let env = { env with terms = VarinfoMap.add v t env.terms; } in
  Lval(Var v,NoOffset), env

let add_opaque_var v env =
  let ty = match v.lv_type with Ctype ty -> ty | _ -> assert false in
  let pv = makePseudoVar ty in
  let env = { env with vars = VarinfoMap.add pv v env.vars; } in
  Var pv, env

let add_opaque_term_lhost lhost env =
  let v = makePseudoVar voidType in
  let env =
    { env with term_lhosts = VarinfoMap.add v lhost env.term_lhosts; }
  in
  Var v, env

let add_opaque_result ty env =
  let pv = makePseudoVar ty in
  let env =
    { env with term_lhosts = VarinfoMap.add pv TResult env.term_lhosts; }
  in
  Var pv, env

let add_opaque_var' v env =
  let ty = match v.lv_type with Ctype ty -> ty | _ -> assert false in
  let pv = makePseudoVar ty in
  let env = { env with vars = VarinfoMap.add pv v env.vars; } in
  Var pv, env

let add_opaque_result' ty env =
  let pv = makePseudoVar ty in
  let env =
    { env with tsets_lhosts = VarinfoMap.add pv TSResult env.tsets_lhosts; }
  in
  Var pv, env

let add_opaque_tsets_elem ts env =
  let v = makePseudoVar voidType in
  let env = { env with tsets_elems = VarinfoMap.add v ts env.tsets_elems; } in
  Lval(Var v,NoOffset), env

let add_opaque_tsets_lhost lhost env =
  let v = makePseudoVar voidType in
  let env =
    { env with tsets_lhosts = VarinfoMap.add v lhost env.tsets_lhosts; }
  in
  Var v, env

let rec force_term_to_exp ~result_type t =
  let e,env = match t.term_node with
    | TLval tlv ->
	let lv,env = force_term_lval_to_lval ~result_type tlv in
	Lval lv, env
    | TAddrOf tlv ->
	let lv,env = force_term_lval_to_lval ~result_type tlv in
	AddrOf lv, env
    | TStartOf tlv ->
	let lv,env = force_term_lval_to_lval ~result_type tlv in
	StartOf lv, env
    | TSizeOfE t' ->
	let e,env = force_term_to_exp ~result_type t' in
	SizeOfE e, env
    | TAlignOfE t' ->
	let e,env = force_term_to_exp ~result_type t' in
	AlignOfE e, env
    | TUnOp(unop,t') ->
	let e,env = force_term_to_exp ~result_type t' in
	UnOp(unop,e,logic_type_to_typ t.term_type), env
    | TBinOp(binop,t1,t2) ->
	let e1,env1 = force_term_to_exp ~result_type t1 in
	let e2,env2 = force_term_to_exp ~result_type t2 in
	let env = merge_term_env env1 env2 in
	BinOp(binop,e1,e2,logic_type_to_typ t.term_type), env
    | TSizeOfStr string -> SizeOfStr string, empty_term_env
    | TConst constant -> Const constant, empty_term_env
    | TCastE(ty,t') ->
	let e,env = force_term_to_exp ~result_type t' in
	CastE(ty,e), env
    | TAlignOf ty -> AlignOf ty, empty_term_env
    | TSizeOf ty -> SizeOf ty, empty_term_env
    | Tapp _ | TDataCons _ | Tif _ | Told _ | Tat _ | Tbase_addr _
    | Tblock_length _ | Tnull | TCoerce _ | TCoerceE _ | TUpdate _
    | Tlambda _ | Ttypeof _ | Ttype _ | Ttsets _
        ->
	add_opaque_term t empty_term_env
  in
  Info(e,exp_info_of_term t), env

and force_term_lval_to_lval ~result_type (lhost,toff) =
  let lhost,env1 = force_term_lhost_to_lhost ~result_type lhost in
  let off,env2 = force_term_offset_to_offset ~result_type toff in
  let env = merge_term_env env1 env2 in
  (lhost,off), env

and force_term_lhost_to_lhost ~result_type lhost = match lhost with
  | TVar v ->
      begin match v.lv_origin with
	| Some v -> Var v, empty_term_env
	| None ->
	    begin match v.lv_type with
	      | Ctype _ty -> add_opaque_var v empty_term_env
	      | _ -> add_opaque_term_lhost lhost empty_term_env
	    end
      end
  | TMem t ->
      let e,env = force_term_to_exp ~result_type t in
      Mem e, env
  | TResult -> add_opaque_result result_type empty_term_env

and force_term_offset_to_offset ~result_type = function
  | TNoOffset -> NoOffset, empty_term_env
  | TField(fi,toff) ->
      let off,env = force_term_offset_to_offset ~result_type toff in
      Field(fi,off), env
  | TIndex(t,toff) ->
      let e,env1 = force_term_to_exp ~result_type t in
      let off,env2 = force_term_offset_to_offset ~result_type toff in
      let env = merge_term_env env1 env2 in
      Index(e,off), env

(* Force back conversion from expression to term, using the environment
 * constructed during the conversion from term to expression. It expects
 * the top-level expression to be wrapped into an Info, as well as every
 * top-level expression that appears under a left-value. *)

let rec force_back_exp_to_term env e =
  let rec internal_force_back env einfo e =
    let einfo = match e with
      | Info(_e,einfo) -> einfo
      | e ->
	  let einfo = {
	    exp_loc = einfo.exp_loc; (* reuse location from above in tree *)
	    exp_type = Ctype(typeOf e);
	    exp_name = [];
	  } in
	  einfo
    in
    let tnode = match stripInfo e with
      | Info _ -> assert false
      | Const c -> TConst c
      | Lval(Var v,NoOffset as lv) ->
	  begin try (VarinfoMap.find v env.terms).term_node
	  with Not_found ->
	    TLval(force_back_lval_to_term_lval env lv)
	  end
      | Lval lv -> TLval(force_back_lval_to_term_lval env lv)
      | SizeOf ty -> TSizeOf ty
      | SizeOfE e -> TSizeOfE(internal_force_back env einfo e)
      | SizeOfStr s -> TSizeOfStr s
      | AlignOf ty -> TAlignOf ty
      | AlignOfE e -> TAlignOfE(internal_force_back env einfo e)
      | UnOp(op,e,_) -> TUnOp(op,internal_force_back env einfo e)
      | BinOp(op,e1,e2,_) ->
	  TBinOp(op,
	  internal_force_back env einfo e1,
	  internal_force_back env einfo e2)
      | CastE(ty,e) -> TCastE(ty,internal_force_back env einfo e)
      | AddrOf lv -> TAddrOf(force_back_lval_to_term_lval env lv)
      | StartOf lv -> TStartOf(force_back_lval_to_term_lval env lv)
    in
    term_of_exp_info tnode einfo
  in
  let einfo = match e with
    | Info(_e,einfo) -> einfo
    | _ ->
	{
	  exp_loc = locUnknown;
	  exp_type = Ctype(typeOf e);
	  exp_name = [];
	}
  in
  internal_force_back env einfo e

and force_back_offset_to_term_offset env = function
  | NoOffset -> TNoOffset
  | Field(fi,off) ->
      TField(fi,force_back_offset_to_term_offset env off)
  | Index(idx,off) ->
      TIndex(
	force_back_exp_to_term env idx,
	force_back_offset_to_term_offset env off)

and force_back_lhost_to_term_lhost env = function
  | Var v ->
      begin try
	let logv = VarinfoMap.find v env.vars in
	logv.lv_type <- Ctype v.vtype;
	TVar logv
      with Not_found ->
	try VarinfoMap.find v env.term_lhosts
	with Not_found -> TVar(cvar_to_lvar v)
      end
  | Mem e -> TMem(force_back_exp_to_term env e)

and force_back_lval_to_term_lval env (host,off) =
  force_back_lhost_to_term_lhost env host,
  force_back_offset_to_term_offset env off

(* Force conversion from expr to term *)

let rec force_exp_to_term loc e =
  (* Precise location when possible *)
  let loc = match e with
    | Info(_e,einfo) -> einfo.exp_loc
    | _e -> loc
  in
  let tnode = match stripInfo e with
    | Info _ -> assert false
    | Const c -> TConst c
    | Lval lv -> TLval(force_lval_to_term_lval loc lv)
    | SizeOf ty -> TSizeOf ty
    | SizeOfE e -> TSizeOfE(force_exp_to_term loc e)
    | SizeOfStr s -> TSizeOfStr s
    | AlignOf ty -> TAlignOf ty
    | AlignOfE e -> TAlignOfE(force_exp_to_term loc e)
    | UnOp(op,e,_) -> TUnOp(op,force_exp_to_term loc e)
    | BinOp(op,e1,e2,_) ->
	TBinOp(op,
	force_exp_to_term loc e1,
	force_exp_to_term loc e2)
    | CastE(ty,e) -> TCastE(ty,force_exp_to_term loc e)
    | AddrOf lv -> TAddrOf(force_lval_to_term_lval loc lv)
    | StartOf lv -> TStartOf(force_lval_to_term_lval loc lv)
  in
  {
    term_node = tnode;
    term_loc = loc;
    term_type = Ctype(typeOf e);
    term_name = [];
  }

and force_offset_to_term_offset loc = function
  | NoOffset -> TNoOffset
  | Field(fi,off) ->
      TField(fi,force_offset_to_term_offset loc off)
  | Index(idx,off) ->
      TIndex(
	force_exp_to_term loc idx,
	force_offset_to_term_offset loc off)

and force_lhost_to_term_lhost loc = function
  | Var v -> TVar(cvar_to_lvar v)
  | Mem e -> TMem(force_exp_to_term loc e)

and force_lval_to_term_lval loc (host,off) =
  force_lhost_to_term_lhost loc host,
  force_offset_to_term_offset loc off

(* Force conversion from expr to predicate *)

let rec force_exp_to_predicate loc e =
  (* Precise location when possible *)
  let loc = match e with
    | Info(_e,einfo) -> einfo.exp_loc
    | _e -> loc
  in
  let pnode = match stripInfo e with
    | Info _ -> assert false
    | Const c ->
	begin match possible_value_of_integral_const c with
	  | Some i -> if i = 0L then Pfalse else Ptrue
	  | None -> assert false
	end
    | UnOp(LNot,e',_) -> Pnot(force_exp_to_predicate loc e')
    | BinOp(LAnd,e1,e2,_) ->
	Pand(force_exp_to_predicate loc e1,force_exp_to_predicate loc e2)
    | BinOp(LOr,e1,e2,_) ->
	Por(force_exp_to_predicate loc e1,force_exp_to_predicate loc e2)
    | BinOp(op,e1,e2,_) ->
	let rel = match op with
	  | Lt -> Rlt
	  | Gt -> Rgt
	  | Le -> Rle
	  | Ge -> Rge
	  | Eq -> Req
	  | Ne -> Rneq
	  | _ -> assert false
	in
	Prel(rel,force_exp_to_term loc e1,force_exp_to_term loc e2)
    | Lval _ | CastE _ | AddrOf _ | StartOf _ | UnOp _
    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
	assert false
  in
  {
    name = [];
    loc = loc;
    content = pnode;
  }

let rec force_exp_to_assertion loc e =
  Logic_const.new_code_annotation
    (AAssert([], force_exp_to_predicate loc e, {status=Unknown}))

(* Transform range in tsets into comprehension, and back when possible *)

class fromRangeToComprehension behavior prj =

  let ranges : (logic_var * term option * term option) list ref = ref [] in

  let add_range vi t1opt t2opt =
    ranges := (vi,t1opt,t2opt) :: !ranges
  in

  let make_comprehension ts =
    let ts =
      List.fold_left (fun ts (v,t1opt,t2opt) ->
	let vt = variable_term locUnknown v in
	let popt = match t1opt,t2opt with
	  | None,None -> None
	  | Some t1,None ->
	      Some(predicate locUnknown (Prel(Rle,t1,vt)))
	  | None,Some t2 ->
	      Some(predicate locUnknown (Prel(Rle,vt,t2)))
	  | Some t1,Some t2 ->
	      let p1 = predicate locUnknown (Prel(Rle,t1,vt)) in
	      let p2 = predicate locUnknown (Prel(Rle,vt,t2)) in
	      Some(predicate locUnknown (Pand(p1,p2)))
	in
	TSComprehension(ts,[v],popt)
      ) ts !ranges
    in
    ranges := [];
    ts
  in
object

  inherit Visitor.generic_frama_c_visitor prj behavior as super

  method vtsets ts = ChangeDoChildrenPost(ts,make_comprehension)

  method vtsets_elem ts =
    match ts with
      | TSAdd_range(ts,t1opt,t2opt) ->
	  let v = make_temp_logic_var Linteger in
	  add_range v t1opt t2opt;
	  let vt = variable_term locUnknown v in
	  ChangeDoChildrenPost (TSAdd_index(ts,vt), fun x -> x)
      | TSLval _ | TSStartOf _ | TSConst _ | TSAdd_index _
      | TSCastE _ | TSat _ | TSapp _ | TSAddrOf _ ->
	  DoChildren

  method vtsets_offset tsoff =
    match tsoff with
      | TSRange(t1opt,t2opt,tsoff') ->
	  let v = make_temp_logic_var Linteger in
	  add_range v t1opt t2opt;
	  let vt = variable_term locUnknown v in
	  ChangeDoChildrenPost (TSIndex(vt,tsoff'), fun x -> x)
      | TSNoOffset | TSIndex _ | TSField _ ->
	  DoChildren

end

let from_range_to_comprehension behavior prj file =
  let visitor = new fromRangeToComprehension behavior prj in
  visitFramacFile visitor file

class fromComprehensionToRange behavior prj =

  let ranges : (term option * term option) LogicVarHashtbl.t =
    LogicVarHashtbl.create 17
  in

  let add_range vi t1opt t2opt =
    LogicVarHashtbl.add ranges vi (t1opt,t2opt)
  in

  let index_variables_of_tset ts =
    let vars = ref LogicVarSet.empty in
    ignore
      (visitCilTsets
	(object
	  inherit nopCilVisitor
	  method vtsets_elem = function
	    | TSAdd_index(_ts,{term_node=TLval(TVar v,TNoOffset)}) ->
		vars := LogicVarSet.add v !vars;
		DoChildren
	    | _ -> DoChildren
	  method vtsets_offset = function
	    | TSIndex({term_node=TLval(TVar v,TNoOffset)},_tsoff) ->
		vars := LogicVarSet.add v !vars;
		DoChildren
	    | _ -> DoChildren
	end)
	ts);
    !vars
  in

  let bounds_of_variable v popt =
    let rec bounds p =
      match p.content with
	| Prel(Rle,{term_node=TLval(TVar v',TNoOffset)},t)
	    when LogicVarComparable.equal v v' ->
	    None,Some t
	| Prel(Rle,t,{term_node=TLval(TVar v',TNoOffset)})
	    when LogicVarComparable.equal v v' ->
	    Some t,None
	| Pand(p1,p2) ->
	    begin match bounds p1, bounds p2 with
	      | (Some t1,None),(None,Some t2) | (None,Some t2),(Some t1,None) ->
		  Some t1,Some t2
	      | _ -> failwith "Not a bounding predicate"
	    end
	| _ -> failwith "Not a bounding predicate"
    in
    match popt with None -> None,None | Some p -> bounds p
  in
object(self)

  inherit Visitor.generic_frama_c_visitor prj behavior as super

  method vtsets = function
    | TSComprehension(ts,[v],popt) ->
	let index_vars = index_variables_of_tset ts in
	(* Only accept for now comprehension on index variables *)
	if LogicVarSet.mem v index_vars then begin
	  let t1opt,t2opt = bounds_of_variable v popt in
	  add_range v t1opt t2opt;
          ChangeTo (visitCilTsets (self :> cilVisitor) ts)
        end else DoChildren
    | TSComprehension _ | TSSingleton _ | TSEmpty | TSUnion _ | TSInter _ ->
	DoChildren

  method vtsets_lhost = function
    | TSVar v ->
	assert (not (LogicVarHashtbl.mem ranges v));
	DoChildren
    | _ -> DoChildren

  method vtsets_elem ts =
    match ts with
      | TSAdd_index(ts',{term_node=TLval(TVar v,TNoOffset)}) ->
	  begin try
	    let t1opt,t2opt = LogicVarHashtbl.find ranges v in
	    let ts = TSAdd_range(ts',t1opt,t2opt) in
	    ChangeDoChildrenPost (ts, fun x -> x)
	  with Not_found -> DoChildren end
      | TSAdd_index _ | TSLval _ | TSStartOf _ | TSConst _ | TSAdd_range _
      | TSCastE _ | TSat _ | TSapp _ | TSAddrOf _ ->
	  DoChildren

  method vtsets_offset tsoff =
    match tsoff with
      | TSIndex({term_node=TLval(TVar v,TNoOffset)},tsoff') ->
	  begin try
	    let t1opt,t2opt = LogicVarHashtbl.find ranges v in
	    let tsoff = TSRange(t1opt,t2opt,tsoff') in
	    ChangeDoChildrenPost (tsoff, fun x -> x)
	  with Not_found -> DoChildren end
      | TSIndex _ | TSRange _ | TSNoOffset | TSField _ ->
	  DoChildren

end

let from_comprehension_to_range behavior prj file =
  let visitor = new fromComprehensionToRange behavior prj in
  visitFramacFile visitor file

(* Force conversion from tsets to expr *)

let rec force_tsets_elem_to_exp ~result_type ts =
  match ts with
    | TSLval tslv ->
	let lv,env = force_tsets_lval_to_lval ~result_type tslv in
	Lval lv, env
    | TSStartOf tslv ->
	let lv,env = force_tsets_lval_to_lval ~result_type tslv in
	StartOf lv, env
    | TSAddrOf tslv ->
        let lv, env = force_tsets_lval_to_lval ~result_type tslv in
        AddrOf lv, env
    | TSConst constant -> Const constant, empty_term_env
    | TSAdd_index(ts',t) ->
	let e1,env1 = force_tsets_elem_to_exp ~result_type ts' in
	let e2,env2 = force_term_to_exp ~result_type t in
	let env = merge_term_env env1 env2 in
	BinOp(PlusPI,e1,e2,logic_type_to_typ (typeOfTsetsElem ts')), env
    | TSAdd_range(_ts',_t1opt,_t2opt) ->
	(* Conversion to expression does not work on range. Conversion of
	 * tsets with ranges to tsets with comprehensions should be performed
	 * first.
	 *)
	assert false
    | TSCastE(ty,ts') ->
	let e,env = force_tsets_elem_to_exp ~result_type ts' in
	CastE(ty,e), env
    | TSat _ | TSapp _ -> add_opaque_tsets_elem ts empty_term_env

and force_tsets_lval_to_lval ~result_type (lhost,toff) =
  let lhost,env1 = force_tsets_lhost_to_lhost ~result_type lhost in
  let off,env2 = force_tsets_offset_to_offset ~result_type toff in
  let env = merge_term_env env1 env2 in
  (lhost,off), env

and force_tsets_lhost_to_lhost ~result_type lhost = match lhost with
  | TSVar v ->
      begin match v.lv_origin with
	| Some v -> Var v, empty_term_env
	| None ->
	    begin match v.lv_type with
	      | Ctype _ty -> add_opaque_var' v empty_term_env
	      | _ -> add_opaque_tsets_lhost lhost empty_term_env
	    end
      end
  | TSMem ts ->
      let e,env = force_tsets_elem_to_exp ~result_type ts in
      Mem e, env
  | TSResult -> add_opaque_result' result_type empty_term_env

and force_tsets_offset_to_offset ~result_type = function
  | TSNoOffset -> NoOffset, empty_term_env
  | TSField(fi,tsoff) ->
      let off,env = force_tsets_offset_to_offset ~result_type tsoff in
      Field(fi,off), env
  | TSIndex(t,tsoff) ->
      let e,env1 = force_term_to_exp ~result_type t in
      let off,env2 = force_tsets_offset_to_offset ~result_type tsoff in
      let env = merge_term_env env1 env2 in
      Index(e,off), env
  | TSRange(_t1opt,_t2opt,_tsoff) ->
      (* Conversion to expression does not work on range. Conversion of
       * tsets with ranges to tsets with comprehensions should be performed
       * first.
       *)
      assert false

(* Force back conversion from expr to tsets *)

let rec force_back_exp_to_tsets_elem env e =
  match stripInfo e with
    | Info _ -> assert false
    | Const c -> TSConst c
    | Lval(Var v,NoOffset as lv) ->
	begin try VarinfoMap.find v env.tsets_elems
	with Not_found ->
	  TSLval(force_back_lval_to_tsets_lval env lv)
	end
    | Lval lv -> TSLval(force_back_lval_to_tsets_lval env lv)
    | BinOp(PlusPI,e1,e2,_ty) ->
	TSAdd_index(
	  force_back_exp_to_tsets_elem env e1,
	  force_back_exp_to_term env e2)
    | CastE(ty,e) -> TSCastE(ty,force_back_exp_to_tsets_elem env e)
    | StartOf lv -> TSStartOf(force_back_lval_to_tsets_lval env lv)
    | AddrOf lv -> TSAddrOf(force_back_lval_to_tsets_lval env lv)
    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ | UnOp _
    | BinOp _ ->
	(* Transformation of expression obtained through conversion from tsets
	 * should not generate parts that are not convertible back.
	 *)
	assert false

and force_back_offset_to_tsets_offset env = function
  | NoOffset -> TSNoOffset
  | Field(fi,off) ->
      TSField(fi,force_back_offset_to_tsets_offset env off)
  | Index(idx,off) ->
      TSIndex(
	force_back_exp_to_term env idx,
	force_back_offset_to_tsets_offset env off)

and force_back_lhost_to_tsets_lhost env = function
  | Var v ->
      begin try
	let logv = VarinfoMap.find v env.vars in
	logv.lv_type <- Ctype v.vtype;
	TSVar logv
      with Not_found ->
	try VarinfoMap.find v env.tsets_lhosts
	with Not_found -> TSVar(cvar_to_lvar v)
      end
  | Mem e -> TSMem(force_back_exp_to_tsets_elem env e)

and force_back_lval_to_tsets_lval env (host,off) =
  force_back_lhost_to_tsets_lhost env host,
  force_back_offset_to_tsets_offset env off

(* Force conversion from tsets to term *)

let rec force_tsets_elem_to_term ts =
  let tnode = match ts with
    | TSLval tslv ->
	let tlv = force_tsets_lval_to_term_lval tslv in
	TLval tlv
    | TSStartOf tslv ->
	let tlv = force_tsets_lval_to_term_lval tslv in
	TStartOf tlv
    | TSAddrOf tslv ->
        let tlv = force_tsets_lval_to_term_lval tslv in
        TAddrOf tlv
    | TSConst constant -> TConst constant
    | TSAdd_index(ts',t) ->
	let t' = force_tsets_elem_to_term ts' in
	TBinOp(PlusPI,t',t)
    | TSAdd_range(_ts',_t1opt,_t2opt) ->
	(* Conversion to term does not work on range. Conversion of
	 * tsets with ranges to tsets with comprehensions should be performed
	 * first.
	 *)
	assert false
    | TSCastE(ty,ts') ->
	let t' = force_tsets_elem_to_term ts' in
	TCastE(ty,t')
    | TSat(ts',lab) ->
	let t' = force_tsets_elem_to_term ts' in
	Tat(t',lab)
    | TSapp (f,labs,args) -> Tapp(f, labs, args)
  in
  {
    term_node = tnode; (* Terms generated have only a correct node *)
    term_loc = locUnknown;
    term_type = Ctype voidType;
    term_name = [];
  }

and force_tsets_lval_to_term_lval (lhost,toff) =
  let lhost = force_tsets_lhost_to_term_lhost lhost in
  let off = force_tsets_offset_to_term_offset toff in
  (lhost,off)

and force_tsets_lhost_to_term_lhost lhost = match lhost with
  | TSVar v -> TVar v
  | TSMem ts -> TMem(force_tsets_elem_to_term ts)
  | TSResult -> TResult

and force_tsets_offset_to_term_offset = function
  | TSNoOffset -> TNoOffset
  | TSField(fi,tsoff) ->
      let toff = force_tsets_offset_to_term_offset tsoff in
      TField(fi,toff)
  | TSIndex(t,tsoff) ->
      let toff = force_tsets_offset_to_term_offset tsoff in
      TIndex(t,toff)
  | TSRange(_t1opt,_t2opt,_tsoff) ->
      (* Conversion to term does not work on range. Conversion of
       * tsets with ranges to tsets with comprehensions should be performed
       * first.
       *)
      assert false

(* Force back conversion from term to tsets *)

let rec force_back_term_to_tsets_elem t =
  match t.term_node with
    | TConst c -> TSConst c
    | TLval tlv -> TSLval(force_back_term_lval_to_tsets_lval tlv)
    | TBinOp(PlusPI,t1,t2) ->
	TSAdd_index(force_back_term_to_tsets_elem t1,t2)
    | Tapp(f,labs,args) -> TSapp(f,labs,args)
    | TCastE(ty,t) -> TSCastE(ty,force_back_term_to_tsets_elem t)
    | TStartOf tlv -> TSStartOf(force_back_term_lval_to_tsets_lval tlv)
    | TAddrOf tlv -> TSAddrOf(force_back_term_lval_to_tsets_lval tlv)
    | Tat(t,lab) -> TSat(force_back_term_to_tsets_elem t,lab)

    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _ | TAlignOf _ | TAlignOfE _ | TUnOp _
    | TBinOp _ | TCoerceE _ | TCoerce _ | Tblock_length _ | Tbase_addr _
    | Told _ | Tif _ | TDataCons _ | Tnull | TUpdate _ | Tlambda _
    | Ttypeof _ | Ttype _ | Ttsets _ ->
	(* Transformation of term obtained through conversion from tsets
	 * should not generate parts that are not convertible back.
	 *)
	assert false

and force_back_term_offset_to_tsets_offset = function
  | TNoOffset -> TSNoOffset
  | TField(fi,toff) ->
      TSField(fi,force_back_term_offset_to_tsets_offset toff)
  | TIndex(idx,toff) ->
      TSIndex(idx,force_back_term_offset_to_tsets_offset toff)

and force_back_term_lhost_to_tsets_lhost = function
  | TVar v -> TSVar v
  | TMem t -> TSMem(force_back_term_to_tsets_elem t)
  | TResult -> TSResult

and force_back_term_lval_to_tsets_lval (host,off) =
  force_back_term_lhost_to_tsets_lhost host,
  force_back_term_offset_to_tsets_offset off

(* Expect conversion to be possible on all sub-terms, otherwise raise an error. *)

let logic_var_to_var { lv_origin = lv } =
  match lv with
    | None -> error_lval ()
    | Some lv -> lv

(* let term_to_exp t =  *)
(*   let e,env = force_term_to_exp ~result_type t in *)
(*   if is_empty_term_env env then e else error_lval () *)

(* let term_lval_to_lval tlv = *)
(*   let lv,env = force_term_lval_to_lval ~result_type tlv in  *)
(*   if is_empty_term_env env then lv else error_lval () *)

(* let term_lhost_to_lhost lhost = *)
(*   let lhost,env = force_term_lhost_to_lhost lhost in  *)
(*   if is_empty_term_env env then lhost else error_lval () *)

(* let term_offset_to_offset toff = *)
(*   let off,env = force_term_offset_to_offset toff in  *)
(*   if is_empty_term_env env then off else error_lval () *)

let rec term_lval_to_lval (lh, lo) = (term_lhost_to_lhost lh, term_offset_to_offset lo)
and term_lhost_to_lhost = function
  | TVar lvar -> Var (logic_var_to_var lvar)
  | TMem lterm -> Mem (term_to_exp lterm)
  | TResult -> error_lval()
and term_offset_to_offset = function
  | TNoOffset -> NoOffset
  | TField (fi, lo) -> Field (fi, term_offset_to_offset lo)
  | TIndex (lexp, lo) -> Index (term_to_exp lexp, term_offset_to_offset lo)
and term_to_exp {term_node = lnode ; term_type = ltype} =
  match lnode with
  | TLval lv -> Lval (term_lval_to_lval lv)
  | TAddrOf lv -> AddrOf (term_lval_to_lval lv)
  | TStartOf lv -> StartOf (term_lval_to_lval lv)
  | TSizeOfE lexp -> SizeOfE (term_to_exp lexp)
  | TAlignOfE lexp -> AlignOfE (term_to_exp lexp)
  | TUnOp (unop, lexp) -> UnOp (unop, term_to_exp lexp, logic_type_to_typ ltype)
  | TBinOp (binop, lexp1, lexp2) -> BinOp (binop, term_to_exp lexp1, term_to_exp lexp2, logic_type_to_typ ltype)
  | TSizeOfStr string -> SizeOfStr string
  | TConst constant -> Const constant
  | TCastE (typ, lexp) -> CastE (typ, term_to_exp lexp)
  | TAlignOf typ -> AlignOf typ
  | TSizeOf typ -> SizeOf typ
  (* additional constructs *)
  | Tapp _ | Tlambda _ | Ttsets _
  | TDataCons _
  | Tif _
  | Told _
  | Tat _
  | Tbase_addr _
  | Tblock_length _
  | Tnull
  | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _
    -> error_lval ()

let rec term_to_lval t =
  match t.term_node with
  | TLval lv -> term_lval_to_lval lv
  | TAddrOf lv -> term_lval_to_lval lv
  | TStartOf lv -> term_lval_to_lval lv
  | Ttsets tset ->
      (match tsets_to_lval tset with
           [ e ] -> e
         |  _ -> error_lval())
  (* additional constructs *)
  | TSizeOfE _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TSizeOfStr _
  | TConst _ | TCastE _ | TAlignOf _ | TSizeOf _ | Tapp _ | Tif _ | Told _
  | Tat _ | Tbase_addr _ | Tblock_length _ | Tnull
  | TCoerce _ | TCoerceE _ | TDataCons _ | TUpdate _ | Tlambda _
  | Ttypeof _ | Ttype _ ->
      error_lval ()

and create_offset_list kind low high o =
  let rec aux acc i =
    if Int64.compare i low < 0 then acc
    else
      aux (Index (Const (CInt64 (i,kind,None)), o)::acc) (Int64.pred i)
  in aux [] high

and create_index_list base kind typ low high =
  let rec aux acc i =
    if Int64.compare i low < 0 then acc
      else
        aux (BinOp(PlusPI,base,Const(CInt64(i,kind,None)),typ)::acc)
          (Int64.pred i)
  in aux [] high

and tsets_offset_to_offset = function
  | TSNoOffset -> [NoOffset]
  | TSIndex(idx,o) ->
      let idx = term_to_exp idx in
      List.map (fun x -> Index(idx,x)) (tsets_offset_to_offset o)
  | TSRange(low,high,o) ->
      begin match low, high with
          Some low, Some high ->
            let low = term_to_exp low in
            let high = term_to_exp high in
            begin match Cil.constFold true low, Cil.constFold true high with
                Const(CInt64(low,kind,_)), Const(CInt64(high,_,_)) ->
                  let o = tsets_offset_to_offset o in
                  List.flatten
                    (List.map (fun o -> create_offset_list kind low high o) o)
              | _ -> error_lval()
            end
        | _ -> error_lval()
      end
  | TSField(fi,o) ->
      List.map(fun x -> Field(fi,x)) (tsets_offset_to_offset o)

and tsets_elem_to_exp = function
    TSLval lv -> List.map (fun x -> Lval x) (tsets_lval_to_lval lv)
  | TSStartOf lv -> List.map (fun x -> StartOf x) (tsets_lval_to_lval lv)
  | TSAddrOf lv -> List.map (fun x -> AddrOf x) (tsets_lval_to_lval lv)
  | TSConst c -> [Const c]
  | TSAdd_index(base,idx) ->
      let typ = Cil.typeOfTsetsElem base in
      let typ = match typ with
        | Ctype ty -> ty
        | _ -> error_lval()
      in
      let base = tsets_elem_to_exp base in
      let idx = term_to_exp idx in
        List.map (fun base -> BinOp(PlusPI,base,idx,typ)) base
  | TSAdd_range(base,low,high) ->
      let typ = Cil.typeOfTsetsElem base in
      let typ = match typ with
          Ctype ty -> ty
        | _ -> error_lval() in
      begin match low, high with
        | Some low, Some high ->
            let low = term_to_exp low in
            let high = term_to_exp high in
            begin match Cil.constFold true low, Cil.constFold true high with
                Const(CInt64(low,kind,_)), Const(CInt64(high,_,_)) ->
                  let base = tsets_elem_to_exp base in
                  List.flatten
                    (List.map (fun x -> create_index_list x kind typ low high)
                       base)
              | _ -> error_lval()
            end
        | _ -> error_lval()
      end
  | TSCastE(typ,e) ->
      List.map (fun x -> Cil.mkCast x typ) (tsets_elem_to_exp e)
  | TSat(_,_) | TSapp _ -> error_lval ()

and tsets_lval_to_lval (h,o) =
  let h = tsets_lhost_to_lhost h in
  let o = tsets_offset_to_offset o in
  List.flatten
    (List.map (fun h -> List.map (fun o -> (h,o)) o) h)

and tsets_lhost_to_lhost = function
    TSVar v -> [Var (logic_var_to_var v)]
  | TSResult -> error_lval()
  | TSMem e -> List.map (fun x -> Mem x) (tsets_elem_to_exp e)

and tsets_elem_to_lval lv =
  let lvs = tsets_elem_to_exp lv in
  List.map (function Lval lv | StartOf lv -> lv | _ -> error_lval()) lvs

and tsets_elem_to_offset h = function
    TSLval(h',o) | TSStartOf (h',o) ->
      (match h with None -> Some h', tsets_offset_to_offset o
         | Some h when Logic_const.is_same_tsets_lhost h h' ->
             Some h, tsets_offset_to_offset o
         | Some _ -> error_lval()
      )
  | TSat ((TSLval(TSResult,_)) as lv,LogicLabel "Post") ->
      tsets_elem_to_offset h lv
  | TSAddrOf _
  | TSAdd_index _ | TSAdd_range _ | TSConst _ | TSCastE _ | TSat _ | TSapp _ ->
      error_lval()

and tsets_to_offset loc =
  let rec aux b loc =
    match loc with
        TSSingleton lv -> tsets_elem_to_offset b lv
      | TSEmpty -> b,[]
      | TSUnion(locs) ->
          List.fold_left
            (fun (b,l) x -> let (b,l') = aux b x in b, l @ l') (b,[]) locs
      | TSComprehension _ | TSInter _ -> error_lval()
  in snd (aux None loc)

and tsets_to_lval loc =
  match loc with
      TSSingleton lv -> tsets_elem_to_lval lv
    | TSUnion locs ->
        List.flatten (List.map tsets_to_lval locs)
    | TSEmpty -> []
    | TSInter _ -> error_lval()
    | TSComprehension(_t1,_quant,_pred) ->  error_lval()
        (*TODO: interpret comprehension, at least some simple patterns *)

and tsets_to_exp loc =
  match loc with
      TSSingleton lv -> tsets_elem_to_exp lv
    | TSUnion locs ->
        List.flatten (List.map tsets_to_exp locs)
    | TSEmpty -> []
    | TSInter _ -> error_lval()
    | TSComprehension(_t1,_quant,_pred) ->  error_lval()
        (*TODO: interpret comprehension, at least some simple patterns *)

(** Utilities to identify [Locations.Zone.t] involved into [rooted_code_annotation]. *)
module To_zone : sig
  type t_ctx = Properties.Interp.To_zone.t_ctx
  val mk_ctx_func_contrat: kernel_function -> state_opt:bool option -> t_ctx
  val mk_ctx_stmt_contrat: kernel_function -> stmt -> state_opt:bool option -> t_ctx
  val mk_ctx_stmt_annot: kernel_function -> stmt -> before:bool -> t_ctx
  type t = Properties.Interp.To_zone.t
  type t_decl = VarinfoSet.t
  type t_pragmas = Properties.Interp.To_zone.t_pragmas
  val from_term: term -> t_ctx -> (t list * t_decl)
    (** Entry point to get zones
        needed to evaluate the [term] relative to the [ctx] of interpretation. *)
  val from_terms: term list -> t_ctx -> (t list * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [terms] relative to the [ctx] of interpretation. *)
  val from_pred: predicate named -> t_ctx -> (t list * t_decl)
    (** Entry point to get zones
        needed to evaluate the [predicate] relative to the [ctx] of interpretation. *)
  val from_preds: predicate named list -> t_ctx -> (t list * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [predicates] relative to the [ctx] of interpretation. *)
  val from_zone: identified_tsets zone -> t_ctx -> (t list * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [predicates] relative to the [ctx] of interpretation. *)
  val from_zones: identified_tsets zone list -> t_ctx -> (t list * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [predicates] relative to the [ctx] of interpretation. *)
  val from_stmt_annot: code_annotation -> before:bool -> (stmt * kernel_function) -> (t list * t_decl) * t_pragmas
    (** Entry point to get zones
        needed to evaluate code annotations of this [stmt]. *)
  val from_stmt_annots:
    ((rooted_code_annotation before_after) -> bool) option ->
    (stmt * kernel_function) -> (t list * t_decl) * t_pragmas
    (** Entry point to get zones
        needed to evaluate code annotations of this [stmt]. *)
  val from_func_annots:
    ((stmt -> unit) -> kernel_function -> unit) ->
    ((rooted_code_annotation before_after) -> bool) option ->
    kernel_function -> (t list * t_decl) * t_pragmas
    (** Entry point to get zones
        needed to evaluate code annotations of this [kf]. *)
  val code_annot_filter:
    (rooted_code_annotation before_after) -> ai:bool ->
    user_assert:bool -> slicing_pragma:bool ->
    loop_inv:bool -> loop_var:bool -> others:bool -> bool
    (** To quickly build a annotation filter *)
  end
  = struct
    type t_ctx = Properties.Interp.To_zone.t_ctx
    let mk_ctx_func_contrat kf ~state_opt = {Properties.Interp.To_zone.state_opt=state_opt ; ki_opt=None ; kf=kf}
    let mk_ctx_stmt_contrat kf ki ~state_opt = {Properties.Interp.To_zone.state_opt=state_opt ; ki_opt=Some(ki,false) ; kf=kf}
    let mk_ctx_stmt_annot kf ki ~before = {Properties.Interp.To_zone.state_opt=Some before ; ki_opt=Some(ki,true) ; kf=kf}
    type t = Properties.Interp.To_zone.t
    type t_decl = VarinfoSet.t
    type t_pragmas = Properties.Interp.To_zone.t_pragmas
    let other_zones = Cilutil.StmtHashtbl.create 7
    let locals = ref VarinfoSet.empty
    let pragmas = ref {Properties.Interp.To_zone.ctrl=Cilutil.StmtSet.empty ; stmt=Cilutil.StmtSet.empty}

    let add_result ~before ki zone =
      let (zone_true, zone_false) =
        try Cilutil.StmtHashtbl.find other_zones ki
        with Not_found -> (Locations.Zone.bottom, Locations.Zone.bottom)
      in
      Cilutil.StmtHashtbl.replace other_zones ki
        (if before then (Locations.Zone.join zone_true zone, zone_false)
         else (zone_true, Locations.Zone.join zone_false zone))

    let get_result () =
      let result =
        (Cilutil.StmtHashtbl.fold
           (fun ki (zone_true, zone_false) other_zones ->
              let add before zone others =
                if Locations.Zone.equal Locations.Zone.bottom zone then
                  others else
                    { Properties.Interp.To_zone.before=before;
                      ki = ki; zone=zone} :: others
              in add true zone_true (add false zone_false other_zones))
           other_zones []),
        !locals
     in (* clear references for the next time when giving the result *)
        Cilutil.StmtHashtbl.clear other_zones;
        locals := VarinfoSet.empty ;
        result

    let get_annot_result () =
      let annot_result = (get_result ()), !pragmas
      in (* clear references for the next time when giving the result *)
        pragmas := {Properties.Interp.To_zone.ctrl=Cilutil.StmtSet.empty ; stmt=Cilutil.StmtSet.empty} ;
        annot_result

    (** Logic_var utility: *)
    let extract_locals logicvars =
      LogicVarSet.fold (fun lv cvars ->
                          match lv.lv_origin with
                            | None -> cvars
                            | Some cvar ->
                                if cvar.Cil_types.vglob
                                then
                                  cvars
                                else VarinfoSet.add cvar cvars)
        logicvars VarinfoSet.empty

    (** Term utility:
        Extract C local variables occuring into a [term]. *)
    let extract_locals_from_term term =
      extract_locals (extract_free_logicvars_from_term term)

    (** Predicate utility:
        Extract C local variables occuring into a [term]. *)
    let extract_locals_from_pred pred =
      extract_locals (extract_free_logicvars_from_predicate pred)

    type abs_label = | AbsLabel_here
                     | AbsLabel_pre
                     | AbsLabel_post
                     | AbsLabel_stmt of stmt
    class populate_zone before_opt ki_opt kf =
      (* interpretation from the
       * - pre-state if  [before_opt=Some true]
       * - post-state if [before_opt=Some false]
       * - pre-state with possible reference to the post-state if [before_opt=None]
       * of a property relative to
       * - the contract of function [kf] when [ki_opt=None]
       * othewise [ki_opt=Some(ki, code_annot)],
       * - the contract of the statement [ki] when [code_annot=false]
       * - the annotation of the statement [ki] when [code_annot=true]
       *)
      object(self)
        inherit
          Visitor.generic_frama_c_visitor  (Project.current())
            (Cil.inplace_visit())
        val mutable current_label = AbsLabel_here

        method private get_ctrl_point () =
          let get_fct_entry_point () =
            (* TODO: to replace by true, None *)
            true, (try Some (Kernel_function.find_first_stmt kf)
                   with Kernel_function.No_Statement -> None)  (* raised when [kf] has no code. *)
          in
          let get_ctrl_point dft =
            let before = Extlib.may_map (fun before -> before) ~dft before_opt in
              match ki_opt with
                | None -> (* function contract *)

                    if before then get_fct_entry_point ()
                    else before, None
                      (* statement contract *)
                | Some (ki,_) ->  (* statement contract and code annotation *)
                    before, Some ki
          in
          let result = match current_label with
            | AbsLabel_stmt stmt -> true, Some stmt
            | AbsLabel_pre -> get_fct_entry_point ()
            | AbsLabel_here -> get_ctrl_point true
            | AbsLabel_post -> get_ctrl_point false
          in (* TODO: the method should be able to return result directly *)
            match result with
              | current_before, Some current_ki -> current_before, current_ki
              | _ -> raise (Extlib.NotYetImplemented "[logic_interp] clause related to a function contract")

        method private change_label: 'a.abs_label -> 'a -> 'a visitAction =
          fun label x ->
          let old_label = current_label in
          current_label <- label;
          ChangeDoChildrenPost
            (x,fun x -> current_label <- old_label; x)

        method private change_label_to_here: 'a.'a -> 'a visitAction =
          fun x ->
            self#change_label AbsLabel_here x

        method private change_label_to_old: 'a.'a -> 'a visitAction =
         fun x ->
          match ki_opt,before_opt with
              (* function contract *)
            | None,Some true -> failwith ("The use of the label Old is forbiden inside clauses related the pre-state of function contracts.")
            | None,None
            | None,Some false -> self#change_label AbsLabel_pre x (* refers to the pre-state of the contract. *)
                (* statement contract *)
            | Some (_ki,false),Some true  -> failwith ("The use of the label Old is forbiden inside clauses related the pre-state of statement contracts.")
            | Some (ki,false),None
            | Some (ki,false),Some false  -> self#change_label (AbsLabel_stmt ki) x (* refers to the pre-state of the contract. *)
                (* code annotation *)
            | Some (_ki,true),None
            | Some (_ki,true),Some _ -> self#change_label AbsLabel_pre x (* refers to the pre-state of the function contract. *)

        method private change_label_to_post: 'a.'a -> 'a visitAction =
         fun x -> (* allowed when [before_opt=None] for function/statement contracts *)
          match ki_opt,before_opt with
              (* function contract *)
            | None,Some _ -> failwith ("Function contract where the use of the label Post is forbiden.")
            | None,None -> self#change_label AbsLabel_post x (* refers to the post-state of the contract. *)
                (* statement contract *)
            | Some (_ki,false),Some _  -> failwith ("Statement contract where the use of the label Post is forbiden.")
            | Some (_ki,false),None -> self#change_label AbsLabel_post x (* refers to the pre-state of the contract. *)
                (* code annotation *)
            | Some (_ki,true), _ -> failwith ("The use of the label Post is forbiden inside code annotations.")

        method private change_label_to_pre: 'a.'a -> 'a visitAction =
         fun x ->
          match ki_opt with
              (* function contract *)
            | None -> failwith ("The use of the label Pre is forbiden inside function contracts.")
                (* statement contract *)
                (* code annotation *)
            | Some _ -> self#change_label AbsLabel_pre x (* refers to the pre-state of the function contract. *)

        method private change_label_to_stmt: 'a.stmt -> 'a -> 'a visitAction =
          fun stmt x ->
          match ki_opt with
              (* function contract *)
            | None -> failwith ("the use of C labels is forbiden inside clauses related function contracts.")
                (* statement contract *)
                (* code annotation *)
            | Some _ -> self#change_label (AbsLabel_stmt stmt) x (* refers to the state at the C label of the statement [stmt]. *)

        method vpredicate p = match p with
          | Pold _ | Pat (_, LogicLabel "Old") -> self#change_label_to_old p
          | Pat (_, LogicLabel "Here") -> self#change_label_to_here p
          | Pat (_, LogicLabel "Pre") -> self#change_label_to_pre p
          | Pat (_, LogicLabel "Post") -> self#change_label_to_post p
          | Pat (_, StmtLabel st) -> self#change_label_to_stmt !st p
          | Pat (_,LogicLabel s) ->
              failwith ("unknown logic label" ^ s)
          | Pfresh _ -> 
              raise (Extlib.NotYetImplemented ("[logic_interp] \\fresh()"))
              (* assert false *) (*VP: can't we do something better? *)
          | _ -> DoChildren

        method vterm t =
          match t.term_node with
              TAddrOf _ | TLval _ | TStartOf _  ->
                let exp = try (* to be removed *)
                  !Db.Properties.Interp.term_to_exp t
                with Invalid_argument str ->
                  raise (Extlib.NotYetImplemented ("[logic_interp] "^ str))
                in
                let current_before, current_ki = self#get_ctrl_point () in
                let loc = try (* to be removed *)
                  !Db.From.find_deps_no_transitivity (Kstmt current_ki) exp
                with Invalid_argument str ->
                  raise (Extlib.NotYetImplemented ("[logic_interp] "^ str))
                in add_result current_before current_ki loc; SkipChildren
            | Told _ | Tat (_, LogicLabel "Old") -> self#change_label_to_old t
            | Tat (_, LogicLabel "Here") -> self#change_label_to_here t
            | Tat (_, LogicLabel "Pre") -> self#change_label_to_pre t
            | Tat (_, LogicLabel "Post") -> self#change_label_to_post t
           | Tat (_, StmtLabel st) -> self#change_label_to_stmt !st t
            | Tat(_,LogicLabel s) ->
                failwith ("unknown logic label" ^ s)
            | _ -> DoChildren

        method vtsets_elem t =
          match t with
            | TSat (_, LogicLabel "Old") -> self#change_label_to_old t
            | TSat (_, LogicLabel "Here") -> self#change_label_to_here t
            | TSat (_, LogicLabel "Pre") -> self#change_label_to_pre t
            | TSat (_, LogicLabel "Post") -> self#change_label_to_post t
            | TSat (_, StmtLabel st) -> self#change_label_to_stmt !st t
            | TSat(_,LogicLabel s) ->
                failwith ("unknown logic label" ^ s)
            | _ ->
                let exps = try (* to be removed *)
                  !Db.Properties.Interp.tsets_elem_to_exp t
                with Invalid_argument str ->
                  raise (Extlib.NotYetImplemented ("[logic_interp] "^ str))
                in
                let current_before, current_ki = self#get_ctrl_point () in
                List.iter
                  (fun e ->
                     let loc = try (* to be removed *)
                       !Db.From.find_deps_no_transitivity (Kstmt current_ki) e
                     with Invalid_argument str ->
                       raise (Extlib.NotYetImplemented ("[logic_interp] "^ str))
                     in add_result current_before current_ki loc) exps;
                SkipChildren
      end

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the [term]
        relative to the [ctx] of interpretation. *)
    let from_term term ctx =
      ignore(visitFramacTerm (new populate_zone
                                ctx.Properties.Interp.To_zone.state_opt
                                ctx.Properties.Interp.To_zone.ki_opt
                                ctx.Properties.Interp.To_zone.kf) term);
      get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [terms]
        relative to the [ctx] of interpretation. *)
    let from_terms terms ctx =
      let f x =
        ignore(visitFramacTerm (new populate_zone
                                  ctx.Properties.Interp.To_zone.state_opt
                                  ctx.Properties.Interp.To_zone.ki_opt
                                  ctx.Properties.Interp.To_zone.kf) x)
      in
        List.iter f terms;
        get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the [pred]
        relative to the [ctx] of interpretation. *)
    let from_pred pred ctx =
      ignore(visitFramacPredicateNamed
               (new populate_zone
                  ctx.Properties.Interp.To_zone.state_opt
                  ctx.Properties.Interp.To_zone.ki_opt
                  ctx.Properties.Interp.To_zone.kf) pred);
      get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [preds]
        relative to the [ctx] of interpretation. *)
    let from_preds preds ctx =
      let f pred =
        ignore(visitFramacPredicateNamed
                 (new populate_zone ctx.Properties.Interp.To_zone.state_opt
                    ctx.Properties.Interp.To_zone.ki_opt
                    ctx.Properties.Interp.To_zone.kf) pred)
      in
        List.iter f preds;
        get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [zones]
        relative to the [ctx] of interpretation. *)
    let from_zone zone ctx =
      begin
        match zone with
            Nothing -> ()
          | Location tset ->
              ignore(
                visitFramacTsets
                  (new populate_zone ctx.Properties.Interp.To_zone.state_opt
                     ctx.Properties.Interp.To_zone.ki_opt
                     ctx.Properties.Interp.To_zone.kf) tset.its_content)
      end ;
      get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [zones]
        relative to the [ctx] of interpretation. *)
    let from_zones zones ctx =
      let f zone =
        match zone with
            Nothing -> ()
          | Location tset ->
              ignore(
                visitFramacTsets
                  (new populate_zone ctx.Properties.Interp.To_zone.state_opt
                     ctx.Properties.Interp.To_zone.ki_opt
                     ctx.Properties.Interp.To_zone.kf) tset.its_content)
      in
        List.iter f zones;
        get_result ()

   (** Used by annotations entry points. *)
    let get_zone_from_annot a before (ki,kf) loop_body_opt =
      let get_zone_from_term x =
        ignore(visitFramacTerm (new populate_zone (Some before) (Some (ki,true)) kf) x)
      and get_zone_from_pred x =
        ignore(visitFramacPredicateNamed (new populate_zone (Some before) (Some (ki,true)) kf) x)
      in match a with
      | APragma (Slice_pragma (SPexpr term) | Impact_pragma (IPexpr term)) ->
            (* to preserve the interpretation of the pragma *)
            get_zone_from_term term;
            (* to select the declaration of the variables *)
            locals := VarinfoSet.union (extract_locals_from_term term) !locals;
            (* to select the reachability of the pragma *)
            pragmas := { !pragmas with Properties.Interp.To_zone.ctrl =
               Cilutil.StmtSet.add ki !pragmas.Properties.Interp.To_zone.ctrl }
        | APragma (Slice_pragma SPctrl) ->
            (* to select the reachability of the pragma *)
            pragmas :=
              { !pragmas with
                  Properties.Interp.To_zone.ctrl =
                  Cilutil.StmtSet.add ki
                    !pragmas.Properties.Interp.To_zone.ctrl
              }
        | APragma (Slice_pragma SPstmt | Impact_pragma IPstmt) ->
            (* to preserve the effect of the statement *)
            pragmas :=
              { !pragmas with
                  Properties.Interp.To_zone.stmt =
                  Cilutil.StmtSet.add ki
                    !pragmas.Properties.Interp.To_zone.stmt}
        | AAssert (_behav,pred,_) ->
            (* to preserve the interpretation of the assertion *)
            get_zone_from_pred pred;
            (* to select the declaration of the variables *)
            locals := VarinfoSet.union (extract_locals_from_pred pred) !locals
        | AAssume pred ->
            (* to preserve the interpretation of the assume clause *)
            get_zone_from_pred pred;
            (* to select the declaration of the variables *)
            locals := VarinfoSet.union (extract_locals_from_pred pred) !locals
        | AInvariant (_behav,true,pred) -> (* loop invariant *)
            (* to preserve the interpretation of the loop invariant *)
            ignore(
              visitFramacPredicateNamed
                (new populate_zone (Some true) (Some (Extlib.the loop_body_opt, true)) kf) pred)
        | AInvariant (_behav,false,pred) -> (* code invariant *)
            (* to preserve the interpretation of the code invariant *)
            get_zone_from_pred pred;
            (* to select the declaration of the variables *)
            locals := VarinfoSet.union (extract_locals_from_pred pred) !locals
        | AVariant (term,_) ->
            (* to preserve the interpretation of the variant *)
            ignore(
              visitFramacTerm
                (new populate_zone (Some true) (Some (Extlib.the loop_body_opt, true)) kf) term)
        | APragma (Loop_pragma (Unroll_level term)) ->
            (* to select the declaration of the variables *)
            locals := VarinfoSet.union (extract_locals_from_term term) !locals
        | APragma (Loop_pragma (Widen_hints terms))
        | APragma (Loop_pragma (Widen_variables terms)) ->
            (* to select the declaration of the variables *)
            List.iter (fun term ->
                         locals := VarinfoSet.union (extract_locals_from_term term) !locals)
              terms
        | AAssigns (zone,deps) -> (* loop assigns *)
            (* to preserve the effect of the loop body *)
            pragmas := { !pragmas with Properties.Interp.To_zone.stmt =
                Cilutil.StmtSet.add ki !pragmas.Properties.Interp.To_zone.stmt};
            (* to preserve the interpretation of the locations *)
            List.iter (fun zone ->
                         match zone with
                             Nothing -> ()
                           | Location tset ->
                               ignore(
                                 visitFramacTsets
                                   (new populate_zone (Some true) (Some (Extlib.the loop_body_opt, true)) kf) tset.its_content))
              (zone::deps)
        | AStmtSpec _ -> (* TODO *)
            raise (Extlib.NotYetImplemented "[logic_interp] statement contract")

   (** Used by annotations entry points. *)
    let get_zone_from_annotation a stmt loop_body_opt =
      let before,a = match a with
        | Before a -> true, a
        | After a -> false, a
      in
      match a with
        | WP _ -> assert false
            (* code_annot_filter must reject it, or else TODO *)
        | User a -> get_zone_from_annot a.annot_content before stmt loop_body_opt
        | AI (_,a) -> get_zone_from_annot a.annot_content before stmt loop_body_opt

    (** Used by annotations entry points. *)
    let get_from_stmt_annots code_annot_filter ((ki, _kf) as stmt) =
      match code_annot_filter with
      | Some code_annot_filter ->
          let code_annot_list = Annotations.get ki in
          let loop_body_opt = match ki.skind with
              Loop(_,{bstmts=body::_},_,_,_) -> Some body
            | _ -> None
          in
          List.iter
            (fun a ->
               if code_annot_filter a then
                 get_zone_from_annotation a stmt loop_body_opt)
            code_annot_list
      | None -> ()


    (** Used by annotations entry points. *)
    let from_ki_annot annot ~before ((ki, _kf) as stmt) =
      let real_ki = match ki.skind with
          Loop(_,{bstmts = loop_entry::_},_,_,_) -> Some loop_entry
        | _ -> None
      in
      get_zone_from_annot annot.annot_content before stmt real_ki

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the code annotations related to this [stmt]. *)
    let from_stmt_annot annot ~before stmt =
      from_ki_annot annot ~before stmt;
      get_annot_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the code annotations related to this [stmt]. *)
    let from_stmt_annots code_annot_filter stmt =
      get_from_stmt_annots code_annot_filter stmt ;
      get_annot_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the code annotations related to this [kf]. *)
    let from_func_annots iter_on_kf_stmt code_annot_filter kf =
      let from_stmt_annots ki =
        get_from_stmt_annots code_annot_filter (ki, kf)
      in iter_on_kf_stmt from_stmt_annots kf;
        get_annot_result ()

    (** To quickly build a annotation filter *)
    let code_annot_filter annot ~ai ~user_assert ~slicing_pragma
         ~loop_inv ~loop_var ~others =
      let a = match annot with
          | Before a -> a
          | After a -> a
      in let code_annot_filter a =
        match a with
          | APragma (Slice_pragma _) -> slicing_pragma
          | AAssume _ | AAssert _ -> user_assert
          | AVariant _ -> loop_var
          | AInvariant(_behav,true,_pred) -> loop_inv
          | AInvariant(_,false,_)
          | AAssigns _
          | APragma (Loop_pragma _)| APragma (Impact_pragma _) -> others
          | AStmtSpec _  (* TODO: statement contract *) -> false
      in match a with
        | WP _ -> others
        | User a -> code_annot_filter a.annot_content
        | AI _ -> ai
  end

let () =
  Properties.Interp.code_annot := code_annot;
  Properties.Interp.lval := lval;
  Properties.Interp.expr := expr;
  Properties.Interp.term_lval_to_lval := term_lval_to_lval;
  Properties.Interp.term_to_exp := term_to_exp;

  Properties.Interp.force_term_to_exp := force_term_to_exp;
  Properties.Interp.force_back_exp_to_term := force_back_exp_to_term;
  Properties.Interp.force_term_lval_to_lval := force_term_lval_to_lval;
  Properties.Interp.force_back_lval_to_term_lval := force_back_lval_to_term_lval;
  Properties.Interp.force_term_offset_to_offset := force_term_offset_to_offset;
  Properties.Interp.force_back_offset_to_term_offset := force_back_offset_to_term_offset;

  Properties.Interp.force_exp_to_term := force_exp_to_term;
  Properties.Interp.force_lval_to_term_lval := force_lval_to_term_lval;
  Properties.Interp.force_exp_to_predicate := force_exp_to_predicate;
  Properties.Interp.force_exp_to_assertion := force_exp_to_assertion;

  Properties.Interp.from_range_to_comprehension := from_range_to_comprehension;
  Properties.Interp.from_comprehension_to_range := from_comprehension_to_range;

  Properties.Interp.force_tsets_elem_to_exp := force_tsets_elem_to_exp;
  Properties.Interp.force_back_exp_to_tsets_elem := force_back_exp_to_tsets_elem;
  Properties.Interp.force_tsets_lval_to_lval := force_tsets_lval_to_lval;
  Properties.Interp.force_back_lval_to_tsets_lval := force_back_lval_to_tsets_lval;
  Properties.Interp.force_tsets_offset_to_offset := force_tsets_offset_to_offset;
  Properties.Interp.force_back_offset_to_tsets_offset := force_back_offset_to_tsets_offset;

  Properties.Interp.force_tsets_elem_to_term := force_tsets_elem_to_term;
  Properties.Interp.force_back_term_to_tsets_elem := force_back_term_to_tsets_elem;
  Properties.Interp.force_tsets_lval_to_term_lval := force_tsets_lval_to_term_lval;
  Properties.Interp.force_back_term_lval_to_tsets_lval := force_back_term_lval_to_tsets_lval;

  Properties.Interp.term_to_lval := term_to_lval;
  Properties.Interp.term_offset_to_offset := term_offset_to_offset;
  Properties.Interp.tsets_to_lval := tsets_to_lval;
  Properties.Interp.tsets_to_offset := tsets_to_offset;
  Properties.Interp.tsets_elem_to_lval := tsets_elem_to_lval;
  Properties.Interp.tsets_to_exp:= tsets_to_exp;
  Properties.Interp.tsets_elem_to_exp:= tsets_elem_to_exp;
  Properties.Interp.To_zone.mk_ctx_func_contrat := To_zone.mk_ctx_func_contrat;
  Properties.Interp.To_zone.mk_ctx_stmt_contrat := To_zone.mk_ctx_stmt_contrat;
  Properties.Interp.To_zone.mk_ctx_stmt_annot := To_zone.mk_ctx_stmt_annot;
  Properties.Interp.To_zone.from_term := To_zone.from_term;
  Properties.Interp.To_zone.from_terms := To_zone.from_terms;
  Properties.Interp.To_zone.from_pred := To_zone.from_pred;
  Properties.Interp.To_zone.from_preds := To_zone.from_preds;
  Properties.Interp.To_zone.from_stmt_annot := To_zone.from_stmt_annot;
  Properties.Interp.To_zone.from_stmt_annots := To_zone.from_stmt_annots;
  Properties.Interp.To_zone.from_func_annots := To_zone.from_func_annots;
  Properties.Interp.To_zone.code_annot_filter := To_zone.code_annot_filter


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
