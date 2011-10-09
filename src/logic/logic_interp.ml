(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
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
open Cil_datatype
open Ast_info

exception Error of Cil_types.location * string

let error loc s = raise (Error (loc, s))

let find_var kf stmt _file x =
  let vi =
    try Globals.Vars.find_from_astinfo x (VLocal kf)
    with Not_found ->
      try
        Globals.Vars.find_from_astinfo x (VFormal kf)
      with Not_found ->
        try
          Globals.Vars.find_from_astinfo x VGlobal
        with Not_found ->
          error (Stmt.loc stmt) ("Unbound variable " ^ x)
  in
  cvar_to_lvar vi

let code_annot kf stmt s =
  let file = Ast.get () in
  let loc = snd (Cabshelper.currentLoc ()) in
  let pa = match Logic_lexer.annot (loc, s) with
    | Logic_ptree.Acode_annot (_,a) -> a
    | _ ->
        error (Stmt.loc stmt)
          "Syntax error (expecting a code annotation)"
  in
  let module LT =
    Logic_typing.Make
      (struct
         let anonCompFieldName = Cabs2cil.anonCompFieldName
         let conditionalConversion = Cabs2cil.logicConditionalConversion

         let find_macro _ = raise Not_found

         let find_var x = find_var kf stmt file x

         let find_enum_tag _ = assert false (*TODO*)

         let find_comp_type ~kind:_ _s = assert false (*TODO*)

         let find_comp_field info s =
           let field = Cil.getCompField info s in
           Field(field,NoOffset)

         let find_type _s = assert false (*TODO*)

         let find_label s = Kernel_function.find_label kf s
         include Logic_env

         let add_logic_function =
           add_logic_function_gen Logic_utils.is_same_logic_profile

         let integral_cast ty t =
           raise
             (Failure
                (Pretty_utils.sfprintf
                   "term %a has type %a, but %a is expected."
                   Cil.d_term t Cil.d_logic_type Linteger Cil.d_type ty))

       end)
  in
  LT.code_annot (Stmt.loc stmt)
    (Logic_utils.get_behavior_names (Kernel_function.get_spec kf))
    (Ctype (Kernel_function.get_return_type kf)) pa

let expr kf stmt s =
  let file = Ast.get () in
  let pa_expr =
    Logic_lexer.lexpr
      (Lexing.dummy_pos,
       s)
  in
  let module LT =
    Logic_typing.Make
      (struct
         let anonCompFieldName = Cabs2cil.anonCompFieldName
         let integralPromotion = Cabs2cil.integralPromotion
         let arithmeticConversion = Cabs2cil.arithmeticConversion
         let conditionalConversion = Cabs2cil.logicConditionalConversion

         let find_macro _ = raise Not_found

         let find_var x = find_var kf stmt file x

         let find_enum_tag _x = assert false (*TODO*)

         let find_comp_type ~kind:_ _s = assert false (*TODO*)

         let find_comp_field info s =
           let field = Cil.getCompField info s in
           Field(field,NoOffset)

         let find_type _s = assert false (*TODO*)

         let find_label s = Kernel_function.find_label kf s
         include Logic_env

         let add_logic_function =
           add_logic_function_gen Logic_utils.is_same_logic_profile
         let integral_cast ty t =
           raise
             (Failure
                (Pretty_utils.sfprintf
                   "term %a has type %a, but %a is expected."
                   Cil.d_term t Cil.d_logic_type Linteger Cil.d_type ty))


       end)
  in
  LT.term (Logic_typing.append_here_label (Logic_typing.Lenv.empty())) pa_expr

let lval kf stmt s =
  match (expr kf stmt s).term_node with
  | TLval lv -> lv
  | _ -> error (Stmt.loc stmt) "Syntax error (expecting an lvalue)"

(* may raise [Invalid_argument "not an lvalue"] *)
let error_lval () = invalid_arg "not an lvalue"

(* Force conversion from terms to expressions by returning, along with
 * the result, a map of the sub-terms that could not be converted as
 * a map from fresh variables to terms or term left-values. *)

let rec logic_type_to_typ = function
  | Ctype typ -> typ
  | Linteger -> TInt(ILongLong,[]) (*TODO: to have an unlimited integer type
                                    in the logic interpretation*)
  | Lreal -> TFloat(FLongDouble,[]) (* TODO: handle reals, not floats... *)
  | Ltype({lt_name = name},[]) when name = Utf8_logic.boolean  ->
      TInt(ILongLong,[])
  | Ltype({lt_name = "set"},[t]) -> logic_type_to_typ t
  | Ltype _ | Lvar _ | Larrow _ -> error_lval ()

let empty_term_env =
  { term_lhosts = Varinfo.Map.empty;
    terms = Varinfo.Map.empty;
    vars = Varinfo.Map.empty }

let is_empty_term_env env =
  Varinfo.Map.is_empty env.term_lhosts
  && Varinfo.Map.is_empty env.terms
  && Varinfo.Map.is_empty env.vars

let merge_term_env env1 env2 =
  {
    term_lhosts =
      Varinfo.Map.fold Varinfo.Map.add env1.term_lhosts env2.term_lhosts;
    terms = Varinfo.Map.fold Varinfo.Map.add env1.terms env2.terms;
    vars = Varinfo.Map.fold Varinfo.Map.add env1.vars env2.vars;
  }

let add_opaque_term t env =
  (* Precise the type when possible *)
  let ty = match t.term_type with Ctype ty -> ty | _ -> voidType in
  let v = makePseudoVar ty in
  let env = { env with terms = Varinfo.Map.add v t env.terms; } in
  Lval(Var v,NoOffset), env

let add_opaque_var v env =
  let ty = match v.lv_type with Ctype ty -> ty | _ -> assert false in
  let pv = makePseudoVar ty in
  let env = { env with vars = Varinfo.Map.add pv v env.vars; } in
  Var pv, env

let add_opaque_term_lhost lhost env =
  let v = makePseudoVar voidType in
  let env =
    { env with term_lhosts = Varinfo.Map.add v lhost env.term_lhosts; }
  in
  Var v, env

let add_opaque_result ty env =
  let pv = makePseudoVar ty in
  let env =
    { env with term_lhosts = Varinfo.Map.add pv (TResult ty) env.term_lhosts; }
  in
  Var pv, env

let add_opaque_var' v env =
  let ty = match v.lv_type with Ctype ty -> ty | _ -> assert false in
  let pv = makePseudoVar ty in
  let env = { env with vars = Varinfo.Map.add pv v env.vars; } in
  Var pv, env

let rec force_term_to_exp t =
  let loc = t.term_loc in
  let e,env = match t.term_node with
    | TLval tlv ->
        (match Logic_utils.unroll_type t.term_type with
           | Ctype (TArray _) -> add_opaque_term t empty_term_env
           | _ -> let lv,env = force_term_lval_to_lval tlv in Lval lv, env)
    | TAddrOf tlv ->
        let lv,env = force_term_lval_to_lval tlv in AddrOf lv, env
    | TStartOf tlv ->
        let lv,env = force_term_lval_to_lval tlv in StartOf lv, env
    | TSizeOfE t' ->
        let e,env = force_term_to_exp t' in SizeOfE e, env
    | TAlignOfE t' ->
        let e,env = force_term_to_exp t' in AlignOfE e, env
    | TUnOp(unop,t') ->
        let e,env = force_term_to_exp t' in
        UnOp(unop,e,logic_type_to_typ t.term_type), env
    | TBinOp(binop,t1,t2) ->
        let e1,env1 = force_term_to_exp t1 in
        let e2,env2 = force_term_to_exp t2 in
        let env = merge_term_env env1 env2 in
        BinOp(binop,e1,e2,logic_type_to_typ t.term_type), env
    | TSizeOfStr string -> SizeOfStr string, empty_term_env
    | TConst constant -> Const constant, empty_term_env
    | TCastE(ty,t') ->
        let e,env = force_term_to_exp t' in CastE(ty,e), env
    | TAlignOf ty -> AlignOf ty, empty_term_env
    | TSizeOf ty -> SizeOf ty, empty_term_env
    | Tapp _ | TDataCons _ | Tif _ | Tat _ | Tbase_addr _
    | Tblock_length _ | Tnull | TCoerce _ | TCoerceE _ | TUpdate _
    | Tlambda _ | Ttypeof _ | Ttype _ | Tcomprehension _
    | Tunion _ | Tinter _ | Tempty_set | Trange _ | Tlet _
        ->
        add_opaque_term t empty_term_env
  in
  new_exp ~loc (Info(new_exp ~loc e,exp_info_of_term t)), env

and force_term_lval_to_lval (lhost,toff) =
  let lhost,env1 = force_term_lhost_to_lhost lhost in
  let off,env2 = force_term_offset_to_offset toff in
  let env = merge_term_env env1 env2 in
  (lhost,off), env

and force_term_lhost_to_lhost lhost = match lhost with
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
      let e,env = force_term_to_exp t in
      Mem e, env
  | TResult ty -> add_opaque_result ty empty_term_env

and force_term_offset_to_offset = function
  | TNoOffset -> NoOffset, empty_term_env
  | TField(fi,toff) ->
      let off,env = force_term_offset_to_offset toff in
      Field(fi,off), env
  | TIndex(t,toff) ->
      let e,env1 = force_term_to_exp t in
      let off,env2 = force_term_offset_to_offset toff in
      let env = merge_term_env env1 env2 in
      Index(e,off), env

(* Force back conversion from expression to term, using the environment
 * constructed during the conversion from term to expression. It expects
 * the top-level expression to be wrapped into an Info, as well as every
 * top-level expression that appears under a left-value. *)

let rec force_back_exp_to_term env e =
  let rec internal_force_back env e =
    let einfo = match e.enode with
      | Info(_e,einfo) -> einfo
      | _ -> { exp_type = Ctype(typeOf e); exp_name = []; }
    in
    let tnode = match (stripInfo e).enode with
      | Info _ -> assert false
      | Const c -> TConst c
      | Lval(Var v,NoOffset as lv) ->
        begin try (Varinfo.Map.find v env.terms).term_node
          with Not_found ->
            TLval(force_back_lval_to_term_lval env lv)
        end
      | Lval lv -> TLval(force_back_lval_to_term_lval env lv)
      | SizeOf ty -> TSizeOf ty
      | SizeOfE e -> TSizeOfE(internal_force_back env e)
      | SizeOfStr s -> TSizeOfStr s
      | AlignOf ty -> TAlignOf ty
      | AlignOfE e -> TAlignOfE(internal_force_back env e)
      | UnOp(op,e,_) -> TUnOp(op,internal_force_back env e)
      | BinOp(op,e1,e2,_) ->
          TBinOp(op,
                 internal_force_back env e1, internal_force_back env e2)
      | CastE(ty,e) -> TCastE(ty,internal_force_back env e)
      | AddrOf lv -> TAddrOf(force_back_lval_to_term_lval env lv)
      | StartOf lv -> TStartOf(force_back_lval_to_term_lval env lv)
    in
    term_of_exp_info e.eloc tnode einfo
  in
  internal_force_back env e

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
            let logv = Varinfo.Map.find v env.vars in
            logv.lv_type <- Ctype v.vtype;
            TVar logv
      with Not_found ->
        try Varinfo.Map.find v env.term_lhosts
        with Not_found -> TVar(cvar_to_lvar v)
    end
  | Mem e -> TMem(force_back_exp_to_term env e)

and force_back_lval_to_term_lval env (host,off) =
  force_back_lhost_to_term_lhost env host,
  force_back_offset_to_term_offset env off

(* Force conversion from expr to term *)

let rec force_exp_to_term e =
  let tnode = match (stripInfo e).enode with
    | Info _ -> assert false
    | Const c -> TConst c
    | Lval lv -> TLval(force_lval_to_term_lval lv)
    | SizeOf ty -> TSizeOf ty
    | SizeOfE e -> TSizeOfE(force_exp_to_term e)
    | SizeOfStr s -> TSizeOfStr s
    | AlignOf ty -> TAlignOf ty
    | AlignOfE e -> TAlignOfE(force_exp_to_term e)
    | UnOp(op,e,_) -> TUnOp(op,force_exp_to_term e)
    | BinOp(op,e1,e2,_) ->
        TBinOp(op, force_exp_to_term e1, force_exp_to_term e2)
    | CastE(ty,e) -> TCastE(ty,force_exp_to_term e)
    | AddrOf lv -> TAddrOf(force_lval_to_term_lval lv)
    | StartOf lv -> TStartOf(force_lval_to_term_lval lv)
  in
  {
    term_node = tnode;
    term_loc = e.eloc;
    term_type = Ctype(typeOf e);
    term_name = [];
  }

and force_offset_to_term_offset = function
  | NoOffset -> TNoOffset
  | Field(fi,off) ->
      TField(fi,force_offset_to_term_offset off)
  | Index(idx,off) ->
      TIndex(
        force_exp_to_term idx,
        force_offset_to_term_offset off)

and force_lhost_to_term_lhost = function
  | Var v -> TVar(cvar_to_lvar v)
  | Mem e -> TMem(force_exp_to_term e)

and force_lval_to_term_lval (host,off) =
  force_lhost_to_term_lhost host,
  force_offset_to_term_offset off

(* Force conversion from expr to predicate *)

let rec force_exp_to_predicate e =
  let pnode = match (stripInfo e).enode with
    | Info _ -> assert false
    | Const c ->
        begin match possible_value_of_integral_const c with
          | Some i -> if My_bigint.equal i My_bigint.zero then Pfalse else Ptrue
          | None -> assert false
        end
    | UnOp(LNot,e',_) -> Pnot(force_exp_to_predicate e')
    | BinOp(LAnd,e1,e2,_) ->
        Pand(force_exp_to_predicate e1,force_exp_to_predicate e2)
    | BinOp(LOr,e1,e2,_) ->
        Por(force_exp_to_predicate e1,force_exp_to_predicate e2)
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
        Prel(rel,force_exp_to_term e1,force_exp_to_term e2)
    | Lval _ | CastE _ | AddrOf _ | StartOf _ | UnOp _
    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
        assert false
  in
  { name = []; loc = e.eloc; content = pnode; }

let rec force_exp_to_assertion e =
  Logic_const.new_code_annotation (AAssert([], force_exp_to_predicate e))

(* Transform range in tsets into comprehension, and back when possible *)

let rec add_range vi t1opt t2opt = ranges := (vi,t1opt,t2opt) :: !ranges
and no_range_offset = function
TNoOffset -> true
  | TField(_,offs) -> no_range_offset offs
  | TIndex({term_type = Ltype ({ lt_name = "set"},[_])},_) -> false
  | TIndex(_,offs) -> no_range_offset offs
and make_comprehension ts =
  let ts = match ts.term_node with
      TLval(ts',offs) when no_range_offset offs ->
        (match ts' with
        | TMem { term_type = Ltype ({lt_name = "set"},[_])} -> ts
        | TMem _ | TVar _ | TResult _ ->
          { ts with term_type = Logic_const.type_of_element ts.term_type}
        )
    | _ -> ts
  in
  let loc = ts.term_loc in
  let ts =
    List.fold_left
      (fun ts (v,t1opt,t2opt) ->
         let vt = variable_term loc v in
         let popt = match t1opt,t2opt with
           | None,None -> None
           | Some t1,None -> Some(predicate t1.term_loc (Prel(Rle,t1,vt)))
           | None,Some t2 -> Some(predicate t2.term_loc (Prel(Rle,vt,t2)))
           | Some t1,Some t2 ->
               let p1 = predicate t1.term_loc (Prel(Rle,t1,vt)) in
               let p2 = predicate t2.term_loc (Prel(Rle,vt,t2)) in
               let loc = (fst t1.term_loc, snd t2.term_loc) in
               Some(predicate loc (Pand(p1,p2)))
         in
         (* NB: no need to update the type, as it is already
            a set of terms (for well-formed terms at least) *)
         { ts with term_node = Tcomprehension(ts,[v],popt) }
      ) ts !ranges
  in
  ranges := [];
  ts
and ranges = ref []


class fromRangeToComprehension behavior prj = object

  inherit Visitor.generic_frama_c_visitor prj behavior as super

  method vterm ts = match ts.term_type with
    | Ltype ({ lt_name = "set"},[_]) ->
      ChangeDoChildrenPost(ts, make_comprehension)
    | _ -> DoChildren

  method vterm_offset tsoff = match tsoff with
    | TIndex ({ term_node =Trange(t1opt,t2opt)} as t,tsoff') ->
        let v = make_temp_logic_var Linteger in
        add_range v t1opt t2opt;
        let vt = variable_term t.term_loc v in
        ChangeDoChildrenPost (TIndex(vt,tsoff'), fun x -> x)
    | TNoOffset | TIndex _ | TField _ -> DoChildren

end

let from_range_to_comprehension behavior prj file =
  let visitor = new fromRangeToComprehension behavior prj in
  Visitor.visitFramacFile visitor file

let range_to_comprehension t =
  let visitor =
    new fromRangeToComprehension (Cil.copy_visit ()) (Project.current ())
  in
  Visitor.visitFramacTerm visitor t


class fromComprehensionToRange behavior prj =
  let ranges = Logic_var.Hashtbl.create 17 in
  let add_range vi t1opt t2opt =
    Logic_var.Hashtbl.add ranges vi (t1opt,t2opt)
  in
  let index_variables_of_term ts =
    let vars = ref Logic_var.Set.empty in
    ignore
      (visitCilTerm
         (object
           inherit nopCilVisitor
           method vterm = function
           | { term_node =
               TBinOp(PlusPI,_ts,{term_node=TLval(TVar v,TNoOffset)})} ->
             vars := Logic_var.Set.add v !vars;
             DoChildren
           | _ -> DoChildren
           method vterm_offset = function
           | TIndex({term_node=TLval(TVar v,TNoOffset)},_tsoff) ->
             vars := Logic_var.Set.add v !vars;
             DoChildren
           | _ -> DoChildren
          end)
        ts);
    !vars
  in
  let bounds_of_variable v popt =
    let error () =
      Kernel.fatal "Cannot identify bounds for variable %a"
        !Ast_printer.d_ident v.lv_name
    in
    let rec bounds p =
      match p.content with
      | Prel(Rle, {term_node = TLval(TVar v',TNoOffset)}, t)
          when Logic_var.equal v v' ->
        None, Some t
      | Prel(Rle, t, {term_node = TLval(TVar v',TNoOffset)})
          when Logic_var.equal v v' ->
        Some t, None
      | Pand(p1,p2) ->
        begin match bounds p1, bounds p2 with
        | (Some t1, None),(None, Some t2) | (None, Some t2),(Some t1, None) ->
          Some t1, Some t2
        | _ -> error ()
        end
      | _ -> error ()
    in
    match popt with None -> None, None | Some p -> bounds p
  in
object(self)

  inherit Visitor.generic_frama_c_visitor prj behavior as super

  val mutable has_set_type = false

  method private propagate_set_type t =
    if has_set_type then
      { t with term_type = Logic_const.make_set_type t.term_type }
    else t

  method vterm t = match t.term_node with
    | Tcomprehension(ts,[v],popt) ->
        let index_vars = index_variables_of_term ts in
        (* Only accept for now comprehension on index variables *)
        if Logic_var.Set.mem v index_vars then begin
          let t1opt,t2opt = bounds_of_variable v popt in
          add_range v t1opt t2opt;
          has_set_type <- false;
          ChangeTo (visitCilTerm (self :> cilVisitor) ts)
        end else begin
          has_set_type <- false;
          DoChildren
        end
    | TBinOp(PlusPI,base,{term_node=TLval(TVar v,TNoOffset)}) ->
          begin try
            let low,high = Logic_var.Hashtbl.find ranges v in
            let range = Logic_const.trange (low,high) in
            let res =
            { t with
                term_node = TBinOp(PlusPI,base,range);
                term_type = Logic_const.make_set_type t.term_type }
            in
            ChangeDoChildrenPost (res, fun x -> has_set_type <- true; x)
          with Not_found -> DoChildren end

    | TBinOp(bop,t1,t2) ->
        has_set_type <- false;
        let t1' = visitCilTerm (self:>Cil.cilVisitor) t1 in
        let has_set_type1 = has_set_type in
        let t2' = visitCilTerm (self:>Cil.cilVisitor) t2 in
        has_set_type <- has_set_type || has_set_type1;
        if t1 != t1' || t2 != t2' || has_set_type then
          ChangeTo
            (self#propagate_set_type { t with term_node = TBinOp(bop,t1',t2')})
        else SkipChildren
    | Tapp(f,prms,args) ->
        has_set_type <- false;
        let visit t =
          let has_set_type1 = has_set_type in
          let res = visitCilTerm (self:>cilVisitor) t in
          has_set_type <- has_set_type || has_set_type1; res
        in
        let args' = mapNoCopy visit args in
        if args != args' || has_set_type then
          ChangeTo
            (self#propagate_set_type { t with term_node = Tapp(f,prms,args') })
        else SkipChildren
     | TDataCons(c,args) ->
        has_set_type <- false;
        let visit t =
          let has_set_type1 = has_set_type in
          let res = visitCilTerm (self:>cilVisitor) t in
          has_set_type <- has_set_type || has_set_type1; res
        in
        let args' = mapNoCopy visit args in
        if args != args' || has_set_type then
          ChangeTo
            (self#propagate_set_type { t with term_node = TDataCons(c,args') })
        else SkipChildren
     | Tif (t1,t2,t3) ->
        has_set_type <- false;
        let t1' = visitCilTerm (self:>Cil.cilVisitor) t1 in
        let has_set_type1 = has_set_type in
        let t2' = visitCilTerm (self:>Cil.cilVisitor) t2 in
        let has_set_type1 = has_set_type || has_set_type1 in
        let t3' = visitCilTerm (self:>Cil.cilVisitor) t3 in
        has_set_type <- has_set_type || has_set_type1;
        if t1 != t1' || t2 != t2' || t3!=t3' || has_set_type then
          ChangeTo
            (self#propagate_set_type { t with term_node = Tif(t1',t2',t3')})
        else SkipChildren
     | TCoerceE(t1,t2) ->
        has_set_type <- false;
        let t1' = visitCilTerm (self:>Cil.cilVisitor) t1 in
        let has_set_type1 = has_set_type in
        let t2' = visitCilTerm (self:>Cil.cilVisitor) t2 in
        has_set_type <- has_set_type || has_set_type1;
        if t1 != t1' || t2 != t2' || has_set_type then
          ChangeTo
            (self#propagate_set_type { t with term_node = TCoerceE(t1',t2')})
        else SkipChildren
     | Tunion l ->
       has_set_type <- false;
        let visit t =
          let has_set_type1 = has_set_type in
          let res = visitCilTerm (self:>cilVisitor) t in
          has_set_type <- has_set_type || has_set_type1; res
        in
        let l' = mapNoCopy visit l in
        if l != l' || has_set_type then
          ChangeTo
             (self#propagate_set_type { t with term_node = Tunion l' })
        else SkipChildren
     | Tinter l ->
       has_set_type <- false;
        let visit t =
          let has_set_type1 = has_set_type in
          let res = visitCilTerm (self:>cilVisitor) t in
          has_set_type <- has_set_type || has_set_type1; res
        in
        let l' = mapNoCopy visit l in
        if l != l' || has_set_type then
          ChangeTo
            (self#propagate_set_type { t with term_node = Tinter l' })
        else SkipChildren
     | Trange(t1,t2) ->
        has_set_type <- false;
        let t1' = optMapNoCopy (visitCilTerm (self:>Cil.cilVisitor)) t1 in
        let has_set_type1 = has_set_type in
        let t2' = optMapNoCopy (visitCilTerm (self:>Cil.cilVisitor)) t2 in
        has_set_type <- has_set_type || has_set_type1;
        if t1 != t1' || t2 != t2' || has_set_type then
          ChangeTo
            (self#propagate_set_type { t with term_node = Trange(t1',t2')})
        else SkipChildren
     | _ ->
         has_set_type <- false;
         ChangeDoChildrenPost (t,self#propagate_set_type)

  method vterm_lval (lh,lo) =
    let lh' = visitCilTermLhost (self:>Cil.cilVisitor) lh in
    let has_set_type1 = has_set_type in
    let lo' = visitCilTermOffset (self :> Cil.cilVisitor) lo in
    has_set_type <- has_set_type || has_set_type1;
    if lh' != lh || lo' != lo then ChangeTo (lh',lo') else SkipChildren

  method vterm_lhost = function
    | TVar v ->
        if Logic_var.Hashtbl.mem ranges v then begin
          Format.eprintf "vterm_lhost: Found: v = %s@." v.lv_name;
          assert false
        end;
        DoChildren
    | _ -> DoChildren

  method vterm_offset off =
    match off with
      | TIndex({term_node=TLval(TVar v,TNoOffset)} as idx,off') ->
          begin try
            let t1opt,t2opt = Logic_var.Hashtbl.find ranges v in
            let trange = Trange(t1opt,t2opt) in
            let toff =
              TIndex
                ({ idx with
                  term_node = trange;
                  term_type = Logic_const.make_set_type idx.term_type },
                 off')
            in
            ChangeDoChildrenPost (toff, fun x -> x)
          with Not_found ->
            DoChildren end
      | TIndex _ | TNoOffset | TField _ ->
          DoChildren

end

let from_comprehension_to_range behavior prj file =
  let visitor = new fromComprehensionToRange behavior prj in
  Visitor.visitFramacFile visitor file

(* Expect conversion to be possible on all sub-terms, otherwise raise an error. *)

let logic_var_to_var { lv_origin = lv } =
  match lv with
    | None -> error_lval ()
    | Some lv -> lv

let create_const_list loc kind low high =
  let rec aux acc i =
    if My_bigint.lt i low then acc
    else
      aux (new_exp ~loc (Const (CInt64 (i,kind,None)))::acc) (My_bigint.pred i)
  in aux [] high

let range low high =
  let loc = fst low.eloc, snd high.eloc in
  match (Cil.constFold true low).enode, (Cil.constFold true high).enode with
      Const(CInt64(low,kind,_)), Const(CInt64(high,_,_)) ->
        create_const_list loc kind low high
    | _ -> error_lval()

let singleton f loc =
  match f loc with
      [ l ] -> l
    | _ -> error_lval()

let rec loc_lval_to_lval ~result (lh, lo) =
  Extlib.product
    (fun x y -> (x,y))
    (loc_lhost_to_lhost ~result lh)
    (loc_offset_to_offset ~result lo)

and loc_lhost_to_lhost ~result = function
  | TVar lvar -> [Var (logic_var_to_var lvar)]
  | TMem lterm -> List.map (fun x -> Mem x) (loc_to_exp ~result lterm)
  | TResult _ ->
      ( match result with
        None -> error_lval()
      | Some v -> [Var v])

and loc_offset_to_offset ~result = function
  | TNoOffset -> [NoOffset]
  | TField (fi, lo) ->
      List.map (fun x -> Field (fi,x)) (loc_offset_to_offset ~result lo)
  | TIndex (lexp, lo) ->
      Extlib.product
	(fun x y -> Index(x,y))
        (loc_to_exp ~result lexp) 
	(loc_offset_to_offset ~result lo)

and loc_to_exp ~result {term_node = lnode ; term_type = ltype; term_loc = loc} =
  match lnode with
  | TLval lv ->
      List.map (fun x -> new_exp ~loc (Lval x)) (loc_lval_to_lval ~result lv)
  | TAddrOf lv ->
      List.map (fun x -> new_exp ~loc (AddrOf x)) (loc_lval_to_lval ~result lv)
  | TStartOf lv ->
      List.map (fun x -> new_exp ~loc (StartOf x)) (loc_lval_to_lval ~result lv)
  | TSizeOfE lexp ->
      List.map (fun x -> new_exp ~loc (SizeOfE x)) (loc_to_exp ~result lexp)
  | TAlignOfE lexp ->
      List.map (fun x -> new_exp ~loc (AlignOfE x)) (loc_to_exp ~result lexp)
  | TUnOp (unop, lexp) ->
      List.map
        (fun x -> new_exp ~loc (UnOp (unop, x, logic_type_to_typ ltype)))
        (loc_to_exp ~result lexp)
  | TBinOp (binop, lexp1, lexp2) ->
      Extlib.product
        (fun x y -> new_exp ~loc (BinOp (binop, x,y, logic_type_to_typ ltype)))
        (loc_to_exp ~result lexp1) 
	(loc_to_exp ~result lexp2)
  | TSizeOfStr string -> [new_exp ~loc (SizeOfStr string)]
  | TConst constant -> [new_exp ~loc (Const constant)]
  | TCastE (typ, lexp) ->
      List.map
        (fun x -> new_exp ~loc (CastE (typ, x))) (loc_to_exp ~result lexp)
  | TAlignOf typ -> [new_exp ~loc (AlignOf typ)]
  | TSizeOf typ -> [new_exp ~loc (SizeOf typ)]
  | Trange (Some low, Some high) ->
      let low = singleton (loc_to_exp ~result) low in
      let high = singleton (loc_to_exp ~result) high in
      range low high
  | Tunion l -> List.concat (List.map (loc_to_exp ~result) l)
  | Tempty_set -> []
  | Tinter _ | Tcomprehension _ -> error_lval()
  | Tat ({term_node = TAddrOf (TVar _, TNoOffset)} as taddroflval, _) ->
      loc_to_exp ~result taddroflval

 (* additional constructs *)
  | Tapp _ | Tlambda _ | Trange _   | Tlet _
  | TDataCons _
  | Tif _
  | Tat _
  | Tbase_addr _
  | Tblock_length _
  | Tnull
  | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _
    -> error_lval ()

let rec loc_to_lval ~result t =
  match t.term_node with
  | TLval lv -> loc_lval_to_lval ~result lv
  | TAddrOf lv -> loc_lval_to_lval ~result lv
  | TStartOf lv -> loc_lval_to_lval ~result lv
  | Tunion l1 -> List.concat (List.map (loc_to_lval ~result) l1)
  | Tempty_set -> []
  | Tinter _ -> error_lval() (* TODO *)
  | Tcomprehension _ -> error_lval()
  | TSizeOfE _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TSizeOfStr _
  | TConst _ | TCastE _ | TAlignOf _ | TSizeOf _ | Tapp _ | Tif _
  | Tat _ | Tbase_addr _ | Tblock_length _ | Tnull | Trange _
  | TCoerce _ | TCoerceE _ | TDataCons _ | TUpdate _ | Tlambda _
  | Ttypeof _ | Ttype _ | Tlet _ ->
      error_lval ()

(* This function fails with "not an lvalue" much too often, and is
   replaced by another function in value/eval_logic.ml. It is left
   here in case someones decides to deactivate the value analysis *)
let loc_to_loc ~result state content =
  let unprotected content =
    let lvals = loc_to_lval ~result content in
      List.fold_left
        (fun acc lval ->
           let loc = !Db.Value.lval_to_loc_state state lval in
           let s = loc.Locations.size in
             assert (Locations.loc_equal acc Locations.loc_bottom ||
                       Int_Base.equal s acc.Locations.size);
             Locations.make_loc
               (Locations.Location_Bits.join
                  loc.Locations.loc
                  acc.Locations.loc)
               s)
        Locations.loc_bottom
        lvals
  in
  begin try
    unprotected content
  with
      Invalid_argument "not an lvalue" as e ->
        let t = content.term_node in
          begin match t with
            | TLval (TVar v, _o) ->
                let c_v = logic_var_to_var v in
                let base = Base.find c_v in
                let loc =
                  Locations.Location_Bits.inject_top_origin
                    Origin.top
                    (Locations.Location_Bits.Top_Param.O.singleton base)

                in
                  Locations.make_loc loc Int_Base.top
            | TLval (TMem {term_node =
                TBinOp((IndexPI|PlusPI),
                       ({ term_node = TCastE (_, t1) } | t1),_o1)},
                     _o2) ->
                let deref_lvals =
                  !Db.Properties.Interp.loc_to_lval ~result:None t1
                in
                  (* Format.printf "input: %a@."
                     Cvalue.V.pretty input_contents ; *)
                let deref_loc =
                  List.fold_left
                    (fun acc lv ->
                       let loc =
                         !Db.Value.lval_to_loc_state state lv
                       in
                         Locations.Location_Bits.join loc.Locations.loc acc)
                    Locations.Location_Bits.bottom
                    deref_lvals
                in
                let deref_loc =
                  Locations.Location_Bits.topify_arith_origin deref_loc
                in
                let loc_bytes =
                  Cvalue.Model.find
                    ~conflate_bottom:true
                    ~with_alarms:CilE.warn_none_mode
                    state
                    (Locations.make_loc deref_loc Int_Base.top)
                in
                let loc =
                  Locations.make_loc
                    (Locations.loc_bytes_to_loc_bits loc_bytes)
                    Int_Base.top
                in
                  loc
            | _ -> raise e
          end
  end

let identified_term_zone_to_loc ~result state t =
  !Db.Properties.Interp.loc_to_loc ~result state t.it_content

let rec loc_to_offset ~result loc =
  let rec aux h =
    function
        TLval(h',o) | TStartOf (h',o) ->
          (match h with None -> Some h', loc_offset_to_offset ~result o
             | Some h when Logic_utils.is_same_lhost h h' ->
                 Some h, loc_offset_to_offset ~result o
             | Some _ -> error_lval()
          )
      | Tat ({ term_node = TLval(TResult _,_)} as lv,LogicLabel (_,"Post")) ->
          aux h lv.term_node
      | Tunion locs -> List.fold_left
            (fun (b,l) x ->
               let (b,l') = aux b x.term_node in b, l @ l') (h,[]) locs
      | Tempty_set -> h,[]
      | Trange _ | TAddrOf _
      | TSizeOfE _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TSizeOfStr _
      | TConst _ | TCastE _ | TAlignOf _ | TSizeOf _ | Tapp _ | Tif _
      | Tat _ | Tbase_addr _ | Tblock_length _ | Tnull
      | TCoerce _ | TCoerceE _ | TDataCons _ | TUpdate _ | Tlambda _
      | Ttypeof _ | Ttype _ | Tcomprehension _ | Tinter _ | Tlet _
          -> error_lval ()
  in snd (aux None loc.term_node)

let term_lval_to_lval ~result = singleton (loc_lval_to_lval ~result)

let term_to_lval ~result = singleton (loc_to_lval ~result)

let term_to_exp ~result = singleton (loc_to_exp ~result)

let term_offset_to_offset ~result = singleton (loc_offset_to_offset ~result)

class make_pre_var formals =
object(self)
  inherit Visitor.frama_c_copy (Project.current ())

  val is_under_pre = Stack.create ()

  method vterm t =
    let loc = t.term_loc in
    match t.term_node with
        TLval(TVar v,_)
          when List.exists (Logic_utils.is_same_var v) formals ->
            Stack.push true is_under_pre;
            ChangeDoChildrenPost
              (t,
               fun x -> ignore (Stack.pop is_under_pre);
                 Logic_const.tat ~loc  (x, Logic_const.old_label))
      | Tat _ -> JustCopy
          (* User is supposed to know what she's doing
             with logic labels. Don't mess with her.
           *)
      | _ when Stack.top is_under_pre ->
          Stack.push false is_under_pre;
          ChangeDoChildrenPost
            (t,
             fun t -> ignore (Stack.pop is_under_pre);
               Logic_const.tat ~loc (t, Logic_const.here_label))
      | _ -> DoChildren

  method vspec s =
    s.spec_behavior <-
      Cil.visitCilBehaviors (self:>Cil.cilVisitor) s.spec_behavior;
    JustCopy

  method vbehavior b =
    b.b_post_cond <-
      List.map
      (fun (k,p) -> (k,Cil.visitCilIdPredicate (self:>Cil.cilVisitor) p))
         b.b_post_cond;
    JustCopy

  initializer Stack.push false is_under_pre
end

let formals_in_ensures kf =
  let formals = Globals.Functions.get_params kf in
  let logic_formals =
    List.fold_left
      (fun l x -> match x.vlogic_var_assoc with
           None -> l
         | Some x -> x::l) [] formals
  in
  let spec = kf.spec in
  Visitor.visitFramacFunspec (new make_pre_var logic_formals) spec

(** Utilities to identify [Locations.Zone.t] involved into
    [rooted_code_annotation]. *)
module To_zone : sig

  type t_ctx = Db.Properties.Interp.To_zone.t_ctx

  val mk_ctx_func_contrat: kernel_function -> state_opt:bool option -> t_ctx
  (** [mk_ctx_func_contrat] to define an interpretation context related to
      [kernel_function] contracts. 
      The control point of the interpretation is defined as follow:
      - pre-state if  [state_opt=Some true]
      - post-state if [state_opt=Some false]
      - pre-state with possible reference to the post-state if
      [state_opt=None]. *)

  val mk_ctx_stmt_contrat: 
    kernel_function -> stmt -> state_opt:bool option -> t_ctx
  (** [mk_ctx_stmt_contrat] to define an interpretation context related to
      [stmt] contracts. 
      The control point of the interpretation is defined as follow:
      - pre-state if  [state_opt=Some true]
      - post-state if [state_opt=Some false]
      - pre-state with possible reference to the post-state if
      [state_opt=None]. *)

  val mk_ctx_stmt_annot: kernel_function -> stmt -> t_ctx
  (** [mk_ctx_stmt_annot] to define an interpretation context related to an
      annotation attached before the [stmt]. *)

  type t = Db.Properties.Interp.To_zone.t
  type t_zone_info = Db.Properties.Interp.To_zone.t_zone_info
  type t_decl = Varinfo.Set.t
  type t_pragmas = Db.Properties.Interp.To_zone.t_pragmas
  val not_yet_implemented : string ref

  val from_term: term -> t_ctx -> (t_zone_info * t_decl)
    (** Entry point to get zones
        needed to evaluate the [term] relative to the [ctx] of
	interpretation. *) 

  val from_terms: term list -> t_ctx -> (t_zone_info * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [terms] relative to the [ctx] of
	interpretation. *) 

  val from_pred: predicate named -> t_ctx -> (t_zone_info * t_decl)
    (** Entry point to get zones
        needed to evaluate the [predicate] relative to the [ctx] of
	interpretation. *) 

  val from_preds: predicate named list -> t_ctx -> (t_zone_info * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [predicates] relative to the [ctx] of
	interpretation. *) 

  val from_zone: identified_term -> t_ctx -> (t_zone_info * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [predicates] relative to the [ctx] of
	interpretation. *) 

  val from_zones: identified_term list -> t_ctx -> (t_zone_info * t_decl)
    (** Entry point to get zones
        needed to evaluate the list of [predicates] relative to the [ctx] of
	interpretation. *)

  val from_stmt_annot: 
    code_annotation -> (stmt * kernel_function) -> 
    (t_zone_info * t_decl) * t_pragmas
  (** Entry point to get zones needed to evaluate code annotations of this
      [stmt]. *)

  val from_stmt_annots:
    (rooted_code_annotation -> bool) option ->
    (stmt * kernel_function) -> (t_zone_info * t_decl) * t_pragmas
  (** Entry point to get zones needed to evaluate code annotations of this
      [stmt]. *) 

  val from_func_annots:
    ((stmt -> unit) -> kernel_function -> unit) ->
    (rooted_code_annotation  -> bool) option ->
    kernel_function -> (t_zone_info * t_decl) * t_pragmas
  (** Entry point to get zones needed to evaluate code annotations of this
      [kf]. *) 

  val code_annot_filter:
    rooted_code_annotation -> ai:bool ->
    user_assert:bool -> slicing_pragma:bool ->
    loop_inv:bool -> loop_var:bool -> others:bool -> bool
    (** To quickly build a annotation filter *)
  end
  = 
struct

  exception NotYetImplemented of string
  type t_ctx = Db.Properties.Interp.To_zone.t_ctx
  
  let mk_ctx_func_contrat kf ~state_opt =
    { Db.Properties.Interp.To_zone.state_opt = state_opt;
      ki_opt = None;
      kf = kf }

  let mk_ctx_stmt_contrat kf ki ~state_opt =
    { Db.Properties.Interp.To_zone.state_opt=state_opt;
      ki_opt= Some(ki, false);
      kf = kf }

  let mk_ctx_stmt_annot kf ki =
    { Db.Properties.Interp.To_zone.state_opt = Some true;
      ki_opt = Some(ki, true);
      kf = kf }

  type t = Db.Properties.Interp.To_zone.t
  type t_zone_info = Db.Properties.Interp.To_zone.t_zone_info
  type t_decl = Varinfo.Set.t
  type t_pragmas = Db.Properties.Interp.To_zone.t_pragmas

  let empty_pragmas =
    { Db.Properties.Interp.To_zone.ctrl = Stmt.Set.empty;
      stmt = Stmt.Set.empty }

  let other_zones = Stmt.Hashtbl.create 7
  let locals = ref Varinfo.Set.empty
  let pragmas = ref empty_pragmas

  let zone_result = ref (Some other_zones)
  let not_yet_implemented = ref ""

  let add_top_zone not_yet_implemented_msg = match !zone_result with
    | None -> (* top zone *) ()
    | Some other_zones ->
      Stmt.Hashtbl.clear other_zones;
      not_yet_implemented := not_yet_implemented_msg;
      zone_result := None

  let add_result ~before ki zone = match !zone_result with
    | None -> (* top zone *) ()
    | Some other_zones ->
      let zone_true, zone_false =
        try Stmt.Hashtbl.find other_zones ki
        with Not_found -> Locations.Zone.bottom, Locations.Zone.bottom
      in
      Stmt.Hashtbl.replace other_zones
        ki
        (if before then Locations.Zone.join zone_true zone, zone_false
         else zone_true, Locations.Zone.join zone_false zone)

  let get_result () =
    let result =
      let zones = match !zone_result with
        | None ->
          (* clear references for the next time when giving the result.
             Note that other_zones has been cleared in [add_top_zone]. *)
          zone_result := Some other_zones;
          None
        | Some other_zones ->
          let z =
            Stmt.Hashtbl.fold
              (fun ki (zone_true, zone_false) other_zones ->
                let add before zone others =
                  if Locations.Zone.equal Locations.Zone.bottom zone then
                    others
                  else
                    { Db.Properties.Interp.To_zone.before = before;
                      ki = ki;
                      zone = zone} :: others
                in
                add true zone_true (add false zone_false other_zones))
              other_zones
              []
          in
          (* clear table for the next time when giving the result *)
          Stmt.Hashtbl.clear other_zones;
          Some z
      in zones, !locals
    in
    (* clear references for the next time when giving the result *)
    locals := Varinfo.Set.empty ;
    result

  let get_annot_result () =
    let annot_result = (get_result ()), !pragmas in
    (* clear references for the next time when giving the result *)
    pragmas := empty_pragmas ;
    annot_result

  (** Logic_var utility: *)
  let extract_locals logicvars =
    Logic_var.Set.fold
      (fun lv cvars -> match lv.lv_origin with
      | None -> cvars
      | Some cvar ->
        if cvar.Cil_types.vglob then cvars
        else Varinfo.Set.add cvar cvars)
      logicvars
      Varinfo.Set.empty

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
       - pre-state if  [before_opt=Some true]
       - post-state if [before_opt=Some false]
       - pre-state with possible reference to the post-state if
       [before_opt=None] of a property relative to
       - the contract of function [kf] when [ki_opt=None]
       otherwise [ki_opt=Some(ki, code_annot)],
       - the contract of the statement [ki] when [code_annot=false]
       - the annotation of the statement [ki] when [code_annot=true] *)
    object(self)
      inherit
        Visitor.generic_frama_c_visitor  (Project.current())
          (Cil.inplace_visit())
      val mutable current_label = AbsLabel_here

      method private get_ctrl_point () =
        let get_fct_entry_point () =
          (* TODO: to replace by true, None *)
          true, 
	  (try Some (Kernel_function.find_first_stmt kf)
           with Kernel_function.No_Statement -> 
	     (* raised when [kf] has no code. *)
	     None)  
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
        | current_before, Some current_stmt -> current_before, current_stmt
        | _ -> raise (NotYetImplemented
                        "[logic_interp] clause related to a function contract")

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
          | None,Some true -> 
	    failwith "The use of the label Old is forbiden inside clauses \
        related the pre-state of function contracts." 
          | None,None
          | None,Some false -> 
	    (* refers to the pre-state of the contract. *)
	    self#change_label AbsLabel_pre x 
          (* statement contract *)
          | Some (_ki,false),Some true  -> 
	    failwith "The use of the label Old is forbiden inside clauses \
related the pre-state of statement contracts."
          | Some (ki,false),None
          | Some (ki,false),Some false  -> 
	    (* refers to the pre-state of the contract. *)
	    self#change_label (AbsLabel_stmt ki) x 
          (* code annotation *)
          | Some (_ki,true),None
          | Some (_ki,true),Some _ -> 
	    (* refers to the pre-state of the function contract. *)
	    self#change_label AbsLabel_pre x 

      method private change_label_to_post: 'a.'a -> 'a visitAction =
        fun x -> 
	  (* allowed when [before_opt=None] for function/statement contracts *)
          match ki_opt,before_opt with
            (* function contract *)
          | None,Some _ -> 
	    failwith "Function contract where the use of the label Post is \
 forbiden."
          | None,None -> 
	    (* refers to the post-state of the contract. *)
	    self#change_label AbsLabel_post x 
          (* statement contract *)
          | Some (_ki,false),Some _  -> 
	    failwith "Statement contract where the use of the label Post is \
forbiden."
          | Some (_ki,false),None -> 
	    (* refers to the pre-state of the contract. *)
	    self#change_label AbsLabel_post x 
          (* code annotation *)
          | Some (_ki,true), _ -> 
	    failwith "The use of the label Post is forbiden inside code \
annotations."

      method private change_label_to_pre: 'a.'a -> 'a visitAction =
        fun x ->
          match ki_opt with
            (* function contract *)
          | None -> 
	    failwith "The use of the label Pre is forbiden inside function \
contracts."
          (* statement contract *)
          (* code annotation *)
          | Some _ -> 
	    (* refers to the pre-state of the function contract. *)
	    self#change_label AbsLabel_pre x 

      method private change_label_to_stmt: 'a.stmt -> 'a -> 'a visitAction =
        fun stmt x ->
          match ki_opt with
            (* function contract *)
          | None -> 
	    failwith "the use of C labels is forbiden inside clauses related \
function contracts."
          (* statement contract *)
          (* code annotation *)
          | Some _ -> 
	    (* refers to the state at the C label of the statement [stmt]. *)
	    self#change_label (AbsLabel_stmt stmt) x

      method vpredicate p = match p with
      | Pat (_, LogicLabel (_,"Old")) -> self#change_label_to_old p
      | Pat (_, LogicLabel (_,"Here")) -> self#change_label_to_here p
      | Pat (_, LogicLabel (_,"Pre")) -> self#change_label_to_pre p
      | Pat (_, LogicLabel (_,"Post")) -> self#change_label_to_post p
      | Pat (_, StmtLabel st) -> self#change_label_to_stmt !st p
      | Pat (_, LogicLabel (_,s)) ->
          failwith ("unknown logic label" ^ s)
      | Pfresh _ ->
        raise (NotYetImplemented "[logic_interp] \\fresh()")
      (* assert false *) (*VP: can't we do something better? *)
      | _ -> DoChildren

      method vterm t =
        match t.term_node with
          | TAddrOf _ | TLval (TMem _,_)
          | TLval(TVar {lv_origin = Some _},_) | TStartOf _  ->
            let exp = try (* to be removed *)
              !Db.Properties.Interp.term_to_exp ~result:None t
            with Invalid_argument str ->
              raise (NotYetImplemented ("[logic_interp] " ^ str))
            in
            let current_before, current_ki = self#get_ctrl_point () in
            let loc = try (* to be removed *)
              !Db.From.find_deps_no_transitivity current_ki exp
            with Invalid_argument str ->
              raise (NotYetImplemented ("[logic_interp] " ^ str))
            in add_result current_before current_ki loc; SkipChildren
          | Tat (_, LogicLabel (_,"Old")) -> self#change_label_to_old t
          | Tat (_, LogicLabel (_,"Here")) -> self#change_label_to_here t
          | Tat (_, LogicLabel (_,"Pre")) -> self#change_label_to_pre t
          | Tat (_, LogicLabel (_,"Post")) -> self#change_label_to_post t
          | Tat (_, StmtLabel st) -> self#change_label_to_stmt !st t
          | Tat (_, LogicLabel (_,s)) ->
            failwith ("unknown logic label" ^ s)
          | _ -> DoChildren
    end

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the [term]
        relative to the [ctx] of interpretation. *)
    let from_term term ctx =
      (* [VP 2011-01-28] TODO: factorize from_terms and from_term, and use
	 a more functional setting. *)
      (try
         ignore(Visitor.visitFramacTerm
		  (new populate_zone
                     ctx.Db.Properties.Interp.To_zone.state_opt
                     ctx.Db.Properties.Interp.To_zone.ki_opt
                     ctx.Db.Properties.Interp.To_zone.kf) term)
       with NotYetImplemented msg -> 
	 add_top_zone msg) ;
      locals := Varinfo.Set.union (extract_locals_from_term term) !locals;
      get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [terms]
        relative to the [ctx] of interpretation. *)
    let from_terms terms ctx =
      let f x =
        (try
           ignore(Visitor.visitFramacTerm
		    (new populate_zone
                       ctx.Db.Properties.Interp.To_zone.state_opt
                       ctx.Db.Properties.Interp.To_zone.ki_opt
                       ctx.Db.Properties.Interp.To_zone.kf) x)
         with NotYetImplemented msg -> 
	   add_top_zone msg) ;
        locals := Varinfo.Set.union (extract_locals_from_term x) !locals
      in
        List.iter f terms;
        get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the [pred]
        relative to the [ctx] of interpretation. *)
    let from_pred pred ctx =
        (try
           ignore(Visitor.visitFramacPredicateNamed
                    (new populate_zone
                       ctx.Db.Properties.Interp.To_zone.state_opt
                       ctx.Db.Properties.Interp.To_zone.ki_opt
                       ctx.Db.Properties.Interp.To_zone.kf) pred)
         with NotYetImplemented msg -> 
	   add_top_zone msg) ;
      locals := Varinfo.Set.union (extract_locals_from_pred pred) !locals;
      get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [preds]
        relative to the [ctx] of interpretation. *)
    let from_preds preds ctx =
      let f pred =
        (try
           ignore(Visitor.visitFramacPredicateNamed
                    (new populate_zone 
		       ctx.Db.Properties.Interp.To_zone.state_opt
                       ctx.Db.Properties.Interp.To_zone.ki_opt
                       ctx.Db.Properties.Interp.To_zone.kf) pred)
         with NotYetImplemented msg -> 
	   add_top_zone msg) ;
        locals := Varinfo.Set.union (extract_locals_from_pred pred) !locals
      in
        List.iter f preds;
        get_result ()

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [zones]
        relative to the [ctx] of interpretation. *)
    let from_zone zone ctx = from_term zone.it_content ctx

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the list of [zones]
        relative to the [ctx] of interpretation. *)
    let from_zones zones ctx =
      from_terms (List.map (fun x -> x.it_content) zones) ctx

   (** Used by annotations entry points. *)
    let get_zone_from_annot a (ki,kf) loop_body_opt =
      assert (!pragmas = empty_pragmas);
      (* check before modification. Anne.*)
      let get_zone_from_term k x =
        (try
           ignore
             (Visitor.visitFramacTerm
                (new populate_zone (Some true) (Some (k, true)) kf) x)
         with NotYetImplemented msg -> 
	   add_top_zone msg) ;
        (* to select the declaration of the variables *)
        locals := Varinfo.Set.union (extract_locals_from_term x) !locals
      and get_zone_from_pred k x =
        (try
           ignore
             (Visitor.visitFramacPredicateNamed
                (new populate_zone (Some true) (Some (k,true)) kf) x)
         with NotYetImplemented msg -> 
	   add_top_zone msg) ;
        (* to select the declaration of the variables *)
        locals := Varinfo.Set.union (extract_locals_from_pred x) !locals
      in
      match a with
      | APragma (Slice_pragma (SPexpr term) | Impact_pragma (IPexpr term)) ->
        (* to preserve the interpretation of the pragma *)
        get_zone_from_term ki term;
        (* to select the reachability of the pragma *)
        pragmas :=
          { !pragmas with Db.Properties.Interp.To_zone.ctrl =
              Stmt.Set.add ki !pragmas.Db.Properties.Interp.To_zone.ctrl }
      | APragma (Slice_pragma SPctrl) ->
        (* to select the reachability of the pragma *)
        pragmas :=
          { !pragmas with
            Db.Properties.Interp.To_zone.ctrl =
              Stmt.Set.add ki !pragmas.Db.Properties.Interp.To_zone.ctrl }
      | APragma (Slice_pragma SPstmt | Impact_pragma IPstmt) ->
        (* to preserve the effect of the statement *)
        pragmas :=
          { !pragmas with
            Db.Properties.Interp.To_zone.stmt =
              Stmt.Set.add ki !pragmas.Db.Properties.Interp.To_zone.stmt}
      | AAssert (_behav,pred) ->
        (* to preserve the interpretation of the assertion *)
        get_zone_from_pred ki pred;
      | AInvariant (_behav,true,pred) -> (* loop invariant *)
        (* WARNING this is obsolete *)
        (* [JS 2010/09/02] TODO: so what is the right way to do? *)
        (* to preserve the interpretation of the loop invariant *)
        get_zone_from_pred (Extlib.the loop_body_opt) pred;
      | AInvariant (_behav,false,pred) -> (* code invariant *)
        (* to preserve the interpretation of the code invariant *)
        get_zone_from_pred ki pred;
      | AVariant (term,_) ->
        (* to preserve the interpretation of the variant *)
        get_zone_from_term (Extlib.the loop_body_opt) term;
      | APragma (Loop_pragma (Unroll_level term)) ->
        (* to select the declaration of the variables *)
        locals := Varinfo.Set.union (extract_locals_from_term term) !locals
      | APragma (Loop_pragma (Widen_hints terms))
      | APragma (Loop_pragma (Widen_variables terms)) ->
        (* to select the declaration of the variables *)
        List.iter
          (fun term ->
            locals := Varinfo.Set.union (extract_locals_from_term term) !locals)
          terms
      | AAssigns (_, WritesAny) -> ()
      | AAssigns (_, Writes l) -> (* loop assigns *)
        let get_zone x =
          get_zone_from_term (Extlib.the loop_body_opt) x.it_content
        in
        List.iter
          (fun (zone,deps) ->
            get_zone zone;
            match deps with
                FromAny -> ()
              | From l -> List.iter get_zone l)
          l
      | AStmtSpec _ -> (* TODO *)
        raise (NotYetImplemented "[logic_interp] statement contract")

   (** Used by annotations entry points. *)
    let get_zone_from_annotation a stmt loop_body_opt =
      match a with
      | User a
      | AI (_,a) -> get_zone_from_annot a.annot_content stmt loop_body_opt

    (** Used by annotations entry points. *)
    let get_from_stmt_annots code_annot_filter ((ki, _kf) as stmt) =
      Extlib.may
        (fun caf ->
           let loop_body_opt = match ki.skind with
             | Loop(_, { bstmts = body :: _ }, _, _, _) -> Some body
             | _ -> None
           in
           Annotations.single_iter_stmt
             (fun a ->
                if caf a then get_zone_from_annotation a stmt loop_body_opt)
             ki)
        code_annot_filter

    (** Used by annotations entry points. *)
    let from_ki_annot annot ((ki, _kf) as stmt) =
      let real_ki = match ki.skind with
          Loop(_,{bstmts = loop_entry::_},_,_,_) -> Some loop_entry
        | _ -> None
      in
      get_zone_from_annot annot.annot_content stmt real_ki

    (** Entry point to get the list of [ki] * [Locations.Zone.t]
        needed to evaluate the code annotations related to this [stmt]. *)
    let from_stmt_annot annot stmt =
      from_ki_annot annot stmt;
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
      let code_annot_filter a =
        match a with
        | APragma (Slice_pragma _) -> slicing_pragma
        | AAssert _ -> user_assert
        | AVariant _ -> loop_var
        | AInvariant(_behav,true,_pred) -> loop_inv
        | AInvariant(_,false,_) -> others
        | AAssigns _ -> others
          (*
            | ALoopBehavior(_behav,_invs,_assigns) ->
            loop_inv || others (* CORRECT ???? *)
          *)
        | APragma (Loop_pragma _)| APragma (Impact_pragma _) -> others
        | AStmtSpec _  (* TODO: statement contract *) -> false
      in match annot with
      | User a -> code_annot_filter a.annot_content
      | AI _ -> ai
  end

exception Prune

let to_result_from_pred p =
  let visitor = object (_self)
    inherit
      Visitor.generic_frama_c_visitor (Project.current())
        (Cil.inplace_visit())

      method vterm_lhost t =
        match t with
          | TResult _ -> raise Prune
          | _ -> DoChildren

  end
  in
  (try
     ignore(Visitor.visitFramacPredicateNamed visitor p);
     false
   with Prune -> 
     true)


let () =
  Db.Properties.Interp.code_annot := code_annot;
  Db.Properties.Interp.lval := lval;
  Db.Properties.Interp.expr := expr;
  Db.Properties.Interp.term_lval_to_lval := term_lval_to_lval;
  Db.Properties.Interp.term_to_exp := term_to_exp;

  Db.Properties.Interp.force_term_to_exp := force_term_to_exp;
  Db.Properties.Interp.force_back_exp_to_term := force_back_exp_to_term;
  Db.Properties.Interp.force_term_lval_to_lval := force_term_lval_to_lval;
  Db.Properties.Interp.force_back_lval_to_term_lval := 
    force_back_lval_to_term_lval;
  Db.Properties.Interp.force_term_offset_to_offset := 
    force_term_offset_to_offset;
  Db.Properties.Interp.force_back_offset_to_term_offset := 
    force_back_offset_to_term_offset;

  Db.Properties.Interp.force_exp_to_term := force_exp_to_term;
  Db.Properties.Interp.force_lval_to_term_lval := force_lval_to_term_lval;
  Db.Properties.Interp.force_exp_to_predicate := force_exp_to_predicate;
  Db.Properties.Interp.force_exp_to_assertion := force_exp_to_assertion;

  Db.Properties.Interp.from_range_to_comprehension := 
    from_range_to_comprehension;
  Db.Properties.Interp.from_comprehension_to_range := 
    from_comprehension_to_range;

  Db.Properties.Interp.term_to_lval := term_to_lval;
  Db.Properties.Interp.range_to_comprehension := range_to_comprehension;
  Db.Properties.Interp.term_offset_to_offset := term_offset_to_offset;

  Db.Properties.Interp.loc_to_lval := loc_to_lval;
  Db.Properties.Interp.loc_to_offset := loc_to_offset;
  Db.Properties.Interp.loc_to_exp := loc_to_exp;
  Db.Properties.Interp.loc_to_loc := loc_to_loc;
  Db.Properties.Interp.identified_term_zone_to_loc := 
    identified_term_zone_to_loc;

  Db.Properties.Interp.To_zone.code_annot_filter := To_zone.code_annot_filter;
  Db.Properties.Interp.To_zone.mk_ctx_func_contrat := 
    To_zone.mk_ctx_func_contrat;
  Db.Properties.Interp.To_zone.mk_ctx_stmt_contrat := 
    To_zone.mk_ctx_stmt_contrat;
  Db.Properties.Interp.To_zone.mk_ctx_stmt_annot := To_zone.mk_ctx_stmt_annot;

  Db.Properties.Interp.To_zone.from_term := To_zone.from_term;
  Db.Properties.Interp.To_zone.from_terms := To_zone.from_terms;
  Db.Properties.Interp.To_zone.from_pred := To_zone.from_pred;
  Db.Properties.Interp.To_zone.from_preds := To_zone.from_preds;
  Db.Properties.Interp.To_zone.from_stmt_annot := To_zone.from_stmt_annot;
  Db.Properties.Interp.To_zone.from_stmt_annots := To_zone.from_stmt_annots;
  Db.Properties.Interp.To_zone.from_func_annots := To_zone.from_func_annots;

  Db.Properties.Interp.to_result_from_pred := to_result_from_pred;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
