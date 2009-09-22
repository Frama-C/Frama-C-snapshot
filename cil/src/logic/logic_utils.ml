(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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
open Logic_const
open Cil_types


(** basic utilities for logic terms and predicates. See also {! Logic_const}
    to build terms and predicates.
    @plugin development guide
*)

(** {1 From C to logic}*)

let isLogicType f t =
  plain_or_set (function Ctype t -> f t | _ -> false) t

(** true if the type is a C array (or a set of)*)
let isLogicArrayType = isLogicType Cil.isArrayType

let isLogicCharType = isLogicType Cil.isCharType

let isLogicVoidType = isLogicType Cil.isVoidType

let isLogicPointerType = isLogicType Cil.isPointerType

(** returns the equivalent C type.
 @raise Failure if the type is purely logical
*)
let logicCType =
  plain_or_set (function Ctype t -> t
                  | Lvar _ -> Cil.intType
                  | _ -> failwith "not a C type")


(** @plugin development guide *)
let mk_dummy_term e ctyp = Logic_const.term e (Ctype ctyp)

let insert_logic_cast typ term =
  let made_term =  mk_dummy_term term typ in
  TCastE(typ, made_term)

(** @plugin development guide *)
let rec expr_to_term e =
  let result = match e.enode with
  | Const c ->
      let tc = TConst c in
 (*     if true  Cil.isIntegralType e_typ
    then *)tc
(* else insert_logic_cast e_typ tc*)
  | SizeOf t -> TSizeOf t
  | SizeOfE e -> TSizeOfE (expr_to_term e)
  | SizeOfStr s -> TSizeOfStr s
  | StartOf lv -> TStartOf (lval_to_term_lval lv)
  | AddrOf lv -> TAddrOf (lval_to_term_lval lv)
  | CastE (ty,e) ->  TCastE (ty,expr_to_term e)
  | BinOp (op, l, r, _) -> TBinOp (op,expr_to_term l,expr_to_term r)
  | UnOp (op, e, _) -> TUnOp (op,expr_to_term e)
  | AlignOfE e -> TAlignOfE (expr_to_term e)
  | AlignOf typ -> TAlignOf typ
  | Lval lv -> TLval (lval_to_term_lval lv)
  | Info (e,_) -> (expr_to_term e).term_node
  in
  let e_typ = Cil.typeOf e in
  mk_dummy_term result e_typ

(** @plugin development guide *)
and lval_to_term_lval (host,offset) =
  host_to_term_host host,offset_to_term_offset offset
and host_to_term_host = function
  | Var s -> TVar (Cil.cvar_to_lvar s)
  | Mem e ->
      TMem (expr_to_term e)
and offset_to_term_offset = function
  | NoOffset -> TNoOffset
  | Index (e,off) -> TIndex (expr_to_term e,offset_to_term_offset off)
  | Field (fi,off) -> TField(fi,offset_to_term_offset off)

(** {1 Various utilities} *)

let rec remove_term_offset o =
  match o with
      TNoOffset -> TNoOffset, TNoOffset
    | TIndex(_,TNoOffset) | TField(_,TNoOffset)-> TNoOffset, o
    | TIndex(e,o) ->
        let (oth,last) = remove_term_offset o in TIndex(e,oth), last
    | TField(f,o) ->
        let (oth,last) = remove_term_offset o in TField(f,oth), last

let bound_var v = Cil_const.make_logic_var v.lv_name v.lv_type

let rec lval_contains_result v =
  match v with
      TResult _ -> true
    | TMem(t) -> contains_result t
    | TVar _ -> false
and loffset_contains_result o =
  match o with
      TNoOffset -> false
    | TField(_,o) -> loffset_contains_result o
    | TIndex(t,o) -> contains_result t || loffset_contains_result o

(** true if the underlying lval contains an occurence of \result. false
    otherwise or if the term is not an lval.
*)
and contains_result t =
  match t.term_node with
      TLval(v,offs) -> lval_contains_result v || loffset_contains_result offs
    | Tat(t,_) -> contains_result t
    | Told t -> contains_result t
    | _ -> false

(** returns the definition of a predicate.
    @raise Not_found if the predicate is only declared
 *)
let get_pred_body pi =
  match pi.l_body with LBpred p -> p | _ -> raise Not_found

(** true if the given term is a lvalue denoting result or part of it *)
let rec is_result t = match t.term_node with
    TLval (TResult _,_) -> true
  | Tat(t,_) -> is_result t
  | Told t -> is_result t
  | _ -> false

let is_same_list f l1 l2 =
  try List.for_all2 f l1 l2 with Invalid_argument _ -> false

let is_same_opt f x1 x2 =
  match x1,x2 with
      None, None -> true
    | Some x1, Some x2 -> f x1 x2
    | None, _ | _, None -> false

let is_same_logic_label l1 l2 =
  match l1,l2 with
      StmtLabel s1, StmtLabel s2 -> !s1 == !s2
    | LogicLabel s1, LogicLabel s2 -> s1 = s2
    | (StmtLabel _ | LogicLabel _),_ -> false

let rec is_same_type t1 t2 =
  match t1,t2 with
      Ctype t1, Ctype t2 -> Cilutil.equals (Cil.typeSig t1) (Cil.typeSig t2)
    | Ltype(t1,l1), Ltype(t2,l2) ->
        t1.lt_name = t2.lt_name && List.for_all2 is_same_type l1 l2
    | Linteger, Linteger -> true
    | Lreal, Lreal -> true
    | Lvar v1, Lvar v2 -> v1 = v2
    | Larrow(args1,rt1), Larrow(args2,rt2) ->
        is_same_list is_same_type args1 args2 && is_same_type rt1 rt2
    | (Ctype _| Ltype _ | Linteger | Lreal | Lvar _ | Larrow _),
        (Ctype _| Ltype _ | Linteger | Lreal | Lvar _ | Larrow _) ->
        false

let is_same_var v1 v2 =
  v1.lv_name = v2.lv_name &&
  is_same_type v1.lv_type v2.lv_type

(*
let is_same_type_info ti1 ti2 = ti1.nb_params = ti2.nb_params
*)

let is_same_string s1 s2  = s1 = s2

let is_same_logic_signature l1 l2 =
  l1.l_var_info.lv_name = l2.l_var_info.lv_name &&
  is_same_opt is_same_type l1.l_type l2.l_type &&
  is_same_list is_same_string l1.l_tparams l2.l_tparams &&
  is_same_list is_same_var l1.l_profile l2.l_profile &&
  is_same_list is_same_logic_label l1.l_labels l2.l_labels

let is_same_logic_profile l1 l2 =
  l1.l_var_info.lv_name = l2.l_var_info.lv_name &&
  is_same_list (fun v1 v2 -> is_same_type v1.lv_type v2.lv_type)
    l1.l_profile l2.l_profile

let is_same_builtin_profile l1 l2 =
  l1.bl_name = l2.bl_name &&
  is_same_list (fun (_,t1) (_,t2) -> is_same_type t1 t2)
  l1.bl_profile l2.bl_profile

let add_logic_function = Logic_env.add_logic_function_gen is_same_logic_profile

let is_same_logic_ctor_info ci1 ci2 =
  ci1.ctor_name = ci2.ctor_name &&
  ci1.ctor_type.lt_name =  ci2.ctor_type.lt_name &&
  is_same_list is_same_type ci1.ctor_params ci2.ctor_params

let rec is_same_term t1 t2 =
  match t1.term_node, t2.term_node with
      TConst c1, TConst c2 -> c1 = c2
    | TLval l1, TLval l2 -> is_same_tlval l1 l2
    | TSizeOf t1, TSizeOf t2 ->
        Cilutil.equals (Cil.typeSig t1) (Cil.typeSig  t2)
    | TSizeOfE t1, TSizeOfE t2 -> is_same_term t1 t2
    | TSizeOfStr s1, TSizeOfStr s2 -> s1 = s2
    | TAlignOf t1, TAlignOf t2 ->
        Cilutil.equals (Cil.typeSig t1) (Cil.typeSig  t2)
    | TAlignOfE t1, TAlignOfE t2 -> is_same_term t1 t2
    | TUnOp (o1,t1), TUnOp(o2,t2) -> o1 = o2 && is_same_term t1 t2
    | TBinOp(o1,l1,r1), TBinOp(o2,l2,r2) ->
        o1 = o2 && is_same_term l1 l2 && is_same_term r1 r2
    | TCastE(typ1,t1), TCastE(typ2,t2) ->
        Cilutil.equals (Cil.typeSig typ1) (Cil.typeSig  typ2) &&
        is_same_term t1 t2
    | TAddrOf l1, TAddrOf l2 -> is_same_tlval l1 l2
    | TStartOf l1, TStartOf l2 -> is_same_tlval l1 l2
    | Tapp(f1,labels1, args1), Tapp(f2, labels2, args2) ->
        is_same_logic_signature f1 f2 &&
        List.for_all2 (fun l1 l2 -> l1 = l2) labels1 labels2 &&
        List.for_all2 is_same_term args1 args2
    | Tif(c1,t1,e1), Tif(c2,t2,e2) ->
        is_same_term c1 c2 && is_same_term t1 t2 && is_same_term e1 e2
    | Told t1, Told t2 -> is_same_term t1 t2
    | Tat(t1,l1), Tat(t2,l2) -> is_same_term t1 t2 && is_same_logic_label l1 l2
    | Tbase_addr t1, Tbase_addr t2 -> is_same_term t1 t2
    | Tblock_length t1, Tblock_length t2 -> is_same_term t1 t2
    | Tnull, Tnull -> true
    | TCoerce(t1,typ1), TCoerce(t2,typ2) ->
        is_same_term t1 t2 &&
          Cilutil.equals (Cil.typeSig typ1) (Cil.typeSig  typ2)
    | TCoerceE(t1,tt1), TCoerceE(t2,tt2) ->
        is_same_term t1 t2 && is_same_term tt1 tt2
    | Tlambda (v1,t1), Tlambda(v2,t2) ->
        is_same_list is_same_var v1 v2 && is_same_term t1 t2
    | TUpdate(t1,i1,nt1), TUpdate(t2,i2,nt2) ->
        is_same_term t1 t2 && i1 == i2 && is_same_term nt1 nt2
    | Ttypeof t1, Ttypeof t2 ->
	is_same_term t1 t2
    | Ttype ty1, Ttype ty2 ->
	Cilutil.equals (Cil.typeSig ty1) (Cil.typeSig ty2)
    | TDataCons(ci1,prms1), TDataCons(ci2,prms2) ->
        is_same_logic_ctor_info ci1 ci2 &&
          is_same_list is_same_term prms1 prms2

    | Tempty_set, Tempty_set -> true
    | (Tunion l1, Tunion l2) | (Tinter l1, Tinter l2) ->
        (try List.for_all2 is_same_term l1 l2
         with Invalid_argument _ -> false)
    | Tcomprehension(e1,q1,p1), Tcomprehension(e2,q2,p2) ->
        is_same_term e1 e2 &&
          (try List.for_all2 (fun x y -> x.lv_id = y.lv_id) q1 q2
           with Invalid_argument _ -> false) &&
          is_same_opt is_same_named_predicate p1 p2
    | Trange(l1,h1), Trange(l2,h2) ->
        is_same_opt is_same_term l1 l2 && is_same_opt is_same_term h1 h2
    | (TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
      | TAlignOf _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TCastE _
      | TAddrOf _ | TStartOf _ | Tapp _ | Tlambda _ | TDataCons _
      | Tif _ | Told _ | Tat _ | Tbase_addr _ | Tblock_length _ | Tnull
      | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _
      | Tcomprehension _ | Tempty_set | Tunion _ | Tinter _ | Trange _
      ),_ -> false

and is_same_logic_info l1 l2 =
  is_same_logic_signature l1 l2 &&
  is_same_logic_body l1.l_body l2.l_body

and is_same_logic_body b1 b2 =
  match b1,b2 with
    | LBreads l1, LBreads l2 -> is_same_list is_same_identified_term l1 l2
    | LBterm t1, LBterm t2 -> is_same_term t1 t2
    | LBpred p1, LBpred p2 -> is_same_named_predicate p1 p2
    | LBinductive l1, LBinductive l2 -> is_same_list is_same_indcase l1 l2
    | (LBinductive _ | LBpred _ | LBterm _ | LBreads _), _ ->
	false

and is_same_indcase (id1,labs1,typs1,p1) (id2,labs2,typs2,p2) =
  id1 = id2 &&
  is_same_list is_same_logic_label labs1 labs2 &&
  is_same_list (=) typs1 typs2 &&
  is_same_named_predicate p1 p2

and is_same_tlval (h1,o1) (h2,o2) =
  is_same_lhost h1 h2 && is_same_offset o1 o2

and is_same_lhost h1 h2 =
  match h1, h2 with
      TVar v1, TVar v2 -> is_same_var v1 v2
    | TMem t1, TMem t2 -> is_same_term t1 t2
    | TResult t1, TResult t2 ->
        Cilutil.equals (Cil.typeSig t1) (Cil.typeSig t2)
    | (TVar _ | TMem _ | TResult _),_ -> false

and is_same_offset o1 o2 =
  match o1, o2 with
      TNoOffset, TNoOffset -> true
    | TField (f1,o1), TField(f2,o2) ->
        f1.fname = f2.fname && is_same_offset o1 o2
    | TIndex(t1,o1), TIndex(t2,o2) ->
        is_same_term t1 t2 && is_same_offset o1 o2
    | (TNoOffset| TField _| TIndex _),_ -> false

and is_same_predicate p1 p2 =
  match p1, p2 with
    | Pfalse, Pfalse -> true
    | Ptrue, Ptrue -> true
    | Papp(i1,labels1,args1), Papp(i2,labels2,args2) ->
        is_same_logic_signature i1 i2 &&
        List.for_all2 (fun l1 l2 -> l1 = l2) labels1 labels2 &&
        List.for_all2 is_same_term args1 args2
    | Prel(r1,lt1,rt1), Prel(r2,lt2,rt2) ->
        r1 = r2 && is_same_term lt1 lt2 && is_same_term rt1 rt2
    | Pand(lp1,rp1), Pand(lp2,rp2) | Por(lp1,rp1), Por(lp2,rp2)
    | Pxor (lp1,rp1), Pxor(lp2,rp2) | Pimplies(lp1,rp1), Pimplies(lp2,rp2)
    | Piff(lp1,rp1), Piff(lp2,rp2) ->
        is_same_named_predicate lp1 lp2 && is_same_named_predicate rp1 rp2
    | Pnot p1, Pnot p2 ->
        is_same_named_predicate p1 p2
    | Pif (c1,t1,e1), Pif(c2,t2,e2) ->
        is_same_term c1 c2 && is_same_named_predicate t1 t2 &&
          is_same_named_predicate e1 e2
    | Plet (v1,t1,p1), Plet(v2,t2,p2) ->
        v1.lv_id = v2.lv_id &&
        is_same_term t1 t2 && is_same_named_predicate p1 p2
    | Pforall(q1,p1), Pforall(q2,p2) ->
        is_same_list is_same_var q1 q2 && is_same_named_predicate p1 p2
    | Pexists(q1,p1), Pexists(q2,p2) ->
        is_same_list is_same_var q1 q2 && is_same_named_predicate p1 p2
    | Pold(p1), Pold(p2) ->
        is_same_named_predicate p1 p2
    | Pat(p1,l1), Pat(p2,l2) ->
        is_same_named_predicate p1 p2 && is_same_logic_label l1 l2
    | Pvalid t1, Pvalid t2 -> is_same_term t1 t2
    | Pvalid_index(l1,h1), Pvalid_index(l2,h2) ->
        is_same_term l1 l2 && is_same_term h1 h2
    | Pvalid_range(b1,l1,h1), Pvalid_range(b2,l2,h2) ->
        is_same_term b1 b2 && is_same_term l1 l2 && is_same_term h1 h2
    | Pfresh t1, Pfresh t2 -> is_same_term t1 t2
    | Psubtype(lt1,rt1), Psubtype(lt2,rt2) ->
        is_same_term lt1 lt2 && is_same_term rt1 rt2
    | Pseparated(seps1), Pseparated(seps2) ->
        (try List.for_all2 is_same_term seps1 seps2
         with Invalid_argument _ -> false)
    | (Pfalse | Ptrue | Papp _ | Prel _ | Pand _ | Por _ | Pimplies _
      | Piff _ | Pnot _ | Pif _ | Plet _ | Pforall _ | Pexists _
      | Pold _ | Pat _ | Pvalid _ | Pvalid_index _ | Pvalid_range _
      | Pfresh _ | Psubtype _ | Pxor _ | Pseparated _
      ), _ -> false

and is_same_named_predicate pred1 pred2 =
  pred1.name = pred2.name && is_same_predicate pred1.content pred2.content

and is_same_identified_predicate p1 p2 =
  is_same_list (=) p1.ip_name p2.ip_name &&
    is_same_predicate p1.ip_content p2.ip_content

and is_same_identified_term l1 l2 =
  is_same_term l1.it_content l2.it_content

let is_same_zone z1 z2 = match (z1,z2) with
    (Nothing, Nothing) -> true
  | Location loc1, Location loc2 -> is_same_identified_term loc1 loc2
  | (Nothing | Location _), _ -> false

let is_same_assigns (a1,from1) (a2,from2) =
  try
    is_same_zone a1 a2 && List.for_all2 is_same_zone from1 from2
  with Invalid_argument _ -> false

let is_same_variant (v1,o1) (v2,o2) =
  is_same_term v1 v2 &&
    (match o1, o2 with None, None -> true | None, _ | _, None -> false
       | Some o1, Some o2 -> o1 = o2)

let is_same_behavior b1 b2 =
  b1.b_name = b2.b_name &&
  is_same_list is_same_identified_predicate b1.b_assumes b2.b_assumes &&
  is_same_list is_same_identified_predicate b1.b_ensures b2.b_ensures  &&
  is_same_list is_same_assigns b1.b_assigns b2.b_assigns

let is_same_spec spec1 spec2 =
  is_same_list
    is_same_identified_predicate spec1.spec_requires spec2.spec_requires
  &&
    is_same_list
    is_same_behavior spec1.spec_behavior spec2.spec_behavior
  &&
    is_same_opt is_same_variant spec1.spec_variant spec2.spec_variant
  &&
    is_same_opt is_same_identified_predicate
    spec1.spec_terminates spec2.spec_terminates
  && spec1.spec_complete_behaviors = spec2.spec_complete_behaviors
  && spec1.spec_disjoint_behaviors = spec2.spec_disjoint_behaviors

let is_same_logic_type_info t1 t2 =
  t1.lt_name = t2.lt_name &&
  is_same_list (=) t1.lt_params  t2.lt_params &&
  is_same_opt (is_same_list is_same_logic_ctor_info)
  t1.lt_ctors t2.lt_ctors

let rec is_same_global_annotation ga1 ga2 =
  match (ga1,ga2) with
    | Dfun_or_pred li1, Dfun_or_pred li2 -> is_same_logic_info li1 li2
    | Daxiomatic (id1,ga1), Daxiomatic (id2,ga2) ->
        id1 = id2 && is_same_list is_same_global_annotation ga1 ga2
    | Dtype t1, Dtype t2 -> is_same_logic_type_info t1 t2
    | Dlemma(n1,ax1,labs1,typs1,st1), Dlemma(n2,ax2,labs2,typs2,st2) ->
        n1 = n2 && ax1 = ax2 &&
        is_same_list is_same_logic_label labs1 labs2 &&
        is_same_list (=) typs1 typs2 && is_same_named_predicate st1 st2
    | Dinvariant li1, Dinvariant li2 -> is_same_logic_info li1 li2
    | Dtype_annot li1, Dtype_annot li2 -> is_same_logic_info li1 li2
    | (Dfun_or_pred _ | Daxiomatic _ | Dtype _ | Dlemma _
      | Dinvariant _ | Dtype_annot _),
        (Dfun_or_pred _ | Daxiomatic _ | Dtype _ | Dlemma _
        | Dinvariant _ | Dtype_annot _) -> false

let is_same_axiomatic ax1 ax2 =
  is_same_list is_same_global_annotation ax1 ax2

let merge_assigns old_assigns fresh_assigns =
  match old_assigns , fresh_assigns with
      [], [] -> []
    | l, [] | [], l -> l
    | [Nothing,_], [Nothing,_] -> [Nothing,[]]
    | [Nothing,_], _ | _, [Nothing,_] ->
        Cil.warning "found contradictory assign clauses. Keeping only one";
        old_assigns
    | l1, l2 -> l1 @ l2

let merge_funspec old_spec fresh_spec =
  if is_same_spec old_spec fresh_spec ||
     Cil.is_empty_funspec fresh_spec
  then ()
  else if Cil.is_empty_funspec old_spec then
    begin
      old_spec.spec_requires <- fresh_spec.spec_requires;
      old_spec.spec_terminates <- fresh_spec.spec_terminates;
      old_spec.spec_behavior <- fresh_spec.spec_behavior;
      old_spec.spec_complete_behaviors <- fresh_spec.spec_complete_behaviors;
      old_spec.spec_disjoint_behaviors <- fresh_spec.spec_disjoint_behaviors;
      old_spec.spec_variant <- fresh_spec.spec_variant;
    end
  else begin
    old_spec.spec_requires <- old_spec.spec_requires @ fresh_spec.spec_requires;
    old_spec.spec_behavior <-
      old_spec.spec_behavior @
      (List.filter
         (fun b ->
            try
              let old_b = List.find (fun x -> x.b_name = b.b_name)
                old_spec.spec_behavior in
              old_b.b_assumes <- old_b.b_assumes @ b.b_assumes;
              old_b.b_ensures <- old_b.b_ensures @ b.b_ensures;
              old_b.b_assigns <- merge_assigns old_b.b_assigns b.b_assigns;
              false
            with Not_found -> true)
         fresh_spec.spec_behavior);
    begin match old_spec.spec_variant,fresh_spec.spec_variant with
      | None,None -> ()
      | Some _, None -> ()
      | None, Some _ -> old_spec.spec_variant <- fresh_spec.spec_variant
      | Some _old, Some _fresh ->
          Cil.warning
            "found two variants for function specification. Keeping only one."
    end;
    begin match old_spec.spec_terminates, fresh_spec.spec_terminates with
        None, None -> ()
      | Some p1, Some p2 when is_same_identified_predicate p1 p2 -> ()
      | _ ->
          Cil.warning
            "found two different terminates clause for function specification. \
           keeping only one"
    end;
    old_spec.spec_complete_behaviors <-
      old_spec.spec_complete_behaviors @ fresh_spec.spec_complete_behaviors;
    old_spec.spec_disjoint_behaviors <-
      old_spec.spec_disjoint_behaviors @ fresh_spec.spec_disjoint_behaviors
  end

let lhost_c_type = function
    TVar v ->
      (match v.lv_type with
           Ctype ty -> ty
         | _ -> assert false)
  | TMem t ->
      (match t.term_type with
           Ctype (TPtr(ty,_)) -> ty
         | _ -> assert false)
  | TResult _ -> assert false

let is_assert ca = match ca.annot_content with AAssert _ -> true | _ -> false

let is_contract ca =
  match ca.annot_content with AStmtSpec _ -> true | _ -> false

let is_stmt_invariant ca =
  match ca.annot_content with  AInvariant(_,f,_) -> not f | _ -> false

let is_loop_invariant ca =
  match ca.annot_content with AInvariant(_,f,_) -> f | _ -> false

let is_invariant ca =
  match ca.annot_content with AInvariant _ -> true | _ -> false

let is_variant ca =
  match ca.annot_content with AVariant _ -> true | _ -> false

let is_assigns ca =
  match ca.annot_content with AAssigns _ -> true | _ -> false

(*
let is_loop_behavior ca =
  match ca.annot_content with ALoopBehavior _ -> true | _ -> false
*)

let is_pragma ca =
  match ca.annot_content with APragma _ -> true | _ -> false

let is_loop_pragma ca =
  match ca.annot_content with APragma (Loop_pragma _) -> true | _ -> false

let is_slice_pragma ca =
  match ca.annot_content with APragma (Slice_pragma _) -> true | _ -> false

let is_impact_pragma ca =
  match ca.annot_content with APragma (Impact_pragma _) -> true | _ -> false

let is_loop_annot s =
  is_loop_invariant s || is_assigns s || is_variant s || is_loop_pragma s

(*
let is_loop_annot s =
  is_loop_invariant s || is_loop_behavior s || is_variant s || is_loop_pragma s
*)

let extract_loop_pragma l =
  List.fold_right
    (fun ca l -> match ca.annot_content with
         APragma (Loop_pragma lp) -> lp::l | _ -> l) l []

(** {2 Parsing utilities} *)

(** Hack to allow typedefs whose names are ACSL keywords: the state of the
    lexer depends on the parser rule. See logic_lexer.mll and
    logic_parser.mly for more details.
*)

(**
  - false => keywords are all ACSL keywords
  - true => only C keywords are recognized as such.
   (other remains plain identifiers/typenames)
*)
let kw_c_mode = ref false

let enter_kw_c_mode () = kw_c_mode := true

let exit_kw_c_mode () = kw_c_mode := false

let is_kw_c_mode () = !kw_c_mode

let rt_type_mode = ref false

(** enter a mode where any identifier is considered a type name. Needed for
    for return type of a logic function, as the list of admissible variables
    will be known afterwards.
*)

let enter_rt_type_mode () = rt_type_mode:=true

let exit_rt_type_mode () = rt_type_mode:=false

let is_rt_type_mode () = !rt_type_mode

(** {1 Invariant checks}
    The following functions checks some invariants of the AST. They raise
    Not_well_formed if the invariant does not hold.
*)

exception Not_well_formed of (location * string)

let check_assigns  ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) l =
  match l with
      [Nothing,[]] | [Nothing,[Nothing]]-> ()
    | _ ->
        if List.exists
          (function
               (Nothing,_) -> true
             | (Location _,[Nothing]) -> false
             | (Location _,l) -> List.mem Nothing l
          ) l then
        raise
          (Not_well_formed (loc,"Mixing \\nothing and a real location."))

let check_all_assigns  ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) l =
  check_assigns ~loc
    (List.fold_left
       (fun l a -> match a with
(*
	    ALoopBehavior(_,_,a) -> a @ l | _ -> l) [] l)
*)
	    AAssigns(_,a) -> a :: l | _ -> l) [] l)

let check_loop_annotation  ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) l =
  ignore
    (List.fold_left
       (fun has_variant annot ->
          match annot with
              AVariant _ when has_variant ->
                raise
                  (Not_well_formed
                     (loc,"loop annotations can have at most one variant."))
            | AVariant _ -> true
            | _ -> has_variant) false l);
  check_all_assigns ~loc l

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
