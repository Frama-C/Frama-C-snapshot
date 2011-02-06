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
open Logic_const
open Cil_types
open Logic_ptree

exception Not_well_formed of Cil_types.location * string

let mk_dummy_term e ctyp = Logic_const.term e (Ctype ctyp)

let rec instantiate subst = function
  | Ltype(ty,prms) -> Ltype(ty, List.map (instantiate subst) prms)
  | Larrow(args,rt) ->
      Larrow(List.map (instantiate subst) args, instantiate subst rt)
  | Lvar v as ty ->
      (* This is an application of type parameters:
         no need to recursively substitute in the resulting type. *)
      (try List.assoc v subst with Not_found -> ty)
  | Ctype _ | Linteger | Lreal as ty -> ty

let rec unroll_type ?(unroll_typedef=true) = function
  | Ltype (tdef,prms) as ty ->
      (match tdef.lt_def with
         | None | Some (LTsum _) -> ty
         | Some (LTsyn ty) ->
             let subst =
               try
                 List.combine tdef.lt_params prms
               with Invalid_argument _ ->
                 Cilmsg.fatal "Logic type used with wrong number of parameters"
             in
             unroll_type ~unroll_typedef (instantiate subst ty)
      )
  | Ctype ty when unroll_typedef -> Ctype (Cil.unrollType ty)
  | Linteger | Lreal | Lvar _ | Larrow _ | Ctype _ as ty  -> ty

(* compute type signature and removes unnecessary attributes *)
  let type_sig_logic ty =
    let doattr =
      Cil.dropAttributes ["const"; "restrict"; "declspec"; "arraylen"]
    in
    typeSigWithAttrs doattr ty

(* ************************************************************************* *)
(** {1 From C to logic}*)
(* ************************************************************************* *)

let isLogicType f t =
  plain_or_set (function Ctype t -> f t | _ -> false) (unroll_type t)

(** true if the type is a C array (or a set of)*)
let isLogicArrayType = isLogicType Cil.isArrayType

let isLogicCharType = isLogicType Cil.isCharType

let isLogicVoidType = isLogicType Cil.isVoidType

let isLogicPointerType = isLogicType Cil.isPointerType

let isLogicVoidPointerType = isLogicType Cil.isVoidPtrType

let logicCType =
  plain_or_set (function Ctype t -> t
                  | Lvar _ -> Cil.intType
                  | _ -> failwith "not a C type")

let plain_array_to_ptr ty =
  match unroll_type ty with
      Ctype(TArray(ty,lo,_,attr) as tarr) ->
        let rec aux attr = function
            TArray(ty,lo,s,attr') ->
              let attr =
                Cil.addAttributes (Cil.filter_qualifier_attributes attr') attr
              in
              TArray(aux attr ty,lo,s,attr')
          | ty -> Cil.typeAddAttributes attr ty
        in
        let length_attr =
          match lo with
              None -> []
            | Some _ -> begin
                try
                  let len = Cil.bitsSizeOf tarr in
                  let len = try len / (Cil.bitsSizeOf ty)
                  with Cil.SizeOfError _ ->
                    Cilmsg.fatal
                      "Inconsistent information: I know the length of \
                       array type %a, but not of its elements."
                      Cil.d_type tarr
                  in
                  (* Normally, overflow is checked in bitsSizeOf itself *)
                  let la = AInt len in
                  [ Attr("arraylen",[la])]
                with Cil.SizeOfError _ ->
                  Cil.warning
                    "Cannot represent length of array as an attribute";
                  []
              end
        in
        Ctype(TPtr(aux (Cil.filter_qualifier_attributes attr) ty,
                   Cil.addAttributes length_attr attr))
    | ty -> ty

let array_to_ptr = plain_or_set plain_array_to_ptr

let typ_to_logic_type e_typ =
  match Cil.unrollType e_typ with
    | TFloat (_,_) ->  Lreal
    | TInt   (_,_) -> Linteger
    | _ -> Ctype (e_typ)

let named_of_identified_predicate ip =
  { name = ip.ip_name;
    loc = ip.ip_loc;
    content = ip.ip_content }

let translate_old_label s p =
  let get_label () =
    match s.labels with
      | [] -> 
          s.labels <- 
            [Label (Printf.sprintf "__sid_%d_label" s.sid, 
                    Cil_datatype.Stmt.loc s,false)]
      | _ -> ()
  in
  let make_new_at_predicate p =
    get_label();
    let res = pat (p, (StmtLabel (ref s)))
    in res.content
  in
  let make_new_at_term t =
    get_label ();
    let res = tat (t, (StmtLabel (ref s))) in
    res.term_node
  in
  let vis = object
      inherit Cil.nopCilVisitor
      method vpredicate = function
        | Pold p -> 
          ChangeDoChildrenPost(make_new_at_predicate p,fun x -> x)
        | Pat(p,lab) when lab = Logic_const.old_label ->
          ChangeDoChildrenPost(make_new_at_predicate p, fun x -> x)
        | _ -> DoChildren
      method vterm_node = function
        | Told t ->
          ChangeDoChildrenPost(make_new_at_term t,fun x -> x)
        | Tat(t,lab) when lab = Logic_const.old_label ->
          ChangeDoChildrenPost(make_new_at_term t, fun x->x)
        | _ -> DoChildren
  end
  in Cil.visitCilPredicateNamed vis p

let insert_logic_cast typ term =
  let made_term =  Logic_const.term term (Ctype typ) in
  TCastE(typ, made_term)

let rec is_C_array t =
  let is_C_array_lhost = function
      TVar { lv_origin = Some _ } -> true
    (* \result always refer to a C value *)
    | TResult _ -> true
    (* dereference implies an access to a C value. *)
    | TMem _ -> true
    | TVar _ -> false
  in
  isLogicArrayType t.term_type &&
  (match t.term_node with
     | TStartOf (lh,_) -> is_C_array_lhost lh
     | TLval(lh,_) -> is_C_array_lhost lh
     | Told t | Tat(t,_) -> is_C_array t
     | Tif(_,t1,t2) -> is_C_array t1 && is_C_array t2
     | Tlet (_,t) -> is_C_array t
     | _ -> false)
         (* TUpdate gives back a logic array, TStartOf has pointer type anyway,
            other constructors are never arrays. *)

(** do not use it on something which is not a C array *)
let rec mk_logic_StartOf t =
  let my_type = array_to_ptr t.term_type in
  match t.term_node with
      TLval s -> { t with term_node = TStartOf s; term_type = my_type }
    | Told t ->
        { t with term_node = Told (mk_logic_StartOf t); term_type = my_type }
    | Tat (t,l) ->
        { t with term_node = Tat(mk_logic_StartOf t,l); term_type = my_type }
    | Tif(c,t1,t2) ->
        { t with
            term_node = Tif(c,mk_logic_StartOf t1, mk_logic_StartOf t2);
            term_type = my_type
        }
    | Tlet (body,t) ->
        { t with term_node = Tlet(body, mk_logic_StartOf t);
            term_type = my_type }
    | _ -> Cilmsg.fatal "mk_logic_StartOf given a non-C-array term"

let isLogicPointer t =
  isLogicPointerType t.term_type || (is_C_array t)

let mk_logic_pointer_or_StartOf t =
  if isLogicPointer t then
    if is_C_array t then mk_logic_StartOf t else t
  else Cilmsg.fatal "%a is neither a pointer nor a C array" d_term t

let rec expr_to_term ~cast:cast e =
  let e_typ = Cil.typeOf e in
  let loc = e.eloc in
  let result = match e.enode with
    | Const c ->
	(* a constant should be translated into a logic constant,
	   but logic constants do not exist yet => keep C constant *)
	(* may be a problem for big constants, also type of a C constant
	   is a C type (ikind) i.e. architecture dependant *)
	let tc = TConst c in tc
    | SizeOf t -> TSizeOf t
    | SizeOfE e -> TSizeOfE (expr_to_term ~cast e)
    | SizeOfStr s -> TSizeOfStr s
    | StartOf lv -> TStartOf (lval_to_term_lval ~cast lv)
    | AddrOf lv -> TAddrOf (lval_to_term_lval ~cast lv)
    | CastE (ty,e) ->  TCastE (ty,expr_to_term ~cast e)
    | BinOp (op, l, r, _) ->
	let nnode = TBinOp (op,expr_to_term ~cast l,expr_to_term ~cast r) in
	if cast && (Cil.isIntegralType e_typ || Cil.isFloatingType e_typ)
	then
	  TCastE (e_typ, Logic_const.term nnode (typ_to_logic_type e_typ))
	else nnode
    | UnOp (op, e, _) ->
	let nnode = TUnOp (op,expr_to_term ~cast e) in
	if cast && (Cil.isIntegralType e_typ || Cil.isFloatingType e_typ)
	then
	  TCastE (e_typ, Logic_const.term nnode (typ_to_logic_type e_typ))
	else nnode
    | AlignOfE e -> TAlignOfE (expr_to_term ~cast e)
    | AlignOf typ -> TAlignOf typ
    | Lval lv -> TLval (lval_to_term_lval ~cast lv)
    | Info (e,_) -> (expr_to_term ~cast e).term_node
  in
  if cast then Logic_const.term ~loc result (Ctype e_typ)
  else
    match e.enode with
    | Const _ -> Logic_const.term ~loc result (Ctype e_typ)
    | _       -> Logic_const.term ~loc result (typ_to_logic_type e_typ)

and lval_to_term_lval ~cast:cast (host,offset) =
  host_to_term_host ~cast host,offset_to_term_offset ~cast offset

and host_to_term_host ~cast:cast = function
  | Var s -> TVar (Cil.cvar_to_lvar s)
  | Mem e -> TMem (expr_to_term ~cast e)

and offset_to_term_offset ~cast:cast = function
  | NoOffset -> TNoOffset
  | Index (e,off) ->
      TIndex (expr_to_term ~cast e,offset_to_term_offset ~cast off)
  | Field (fi,off) -> TField(fi,offset_to_term_offset ~cast off)


let array_with_range arr size =
  let arr = Cil.mkCast arr Cil.charPtrType in
  let arr' = expr_to_term ~cast:false arr
  and range_end =
    Logic_const.term ~loc:size.term_loc
      (TBinOp (MinusA, size, Cil.lconstant 1L))
      size.term_type
  in
  let range = Logic_const.trange (Some (Cil.lconstant 0L), Some (range_end)) in
  Logic_const.term ~loc:arr.eloc (TBinOp (PlusPI, arr', range))
    (typ_to_logic_type Cil.charPtrType)


(* ************************************************************************* *)
(** {1 Various utilities} *)
(* ************************************************************************* *)

let rec remove_term_offset o =
  match o with
      TNoOffset -> TNoOffset, TNoOffset
    | TIndex(_,TNoOffset) | TField(_,TNoOffset)-> TNoOffset, o
    | TIndex(e,o) ->
        let (oth,last) = remove_term_offset o in TIndex(e,oth), last
    | TField(f,o) ->
        let (oth,last) = remove_term_offset o in TField(f,oth), last

let rec lval_contains_result v =
  match v with
      TResult _ -> true
    | TMem t -> contains_result t
    | TVar _ -> false
and loffset_contains_result o =
  match o with
      TNoOffset -> false
    | TField(_,o) -> loffset_contains_result o
    | TIndex(t,o) -> contains_result t || loffset_contains_result o

(** @return [true] if the underlying lval contains an occurence of
    \result; [false] otherwise or if the term is not an lval. *)
and contains_result t =
  match t.term_node with
      TLval(v,offs) -> lval_contains_result v || loffset_contains_result offs
    | Tat(t,_) -> contains_result t
    | Told t -> contains_result t
    | _ -> false

(** @return the definition of a predicate.
    @raise Not_found if the predicate is only declared *)
let get_pred_body pi =
  match pi.l_body with LBpred p -> p | _ -> raise Not_found

let is_result = Logic_const.is_result

let is_same_list f l1 l2 =
  try List.for_all2 f l1 l2 with Invalid_argument _ -> false

let is_same_logic_label l1 l2 =
  match l1, l2 with
    StmtLabel s1, StmtLabel s2 -> !s1 == !s2
  | StmtLabel _, LogicLabel _
  | LogicLabel _, StmtLabel _
  | LogicLabel (Some _,_), LogicLabel (None, _)
  | LogicLabel (None,_), LogicLabel (Some _, _) -> false
  | LogicLabel (Some s1, l1), LogicLabel (Some s2, l2)  -> (s1 == s2) && (l1 = l2)
  | LogicLabel (None, l1), LogicLabel (None, l2)  -> l1 = l2

let is_same_opt f x1 x2 =
  match x1,x2 with
      None, None -> true
    | Some x1, Some x2 -> f x1 x2
    | None, _ | _, None -> false

let rec is_same_type t1 t2 =
  match t1,t2 with
      Ctype t1, Ctype t2 ->
        Cilutil.equals (type_sig_logic t1) (type_sig_logic t2)
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

let is_same_string (s1: string) s2  = s1 = s2

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

let is_same_constant c1 c2 =
  match c1, c2 with
    CEnum e1, CEnum e2 ->
      e1.einame = e2.einame &&
  e1.eival = e2.eival &&
  e1.eihost.ename = e2.eihost.ename
  | _ -> c1 = c2

let rec is_same_term t1 t2 =
  match t1.term_node, t2.term_node with
      TConst c1, TConst c2 -> is_same_constant c1 c2
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
        is_same_term e1 e2 && is_same_list is_same_var q1 q2 &&
          is_same_opt is_same_named_predicate p1 p2
    | Trange(l1,h1), Trange(l2,h2) ->
        is_same_opt is_same_term l1 l2 && is_same_opt is_same_term h1 h2
    | Tlet(d1,b1), Tlet(d2,b2) ->
        is_same_logic_info d1 d2 && is_same_term b1 b2
    | (TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
      | TAlignOf _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TCastE _
      | TAddrOf _ | TStartOf _ | Tapp _ | Tlambda _ | TDataCons _
      | Tif _ | Told _ | Tat _ | Tbase_addr _ | Tblock_length _ | Tnull
      | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _
      | Tcomprehension _ | Tempty_set | Tunion _ | Tinter _ | Trange _
      | Tlet _
      ),_ -> false

and is_same_logic_info l1 l2 =
  is_same_logic_signature l1 l2 &&
  is_same_logic_body l1.l_body l2.l_body

and is_same_logic_body b1 b2 =
  match b1,b2 with
    | LBnone, LBnone -> true
    | LBreads l1, LBreads l2 -> is_same_list is_same_identified_term l1 l2
    | LBterm t1, LBterm t2 -> is_same_term t1 t2
    | LBpred p1, LBpred p2 -> is_same_named_predicate p1 p2
    | LBinductive l1, LBinductive l2 -> is_same_list is_same_indcase l1 l2
    | (LBnone | LBinductive _ | LBpred _ | LBterm _ | LBreads _), _ ->
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
    | (TVar _ | TMem _ | TResult _ ),_ -> false

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
        List.for_all2
	  (fun (x,y) (z,t) ->
	    is_same_logic_label x z &&
	      is_same_logic_label y t)
	  labels1
	  labels2
	&&
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
    | Plet (d1,p1), Plet(d2,p2) ->
        is_same_logic_info d1 d2 && is_same_named_predicate p1 p2
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

let is_same_deps z1 z2 = match (z1,z2) with
    (FromAny, FromAny) -> true
  | From loc1, From loc2 -> is_same_list is_same_identified_term loc1 loc2
  | (FromAny | From _), _ -> false

let is_same_from (b1,f1) (b2,f2) =
  is_same_identified_term b1 b2 && is_same_deps f1 f2

let is_same_assigns a1 a2 =
match (a1,a2) with
    (WritesAny, WritesAny) -> true
  | Writes loc1, Writes loc2 -> is_same_list is_same_from loc1 loc2
  | (WritesAny | Writes _), _ -> false

let is_same_variant (v1,o1 : _ Cil_types.variant) (v2,o2: _ Cil_types.variant) =
  is_same_term v1 v2 &&
    (match o1, o2 with None, None -> true | None, _ | _, None -> false
       | Some o1, Some o2 -> o1 = o2)

let is_same_post_cond ((k1: Cil_types.termination_kind),p1) (k2,p2) =
  k1 = k2 && is_same_identified_predicate p1 p2

let is_same_behavior b1 b2 =
  b1.b_name = b2.b_name &&
  is_same_list is_same_identified_predicate b1.b_assumes b2.b_assumes &&
  is_same_list is_same_identified_predicate b1.b_requires b2.b_requires &&
  is_same_list is_same_post_cond b1.b_post_cond b2.b_post_cond  &&
  is_same_assigns b1.b_assigns b2.b_assigns

let is_same_spec spec1 spec2 =
    is_same_list
    is_same_behavior spec1.spec_behavior spec2.spec_behavior
  &&
    is_same_opt is_same_variant spec1.spec_variant spec2.spec_variant
  &&
    is_same_opt is_same_identified_predicate
    spec1.spec_terminates spec2.spec_terminates
  && spec1.spec_complete_behaviors = spec2.spec_complete_behaviors
  && spec1.spec_disjoint_behaviors = spec2.spec_disjoint_behaviors

let is_same_logic_type_def d1 d2 =
  match d1,d2 with
      LTsum l1, LTsum l2 -> is_same_list is_same_logic_ctor_info l1 l2
    | LTsyn ty1, LTsyn ty2 -> is_same_type ty1 ty2
    | (LTsyn _ | LTsum _), _ -> false

let is_same_logic_type_info t1 t2 =
  t1.lt_name = t2.lt_name &&
  is_same_list (=) t1.lt_params  t2.lt_params &&
  is_same_opt is_same_logic_type_def t1.lt_def t2.lt_def

let is_same_loop_pragma p1 p2 =
  match p1,p2 with
      Unroll_level t1, Unroll_level t2 -> is_same_term t1 t2
    | Widen_hints l1, Widen_hints l2 -> is_same_list is_same_term l1 l2
    | Widen_variables l1, Widen_variables l2 -> is_same_list is_same_term l1 l2
    | (Unroll_level _ | Widen_hints _ | Widen_variables _), _ -> false

let is_same_slice_pragma p1 p2 =
  match p1,p2 with
      SPexpr t1, SPexpr t2 -> is_same_term t1 t2
    | SPctrl, SPctrl | SPstmt, SPstmt -> true
    | (SPexpr _ | SPctrl | SPstmt), _ -> false

let is_same_impact_pragma p1 p2 =
  match p1,p2 with
    | IPexpr t1, IPexpr t2 -> is_same_term t1 t2
    | IPstmt, IPstmt -> true
    | (IPexpr _ | IPstmt), _ -> false

let is_same_pragma p1 p2 =
  match p1,p2 with
      | Loop_pragma p1, Loop_pragma p2 -> is_same_loop_pragma p1 p2
      | Slice_pragma p1, Slice_pragma p2 -> is_same_slice_pragma p1 p2
      | Impact_pragma p1, Impact_pragma p2 -> is_same_impact_pragma p1 p2
      | (Loop_pragma _ | Slice_pragma _ | Impact_pragma _), _ -> false

let is_same_code_annotation ca1 ca2 =
  match ca1.annot_content, ca2.annot_content with
    | AAssert(l1,p1), AAssert(l2,p2) ->
        is_same_list (=) l1 l2 && is_same_named_predicate p1 p2
    | AStmtSpec s1, AStmtSpec s2 -> is_same_spec s1 s2
    | AInvariant(l1,b1,p1), AInvariant(l2,b2,p2) ->
        is_same_list (=) l1 l2 && b1 = b2 && is_same_named_predicate p1 p2
    | AVariant v1, AVariant v2 -> is_same_variant v1 v2
    | AAssigns(l1,a1), AAssigns(l2,a2) ->
        is_same_list (=) l1 l2 && is_same_assigns a1 a2
    | APragma p1, APragma p2 -> is_same_pragma p1 p2
    | (AAssert _ | AStmtSpec _ | AInvariant _
      | AVariant _ | AAssigns _ | APragma _ ), _ -> false

let rec is_same_global_annotation ga1 ga2 =
  match (ga1,ga2) with
    | Dfun_or_pred (li1,_), Dfun_or_pred (li2,_) -> is_same_logic_info li1 li2
    | Daxiomatic (id1,ga1,_), Daxiomatic (id2,ga2,_) ->
        id1 = id2 && is_same_list is_same_global_annotation ga1 ga2
    | Dtype (t1,_), Dtype (t2,_) -> is_same_logic_type_info t1 t2
    | Dlemma(n1,ax1,labs1,typs1,st1,_), Dlemma(n2,ax2,labs2,typs2,st2,_) ->
        n1 = n2 && ax1 = ax2 &&
        is_same_list is_same_logic_label labs1 labs2 &&
        is_same_list (=) typs1 typs2 && is_same_named_predicate st1 st2
    | Dinvariant (li1,_), Dinvariant (li2,_) -> is_same_logic_info li1 li2
    | Dtype_annot (li1,_), Dtype_annot (li2,_) -> is_same_logic_info li1 li2
    | (Dfun_or_pred _ | Daxiomatic _ | Dtype _ | Dlemma _
      | Dinvariant _ | Dtype_annot _),
        (Dfun_or_pred _ | Daxiomatic _ | Dtype _ | Dlemma _
        | Dinvariant _ | Dtype_annot _) -> false

let is_same_axiomatic ax1 ax2 =
  is_same_list is_same_global_annotation ax1 ax2

let is_same_pl_constant c1 c2 =
  match c1,c2 with
      | IntConstant s1, IntConstant s2
      | FloatConstant s1, FloatConstant s2
      | StringConstant s1, StringConstant s2
      | WStringConstant s1, WStringConstant s2 -> s1 = s2
      | (IntConstant _| FloatConstant _ 
        | StringConstant _ | WStringConstant _), _ -> false

let rec is_same_pl_type t1 t2 =
  match t1, t2 with
      | LTvoid, LTvoid
      | LTinteger, LTinteger
      | LTreal, LTreal -> true
      | LTint k1, LTint k2 ->
        (match k1, k2 with
          | IBool, IBool 
          | IChar, IChar
          | ISChar, ISChar
          | IUChar, IUChar
          | IInt, IInt
          | IUInt, IUInt
          | IShort, IShort
          | IUShort, IUShort
          | ILong, ILong
          | IULong, IULong 
          | ILongLong, ILongLong
          | IULongLong, IULongLong -> true
          | (IBool | IChar | ISChar | IUChar | IInt | IUInt
            | IShort | IUShort | ILong 
            | IULong | ILongLong | IULongLong), _ -> false
        )
      | LTfloat k1, LTfloat k2 ->
        (match k1,k2 with
          | FFloat, FFloat | FDouble, FDouble | FLongDouble, FLongDouble -> true
          | (FFloat | FDouble | FLongDouble),_ -> false)
      | LTarray (t1,c1), LTarray(t2,c2) ->
        is_same_pl_type t1 t2 && is_same_opt is_same_pl_constant c1 c2
      | LTpointer t1, LTpointer t2 -> is_same_pl_type t1 t2
      | LTenum s1, LTenum s2 | LTstruct s1, LTstruct s2 
      | LTunion s1, LTunion s2 -> s1 = s2
      | LTnamed (s1,prms1), LTnamed(s2,prms2) ->
        s1 = s2 && is_same_list is_same_pl_type prms1 prms2
      | LTarrow(prms1,t1), LTarrow(prms2,t2) ->
        is_same_list is_same_pl_type prms1 prms2 && is_same_pl_type t1 t2
      | (LTvoid | LTinteger | LTreal | LTint _ | LTfloat _ | LTarrow _
        | LTarray _ | LTpointer _ | LTenum _ 
        | LTunion _ | LTnamed _ | LTstruct _),_ ->
        false

let is_same_quantifiers =
  is_same_list (fun (t1,x1) (t2,x2) -> x1 = x2 && is_same_pl_type t1 t2)

let is_same_unop op1 op2 =
  match op1,op2 with
    | Uminus, Uminus
    | Ubw_not, Ubw_not
    | Ustar, Ustar
    | Uamp, Uamp -> true
    | (Uminus | Ustar | Uamp | Ubw_not), _ -> false

let is_same_binop op1 op2 =
  match op1, op2 with
    | Badd, Badd | Bsub, Bsub | Bmul, Bmul | Bdiv, Bdiv | Bmod, Bmod
    | Bbw_and, Bbw_and | Bbw_or, Bbw_or | Bbw_xor, Bbw_xor
    | Blshift, Blshift | Brshift, Brshift -> true
    | (Badd | Bsub | Bmul | Bdiv | Bmod | Bbw_and | Bbw_or
      | Bbw_xor | Blshift | Brshift),_ -> false 

let is_same_relation r1 r2 =
  match r1, r2 with
    | Lt, Lt | Gt, Gt | Le, Le | Ge, Ge | Eq, Eq | Neq, Neq -> true
    | (Lt | Gt | Le | Ge | Eq | Neq), _ -> false

let rec is_same_path_elt p1 p2 =
  match p1, p2 with
      PLpathField s1, PLpathField s2 -> s1 = s2
    | PLpathIndex e1, PLpathIndex e2 -> is_same_lexpr e1 e2
    | (PLpathField _ | PLpathIndex _), _ -> false

and is_same_update_term t1 t2 =
  match t1, t2 with
    | PLupdateTerm e1, PLupdateTerm e2 -> is_same_lexpr e1 e2
    | PLupdateCont l1, PLupdateCont l2 ->
      let is_same_elt (p1,e1) (p2,e2) = 
        is_same_list is_same_path_elt p1 p2 && is_same_update_term e1 e2
      in is_same_list is_same_elt l1 l2
    | (PLupdateTerm _ | PLupdateCont _), _ -> false

and is_same_lexpr l1 l2 =
  match l1.lexpr_node,l2.lexpr_node with
    | PLvar s1, PLvar s2 -> s1 = s2
    | PLapp (s1,l1,arg1), PLapp (s2,l2,arg2) ->
      s1 = s2 && is_same_list (=) l1 l2 && is_same_list is_same_lexpr arg1 arg2
    | PLlambda(q1,e1), PLlambda(q2,e2) 
    | PLforall (q1,e1), PLforall(q2,e2) 
    | PLexists(q1,e1), PLexists(q2,e2) ->
      is_same_quantifiers q1 q2 && is_same_lexpr e1 e2
    | PLlet(x1,d1,e1), PLlet(x2,d2,e2) ->
      x1 = x2 && is_same_lexpr d1 d2 && is_same_lexpr e1 e2
    | PLconstant c1, PLconstant c2 -> is_same_pl_constant c1 c2
    | PLunop(op1,e1), PLunop(op2,e2) ->
      is_same_unop op1 op2 && is_same_lexpr e1 e2
    | PLbinop(le1,op1,re1), PLbinop(le2,op2,re2) ->
      is_same_binop op1 op2 && is_same_lexpr le1 le2 && is_same_lexpr re1 re2
    | PLdot(e1,f1), PLdot(e2,f2) | PLarrow(e1,f1), PLarrow(e2,f2) -> 
      f1 = f2 && is_same_lexpr e1 e2
    | PLarrget(b1,o1), PLarrget(b2,o2) -> 
      is_same_lexpr b1 b2 && is_same_lexpr o1 o2
    | PLold e1, PLold e2 -> is_same_lexpr e1 e2
    | PLat (e1,s1), PLat(e2,s2) -> s1 = s2 && is_same_lexpr e1 e2
    | PLbase_addr e1, PLbase_addr e2 | PLblock_length e1, PLblock_length e2 ->
      is_same_lexpr e1 e2
    | PLresult, PLresult | PLnull, PLnull 
    | PLfalse, PLfalse | PLtrue, PLtrue | PLempty, PLempty -> 
      true
    | PLcast(t1,e1), PLcast(t2,e2) | PLcoercion(e1,t1), PLcoercion (e2,t2)-> 
      is_same_pl_type t1 t2 && is_same_lexpr e1 e2
    | PLrange(l1,h1), PLrange(l2,h2) ->
      is_same_opt is_same_lexpr l1 l2 && is_same_opt is_same_lexpr h1 h2
    | PLsizeof t1, PLsizeof t2 -> is_same_pl_type t1 t2
    | PLsizeofE e1,PLsizeofE e2 | PLtypeof e1,PLtypeof e2-> is_same_lexpr e1 e2
    | PLcoercionE (b1,t1), PLcoercionE(b2,t2) 
    | PLsubtype(b1,t1), PLsubtype(b2,t2) ->
      is_same_lexpr b1 b2 && is_same_lexpr t1 t2
    | PLupdate(b1,p1,r1), PLupdate(b2,p2,r2) ->
      is_same_lexpr b1 b2
      && is_same_list is_same_path_elt p1 p2 && is_same_update_term r1 r2
    | PLinitIndex l1, PLinitIndex l2 ->
      let is_same_elt (i1,v1) (i2,v2) = 
        is_same_lexpr i1 i2 && is_same_lexpr v1 v2
      in is_same_list is_same_elt l1 l2
    | PLinitField l1, PLinitField l2 ->
      let is_same_elt (s1,v1) (s2,v2) = s1 = s2 && is_same_lexpr v1 v2 in
      is_same_list is_same_elt l1 l2
    | PLtype t1, PLtype t2 -> is_same_pl_type t1 t2
    | PLrel(le1,r1,re1), PLrel(le2,r2,re2) ->
      is_same_relation r1 r2 && is_same_lexpr le1 le2 && is_same_lexpr re1 re2
    | PLand(l1,r1), PLand(l2,r2) | PLor(l1,r1), PLor(l2,r2)
    | PLimplies(l1,r1), PLimplies(l2,r2) | PLxor(l1,r1), PLxor(l2,r2)
    | PLiff(l1,r1), PLiff(l2,r2) -> 
      is_same_lexpr l1 l2 && is_same_lexpr r1 r2
    | PLnot e1, PLnot e2 
    | PLvalid e1, PLvalid e2 
    | PLfresh e1, PLfresh e2 -> 
      is_same_lexpr e1 e2
    | PLvalid_index (b1,o1), PLvalid_index(b2,o2) ->
      is_same_lexpr b1 b2 && is_same_lexpr o1 o2
    | PLvalid_range (b1,l1,h1), PLvalid_range(b2,l2,h2) ->
      is_same_lexpr b1 b2 && is_same_lexpr l1 l2 && is_same_lexpr h1 h2
    | PLseparated l1, PLseparated l2 ->
      is_same_list is_same_lexpr l1 l2
    | PLif(c1,t1,e1), PLif(c2,t2,e2) ->
      is_same_lexpr c1 c2 && is_same_lexpr t1 t2 && is_same_lexpr e1 e2
    | PLnamed(s1,e1), PLnamed(s2,e2) -> s1 = s2 && is_same_lexpr e1 e2
    | PLcomprehension(e1,q1,p1), PLcomprehension(e2,q2,p2) ->
      is_same_lexpr e1 e2 && is_same_quantifiers q1 q2 
      && is_same_opt is_same_lexpr p1 p2
    | PLsingleton e1, PLsingleton e2 -> is_same_lexpr e1 e2 
    | PLunion l1, PLunion l2 | PLinter l1, PLinter l2 -> 
      is_same_list is_same_lexpr l1 l2
    | (PLvar _ | PLapp _ | PLlambda _ | PLlet _ | PLconstant _ | PLunop _
      | PLbinop _ | PLdot _ | PLarrow _ | PLarrget _ | PLold _ | PLat _
      | PLbase_addr _ | PLblock_length _ | PLresult | PLnull | PLcast _
      | PLrange _ | PLsizeof _ | PLsizeofE _ | PLtypeof _ | PLcoercion _
      | PLcoercionE _ | PLupdate _ | PLinitIndex _ | PLtype _ | PLfalse
      | PLtrue | PLinitField _ | PLrel _ | PLand _ | PLor _ | PLxor _
      | PLimplies _ | PLiff _ | PLnot _ | PLif _ | PLforall _ 
      | PLexists _ | PLvalid _ | PLvalid_index _ | PLvalid_range _ 
      | PLseparated _ | PLfresh _ | PLnamed _ | PLsubtype _ 
      | PLcomprehension _ | PLunion _ | PLinter _ | PLsingleton _ | PLempty
    ),_ -> false

let get_behavior_names spec =
  List.fold_left (fun acc b ->  b.b_name::acc) [] spec.spec_behavior

let merge_assigns a1 a2 =
  if is_same_assigns a1 a2 then a1
  else 
    match (a1,a2) with
        WritesAny,_ -> a2
      | _,WritesAny -> a1
      | _ -> Cil.warning "incompatible assigns clauses. Keeping only one."; a1

let merge_behaviors ~silent old_behaviors fresh_behaviors =
  old_behaviors @
    (List.filter
       (fun b ->
          try
            let old_b = List.find (fun x -> x.b_name = b.b_name)
              old_behaviors in
	      if not (is_same_behavior b old_b) then begin
		if not(silent) then
		  Cil.warning "found different behaviors with the same name. Merging them" ;
		old_b.b_assumes <- old_b.b_assumes @ b.b_assumes;
		old_b.b_requires <- old_b.b_requires @ b.b_requires;
		old_b.b_post_cond <- old_b.b_post_cond @ b.b_post_cond;
		old_b.b_assigns <- merge_assigns old_b.b_assigns b.b_assigns;
	      end ;
              false
          with Not_found -> true)
       fresh_behaviors)

let merge_funspec ?(silent_about_merging_behav=false) old_spec fresh_spec =
  if is_same_spec old_spec fresh_spec ||
     Cil.is_empty_funspec fresh_spec
  then ()
  else if Cil.is_empty_funspec old_spec then
    begin
      old_spec.spec_terminates <- fresh_spec.spec_terminates;
      old_spec.spec_behavior <- fresh_spec.spec_behavior;
      old_spec.spec_complete_behaviors <- fresh_spec.spec_complete_behaviors;
      old_spec.spec_disjoint_behaviors <- fresh_spec.spec_disjoint_behaviors;
      old_spec.spec_variant <- fresh_spec.spec_variant;
    end
  else begin
    old_spec.spec_behavior <-  merge_behaviors ~silent:silent_about_merging_behav
      old_spec.spec_behavior fresh_spec.spec_behavior ;
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
  | TResult ty -> ty

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

let extract_contract l =
  List.fold_right
    (fun ca l -> match ca.annot_content with
         AStmtSpec spec -> spec :: l | _ -> l) l []

(* ************************************************************************* *)
(** {2 Parsing utilities} *)
(* ************************************************************************* *)

(** Hack to allow typedefs whose names are ACSL keywords: the state of the
    lexer depends on the parser rule. See logic_lexer.mll and
    logic_parser.mly for more details. *)

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
    will be known afterwards. *)
let enter_rt_type_mode () = rt_type_mode:=true

let exit_rt_type_mode () = rt_type_mode:=false

let is_rt_type_mode () = !rt_type_mode

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
