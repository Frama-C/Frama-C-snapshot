(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil
open Logic_const
open Cil_types

exception Not_well_formed of Cil_types.location * string

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
                 Kernel.fatal "Logic type used with wrong number of parameters"
             in
             unroll_type ~unroll_typedef (instantiate subst ty)
      )
  | Ctype ty when unroll_typedef -> Ctype (Cil.unrollType ty)
  | Linteger | Lreal | Lvar _ | Larrow _ | Ctype _ as ty  -> ty

let is_instance_of vars t1 t2 =
  let rec aux map t1 t2 =
    match (unroll_type t1, unroll_type t2) with
      | _, Lvar s when List.mem s vars ->
          if Datatype.String.Map.mem s map then
            Cil_datatype.Logic_type.equal t1 (Datatype.String.Map.find s map),
            map
          else
            true, Datatype.String.Map.add s t1 map
      | Ltype(ty1,prms1), Ltype(ty2,prms2) ->
          if Cil_datatype.Logic_type_info.equal ty1 ty2 then
            aux_list map prms1 prms2
          else false, map
      | Larrow(args1,rt1), Larrow(args2,rt2) ->
          let flag,map as res = aux map rt1 rt2 in
          if flag then aux_list map args1 args2 else res
      | Ctype t1, Ctype t2 ->
          Cil_datatype.Typ.equal
            (Cil.typeDeepDropAllAttributes t1)
            (Cil.typeDeepDropAllAttributes t2), 
          map
      | (Lvar _ | Ctype _ | Linteger | Lreal | Ltype _ | Larrow _), _ ->
          Cil_datatype.Logic_type.equal t1 t2, map
  and aux_list map l1 l2 =
    match l1, l2 with
      | [], [] -> true, map
      | [], _ | _, [] -> false, map
      | t1 :: tl1, t2 :: tl2 ->
          let flag, map as res = aux map t1 t2 in
          if flag then aux_list map tl1 tl2 else res
  in fst (aux Datatype.String.Map.empty t1 t2)

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
    | Ctype(TArray(ty,lo,_,attr) as tarr) ->
        let length_attr =
          match lo with
          | None -> []
          | Some _ ->
            try
              let len = Cil.bitsSizeOf tarr in
              let len = try len / (Cil.bitsSizeOf ty)
                with Cil.SizeOfError _ ->
                  Kernel.fatal
                    "Inconsistent information: I know the length of \
                       array type %a, but not of its elements."
                    Cil_printer.pp_typ tarr
              in
              (* Normally, overflow is checked in bitsSizeOf itself *)
              let la = AInt (Integer.of_int len) in
              [ Attr("arraylen",[la])]
            with Cil.SizeOfError _ ->
              Kernel.warning ~current:true
                "Cannot represent length of array as an attribute";
              []
        in
        Ctype(TPtr(ty, Cil.addAttributes length_attr attr))
    | ty -> ty

let array_to_ptr = plain_or_set plain_array_to_ptr

let typ_to_logic_type e_typ = 
  let ty = Cil.unrollType e_typ in
  if Cil.isIntegralType ty then Linteger
  else if Cil.isFloatingType ty then Lreal
  else Ctype e_typ

let predicate_of_identified_predicate ip = ip.ip_content

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
    in res.pred_content
  in
  let make_new_at_term t =
    get_label ();
    let res = tat (t, (StmtLabel (ref s))) in
    res.term_node
  in
  let vis = object
      inherit Cil.nopCilVisitor
      method! vpredicate_node = function
        | Pat(p,lab) when lab = Logic_const.old_label ->
          ChangeDoChildrenPost(make_new_at_predicate p, fun x -> x)
        | _ -> DoChildren
      method! vterm_node = function
        | Tat(t,lab) when lab = Logic_const.old_label ->
          ChangeDoChildrenPost(make_new_at_term t, fun x->x)
        | _ -> DoChildren
  end
  in Cil.visitCilPredicate vis p

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
    | Tif(c,t1,t2) ->
        { t with
            term_node = Tif(c,mk_logic_StartOf t1, mk_logic_StartOf t2);
            term_type = my_type
        }
    | Tlet (body,t) ->
        { t with term_node = Tlet(body, mk_logic_StartOf t);
            term_type = my_type }
    | _ -> Kernel.fatal "mk_logic_StartOf given a non-C-array term"

(* Make an AddrOf. Given an lval of type T will give back an expression of
 * type ptr(T)  *)
let mk_logic_AddrOf ?(loc=Cil_datatype.Location.unknown) lval typ =
  match lval with
    | TMem e, TNoOffset -> Logic_const.term ~loc e.term_node e.term_type
    | b, TIndex(z, TNoOffset) when isLogicZero z ->
      Logic_const.term ~loc (TStartOf (b, TNoOffset))
        (Ctype (TPtr (logicCType typ,[]))) (* array *)
    | _ ->
      Logic_const.term ~loc (TAddrOf lval) (Ctype (TPtr (logicCType typ,[])))

let isLogicPointer t =
  isLogicPointerType t.term_type || (is_C_array t)

let mk_logic_pointer_or_StartOf t =
  if isLogicPointer t then
    if is_C_array t then mk_logic_StartOf t else t
  else
    Kernel.fatal ~source:(fst t.term_loc)
      "%a is neither a pointer nor a C array" Cil_printer.pp_term t

let need_logic_cast oldt newt =
  not (Cil_datatype.Logic_type.equal (Ctype oldt) (Ctype newt))

(* Does the same kind of optimization than [Cil.mkCastT] for [Ctype]. *)
let mk_cast ?(loc=Cil_datatype.Location.unknown) ?(force=false) newt t =
  let mk_cast t = (* to new type [newt] *)
    let typ = Cil.type_remove_attributes_for_logic_type newt 
    in term ~loc (TCastE (typ, t)) (Ctype typ)
  in
  match t.term_type with
  | Ctype oldt ->
      if not (need_logic_cast oldt newt) && not force then t
      else begin
      match Cil.unrollType newt, t.term_node with
      | TPtr _, TCastE (_, t') ->
          (match t'.term_type with
           | Ctype typ' ->
               (match unrollType typ', t'.term_node with
                | (TPtr _ as typ''), _ ->
                    (* Old cast can be removed...*)
                    if need_logic_cast newt typ'' then mk_cast t'
                    else (* In fact, both casts can be removed. *) t'
                | _, TConst (Integer (i,_)) when Integer.is_zero i -> mk_cast t'
                | _ -> mk_cast t
               )
           | _ -> mk_cast t)
      | _ -> (* Do not remove old cast because they are conversions !!! *)
          mk_cast t
      end
  | _ -> mk_cast t

let real_of_float s f =
  { r_literal = s ; r_nearest = f ; r_upper = f ; r_lower = f }

let constant_to_lconstant c = match c with
  | CInt64(i,_,s) -> Integer (i,s)
  | CStr s -> LStr s
  | CWStr s -> LWStr s
  | CChr s -> LChr s
  | CReal (f,_,Some s) -> LReal (real_of_float s f)
  | CEnum e -> LEnum e
  | CReal (f,fkind,None) ->
      let s = match fkind with
        | FFloat -> Format.sprintf "%.8ef" f
        | FDouble | FLongDouble -> Format.sprintf "%.16ed" f
      in
      LReal (real_of_float s f)

let lconstant_to_constant c = match c with
  | Integer (i,s) -> CInt64(i,Cil.intKindForValue i false,s)
  | LStr s -> CStr s
  | LWStr s -> CWStr s
  | LChr s -> CChr s
  | LReal r -> CReal (r.r_nearest,FDouble,Some r.r_literal)
  | LEnum e -> CEnum e


let string_to_float_lconstant str =
    let l = String.length str in
  let hasSuffix s =
    let ls = String.length s in
    l >= ls &&
    s = Transitioning.String.uppercase_ascii (String.sub str (l - ls) ls)
  in
  (* Maybe it ends in U or UL. Strip those *)
  let baseint, kind =
    if  hasSuffix "L" || hasSuffix "l" then
      String.sub str 0 (l - 1), Some FLongDouble
    else if hasSuffix "F" || hasSuffix "f" then
      String.sub str 0 (l - 1), Some FFloat
    else if hasSuffix "D" || hasSuffix "d" then
      String.sub str 0 (l - 1), Some FDouble
    else
      str, None
  in
  match kind with 
    | Some k -> 
        let f = Floating_point.parse_kind k baseint in
        LReal(real_of_float str f.Floating_point.f_nearest)

    | None -> (* parse as double precision interval, because we
                 do not have better *)
        let f = Floating_point.double_precision_of_string baseint in
	let open Floating_point in
	LReal { r_nearest = f.f_nearest ; r_upper = f.f_upper ; r_lower = f.f_lower ;
		r_literal = str }

let numeric_coerce ltyp t =
  let coerce t =
    Logic_const.term ~loc:t.term_loc (TLogic_coerce(ltyp, t)) ltyp
  in
  let oldt = unroll_type t.term_type in 
  if Cil_datatype.Logic_type.equal oldt ltyp then t
  else match t.term_node with
    | TLogic_coerce(_,e) -> coerce e
    | TConst(Integer(i,_)) ->
        (match oldt, ltyp with
        | Ctype (TInt(ikind,_)), Linteger when Cil.fitsInInt ikind i ->
            { t with term_type = Linteger }
        | _ -> coerce t)
    | TCastE(typ, ({ term_node = TConst(Integer(i,_))} as t')) ->
        (match unrollType typ with
        | TInt (ikind,_) when Cil.fitsInInt ikind i ->
            (match unroll_type t'.term_type with
            | Linteger -> t'
            | Ctype (TInt (ikind,_)) when Cil.fitsInInt ikind i ->
                { t' with term_type = Linteger }
            | _ -> coerce t')
        | _ -> coerce t)
    | _ -> coerce t

let rec expr_to_term ~cast e =
  let e_typ = unrollType (Cil.typeOf e) in
  let loc = e.eloc in
  let result = match e.enode with
    | Const c -> TConst (constant_to_lconstant c)
    | SizeOf t -> TSizeOf t
    | SizeOfE e -> TSizeOfE (expr_to_term ~cast e)
    | SizeOfStr s -> TSizeOfStr s
    | StartOf lv -> TStartOf (lval_to_term_lval ~cast lv)
    | AddrOf lv -> TAddrOf (lval_to_term_lval ~cast lv)
    | CastE (ty,e) -> (mk_cast (unrollType ty) (expr_to_term ~cast e)).term_node
    | BinOp (op, l, r, _) ->
      let l' = expr_to_term_coerce ~cast l in
      let r' = expr_to_term_coerce ~cast r in
      (* type of the conversion of e in the logic. Beware that boolean
         operators have boolean type. *)
      let tcast =
        match op, cast with
        | ( Cil_types.Lt | Cil_types.Gt | Cil_types.Le | Cil_types.Ge 
	  | Cil_types.Eq | Cil_types.Ne| Cil_types.LAnd | Cil_types.LOr),
          _ -> Some Logic_const.boolean_type
        | _, true -> Some (typ_to_logic_type e_typ)
        | _, false -> None
      in
      let tnode = TBinOp (op,l',r') in
      (* if [cast], we add a cast. Otherwise, when [op] is an operator
         returning a boolean, we need to cast the whole expression as an
         integral type, because (1) the recursive subcalls expect an
         integer/float/pointer here, and (2) there is no implicit conversion
         Boolean -> integer. *)
      begin match tcast with
        | Some lt -> (mk_cast e_typ (Logic_const.term tnode lt)).term_node
        | None -> tnode
      end
    | UnOp (op, u, _) ->
      let u' = expr_to_term_coerce ~cast u in
      (* See comments for binop case above. *)
      let tcast = match op, cast with
        | Cil_types.LNot, _ -> Some Logic_const.boolean_type
        | _, true -> Some (typ_to_logic_type e_typ)
        | _, false -> None
      in
      let tnode = TUnOp (op, u') in
      begin match tcast with
        | Some lt -> (mk_cast e_typ (Logic_const.term tnode lt)).term_node
        | None -> tnode
      end
    | AlignOfE e -> TAlignOfE (expr_to_term ~cast e)
    | AlignOf typ -> TAlignOf typ
    | Lval lv -> TLval (lval_to_term_lval ~cast lv)
    | Info (e,_) -> (expr_to_term ~cast e).term_node
  in
  if cast then Logic_const.term ~loc result (Ctype e_typ)
  else
    match e.enode with
    | Const _ | Lval _ | CastE _ ->
       (* all immediate values keep their C type by default, and are only lifted
          to integer/real if needed. *)
       Logic_const.term ~loc result (Ctype e_typ)
    | _ -> Logic_const.term ~loc result (typ_to_logic_type e_typ)

and expr_to_term_coerce ~cast e =
  let t = expr_to_term ~cast e in
  match t.term_type with
  | Ctype typ when Cil.isIntegralType typ || Cil.isFloatingType typ ->
      let ltyp = typ_to_logic_type typ in
      numeric_coerce ltyp t
  | _ -> t

and lval_to_term_lval ~cast (host,offset) =
  host_to_term_host ~cast host, offset_to_term_offset ~cast offset

and host_to_term_host ~cast = function
  | Var s -> TVar (Cil.cvar_to_lvar s)
  | Mem e -> TMem (expr_to_term ~cast e) (*no need of numeric coercion - pointer *)

and offset_to_term_offset ~cast:cast = function
  | NoOffset -> TNoOffset
  | Index (e,off) ->
    TIndex (expr_to_term_coerce ~cast e,offset_to_term_offset ~cast off)
  | Field (fi,off) -> TField(fi,offset_to_term_offset ~cast off)


let array_with_range arr size =
  let loc = arr.eloc in
  let arr = Cil.stripCasts arr in
  let typ_arr = typeOf arr in
  let no_cast = isCharPtrType typ_arr || isCharArrayType typ_arr in
  let char_ptr = typ_to_logic_type Cil.charPtrType in
  let arr = expr_to_term ~cast:true arr in
  let arr =
    if no_cast then arr
    else mk_cast ~loc Cil.charPtrType arr
  and range_end =
    Logic_const.term ~loc:size.term_loc
      (TBinOp (MinusA, size, Cil.lconstant Integer.one))
      size.term_type
  in
  let range = Logic_const.trange (Some (Cil.lconstant Integer.zero),
                                  Some (range_end)) in
  Logic_const.term ~loc(TBinOp (PlusPI, arr, range)) char_ptr

let remove_logic_coerce t =
  match t.term_node with
    | TLogic_coerce(_,t) -> t
    | _ -> t

(* ************************************************************************* *)
(** {1 Various utilities} *)
(* ************************************************************************* *)

let rec remove_term_offset o =
  match o with
      TNoOffset -> TNoOffset, TNoOffset
    | TIndex(_,TNoOffset) | TField(_,TNoOffset) | TModel(_,TNoOffset) ->
        TNoOffset, o
    | TIndex(e,o) ->
        let (oth,last) = remove_term_offset o in TIndex(e,oth), last
    | TField(f,o) ->
        let (oth,last) = remove_term_offset o in TField(f,oth), last
    | TModel(f,o) ->
        let oth,last = remove_term_offset o in TModel(f,oth), last

let rec lval_contains_result v =
  match v with
      TResult _ -> true
    | TMem t -> contains_result t
    | TVar _ -> false
and loffset_contains_result o =
  match o with
      TNoOffset -> false
    | TField(_,o) | TModel(_,o) -> loffset_contains_result o
    | TIndex(t,o) -> contains_result t || loffset_contains_result o

(** @return [true] if the underlying lval contains an occurence of
    \result; [false] otherwise or if the term is not an lval. *)
and contains_result t =
  match t.term_node with
      TLval(v,offs) -> lval_contains_result v || loffset_contains_result offs
    | Tat(t,_) -> contains_result t
    | _ -> false

(** @return the definition of a predicate.
    @raise Not_found if the predicate is only declared *)
let get_pred_body pi =
  match pi.l_body with LBpred p -> p | _ -> raise Not_found

let is_result = Logic_const.is_result

let is_trivially_false p =
  match p.pred_content with
      Pfalse -> true
    | _ -> false

let is_trivially_true p =
  match p.pred_content with
      Ptrue -> true
    | _ -> false

let is_same_list f l1 l2 =
  try List.for_all2 f l1 l2 with Invalid_argument _ -> false

(* [VP 2011-04-19] StmtLabel case is a bit restricted, but it's not really 
   possible to do any better, and this function should not be called in 
   contexts where it matters. *)
let is_same_logic_label l1 l2 =
  match l1, l2 with
    StmtLabel s1, StmtLabel s2 -> !s1 == !s2
  | StmtLabel _, LogicLabel _
  | LogicLabel _, StmtLabel _ -> false
    (* What is important here is the name of the logic label, not 
       the hypothetical statement it is referring to. *)
  | LogicLabel (_, l1), LogicLabel (_, l2)  -> l1 = l2

(* same remark as above *)
let compare_logic_label l1 l2 =
  match l1, l2 with
  | StmtLabel s1, StmtLabel s2 -> Cil_datatype.Stmt.compare !s1 !s2
  | StmtLabel _, LogicLabel _ -> 1
  | LogicLabel _, StmtLabel _ -> -1
  | LogicLabel (_,l1), LogicLabel(_,l2) -> String.compare l1 l2

let is_same_opt f x1 x2 =
  match x1,x2 with
      None, None -> true
    | Some x1, Some x2 -> f x1 x2
    | None, _ | _, None -> false

let compare_opt f x1 x2 =
  match x1, x2 with
  | None, None -> 0
  | Some _, None -> 1
  | None, Some _ -> -1
  | Some x1, Some x2 -> f x1 x2

let is_same_c_type t1 t2 =
  Cil_datatype.Logic_type_ByName.equal (Ctype t1) (Ctype t2)

let is_same_type t1 t2 = Cil_datatype.Logic_type_ByName.equal t1 t2

let is_same_var v1 v2 =
  v1.lv_name = v2.lv_name &&
  is_same_type v1.lv_type v2.lv_type

let compare_var v1 v2 =
  let res = String.compare v1.lv_name v2.lv_name in
  if res = 0 then
    Cil_datatype.Logic_type_ByName.compare v1.lv_type v2.lv_type
  else res

let is_same_string (s1: string) s2  = s1 = s2

let is_same_logic_signature l1 l2 =
  l1.l_var_info.lv_name = l2.l_var_info.lv_name &&
  is_same_opt is_same_type l1.l_type l2.l_type &&
  is_same_list is_same_string l1.l_tparams l2.l_tparams &&
  is_same_list is_same_var l1.l_profile l2.l_profile &&
  is_same_list is_same_logic_label l1.l_labels l2.l_labels

let compare_logic_signature l1 l2 =
  let res = String.compare l1.l_var_info.lv_name l2.l_var_info.lv_name in
  if res = 0 then
    let res =
      compare_opt Cil_datatype.Logic_type_ByName.compare l1.l_type l2.l_type
    in
    if res = 0 then
      let res = Extlib.list_compare String.compare l1.l_tparams l2.l_tparams in
      if res = 0 then
        let res = Extlib.list_compare compare_var l1.l_profile l2.l_profile in
        if res = 0 then
          Extlib.list_compare compare_logic_label l1.l_labels l2.l_labels
        else res
      else res
    else res
  else res

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

let compare_logic_ctor_info ci1 ci2 =
  let res = String.compare ci1.ctor_name ci2.ctor_name in
  if res = 0 then
    let res = String.compare ci1.ctor_type.lt_name ci2.ctor_type.lt_name in
    if res = 0 then
      Extlib.list_compare
        Cil_datatype.Logic_type_ByName.compare ci1.ctor_params ci2.ctor_params
    else res
  else res

let is_same_constant = Cil.compareConstant

let is_same_pconstant c1 c2 =
  let open Logic_ptree in
  match c1, c2 with
    | IntConstant c1, IntConstant c2 -> c1 = c2
    | IntConstant _, _ | _, IntConstant _ -> false
    | FloatConstant c1, FloatConstant c2 -> c1 = c2
    | FloatConstant _,_ | _,FloatConstant _ -> false
    | StringConstant c1, StringConstant c2 -> c1 = c2
    | StringConstant _,_ | _,StringConstant _ -> false
    | WStringConstant c1, WStringConstant c2 -> c1 = c2

let is_same_binop o1 o2 =
  match o1,o2 with
    | PlusA, PlusA
    | (PlusPI | IndexPI), (PlusPI | IndexPI) (* Semantically equivalent *)
    | MinusA, MinusA
    | MinusPI, MinusPI
    | MinusPP, MinusPP
    | Mult, Mult
    | Div, Div
    | Mod, Mod
    | Shiftlt, Shiftlt
    | Shiftrt, Shiftrt
    | Cil_types.Lt, Cil_types.Lt
    | Cil_types.Gt, Cil_types.Gt
    | Cil_types.Le, Cil_types.Le
    | Cil_types.Ge, Cil_types.Ge
    | Cil_types.Eq, Cil_types.Eq
    | Cil_types.Ne, Cil_types.Ne
    | BAnd, BAnd
    | BXor, BXor
    | BOr, BOr
    | LAnd, LAnd
    | LOr, LOr ->
        true
    | (PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP | Mult | Div
      | Mod | Shiftlt | Shiftrt | Cil_types.Lt | Cil_types.Gt | Cil_types.Le 
      | Cil_types.Ge | Cil_types.Eq | Cil_types.Ne 
      | BAnd | BXor | BOr | LAnd | LOr), _ ->
        false

let _compare_c c1 c2 =
   match c1, c2 with
     | CEnum e1, CEnum e2 ->
       e1.einame = e2.einame && e1.eihost.ename = e2.eihost.ename &&
       (match constFoldToInt e1.eival, constFoldToInt e2.eival with
          | Some i1, Some i2 -> Integer.equal i1 i2
          | _ -> false)
     | CInt64 (i1,k1,_), CInt64(i2,k2,_) -> 
       k1 = k2 && Integer.equal i1 i2
     | CStr s1, CStr s2 -> s1 = s2
     | CWStr l1, CWStr l2 ->
       (try List.for_all2 (fun x y -> Int64.compare x y = 0) l1 l2
        with Invalid_argument _ -> false)
     | CChr c1, CChr c2 -> c1 = c2
     | CReal(f1,k1,_), CReal(f2,k2,_) -> k1 = k2 && f1 = f2
     | (CEnum _ | CInt64 _ | CStr _ | CWStr _ | CChr _ | CReal _), _ -> false

let rec is_same_term t1 t2 =
  match t1.term_node, t2.term_node with
      TConst c1, TConst c2 -> Cil_datatype.Logic_constant.equal c1 c2
    | TLval l1, TLval l2 -> is_same_tlval l1 l2
    | TSizeOf t1, TSizeOf t2 -> Cil_datatype.TypByName.equal t1 t2
    | TSizeOfE t1, TSizeOfE t2 -> is_same_term t1 t2
    | TSizeOfStr s1, TSizeOfStr s2 -> s1 = s2
    | TAlignOf t1, TAlignOf t2 -> Cil_datatype.TypByName.equal t1 t2
    | TAlignOfE t1, TAlignOfE t2 -> is_same_term t1 t2
    | TUnOp (o1,t1), TUnOp(o2,t2) -> o1 = o2 && is_same_term t1 t2
    | TBinOp(o1,l1,r1), TBinOp(o2,l2,r2) ->
        is_same_binop o1 o2 && is_same_term l1 l2 && is_same_term r1 r2
    | TCastE(typ1,t1), TCastE(typ2,t2) ->
        Cil_datatype.TypByName.equal typ1 typ2 && is_same_term t1 t2
    | TAddrOf l1, TAddrOf l2 -> is_same_tlval l1 l2
    | TStartOf l1, TStartOf l2 -> is_same_tlval l1 l2
    | Tapp(f1,labels1, args1), Tapp(f2, labels2, args2) ->
      is_same_logic_signature f1 f2
      && List.for_all2
        (fun (x,y) (t,z) -> is_same_logic_label x t && is_same_logic_label y z) 
        labels1 labels2
      && List.for_all2 is_same_term args1 args2
    | Tif(c1,t1,e1), Tif(c2,t2,e2) ->
        is_same_term c1 c2 && is_same_term t1 t2 && is_same_term e1 e2
    | Tbase_addr (l1,t1), Tbase_addr (l2,t2)
    | Tblock_length (l1,t1), Tblock_length (l2,t2)
    | Toffset (l1,t1), Toffset (l2,t2)
    | Tat(t1,l1), Tat(t2,l2) -> is_same_logic_label l1 l2 && is_same_term t1 t2
    | Tnull, Tnull -> true
    | TCoerce(t1,typ1), TCoerce(t2,typ2) ->
        is_same_term t1 t2 && Cil_datatype.TypByName.equal typ1 typ2
    | TCoerceE(t1,tt1), TCoerceE(t2,tt2) ->
        is_same_term t1 t2 && is_same_term tt1 tt2
    | Tlambda (v1,t1), Tlambda(v2,t2) ->
        is_same_list is_same_var v1 v2 && is_same_term t1 t2
    | TUpdate(t1,i1,nt1), TUpdate(t2,i2,nt2) ->
        is_same_term t1 t2 && is_same_offset i1 i2 && is_same_term nt1 nt2
    | Ttypeof t1, Ttypeof t2 ->
	is_same_term t1 t2
    | Ttype ty1, Ttype ty2 -> Cil_datatype.TypByName.equal ty1 ty2
    | TDataCons(ci1,prms1), TDataCons(ci2,prms2) ->
        is_same_logic_ctor_info ci1 ci2 &&
          is_same_list is_same_term prms1 prms2

    | Tempty_set, Tempty_set -> true
    | (Tunion l1, Tunion l2) | (Tinter l1, Tinter l2) ->
        (try List.for_all2 is_same_term l1 l2
         with Invalid_argument _ -> false)
    | Tcomprehension(e1,q1,p1), Tcomprehension(e2,q2,p2) ->
        is_same_term e1 e2 && is_same_list is_same_var q1 q2 &&
          is_same_opt is_same_predicate p1 p2
    | Trange(l1,h1), Trange(l2,h2) ->
        is_same_opt is_same_term l1 l2 && is_same_opt is_same_term h1 h2
    | Tlet(d1,b1), Tlet(d2,b2) ->
        is_same_logic_info d1 d2 && is_same_term b1 b2
    | TLogic_coerce(ty1,t1), TLogic_coerce(ty2,t2) ->
        is_same_type ty1 ty2 && is_same_term t1 t2
    | (TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
      | TAlignOf _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TCastE _
      | TAddrOf _ | TStartOf _ | Tapp _ | Tlambda _ | TDataCons _
      | Tif _ | Tat _ | Tbase_addr _ | Tblock_length _ | Toffset _ | Tnull
      | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _
      | Tcomprehension _ | Tempty_set | Tunion _ | Tinter _ | Trange _
      | Tlet _ | TLogic_coerce _
      ),_ -> false

and is_same_logic_info l1 l2 =
  is_same_logic_signature l1 l2 &&
  is_same_logic_body l1.l_body l2.l_body

and is_same_logic_body b1 b2 =
  match b1,b2 with
    | LBnone, LBnone -> true
    | LBreads l1, LBreads l2 -> is_same_list is_same_identified_term l1 l2
    | LBterm t1, LBterm t2 -> is_same_term t1 t2
    | LBpred p1, LBpred p2 -> is_same_predicate p1 p2
    | LBinductive l1, LBinductive l2 -> is_same_list is_same_indcase l1 l2
    | (LBnone | LBinductive _ | LBpred _ | LBterm _ | LBreads _), _ ->
	false

and is_same_indcase (id1,labs1,typs1,p1) (id2,labs2,typs2,p2) =
  id1 = id2 &&
  is_same_list is_same_logic_label labs1 labs2 &&
  is_same_list (=) typs1 typs2 &&
  is_same_predicate p1 p2

and is_same_tlval (h1,o1) (h2,o2) =
  is_same_lhost h1 h2 && is_same_offset o1 o2

and is_same_lhost h1 h2 =
  match h1, h2 with
      TVar v1, TVar v2 -> is_same_var v1 v2
    | TMem t1, TMem t2 -> is_same_term t1 t2
    | TResult t1, TResult t2 -> Cil_datatype.TypByName.equal t1 t2
    | (TVar _ | TMem _ | TResult _ ),_ -> false

and is_same_offset o1 o2 =
  match o1, o2 with
      TNoOffset, TNoOffset -> true
    | TField (f1,o1), TField(f2,o2) ->
        f1.fname = f2.fname && is_same_offset o1 o2
    | TModel(f1,o1), TModel(f2,o2) ->
        f1.mi_name = f2.mi_name && is_same_offset o1 o2
    | TIndex(t1,o1), TIndex(t2,o2) ->
        is_same_term t1 t2 && is_same_offset o1 o2
    | (TNoOffset| TField _| TIndex _ | TModel _),_ -> false

and is_same_predicate_node p1 p2 =
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
        is_same_predicate lp1 lp2 && is_same_predicate rp1 rp2
    | Pnot p1, Pnot p2 ->
        is_same_predicate p1 p2
    | Pif (c1,t1,e1), Pif(c2,t2,e2) ->
        is_same_term c1 c2 && is_same_predicate t1 t2 &&
          is_same_predicate e1 e2
    | Plet (d1,p1), Plet(d2,p2) ->
        is_same_logic_info d1 d2 && is_same_predicate p1 p2
    | Pforall(q1,p1), Pforall(q2,p2) ->
        is_same_list is_same_var q1 q2 && is_same_predicate p1 p2
    | Pexists(q1,p1), Pexists(q2,p2) ->
        is_same_list is_same_var q1 q2 && is_same_predicate p1 p2
    | Pat(p1,l1), Pat(p2,l2) ->
        is_same_logic_label l1 l2 && is_same_predicate p1 p2
    | Pallocable (l1,t1), Pallocable (l2,t2)
    | Pfreeable (l1,t1), Pfreeable (l2,t2)
    | Pvalid (l1,t1), Pvalid (l2,t2)
    | Pvalid_read (l1,t1), Pvalid_read (l2,t2)
    | Pinitialized (l1,t1), Pinitialized (l2,t2) -> 
      is_same_logic_label l1 l2 && is_same_term t1 t2
    | Pvalid_function t1, Pvalid_function t2 ->
        is_same_term t1 t2
    | Pdangling (l1,t1), Pdangling (l2,t2) -> 
	is_same_logic_label l1 l2 && is_same_term t1 t2
    | Pfresh (l1,m1,t1,n1), Pfresh (l2,m2,t2,n2) -> 
	is_same_logic_label l1 l2 && is_same_logic_label m1 m2 &&
	is_same_term t1 t2 && is_same_term n1 n2
    | Psubtype(lt1,rt1), Psubtype(lt2,rt2) ->
        is_same_term lt1 lt2 && is_same_term rt1 rt2
    | Pseparated(seps1), Pseparated(seps2) ->
        (try List.for_all2 is_same_term seps1 seps2
         with Invalid_argument _ -> false)
    | (Pfalse | Ptrue | Papp _ | Prel _ | Pand _ | Por _ | Pimplies _
      | Piff _ | Pnot _ | Pif _ | Plet _ | Pforall _ | Pexists _
      | Pat _ | Pvalid _ | Pvalid_read _ | Pvalid_function _
      | Pinitialized _ | Pdangling _
      | Pfresh _ | Pallocable _ | Pfreeable _ | Psubtype _ | Pxor _ | Pseparated _
      ), _ -> false

and is_same_predicate pred1 pred2 =
  is_same_list Datatype.String.equal pred1.pred_name pred2.pred_name &&
    is_same_predicate_node pred1.pred_content pred2.pred_content

and is_same_identified_predicate p1 p2 =
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

let is_same_allocation a1 a2 =
  match (a1,a2) with
    (FreeAllocAny, FreeAllocAny) -> true
  | FreeAlloc(f1,a1), FreeAlloc(f2,a2) -> 
      is_same_list is_same_identified_term f1 f2 &&
	is_same_list is_same_identified_term a1 a2
  | (FreeAllocAny | FreeAlloc _), _ -> false

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
      Unroll_specs l1, Unroll_specs l2 -> is_same_list is_same_term l1 l2
    | Widen_hints l1, Widen_hints l2 -> is_same_list is_same_term l1 l2
    | Widen_variables l1, Widen_variables l2 -> is_same_list is_same_term l1 l2
    | (Unroll_specs _ | Widen_hints _ | Widen_variables _), _ -> false

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

let is_same_extension (e1, c1) (e2, c2) =
  Datatype.String.equal e1 e2 &&
  match c1, c2 with
  | Ext_id i1, Ext_id i2 -> i1 = i2
  | Ext_terms t1, Ext_terms t2 ->
      is_same_list is_same_term t1 t2
  | Ext_preds p1, Ext_preds p2 ->
      is_same_list is_same_predicate p1 p2
  | (Ext_id _ | Ext_preds _ | Ext_terms _), _ -> false

let is_same_code_annotation (ca1:code_annotation) (ca2:code_annotation) =
  match ca1.annot_content, ca2.annot_content with
    | AAssert(l1,p1), AAssert(l2,p2) ->
        is_same_list (=) l1 l2 && is_same_predicate p1 p2
    | AStmtSpec (l1,s1), AStmtSpec (l2,s2) -> 
	is_same_list (=) l1 l2 && is_same_spec s1 s2
    | AInvariant(l1,b1,p1), AInvariant(l2,b2,p2) ->
        is_same_list (=) l1 l2 && b1 = b2 && is_same_predicate p1 p2
    | AVariant v1, AVariant v2 -> is_same_variant v1 v2
    | AAssigns(l1,a1), AAssigns(l2,a2) ->
        is_same_list (=) l1 l2 && is_same_assigns a1 a2
    | AAllocation(l1,fa1), AAllocation(l2,fa2) ->
        is_same_list (=) l1 l2 && 
	  is_same_allocation fa1 fa2
    | APragma p1, APragma p2 -> is_same_pragma p1 p2
    | AExtended (l1,e1), AExtended(l2,e2) ->
      is_same_list (=) l1 l2 && is_same_extension e1 e2
    | (AAssert _ | AStmtSpec _ | AInvariant _ | AExtended _
      | AVariant _ | AAssigns _ | AAllocation _ | APragma _ ), _ -> false

let is_same_model_info mi1 mi2 =
  mi1.mi_name = mi2.mi_name &&
  is_same_c_type mi1.mi_base_type mi2.mi_base_type &&
  is_same_type mi1.mi_field_type mi2.mi_field_type

let rec is_same_global_annotation ga1 ga2 =
  match (ga1,ga2) with
    | Dfun_or_pred (li1,_), Dfun_or_pred (li2,_) -> is_same_logic_info li1 li2
    | Daxiomatic (id1,ga1,_), Daxiomatic (id2,ga2,_) ->
        id1 = id2 && is_same_list is_same_global_annotation ga1 ga2
    | Dtype (t1,_), Dtype (t2,_) -> is_same_logic_type_info t1 t2
    | Dlemma(n1,ax1,labs1,typs1,st1,_), Dlemma(n2,ax2,labs2,typs2,st2,_) ->
        n1 = n2 && ax1 = ax2 &&
        is_same_list is_same_logic_label labs1 labs2 &&
        is_same_list (=) typs1 typs2 && is_same_predicate st1 st2
    | Dinvariant (li1,_), Dinvariant (li2,_) -> is_same_logic_info li1 li2
    | Dtype_annot (li1,_), Dtype_annot (li2,_) -> is_same_logic_info li1 li2
    | Dmodel_annot (li1,_), Dmodel_annot (li2,_) -> is_same_model_info li1 li2
    | Dcustom_annot (c1, n1, _), Dcustom_annot (c2, n2,_) -> n1 = n2 && c1 = c2
    | Dvolatile(t1,r1,w1,_), Dvolatile(t2,r2,w2,_) ->
      is_same_list is_same_identified_term t1 t2 &&
        is_same_opt (fun x y -> x.vname = y.vname) r1 r2 &&
        is_same_opt (fun x y -> x.vname = y.vname) w1 w2
    | (Dfun_or_pred _ | Daxiomatic _ | Dtype _ | Dlemma _
      | Dinvariant _ | Dtype_annot _ | Dcustom_annot _ | Dmodel_annot _ | Dvolatile _),
        (Dfun_or_pred _ | Daxiomatic _ | Dtype _ | Dlemma _
        | Dinvariant _ | Dtype_annot _ | Dcustom_annot _ | Dmodel_annot _ | Dvolatile _) -> false

let is_same_axiomatic ax1 ax2 =
  is_same_list is_same_global_annotation ax1 ax2

let is_same_pl_constant c1 c2 =
  let open Logic_ptree in
  match c1,c2 with
      | IntConstant s1, IntConstant s2
      | FloatConstant s1, FloatConstant s2
      | StringConstant s1, StringConstant s2
      | WStringConstant s1, WStringConstant s2 -> s1 = s2
      | (IntConstant _| FloatConstant _
        | StringConstant _ | WStringConstant _), _ -> false

let rec is_same_pl_type t1 t2 =
  let open Logic_ptree in
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
      | LTattribute(t1,attr1), LTattribute(t2,attr2) ->
	is_same_pl_type t1 t2 && attr1 = attr2
      | (LTvoid | LTinteger | LTreal | LTint _ | LTfloat _ | LTarrow _
        | LTarray _ | LTpointer _ | LTenum _
        | LTunion _ | LTnamed _ | LTstruct _ | LTattribute _),_ ->
        false

let is_same_quantifiers =
  is_same_list (fun (t1,x1) (t2,x2) -> x1 = x2 && is_same_pl_type t1 t2)

let is_same_unop op1 op2 =
  let open Logic_ptree in
  match op1,op2 with
    | Uminus, Uminus
    | Ubw_not, Ubw_not
    | Ustar, Ustar
    | Uamp, Uamp -> true
    | (Uminus | Ustar | Uamp | Ubw_not), _ -> false

let is_same_binop op1 op2 =
  let open Logic_ptree in
  match op1, op2 with
    | Badd, Badd | Bsub, Bsub | Bmul, Bmul | Bdiv, Bdiv | Bmod, Bmod
    | Bbw_and, Bbw_and | Bbw_or, Bbw_or | Bbw_xor, Bbw_xor
    | Blshift, Blshift | Brshift, Brshift -> true
    | (Badd | Bsub | Bmul | Bdiv | Bmod | Bbw_and | Bbw_or
      | Bbw_xor | Blshift | Brshift),_ -> false

let is_same_relation r1 r2 =
  let open Logic_ptree in
  match r1, r2 with
    | Lt, Lt | Gt, Gt | Le, Le | Ge, Ge | Eq, Eq | Neq, Neq -> true
    | (Lt | Gt | Le | Ge | Eq | Neq), _ -> false

let rec is_same_path_elt p1 p2 =
  let open Logic_ptree in
  match p1, p2 with
      PLpathField s1, PLpathField s2 -> s1 = s2
    | PLpathIndex e1, PLpathIndex e2 -> is_same_lexpr e1 e2
    | (PLpathField _ | PLpathIndex _), _ -> false

and is_same_update_term t1 t2 =
  let open Logic_ptree in
  match t1, t2 with
    | PLupdateTerm e1, PLupdateTerm e2 -> is_same_lexpr e1 e2
    | PLupdateCont l1, PLupdateCont l2 ->
      let is_same_elt (p1,e1) (p2,e2) =
        is_same_list is_same_path_elt p1 p2 && is_same_update_term e1 e2
      in is_same_list is_same_elt l1 l2
    | (PLupdateTerm _ | PLupdateCont _), _ -> false

and is_same_lexpr l1 l2 =
  let open Logic_ptree in
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
    | PLlist l1, PLlist l2 ->
      is_same_list is_same_lexpr l1 l2
    | PLold e1, PLold e2 -> is_same_lexpr e1 e2
    | PLat (e1,s1), PLat(e2,s2) -> s1 = s2 && is_same_lexpr e1 e2
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
    | PLrepeat (l1, r1), PLrepeat (l2,r2)
    | PLand(l1,r1), PLand(l2,r2) | PLor(l1,r1), PLor(l2,r2)
    | PLimplies(l1,r1), PLimplies(l2,r2) | PLxor(l1,r1), PLxor(l2,r2)
    | PLiff(l1,r1), PLiff(l2,r2) ->
      is_same_lexpr l1 l2 && is_same_lexpr r1 r2
    | PLnot e1, PLnot e2 ->
	is_same_lexpr e1 e2
    | PLfresh (l1,e11,e12), PLfresh (l2,e21,e22) ->
	l1=l2 && is_same_lexpr e11 e21 && is_same_lexpr e12 e22
    | PLallocable (l1,e1), PLallocable (l2,e2)
    | PLfreeable (l1,e1), PLfreeable (l2,e2)
    | PLvalid (l1,e1), PLvalid (l2,e2)
    | PLvalid_read (l1,e1), PLvalid_read (l2,e2)
    | PLbase_addr (l1,e1), PLbase_addr (l2,e2)
    | PLoffset (l1,e1), PLoffset (l2,e2)
    | PLblock_length (l1,e1), PLblock_length (l2,e2)
    | PLinitialized (l1,e1), PLinitialized (l2,e2) ->
	l1=l2 && is_same_lexpr e1 e2
    | PLvalid_function e1, PLvalid_function e2 ->
      is_same_lexpr e1 e2
    | PLdangling (l1,e1), PLdangling (l2,e2) ->
	l1=l2 && is_same_lexpr e1 e2
    | PLseparated l1, PLseparated l2 ->
      is_same_list is_same_lexpr l1 l2
    | PLif(c1,t1,e1), PLif(c2,t2,e2) ->
      is_same_lexpr c1 c2 && is_same_lexpr t1 t2 && is_same_lexpr e1 e2
    | PLnamed(s1,e1), PLnamed(s2,e2) -> s1 = s2 && is_same_lexpr e1 e2
    | PLcomprehension(e1,q1,p1), PLcomprehension(e2,q2,p2) ->
      is_same_lexpr e1 e2 && is_same_quantifiers q1 q2
      && is_same_opt is_same_lexpr p1 p2
    | PLset l1, PLset l2 | PLunion l1, PLunion l2 | PLinter l1, PLinter l2 ->
      is_same_list is_same_lexpr l1 l2
    | (PLvar _ | PLapp _ | PLlambda _ | PLlet _ | PLconstant _ | PLunop _
      | PLbinop _ | PLdot _ | PLarrow _ | PLarrget _ | PLlist _ | PLrepeat _
      | PLold _ | PLat _
      | PLbase_addr _ | PLblock_length _ | PLoffset _ 
      | PLresult | PLnull | PLcast _
      | PLrange _ | PLsizeof _ | PLsizeofE _ | PLtypeof _ | PLcoercion _
      | PLcoercionE _ | PLupdate _ | PLinitIndex _ | PLtype _ | PLfalse
      | PLtrue | PLinitField _ | PLrel _ | PLand _ | PLor _ | PLxor _
      | PLimplies _ | PLiff _ | PLnot _ | PLif _ | PLforall _
      | PLexists _ | PLvalid _ | PLvalid_read _ | PLvalid_function _
      | PLfreeable _ | PLallocable _ 
      | PLinitialized _ | PLdangling _ | PLseparated _ | PLfresh _ | PLnamed _
      | PLsubtype _ | PLcomprehension _ | PLunion _ | PLinter _
      | PLset _ | PLempty
    ),_ -> false

let hash_label l = 
  match l with
      StmtLabel _ -> 0 (* We can't rely on sid at this point. *)
    | LogicLabel (_,l) -> 19 + Hashtbl.hash l

exception StopRecursion of int

let rec hash_term (acc,depth,tot) t =
  if tot <= 0 || depth <= 0 then raise (StopRecursion acc)
  else begin
    match t.term_node with
      | TConst c -> (acc + Cil_datatype.Logic_constant.hash c, tot - 1)
      | TLval lv -> hash_term_lval (acc+19,depth - 1,tot -1) lv
      | TSizeOf t -> (acc + 38 + Cil_datatype.TypByName.hash t, tot - 1)
      | TSizeOfE t -> hash_term (acc+57,depth -1, tot-1) t
      | TSizeOfStr s -> (acc + 76 + Hashtbl.hash s, tot - 1)
      | TAlignOf t -> (acc + 95 + Cil_datatype.TypByName.hash t, tot - 1)
      | TAlignOfE t -> hash_term (acc+114,depth-1,tot-1) t
      | TUnOp(op,t) -> hash_term (acc+133+Hashtbl.hash op,depth-1,tot-2) t
      | TBinOp(bop,t1,t2) ->
        let hash1,tot1 = 
          hash_term (acc+152+Hashtbl.hash bop,depth-1,tot-2) t1 
        in
        hash_term (hash1,depth-1,tot1) t2
      | TCastE(ty,t) ->
        let hash1 = Cil_datatype.TypByName.hash ty in
        hash_term (acc+171+hash1,depth-1,tot-2) t
      | TAddrOf lv -> hash_term_lval (acc+190,depth-1,tot-1) lv
      | TStartOf lv -> hash_term_lval (acc+209,depth-1,tot-1) lv
      | Tapp (li,labs,apps) ->
        let hash1 = acc + 228 + Hashtbl.hash li.l_var_info.lv_name in
        let hash_lb (acc,tot) (_,lb) =
          if tot = 0 then raise (StopRecursion acc)
          else (acc + hash_label lb,tot - 1)
        in
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        let res = List.fold_left hash_lb (hash1,tot-1) labs in
        List.fold_left hash_one_term res apps
      | Tlambda(quants,t) ->
        let hash_var (acc,tot) lv =
          if tot = 0 then raise (StopRecursion acc)
          else (acc + Hashtbl.hash lv.lv_name,tot-1)
        in
        let (acc,tot) = List.fold_left hash_var (acc+247,tot-1) quants in
        hash_term (acc,depth-1,tot-1) t
      | TDataCons(ctor,args) ->
        let hash = acc + 266 + Hashtbl.hash ctor.ctor_name in
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        List.fold_left hash_one_term (hash,tot-1) args
      | Tif(t1,t2,t3) ->
        let hash1,tot1 = hash_term (acc+285,depth-1,tot) t1 in
        let hash2,tot2 = hash_term (hash1,depth-1,tot1) t2 in
        hash_term (hash2,depth-1,tot2) t3
      | Tat(t,l) ->
        let hash = acc + 304 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Tbase_addr (l,t) -> 
        let hash = acc + 323 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Tblock_length (l,t) -> 
        let hash = acc + 342 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Toffset (l,t) -> 
        let hash = acc + 351 + hash_label l in
        hash_term (hash,depth-1,tot-2) t
      | Tnull -> acc+361, tot - 1
      | TCoerce(t,ty) ->
        let hash = Cil_datatype.TypByName.hash ty in
        hash_term (acc+380+hash,depth-1,tot-2) t
      | TCoerceE(t1,t2) ->
        let hash1,tot1 = hash_term (acc+399,depth-1,tot-1) t1 in
        hash_term (hash1,depth-1,tot1) t2
      | TUpdate(t1,off,t2) ->
        let hash1,tot1 = hash_term (acc+418,depth-1,tot-1) t1 in
        let hash2,tot2 = hash_term_offset (hash1,depth-1,tot1) off in
        hash_term (hash2,depth-1,tot2) t2
      | Ttypeof t -> hash_term (acc+437,depth-1,tot-1) t
      | Ttype t -> acc + 456 + Cil_datatype.TypByName.hash t, tot - 1
      | Tempty_set -> acc + 475, tot - 1
      | Tunion tl ->
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        List.fold_left hash_one_term (acc+494,tot-1) tl
      | Tinter tl ->
        let hash_one_term (acc,tot) t = hash_term (acc,depth-1,tot) t in
        List.fold_left hash_one_term (acc+513,tot-1) tl
      | Tcomprehension (t,quants,_) -> (* TODO: hash predicates *)
        let hash_var (acc,tot) lv =
          if tot = 0 then raise (StopRecursion acc)
          else (acc + Hashtbl.hash lv.lv_name,tot-1)
        in
        let (acc,tot) = List.fold_left hash_var (acc+532,tot-1) quants in
        hash_term (acc,depth-1,tot-1) t
      | Trange(t1,t2) ->
        let acc = acc + 551 in
        let acc,tot = 
          match t1 with
              None -> acc,tot - 1
            | Some t -> hash_term (acc,depth-1,tot-2) t
        in
        if tot <= 0 then raise (StopRecursion acc)
        else
          (match t2 with
              None -> acc, tot - 1
            | Some t -> hash_term (acc,depth-1,tot-1) t)
      | Tlet(li,t) ->
        hash_term 
          (acc + 570 + Hashtbl.hash li.l_var_info.lv_name, depth-1, tot-1)
          t
      | TLogic_coerce(_,t) ->
          hash_term (acc + 587, depth - 1, tot - 1) t
  end

and hash_term_lval (acc,depth,tot) (h,o) =
  if depth <= 0 || tot <= 0 then raise (StopRecursion acc)
  else begin
    let hash, tot = hash_term_lhost (acc, depth-1, tot - 1) h in
    hash_term_offset (hash, depth-1, tot) o
  end

and hash_term_lhost (acc,depth,tot) h =
  if depth<=0 || tot <= 0 then raise (StopRecursion acc)
  else begin
    match h with
      | TVar lv -> acc + Hashtbl.hash lv.lv_name, tot - 1
      | TResult t -> acc + 19 + Cil_datatype.TypByName.hash t, tot - 2
      | TMem t -> hash_term (acc+38,depth-1,tot-1) t
  end

and hash_term_offset (acc,depth,tot) o =
  if depth<=0 || tot <= 0 then raise (StopRecursion acc)
  else begin
    match o with
      | TNoOffset -> acc, tot - 1
      | TField(fi,o) ->
        hash_term_offset (acc+19+Hashtbl.hash fi.fname,depth-1,tot-1) o
      | TModel(mi,o) ->
          hash_term_offset 
            (acc+31+Cil_datatype.Model_info.hash mi,depth-1,tot-1) o
      | TIndex (t,o) ->
        let hash, tot = hash_term (acc+37,depth-1,tot-1) t in
        hash_term_offset (hash,depth-1,tot) o
  end

let hash_term t = 
  try fst (hash_term (0,10,100) t)
  with StopRecursion h -> h

let rec compare_term t1 t2 =
  match t1.term_node, t2.term_node with
    TConst c1, TConst c2 -> Cil_datatype.Logic_constant.compare c1 c2
  | TConst _, _ -> 1
  | _,TConst _ -> -1
  | TLval l1, TLval l2 -> compare_tlval l1 l2
  | TLval _, _ -> 1
  | _, TLval _ -> -1
  | TSizeOf t1, TSizeOf t2 -> Cil_datatype.TypByName.compare t1 t2
  | TSizeOf _, _ -> 1
  | _, TSizeOf _ -> -1
  | TSizeOfE t1, TSizeOfE t2 -> compare_term t1 t2
  | TSizeOfE _, _ -> 1
  | _, TSizeOfE _ -> -1
  | TSizeOfStr s1, TSizeOfStr s2 -> String.compare s1 s2
  | TSizeOfStr _, _ -> 1
  | _, TSizeOfStr _ -> -1
  | TAlignOf t1, TAlignOf t2 -> Cil_datatype.TypByName.compare t1 t2
  | TAlignOf _, _ -> 1
  | _, TAlignOf _ -> -1
  | TAlignOfE t1, TAlignOfE t2 -> compare_term t1 t2
  | TAlignOfE _, _ -> 1
  | _, TAlignOfE _ -> -1
  | TUnOp (o1,t1), TUnOp(o2,t2) -> 
    let res = Pervasives.compare o1 o2 in
    if res = 0 then compare_term t1 t2 else res
  | TUnOp _, _ -> 1
  | _, TUnOp _ -> -1
  | TBinOp(o1,l1,r1), TBinOp(o2,l2,r2) ->
    let res = Pervasives.compare o1 o2 in
    if res = 0 then
      let res = compare_term l1 l2 in
      if res = 0 then compare_term r1 r2 else res
    else res
  | TBinOp _, _ -> 1
  | _, TBinOp _ -> -1
  | TCastE(typ1,t1), TCastE(typ2,t2) ->
    let res = Cil_datatype.TypByName.compare typ1 typ2 in
    if res = 0 then compare_term t1 t2 else res
  | TCastE _, _ -> 1
  | _, TCastE _ -> -1
  | TAddrOf l1, TAddrOf l2 -> compare_tlval l1 l2
  | TAddrOf _, _ -> 1
  | _, TAddrOf _ -> -1
  | TStartOf l1, TStartOf l2 -> compare_tlval l1 l2
  | TStartOf _, _ -> 1
  | _, TStartOf _ -> -1
  | Tapp(f1,labels1, args1), Tapp(f2, labels2, args2) ->
    let res = compare_logic_signature f1 f2 in
    if res = 0 then
      let compare_labels (x,y) (t,z) =
        let res = compare_logic_label x t in
        if res = 0 then compare_logic_label y z else res
      in
      let res = Extlib.list_compare compare_labels labels1 labels2 in
      if res = 0 then Extlib.list_compare compare_term args1 args2 else res
    else res
  | Tapp _, _ -> 1
  | _, Tapp _ -> -1
  | Tif(c1,t1,e1), Tif(c2,t2,e2) ->
    let res = compare_term c1 c2 in
    if res = 0 then
      let res = compare_term t1 t2 in
      if res = 0 then compare_term e1 e2 else res
    else res
  | Tif _, _ -> 1
  | _, Tif _ -> -1
  | Tbase_addr (l1,t1), Tbase_addr (l2,t2)
  | Tblock_length (l1,t1), Tblock_length (l2,t2)
  | Toffset (l1,t1), Toffset (l2,t2)
  | Tat(t1,l1), Tat(t2,l2) -> 
    let res = compare_logic_label l1 l2 in
    if res = 0 then compare_term t1 t2 else res
  | Tbase_addr _, _ -> 1
  | _, Tbase_addr _ -> -1
  | Tblock_length _, _ -> 1
  | _, Tblock_length _ -> -1
  | Toffset _, _ -> 1
  | _, Toffset _ -> -1
  | Tat _, _ -> 1
  | _, Tat _ -> -1
  | Tnull, Tnull -> 0
  | Tnull, _ -> 1
  | _, Tnull -> -1
  | TCoerce(t1,typ1), TCoerce(t2,typ2) ->
    let res = compare_term t1 t2 in
    if res = 0 then Cil_datatype.TypByName.compare typ1 typ2 else res
  | TCoerce _, _ -> 1
  | _, TCoerce _ -> -1
  | TCoerceE(t1,tt1), TCoerceE(t2,tt2) ->
    let res = compare_term t1 t2 in
    if res = 0 then compare_term tt1 tt2 else res
  | TCoerceE _, _ -> 1
  | _, TCoerceE _ -> -1
  | Tlambda (v1,t1), Tlambda(v2,t2) ->
    let res = Extlib.list_compare compare_var v1 v2 in
    if res = 0 then compare_term t1 t2 else res
  | Tlambda _, _ -> 1
  | _, Tlambda _ -> -1
  | TUpdate(t1,i1,nt1), TUpdate(t2,i2,nt2) ->
    let res = compare_term t1 t2 in
    if res = 0 then 
      let res = compare_offset i1 i2 in
      if res = 0 then compare_term nt1 nt2 else res
    else res
  | TUpdate _, _ -> 1
  | _, TUpdate _ -> -1
  | Ttypeof t1, Ttypeof t2 -> compare_term t1 t2
  | Ttypeof _, _ -> 1
  | _, Ttypeof _ -> -1
  | Ttype ty1, Ttype ty2 -> Cil_datatype.TypByName.compare ty1 ty2
  | Ttype _, _ -> 1
  | _, Ttype _ -> -1
  | TDataCons(ci1,prms1), TDataCons(ci2,prms2) ->
    let res = compare_logic_ctor_info ci1 ci2 in
    if res = 0 then Extlib.list_compare compare_term prms1 prms2 else res
  | TDataCons _, _ -> 1
  | _, TDataCons _ -> -1
  | Tempty_set, Tempty_set -> 0
  | Tempty_set, _ -> 1
  | _, Tempty_set -> -1
  | (Tunion l1, Tunion l2) | (Tinter l1, Tinter l2) ->
    Extlib.list_compare compare_term l1 l2
  | Tunion _, _ -> 1
  | _, Tunion _ -> -1
  | Tinter _, _ -> 1
  | _, Tinter _ -> -1
  | Tcomprehension(e1,q1,p1), Tcomprehension(e2,q2,p2) ->
    let res = compare_term e1 e2 in
    if res = 0 then 
      let res = Extlib.list_compare compare_var q1 q2 in
      if res = 0 then compare_opt compare_predicate p1 p2 else res
    else res
  | Tcomprehension _, _ -> 1
  | _, Tcomprehension _ -> -1
  | Trange(l1,h1), Trange(l2,h2) ->
    let res = compare_opt compare_term l1 l2 in
    if res = 0 then compare_opt compare_term h1 h2 else res
  | Trange _, _ -> 1
  | _, Trange _ -> -1
  | Tlet(d1,b1), Tlet(d2,b2) ->
    let res = compare_logic_info d1 d2 in
    if res = 0 then compare_term b1 b2 else res
  | Tlet _, _ -> 1
  | _, Tlet _ -> -1
  | TLogic_coerce(ty1,t1), TLogic_coerce(ty2,t2) ->
    let res = Cil_datatype.Logic_type_ByName.compare ty1 ty2 in
    if res = 0 then compare_term t1 t2 else res

and compare_logic_info l1 l2 =
  let res = compare_logic_signature l1 l2 in
  if res = 0 then compare_logic_body l1.l_body l2.l_body else res

and compare_logic_body b1 b2 =
  match b1,b2 with
    | LBnone, LBnone -> 0
    | LBnone, _ -> 1
    | _, LBnone -> -1
    | LBreads l1, LBreads l2 ->
      Extlib.list_compare compare_identified_term l1 l2
    | LBreads _, _ -> 1
    | _, LBreads _ -> -1
    | LBterm t1, LBterm t2 -> compare_term t1 t2
    | LBterm _, _ -> 1
    | _, LBterm _ -> -1
    | LBpred p1, LBpred p2 -> compare_predicate p1 p2
    | LBpred _, _ -> 1
    | _, LBpred _ -> -1
    | LBinductive l1, LBinductive l2 ->
      Extlib.list_compare compare_indcase l1 l2

and compare_indcase (id1,labs1,typs1,p1) (id2,labs2,typs2,p2) =
  let res = String.compare id1 id2 in
  if res = 0 then
    let res = Extlib.list_compare compare_logic_label labs1 labs2 in
    if res = 0 then
      let res =
        Extlib.list_compare String.compare typs1 typs2
      in
      if res = 0 then compare_predicate p1 p2 else res
    else res
  else res

and compare_tlval (h1,o1) (h2,o2) =
  let res = compare_lhost h1 h2 in
  if res = 0 then compare_offset o1 o2 else res

and compare_lhost h1 h2 =
  match h1, h2 with
  | TVar v1, TVar v2 -> compare_var v1 v2
  | TVar _, _ -> 1
  | _, TVar _ -> -1
  | TMem t1, TMem t2 -> compare_term t1 t2
  | TMem _, _ -> 1
  | _, TMem _ -> -1
  | TResult t1, TResult t2 -> Cil_datatype.TypByName.compare t1 t2

and compare_offset o1 o2 =
  match o1, o2 with
  | TNoOffset, TNoOffset -> 0
  | TNoOffset, _ -> 1
  | _, TNoOffset -> -1
  | TField (f1,o1), TField(f2,o2) ->
    let res = String.compare f1.fname f2.fname in
    if res = 0 then compare_offset o1 o2 else res
  | TField _, _ -> 1
  | _, TField _ -> -1
  | TModel(f1,o1), TModel(f2,o2) ->
    let res = String.compare f1.mi_name f2.mi_name in
    if res = 0 then compare_offset o1 o2 else res
  | TModel _, _ -> 1
  | _, TModel _ -> -1
  | TIndex(t1,o1), TIndex(t2,o2) ->
    let res = compare_term t1 t2 in
    if res = 0 then compare_offset o1 o2 else res

and compare_predicate_node p1 p2 =
  match p1, p2 with
  | Pfalse, Pfalse -> 0
  | Pfalse, _ -> 1
  | _, Pfalse -> -1
  | Ptrue, Ptrue -> 0
  | Ptrue, _ -> 1
  | _, Ptrue -> -1
  | Papp(i1,labels1,args1), Papp(i2,labels2,args2) ->
    let res = compare_logic_signature i1 i2 in
    if res = 0 then
      let compare_labels (x,y) (z,t) =
	let res = compare_logic_label x z in
        if res = 0 then compare_logic_label y t else res
      in
      let res = Extlib.list_compare compare_labels labels1 labels2 in
      if res = 0 then Extlib.list_compare compare_term args1 args2 else res
    else res
  | Papp _, _ -> 1
  | _, Papp _ -> -1
  | Prel(r1,lt1,rt1), Prel(r2,lt2,rt2) ->
    let res = Pervasives.compare r1 r2 in
    if res = 0 then
      let res = compare_term lt1 lt2 in
      if res = 0 then compare_term rt1 rt2 else res
    else res
  | Prel _, _ -> 1
  | _, Prel _ -> -1
  | Pand(lp1,rp1), Pand(lp2,rp2) | Por(lp1,rp1), Por(lp2,rp2)
  | Pxor (lp1,rp1), Pxor(lp2,rp2) | Pimplies(lp1,rp1), Pimplies(lp2,rp2)
  | Piff(lp1,rp1), Piff(lp2,rp2) ->
    let res = compare_predicate lp1 lp2 in
    if res = 0 then compare_predicate rp1 rp2 else res
  | Pand _, _ -> 1
  | _, Pand _ -> -1
  | Por _, _ -> 1
  | _, Por _ -> -1
  | Pxor _, _ -> 1
  | _, Pxor _ -> -1
  | Pimplies _, _ -> 1
  | _, Pimplies _ -> -1
  | Piff _, _ -> 1
  | _, Piff _ -> -1
  | Pnot p1, Pnot p2 -> compare_predicate p1 p2
  | Pnot _, _ -> 1
  | _, Pnot _ -> -1
  | Pif (c1,t1,e1), Pif(c2,t2,e2) ->
    let res = compare_term c1 c2 in
    if res = 0 then
      let res = compare_predicate t1 t2 in
      if res = 0 then compare_predicate e1 e2 else res
    else res
  | Pif _, _ -> 1
  | _, Pif _ -> -1
  | Plet (d1,p1), Plet(d2,p2) ->
    let res = compare_logic_info d1 d2 in
    if res = 0 then compare_predicate p1 p2 else res
  | Plet _, _ -> 1
  | _, Plet _ -> -1
  | Pforall(q1,p1), Pforall(q2,p2) | Pexists(q1,p1), Pexists(q2,p2) ->
    let res = Extlib.list_compare compare_var q1 q2 in
    if res = 0 then compare_predicate p1 p2 else res
  | Pforall _, _ -> 1
  | _, Pforall _ -> -1
  | Pexists _, _ -> 1
  | _, Pexists _ -> -1
  | Pat(p1,l1), Pat(p2,l2) ->
    let res = compare_logic_label l1 l2 in
    if res = 0 then compare_predicate p1 p2 else res
  | Pat _, _ -> 1
  | _, Pat _ -> -1
  | Pallocable (l1,t1), Pallocable (l2,t2)
  | Pfreeable (l1,t1), Pfreeable (l2,t2)
  | Pvalid (l1,t1), Pvalid (l2,t2)
  | Pvalid_read (l1,t1), Pvalid_read (l2,t2)
  | Pinitialized (l1,t1), Pinitialized (l2,t2)
  | Pdangling (l1,t1), Pdangling (l2,t2) ->
    let res = compare_logic_label l1 l2 in
    if res = 0 then compare_term t1 t2 else res
  | Pallocable _, _ -> 1
  | _, Pallocable _ -> -1
  | Pfreeable _, _ -> 1
  | _, Pfreeable _ -> -1
  | Pvalid _, _ -> 1
  | _, Pvalid _ -> -1
  | Pvalid_read _, _ -> 1
  | _, Pvalid_read _ -> -1
  | Pinitialized _, _ -> 1
  | _, Pinitialized _ -> -1
  | Pdangling _, _ -> 1
  | _, Pdangling _ -> -1
  | Pvalid_function t1, Pvalid_function t2 ->
    compare_term t1 t2
  | Pvalid_function _, _ -> 1
  | _, Pvalid_function _ -> -1
  | Pfresh (l1,m1,t1,n1), Pfresh (l2,m2,t2,n2) -> 
    let res = compare_logic_label l1 l2 in
    if res = 0 then
      let res = compare_logic_label m1 m2 in
      if res = 0 then
	let res = compare_term t1 t2 in
        if res = 0 then compare_term n1 n2 else res
      else res
    else res
  | Pfresh _, _ -> 1
  | _, Pfresh _ -> -1
  | Psubtype(lt1,rt1), Psubtype(lt2,rt2) ->
    let res = compare_term lt1 lt2 in
    if res = 0 then compare_term rt1 rt2 else res
  | Psubtype _, _ -> 1
  | _, Psubtype _ -> -1
  | Pseparated(seps1), Pseparated(seps2) ->
    Extlib.list_compare compare_term seps1 seps2
 
and compare_predicate pred1 pred2 =
  let res = Extlib.list_compare String.compare pred1.pred_name pred2.pred_name in
  if res = 0 then compare_predicate_node pred1.pred_content pred2.pred_content else res

(* unused for now *)
(* and compare_identified_predicate p1 p2 =
  let res = Extlib.list_compare String.compare p1.ip_name p2.ip_name in
  if res = 0 then compare_predicate p1.ip_content p2.ip_content else res
*)
and compare_identified_term l1 l2 = compare_term l1.it_content l2.it_content

let get_behavior_names spec =
  List.fold_left (fun acc b ->  b.b_name::acc) [] spec.spec_behavior

let merge_allocation fa1 fa2 =
  if is_same_allocation fa1 fa2 then fa1
  else
    match (fa1,fa2) with
    | FreeAllocAny, _ -> fa2
    | _, FreeAllocAny -> fa1
    | FreeAlloc([],a),FreeAlloc(f,[]) 
    | FreeAlloc(f,[]),FreeAlloc([],a) -> FreeAlloc(f,a);
    | _ ->
      Kernel.warning ~once:true ~current:true
	"incompatible allocations clauses. Keeping only the first one.";
      fa1

let concat_allocation fa1 fa2 =
  if is_same_allocation fa1 fa2 then fa1
  else
    match (fa1,fa2) with
    | FreeAllocAny, _ -> fa2
    | _, FreeAllocAny -> fa1
    | FreeAlloc(f1,a1),FreeAlloc(f2,a2) -> FreeAlloc(f1@f2,a1@a2) 

(* Merge two from clauses (arguments of constructor Writes). For each assigned
   location, find the From clauses and verify that they are equal. This avoids
   duplicates. Beware: this is quadratic in case of mismatch between the two
   assigns lists. However, in most cases the lists are the same *)
let merge_assigns_list l1 l2 =
  (* Find [asgn] in the list of from clauses given as second argument *)
  let rec matches asgn = function
    | [] -> None, []
    | (asgn', _ as hd) :: q ->
        if is_same_identified_term asgn asgn' then
          Some hd, q  (* Return matching from clause *)
        else
          let r, l = matches asgn q in (* Search further on *)
          r, hd :: l
  in
  let rec aux l1 l2 = match l1, l2 with
    | [], [] -> [] (* Merge finished *)
    | [], _ :: _ -> aux l2 l1 (* to get the warnings on the elements of l2 *)
    | (asgn1, from1 as cl1) :: q1, l2 ->
        match matches asgn1 l2 with
          | None, l2 -> (* asgn1 is only in l1 *)
              (* Warn only if asgn1 is not \result, as \result is only
                 useful to specify a \from clause (and is removed without one)*)
              if not (Logic_const.is_result asgn1.it_content) then begin
                let loc = asgn1.it_content.term_loc in
                Kernel.warning ~once:true ~source:(fst loc)
                  "location %a is not present in all assigns clauses"
                  Cil_printer.pp_identified_term asgn1;
              end;
              (asgn1, from1) :: aux q1 l2
          | Some (asgn2, from2 as cl2), q2 ->
              (* asgn1 is in l1 and l2. Check the from clauses *)
              if is_same_deps from1 from2 || from2 = FromAny then
                cl1 :: aux q1 q2
              else if from1 = FromAny then
                cl2 :: aux q1 q2
              else begin
                let loc1 = asgn1.it_content.term_loc in
                let loc2 = asgn2.it_content.term_loc in
                Kernel.warning ~once:true ~source:(fst loc1)
                  "@[incompatible@ from@ clauses (%a:'%a'@ and@ %a:'%a').@ \
                    Keeping@ only@ the first@ one.@]"
                  Cil_printer.pp_location loc1 Cil_printer.pp_from cl1 
		  Cil_printer.pp_location loc2 Cil_printer.pp_from cl2;
                cl1 :: aux q1 q2
              end
  in
  aux l1 l2

let merge_assigns a1 a2 =
  if is_same_assigns a1 a2 then a1
  else
    match (a1,a2) with
    | WritesAny, _ -> a2
    | _, WritesAny -> a1
    | Writes l1, Writes l2 -> Writes (merge_assigns_list l1 l2)

let concat_assigns a1 a2 =
  match a1,a2 with
      | WritesAny, _ | _, WritesAny -> WritesAny
      | Writes l1, Writes l2 -> Writes (l1 @ l2)

let merge_ip_list l1 l2 =
  List.fold_right
    (fun p acc ->
      if List.exists (fun x -> is_same_identified_predicate p x) acc then acc
      else p::acc)
    l1 l2

let merge_post_cond l1 l2 =
  List.fold_right
    (fun (k1,p1 as pc) acc ->
      if
        List.exists
          (fun (k2,p2) -> k1 = k2 && is_same_identified_predicate p1 p2) acc
      then acc
      else pc::acc)
    l1 l2

let merge_behaviors ~silent old_behaviors fresh_behaviors =
  old_behaviors @
    (List.filter
       (fun b ->
         try
           let old_b = List.find (fun x -> x.b_name = b.b_name) old_behaviors in
	   if not (is_same_behavior b old_b) then begin
	     if not silent then
	       Kernel.warning ~current:true "found two %s. Merging them%t"
                 (if Cil.is_default_behavior b then "contracts"
                  else "behaviors named " ^ b.b_name)
                 (fun fmt ->
                    if Kernel.debug_atleast 1 then
                      Format.fprintf fmt ":@ @[%a@] vs. @[%a@]"
                        Cil_printer.pp_behavior b Cil_printer.pp_behavior old_b)
             ;
	     old_b.b_assumes <- merge_ip_list old_b.b_assumes b.b_assumes;
	     old_b.b_requires <- merge_ip_list old_b.b_requires b.b_requires;
	     old_b.b_post_cond <-
               merge_post_cond old_b.b_post_cond b.b_post_cond;
	     old_b.b_assigns <- merge_assigns old_b.b_assigns b.b_assigns;
	     old_b.b_allocation <- merge_allocation old_b.b_allocation b.b_allocation;
	   end ;
           false
         with Not_found -> true)
       fresh_behaviors)

let merge_funspec ?(silent_about_merging_behav=false) old_spec fresh_spec =
  if not (is_same_spec old_spec fresh_spec || Cil.is_empty_funspec fresh_spec)
  then 
    if Cil.is_empty_funspec old_spec then begin
      old_spec.spec_terminates <- fresh_spec.spec_terminates;
      old_spec.spec_behavior <- fresh_spec.spec_behavior;
      old_spec.spec_complete_behaviors <- fresh_spec.spec_complete_behaviors;
      old_spec.spec_disjoint_behaviors <- fresh_spec.spec_disjoint_behaviors;
      old_spec.spec_variant <- fresh_spec.spec_variant;
    end else begin
      old_spec.spec_behavior <- 
	merge_behaviors ~silent:silent_about_merging_behav
	old_spec.spec_behavior fresh_spec.spec_behavior ;
      (match old_spec.spec_variant,fresh_spec.spec_variant with
      | None,None -> ()
      | Some _, None -> ()
      | None, Some _ -> old_spec.spec_variant <- fresh_spec.spec_variant
      | Some _old, Some _fresh ->
        Kernel.warning ~current:true
          "found two variants for function specification. Keeping only the first one.");
      (match old_spec.spec_terminates, fresh_spec.spec_terminates with
      | None, None -> ()
      | Some p1, Some p2 when is_same_identified_predicate p1 p2 -> ()
      | _ ->
        Kernel.warning ~current:true
          "found two different terminates clause for function specification. \
           keeping only the fist one");
      old_spec.spec_complete_behaviors <-
	List.fold_left (fun acc b -> 
	  if List.mem b old_spec.spec_complete_behaviors then acc
	  else b::acc)
	old_spec.spec_complete_behaviors
	fresh_spec.spec_complete_behaviors ;
      old_spec.spec_disjoint_behaviors <-
	List.fold_left (fun acc b -> 
	  if List.mem b old_spec.spec_disjoint_behaviors then acc
	  else b::acc)
	old_spec.spec_disjoint_behaviors
	fresh_spec.spec_disjoint_behaviors

    end

let clear_funspec spec =
  let tmp = Cil.empty_funspec () in
  spec.spec_terminates <- tmp.spec_terminates;
  spec.spec_behavior <- tmp.spec_behavior;
  spec.spec_complete_behaviors <- tmp.spec_complete_behaviors;
  spec.spec_disjoint_behaviors <- tmp.spec_disjoint_behaviors;
  spec.spec_variant <- tmp.spec_variant

let lhost_c_type thost =
  let extract_ctype lty =
    let get = function
      | Ctype typ -> Some typ
      | Ltype _ | Lvar _ | Linteger | Lreal | Larrow _ -> None
    in
    match Logic_const.plain_or_set get lty with
    | None ->
      Kernel.fatal "[lhost_c_type] logic type %a does not represent a C type"
        Cil_datatype.Logic_type.pretty lty
    | Some ty ->
      ty
  in
  match thost with
  | TVar v -> extract_ctype v.lv_type
  | TMem t ->
    let ty = extract_ctype t.term_type in
    (match Cil.unrollType ty with
    | TPtr(ty, _) -> ty
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

let is_allocation ca = 
  match ca.annot_content with AAllocation _ -> true | _ -> false

let is_assigns ca =
  match ca.annot_content with AAssigns _ -> true | _ -> false

let is_pragma ca =
  match ca.annot_content with APragma _ -> true | _ -> false

let is_loop_pragma ca =
  match ca.annot_content with APragma (Loop_pragma _) -> true | _ -> false

let is_slice_pragma ca =
  match ca.annot_content with APragma (Slice_pragma _) -> true | _ -> false

let is_impact_pragma ca =
  match ca.annot_content with APragma (Impact_pragma _) -> true | _ -> false

let is_loop_extension ca =
  match ca.annot_content with AExtended _ -> true | _ -> false

let is_loop_annot s =
  is_loop_invariant s || is_assigns s || is_allocation s ||
  is_variant s || is_loop_pragma s || is_loop_extension s

let is_trivial_annotation a =
  match a.annot_content with
    | AAssert (_,a) -> is_trivially_true a
    | APragma _ | AStmtSpec _ | AInvariant _ | AVariant _
    | AAssigns _| AAllocation _ | AExtended _
      -> false

let is_property_pragma = function
  | Loop_pragma (Unroll_specs _ | Widen_hints _ | Widen_variables _)
  | Slice_pragma (SPexpr _ | SPctrl | SPstmt)
  | Impact_pragma (IPexpr _ | IPstmt) -> false
(* If at some time a pragma becomes something which should be proven,
   update the pragma-related code in gui/property_navigator.ml *)


let extract_loop_pragma l =
  List.fold_right
    (fun ca l -> match ca.annot_content with
         APragma (Loop_pragma lp) -> lp::l | _ -> l) l []

let extract_contract l =
  List.fold_right
    (fun ca l -> match ca.annot_content with
         AStmtSpec (l1,spec) -> (l1,spec) :: l | _ -> l) l []

class complete_types =
  object
    inherit Cil.nopCilVisitor
    method! vterm t =
      match t.term_node with
        | TLval (TVar v, TNoOffset) 
            when isLogicType Cil.isCompleteType v.lv_type &&
              not (isLogicType Cil.isCompleteType t.term_type) ->
          ChangeDoChildrenPost({ t with term_type = v.lv_type }, fun x -> x)
        | _ -> DoChildren
  end

let complete_types f = Cil.visitCilFileSameGlobals (new complete_types) f

(* ************************************************************************* *)
(** {2 Parsing utilities} *)
(* ************************************************************************* *)

(** Hack to allow typedefs whose names are ACSL keywords: the state of the
    lexer depends on the parser rule. See logic_lexer.mll and
    logic_parser.mly for more details. *)

let extensions = ref Datatype.String.Set.empty

let register_extension s =
  extensions := Datatype.String.Set.add s !extensions

let is_extension s = Datatype.String.Set.mem s !extensions

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

let pointer_comparable ?loc t1 t2 =
  let preds = Logic_env.find_all_logic_functions "\\pointer_comparable" in
  let cfct_ptr = TPtr (TFun(Cil.voidType,None,false,[]),[]) in
  let fct_ptr = Ctype cfct_ptr in
  let obj_ptr = Ctype Cil.voidPtrType in
  let discriminate t =
    let loc = t.term_loc in
    match unroll_type t.term_type with
      | Ctype ty ->
          (match Cil.unrollTypeDeep ty with
             | TPtr(TFun _,_) ->
               mk_cast ~loc cfct_ptr t, fct_ptr
             | TPtr(TVoid _,_) -> t, obj_ptr
             | TPtr _ | TInt _ | TFloat _ | TEnum _ ->
               (* Value may emit pointer_comparable alarms on anything that
                  may be compared. We cast scalar to void* to account for
                  this *)
               mk_cast ~loc Cil.voidPtrType t, obj_ptr
             | TVoid _ | TFun _ | TNamed _ | TComp _ | TBuiltin_va_list _
             | TArray _ (* in logic array do not decay implicitely 
                           into pointers. *)
               ->
                 Kernel.fatal 
                   "Trying to call \\pointer_comparable on non-pointer value"
          )
      | _ ->
          Kernel.fatal
            "Trying to call \\pointer_comparable on non-C pointer type value"
  in
  let t1, ty1 = discriminate t1 in
  let t2, ty2 = discriminate t2 in
  let pi =
    try
      List.find
        (function
           | { l_profile = [v1; v2] } ->
               is_same_type v1.lv_type ty1 &&
               is_same_type v2.lv_type ty2
           | _ -> false) preds
    with Not_found ->
      Kernel.fatal "built-in predicate \\pointer_comparable not found"
  in
  Logic_const.unamed ?loc (Papp (pi, [], [t1;t2]))

let points_to_valid_string ?loc s =
  match Logic_env.find_all_logic_functions "\\points_to_valid_string" with
    [ pi ] ->
      Logic_const.unamed ?loc (Papp (pi, [], [s]))
  | _ -> assert false

let is_min_max_function name li =
  li.l_var_info.lv_name = name &&
  match li.l_profile with
  | [e] ->
    Cil_datatype.Logic_type.equal e.lv_type (Logic_const.make_set_type Linteger)
  | _ -> false
let is_max_function li = is_min_max_function "\\max" li
let is_min_function li = is_min_max_function "\\min" li


let rec constFoldTermToInt ?(machdep=true) (e: term) : Integer.t option =
  match e.term_node with
  | TBinOp(bop, e1, e2) -> constFoldBinOpToInt ~machdep bop e1 e2
  | TUnOp(unop, e) -> constFoldUnOpToInt ~machdep unop e
  | TConst(LChr c) -> Some (charConstToInt c)
  | TConst(LEnum {eival = v}) -> Cil.constFoldToInt ~machdep v
  | TConst (Integer (i, _)) -> Some i
  | TConst (LReal _ | LWStr _ | LStr _) -> None
  | TSizeOf typ -> constFoldSizeOfToInt ~machdep typ
  | TSizeOfE t -> begin
    match unroll_type t.term_type with
    | Ctype typ -> constFoldSizeOfToInt ~machdep typ
    | _ -> None
  end
  | TSizeOfStr s -> Some (Integer.of_int (1 + String.length s))
  | TAlignOf t -> begin
    try Some (Integer.of_int (Cil.bytesAlignOf t))
    with Cil.SizeOfError _ -> None
  end
  | TAlignOfE _ -> None (* exp case is very complex, and possibly incorrect *)
  | TCastE (typ, e) -> constFoldCastToInt ~machdep typ e
  | Toffset (_, t) -> if machdep then constFoldToffset t else None
  | Tif (c, e1, e2) -> begin
    match constFoldTermToInt ~machdep c with
    | None -> None
    | Some i ->
      constFoldTermToInt ~machdep (if Integer.is_zero i then e2 else e1)
  end
  | TLogic_coerce (lt, e) ->
    if lt = Linteger then constFoldTermToInt ~machdep e else None
  | Tnull -> Some Integer.zero 
  | Tapp (li, _, [{term_node = (Tunion args |
                                TLogic_coerce (_, {term_node = Tunion args}))}])
      when is_max_function li ->
    constFoldMinMax ~machdep Integer.max args
  | Tapp (li, _, [{term_node = (Tunion args |
                                TLogic_coerce (_, {term_node = Tunion args}))}])
      when is_min_function li ->
    constFoldMinMax ~machdep Integer.min args

  | TLval _ | TAddrOf _ | TStartOf _ | Tapp _ | Tlambda _ | TDataCons _
  | Tat _ | Tbase_addr _ | Tblock_length _ | TCoerce _ | TCoerceE _
  | TUpdate _ | Ttypeof _ | Ttype _ | Tempty_set | Tunion _ | Tinter _
  | Tcomprehension _ | Trange _ | Tlet _ ->
    None

and constFoldCastToInt ~machdep typ e =
  try
    let ik = match Cil.unrollType typ with
      | TInt (ik, _) -> ik
      | TPtr _ -> theMachine.upointKind
      | TEnum (ei,_) -> ei.ekind
      | _ -> raise Exit
    in
    match constFoldTermToInt ~machdep e with
    | Some i  -> Some (fst (Cil.truncateInteger64 ik i))
    | _ -> None
  with Exit -> None

and constFoldSizeOfToInt ~machdep typ =
  if machdep then
    try Some (Integer.of_int (bytesSizeOf typ))
    with SizeOfError _ -> None
  else None

and constFoldUnOpToInt ~machdep unop e =
  let i = constFoldTermToInt ~machdep e in
  match i with
  | None -> None
  | Some i ->
    match unop with
    | Neg -> Some (Integer.neg i)
    | BNot -> Some (Integer.lognot i)
    | LNot ->
      Some (if Integer.equal i Integer.zero then Integer.one else Integer.zero)

and constFoldBinOpToInt ~machdep bop e1 e2 =
  match constFoldTermToInt ~machdep e1, constFoldTermToInt ~machdep e2 with
  | Some i1, Some i2 -> begin
    let comp op = Some (if op i1 i2 then Integer.one else Integer.zero) in
    let logic op =
      let b1 = not (Integer.is_zero i1) and b2 = not (Integer.is_zero i2) in
      Some (if op b1 b2 then Integer.one else Integer.zero)
    in
    match bop with
    | PlusA -> Some (Integer.add i1 i2)
    | MinusA -> Some (Integer.sub i1 i2)
    | PlusPI | IndexPI | MinusPI | MinusPP -> None
    | Mult -> Some (Integer.mul i1 i2)
    | Div ->
      if Integer.(equal zero i2) && Integer.(is_zero (rem i1 i2)) then None
      else Some (Integer.div i1 i2)
    | Mod -> if Integer.(equal zero i2) then None else Some (Integer.rem i1 i2)
    | BAnd -> Some (Integer.logand i1 i2)
    | BOr -> Some (Integer.logor i1 i2)
    | BXor -> Some (Integer.logxor i1 i2)

    | Shiftlt when Integer.(ge i2 zero) -> Some (Integer.shift_left i1 i2)
    | Shiftrt when Integer.(ge i2 zero) -> Some (Integer.shift_right i1 i2)
    | Shiftlt | Shiftrt -> None

    | Cil_types.Eq -> comp Integer.equal
    | Cil_types.Ne -> comp (fun i1 i2 -> not (Integer.equal i1 i2))
    | Cil_types.Le -> comp Integer.le
    | Cil_types.Ge -> comp Integer.ge
    | Cil_types.Lt -> comp Integer.lt
    | Cil_types.Gt -> comp Integer.gt

    | LAnd -> logic (&&)
    | LOr -> logic (||)

  end
  | None, _ | _, None -> None

(* [t] is the argument of [\offset] *)
and constFoldToffset t =
  match t.term_node with
  | TStartOf (TVar v, offset) | TAddrOf (TVar v, offset) -> begin
    try
      let start, _width = bitsLogicOffset v.lv_type offset in
      let size_char = Integer.eight in
      if Integer.(is_zero (rem start size_char)) then
        Some (Integer.div start size_char)
      else None (* bitfields *)
    with Cil.SizeOfError _ -> None
  end
  | _ -> None

(* This function supposes that ~machdep is [true] *)
and bitsLogicOffset ltyp off : Integer.t * Integer.t =
  let rec loopOff typ width start = function
    | TNoOffset -> start, width
    | TIndex(e, off) -> begin
      let ei = match constFoldTermToInt e with
        | Some i -> i
	| None -> raise (SizeOfError ("Index is not constant", typ))
      in
      let typ_e = Cil.typeOf_array_elem typ in
      let size_e = Integer.of_int (Cil.bitsSizeOf typ_e) in
      loopOff typ size_e (Integer.(add start (mul ei size_e))) off
    end
    | TField(f, off) ->
      if f.fcomp.cstruct then begin
        (* Force the computation of the fields fsize_in_bits and
           foffset_in_bits *)
        ignore (Cil.bitsOffset typ (Field (f, NoOffset)));
        let size = Integer.of_int (Extlib.the f.fsize_in_bits) in
        let offset_f = Integer.of_int (Extlib.the f.foffset_in_bits) in
        loopOff f.ftype size (Integer.add start offset_f) off
      end
      else
        (* All union fields start at offset 0 *)
        loopOff f.ftype (Integer.of_int (Cil.bitsSizeOf f.ftype)) start off
    | TModel _ -> raise (SizeOfError ("bitsLogicOffset on model field", typ))
  in
  match unroll_type ltyp with
  | Ctype typ -> loopOff typ Integer.zero Integer.zero off
  | _ -> raise (SizeOfError ("bitsLogicOffset on logic type", Cil.voidPtrType))

(* Handle \min(\union(args)) or \max(\union(args)), depending on [f] *)
and constFoldMinMax ~machdep f args =
  match args with
  | [] -> None (* meaningless, cannot simplify *)
  | arg :: args ->
    let aux res t =
      match res, constFoldTermToInt ~machdep t with
        | None, _ | _, None -> None
        | Some i, Some i' -> Some (f i i')
    in
    List.fold_left aux (constFoldTermToInt ~machdep arg) args

let rec fold_itv f b e acc =
  if Integer.equal b e then f acc b
  else fold_itv f (Integer.succ b) e (f acc b)

(* Find the initializer for index [i] in [init] *)
let find_init_by_index init i =
  let same_offset (off, _) = match off with
    | Index (i', NoOffset) ->
      Integer.equal i (Extlib.the (Cil.isInteger i'))
    | _ -> false
  in
  snd (List.find same_offset init)

(* Find the initializer for field [f] in [init] *)
let find_init_by_field init f =
  let same_offset (off, _) = match off with
    | Field (f', NoOffset) -> f == f'
    | _ -> false
  in
  snd (List.find same_offset init)

exception CannotSimplify

(* Evaluate the bounds of the range [b..e] as constants. The array being
   indexed has type [typ]. If [b] or [e] are not specified, use default
   values. *)
let const_fold_trange_bounds typ b e =
  let extract = function None -> raise CannotSimplify | Some i -> i in
  let b = match b with
    | Some tb -> extract (constFoldTermToInt tb)
    | None -> Integer.zero
  in
  let e = match e with
    | Some te -> extract (constFoldTermToInt te)
    | None ->
      match Cil.unrollType typ with
      | TArray (_, Some size, _, _) ->
        Integer.pred (extract (Cil.isInteger size))
      | _ -> raise CannotSimplify
  in
  b, e

(** Find the value corresponding to the logic offset [loff] inside the
    initialiser [init]. Zero is used as a default value when the initialiser is
    incomplete. [loff] must have an integral type. Returns a set of values
    when [loff] contains ranges. *)
let find_initial_value init loff =
  let module S = Datatype.Integer.Set in
  let extract = function None -> raise CannotSimplify | Some i -> i in
  let rec aux loff init =
    match loff, init with
    | TNoOffset, SingleInit e -> S.singleton (extract (Cil.constFoldToInt e))
    | TIndex (i, loff), CompoundInit (typ, l) -> begin
      (* Add the initializer at offset [Index(i, loff)] to [acc]. *)
      let add_index acc i =
        let vi =
          try aux loff (find_init_by_index l i)
          with Not_found -> S.singleton Integer.zero
        in
        S.union acc vi
      in
      match i.term_node with
      | Tunion tl ->
        let conv t = extract (constFoldTermToInt t) in
        List.fold_left add_index S.empty (List.map conv tl)
      | Trange (b, e) ->
        let b, e = const_fold_trange_bounds typ b e in
        fold_itv add_index b e S.empty
      | _ ->
        let i = extract (constFoldTermToInt i) in
        add_index S.empty i
    end
    | TField (f, loff), CompoundInit (_, l) ->
      if f.fcomp.cstruct then
        try aux loff (find_init_by_field l f)
        with Not_found -> S.singleton Integer.zero
      else (* too complex, a value might be written through another field *)
        raise CannotSimplify
    | TNoOffset, CompoundInit _
    | (TIndex _ | TField _), SingleInit _ -> assert false
    | TModel _, _ -> raise CannotSimplify
  in
  try
    match init with
    | None -> Some (S.singleton Integer.zero)
    | Some init -> Some (aux loff init)
  with CannotSimplify -> None

(** Evaluate the given term l-value in the initial state *)
let eval_term_lval global_find_init (lhost, loff) =
  match lhost with
  | TVar lvi -> begin
    (** See if we can evaluate the l-value using the initializer of lvi*)
    let off_type = Cil.typeTermOffset lvi.lv_type loff in
    if Logic_const.plain_or_set Cil.isLogicIntegralType off_type then
      match lvi.lv_origin with
      | Some vi when vi.vglob && Cil.typeHasQualifier "const" vi.vtype ->
        find_initial_value (global_find_init vi) loff
      | _ -> None
    else None
  end
  | _ -> None

class simplify_const_lval global_find_init = object (self)
  inherit Cil.genericCilVisitor (Cil.copy_visit (Project.current ()))

  method! vterm t =
    match t.term_node with
    | TLval tlv -> begin
      (* simplify recursively tlv before attempting evaluation *)
      let tlv = Cil.visitCilTermLval (self:>Cil.cilVisitor) tlv in
      match eval_term_lval global_find_init tlv with
      | None -> Cil.SkipChildren
      | Some itvs ->
        (* Replace the value/set of values found by something that has the
           expected logic type (plain/Set) *)
        let typ = Logic_const.plain_or_set Extlib.id t.term_type in
        let aux i l = Logic_const.term (TConst (Integer (i,None))) typ :: l in
        let l = Datatype.Integer.Set.fold aux itvs [] in
        match l, Logic_const.is_plain_type t.term_type with
        | [i], true -> Cil.ChangeTo i
        | _, false -> Cil.ChangeTo (Logic_const.term (Tunion l) t.term_type)
        | _ -> Cil.SkipChildren
    end
    | _ -> Cil.DoChildren
end

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
