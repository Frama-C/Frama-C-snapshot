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

open Cil_types

(** Constructors and utility functions for the logic.
    @plugin development guide *)

(** {1 Identification Numbers} *)

let new_code_annotation, fresh_annot_id =
  let count = ref 0 in
  (fun annot -> incr count; { annot_content = annot; annot_id = !count }),
  (fun () -> incr count; !count)

let refresh_code_annotation annot = new_code_annotation annot.annot_content

let new_predicate, fresh_predicate_id =
  let count = ref 0 in
  (fun p -> incr count;
     { ip_id = !count;
       ip_content = p.content; ip_loc = p.loc; ip_name = p.name }),
  (fun () -> incr count; !count)

let pred_of_id_pred p =
  { name = p.ip_name; loc = p.ip_loc; content = p.ip_content }

let new_location, fresh_location_id =
  let count = ref 0 in
  (fun l -> incr count; { its_id = !count; its_content = l }),
  (fun () -> incr count; !count)

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
       (fun l a -> match a with AAssigns a -> a :: l | _ -> l) [] l)

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

(** {1 Smart constructors} *)
(** {2 Predicate constructors} *)

let unamed ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p =
  {content = p ; loc = loc; name = [] }

let ptrue = unamed Ptrue

let pfalse = unamed Pfalse

let pold ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = match p.content with
| Ptrue | Pfalse -> p
| _ -> {p with content = Pold p; loc = loc}

let papp ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,lab,a) =
  unamed ~loc (Papp(p,lab,a))

let pand ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1, p2) =
  match p1.content, p2.content with
| Ptrue, _ -> p2
| _, Ptrue -> p1
| Pfalse, _ -> p1
| _, Pfalse -> p2
| _, _ -> unamed ~loc (Pand (p1, p2))

let por ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1, p2) =
match p1.content, p2.content with
| Ptrue, _ -> p1
| _, Ptrue -> p2
| Pfalse, _ -> p2
| _, Pfalse -> p1
| _, _ -> unamed ~loc (Por (p1, p2))

let pxor ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1, p2) =
  match p1.content, p2.content with
      Ptrue, Ptrue -> unamed ~loc Pfalse
    | Ptrue, _ -> p1
    | _, Ptrue -> p2
    | Pfalse, _ -> p2
    | _, Pfalse -> p1
    | _,_ -> unamed ~loc (Pxor (p1,p2))

let pnot ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p2 = match p2.content with
| Ptrue -> {p2 with content = Pfalse; loc = loc }
| Pfalse ->  {p2 with content = Ptrue; loc = loc }
| _ -> unamed ~loc (Pnot p2)

let pands l = List.fold_right (fun p1 p2 -> pand (p1, p2)) l ptrue

let plet ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = match p.content with
| (_, _, ({content = Ptrue} as p)) -> p
| (v, t, p) -> unamed ~loc (Plet (v, t, p))

let pimplies ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p1,p2) =
match p1.content,p2.content with
| Ptrue, _ | _, Ptrue -> p2
| Pfalse, _ ->
    {name = p1.name ; loc = loc; content = Ptrue }
| _, _ -> unamed ~loc (Pimplies (p1, p2))

let pif ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (t,p2,p3) =
match (p2.content,p3.content) with
| Ptrue, Ptrue  -> ptrue
| Pfalse, Pfalse -> pfalse
| _,_ -> unamed ~loc (Pif (t,p2,p3))

let piff ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p2,p3) =
match (p2.content,p3.content) with
| Pfalse, Pfalse -> ptrue
| Ptrue, _  -> p3
| _, Ptrue -> p2
| _,_ -> unamed ~loc (Piff (p2,p3))

(** @plugin development guide *)
let prel ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (a,b,c) =
  unamed ~loc (Prel(a,b,c))

let pforall ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (l,p) = match l with
| [] -> p
| _ -> match p.content with
  | Ptrue -> p
  | _ -> unamed ~loc (Pforall (l,p))

let pexists ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (l,p) = match l with
| [] -> p
| _ -> match p.content with
  | Pfalse -> p
  | _ -> unamed ~loc (Pexists (l,p))

let pfresh ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = unamed ~loc (Pfresh p)
let pvalid ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) p = unamed ~loc (Pvalid p)
let pat ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) = unamed ~loc (Pat (p,q))
let pvalid_index ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) =
  unamed ~loc (Pvalid_index (p,q))
let pvalid_range ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q,r) =
  unamed ~loc (Pvalid_range (p,q,r))
let psubtype ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) (p,q) =
  unamed ~loc (Psubtype (p,q))

let pseparated  ?(loc=Lexing.dummy_pos,Lexing.dummy_pos) seps =
  unamed ~loc (Pseparated seps)

(** {2 terms} *)
let taddrof ?(loc=Lexing.dummy_pos, Lexing.dummy_pos) lv typ =
  match lv with
    | TMem h, TNoOffset -> h
    | _ -> { term_node = TAddrOf lv;
             term_type = typ;
             term_name = [];
             term_loc = loc}

(** {1 From C to logic}*)

(** @plugin development guide *)
let mk_dummy_term e ctyp =
  { term_node = e ; term_loc = Lexing.dummy_pos,Lexing.dummy_pos;
    term_type = Ctype ctyp; term_name = []}

(** @plugin development guide *)
let rec expr_to_term e =
  let result = match e with
  | Const c -> TConst c
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
  mk_dummy_term result (Cil.typeOf e)

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

(*TODO: remove the assert false*)
let rec expr_to_tsets_elem = function
  | Info(e,_) -> expr_to_tsets_elem e
  | Const c -> TSConst c
  | SizeOf _ -> assert false
  | SizeOfE _ -> assert false
  | SizeOfStr _ -> assert false
  | StartOf lv -> TSStartOf (lval_to_tsets_lval lv)
  | AddrOf (h, off) ->
      TSAddrOf(host_to_tsets_host h, offset_to_tsets_offset off)
  | CastE (ty,e) ->  TSCastE (ty,expr_to_tsets_elem e)
  | BinOp ((PlusPI|IndexPI), l, r, _) ->
      TSAdd_index(expr_to_tsets_elem l,expr_to_term r)
  | BinOp (MinusPI,l,r,_) ->
      TSAdd_index(expr_to_tsets_elem l,
                  mk_dummy_term(TUnOp(Neg,expr_to_term r)) (Cil.typeOf r))
  | BinOp _ -> assert false
  | UnOp _ -> assert false
  | AlignOfE _ -> assert false
  | AlignOf _ -> assert false
  | Lval lv -> TSLval (lval_to_tsets_lval lv)
and lval_to_tsets_lval (host,offset) =
  host_to_tsets_host host,offset_to_tsets_offset offset
and host_to_tsets_host = function
  | Var s -> TSVar (Cil.cvar_to_lvar s)
  | Mem e ->
      TSMem (expr_to_tsets_elem e)
and offset_to_tsets_offset = function
  | NoOffset -> TSNoOffset
  | Index (e,off) -> TSIndex (expr_to_term e,offset_to_tsets_offset off)
  | Field (fi,off) -> TSField(fi,offset_to_tsets_offset off)

(** {1 Various utilities} *)

let rec remove_term_offset o =
  match o with
      TNoOffset -> TNoOffset, TNoOffset
    | TIndex(_,TNoOffset) | TField(_,TNoOffset)-> TNoOffset, o
    | TIndex(e,o) ->
        let (oth,last) = remove_term_offset o in TIndex(e,oth), last
    | TField(f,o) ->
        let (oth,last) = remove_term_offset o in TField(f,oth), last

let rec term_to_tsets_elem t =
  match t.term_node with
    | TConst c -> TSConst c
    | TLval lv -> TSLval (term_lval_to_tsets_lval lv)
    | TBinOp((PlusPI|IndexPI),l,r) ->
        TSAdd_index(term_to_tsets_elem l, r)
    | TBinOp(MinusPI,l,r) ->
        TSAdd_index(term_to_tsets_elem l,
                    { r with term_node = TUnOp(Neg,r)})
    | TCastE(typ,term) -> TSCastE(typ, term_to_tsets_elem term)
    | TAddrOf lv -> TSAddrOf(term_lval_to_tsets_lval lv)
    | TStartOf lv -> TSStartOf (term_lval_to_tsets_lval lv)
    | Tapp(f,labs,args) -> TSapp(f,labs,args)
    | Told t -> TSat(term_to_tsets_elem t,LogicLabel "old")
    | Tnull -> TSCastE(Cil.voidPtrType,TSConst(CInt64 (Int64.zero,IULong,None)))
    | Tat (t,l) -> TSat(term_to_tsets_elem t, l)
    | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
    | Tlambda _ | TDataCons _ | Tif _ | Tbase_addr _ | Tblock_length _
    | TAlignOf _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TCoerce _ | TCoerceE _
    | TUpdate _ | Ttypeof _ | Ttype _
        ->
        Cil.errorLoc
          t.term_loc "cannot convert term %a to tset element" Cil.d_term t;
          raise Errormsg.Error
    | Ttsets _ -> assert false (* should have been captured before *)

and term_lval_to_tsets_lval (h,o) =
  (term_host_to_tsets_host h, term_offset_to_tsets_offset o)

and term_host_to_tsets_host = function
    TVar v -> TSVar v
  | TResult -> TSResult
  | TMem e -> TSMem (term_to_tsets_elem e)

and term_offset_to_tsets_offset = function
    TNoOffset -> TSNoOffset
  | TField(f,o) -> TSField(f,term_offset_to_tsets_offset o)
  | TIndex(t,o) -> TSIndex(t, term_offset_to_tsets_offset o)

let term_to_tsets t =
  match t.term_node with
      Ttsets set -> set
    | _ -> TSSingleton (term_to_tsets_elem t)

let bound_var v = Cil.make_logic_var v.lv_name v.lv_type

let rec lval_contains_result v =
  match v with
      TResult -> true
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
    | _ -> false

let rec tsets_contains_result = function
    TSSingleton lv ->
      tsets_elem_contains_result lv
  | TSEmpty -> false
  | TSUnion locs -> List.exists tsets_contains_result locs
  | TSInter locs -> List.exists tsets_contains_result locs
  | TSComprehension(t,_,_) -> tsets_contains_result t

and tsets_elem_contains_result = function
  | TSLval lv | TSStartOf lv | TSAddrOf lv -> tsets_lval_contains_result lv
  | TSAdd_index(lh,i) ->
      tsets_elem_contains_result lh || contains_result i
  | TSAdd_range(lh,low,high) ->
      let opt_contains = function None -> false
        | Some t -> contains_result t
      in tsets_elem_contains_result lh || opt_contains low || opt_contains high
  | TSConst _ -> false
  | TSCastE(_,elem) -> tsets_elem_contains_result elem
  | TSat(l,_) -> tsets_elem_contains_result l
  | TSapp (_,_,args) -> List.exists contains_result args

and tsets_lval_contains_result (h,o) =
      tsets_lhost_contains_result h || tsets_offset_contains_result o

and tsets_lhost_contains_result = function
    TSVar _ -> false
  | TSResult -> true
  | TSMem t -> tsets_elem_contains_result t

and tsets_offset_contains_result = function
    TSNoOffset -> false
  | TSIndex (t,o) -> contains_result t || tsets_offset_contains_result o
  | TSRange(low,high,o) ->
      let opt_contains = function None -> false
        | Some t -> contains_result t
      in opt_contains low || opt_contains high || tsets_offset_contains_result o
  | TSField (_,o) -> tsets_offset_contains_result o

(** returns the definition of a predicate.
    @raise Not_found if the predicate is only declared
 *)
let get_pred_body pi =
  match pi.l_body with LBpred p -> p | _ -> raise Not_found

(** true if the given term is a lvalue denoting result or part of it *)
let is_result t = match t.term_node with
    TLval (TResult,_) -> true
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

let rec tsets_elem_is_result = function
  | TSLval(TSResult,_) -> true
  | TSStartOf _ -> false
      (* According to ISO, a function can't return an array *)
  | TSLval _ | TSConst _ -> false
  | TSAddrOf _ -> false
  | TSAdd_range(t,_,_) -> tsets_elem_is_result t
  | TSAdd_index (t,_) -> tsets_elem_is_result t
  | TSCastE(_,elem) -> tsets_elem_is_result elem
  | TSat(l,_) -> tsets_elem_is_result l
  | TSapp _ -> false

(** [true] if all the tsets denotes result or part of it*)
let rec tsets_is_result = function
    TSSingleton ts -> tsets_elem_is_result ts
  | TSEmpty -> false
  | TSUnion locs -> List.for_all tsets_is_result locs
  | TSInter locs -> List.exists tsets_is_result locs
  | TSComprehension(t,_,_) -> tsets_is_result t

let rec is_same_type t1 t2 =
  match t1,t2 with
      Ctype t1, Ctype t2 -> Cilutil.equals (Cil.typeSig t1) (Cil.typeSig t2)
    | Ltype(t1,l1), Ltype(t2,l2) ->
        t1 = t2 && List.for_all2 is_same_type l1 l2
    | Linteger, Linteger -> true
    | Lreal, Lreal -> true
    | Lvar v1, Lvar v2 -> v1 = v2
    | Larrow(args1,rt1), Larrow(args2,rt2) ->
        List.length args1 = List.length args2 &&
        List.for_all2 is_same_type args1 args2 && is_same_type rt1 rt2
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

let  is_same_logic_signature l1 l2 =
  l1.l_name = l2.l_name &&
  is_same_opt is_same_type l1.l_type l2.l_type &&
  is_same_list is_same_string l1.l_tparams l2.l_tparams &&
  is_same_list is_same_var l1.l_profile l2.l_profile &&
  is_same_list is_same_logic_label l1.l_labels l2.l_labels

let is_same_logic_ctor_info ci1 ci2 =
  ci1.ctor_name = ci2.ctor_name &&
  is_same_type ci1.ctor_type ci2.ctor_type &&
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
    | Ttsets tset1, Ttsets tset2 ->
        is_same_tsets tset1 tset2
    | TDataCons(ci1,prms1), TDataCons(ci2,prms2) ->
        is_same_logic_ctor_info ci1 ci2 &&
          is_same_list is_same_term prms1 prms2
    | (TConst _ | TLval _ | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
      | TAlignOf _ | TAlignOfE _ | TUnOp _ | TBinOp _ | TCastE _
      | TAddrOf _ | TStartOf _ | Tapp _ | Tlambda _ | TDataCons _
      | Tif _ | Told _ | Tat _ | Tbase_addr _ | Tblock_length _ | Tnull
      | TCoerce _ | TCoerceE _ | TUpdate _ | Ttypeof _ | Ttype _
      | Ttsets _
      ),_ -> false

and is_same_logic_info l1 l2 =
  is_same_logic_signature l1 l2 &&
  is_same_logic_body l1.l_body l2.l_body

and is_same_logic_body b1 b2 =
  match b1,b2 with
    | LBreads l1, LBreads l2 -> is_same_list is_same_tsets l1 l2
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
    | TResult, TResult -> true
    | (TVar _ | TMem _ | TResult),_ -> false

and is_same_offset o1 o2 =
  match o1, o2 with
      TNoOffset, TNoOffset -> true
    | TField (f1,o1), TField(f2,o2) ->
        f1.fname = f2.fname && is_same_offset o1 o2
    | TIndex(t1,o1), TIndex(t2,o2) ->
        is_same_term t1 t2 && is_same_offset o1 o2
    | (TNoOffset| TField _| TIndex _),_ -> false

and is_same_tsets_lhost h1 h2 =
  match h1,h2 with
      TSVar v1, TSVar v2 -> is_same_var v1 v2
    | TSMem t1, TSMem t2 -> is_same_tsets_elem t1 t2
    | TSResult, TSResult -> true
    | (TSVar _| TSMem _|TSResult), _ -> false

and is_same_tsets_offset o1 o2 =
  match o1,o2 with
      TSNoOffset, TSNoOffset -> true
    | TSIndex(t1,o1), TSIndex(t2,o2) ->
        is_same_term t1 t2 && is_same_tsets_offset o1 o2
    | TSRange(l1,h1,o1), TSRange(l2,h2,o2) ->
        is_same_opt is_same_term l1 l2 && is_same_opt is_same_term h1 h2 &&
          is_same_tsets_offset o1 o2
    | TSField(f1,o1), TSField(f2,o2) ->
        f1.fname = f2.fname && is_same_tsets_offset o1 o2
    | (TSNoOffset | TSIndex _ | TSRange _ | TSField _),_ -> false

and is_same_tsets_lval (h1,o1) (h2,o2) =
  is_same_tsets_lhost h1 h2 && is_same_tsets_offset o1 o2

and is_same_tsets_elem e1 e2 = match e1,e2 with
    TSLval lv1, TSLval lv2 -> is_same_tsets_lval lv1 lv2
  | TSStartOf lv1, TSStartOf lv2 -> is_same_tsets_lval lv1 lv2
  | TSAddrOf lv1, TSAddrOf lv2 -> is_same_tsets_lval lv1 lv2
  | TSConst c1, TSConst c2 -> c1 = c2
  | TSAdd_index(t1,i1), TSAdd_index(t2,i2) ->
      is_same_tsets_elem t1 t2 && is_same_term i1 i2
  | TSAdd_range(t1,low1,high1), TSAdd_range(t2,low2,high2) ->
      is_same_tsets_elem t1 t2 && is_same_opt is_same_term low1 low2 &&
        is_same_opt is_same_term high1 high2
  | TSCastE(t1,e1), TSCastE(t2,e2) ->
        Cilutil.equals (Cil.typeSig t1) (Cil.typeSig  t2) &&
      is_same_tsets_elem e1 e2
  | TSat(t1,l1), TSat(t2,l2) ->
      is_same_tsets_elem t1 t2 && is_same_logic_label l1 l2
  | TSapp(f1,labs1,args1), TSapp(f2, labs2, args2) ->
      (try
        is_same_logic_signature f1 f2 &&
        List.for_all2 (fun l1 l2 -> l1 = l2) labs1 labs2 &&
        List.for_all2 is_same_term args1 args2
       with Invalid_argument _ -> false)
  | (TSLval _ | TSStartOf _ | TSConst _ | TSAdd_index _
    | TSAdd_range _ | TSCastE _ | TSat _ | TSapp _ | TSAddrOf _),_ -> false

and is_same_tsets t1 t2 =
  match t1,t2 with
      TSSingleton t1, TSSingleton t2 -> is_same_tsets_elem t1 t2
    | TSEmpty, TSEmpty -> true
    | (TSUnion l1, TSUnion l2) | (TSInter l1, TSInter l2) ->
        (try List.for_all2 is_same_tsets l1 l2
         with Invalid_argument _ -> false)
    | TSComprehension(e1,q1,p1), TSComprehension(e2,q2,p2) ->
        is_same_tsets e1 e2 &&
          (try List.for_all2 (fun x y -> x.lv_id = y.lv_id) q1 q2
           with Invalid_argument _ -> false) &&
          is_same_opt is_same_named_predicate p1 p2
    | (TSSingleton _ | TSEmpty | TSUnion _ | TSInter _ | TSComprehension _ ),_
        -> false

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
    | Pvalid t1, Pvalid t2 -> is_same_tsets t1 t2
    | Pvalid_index(l1,h1), Pvalid_index(l2,h2) ->
        is_same_term l1 l2 && is_same_term h1 h2
    | Pvalid_range(b1,l1,h1), Pvalid_range(b2,l2,h2) ->
        is_same_term b1 b2 && is_same_term l1 l2 && is_same_term h1 h2
    | Pfresh t1, Pfresh t2 -> is_same_term t1 t2
    | Psubtype(lt1,rt1), Psubtype(lt2,rt2) ->
        is_same_term lt1 lt2 && is_same_term rt1 rt2
    | Pseparated(seps1), Pseparated(seps2) ->
        (try List.for_all2 is_same_tsets seps1 seps2
         with Invalid_argument _ -> false)
    | (Pfalse | Ptrue | Papp _ | Prel _ | Pand _ | Por _ | Pimplies _
      | Piff _ | Pnot _ | Pif _ | Plet _ | Pforall _ | Pexists _
      | Pold _ | Pat _ | Pvalid _ | Pvalid_index _ | Pvalid_range _
      | Pfresh _ | Psubtype _ | Pxor _ | Pseparated _
      ), _ -> false

and is_same_named_predicate pred1 pred2 =
  pred1.name = pred2.name && is_same_predicate pred1.content pred2.content

(*
and is_same_predicate_signature p1 p2 =
  p1.p_name = p2.p_name &&
  is_same_list is_same_var p1.p_profile p2.p_profile &&
  is_same_list is_same_logic_label p1.p_labels p2.p_labels
*)

(*
and is_same_predicate_info p1 p2 =
  is_same_predicate_signature p1 p2 &&
  is_same_predicate_body p1.p_body p2.p_body
*)

(*
and is_same_predicate_body p1 p2 =
  match p1,p2 with
      PReads ts1, PReads ts2 -> is_same_list is_same_tsets ts1 ts2
    | PDefinition p1, PDefinition p2 -> is_same_named_predicate p1 p2
    | (PReads _ | PDefinition _),_ -> false
*)

let is_same_identified_predicate p1 p2 =
  is_same_list (=) p1.ip_name p2.ip_name &&
    is_same_predicate p1.ip_content p2.ip_content

let is_same_identified_tsets l1 l2 =
  is_same_tsets l1.its_content l2.its_content

let is_same_zone z1 z2 = match (z1,z2) with
    (Nothing, Nothing) -> true
  | Location loc1, Location loc2 -> is_same_identified_tsets loc1 loc2
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

let rec is_same_global_annotation ga1 ga2 =
  match (ga1,ga2) with
    | Dfun_or_pred li1, Dfun_or_pred li2 -> is_same_logic_info li1 li2
    | Daxiomatic (id1,ga1), Daxiomatic (id2,ga2) ->
        id1 = id2 && is_same_list is_same_global_annotation ga1 ga2
    | Dtype (t1,l1), Dtype(t2,l2) -> t1 = t2 && is_same_list (=) l1 l2
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
        Cil.warn "found contradictory assign clauses. Keeping \
                         only one";
        old_assigns
    | l1, l2 -> l1 @ l2

(* a discuter ??
let merge_logic_reads old fresh =
  if not (is_same_list is_same_tsets old.l_reads fresh.l_reads) then
    old.l_reads <-
      List.fold_left
      (fun l x -> if List.exists (is_same_tsets x) l then l else x::l)
      old.l_reads fresh.l_reads
*)

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
          Cil.warn
            "found two variants for function specification. Keeping only one."
    end;
    begin match old_spec.spec_terminates, fresh_spec.spec_terminates with
        None, None -> ()
      | Some p1, Some p2 when is_same_identified_predicate p1 p2 -> ()
      | _ ->
          Cil.warn
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
  | TResult -> assert false

let is_assert ca = match ca.annot_content with AAssert _ -> true | _ -> false

let is_assume ca =
  match ca.annot_content with AAssume _ -> true | _ -> false

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

let extract_loop_pragma l =
  List.fold_right
    (fun ca l -> match ca.annot_content with
         APragma (Loop_pragma lp) -> lp::l | _ -> l) l []

(** {2 Parsing utilities} *)

(** C typedefs *)
(**
  -  true => identifier is a type name
  -  false => identifier is a plain identifier
*)
let typenames: (string, bool) Hashtbl.t = Hashtbl.create 13

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

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
