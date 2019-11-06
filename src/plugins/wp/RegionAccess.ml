(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
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

open Cil_types
open Layout
open Region

(* -------------------------------------------------------------------------- *)
(* --- Location Compiler                                                  --- *)
(* -------------------------------------------------------------------------- *)

type addr = {
  addrof : Region.region ;
  typeOfPointed : typ ;
  shift : bool ;
}

type value =
  | Pure
  | Read_at of typ * region
  | Addr_of of addr

[@@@ warning "-32"]
let pp_value fmt = function
  | Pure -> Format.pp_print_string fmt "scalar"
  | Read_at(_,r) -> Format.fprintf fmt "read %a" R.pretty r
  | Addr_of a ->
      if a.shift then
        Format.fprintf fmt "addr %a+" R.pretty a.addrof
      else
        Format.fprintf fmt "addr %a" R.pretty a.addrof
[@@@ warning "+32"]

(* -------------------------------------------------------------------------- *)
(* --- Strings                                                            --- *)
(* -------------------------------------------------------------------------- *)

let cc_string map exp =
  let cst = Pretty_utils.to_string Cil_datatype.Exp.pretty exp in
  let addrof = Region.of_cstring map ~eid:exp.eid ~cst in
  { addrof ; typeOfPointed = Cil.charType ; shift=false }

(* -------------------------------------------------------------------------- *)
(* --- Reading Values                                                     --- *)
(* -------------------------------------------------------------------------- *)

let read acs = function
  | Pure -> ()
  | Addr_of _ -> ()
  | Read_at(tr,r) ->
      acs_deref r (Value,tr) ;
      acs_read r acs

let points_to = function { shift ; addrof = pointed ; typeOfPointed = typ } ->
  acs_deref pointed ((if shift then Array else Deref),typ)

let addrof map = function
  | Pure -> failwith "Wp.Region: physical address"
  | Read_at(tr,r) ->
      acs_deref r (Value,tr) ;
      {
        addrof = add_pointed map r ;
        typeOfPointed = Cil.typeOf_pointed tr ;
        shift = false ;
      }
  | Addr_of addr -> addr

let cast ty value =
  if Cil.isPointerType ty then
    match value with
    | Addr_of addr ->
        Addr_of { addr with typeOfPointed = Cil.typeOf_pointed ty }
    | Read_at (_,r) -> Read_at(ty,r)
    | Pure -> Pure
  else
    value

let is_pointer_value = function
  | Pure -> false
  | Addr_of _ -> true
  | Read_at(tr,_) -> Cil.isPointerType tr

let merge_type t t' =
  if Cil.isVoidType t then t' else
  if Cil.isVoidType t' then t else
  if Cil_datatype.Typ.equal t t' then t
  else failwith "Wp.Region: merge incompatible pointer types"

let merge_addrof (map:map) v1 v2 =
  if not (is_pointer_value v1) then v2 else
  if not (is_pointer_value v2) then v1 else
    let a1 = addrof map v1 in
    let a2 = addrof map v2 in
    let typeOfPointed = merge_type a1.typeOfPointed a2.typeOfPointed in
    let addrof = Region.alias map a1.addrof a2.addrof in
    let shift = a1.shift || a2.shift in
    Addr_of { addrof ; typeOfPointed ; shift }

(* -------------------------------------------------------------------------- *)
(* --- Expressions & L-values                                             --- *)
(* -------------------------------------------------------------------------- *)

let rec cc_exp (map:map) exp =
  match exp.enode with
  | BinOp( (PlusPI | IndexPI | MinusPI) , a , b , _ ) ->
      cc_read map b ;
      let { addrof = pointed } as addr = cc_addr map a in
      acs_shift pointed (Eval exp) ;
      Addr_of { addr with shift = true }
  | AddrOf lv | StartOf lv ->
      Addr_of {
        addrof = cc_lval map lv ;
        typeOfPointed = Cil.typeOfLval lv ;
        shift = false ;
      }
  | Lval lv -> Read_at (Cil.typeOfLval lv , cc_lval map lv)
  | CastE(ty,e) -> cast ty (cc_exp map e)
  | Info(e,_) -> cc_exp map e
  | Const (CStr _ | CWStr _) -> Addr_of (cc_string map exp)
  | Const (CInt64 _ | CChr _ | CEnum _ | CReal _)
  | SizeOf _ | SizeOfE _ | SizeOfStr _
  | AlignOf _ | AlignOfE _ -> Pure
  | UnOp(_,e,ty) ->
      assert (not (Cil.isPointerType ty)) ;
      cc_read map e ; Pure
  | BinOp(_,a,b,ty) ->
      assert (not (Cil.isPointerType ty)) ;
      cc_read map a ; cc_read map b ; Pure

and cc_host map = function
  | Var x -> of_cvar map x , x.vtype
  | Mem e ->
      let a = cc_addr map e in
      points_to a ; (* deref, not read !*)
      a.addrof , a.typeOfPointed

and cc_lval map (host , offset) =
  let r,ty = cc_host map host in cc_offset map r ty offset

and cc_offset map r ty = function
  | Cil_types.NoOffset -> r
  | Cil_types.Field(fd,ofs) ->
      let df = Offset.field fd in
      cc_offset map (add_offset map r df) fd.ftype ofs
  | Cil_types.Index(e,ofs) ->
      cc_read map e ;
      let de = Offset.index ty in
      let te = Offset.typeof de in
      cc_offset map (add_offset map r de) te ofs

and cc_addr map a = addrof map (cc_exp map a)

and cc_read map e = read (Eval e) (cc_exp map e)

and cc_comp map e =
  match cc_exp map e with
  | Pure | Addr_of _ -> failwith "Wp.Region: comp expected"
  | Read_at(_,r) -> r

let cc_writes map stmt tgt typ e =
  acs_deref tgt (Value,typ) ;
  acs_write tgt (Assigned stmt) ;
  match Cil.unrollType typ with
  | TPtr _ ->
      let a = cc_addr map e in
      points_to a ; (* deref, not read! *)
      do_alias map a.addrof (add_pointed map tgt)
  | TComp _ ->
      let src = cc_comp map e in
      acs_copy ~src ~tgt
  | _ ->
      cc_read map e

let cc_assign map stmt lv e =
  cc_writes map stmt (cc_lval map lv) (Cil.typeOfLval lv) e

let cc_return map stmt e =
  cc_writes map stmt (Region.of_return map) (Cil.typeOf e) e

(* -------------------------------------------------------------------------- *)
(* ---  Stmt & Instructions                                               --- *)
(* -------------------------------------------------------------------------- *)

let rec cc_init map stmt lv = function
  | SingleInit e -> cc_assign map stmt lv e
  | CompoundInit(_,content) ->
      List.iter
        (fun (ofs,vi) ->
           cc_init map stmt (Cil.addOffsetLval ofs lv) vi
        ) content

let cc_local_init map stmt x = function
  | AssignInit vi -> cc_init map stmt (Var x,NoOffset) vi
  | ConsInit _ -> failwith "Wp.Region: cons-init not implemented"

let cc_instr map stmt = function
  | Set(lv,e,_) -> cc_assign map stmt lv e
  | Call _ -> failwith "Wp.Region: call not implemented"
  | Local_init(x,vi,_) -> cc_local_init map stmt x vi
  | Asm _ | Skip _ | Code_annot _ -> ()

let cc_skind map stmt =
  match stmt.skind with
  | Instr instr -> cc_instr map stmt instr
  | Return(Some ve,_) -> cc_return map stmt ve
  | If(e,_,_,_) -> cc_read map e
  | Switch(e,_,_,_) -> cc_read map e

  | Return(None,_) | Goto _ | Break _ | Continue _ | Loop _
  | Block _ | UnspecifiedSequence _
  | Throw _ | TryCatch _ | TryFinally _ | TryExcept _ -> ()

(* -------------------------------------------------------------------------- *)
(* --- ACSL Terms                                                         --- *)
(* -------------------------------------------------------------------------- *)

let rec cc_term map t = read (Tval t) (cc_term_value map t)

and cc_term_value (map:map) (term:term) =
  match term.term_node with
  | TLval lv ->
      begin match cc_term_lval map lv with
        | None -> Pure
        | Some(ty,reg) -> Read_at(ty,reg)
      end
  | TAddrOf lv | TStartOf lv ->
      begin match cc_term_lval map lv with
        | None -> failwith "Wp.Region: pure term-value"
        | Some(ty,reg) -> Addr_of {
            addrof = reg ;
            typeOfPointed = ty ;
            shift = false ;
          }
      end
  | TBinOp( (PlusPI | IndexPI | MinusPI) , a , b ) ->
      begin
        cc_term map b ;
        let { addrof = pointed } as addr = cc_term_addr map a in
        acs_shift pointed (Tval term) ;
        Addr_of { addr with shift = true }
      end

  | Tnull ->
      Addr_of {
        addrof = Region.of_null map ;
        typeOfPointed = Cil.charType ;
        shift = false ;
      }

  | TUnOp(_,a) -> cc_term map a ; Pure
  | TBinOp(_,a,b) -> cc_term map a ; cc_term map b ; Pure

  | Tat(t,_) -> cc_term_value map t

  | TCastE(ty,t) -> cast ty @@ cc_term_value map t
  | TLogic_coerce (Ctype ty,t) -> cast ty @@ cc_term_value map t
  | TLogic_coerce (_,t) -> cc_term_value map t

  | TConst _
  | TSizeOf _ | TSizeOfE _ | TSizeOfStr _
  | TAlignOf _ | TAlignOfE _ | Ttype _ | Ttypeof _
    -> Pure

  | TDataCons(_,ts) -> List.iter (cc_term map) ts ; Pure
  | TUpdate(w,ofs,v) ->
      cc_term map w ;
      cc_term map v ;
      cc_term_offset_read map ofs ;
      Pure

  | Tbase_addr(_at,t) -> cast Cil.voidPtrType @@ cc_term_value map t
  | Tblock_length(_at,t) | Toffset(_at,t) -> cc_term map t ; Pure

  | Tif(c,a,b) ->
      cc_term map c ;
      merge_addrof map (cc_term_value map a) (cc_term_value map b)

  | Tempty_set -> Pure
  | Tunion ts | Tinter ts ->
      List.fold_left
        (fun v t -> merge_addrof map v (cc_term_value map t)) Pure ts

  | Tcomprehension(t,_,None) -> cc_term_value map t
  | Tcomprehension(t,_,Some p) -> cc_pred map p ; cc_term_value map t
  | Trange(a,b) -> cc_term_option map a ; cc_term_option map b ; Pure

  | Tlet _ | Tlambda _ | Tapp _ ->
      failwith "Wp.Region: unsupported logic functions and bindings"

and cc_term_lval map (lhost,loffset) =
  match lhost with
  | TResult typ -> Some(typ,of_return map)
  | TVar lvar ->
      begin
        match lvar.lv_origin with
        | Some x ->
            let ty,rv = cc_term_offset map (of_cvar map x) x.vtype loffset in
            Some(ty,rv)
        | None ->
            cc_term_offset_read map loffset ;
            None
      end
  | TMem p ->
      begin
        let a = cc_term_addr map p in
        points_to a ;
        let ty,ra = cc_term_offset map a.addrof a.typeOfPointed loffset in
        Some(ty,ra)
      end

and cc_term_offset map r ty = function
  | TNoOffset -> ty,r
  | TField(fd,ofs) ->
      let df = Offset.field fd in
      cc_term_offset map (add_offset map r df) fd.ftype ofs
  | TIndex(t,ofs) ->
      cc_term map t ;
      let de = Offset.index ty in
      let te = Offset.typeof de in
      cc_term_offset map (add_offset map r de) te ofs
  | TModel _ -> failwith "Wp.Region: model field"

and cc_term_offset_read map = function
  | TNoOffset -> ()
  | TField(_,ofs) -> cc_term_offset_read map ofs
  | TModel(_,ofs) -> cc_term_offset_read map ofs
  | TIndex(t,ofs) -> cc_term map t ; cc_term_offset_read map ofs

and cc_term_addr map t = addrof map @@ cc_term_value map t

and cc_term_option map = function None -> () | Some t -> cc_term map t

(* -------------------------------------------------------------------------- *)
(* --- ACSL Predicates                                                    --- *)
(* -------------------------------------------------------------------------- *)

and cc_pred (map:map) (p:predicate) =
  match p.pred_content with
  | Pfalse | Ptrue -> ()

  | Prel(_,a,b) ->
      cc_term map a ; cc_term map b

  | Pnot a -> cc_pred map a
  | Pif(t,a,b) ->
      cc_term map t ; cc_pred map a ; cc_pred map b
  | Pand(a,b) | Por(a,b) | Pxor(a,b) | Pimplies(a,b) | Piff(a,b) ->
      cc_pred map a ; cc_pred map b

  | Pforall(_,p) | Pexists(_,p) -> cc_pred map p

  | Pseparated ts -> List.iter (cc_term map) ts
  | Pvalid(_,t) | Pvalid_read(_,t) | Pvalid_function t
  | Pinitialized(_,t) | Pdangling(_,t) | Pallocable(_,t)
  | Pfreeable(_,t) -> cc_term map t
  | Pfresh(_,_,ptr,n) -> cc_term map ptr ; cc_term map n

  | Pat(p,_at) -> cc_pred map p

  | Plet _ | Papp _ ->
      failwith "Wp.Region: unsupported logic predicates and bindings"

(* -------------------------------------------------------------------------- *)
(* --- ACSL Spec & Defs                                                   --- *)
(* -------------------------------------------------------------------------- *)

class visitor map =
  object
    inherit Visitor.frama_c_inplace as super

    method! vpredicate p = cc_pred map p ; Cil.SkipChildren
    method! vterm t = cc_term map t ; Cil.SkipChildren
    method! vstmt s = cc_skind map s ; super#vstmt s
    (* vpredicate and vterm are called from vcode_annot *)

    (* speed up: skip non interesting subtrees *)
    method! vloop_pragma _ =  Cil.SkipChildren (* no need *)
    method! vvdec _ = Cil.SkipChildren (* done via stmt *)
    method! vexpr _ = Cil.SkipChildren (* done via stmt *)
    method! vlval _ = Cil.SkipChildren (* done via stmt *)
    method! vattr _ = Cil.SkipChildren (* done via stmt *)
    method! vinst _ =  Cil.SkipChildren (* done via stmt *)
  end

let cc_fundec map def =
  let visitor = new visitor map in
  ignore (Cil.visitCilFunction (visitor:>Cil.cilVisitor) def)

let cc_spec map spec =
  let visitor = new visitor map in
  ignore (Cil.visitCilFunspec (visitor:>Cil.cilVisitor) spec)

(* -------------------------------------------------------------------------- *)
(* --- L-path Iterator                                                    --- *)
(* -------------------------------------------------------------------------- *)

open RegionAnnot

let iter_star map f t r =
  let pointed = add_pointed map r in
  acs_deref pointed (Deref,t) ; f pointed

let iter_shift map f t r =
  let pointed = add_pointed map r in
  acs_deref pointed (Array,t) ; f r

let iter_index map f tarr r =
  f (add_offset map r (Offset.index tarr))

let iter_fields map f fds r =
  List.iter (fun fd -> f (add_offset map r (Offset.field fd))) fds

let rec iter_lpath map f lv =
  match lv.lnode with
  | L_var x -> f (of_cvar map x)
  | L_region a -> f (of_name map a)
  | L_cast(_,a) -> iter_lpath map f a
  | L_addr a -> iter_lpath map (fun r -> f (get_addrof map r)) a
  | L_star(te,a) -> iter_lpath map (iter_star map f te) a
  | L_shift(a,te,_) -> iter_lpath map (iter_shift map f te) a
  | L_index(a,_,_) -> iter_lpath map (iter_index map f lv.ltype) a
  | L_field(a,fs) -> iter_lpath map (iter_fields map f fs) a

(* -------------------------------------------------------------------------- *)
(* --- Region Specs                                                       --- *)
(* -------------------------------------------------------------------------- *)

let cc_lpath map rclass _rpattern lv =
  iter_lpath map (Region.add_alias map ~into:rclass) lv

let cc_region map spec =
  let rclass = Region.of_class map spec.region_name in
  let rpattern = spec.region_pattern in
  List.iter (cc_lpath map rclass rpattern) spec.region_lpath

(* -------------------------------------------------------------------------- *)
