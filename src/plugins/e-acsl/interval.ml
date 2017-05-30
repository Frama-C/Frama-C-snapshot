(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
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

(* Implement Figure 3 of J. Signoles' JFLA'15 paper "Rester statique pour
   devenir plus rapide, plus précis et plus mince". *)

exception Not_an_integer

(* ********************************************************************* *)
(* Basic datatypes and operations *)
(* ********************************************************************* *)

(* constructors *)

let singleton_of_int n = Ival.inject_singleton (Integer.of_int n)

let rec interv_of_typ ty = match Cil.unrollType ty with
  | TInt (k,_) as ty ->
    let n = Cil.bitsSizeOf ty in
    let l, u =
      if Cil.isSigned k then Cil.min_signed_number n, Cil.max_signed_number n
      else Integer.zero, Cil.max_unsigned_number n
    in
    Ival.inject_range (Some l) (Some u)
  | TEnum(enuminfo, _) -> interv_of_typ (TInt (enuminfo.ekind, []))
  | _ ->
    raise Not_an_integer

let interv_of_unknown_block =
  (* since we have no idea of the size of this block, we take the largest
     possible one which is unfortunately quite large *)
  lazy
    (Ival.inject_range (Some Integer.zero) (Some (Bit_utils.max_byte_address ())))

(* imperative environments *)

module Env = struct
  open Cil_datatype
  let tbl: Ival.t Logic_var.Hashtbl.t = Logic_var.Hashtbl.create 7
  let clear () = Logic_var.Hashtbl.clear tbl
  let add = Logic_var.Hashtbl.add tbl
  let find = Logic_var.Hashtbl.find tbl
end

(* ********************************************************************* *)
(* Main algorithm *)
(* ********************************************************************* *)

let infer_sizeof ty =
  try singleton_of_int (Cil.bytesSizeOf ty)
  with Cil.SizeOfError _ -> interv_of_typ Cil.theMachine.Cil.typeOfSizeOf

let infer_alignof ty = singleton_of_int (Cil.bytesAlignOf ty)

let rec infer t =
  let get_cty t = match t.term_type with Ctype ty -> ty | _ -> assert false in
  match t.term_node with
  | TConst (Integer (n,_)) -> Ival.inject_singleton n
  | TConst (LChr c) ->
    let n = Cil.charConstToInt c in
    Ival.inject_singleton n
  | TConst (LEnum enumitem) ->
    let rec find_idx n = function
      | [] -> assert false
      | ei :: l -> if ei == enumitem then n else find_idx (n + 1) l
    in
    let n = Integer.of_int (find_idx 0 enumitem.eihost.eitems) in
    Ival.inject_singleton n
  | TLval lv -> infer_term_lval lv
  | TSizeOf ty -> infer_sizeof ty
  | TSizeOfE t -> infer_sizeof (get_cty t)
  | TSizeOfStr str -> singleton_of_int (String.length str + 1 (* '\0' *))
  | TAlignOf ty -> infer_alignof ty
  | TAlignOfE t -> infer_alignof (get_cty t)

  | TUnOp (Neg, t) ->
    Ival.neg_int (infer t)
  | TUnOp (BNot, t) ->
    Ival.bitwise_not (infer t)
  | TUnOp (LNot, _)

  | TBinOp ((Lt | Gt | Le | Ge | Eq | Ne | LAnd | LOr), _, _) ->
    Ival.zero_or_one
  | TBinOp (PlusA, t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.add_int i1 i2
  | TBinOp (MinusA, t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.sub_int i1 i2
  | TBinOp (Mult, t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.mul i1 i2
  | TBinOp (Div, t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.div i1 i2
  | TBinOp (Mod, t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.c_rem i1 i2
  | TBinOp (Shiftlt , _, _) -> Error.not_yet "right shift"
  | TBinOp (Shiftrt , _, _) -> Error.not_yet "left shift"
  | TBinOp (BAnd, _, _) -> Error.not_yet "bitwise and"
  | TBinOp (BXor, t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.bitwise_xor i1 i2
  | TBinOp (BOr, t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.bitwise_or i1 i2
  | TCastE (ty, t)
  | TCoerce (t, ty) ->
    (try
       let it = infer t in
       let ity = interv_of_typ ty in
       Ival.meet it ity
     with Not_an_integer ->
       if Cil.isIntegralType ty then begin
         (* heterogeneous cast from a non-integral term to an integral type:
            consider that one eventually gets an integral type even if it is
            not sure. *)
         Options.warning
           ~once:true "possibly unsafe cast from term '%a' to typ '%a'."
           Printer.pp_term t
           Printer.pp_typ ty;
         interv_of_typ ty
       end else
         raise Not_an_integer)
  | Tif (_, t2, t3) ->
    let i2 = infer t2 in
    let i3 = infer t3 in
    Ival.join i2 i3
  | Tat (t, _) -> infer t
  | TBinOp (MinusPP, t, _) ->
    (match Cil.unrollType (get_cty t) with
     | TArray(_, _, { scache = Computed n (* size in bits *) }, _) ->
       (* the second argument must be in the same block than [t]. Consequently
          the result of the difference belongs to [0; \block_length(t)] *)
       let nb_bytes = if n mod 8 = 0 then n / 8 else n / 8 + 1 in
       Ival.inject_range (Some Integer.zero) (Some (Integer.of_int nb_bytes))
     | TArray _ | TPtr _ -> Lazy.force interv_of_unknown_block
     | _ -> assert false)
  | Tblock_length (_, t)
  | Toffset(_, t) ->
    (match Cil.unrollType (get_cty t) with
     | TArray(_, _, { scache = Computed n (* size in bits *) }, _) ->
       let nb_bytes = if n mod 8 = 0 then n / 8 else n / 8 + 1 in
       singleton_of_int nb_bytes
     | TArray _ | TPtr _ -> Lazy.force interv_of_unknown_block
     | _ -> assert false)
  | Tnull  -> singleton_of_int 0
  | TLogic_coerce (_, t) -> infer t
  | TCoerceE (t1, t2) ->
    let i1 = infer t1 in
    let i2 = infer t2 in
    Ival.meet i1 i2

  | Tapp (li, _, _args) ->
    (match li.l_type with
    | None -> assert false (* [None] only possible for predicates *)
    | Some Linteger -> Error.not_yet "logic function returning an integer"
    | Some (Ctype ty) -> interv_of_typ ty
    | Some _ -> raise Not_an_integer)
  | Tunion _ -> Error.not_yet "tset union"
  | Tinter _ -> Error.not_yet "tset intersection"
  | Tcomprehension (_,_,_) -> Error.not_yet "tset comprehension"
  | Trange (_,_) -> Error.not_yet "trange"
  | Tlet (_,_) -> Error.not_yet "let binding"

  | TConst (LStr _ | LWStr _ | LReal _)
  | TBinOp (PlusPI,_,_)
  | TBinOp (IndexPI,_,_)
  | TBinOp (MinusPI,_,_)
  | TAddrOf _
  | TStartOf _
  | Tlambda (_,_)
  | TDataCons (_,_)
  | Tbase_addr (_,_)
  | TUpdate (_,_,_)
  | Ttypeof _
  | Ttype _
  | Tempty_set  -> raise Not_an_integer

and infer_term_lval (host, offset as tlv) =
  match offset with
  | TNoOffset -> infer_term_host host
  | _ ->
    let ty = Logic_utils.logicCType (Cil.typeOfTermLval tlv) in
    interv_of_typ ty

and infer_term_host = function
  | TVar v ->
    (try Env.find v
     with Not_found -> interv_of_typ (Logic_utils.logicCType v.lv_type))
  | TResult ty -> interv_of_typ ty
  | TMem t ->
    let ty = Logic_utils.logicCType t.term_type in
    match Cil.unrollType ty with
    | TPtr(ty, _) | TArray(ty, _, _, _) -> interv_of_typ ty
    | _ ->
      Options.fatal "unexpected type %a for term %a"
        Printer.pp_typ ty
        Printer.pp_term t

(*
Local Variables:
compile-command: "make"
End:
 *)
