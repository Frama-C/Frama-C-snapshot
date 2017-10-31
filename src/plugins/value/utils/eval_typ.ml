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
open Cvalue

let is_bitfield typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt _] -> true
           | _ -> false)
    | _ -> false

let bitfield_size_attributes attrs =
  match Cil.findAttribute Cil.bitfield_attribute_name attrs with
  | [AInt size] -> Some size
  | _ -> None

let sizeof_lval_typ typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) as t ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt i] -> Int_Base.Value i
           | _ -> Bit_utils.sizeof t)
    | t -> Bit_utils.sizeof t

let offsetmap_matches_type typ_lv o =
  let aux typ_matches = match V_Offsetmap.single_interval_value o with
    | None -> true (* multiple bindings. Assume that type matches *)
    | Some v ->
      let v = V_Or_Uninitialized.get_v v in
      try typ_matches (V.project_ival_bottom v)
      with V.Not_based_on_null -> true (* Do not mess with pointers *)
  in
  let is_float = function
    | Ival.Float _ -> true
    | Ival.Top _ -> false
    | Ival.Set _ as i -> Ival.(equal zero i || equal bottom i)
  in
  let is_int = function
    | Ival.Top _ | Ival.Set _ -> true
    | Ival.Float _ -> false
  in
  match Cil.unrollType typ_lv with
  | TFloat _ -> aux is_float
  | TInt _ | TEnum _ | TPtr _ -> aux is_int
  | _ -> true


type fct_pointer_compatibility =
  | Compatible
  | Incompatible
  | Incompatible_but_accepted

let compatible_functions ~typ_pointed ~typ_fun =
  (* our own notion of weak compatibility:
     - attributes and qualifiers are always ignored
     - all pointers types are considered compatible
     - enums and integer types with the same signedness and size are equal *)
  let weak_compatible t1 t2 =
    Cabs2cil.areCompatibleTypes t1 t2 ||
      match Cil.unrollType t1, Cil.unrollType t2 with
      | TVoid _, TVoid _ -> true
      | TPtr _, TPtr _ -> true
      | (TInt (ik1, _) | TEnum ({ekind = ik1}, _)),
        (TInt (ik2, _) | TEnum ({ekind = ik2}, _)) ->
        Cil.isSigned ik1 = Cil.isSigned ik2 &&
        Cil.bitsSizeOfInt ik1 = Cil.bitsSizeOfInt ik2
      | TFloat (fk1, _), TFloat (fk2, _) -> fk1 = fk2
      | TComp (ci1, _, _), TComp (ci2, _, _) ->
        Cil_datatype.Compinfo.equal ci1 ci2
      | _ -> false
  in
  if Cabs2cil.areCompatibleTypes typ_pointed typ_fun then Compatible
  else
    let continue = match Cil.unrollType typ_pointed, Cil.unrollType typ_fun with
      | TFun (ret1, args1, var1, _), TFun (ret2, args2, var2, _) ->
          (* Either both functions are variadic, or none. Otherwise, it
             will be too complicated to make the argument match *)
          var1 = var2 &&
          (* Both functions return something weakly compatible *)
          weak_compatible ret1 ret2 &&
          (* Argument lists of the same length, with compatible arguments
             or unspecified argument lists *)
          (match args1, args2 with
             | None, None | None, Some _ | Some _, None -> true
             | Some lp, Some lf ->
               (* See corresponding function fold_left2_best_effort in
                  Function_args *)
               let rec comp lp lf = match lp, lf with
                 | _, [] -> true (* accept too many arguments passed *)
                 | [], _ :: _ -> false (* fail on too few arguments *)
                 | (_, tp, _) :: qp, (_, tf, _) :: qf ->
                   weak_compatible tp tf && comp qp qf
               in
               comp lp lf
          )
      | _ -> false
    in
    if continue then Incompatible_but_accepted else Incompatible

let resolve_functions ~typ_pointer v =
  let warn = ref false in
  let aux base offs acc =
    match base with
    | Base.String (_,_) | Base.Null | Base.CLogic_Var _ | Base.Allocated _ ->
      warn := true; acc
    | Base.Var (v,_) ->
      if Cil.isFunctionType v.vtype then (
        if Ival.contains_non_zero offs then warn := true;
        if Ival.contains_zero offs then
          let compatible = compatible_functions typ_pointer v.vtype in
          if compatible <> Compatible then warn := true;
          if compatible = Incompatible then acc
          else Kernel_function.Hptset.add (Globals.Functions.get v) acc
        else (warn := true; acc)
      ) else (warn := true; acc)
  in
  try
    let acc_init = Kernel_function.Hptset.empty in
    let kfs = Locations.Location_Bytes.fold_topset_ok aux v acc_init in
    `Value kfs, !warn
  with Abstract_interp.Error_Top -> `Top, true


let rec expr_contains_volatile expr =
  let rec aux expr = match expr.enode with
    | Lval lval -> lval_contains_volatile lval
    | UnOp (_, e, _) | CastE (_, e) | Info (e, _) -> aux e
    | AddrOf lv | StartOf lv -> lval_contains_volatile lv
    | BinOp (_, e1, e2, _) -> aux e1 || aux e2
    | _ -> false
  in
  aux expr

and lval_contains_volatile (lhost, offset) =
  host_contains_volatile lhost || offset_contains_volatile offset

and host_contains_volatile = function
  | Var vi -> Cil.typeHasQualifier "volatile" vi.vtype
  | Mem e ->
    Cil.typeHasQualifier "volatile" (Cil.(typeOf_pointed (typeOf e))) ||
    expr_contains_volatile e

and offset_contains_volatile = function
  | NoOffset -> false
  | Field (fi, o) -> Cil.typeHasQualifier "volatile" fi.ftype
                     || offset_contains_volatile o
  | Index (e, o) ->
    offset_contains_volatile o || expr_contains_volatile e

let kf_assigns_only_result_or_volatile kf =
  try
    let spec = Annotations.funspec ~populate:false kf in
    let ok behavior = match behavior.b_assigns with
      | Writes l ->
        let aux ({it_content = t}, _) =
          Logic_utils.is_result t ||
          (match Logic_utils.unroll_type t.term_type with
           | Ctype typ ->
             Cil.typeHasQualifier "volatile" typ
           | _ -> false)
        in
        List.for_all aux l
      | WritesAny -> false
    in
    List.for_all ok spec.spec_behavior
  with Annotations.No_funspec _ -> false


(* Scalar types *)

type integer_range = { i_bits: int; i_signed: bool }

module DatatypeIntegerRange =
  Datatype.Make(struct
    include Datatype.Serializable_undefined

    type t = integer_range
    let reprs = [{i_bits = 1; i_signed = true}]
    let name = "Value.Eval_typ.DatatypeIntegerRange"
    let mem_project = Datatype.never_any_project
  end)

let ik_range ik : integer_range =
  { i_bits = Cil.bitsSizeOfInt ik; i_signed = Cil.isSigned ik }

let ik_attrs_range ik attrs =
  let i_bits =
    match bitfield_size_attributes attrs with
    | None -> Cil.bitsSizeOfInt ik
    | Some size -> Integer.to_int size
  in
  { i_bits; i_signed = Cil.isSigned ik }

let range_inclusion r1 r2 =
  match r1.i_signed, r2.i_signed with
  | true, true -> let r = r1.i_bits <= r2.i_bits in (r, r)
  | false, false -> (true,  r1.i_bits <= r2.i_bits)
  | true, false ->  (false, r1.i_bits <= r2.i_bits+1)
  | false, true ->  (true,  r1.i_bits <= r2.i_bits-1)

let range_lower_bound r =
  if r.i_signed then Cil.min_signed_number r.i_bits else Integer.zero

let range_upper_bound r =
  if r.i_signed
  then Cil.max_signed_number r.i_bits
  else Cil.max_unsigned_number r.i_bits


type scalar_typ =
  | TSInt of integer_range
  | TSPtr of integer_range
  | TSFloat of fkind
  | TSNotScalar

let classify_as_scalar typ =
  match Cil.unrollType typ with
  | TInt (ik, attrs) | TEnum ({ekind=ik}, attrs) ->
    TSInt (ik_attrs_range ik attrs)
  | TPtr _ ->
     TSPtr { i_bits = Cil.bitsSizeOfInt Cil.theMachine.Cil.upointKind;
             i_signed = Cil.isSigned Cil.theMachine.Cil.upointKind }
  | TFloat (fk, _) -> TSFloat fk
  | _ -> TSNotScalar

let need_cast t1 t2 =
  match classify_as_scalar t1, classify_as_scalar t2 with
  | (TSInt ir1 | TSPtr ir1), (TSInt ir2 | TSPtr ir2) -> ir1 <> ir2
  | TSFloat fk1, TSFloat fk2 -> fk1 <> fk2
  | (TSInt _ | TSPtr _ | TSFloat _), (TSInt _ | TSPtr _ | TSFloat _) -> true
  | _ -> false
