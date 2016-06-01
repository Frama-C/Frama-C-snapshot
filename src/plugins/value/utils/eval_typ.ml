(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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
open Abstract_interp
open Cvalue

let is_bitfield typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt _] -> true
           | _ -> false)
    | _ -> false

let bitfield_size typlv =
  match Cil.unrollType typlv with
  | TInt (_, attrs) | TEnum (_, attrs) ->
    (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
     | [AInt size] -> Some size
     | _ -> None)
  | _ -> None

let sizeof_lval_typ typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) as t ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt i] -> Int_Base.Value i
           | _ -> Bit_utils.sizeof t)
    | t -> Bit_utils.sizeof t

(* TODO: this should probably be also put directly in reinterpret_int *)
let cast_lval_if_bitfield typlv size v =
  match size with
    | Int_Base.Top -> v (* Bitfields have known sizes *)
    | Int_Base.Value size ->
      if is_bitfield typlv then begin
        try
          ignore (V.project_ival_bottom v);
          let signed = Bit_utils.is_signed_int_enum_pointer typlv in
          let v, _ok = Cvalue.V.cast ~size ~signed v in
          v (* TODO: handle not ok case as a downcast *)
        with
        | V.Not_based_on_null (* from [project_ival] *) ->
          (* [v] is a pointer: check there are enough bits in
             the bit-field to contain it. *)
          if Int.ge size (Int.of_int (Bit_utils.sizeofpointer ())) ||
            V.is_imprecise v
          then v
          else begin
            Value_parameters.result
              "casting address to a bit-field of %s bits: \
                this is smaller than sizeof(void*)" (Int.to_string size);
            V.topify_arith_origin v
          end
      end
      else v



let need_cast t1 t2 =
  match Cil.unrollType t1, Cil.unrollType t2 with
    | (TInt _| TEnum _| TPtr _), (TInt _| TEnum _| TPtr _) | TFloat _, TFloat _
      ->
      (try Cil.bitsSizeOf t1 <> Cil.bitsSizeOf t2
       with Cil.SizeOfError _ -> true)
    | TComp (c1, _, _), TComp (c2, _, _) -> c1 != c2
    | _ -> true


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
  try
    ignore (Cabs2cil.compatibleTypes typ_pointed typ_fun); Compatible
  with Failure _ ->
    let compatible_sizes t1 t2 =
      try Cil.bitsSizeOf t1 = Cil.bitsSizeOf t2
      with Cil.SizeOfError _ -> false
    in
    let continue = match Cil.unrollType typ_pointed, Cil.unrollType typ_fun with
      | TFun (ret1, args1, var1, _), TFun (ret2, args2, var2, _) ->
          (* Either both functions are variadic, or none. Otherwise, it
             will be too complicated to make the argument match *)
          var1 = var2 &&
          (* Both functions return something of the same size, or nothing*)
          (match Cil.unrollType ret1, Cil.unrollType ret2 with
             | TVoid _, TVoid _ -> true (* let's avoid relying on the size
                                           of void *)
             | TVoid _, _ | _, TVoid _ -> false
             | t1, t2 -> compatible_sizes t1 t2
          ) &&
          (* Argument lists of the same length, with compatible sizes between
             the arguments, or unspecified argument lists *)
          (match args1, args2 with
             | None, None | None, Some _ | Some _, None -> true
             | Some lp, Some lf ->
               (* See corresponding function fold_left2_best_effort in
                  Function_args *)
               let rec comp lp lf = match lp, lf with
                 | _, [] -> true (* accept too many arguments passed *)
                 | [], _ :: _ -> false (* fail on too few arguments *)
                 | (_, tp, _) :: qp, (_, tf, _) :: qf ->
                   compatible_sizes tp tf && comp qp qf
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
  with Locations.Location_Bytes.Error_Top -> `Top, true


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
  | Field (_, o) -> offset_contains_volatile o
  | Index (e, o) ->
    offset_contains_volatile o || expr_contains_volatile e

