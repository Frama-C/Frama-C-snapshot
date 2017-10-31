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

let backward_int_relation op v1 v2 =
  let v1' = V.backward_comp_int_left op v1 v2 in
  let op' = Abstract_interp.Comp.sym op in
  let v2' = V.backward_comp_int_left op' v2 v1 in
  if Value_parameters.UndefinedPointerComparisonPropagateAll.get ()
  && not (Cvalue_forward.are_comparable op v1 v2)
  then begin
    if not (Cvalue.V.equal v1 v1' || Cvalue.V.is_bottom v1') then
      Value_parameters.result
        ~current:true ~once:true
        ~dkey:Value_parameters.dkey_pointer_comparison
        "not reducing %a to %a because of UPCPA" V.pretty v1 V.pretty v1';
    if not (Cvalue.V.equal v2 v2' || Cvalue.V.is_bottom v2') then
      Value_parameters.result
        ~current:true ~once:true
        ~dkey:Value_parameters.dkey_pointer_comparison
        "not reducing %a to %a because of UPCPA" V.pretty v2 V.pretty v2';
    None
  end
  else Some (v1', v2')

let backward_float_relation round fkind op v1 v2 =
  let v1' = V.backward_comp_float_left op round fkind v1 v2 in
  let op' = Abstract_interp.Comp.sym op in
  let v2' = V.backward_comp_float_left op' round fkind v2 v1 in
  Some (v1', v2')

let backward_relation t =
  match Cil.unrollType t with
  | TInt _ | TEnum _ | TPtr _ -> backward_int_relation
  | TFloat (fk, _) ->
    backward_float_relation
      (Value_parameters.AllRoundingModes.get ()) (Value_util.float_kind fk)
  | _ -> assert false (* should never occur anyway *)

let backward_shift_rhs typ_e1 v1 v2 =
  let size = Cil.bitsSizeOf typ_e1 in
  let size_int = Integer.pred (Integer.of_int size) in
  let valid_range_rhs =
    V.inject_ival (Ival.inject_range (Some Integer.zero) (Some size_int))
  in
  if V.is_included v2 valid_range_rhs
  then None
  else Some (v1, V.narrow v2 valid_range_rhs)

let backward_shift_left typ_e1 v1 v2 =
  let res = backward_shift_rhs typ_e1 v1 v2 in
  let warn_negative =
    Value_parameters.WarnLeftShiftNegative.get() &&
    Bit_utils.is_signed_int_enum_pointer typ_e1
  in
  if warn_negative then
    let v1, v2 = match res with
      | None -> v1, v2
      | Some (v1, v2) -> v1, v2
    in
    let valid_range_lhs =
      V.inject_ival (Ival.inject_range (Some Integer.zero) None)
    in
    if V.is_included v1 valid_range_lhs
    then res
    else Some (V.narrow v1 valid_range_lhs, v2)
  else res

(* res == v1 +/- v2 *)
let backward_add_int typ ~res_value ~v1 ~v2 pos =
  (* v1 == res -/+ v2 *)
  let v1' = V.add_untyped Int_Base.(if pos then minus_one else one) res_value v2
  (* +/- v2 == res - v1 *)
  and v2' =
    if pos
    then V.add_untyped Int_Base.minus_one res_value v1
    else V.add_untyped Int_Base.minus_one v1 res_value
  in
  (* TODO: no need for reinterpret if no overflow. *)
  Some (Cvalue_forward.unsafe_reinterpret typ v1',
        Cvalue_forward.unsafe_reinterpret typ v2')

let backward_add_float fk ~res_value ~v1 ~v2 pos =
  try
    let f = Ival.project_float (V.project_ival res_value) in
    let f_1ulp = Fval.enlarge_1ulp (Value_util.float_kind fk) f in
    let res_1ulp = V.inject_ival (Ival.inject_float f_1ulp) in
    let round = Value_util.get_rounding_mode () in
    (* TODO: round *)
    let add op v1 v2 =
      fst (Cvalue_forward.forward_binop_float round v1 op v2)
    in
    let v1' = add (if pos then MinusA else PlusA) res_1ulp v2
    and v2' =
      if pos
      then add MinusA res_1ulp v1
      else add MinusA v1 res_1ulp
    in
    Some (v1', v2')
  with V.Not_based_on_null | Ival.Nan_or_infinite | Fval.Non_finite ->
    None


(* Backward reduction for (v1 +/- size * v2 == res),
   where the non-null bases of v1 and v2 cannot interfere with each other,
   and where res is not a garbled-mix.
   Decompose each operand into its integer part and its address part.
   As address part cannot interfere, new integer parts are computed pointwise,
   and new addresses are computed from the integer part of the other side. *)
let unsafe_backward_add_ptr size ~res_value ~v1 ~v2 pos =
  let scale = Int_Base.project size in
  let i1 = V.find Base.null v1 in
  (* Compute the reduced value for v2 = (+/- (res - v1)) / size. *)
  let i2', _ =
    if pos
    then V.sub_untyped_pointwise res_value v1
    else V.sub_untyped_pointwise v1 res_value
  in
  let i2' = Ival.scale_div ~pos:false scale i2' in
  let p2' =
    (* If the operation is v1 + v2, the offset v2 may be a precise pointer.
       Otherwise, we would be multiplying an address by a constant, which makes
       no sense for pointer arithmetics. *)
    if (Int_Base.equal size Int_Base.one && pos)
    || (Int_Base.equal size Int_Base.minus_one && not pos)
    then
      let factor = Int_Base.minus_one in
      if pos
      then V.add_untyped ~factor res_value (Cvalue.V.inject_ival i1)
      else V.add_untyped ~factor (Cvalue.V.inject_ival i1) res_value
    else
      (* Otherwise, the offset may be a garbled mix, if v1 can be an integer. *)
    if V.is_imprecise v2 && not (Ival.is_bottom i1)
    then v2
    else V.bottom
  in
  let v2' = V.add Base.null i2' p2' in
  let v2 = Cvalue.V.narrow v2 v2' in
  (* Compute the reduced value for v1 = res +/- size * v2. *)
  let i2 = V.find Base.null v2 in
  let factor = if pos then size else Int_Base.neg size in
  let i1', _ = V.sub_untyped_pointwise ~factor res_value v2 in
  let factor = if pos then Int_Base.neg size else size in
  let p1' = V.add_untyped ~factor res_value (Cvalue.V.inject_ival i2) in
  let v1 = V.add Base.null i1' p1' in
  v1, v2


(* v1 +/- v2 == res *)
let backward_add_ptr typ ~res_value ~v1 ~v2 pos =
  (* Remove non-null bases from v1 and v2 which don't appear in the result nor
     the other operand. *)
  let reduced = ref false in
  let remove_lonely_bases ~other:v' v =
    let test base =
      let b =
        Base.is_null base || V.may_reach base v' || V.may_reach base res_value
      in
      if not b then reduced := true;
      b
    in
    try V.filter_base test v
    with Abstract_interp.Error_Top -> v
  in
  let v1 = remove_lonely_bases ~other:v2 v1
  and v2 = remove_lonely_bases ~other:v1 v2 in
  let default = if !reduced then Some (v1, v2) else None in
  (* If the result is imprecise, or the size is zero or top, no more reduction
     is possible. *)
  let size = Bit_utils.osizeof_pointed typ in
  if Int_Base.is_zero size || Int_Base.is_top size || V.is_imprecise res_value
  then default
  else
    match v1, v2 with
    | V.Map _, V.Map _ ->
      Some (unsafe_backward_add_ptr size ~res_value ~v1 ~v2 pos)
    | (V.Top _ as t), V.Map m
    | V.Map m, (V.Top _ as t) ->
      (* If the garbled mix contains some non-null base of the map,
         no further reduction. *)
      let intersects =
        V.M.fold
          (fun b _ acc -> acc || (not (Base.is_null b) && V.may_reach b t))
          m false
      in
      if intersects
      then default
      else Some (unsafe_backward_add_ptr size ~res_value ~v1 ~v2 pos)
    | V.Top _, V.Top _ -> default

let convert default = function
  | `Bottom -> V.bottom
  | `Value None -> default
  | `Value (Some v) -> v

(* Correct only when no overflow occurs. *)
let _backward_mult typ v1 v2 res_value =
  let result = res_value in
  let v1' = Cvalue.V.backward_mult_int_left ~right:v2 ~result
  and v2' = Cvalue.V.backward_mult_int_left ~right:v1 ~result in
  let v1 = convert v1 v1'
  and v2 = convert v2 v2' in
  Cvalue_forward.unsafe_reinterpret typ v1,
  Cvalue_forward.unsafe_reinterpret typ v2

let backward_band ~v1 ~v2 ~res typ =
  let size = Cil.bitsSizeOf typ in
  let signed = Bit_utils.is_signed_int_enum_pointer typ in
  let bitwise_and = V.bitwise_and ~size ~signed in
  (* Reduction of a when a & b = res. *)
  let backward_band_aux a b =
    (* For each bit, if a & _ = 1 then a = 1. [a1] is [a] with all such bits at 1
       (for the others, res = 0 and this bitwise_or has no effect on a). *)
    let a1 = V.bitwise_or res a in
    (* For each bit, if a & 1 = 0 then a = 0. [a2] is [a] with all such bits at 0
       (for the others, not (xor res b) = 1 and this bitwise_and has no effect
       on a). *)
    let a2 =
      bitwise_and a (V.bitwise_not_size ~size ~signed (V.bitwise_xor res b))
    in
    V.narrow a1 a2
  in
  backward_band_aux v1 v2, backward_band_aux v2 v1

let backward_bor ~v1 ~v2 ~res typ =
  let size = Cil.bitsSizeOf typ in
  let signed = Bit_utils.is_signed_int_enum_pointer typ in
  let bitwise_and = V.bitwise_and ~size ~signed in
  (* Reduction of a when a | b = res. *)
  let backward_bor_aux a b =
    (* For each bit, if a | _ = 0 then a = 0. [a1] is [a] with all such bits at 0
       (for the others, res = 1 and this bitwise_and has no effect on a). *)
    let a1 = bitwise_and res a in
    (* For each bit, if a | 0 = 1 then a = 1. [a2] is [a] with all such bits at 1
       (for the others, xor res b = 0 and this bitwise_or has no effect on a). *)
    let a2 = V.bitwise_or (V.bitwise_xor res b) a in
    V.narrow a1 a2
  in
  backward_bor_aux v1 v2, backward_bor_aux v2 v1

let backward_binop ~typ_res ~res_value ~typ_e1 v1 binop v2 =
  let typ = Cil.unrollType typ_res in
  match binop, typ with
  | PlusA, TInt _ ->  backward_add_int typ ~res_value ~v1 ~v2 true
  | MinusA, TInt _ -> backward_add_int typ ~res_value ~v1 ~v2 false

  | PlusA, TFloat (fk, _) ->  backward_add_float fk ~res_value ~v1 ~v2 true
  | MinusA, TFloat (fk, _) -> backward_add_float fk ~res_value ~v1 ~v2 false

  | (PlusPI | IndexPI), TPtr _ -> backward_add_ptr typ ~res_value ~v1 ~v2 true
  | MinusPI, TPtr _ ->            backward_add_ptr typ ~res_value ~v1 ~v2 false

  | MinusPP, TInt _ ->
    let factor = Bit_utils.osizeof_pointed typ_e1 in
    let v1 = V.add_untyped factor v2 res_value
    and v2 = V.add_untyped (Int_Base.neg factor) v1 res_value in
    Some (v1, v2)

  (* comparison operators *)
  | (Eq | Ne | Le | Lt | Ge | Gt), _ -> begin
      let binop = Value_util.conv_comp binop in
      let inv_binop = Abstract_interp.Comp.inv binop in
      match V.is_included V.singleton_zero res_value,
            V.is_included V.singleton_one  res_value with
      | true, true  ->
        (* comparison evaluates to {0;1}, cannot reduce *)
        None
      | false, true ->
        (* comparison relation holds *)
        backward_relation (Cil.unrollType typ_e1) binop v1 v2
      | true, false ->
        (* comparison relation does not hold *)
        backward_relation (Cil.unrollType typ_e1) inv_binop v1 v2
      | _ ->
        assert false (* bottom *)
    end

  | (Shiftrt | Shiftlt), TFloat _ -> None
  | Shiftrt, _ -> backward_shift_rhs typ_e1 v1 v2
  | Shiftlt, _ -> backward_shift_left typ_e1 v1 v2

  | Mod, TInt _ ->
    (* the following equality only holds when v1 does not change sign, which
       is why we split its range: v1 == (v1 / v2) * v2 + res *)
    let v1' v1 res =
      V.add_untyped Int_Base.one res (V.mul (V.div v1 v2) v2)
    in
    let ge = Abstract_interp.Comp.Ge and le = Abstract_interp.Comp.Le in
    let v1_pos =  V.backward_comp_int_left ge v1 V.singleton_zero in
    let res_pos = V.backward_comp_int_left ge res_value V.singleton_zero in
    let v1'_pos = v1' v1_pos res_pos in
    let v1_neg =  V.backward_comp_int_left le v1 V.singleton_zero in
    let res_neg = V.backward_comp_int_left le res_value V.singleton_zero in
    let v1'_neg = v1' v1_neg res_neg in
    let v1' = V.join v1'_pos v1'_neg in
    (* v2 *)
    let v2' =
      if V.intersects v1 res_value then
        (* v1 % v2 == v1 => always true for v2 farthest from 0 than v1 *)
        v2
      else
        (* v2 = (v1 - res) / (v1 / v2) *)
        V.div
          (V.add_untyped Int_Base.minus_one v1 res_value)
          (V.div  v1 v2)
    in
    Some (v1', v2')

(*
  | Mult, TInt _ -> Some (backward_mult typ v1 v2 res_value)
*)

  | BAnd, TInt _ -> Some (backward_band ~v1 ~v2 ~res:res_value typ)

  | BOr, TInt _ -> Some (backward_bor ~v1 ~v2 ~res:res_value typ)

  | _, _ -> None

let backward_unop ~typ_arg op ~arg:_ ~res =
  match op with
  | LNot -> None (* handled by the generic mechanism *)
  | BNot -> None (* No real idea of what should be done *)
  | Neg ->
    try
      let v = V.project_ival res in
      if Cil.isIntegralType typ_arg then
        Some (V.inject_ival (Ival.neg_int v))
      else begin
        assert (Cil.isFloatingType typ_arg);
        let f = Ival.project_float v in
        Some (V.inject_ival (Ival.inject_float (Fval.neg f)))
      end
    with V.Not_based_on_null | Ival.Nan_or_infinite -> None

(* ikind of an (unrolled) integer type *)
let ikind = function
  | TInt (ik, _) | TEnum ({ekind = ik}, _) -> ik
  | TPtr _ -> Cil.(theMachine.upointKind)
  | _ -> assert false

(* does [v] fits in the integer type corresponding to [ik]? *)
let fits_in_ikind ik v =
  let size = Cil.bitsSizeOfInt ik in
  let signed = Cil.isSigned ik in
  let all_values = V.create_all_values ~size ~signed in
  V.is_included v all_values

let cast_float_to_double_inverse v =
  try
    let i = V.project_ival v in
    let f = Ival.project_float i in
    let f' = Fval.cast_float_to_double_inverse f in
    Some (V.inject_ival (Ival.inject_float f'))
  with V.Not_based_on_null | Ival.Nan_or_infinite ->
    None

let downcast_enabled ~ik_src ~ik_dst =
  if Cil.isSigned ik_dst
  then
    Kernel.SignedDowncast.get () ||
    (* In this case, -val-warn-signed-converted-downcast behaves exactly
       as -warn-signed-downcast *)
    (Cil.isSigned ik_src && Value_parameters.WarnSignedConvertedDowncast.get ())
  else Kernel.UnsignedDowncast.get ()

(* see .mli *)
let backward_cast ~src_typ ~dst_typ ~src_val ~dst_val =
  (*  Kernel.result "%a %a %a %a" Printer.pp_typ src_typ Printer.pp_typ dst_typ
      V.pretty src_val V.pretty dst_val; *)
  match dst_typ, src_typ with
  | (TInt _  | TEnum _ | TPtr _), (TInt _  | TEnum _ | TPtr _) ->
    let ik_dst = ikind dst_typ in
    let ik_src = ikind src_typ in
    if Cil.intTypeIncluded ik_src ik_dst (*the cast is statically the identity*)
    || downcast_enabled ~ik_src ~ik_dst (* the cast may not be the identity, but
                                           the alarms on downcasts ensure that [src_val] must fit in [dst_typ] *)
    || fits_in_ikind ik_dst src_val (* the cast is dynamically the identity*)
    then
      (* in each case, the cast to [dst_typ] is the identity on [src_val]*)
      Some dst_val
    else None

  | TFloat (fk_dst, _), TFloat (fk_src, _) -> begin
      let f_dst = Value_util.float_kind fk_dst in
      let f_src = Value_util.float_kind fk_src in
      match f_dst, f_src with
      | Fval.Float64, Fval.Float64 | Fval.Float32, Fval.Float32 ->
        (* dummy cast *)
        Some (V.narrow src_val dst_val)
      | Fval.Float32, Fval.Float64 ->
        (* dst_val is in float32, hence less precise than src_val (or the
           expected result) that are (and should be) in double. Simply
           intersect both ranges, which will result in a float32. *)
        Some dst_val
      | Fval.Float64, Fval.Float32 ->
        (* Cannot intersect, because this may create double bounds while
           the result should be in float32. First, find the float32 that can
           correspond to [dst_val], then refine [src_val] accordingly *)
        cast_float_to_double_inverse dst_val
    end

  | TInt _, TFloat (fkind, _) ->
    let single_precision = fkind = FFloat in
    V.cast_float_to_int_inverse ~single_precision dst_val

  | TFloat (fkind, _), TInt _ ->
    let single_precision = fkind = FFloat in
    V.cast_int_to_float_inverse ~single_precision dst_val

  | _ -> None


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
