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

open Eval
open Cvalue
open Cil_types

let return v = v, Alarmset.none

(* --------------------------------------------------------------------------
                               Comparison
   -------------------------------------------------------------------------- *)

(* Literal strings can only be compared if their contents are recognizably
   different (or the strings are physically the same). *)
let are_comparable_string pointer1 pointer2 =
  try
    Locations.Location_Bytes.iter_on_strings ~skip:None
      (fun base1 s1 offs1 len1 ->
         Locations.Location_Bytes.iter_on_strings ~skip:(Some base1)
           (fun _ s2 offs2 len2 ->
              let delta = offs1 - offs2 in
              let start = if delta <= 0 then -delta else 0
              and max = min len2 (len1 - delta) in
              let length = max - start + 1 in
              let sub1 = String.sub s1 (start + delta) length
              and sub2 = String.sub s2 start length in
              if String.compare sub1 sub2 = 0
              then raise Not_found)
           pointer1)
      pointer2;
    true
  with
  | Not_found -> false
  | Invalid_argument _s -> assert false

(* Under-approximation of the fact that a pointer is actually correct w.r.t.
   what can be created through pointer arithmetics. See C99 6.5.6 and 6.5.8
   for the definition of possible pointers, and in particular the definition
   of "one past". Value does not currently check that all pointers are
   possible, but flags impossible ones using pointer_comparable alarms when
   performing a comparison.

   In practice, function pointers are considered possible or one past
   when their offset is 0. For object pointers, the offset is checked
   against the validity of each base, taking past-one into account. *)
let possible_pointer ~one_past location =
  let location = Locations.loc_bytes_to_loc_bits location in
  let is_possible_offset base offs =
    if Base.is_function base then
      Ival.is_zero offs
    else
      let size = if one_past then Integer.zero else Integer.one in
      Base.is_valid_offset ~for_writing:false size base offs
  in
  Locations.Location_Bits.for_all is_possible_offset location

(* Are [ev1] and [ev2] safely comparable, or does their comparison involves
   invalid pointers, or is undefined (typically pointers in different bases). *)
let are_comparable_reason op ev1 ev2 =
  let open Locations in
  (* If both of the operands have arithmetic type, the comparison is valid. *)
  if Location_Bytes.is_included ev1 Location_Bytes.top_int
  && Location_Bytes.is_included ev2 Location_Bytes.top_int
  then true, `Ok
  else
    let null_1, rest_1 = Location_Bytes.split Base.null ev1
    and null_2, rest_2 = Location_Bytes.split Base.null ev2 in
    (* Note that here, rest_1 and rest_2 cannot be both bottom. *)
    let is_bottom1 = Location_Bytes.is_bottom rest_1
    and is_bottom2 = Location_Bytes.is_bottom rest_2 in
    let arith_compare_ok, reason =
      if op = Abstract_interp.Comp.Eq || op = Abstract_interp.Comp.Ne
      then
        (* A pointer can be compared to a null pointer constant
           by equality operators. *)
        if (Ival.is_included null_1 Ival.zero || is_bottom2)
        && (Ival.is_included null_2 Ival.zero || is_bottom1)
        then true, `Ok
        else false, `Eq_Different_bases_including_null
      else
        (* Pointers cannot be compared to arithmetic values by
           relational operators. *)
      if Ival.is_bottom null_1 && Ival.is_bottom null_2
      then true, `Ok
      else false, `Rel_Different_bases_including_null
    in
    if not arith_compare_ok
    then false, reason
    else
      (* Both pointers have to be almost valid (they can be pointers to one past
         an array object. *)
    if (not (possible_pointer ~one_past:true rest_1)) ||
       (not (possible_pointer ~one_past:true rest_2))
    then false, `Invalid_pointer
    else
      (* Equality operators allow the comparison between an almost valid pointer
         and the null pointer (other cases where is_bottom1 or is_bottom2 have
         been managed by arith_compare_ok). *)
    if is_bottom1 || is_bottom2
    then true, `Ok
    else
      (* If both pointers point to the same base, the comparison is valid. *)
      let single_base_ok =
        try
          let base_1, _ = Location_Bytes.find_lonely_key rest_1
          and base_2, _ = Location_Bytes.find_lonely_key rest_2 in
          Base.equal base_1 base_2
        with Not_found -> false
      in
      if single_base_ok
      then true, `Ok
      else if not (op = Abstract_interp.Comp.Eq || op = Abstract_interp.Comp.Ne)
      (* For relational operators, the comparison of pointers on different
         bases is undefined. *)
      then false, `Rel_different_bases
      else
        (* If both addresses are valid, they can be compared for equality. *)
      if (possible_pointer ~one_past:false rest_1) &&
         (possible_pointer ~one_past:false rest_2)
      then
        (* But beware of the comparisons of literal strings. *)
        if are_comparable_string rest_1 rest_2
        then true, `Ok
        else false, `Shareable_strings
      else false, `Invalid_pointer

let are_comparable op ev1 ev2 =
  fst (are_comparable_reason op ev1 ev2)

let pp_incomparable_reason fmt = function
  | `Ok -> ()
  | `Shareable_strings ->
    Format.pp_print_string fmt
      "equality between pointers to strings that may overlap"
  | `Invalid_pointer ->
    Format.pp_print_string fmt "invalid pointer(s)"
  | `Rel_different_bases ->
    Format.pp_print_string fmt
      "relational comparison to pointers in different bases"
  | `Eq_Different_bases_including_null ->
    Format.pp_print_string fmt
      "equality between a pointer and a constant"
  | `Rel_Different_bases_including_null ->
    Format.pp_print_string fmt
      "relational comparison between a pointer and a constant"


(* --------------------------------------------------------------------------
                              Integer overflow
   -------------------------------------------------------------------------- *)

let overflow_alarms ~signed ~min:mn ~max:mx expr =
  let signed lower bound alarms =
    Extlib.may_map ~dft:alarms (fun n ->
        let kind = if signed then Alarms.Signed else Alarms.Unsigned in
        Alarmset.add (Alarms.Overflow (kind, expr, n, lower)) alarms)
      bound
  in
  let alarms = signed Alarms.Lower_bound mn Alarmset.none in
  signed Alarms.Upper_bound mx alarms

let truncate_integer expr range value =
  let signed = range.Eval_typ.i_signed in
  let size = range.Eval_typ.i_bits in
  let mn, mx =
    if signed then
      let b = Integer.two_power_of_int (size-1) in
      Integer.neg b, Integer.pred b
    else
      Integer.zero, Integer.pred (Integer.two_power_of_int size)
  in
  let warn_under, warn_over =
    try
      let i = V.project_ival value in
      let imn, imx = Ival.min_and_max i in
      let u = match imn with
        | Some bound when Integer.ge bound mn -> None
        | _ -> Some mn
      and o = match imx with
        | Some bound when Integer.le bound mx -> None
        | _ -> Some mx
      in
      u, o
    with V.Not_based_on_null ->
      (* Catch bottom case here: there is no overflow in this case. *)
      if V.is_bottom value then None, None else Some mn, Some mx
  in
  match warn_under, warn_over with
  | None, None -> value, Alarmset.none
  | _ ->
    let all_values =
      Cvalue.V.inject_ival (Ival.inject_range (Some mn) (Some mx))
    in
    let alarms = overflow_alarms ~signed ~min:warn_under ~max:warn_over expr in
    (* Take care of pointers addresses that may have crept in,
       as they may alias with the NULL base *)
    if V.is_arithmetic value
    then V.narrow all_values value, alarms
    else value, alarms

let rewrap_integer range value =
  let size = Integer.of_int range.Eval_typ.i_bits in
  let v, identity = V.cast ~signed:range.Eval_typ.i_signed ~size value in
  if range.Eval_typ.i_signed && not identity then
    Value_util.warning_once_current
      "2's complement assumed for overflow";
  v

let unsafe_cast_float fkind v =
  match Value_util.float_kind fkind with
  | Fval.Float32 ->
    let rounding_mode = Value_util.get_rounding_mode () in
    Cvalue.V.cast_float ~rounding_mode v
  | Fval.Float64 -> Cvalue.V.cast_double v

let cast_float expr fkind v =
  let addresses, overflow, r = unsafe_cast_float fkind v in
  let alarms =
    if overflow || addresses
    then Alarmset.singleton (Alarms.Is_nan_or_infinite (expr, fkind))
    else Alarmset.none
  in
  r, alarms


(* --------------------------------------------------------------------------
                       Binary Operators Evaluation
   -------------------------------------------------------------------------- *)

(* Reduce the rhs argument of a shift so that it fits inside [size] bits. *)
let reduce_shift_rhs typ expr value =
  let size = Cil.bitsSizeOf typ in
  let size_int = Integer.pred (Integer.of_int size) in
  let valid_range_rhs =
    V.inject_ival (Ival.inject_range (Some Integer.zero) (Some size_int))
  in
  if V.is_included value valid_range_rhs
  then return value
  else
    let alarms = Alarmset.singleton (Alarms.Invalid_shift (expr, Some size))
    and value = Cvalue.V.narrow value valid_range_rhs in
    value, alarms

(* Reduce both arguments of a left shift. *)
let reduce_shift_left typ e1 v1 e2 v2 =
  let v2, alarms2 = reduce_shift_rhs typ e2 v2 in
  let warn_negative = Value_parameters.WarnLeftShiftNegative.get ()
                      && Bit_utils.is_signed_int_enum_pointer typ
  in
  let v1, alarms1 = (* Cannot left-shift a negative value *)
    if warn_negative then
      let valid_range_lhs =
        V.inject_ival (Ival.inject_range (Some Integer.zero) None)
      in
      if V.is_included v1 valid_range_lhs
      then return v1
      else
        let alarms = Alarmset.singleton (Alarms.Invalid_shift (e1, None))
        and v1 = Cvalue.V.narrow v1 valid_range_lhs in
        v1, alarms
    else return v1
  in
  v1, v2, Alarmset.union alarms1 alarms2

let division_alarms expr value =
  if Cvalue.V.contains_zero value
  then
    let alarms = Alarmset.singleton (Alarms.Division_by_zero expr) in
    if V.is_arithmetic value
    then alarms
    else Alarmset.add (Alarms.Pointer_comparison (None, expr)) alarms
  else Alarmset.none

let forward_minus_pp ~context ~typ ev1 ev2 =
  let conv minus_offs =
    try
      let size = Int_Base.project (Bit_utils.osizeof_pointed typ) in
      if Integer.is_one size
      then minus_offs
      else Ival.scale_div ~pos:true size minus_offs
    with Abstract_interp.Error_Top -> Ival.top
  in
  if not (Value_parameters.WarnPointerSubstraction.get ()) then
    (* Generate garbled mix if the two pointers disagree on their base *)
    let minus_val = V.add_untyped Int_Base.minus_one ev1 ev2 in
    try
      return (V.inject_ival (conv (Cvalue.V.project_ival minus_val)))
    with Cvalue.V.Not_based_on_null ->
      return (V.join (V.topify_arith_origin ev1) (V.topify_arith_origin ev2))
  else
    (* Pointwise arithmetics.*)
    (* TODO: we may be able to reduce the bases that appear only on one side *)
    let minus_offs, warn = V.sub_untyped_pointwise ev1 ev2 in
    let alarms = if warn
      then
        let e1 = context.left_operand and e2 = context.right_operand in
        Alarmset.singleton (Alarms.Differing_blocks (e1, e2))
      else Alarmset.none
    in
    let offs = conv minus_offs in
    V.inject_ival offs, alarms

(* Evaluation of some operations on Cvalue.V. [typ] is the type of [ev1].
   The function must behave as if it was acting on unbounded integers *)
let forward_binop_int ~context ~logic ~typ ev1 op ev2 =
  let e1 = context.left_operand and e2 = context.right_operand in
  match op with
  | PlusPI
  | IndexPI -> return (V.add_untyped (Bit_utils.osizeof_pointed typ) ev1 ev2)
  | MinusPI ->
    let int_base = Int_Base.neg (Bit_utils.osizeof_pointed typ) in
    return (V.add_untyped int_base ev1 ev2)
  | PlusA   -> return (V.add_untyped (Int_Base.one) ev1 ev2)
  | MinusA  -> return (V.add_untyped Int_Base.minus_one ev1 ev2)
  | MinusPP -> forward_minus_pp ~context ~typ ev1 ev2
  | Mod     -> V.c_rem ev1 ev2, division_alarms e2 ev2
  | Div     -> V.div ev1 ev2, division_alarms e2 ev2
  | Mult    -> return (V.mul ev1 ev2)
  | Shiftrt ->
    if logic
    then V.shift_right ev1 ev2, Alarmset.none
    else
      let ev2, alarms = reduce_shift_rhs typ e2 ev2 in
      V.shift_right ev1 ev2, alarms
  | Shiftlt ->
    if logic
    then V.shift_left ev1 ev2, Alarmset.none
    else
      let ev1, ev2, alarms = reduce_shift_left typ e1 ev1 e2 ev2 in
      V.shift_left ev1 ev2, alarms
  | BXor    -> return (V.bitwise_xor ev1 ev2)
  | BOr     -> return (V.bitwise_or ev1 ev2)
  | BAnd    ->
    let size = Cil.bitsSizeOf typ in
    let signed = Bit_utils.is_signed_int_enum_pointer typ in
    return (V.bitwise_and ~size ~signed ev1 ev2)
  (* Strict evaluation. The caller of this function is supposed to take
     into account the laziness of those operators itself *)
  | LOr  -> return (
      V.interp_boolean
        ~contains_zero:(V.contains_zero ev1 && V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 || V.contains_non_zero ev2))
  | LAnd -> return (
      V.interp_boolean
        ~contains_zero: (V.contains_zero ev1 || V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 && V.contains_non_zero ev2))
  | Eq | Ne | Ge | Le | Gt | Lt ->
    let op = Value_util.conv_comp op in
    let ok, reason = are_comparable_reason op ev1 ev2 in
    if reason <> `Ok && not logic then
      Value_parameters.result
        ~current:true ~once:true
        ~dkey:Value_parameters.dkey_pointer_comparison
        "invalid pointer comparison: %a" pp_incomparable_reason reason;
    let alarms =
      if not ok
      then (* Detect zero expressions created by the evaluator *)
        if Value_util.is_value_zero e1 then
          Alarmset.singleton (Alarms.Pointer_comparison (None, e2))
        else
          Alarmset.singleton (Alarms.Pointer_comparison (Some e1, e2))
      else Alarmset.none
    in
    let signed = Bit_utils.is_signed_int_enum_pointer (Cil.unrollType typ) in
    let r = V.inject_comp_result (V.forward_comp_int ~signed op ev1 ev2) in
    if not ok && Value_parameters.UndefinedPointerComparisonPropagateAll.get ()
       && not logic
    then begin
      Value_parameters.result
        ~current:true ~once:true
        ~dkey:Value_parameters.dkey_pointer_comparison
        "evaluating condition to %a instead of %a because of UPCPA"
        V.pretty V.zero_or_one V.pretty r;
      V.zero_or_one, alarms
    end
    else r, alarms

let forward_binop_float round ev1 op ev2 =
  let conv v =
    try Ival.project_float (V.project_ival v)
    with
    | V.Not_based_on_null
    | Ival.Nan_or_infinite (* raised by project_ival. probably useless *) ->
      Fval.top
  in
  let f1 = conv ev1 and f2 = conv ev2 in
  let binary_float_floats (_name: string) f =
    try
      let alarm, f = f round f1 f2 in
      V.inject_ival (Ival.inject_float f), alarm
    with
    | Fval.Non_finite ->
      V.bottom, true
  in
  match op with
  | PlusA ->   binary_float_floats "+." Fval.add
  | MinusA ->  binary_float_floats "-." Fval.sub
  | Mult ->    binary_float_floats "*." Fval.mul
  | Div ->     binary_float_floats "/." Fval.div
  | Eq | Ne | Lt | Gt | Le | Ge ->
    let op = Value_util.conv_comp op in
    V.inject_comp_result (Fval.forward_comp op f1 f2), false
  | _ -> assert false

let forward_binop_float_alarm ~context round flkind ev1 op ev2 =
  let res, alarm = forward_binop_float round ev1 op ev2 in
  let alarm = if alarm
    then
      let expr = context.binary_result in
      Alarmset.singleton (Alarms.Is_nan_or_infinite (expr, flkind))
    else Alarmset.none
  in
  res, alarm


(* --------------------------------------------------------------------------
                       Unary Operators Evaluation
   -------------------------------------------------------------------------- *)

(* This function evaluates a unary minus, but does _not_ check for overflows.
   This is left to the caller *)
let forward_uneg ~context:{operand} v t =
  match Cil.unrollType t with
  | TFloat (fkind, _) ->
    begin try
        let v = V.project_ival_bottom v in
        let f = Ival.project_float v in
        return (V.inject_ival (Ival.inject_float (Fval.neg f)))
      with
      | V.Not_based_on_null ->
        V.topify_arith_origin v,
        Alarmset.singleton (Alarms.Is_nan_or_infinite (operand, fkind))
      | Ival.Nan_or_infinite ->
        (* raised by project_float; probably useless *)
        if V.is_bottom v then v, Alarmset.none
        else
          V.top_float,
          Alarmset.singleton (Alarms.Is_nan_or_infinite (operand, fkind))
    end
  | _ ->
    try
      let v = V.project_ival v in
      return (V.inject_ival (Ival.neg_int v))
    with V.Not_based_on_null -> return (V.topify_arith_origin v)

let forward_unop ~context typ op value =
  match op with
  | Neg -> forward_uneg ~context value typ
  | BNot -> begin
      match Cil.unrollType typ with
      | TInt (ik, _) | TEnum ({ekind=ik}, _) ->
        let size = Cil.bitsSizeOfInt ik in
        let signed = Cil.isSigned ik in
        V.bitwise_not_size ~signed ~size value, Alarmset.none
      | _ -> assert false
    end
  | LNot ->
    let ok, reason =
      are_comparable_reason Abstract_interp.Comp.Eq V.singleton_zero value
    in
    if reason <> `Ok then
      Value_parameters.result
        ~current:true ~once:true
        ~dkey:Value_parameters.dkey_pointer_comparison
        "invalid pointer negation: %a" pp_incomparable_reason reason;
    let alarms =
      if not ok
      then Alarmset.singleton (Alarms.Pointer_comparison (None, context.operand))
      else Alarmset.none
    in
    let value =
      let r =
        let eq = Abstract_interp.Comp.Eq in
        (* [!c] holds iff [c] is equal to [O] *)
        if Cil.isFloatingType typ then
          try
            let i = V.project_ival value in
            let f = Ival.project_float i in
            V.inject_comp_result (Fval.forward_comp eq f Fval.zero)
          with V.Not_based_on_null | Ival.Nan_or_infinite -> V.zero_or_one
        else
          let signed = Bit_utils.is_signed_int_enum_pointer typ in
          V.inject_comp_result
            (V.forward_comp_int ~signed eq value V.singleton_zero)
      in
      if (not ok &&
          Value_parameters.UndefinedPointerComparisonPropagateAll.get ())
      then (
        Value_parameters.result
          ~current:true ~once:true
          ~dkey:Value_parameters.dkey_pointer_comparison
          "evaluating operator ! to %a instead of %a because of UPCPA"
          V.pretty V.zero_or_one V.pretty r;
        V.zero_or_one)
      else r
    in
    value, alarms

(* --------------------------------------------------------------------------
                              Downcast checks
   -------------------------------------------------------------------------- *)

type integer_range = Eval_typ.integer_range = { i_bits: int; i_signed: bool }

(* If these options are enabled, no integer or pointer value can wrap during the
   analysis. Therefore, the concretization of the abstract value {{ &x }} is
   exactly &x. Otherwise, the integer representation of this address could have
   been wrapped around in an integer type (by integer overflow or a downcast),
   and we don't known how it is now represented in its C type. For instance, an
   address can be negative in a signed type, and thus cannot be safely converted
   even into the (unsigned) pointer type. *)
let no_wrapping () =
  Kernel.SignedDowncast.get () && Kernel.UnsignedDowncast.get ()
  && Kernel.SignedOverflow.get () && Kernel.UnsignedOverflow.get ()

(* Depends on machdep *)
let ptr_range () = Eval_typ.ik_range Cil.theMachine.Cil.upointKind

(* Check whether [v] of fits within the range [range]. Returns two [ok]
   booleans, one for each bound. *)
let value_inclusion v range =
  let i, p = V.split Base.null v in
  (* Check whether the pointer part fits within [dst]. *)
  let ok_ptr_min, ok_ptr_max =
    if V.is_bottom p
    then true, true
    (* If no wrapping is possible and the value [p] is a nearly valid pointer,
       then [p] fits within the pointer type [upointKind]. In this case, do not
       emit alarms if this type is included in the destination type. *)
    else if no_wrapping () && possible_pointer ~one_past:true p
    then Eval_typ.range_inclusion (ptr_range ()) range
    else false, false
  in
  (* Check whether the integer part fits within [dst]. *)
  let ok_min, ok_max =
    if Ival.is_bottom i then
      true, true
    else
      let min_v, max_v = Ival.min_and_max i in
      (match min_v with
       | None -> false
       | Some min_v -> Integer.le (Eval_typ.range_lower_bound range) min_v),
      (match max_v with
       | None -> false
       | Some max_v -> Integer.ge (Eval_typ.range_upper_bound range) max_v)
  in
  ok_ptr_min && ok_min, ok_ptr_max && ok_max

(* How an integer cast should be analyzed *)
type cast_effects =
  | Identity (* nothing to do: upcasts, or downcasts that have no effect. *)
  | DowncastWrap (* downcast, using modulo semantics *)
  | DowncastTruncate of Ival.t * Alarmset.t
  (* truncation to the given range + emission of the given alarms *)

(* Effects of a downcast to [range]. Which alarms are built depends on
   [ok_min] and [ok_max]. [ov_kind] and [exp] are used to build the alarm. *)
let downcast_effects range (ok_min, ok_max) ov_kind exp =
  if ok_min && ok_max then Identity
  else
    let open Alarms in
    let alarm_min, min =
      if not ok_min then
        let min = Eval_typ.range_lower_bound range in
        Alarmset.singleton (Overflow (ov_kind, exp, min, Lower_bound)), Some min
      else Alarmset.none, None
    and alarm_max, max =
      if not ok_max then
        let max = Eval_typ.range_upper_bound range in
        Alarmset.singleton (Overflow (ov_kind, exp, max, Upper_bound)), Some max
      else Alarmset.none, None
    in
    let i = Ival.inject_range min max in
    let alarms = Alarmset.union alarm_min alarm_max in
    DowncastTruncate (i, alarms)

let meet_ok (ol1, ou1) (ol2, ou2) = (ol1 || ol2, ou1 || ou2)

(* Effects of the cast of [exp] from [src] to [dst], assuming [exp] has initial
   value [v]. *)
let cast_effects ~src ~dst v_src exp =
  let ok_low, ok_up as ok = Eval_typ.range_inclusion src dst in
  if ok_low && ok_up then
    Identity (* Upcast. Nothing to check. *)
  else if dst.i_signed then (* signed downcast *)
    if Kernel.SignedDowncast.get () ||
       Value_parameters.WarnSignedConvertedDowncast.get ()
    then
      let ok' = value_inclusion v_src dst in
      downcast_effects dst (meet_ok ok' ok) Alarms.Signed_downcast exp
    else DowncastWrap
  else (* unsigned downcast *)
  if Kernel.UnsignedDowncast.get () then
    let ok' = value_inclusion v_src dst in
    downcast_effects dst (meet_ok ok' ok) Alarms.Unsigned_downcast exp
  else DowncastWrap


let signed_ikind = function
  | IBool                   -> IBool
  | IChar | ISChar | IUChar -> ISChar
  | IInt | IUInt            -> IInt
  | IShort | IUShort        -> IShort
  | ILong | IULong          -> ILong
  | ILongLong | IULongLong  -> ILongLong

let rec signed_counterpart typ =
  match Cil.unrollType typ with
  | TInt (ik, attrs) -> TInt (signed_ikind ik, attrs)
  | TEnum ({ekind = ik} as info, attrs) ->
    let info = { info with ekind = signed_ikind ik} in
    TEnum (info, attrs)
  | TPtr _ -> signed_counterpart Cil.(theMachine.upointType)
  | _ -> assert false

module MemoDowncastConvertedAlarm =
  State_builder.Hashtbl(Cil_datatype.Exp.Hashtbl)(Cil_datatype.Exp)
    (struct
      let name = "Value.Cvalue_forward.MemoDowncastConvertedAlarm"
      let size = 16
      let dependencies = [Ast.self]
    end)
let exp_alarm_signed_converted_downcast =
  MemoDowncastConvertedAlarm.memo
    (fun exp ->
       let src_typ = Cil.typeOf exp in
       let signed_typ = signed_counterpart src_typ in
       let signed_exp = Cil.new_exp ~loc:exp.eloc (CastE (signed_typ, exp)) in
       signed_exp)

(* Relaxed semantics for downcasts into signed types:
   first converts the value to the signed counterpart of the source type, and
   then downcasts it into the signed destination type. Emits only alarms for
   the second cast.
   This function also returns the value on which the problematic cast is
   performed. *)
let relaxed_cast_effects exp ~src ~dst v =
  let ok_low, ok_up = Eval_typ.range_inclusion src dst in
  if ok_low && ok_up then
    Identity, v (* No downcast. Nothing to check. *)
  else
    (* If needed, convert the expression, integer range and value to their
       signed counterparts, then downcast. *)
    let exp, src, v =
      if not src.i_signed && dst.i_signed
      then
        let size = Integer.of_int src.i_bits in
        let signed_v, _ = Cvalue.V.cast ~signed:true ~size v in
        let signed_src = { src with i_signed = true } in
        let signed_exp = exp_alarm_signed_converted_downcast exp in
        signed_exp, signed_src, signed_v
      else exp, src, v
    in
    (* Downcast effects *)
    cast_effects ~src ~dst v exp, v


(* --------------------------------------------------------------------------
                                  Cast
   -------------------------------------------------------------------------- *)

(* Re-export type here *)
type scalar_typ = Eval_typ.scalar_typ =
  | TSInt of integer_range
  | TSPtr of integer_range
  | TSFloat of fkind
  | TSNotScalar

let reinterpret_int range v =
  let size = Integer.of_int range.i_bits in
  fst (V.cast ~signed:range.i_signed ~size v)

let cast_int exp ~src ~dst v =
  let effects, v =
    if
      Value_parameters.WarnSignedConvertedDowncast.get ()
      && not (Kernel.SignedDowncast.get ())
    then relaxed_cast_effects exp ~src ~dst v
    else cast_effects ~src ~dst v exp, v
  in
  match effects with
  | Identity ->
    let v =
      if V.is_topint v
      then reinterpret_int dst v
      else reinterpret_int dst v
    in
    v, Alarmset.none
  | DowncastWrap ->
    let size = Integer.of_int dst.i_bits in
    let v', _ = V.cast ~signed:dst.i_signed ~size v in
    v', Alarmset.none
  | DowncastTruncate (dst_range, alarms) ->
    let irange, pointer_part = V.split Base.null v in
    let irange = Ival.narrow irange dst_range in
    let v_irange = V.inject_ival irange in
    (V.join v_irange pointer_part), alarms

let unsafe_reinterpret typ v =
  match Eval_typ.classify_as_scalar typ with
  | TSInt ik | TSPtr ik  -> reinterpret_int ik v
  | TSFloat fk -> let _, _, v = unsafe_cast_float fk v in v
  | TSNotScalar -> v

(* TODO: reinterpret should never raise an alarm. Infinite/NaN should
   be in the lattice, and handled a bit later. *)
let reinterpret exp typ v =
  match Eval_typ.classify_as_scalar typ with
  | TSFloat fkind -> cast_float exp fkind v
  | TSInt ik | TSPtr ik -> reinterpret_int ik v, Alarmset.none
  | TSNotScalar -> v, Alarmset.none

(* Conversion from floating-point to integer *)
let float_to_int_alarms irange fkind exp v =
  let size = irange.i_bits in
  let signed = irange.i_signed in
  let addr, non_finite, overflows, r =
    Cvalue.V.cast_float_to_int ~signed ~size v
  in
  let alarms =
    if addr || non_finite
    then Alarmset.singleton (Alarms.Is_nan_or_infinite (exp, fkind))
    else Alarmset.none
  in
  let alarms =
    if overflows <> (false, false)
    then
      let dst_range = Ival.create_all_values ~signed ~size in
      let mn, mx = Ival.min_and_max dst_range in
      let emit overflow kind bound alarms = match bound with
        | None -> alarms
        | Some n ->
          if overflow
          then Alarmset.add (Alarms.Float_to_int (exp, n, kind)) alarms
          else alarms
      in
      let alarms = emit (fst overflows) Alarms.Lower_bound mn alarms in
      let alarms = emit (snd overflows) Alarms.Upper_bound mx alarms in
      alarms
    else alarms
  in
  r, alarms

let do_promotion ~rounding_mode ~src_typ ~dst_typ exp v =
  match Eval_typ.classify_as_scalar dst_typ,
        Eval_typ.classify_as_scalar src_typ
  with
  | TSFloat fkind, (TSInt _ | TSPtr _) -> (* Cannot overflow with 32 bits float *)
    let v, _ok = Cvalue.V.cast_int_to_float rounding_mode v in
    cast_float exp fkind v

  | (TSInt dst | TSPtr dst), TSFloat fk ->
    float_to_int_alarms dst fk exp v

  | (TSInt dst | TSPtr dst), (TSInt src | TSPtr src) ->
    cast_int exp ~src ~dst v

  | TSFloat fk, TSFloat _ ->
    cast_float exp fk v

  | (TSNotScalar, _) | (_, TSNotScalar) ->
    return v (*unclear whether this happens*)


(* --------------------------------------------------------------------------
                                  Misc
   -------------------------------------------------------------------------- *)

let make_volatile ?typ v =
  let is_volatile = match typ with
    | None -> true
    | Some typ -> Cil.typeHasQualifier "volatile" typ
  in
  if is_volatile && not (V.is_bottom v)
  then
    match v with
    | V.Top _ -> v
    | V.Map m ->
      let aux b _ acc = V.join acc (V.inject b Ival.top) in
      V.M.fold aux m V.bottom
  else v

let eval_float_constant exp f fkind fstring =
  let fl, fu = match fstring with
    | Some string when Value_parameters.AllRoundingModesConstants.get () ->
      let parsed_f = Floating_point.parse_kind fkind string in
      parsed_f.Floating_point.f_lower, parsed_f.Floating_point.f_upper
    | None | Some _ -> f, f
  in
  let fl = Fval.F.of_float fl
  and fu = Fval.F.of_float fu in
  try
    let non_finite, af = Fval.inject_r fl fu in
    let v = V.inject_ival (Ival.inject_float af) in
    let alarms =
      if non_finite
      then Alarmset.singleton (Alarms.Is_nan_or_infinite (exp, fkind))
      else Alarmset.none
    in
    let v, alarms' = cast_float exp fkind v in
    `Value v, Alarmset.union alarms alarms'
  with Fval.Non_finite ->
    let alarms = Alarmset.singleton (Alarms.Is_nan_or_infinite (exp, fkind)) in
    `Bottom, alarms


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
