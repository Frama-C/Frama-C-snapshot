(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

let is_invalid b = if b then Alarmset.False else Alarmset.Unknown

let status_of_alarm a =
  let open Abstract_interp in
  match a with
  | SureAlarm -> False
  | Alarm -> Unknown
  | NoAlarm -> True

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

let warn_pointer_comparison typ =
  match Value_parameters.WarnPointerComparison.get () with
  | "none" -> false
  | "all" -> true
  | "pointer" -> Cil.isPointerType (Cil.unrollType typ)
  | _ -> assert false

let propagate_all_pointer_comparison =
  Value_parameters.UndefinedPointerComparisonPropagateAll.get

let forward_comparison ~logic typ comp ~left:(e1, v1) ~right:(e2, v2) ~result =
  let ok, reason = are_comparable_reason comp v1 v2 in
  if reason <> `Ok && not logic then
    Value_parameters.result
      ~current:true ~once:true
      ~dkey:Value_parameters.dkey_pointer_comparison
      "invalid pointer comparison: %a" pp_incomparable_reason reason;
  let alarms =
    if ok || not (warn_pointer_comparison typ)
    then Alarmset.none
    else Alarmset.singleton (Alarms.Pointer_comparison (e1, e2))
  in
  let value =
    if ok
    then result
    else if not (Cil.isPointerType typ)
    then V.zero_or_one
    else if logic || not (propagate_all_pointer_comparison ())
    then result
    else
      begin
        Value_parameters.result
          ~current:true ~once:true
          ~dkey:Value_parameters.dkey_pointer_comparison
          "evaluating condition to %a instead of %a because of UPCPA"
          V.pretty V.zero_or_one V.pretty result;
        V.zero_or_one
      end
  in
  value, alarms

(* --------------------------------------------------------------------------
                              Integer overflow
   -------------------------------------------------------------------------- *)

type integer_range = Eval_typ.integer_range = { i_bits: int; i_signed: bool }

(* Build alarms for int overflows/downcast when [i] does not fit inside
   [minr .. maxr]. [kind] is the kind of alarms that must be produced,
   and [exp] the expression on which the overflow/downcast occurs.*)
let alarms_for_int_overflow_ival kind exp i (minr, maxr) =
  let set_alarm bound direction status alarms =
    Alarmset.set (Alarms.Overflow (kind, exp, bound, direction)) status alarms
  in
  let ir = Ival.inject_range (Some minr) (Some maxr) in
  if Ival.is_included i ir then
    Alarmset.none
  else
    let open Abstract_interp.Comp in
    let l_status = Ival.forward_comp_int Ge i (Ival.inject_singleton minr) in
    let alarms = set_alarm minr Alarms.Lower_bound l_status Alarmset.none in
    let u_status = Ival.forward_comp_int Le i (Ival.inject_singleton maxr) in
    set_alarm maxr Alarms.Upper_bound u_status alarms

(* Build alarms for int overflows/downcast when [v] does not fit inside
   [range]. [src_range] represents the possible values for [v], and
   is used to statically restrict the alarms when [v] is a pointer.
   See {!alarms_for_int_overflow_ival} for [kind] and [exp] *)
let alarms_for_int_overflow kind exp range ?src_range value =
  try
    let i = V.project_ival value in
    alarms_for_int_overflow_ival kind exp i range
  with V.Not_based_on_null ->
    if V.is_bottom value
    then Alarmset.none (* bottom case: there is no overflow in this case. *)
    else
      (* Build the max possible range for [v] as an ival. *)
      let i = match src_range with
        | None -> Ival.top
        | Some r ->
          let min = Eval_typ.range_lower_bound r in
          let max = Eval_typ.range_upper_bound r in
          Ival.inject_range (Some min) (Some max)
      in
      alarms_for_int_overflow_ival kind exp i range

let truncate_int kind expr range ?src_range value =
  let mn, mx = Eval_typ.(range_lower_bound range, range_upper_bound range) in
  let alarms = alarms_for_int_overflow kind expr (mn, mx) ?src_range value in
  let dst_range = Ival.inject_range (Some mn) (Some mx) in
  let irange, _ = V.split Base.null value in
  let irange = Ival.narrow irange dst_range in
  let value = V.add Base.null irange value in
  value, alarms

let truncate_integer expr range value =
  let kind = if range.i_signed then Alarms.Signed else Alarms.Unsigned in
  truncate_int kind expr range value

let rewrap_int ?(warn=false) range value =
  let size = Integer.of_int range.i_bits in
  let v, identity = V.cast_int_to_int ~signed:range.i_signed ~size value in
  if warn && range.i_signed && not identity
  then Value_util.warning_once_current "2's complement assumed for overflow";
  v

let rewrap_integer = rewrap_int ~warn:true


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
    let value =
      if Cvalue.V.is_arithmetic value
      then Cvalue.V.narrow value valid_range_rhs
      else Cvalue.V.topify_arith_origin value
    in
    let status = is_invalid (Cvalue.V.is_bottom value) in
    let alarms =
      Alarmset.singleton ~status (Alarms.Invalid_shift (expr, Some size))
    in
    value, alarms

(* Reduce both arguments of a left shift. *)
let reduce_shift_left typ e1 v1 e2 v2 =
  let v2, alarms = reduce_shift_rhs typ e2 v2 in
  let warn_negative = Value_parameters.WarnLeftShiftNegative.get ()
                      && Bit_utils.is_signed_int_enum_pointer typ
  in
  let v1, alarms = (* Cannot left-shift a negative value *)
    if warn_negative then
      let valid_range_lhs =
        V.inject_ival (Ival.inject_range (Some Integer.zero) None)
      in
      if V.is_included v1 valid_range_lhs
      then v1, alarms
      else
        let v1 =
          if Cvalue.V.is_arithmetic v1
          then Cvalue.V.narrow v1 valid_range_lhs
          else Cvalue.V.topify_arith_origin v1
        in
        let status = is_invalid (Cvalue.V.is_bottom v1) in
        let alarm = Alarms.Invalid_shift (e1, None) in
        let alarms = Alarmset.set alarm status alarms in
        v1, alarms
    else v1, alarms
  in
  v1, v2, alarms

let int_division_alarms expr value =
  if Cvalue.V.contains_zero value
  then
    let status = is_invalid (Cvalue.V.is_zero value) in
    Alarmset.singleton ~status (Alarms.Division_by_zero expr)
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
        let status = is_invalid (Ival.is_bottom minus_offs) in
        Alarmset.singleton ~status (Alarms.Differing_blocks (e1, e2))
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
  | Mod     -> V.c_rem ev1 ev2, int_division_alarms e2 ev2
  | Div     -> V.div ev1 ev2, int_division_alarms e2 ev2
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
    let signed = Bit_utils.is_signed_int_enum_pointer (Cil.unrollType typ) in
    let result = V.inject_comp_result (V.forward_comp_int ~signed op ev1 ev2) in
    (* Detect zero expressions created by the evaluator. *)
    let e1 = if Value_util.is_value_zero e1 then None else Some e1 in
    forward_comparison ~logic typ op ~left:(e1, ev1) ~right:(e2, ev2) ~result

let is_finite_alarm ~remove_infinite ?status expr flkind =
  let alarm =
    if remove_infinite
    then Alarms.Is_nan_or_infinite (expr, flkind)
    else Alarms.Is_nan (expr, flkind)
  in
  Alarmset.singleton ?status alarm

let restrict_float ~remove_infinite fkind expr v =
  let kind = Fval.kind fkind in
  let evaluate, backward_propagate =
    if remove_infinite
    then Fval.is_finite, Fval.backward_is_finite
    else Fval.is_not_nan, fun _fkind -> Fval.backward_is_not_nan
  in
  match Cvalue.V.project_float v with
  | exception Cvalue.V.Not_based_on_null ->
    if Cvalue.V.is_bottom v
    then v, Alarmset.none
    else v, is_finite_alarm ~remove_infinite expr fkind
  | res ->
    match evaluate res with
    | Abstract_interp.Comp.False as status ->
      V.bottom, is_finite_alarm ~remove_infinite ~status expr fkind
    | Abstract_interp.Comp.True ->
      V.inject_float res, Alarmset.none
    | Abstract_interp.Comp.Unknown ->
      let res = Bottom.non_bottom (backward_propagate kind res) in
      V.inject_float res, is_finite_alarm ~remove_infinite expr fkind

let forward_binop_float fkind ev1 op ev2 =
  match V.project_float ev1, V.project_float ev2 with
  | exception V.Not_based_on_null ->
    V.join (V.topify_arith_origin ev1) (V.topify_arith_origin ev2)
  | f1, f2 ->
    let binary_float_floats (_name: string) f =
      V.inject_float (f fkind f1 f2)
    in
    match op with
    | PlusA ->   binary_float_floats "+." Fval.add
    | MinusA ->  binary_float_floats "-." Fval.sub
    | Mult ->    binary_float_floats "*." Fval.mul
    | Div ->     binary_float_floats "/." Fval.div
    | Eq | Ne | Lt | Gt | Le | Ge ->
      let op = Value_util.conv_comp op in
      V.inject_comp_result (Fval.forward_comp op f1 f2)
    | _ -> assert false


(* --------------------------------------------------------------------------
                       Unary Operators Evaluation
   -------------------------------------------------------------------------- *)

(* This function evaluates a unary minus, but does _not_ check for overflows.
   This is left to the caller *)
let forward_uneg v t =
  try
    match Cil.unrollType t with
    | TFloat _ ->
      let v = V.project_float v in
      return (V.inject_ival (Ival.inject_float (Fval.neg v)))
    | _ ->
      let v = V.project_ival v in
      return (V.inject_ival (Ival.neg_int v))
  with V.Not_based_on_null ->
    if Cvalue.V.is_bottom v
    then return v
    else return (V.topify_arith_origin v)

let forward_unop ~context typ op value =
  match op with
  | Neg -> forward_uneg value typ
  | BNot -> begin
      match Cil.unrollType typ with
      | TInt (ik, _) | TEnum ({ekind=ik}, _) ->
        let size = Cil.bitsSizeOfInt ik in
        let signed = Cil.isSigned ik in
        V.bitwise_not_size ~signed ~size value, Alarmset.none
      | _ -> assert false
    end
  | LNot ->
    let result =
      let eq = Abstract_interp.Comp.Eq in
      (* [!c] holds iff [c] is equal to [O] *)
      if Cil.isFloatingType typ then
        try
          let i = V.project_ival value in
          let f = Ival.project_float i in
          V.inject_comp_result (Fval.forward_comp eq f Fval.plus_zero)
        with V.Not_based_on_null -> V.zero_or_one
      else
        let signed = Bit_utils.is_signed_int_enum_pointer typ in
        V.inject_comp_result
          (V.forward_comp_int ~signed eq value V.singleton_zero)
    in
    let comp = Abstract_interp.Comp.Eq in
    let left = None, V.singleton_zero in
    let right = context.operand, value in
    forward_comparison ~logic:false typ comp ~left ~right ~result

(* --------------------------------------------------------------------------
                              Relaxed Downcasts
   -------------------------------------------------------------------------- *)

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
   the second cast. *)
let relaxed_signed_downcast expr ~src ~dst value =
  let expr, src, value =
    if not src.i_signed
    then
      let signed_src = { src with i_signed = true } in
      let signed_v = rewrap_int signed_src value in
      let signed_exp = exp_alarm_signed_converted_downcast expr in
      signed_exp, signed_src, signed_v
    else expr, src, value
  in
  truncate_int Alarms.Signed_downcast expr dst ~src_range:src value


(* --------------------------------------------------------------------------
                                  Cast
   -------------------------------------------------------------------------- *)

(* Re-export type here *)
type scalar_typ = Eval_typ.scalar_typ =
  | TSInt of integer_range
  | TSPtr of integer_range
  | TSFloat of fkind
  | TSNotScalar

let cast_int_to_int expr ~src ~dst value =
  (* Regain some precision in case a transfer function was imprecise.
     This should probably be done in the transfer function, though. *)
  let value =  if V.is_topint value then rewrap_int dst value else value in
  if Eval_typ.range_inclusion src dst
  then value, Alarmset.none (* Upcast, nothing to check. *)
  else if dst.i_signed then (* Signed downcast. *)
    if Kernel.SignedDowncast.get ()
    then truncate_int Alarms.Signed_downcast expr dst ~src_range:src value
    else if Value_parameters.WarnSignedConvertedDowncast.get ()
    then relaxed_signed_downcast expr ~src ~dst value
    else rewrap_int dst value, Alarmset.none
  else (* Unsigned downcast. *)
  if Kernel.UnsignedDowncast.get ()
  then truncate_int Alarms.Unsigned_downcast expr dst ~src_range:src value
  else rewrap_int dst value, Alarmset.none

let reinterpret_as_int range v =
  let size = Integer.of_int range.i_bits in
  Cvalue.V.reinterpret_as_int ~signed:range.i_signed ~size v

let reinterpret typ v =
  match Eval_typ.classify_as_scalar typ with
  | TSInt ik | TSPtr ik  -> reinterpret_as_int ik v
  | TSFloat fk -> Cvalue.V.reinterpret_as_float fk v
  | TSNotScalar -> v

(* Cast from floating-point to integer. [context] is the expression being cast
   and its size, to build the alarms. *)
let cast_float_to_int_alarms irange context v =
  let size = irange.i_bits in
  let signed = irange.i_signed in
  let _nan, overflows, r = Cvalue.V.cast_float_to_int ~signed ~size v in
  let alarms = Alarmset.none in
  let alarms =
    if overflows <> (Abstract_interp.NoAlarm, Abstract_interp.NoAlarm)
    then
      let dst_range = Ival.create_all_values ~signed ~size in
      let mn, mx = Ival.min_and_max dst_range in
      let emit overflow kind bound alarms = match bound with
        | None -> alarms
        | Some n ->
          let exp = context () in
          let status = status_of_alarm overflow in
          Alarmset.set (Alarms.Float_to_int (exp, n, kind)) status alarms
      in
      let alarms = emit (fst overflows) Alarms.Lower_bound mn alarms in
      let alarms = emit (snd overflows) Alarms.Upper_bound mx alarms in
      alarms
    else alarms
  in
  r, alarms

let cast ~src_typ ~dst_typ exp v =
  match Eval_typ.classify_as_scalar dst_typ,
        Eval_typ.classify_as_scalar src_typ
  with
  | TSFloat fkind, (TSInt _ | TSPtr _) -> (*Cannot overflow with 32 bits float*)
    return (Cvalue.V.cast_int_to_float (Fval.kind fkind) v)

  | (TSInt dst | TSPtr dst), TSFloat _fk ->
    cast_float_to_int_alarms dst (fun () -> exp) v

  | (TSInt dst | TSPtr dst), (TSInt src | TSPtr src) ->
    cast_int_to_int exp ~src ~dst v

  | TSFloat fkind, TSFloat _ ->
    return (Cvalue.V.cast_float_to_float (Fval.kind fkind) v)

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

let eval_float_constant f fkind fstring =
  let fl, fu = match fstring with
    | Some string when fkind = Cil_types.FLongDouble ||
                       Value_parameters.AllRoundingModesConstants.get () ->
      let parsed_f = Floating_point.parse_kind fkind string in
      parsed_f.Floating_point.f_lower, parsed_f.Floating_point.f_upper
    | None | Some _ -> f, f
  in
  let fl = Fval.F.of_float fl
  and fu = Fval.F.of_float fu in
  let af = Fval.inject (Fval.kind fkind) fl fu in
  V.inject_float af


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
