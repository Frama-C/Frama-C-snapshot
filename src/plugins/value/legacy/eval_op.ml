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

open Cvalue

open Cil_types
open Abstract_interp

let offsetmap_of_v ~typ v =
  let size = Int.of_int (Cil.bitsSizeOf typ) in
  let v = V_Or_Uninitialized.initialized v in
  V_Offsetmap.create ~size v ~size_v:size 

let wrap_int i = Some (offsetmap_of_v ~typ:Cil.intType i)
let wrap_ptr p = Some (offsetmap_of_v ~typ:Cil.intPtrType p)
let wrap_double d = Some (offsetmap_of_v ~typ:Cil.doubleType d)
let wrap_float d = Some (offsetmap_of_v ~typ:Cil.floatType d)
let wrap_size_t i =
  Some (offsetmap_of_v ~typ:(Cil.theMachine.Cil.typeOfSizeOf) i)


let reinterpret_int ~with_alarms:_ ikind attrs v =
  let range = Eval_typ.ik_attrs_range ikind attrs in
  let size = Integer.of_int (range.Eval_typ.i_bits) in
  let signed = range.Eval_typ.i_signed in
  let v', _ok = V.cast ~signed ~size v in
  v'

let reinterpret_float ~with_alarms fkind v =
  let conv = match Value_util.float_kind fkind with
    | Fval.Float32 ->
        let rounding_mode = Value_util.get_rounding_mode () in
        Cvalue.V.cast_float ~rounding_mode
    | Fval.Float64 -> Cvalue.V.cast_double
  in
  let addresses, overflow, r = conv v in
  if overflow || addresses
  then begin
    Warn.warn_nan_infinite with_alarms;
  end;
  r

let reinterpret ~with_alarms t v =
  match Cil.unrollType t with
  | TInt (ikind, attrs) | TEnum ({ekind=ikind}, attrs) ->
      reinterpret_int ~with_alarms ikind attrs v
  | TPtr _ -> reinterpret_int ~with_alarms Cil.theMachine.Cil.upointKind [] v
  | TFloat (fkind, _) ->
      reinterpret_float ~with_alarms fkind v
  | TBuiltin_va_list _ ->
      V.topify_arith_origin v
  | TComp _ | TArray _ | TFun _ ->
    (* Nothing can/should be done on struct and arrays, that are either already
       imprecise as a Cvalue.V, or read in a precise way. It is not clear
       that a TFun can be obtained here, but one never knows. *)
    v
  | TNamed _ -> assert false
  | TVoid _ -> assert false


let v_uninit_of_offsetmap ~typ offsm =
  let size = Eval_typ.sizeof_lval_typ typ in
  match size with
    | Int_Base.Top -> V_Offsetmap.find_imprecise_everywhere offsm
    | Int_Base.Value size ->
      let validity = Base.validity_from_size size in
      let offsets = Ival.zero in
      let _alarm, r =
        V_Offsetmap.find ~validity ~conflate_bottom:false ~offsets ~size offsm
      in
      r

let do_promotion ~with_alarms rounding_mode ~src_typ ~dst_typ v =
  match Cil.unrollType dst_typ, Cil.unrollType src_typ with
  | TFloat _, TInt _ ->
      (* Cannot overflow with 32 bits float *)
      let v, _ok = Cvalue.V.cast_int_to_float rounding_mode v in
      v
  | TInt (kind,_), TFloat (_fkind, _) ->
      let size = Cil.bitsSizeOfInt kind in
      let signed = Cil.isSigned kind in
      let addr, non_finite, overflows, r =
        Cvalue.V.cast_float_to_int ~signed ~size v
      in
      Warn.warn_float ~with_alarms ~non_finite ~addr ();
      if overflows <> (false, false)
      then Warn.warn_float_to_int_overflow with_alarms;
      r
  | TInt (ikind, attrs), TInt _ ->
      reinterpret_int ~with_alarms ikind attrs v
  | TFloat (fkind, _), TFloat _ ->
      reinterpret_float ~with_alarms fkind v
  | _, _ -> v

let handle_overflow ~with_alarms ~warn_unsigned typ interpreted_e =
  match Cil.unrollType typ with
    | TInt(kind, _) ->
        let signed = Cil.isSigned kind in
        let size = Cil.bitsSizeOfInt kind in
        let mn, mx =
          if signed then
            let b = Int.two_power_of_int (size-1) in
            Int.neg b, Int.pred b
          else
            Int.zero, Int.pred (Int.two_power_of_int size)
        in
        let warn_under, warn_over =
          try
            let i = V.project_ival interpreted_e in
            let imn, imx = Ival.min_and_max i in
            let u =
              match imn with
                | Some bound when Int.ge bound mn -> None
                | _ -> Some mn
            in
            let o =
              match imx with
                | Some bound when Int.le bound mx -> None
                | _ -> Some mx
            in
            u, o
          with V.Not_based_on_null ->
            (* Catch bottom case here: there is no overflow in this case. *)
            if V.is_bottom interpreted_e then
              None, None
            else
              Some mn, Some mx
        in
	(match warn_under, warn_over with
	   | None, None ->
	       interpreted_e
	   | _ ->
	       if (signed && Kernel.SignedOverflow.get ()) ||
                  (not signed && warn_unsigned && Kernel.UnsignedOverflow.get())
               then
	         let all_values =
		   Cvalue.V.inject_ival 
		     (Ival.inject_range (Some mn) (Some mx))
	         in
	         Warn.warn_integer_overflow with_alarms;
                 (* Take care of pointers addresses that may have crept in,
                    as they may alias with the NULL base *)
                 try
                   ignore (V.project_ival interpreted_e);
                   V.narrow all_values interpreted_e
                 with V.Not_based_on_null -> interpreted_e
               else begin (* [interpreted_e] has been computed modulo [size] *)
                 if signed then
                   Value_util.warning_once_current
                     "2's complement assumed for overflow";
                 interpreted_e;
               end)
    | _ -> interpreted_e

let eval_binop_float ~with_alarms round ev1 op ev2 = 
  let conv v = 
    try Ival.project_float (V.project_ival v)
    with
      | V.Not_based_on_null
      | Ival.Nan_or_infinite (* raised by project_ival. probably useless *) ->
          Fval.top
  in
  let f1 = conv ev1 in
  let f2 = conv ev2 in
  let binary_float_floats (_name: string) f =
    try
      let alarm, f = f round f1 f2 in
      if alarm then Warn.warn_nan_infinite with_alarms;
      V.inject_ival (Ival.inject_float f)
    with
      | Fval.Non_finite ->
        Warn.warn_nan_infinite with_alarms;
        V.bottom
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

let eval_minus_pp ~with_alarms ~te1 ev1 ev2 =
  (* Difference between the two pointers is [minus_offs] bytes, convert
     to pointer difference. *)
  let conv minus_offs =
    try
      let size = Int_Base.project (Bit_utils.osizeof_pointed te1) in
      if Int.is_one size then
        minus_offs
      else
        Ival.scale_div ~pos:true size minus_offs
    with Int_Base.Error_Top -> Ival.top
  in
  if not (Value_parameters.WarnPointerSubstraction.get ()) then begin
    (* Generate garbled mix if the two pointers disagree on their base *)
    let minus_val = V.add_untyped Int_Base.minus_one ev1 ev2 in
    try
      V.inject_ival (conv (Cvalue.V.project_ival minus_val))
    with Cvalue.V.Not_based_on_null ->
      V.join (V.topify_arith_origin ev1) (V.topify_arith_origin ev2)
  end else begin
    (* Pointwise arithmetics.*)
    (* TODO: we may be able to reduce the bases that appear only on one side *)
    let minus_offs, warn = V.sub_untyped_pointwise ev1 ev2 in
    if warn then Warn.warn_pointer_subtraction with_alarms;
    let offs = conv minus_offs in
    V.inject_ival offs
  end

(* eval some operations on location_bytes. This function is more low-level
   than eval_binop, that evaluates the expressions in the given state. Here,
   we suppose someone else has done the evaluation, and combine the results.
   [te1] is the type of [ev1]. The function must behave as if it was acting on
   unbounded integers *)
let eval_binop_int ~with_alarms ~te1 ev1 op ev2 =
  match op with
    | PlusPI | IndexPI -> V.add_untyped (Bit_utils.osizeof_pointed te1) ev1 ev2
    | MinusPI ->
        V.add_untyped (Int_Base.neg (Bit_utils.osizeof_pointed te1)) ev1 ev2
    | PlusA ->  V.add_untyped (Int_Base.one) ev1 ev2
    | MinusA -> V.add_untyped Int_Base.minus_one ev1 ev2
    | MinusPP -> eval_minus_pp ~with_alarms ~te1 ev1 ev2
    | Mod -> V.c_rem ev1 ev2
    | Div -> V.div ev1 ev2
    | Mult -> V.mul ev1 ev2
    | BXor -> V.bitwise_xor ev1 ev2
    | BOr -> V.bitwise_or ev1 ev2
    | BAnd ->
        let size = Cil.bitsSizeOf te1 in
        let signed = Bit_utils.is_signed_int_enum_pointer te1 in
        V.bitwise_and ~size ~signed ev1 ev2
    | Eq | Ne | Ge | Le | Gt | Lt as op ->
      let op = Value_util.conv_comp op in
      let warn = not (Cvalue_forward.are_comparable op ev1 ev2) in
      if warn then Warn.warn_pointer_comparison te1 with_alarms;
      if warn && Value_parameters.UndefinedPointerComparisonPropagateAll.get ()
      then V.zero_or_one
      else
        let signed = Bit_utils.is_signed_int_enum_pointer te1 in
        V.inject_comp_result (V.forward_comp_int ~signed op ev1 ev2)
    | Shiftrt -> V.shift_right ev1 ev2
    | Shiftlt -> V.shift_left ev1 ev2
    (* Strict evaluation. The caller of this function is supposed to take
       into account the laziness of those operators itself *)
    | LOr -> V.interp_boolean
        ~contains_zero:(V.contains_zero ev1 && V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 || V.contains_non_zero ev2)
    | LAnd -> V.interp_boolean
        ~contains_zero: (V.contains_zero ev1 || V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 && V.contains_non_zero ev2)

(* This function evaluates a unary minus, but does _not_ check for overflows.
   This is left to the caller *)
and eval_uneg ~with_alarms v t =
  match Cil.unrollType t with
  | TFloat (_fkind, _) ->
      (try
          let v = V.project_ival_bottom v in
          let f = Ival.project_float v in
          V.inject_ival
            (Ival.inject_float (Fval.neg f))
        with
        | V.Not_based_on_null ->
            Warn.warn_float ~with_alarms ~addr:true ();
            V.topify_arith_origin v
        | Ival.Nan_or_infinite (* raised by project_float; probably useless*) ->
          if V.is_bottom v then v
          else begin
            Warn.warn_float ~with_alarms ~non_finite:true ();
            V.top_float
          end
      )
  | _ ->
      try
        let v = V.project_ival v in
        V.inject_ival (Ival.neg_int v)
      with V.Not_based_on_null -> V.topify_arith_origin v


let eval_unop ~check_overflow ~with_alarms v t op =
  match op with
  | Neg ->
      let r = eval_uneg ~with_alarms v t in
      if check_overflow
      then handle_overflow ~with_alarms ~warn_unsigned:true t r
      else r
  | BNot -> V.bitwise_not v

  | LNot ->
      let warn = not (Cvalue_forward.are_comparable Comp.Eq V.singleton_zero v) in
      if warn then Warn.warn_pointer_comparison t with_alarms;
      if (warn &&
             Value_parameters.UndefinedPointerComparisonPropagateAll.get ())
      then
        V.zero_or_one
      else
      (* [!c] holds iff [c] is equal to [O] *)
      if Cil.isFloatingType t then
        try
          let i = V.project_ival v in
          let f = Ival.project_float i in
          V.inject_comp_result (Fval.forward_comp Comp.Eq f Fval.zero)
        with V.Not_based_on_null | Ival.Nan_or_infinite -> V.zero_or_one
      else
        let signed = Bit_utils.is_signed_int_enum_pointer t in
        V.inject_comp_result
          (V.forward_comp_int ~signed Comp.Eq v V.singleton_zero)

let backward_comp_int_left positive comp l r =
  if (Value_parameters.UndefinedPointerComparisonPropagateAll.get())
    && not (Cvalue_forward.are_comparable comp l r)
  then l
  else
    let binop = if positive then comp else Comp.inv comp in
    V.backward_comp_int_left binop l r

let backward_comp_float_left round fkind positive comp l r =
  let binop = if positive then comp else Comp.inv comp in
  V.backward_comp_float_left binop round fkind l r

let backward_comp_left_from_type t =
  match Cil.unrollType t with
  | TInt _ | TEnum _ | TPtr _ -> backward_comp_int_left
  | TFloat (fk, _) ->
    backward_comp_float_left
      (Value_parameters.AllRoundingModes.get ()) (Value_util.float_kind fk)
  | _ -> (fun _ _ v _ -> v) (* should never occur anyway *)

exception Unchanged
exception Reduce_to_bottom

let reduce_by_initialized_defined f loc state =
  try
    let base, offset =
      Locations.Location_Bits.find_lonely_key loc.Locations.loc
    in
    if Base.is_weak base then raise Unchanged;
    let size = Int_Base.project loc.Locations.size in
    let ll = Ival.project_int offset in
    let lh = Int.pred (Int.add ll size) in
    let offsm = match Model.find_base_or_default base state with
      | `Bottom | `Top -> raise Unchanged
      | `Value offsm -> offsm
    in
    let aux (offl, offh) (v, modu, shift) acc =
      let v' = f v in
      if v' != v then begin
        if V_Or_Uninitialized.is_bottom v' then raise Reduce_to_bottom;
        let il = Int.max offl ll and ih = Int.min offh lh in
        let abs_shift = Integer.pos_rem (Rel.add_abs offl shift) modu in
        (* il and ih are the bounds of the interval to reduce.
           We change the initialized flags in the following cases:
           - either we overwrite entire values, or the partly overwritten
           value is at the beginning or at the end of the subrange
           - or we do not lose information on misaligned or partial values:
           the result is a singleton *)
        if V_Or_Uninitialized.(cardinal_zero_or_one v' || is_isotropic v') ||
	  ((Int.equal offl il || Int.equal (Int.pos_rem ll modu) abs_shift) &&
           (Int.equal offh ih || Int.equal
                                   (Int.pos_rem (Int.succ lh) modu) abs_shift))
        then
          let diff = Rel.sub_abs il offl in
          let shift_il = Rel.pos_rem (Rel.sub shift diff) modu in
          V_Offsetmap.add (il, ih) (v', modu, shift_il) acc
        else acc
      end
      else acc
     in
     let noffsm =
       V_Offsetmap.fold_between ~entire:true (ll, lh) aux offsm offsm
     in
     Model.add_base base noffsm state
   with
     | Reduce_to_bottom -> Model.bottom
     | Unchanged -> state
     | Int_Base.Error_Top (* from Int_Base.project *)
     | Not_found (* from find_lonely_key *)
     | Ival.Not_Singleton_Int (* from Ival.project_int *) ->
         state

let reduce_by_valid_loc ~positive ~for_writing loc typ state =
  try
    let value = Cvalue.Model.find state loc in
    if Cvalue.V.is_imprecise value then
      (* we won't reduce anything anyway, and we may lose information if loc
         contains misaligned data *)
      raise Exit;
    let loc_bits = Locations.loc_bytes_to_loc_bits value in
    let size = Bit_utils.sizeof_pointed typ in
    let value_as_loc = Locations.make_loc loc_bits size in
    let reduced_value =
      Locations.loc_to_loc_without_size
        (if positive
          then Locations.valid_part ~for_writing value_as_loc
          else Locations.invalid_part value_as_loc )
    in
    if V.equal value reduced_value
    then state
    else begin
      if V.equal V.bottom reduced_value
      then Cvalue.Model.bottom
      else
        Cvalue.Model.reduce_previous_binding state loc reduced_value
    end
  with Exit -> state

let make_loc_contiguous loc =
  try
    let base, offset =
      Locations.Location_Bits.find_lonely_key loc.Locations.loc
    in
    match offset, loc.Locations.size with
    | Ival.Top (Some min, Some max, _rem, modu), Int_Base.Value size
         when Int.equal modu size ->
       let size' = Int.add (Int.sub max min) modu in
       let i = Ival.inject_singleton min in
       let loc_bits = Locations.Location_Bits.inject base i in
       Locations.make_loc loc_bits (Int_Base.inject size')
    | _ -> loc
  with Not_found -> loc

let apply_on_all_locs f loc state =
  match loc.Locations.size with
  | Int_Base.Top -> state
  | Int_Base.Value _ as size ->
    try
      let loc = Locations.valid_part ~for_writing:false loc in
      let loc = loc.Locations.loc in
      let plevel = Value_parameters.ArrayPrecisionLevel.get() in
      ignore (Locations.Location_Bits.cardinal_less_than loc plevel);
      Locations.Location_Bits.fold_enum
        (fun l acc -> f (Locations.make_loc l size) acc) loc state
    with Not_less_than | Locations.Location_Bits.Error_Top -> state

(* Display [o] as a single value, when this is more readable and more precise
   than the standard display. *)
let pretty_stitched_offsetmap fmt typ o =
  if Cil.isArithmeticOrPointerType typ &&
     not (Cvalue.V_Offsetmap.is_single_interval o)
  then
    let v = v_uninit_of_offsetmap ~typ o in
    if not (Cvalue.V_Or_Uninitialized.is_isotropic v)
    then
      Format.fprintf fmt "@\nThis amounts to: %a"
        Cvalue.V_Or_Uninitialized.pretty v

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
