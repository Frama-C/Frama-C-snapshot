(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

let pp_v v fmt = V.pretty fmt v

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


let reinterpret_int ~with_alarms ikind v =
  let size = Int.of_int (Cil.bitsSizeOfInt ikind) in
  let signed = Cil.isSigned ikind in
  let v', ok = V.cast ~signed ~size v in
  if not ok then
    Valarms.do_warn with_alarms.CilE.imprecision_tracing
      (fun () ->
         Kernel.warning ~once:true ~current:true
           "@[casting address@ to a type@ smaller@ than sizeof(void*):@ \
                  @[%a@]@]" V.pretty v
      );
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
    Valarms.warn_nan_infinite
      with_alarms (Some fkind) (fun fmt -> V.pretty fmt v);
  end;
  r

let reinterpret ~with_alarms t v =
  match Cil.unrollType t with
  | TInt (ikind, _)
  | TEnum ({ekind=ikind},_) ->
      reinterpret_int ~with_alarms ikind v
  | TPtr _ -> reinterpret_int ~with_alarms Cil.theMachine.Cil.upointKind v
  | TFloat (fkind, _) ->
      reinterpret_float ~with_alarms fkind v
  | TBuiltin_va_list _ ->
      (Valarms.do_warn with_alarms.CilE.imprecision_tracing 
	 (fun () ->
           Value_util.warning_once_current
             "cast to __builtin_va_list is not precisely implemented yet%t"
             Value_util.pp_callstack)
      );
      V.topify_arith_origin v
  | TComp _ | TArray _ | TFun _ ->
    (* Nothing can/should be done on struct and arrays, that are either already
       imprecise as a Cvalue.V, or read in a precise way. It is not clear
       that a TFun can be obtained here, but one never knows. *)
    v
  | TNamed _ -> assert false
  | TVoid _ -> assert false


let v_uninit_of_offsetmap ~with_alarms ~typ offsm =
  let size = Eval_typ.sizeof_lval_typ typ in
  match size with
    | Int_Base.Top -> V_Offsetmap.find_imprecise_everywhere offsm
    | Int_Base.Value size ->
      let validity = Base.Known (Integer.zero, Integer.pred size) in
      let offsets = Ival.zero in
      let alarm, r =
        V_Offsetmap.find ~validity ~conflate_bottom:false ~offsets ~size offsm
      in
      if alarm then Valarms.warn_mem_read with_alarms;
      r

let v_of_offsetmap ~with_alarms ~typ offsm =
  let v_uninit = v_uninit_of_offsetmap ~with_alarms ~typ offsm in
  let v = V_Or_Uninitialized.get_v v_uninit in
  reinterpret ~with_alarms typ v


let do_promotion ~with_alarms rounding_mode ~src_typ ~dst_typ v msg =
  match Cil.unrollType dst_typ, Cil.unrollType src_typ with
  | TFloat _, TInt _ ->
      (* Cannot overflow with 32 bits float *)
      let v, _ok = Cvalue.V.cast_int_to_float rounding_mode v in
      v
  | TInt (kind,_), TFloat (fkind, _) ->
      let size = Cil.bitsSizeOfInt kind in
      let signed = Cil.isSigned kind in
      let addr, non_finite, overflows, r =
        Cvalue.V.cast_float_to_int ~signed ~size v
      in
      Warn.warn_float ~with_alarms ~non_finite ~addr (Some fkind) msg;
      if overflows <> (false, false)
      then begin
        let dst_range = Ival.create_all_values ~signed ~size in
        let mn, mx = Ival.min_and_max dst_range in
        let mn = if fst overflows then mn else None in
        let mx = if snd overflows then mx else None in
        Valarms.warn_float_to_int_overflow with_alarms mn mx msg;
      end;
      r
  | TInt (ikind, _), TInt _ ->
      reinterpret_int ~with_alarms ikind v
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
	         Valarms.warn_integer_overflow with_alarms
                   ~signed ~min:warn_under ~max:warn_over;
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

let eval_binop_float ~with_alarms round flkind ev1 op ev2 = 
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
      if alarm then
        Valarms.warn_nan_infinite with_alarms
          flkind (fun fmt -> Fval.pretty_overflow fmt f);
      V.inject_ival (Ival.inject_float f)
    with
      | Fval.Non_finite ->
          Valarms.warn_nan_infinite with_alarms flkind (pp_v V.top_int);
          V.bottom
  in
  match op with
    | PlusA ->   binary_float_floats "+." Fval.add
    | MinusA ->  binary_float_floats "-." Fval.sub
    | Mult ->    binary_float_floats "*." Fval.mul
    | Div ->     binary_float_floats "/." Fval.div
    | Eq ->
        let contains_zero, contains_non_zero =
          Fval.equal_float_ieee f1 f2
        in
        V.interp_boolean ~contains_zero ~contains_non_zero
    | Ne ->
        let contains_non_zero, contains_zero =
          Fval.equal_float_ieee f1 f2
        in
        V.interp_boolean ~contains_zero ~contains_non_zero
    | Lt ->
        V.interp_boolean
          ~contains_zero:(Fval.maybe_le_ieee_float f2 f1)
          ~contains_non_zero:(Fval.maybe_lt_ieee_float f1 f2)
    | Le ->
        V.interp_boolean
          ~contains_zero:(Fval.maybe_lt_ieee_float f2 f1)
          ~contains_non_zero:(Fval.maybe_le_ieee_float f1 f2)
    | Gt ->
        V.interp_boolean
          ~contains_zero:(Fval.maybe_le_ieee_float f1 f2)
          ~contains_non_zero:(Fval.maybe_lt_ieee_float f2 f1)
    | Ge ->
        V.interp_boolean
          ~contains_zero:(Fval.maybe_lt_ieee_float f1 f2)
          ~contains_non_zero:(Fval.maybe_le_ieee_float f2 f1)
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
    if warn then Valarms.warn_pointer_subtraction with_alarms;
    let offs = conv minus_offs in
    V.inject_ival offs
  end

(* eval some operations on location_bytes. This function is more low-level
   than eval_binop, that evaluates the expressions in the given state. Here,
   we suppose someone else has done the evaluation, and combine the results.
   [te1] is the type of [ev1]. The function must behave as if it was acting on
   unbounded integers *)
let eval_binop_int ~with_alarms ~te1 ev1 op ev2 =
  let r = match op with
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
    | Eq | Ne | Ge | Le | Gt | Lt ->
      let warn = not (Warn.are_comparable op ev1 ev2) in
      if warn then Valarms.warn_pointer_comparison with_alarms;
      if warn && Value_parameters.UndefinedPointerComparisonPropagateAll.get ()
      then V.zero_or_one
      else
        let signed = Bit_utils.is_signed_int_enum_pointer(Cil.unrollType te1) in
        V.eval_comp ~signed op ev1 ev2
    | Shiftrt -> V.shift_right ev1 ev2
    | Shiftlt -> V.shift_left ev1 ev2
    (* Strict evaluation. The caller of this function is supposed to take
       into account the lazyness of those operators itself *)
    | LOr -> V.interp_boolean
        ~contains_zero:(V.contains_zero ev1 && V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 || V.contains_non_zero ev2)
    | LAnd -> V.interp_boolean
        ~contains_zero: (V.contains_zero ev1 || V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 && V.contains_non_zero ev2)
  in
  begin
    match r, ev1, ev2 with
    | V.Top _, V.Map _, V.Map _ ->
      Valarms.do_warn with_alarms.CilE.imprecision_tracing
        (fun () -> Value_parameters.warning ~once:true ~current:true
	  "Operation %a %a %a incurs a loss of precision"
	  V.pretty ev1 Printer.pp_binop op V.pretty ev2)
    | _ -> ()
  end;
  r

(* This function evaluates a unary minus, but does _not_ check for overflows.
   This is left to the caller *)
and eval_uneg ~with_alarms v t =
  match Cil.unrollType t with
  | TFloat (fkind, _) ->
      (try
          let v = V.project_ival_bottom v in
          let f = Ival.project_float v in
          V.inject_ival
            (Ival.inject_float (Fval.neg f))
        with
        | V.Not_based_on_null ->
            Warn.warn_float ~with_alarms ~addr:true (Some fkind) (pp_v v);
            V.topify_arith_origin v
        | Ival.Nan_or_infinite (* raised by project_float; probably useless*) ->
          if V.is_bottom v then v
          else begin
            Warn.warn_float ~with_alarms ~non_finite:true (Some fkind) (pp_v v);
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
  | BNot ->
      (try
          let v = V.project_ival v in
          V.inject_ival
            (Ival.apply_set_unary "~" Int.lognot v)
        with V.Not_based_on_null -> V.topify_arith_origin v)

  | LNot ->
      (* TODO:  on float, LNot is equivalent to == 0.0 *)
      let warn = not (Warn.are_comparable Eq V.singleton_zero v) in
      if warn then Valarms.warn_pointer_comparison with_alarms;
      if (warn &&
             Value_parameters.UndefinedPointerComparisonPropagateAll.get ())
        || not (Cil.isIntegralType t || Cil.isPointerType t)
      then
        V.zero_or_one
      else
        V.interp_boolean
          ~contains_zero:(V.contains_non_zero v)
          ~contains_non_zero:(V.is_included V.singleton_zero v)

let inv_binop_rel = function
  | Gt -> Le
  | Lt -> Ge
  | Le -> Gt
  | Ge -> Lt
  | Eq -> Ne
  | Ne -> Eq
  | _ -> assert false


let reduce_rel_int positive binop cond_expr value =
  if (Value_parameters.UndefinedPointerComparisonPropagateAll.get())
    && not (Warn.are_comparable binop value cond_expr)
  then value
  else
    match positive,binop with
    | false, Eq | true,  Ne -> V.diff_if_one value cond_expr
    | true,  Eq | false, Ne -> V.narrow value cond_expr
    | true,  (Le | Ge | Lt | Gt) ->
      V.filter_le_ge_lt_gt_int binop value ~cond_expr
    | false, (Le | Ge | Lt | Gt) ->
      V.filter_le_ge_lt_gt_int (inv_binop_rel binop) value ~cond_expr
    | _,_ -> value

let reduce_rel_float_double round fkind positive binop cond_expr value =
  match positive,binop with
    (* Shared with [int] case *)
    | false, Eq | true,  Ne -> V.diff_if_one value cond_expr
    | true,  Eq | false, Ne -> V.narrow value cond_expr
    (* Float functions *)
    | true,  (Le | Ge | Lt | Gt) ->
      V.filter_le_ge_lt_gt_float binop round fkind value ~cond_expr
    | false, (Le | Ge | Lt | Gt) ->
      let inv_binop = inv_binop_rel binop in
      V.filter_le_ge_lt_gt_float inv_binop round fkind value ~cond_expr
    | _,_ -> value

let reduce_rel_from_type t =
  match Cil.unrollType t with
  | TInt _ | TEnum _ | TPtr _ -> reduce_rel_int
  | TFloat (fk, _) ->
    reduce_rel_float_double
      (Value_parameters.AllRoundingModes.get ()) (Value_util.float_kind fk)
  | _ -> (fun _ _ _ v -> v) (* should never occur anyway *)

let eval_float_constant ~with_alarms f fkind fstring =
  let fl, fu = 
    match fstring with
    | Some string when Value_parameters.AllRoundingModesConstants.get ()->
      let parsed_f = Floating_point.parse_kind fkind string in
      parsed_f.Floating_point.f_lower, parsed_f.Floating_point.f_upper
    | None | Some _ -> f, f
  in
  let fl = Fval.F.of_float fl in
  let fu = Fval.F.of_float fu in
  try
    let non_finite, af = Fval.inject_r fl fu in
    let v = V.inject_ival (Ival.inject_float af) in
    if non_finite then begin
      Warn.warn_float ~with_alarms ~non_finite (Some fkind) (pp_v v)
    end;
    v
  with Fval.Non_finite ->
    Warn.warn_float ~with_alarms ~non_finite:true (Some fkind)
      (fun fmt -> Format.pp_print_string fmt "INFINITY");
    Valarms.do_warn with_alarms.CilE.others
      (fun _ ->
        Value_parameters.result ~current:true
          "Floating-point literal (or constant expression) is not \
          finite. This path is assumed to be dead.";
      );
    V.bottom

let make_volatile ?typ v =
  let is_volatile = match typ with
    | None -> true
    | Some typ -> Cil.typeHasQualifier "volatile" typ
  in
  if is_volatile && not (Cvalue.V.is_bottom v)
  then
    match v with
    | V.Top _ -> v
    | V.Map m ->
      let aux b _ acc = V.join acc (V.inject b Ival.top) in
      V.M.fold aux m V.bottom
  else v

let add_binding_unspecified ~with_alarms ?(remove_invalid=false) ~exact state loc value =
  let loc', reduced_loc =
    if remove_invalid then
      let loc' = Locations.valid_part ~for_writing:true loc in
      loc', not (Locations.Location.equal loc loc')
    else loc, false
  in
  let alarm, state = Model.add_binding_unspecified ~exact state loc' value in
  if alarm || reduced_loc then Valarms.warn_mem_write with_alarms;
  state

let add_binding ~with_alarms ?(remove_invalid=false) ~exact state loc value =
  let value = V_Or_Uninitialized.initialized value in
  add_binding_unspecified ~with_alarms ~remove_invalid ~exact state loc value

let copy_offsetmap ~with_alarms src_loc size mm =
  let alarm, r = Model.copy_offsetmap src_loc size mm in
  if alarm then Valarms.warn_mem_read with_alarms;
  r

let paste_offsetmap ~with_alarms ?(remove_invalid=false) ~reducing ~from ~dst_loc ~size ~exact m =
  let dst_loc, reduced_loc =
    if remove_invalid then
      let loc = Locations.make_loc dst_loc (Int_Base.inject size) in
      let for_writing = not reducing in
      let loc' = Locations.valid_part ~for_writing loc in
      let dst_loc' = loc'.Locations.loc in
      dst_loc', not (Locations.Location_Bits.equal dst_loc dst_loc')
    else dst_loc, false
  in
  let alarm, r =
    Cvalue.Model.paste_offsetmap ~reducing ~from ~dst_loc ~size ~exact m
  in
  if alarm || reduced_loc then Valarms.warn_mem_write with_alarms;
  r

let project_with_alarms ~with_alarms ~conflate_bottom loc v =
  let v_v = V_Or_Uninitialized.get_v v in
  (* Warn about indeterminateness only when [conflate_bottom] is true.
     Otherwise, the alarm [\initialized(loc)] or [\dangling_bits(loc)] may be
     emitted for padding bits, and will be unprovable. This is a bit of
     a hack, though. *)
  if conflate_bottom then ignore (Warn.maybe_warn_indeterminate ~with_alarms v);
  Warn.maybe_warn_completely_indeterminate ~with_alarms loc v v_v;
  v_v


let find ~with_alarms ?(conflate_bottom=true) state loc =
  let alarm, v = Model.find_unspecified ~conflate_bottom state loc in
  if alarm then Valarms.warn_mem_read with_alarms;
  project_with_alarms ~with_alarms ~conflate_bottom loc v


exception Unchanged
exception Reduce_to_bottom

let reduce_by_initialized_defined f loc state =
  try
    let base, offset =
      Locations.Location_Bits.find_lonely_key loc.Locations.loc
    in
    let size = Int_Base.project loc.Locations.size in
    let ll = Ival.project_int offset in
    let lh = Int.pred (Int.add ll size) in
    let offsm = match Model.find_base_or_default base state with
      | `Bottom | `Top -> raise Unchanged
      | `Map offsm -> offsm
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
    let _, value = Cvalue.Model.find state loc in
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


let write_abstract_value ~with_alarms state lv typ_lv loc_lv v =
  let v =
    if Cil.typeHasQualifier "volatile" typ_lv then
      make_volatile v (* Do not cast further, the offsetmap layer
                         prefers this form. *)
    else
      Eval_typ.cast_lval_if_bitfield typ_lv loc_lv.Locations.size v
  in
  match loc_lv.Locations.loc with
  | Locations.Location_Bits.Top (Base.SetLattice.Top, orig) ->
    Value_parameters.result
      "State before degeneration:@\n======%a@\n======="
      Cvalue.Model.pretty state;
    Value_util.warning_once_current
      "writing at a completely unknown address@[%a@].@\nAborting."
      Origin.pretty_as_reason orig;
    raise Db.Value.Aborted
  | _ ->
    let exact = Locations.cardinal_zero_or_one loc_lv in
    Valarms.set_syntactic_context (Valarms.SyMem lv);
    add_binding ~with_alarms ~exact state loc_lv v

(* Display [o] as a single value, when this is more readable and more precise
   than the standard display. *)
let pretty_stitched_offsetmap fmt typ o =
  if Cil.isArithmeticOrPointerType typ &&
     not (Cvalue.V_Offsetmap.is_single_interval o)
  then
    let v =
      v_uninit_of_offsetmap ~with_alarms:CilE.warn_none_mode ~typ o
    in
    if not (Cvalue.V_Or_Uninitialized.is_isotropic v)
    then
      Format.fprintf fmt "@\nThis amounts to: %a"
        Cvalue.V_Or_Uninitialized.pretty v

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
