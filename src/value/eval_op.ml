(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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


let is_bitfield typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt _] -> true
           | _ -> false)
    | _ -> false

let sizeof_lval_typ typlv =
  match Cil.unrollType typlv with
    | TInt (_, attrs) | TEnum (_, attrs) as t ->
        (match Cil.findAttribute Cil.bitfield_attribute_name attrs with
           | [AInt i] -> Int_Base.Value i
           | _ -> Bit_utils.sizeof t)
    | t -> Bit_utils.sizeof t


(* TODO: this should probably be also put directly in reinterpret_int *)
let cast_lval_bitfield typlv size v =
  match size with
    | Int_Base.Top -> v (* Bitfields have known sizes *)
    | Int_Base.Value size ->
        if is_bitfield typlv then
          let signed = Bit_utils.is_signed_int_enum_pointer typlv in
          let v, _ok = Cvalue.V.cast ~size ~signed v in
          v (* TODO: handle not ok case as a downcast *)
        else v


let reinterpret_int ~with_alarms ikind v =
  let size = Int.of_int (Cil.bitsSizeOfInt ikind) in
  let signed = Cil.isSigned ikind in
  let v', ok = V.cast ~signed ~size v in
  if not ok then
    CilE.do_warn with_alarms.CilE.imprecision_tracing
      (fun _ ->
         Kernel.warning ~once:true ~current:true
           "@[casting address@ to a type@ smaller@ than sizeof(void*):@ \
                  @[%a@]@]" V.pretty v
      );
  v'

let reinterpret_float ~with_alarms fkind v =
  let conv = match fkind with
    | FFloat ->
        let rounding_mode = Value_util.get_rounding_mode () in
        Cvalue.V.cast_float ~rounding_mode
    | FDouble -> Cvalue.V.cast_double
    | FLongDouble ->
        let mach = Cil.theMachine.Cil.theMachine in
        if mach.sizeof_longdouble <> mach.sizeof_double then
          Value_parameters.error ~once:true
            "type long double not implemented. Using double instead";
        Cvalue.V.cast_double
  in
  let addresses, overflow, r = conv v in
  if overflow || addresses
  then begin
    CilE.warn_nan_infinite with_alarms (Some fkind) (fun fmt -> V.pretty fmt v);
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
      (CilE.do_warn with_alarms.CilE.imprecision_tracing 
	 (fun _ ->
           Value_util.warning_once_current
             "cast to __builtin_va_list is not precisely implemented yet%t"
             Value_util.pp_callstack)
      );
      V.topify_arith_origin v
  | TComp _ | TArray _ | TFun _ ->
    (* Nothing can/should be done on struct and arrays, that are either already
       imprecise as a Cvalue.V, or read in a precise way. It is not clear
       that a TFun can be obtained here, but one never know. *)
    v
  | TNamed _ -> assert false
  | TVoid _ -> assert false


let v_uninit_of_offsetmap ~with_alarms ~typ offsm =
  let size = Bit_utils.sizeof typ in
  match size with
    | Int_Base.Top -> V_Offsetmap.find_imprecise_everywhere offsm
    | Int_Base.Value size ->
      let validity = Base.Known (Integer.zero, Integer.pred size) in
      let offsets = Ival.singleton_zero in
      V_Offsetmap.find
        ~with_alarms ~validity ~conflate_bottom:false ~offsets ~size offsm

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
      let addr, top, overflow, r =
        Cvalue.V.cast_float_to_int ~signed ~size v
      in
      Warn.warn_float ~with_alarms ~overflow:top ~addr (Some fkind) msg;
      if overflow
      then begin
        let dst_range = Ival.create_all_values ~modu:Int.one ~signed ~size in
        let mn, mx = Ival.min_and_max dst_range in
        (* TODO: Currently, we always emit the alarm for both sides *)
        CilE.warn_float_to_int_overflow with_alarms mn mx msg;
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
	         CilE.warn_integer_overflow with_alarms
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
      | V.Not_based_on_null | Ival.Float_abstract.Nan_or_infinite ->
          Ival.Float_abstract.top
  in
  let f1 = conv ev1 in
  let f2 = conv ev2 in
  let binary_float_floats (_name: string) f =
    try
      let alarm, f = f round f1 f2 in
      if alarm then
        CilE.warn_nan_infinite
          with_alarms flkind (fun fmt -> Ival.Float_abstract.pretty fmt f);
      V.inject_ival (Ival.inject_float f)
    with
      | Ival.Float_abstract.Nan_or_infinite ->
          CilE.warn_nan_infinite with_alarms flkind (pp_v V.top_int);
          V.top_float
      | Ival.Float_abstract.Bottom ->
          CilE.warn_nan_infinite with_alarms flkind (pp_v V.bottom);
          V.bottom
  in
  match op with
    | PlusA ->   binary_float_floats "+." Ival.Float_abstract.add_float
    | MinusA ->  binary_float_floats "-." Ival.Float_abstract.sub_float
    | Mult ->    binary_float_floats "*." Ival.Float_abstract.mult_float
    | Div ->     binary_float_floats "/." Ival.Float_abstract.div_float
    | Eq ->
        let contains_zero, contains_non_zero =
          Ival.Float_abstract.equal_float_ieee f1 f2
        in
        V.interp_boolean ~contains_zero ~contains_non_zero
    | Ne ->
        let contains_non_zero, contains_zero =
          Ival.Float_abstract.equal_float_ieee f1 f2
        in
        V.interp_boolean ~contains_zero ~contains_non_zero
    | Lt ->
        V.interp_boolean
          ~contains_zero:(Ival.Float_abstract.maybe_le_ieee_float f2 f1)
          ~contains_non_zero:(Ival.Float_abstract.maybe_lt_ieee_float f1 f2)
    | Le ->
        V.interp_boolean
          ~contains_zero:(Ival.Float_abstract.maybe_lt_ieee_float f2 f1)
          ~contains_non_zero:(Ival.Float_abstract.maybe_le_ieee_float f1 f2)
    | Gt ->
        V.interp_boolean
          ~contains_zero:(Ival.Float_abstract.maybe_le_ieee_float f1 f2)
          ~contains_non_zero:(Ival.Float_abstract.maybe_lt_ieee_float f2 f1)
    | Ge ->
        V.interp_boolean
          ~contains_zero:(Ival.Float_abstract.maybe_lt_ieee_float f1 f2)
          ~contains_non_zero:(Ival.Float_abstract.maybe_le_ieee_float f2 f1)
    | _ -> assert false

(* eval some operations on location_bytes. This function is more low-level
   than eval_binop, that evaluates the expressions in the given state. Here,
   we suppose someone else has done the evaluation, and combine the results.
   [te1] is the type of [ev1]. [typ] is optional. If it is not passed, the
   function must behave as if it was acting on unbounded integers *)
let eval_binop_int ~with_alarms ?typ ~te1 ev1 op ev2 =
  match op with
    | PlusPI | IndexPI -> V.add_untyped (Bit_utils.osizeof_pointed te1) ev1 ev2
    | MinusPI ->
        V.add_untyped (Int_Base.neg (Bit_utils.osizeof_pointed te1)) ev1 ev2
    | PlusA ->  V.add_untyped (Int_Base.one) ev1 ev2
    | MinusA -> V.add_untyped Int_Base.minus_one ev1 ev2
    | MinusPP ->
      if not (Value_parameters.WarnPointerSubstraction.get ()) then begin
        (* Generate garbled mix if the two pointers disagree on their base *)
        let minus_val = V.add_untyped Int_Base.minus_one ev1 ev2 in
        try
          let size = Int_Base.project (Bit_utils.sizeof_pointed te1) in
          let size = Int.div size Int.eight in
          if Int.is_one size then
            minus_val
          else
            let minus_val = Cvalue.V.project_ival minus_val in
            Cvalue.V.inject_ival (Ival.scale_div ~pos:true size minus_val)
        with
          | Int_Base.Error_Top
          | Cvalue.V.Not_based_on_null ->
            V.join (V.topify_arith_origin ev1) (V.topify_arith_origin ev2)
      end else begin
        (* Pointwise arithmetics.*)
        (* TODO: we may be able to reduce the bases that appear only on one
           side *)
        let minus_offs, warn = V.sub_untyped_pointwise ev1 ev2 in
        if warn then CilE.warn_pointer_subtraction with_alarms;
        let offs = 
        try
          let size = Int_Base.project (Bit_utils.sizeof_pointed te1) in
          let size = Int.div size Int.eight in
          if Int.is_one size
          then minus_offs
          else Ival.scale_div ~pos:true size minus_offs
        with Int_Base.Error_Top -> Ival.top
        in
        V.inject_ival offs
      end
    | Mod -> V.c_rem ~with_alarms ev1 ev2
    | Div -> V.div ~with_alarms ev1 ev2
    | Mult -> V.mul ~with_alarms ev1 ev2
    | BXor -> V.bitwise_xor ~with_alarms ev1 ev2
    | BOr -> V.bitwise_or ~with_alarms ev1 ev2
    | BAnd ->
        let size = Cil.bitsSizeOf te1 in
        let signed = Bit_utils.is_signed_int_enum_pointer te1 in
        V.bitwise_and ~size ~signed ev1 ev2

    | Eq | Ne | Ge | Le | Gt | Lt ->
      let warn = Warn.check_not_comparable op ev1 ev2 in
      if warn then CilE.warn_pointer_comparison with_alarms;
      if warn && Value_parameters.UndefinedPointerComparisonPropagateAll.get ()
      then V.zero_or_one
      else
        let signed = Bit_utils.is_signed_int_enum_pointer(Cil.unrollType te1) in
        V.eval_comp ~signed op ev1 ev2
    | Shiftrt | Shiftlt ->
	begin
	  let f = 
	    if op = Shiftlt then V.shift_left else V.shift_right
	  in
          let size = match typ with
            | None -> None
            | Some t -> 
                let t = Cil.unrollType t in
                let warn_negative = 
                  Value_parameters.WarnLeftShiftNegative.get() &&
                  Bit_utils.is_signed_int_enum_pointer t
                in
                Some (warn_negative, Cil.bitsSizeOf t)
	  in
          f ~with_alarms ~size ev1 ev2
	end

    (* Strict evaluation. The caller of this function is supposed to take
       into account the lazyness of those operators itself *)
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
  | TFloat (fkind, _) ->
      (try
          let v = V.project_ival v in
          let f = Ival.project_float v in
          V.inject_ival
            (Ival.inject_float (Ival.Float_abstract.neg_float f))
        with
        | V.Not_based_on_null ->
            Warn.warn_float ~with_alarms ~addr:true (Some fkind) (pp_v v);
            V.topify_arith_origin v
	| Ival.Float_abstract.Nan_or_infinite ->
            Warn.warn_float ~with_alarms ~overflow:true (Some fkind) (pp_v v);
            V.top_float
      )
  | _ ->
      try
        let v = V.project_ival v in
        V.inject_ival (Ival.neg v)
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
      let warn = Warn.check_not_comparable Eq V.singleton_zero v in
      if warn then CilE.warn_pointer_comparison with_alarms;
      if (warn &&
             Value_parameters.UndefinedPointerComparisonPropagateAll.get ())
        || not (Cil.isIntegralType t || Cil.isPointerType t)
      then
        V.zero_or_one
      else
        V.interp_boolean
          ~contains_zero:(V.contains_non_zero v)
          ~contains_non_zero:(V.is_included V.singleton_zero v)



let reduce_rel_symmetric_int positive binop cond_expr value =
  match positive,binop with
    | false, Eq | true,  Ne -> V.diff_if_one value cond_expr
    | true,  Eq | false, Ne -> 
      if (Value_parameters.UndefinedPointerComparisonPropagateAll.get())
         && Warn.check_not_comparable binop value cond_expr
      then value
      else V.narrow value cond_expr
    | _,_ -> value

let reduce_rel_symmetric_float = reduce_rel_symmetric_int

let reduce_rel_antisymmetric_int ~typ_loc:_ positive binop cond_expr value =
  match positive,binop with
    | true,  Le | false, Gt -> V.filter_le value ~cond_expr
    | true,  Ge | false, Lt -> V.filter_ge value ~cond_expr
    | false, Le | true,  Gt -> V.filter_gt value ~cond_expr
    | false, Ge | true,  Lt -> V.filter_lt value ~cond_expr
    | _,_ -> value

let reduce_rel_antisymmetric_float round ~typ_loc positive binop cond_expr value =
  match positive,binop with
    | true, Le | false, Gt -> V.filter_le_float round ~typ_loc value ~cond_expr
    | true, Ge | false, Lt -> V.filter_ge_float round ~typ_loc value ~cond_expr
    | false, Le | true, Gt -> V.filter_gt_float round ~typ_loc value ~cond_expr
    | false, Ge | true, Lt -> V.filter_lt_float round ~typ_loc value ~cond_expr
    | _,_ -> value


type reduce_rel_int_float = {
  reduce_rel_symmetric: bool -> binop -> V.t -> V.t -> V.t;
  reduce_rel_antisymmetric: typ_loc:typ -> bool -> binop -> V.t -> V.t -> V.t;
}

let reduce_rel_int = {
  reduce_rel_symmetric = reduce_rel_symmetric_int;
  reduce_rel_antisymmetric = reduce_rel_antisymmetric_int;
}

let reduce_rel_float round = {
  reduce_rel_symmetric = reduce_rel_symmetric_float;
  reduce_rel_antisymmetric = reduce_rel_antisymmetric_float round;
}

let eval_float_constant ~with_alarms f fkind fstring =
  let fl, fu = 
    match fstring with
    | Some string when Value_parameters.AllRoundingModesConstants.get ()->
      let parsed_f = Floating_point.parse_kind fkind string in
      parsed_f.Floating_point.f_lower, parsed_f.Floating_point.f_upper
    | None | Some _ -> f, f
  in
  let fl = Ival.F.of_float fl in
  let fu = Ival.F.of_float fu in
  try
    let overflow, af = Ival.Float_abstract.inject_r fl fu in
    let v = V.inject_ival (Ival.inject_float af) in
    if overflow then begin
      Warn.warn_float ~with_alarms ~overflow:true (Some fkind) (pp_v v)
    end;
    v
  with Ival.Float_abstract.Bottom ->
    Warn.warn_float ~with_alarms ~overflow:true (Some fkind)
      (fun fmt -> Format.pp_print_string fmt "INFINITY");
    Value_parameters.result ~current:true
      "Floating-point literal (or constant expression) is not \
      finite. This path is assumed to be dead.";
    V.bottom

let light_topify v =
  match v with
  | V.Top _ -> v
  | V.Map m ->
    let aux b _ acc = V.join acc (V.inject b Ival.top) in
    V.M.fold aux m V.bottom

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
