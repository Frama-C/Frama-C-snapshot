(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Cil
open Ast_printer
open Abstract_interp
open Locations
open Cvalue
open Bit_utils
open Value_util


exception Distinguishable_strings

exception Not_an_exact_loc

exception Reduce_to_bottom

exception Offset_not_based_on_Null of
          Locations.Zone.t option * Location_Bytes.t

exception Cannot_find_lv

exception Too_linear


type cond =
    { exp: exp; (* The condition of the branch*)
      positive: bool; (* true: normal false: negated *)}

(* Returns boolean telling to display a warning,
   and optionally reduce the values of [ev1] and [ev2],
   knowing that they are involved in a comparison *)
(* TODO: this function currently never reduces ev1 and ev2 *)
let check_comparable op ev1 ev2 =
  try
    if not (Location_Bytes.is_included ev1 Location_Bytes.top_int)
      || not (Location_Bytes.is_included ev2 Location_Bytes.top_int)
    then begin
        let null_1, rest_1 = Location_Bytes.split Base.null ev1 in
        let null_2, rest_2 = Location_Bytes.split Base.null ev2 in
        let is_bottom1 = Location_Bytes.is_bottom rest_1 in
        let is_bottom2 = Location_Bytes.is_bottom rest_2 in

        (* First check if a non-zero integer is compared to an address *)
        if  ((not (Ival.is_included null_1 Ival.zero)) && (not is_bottom2))
         || ((not (Ival.is_included null_2 Ival.zero)) && (not is_bottom1))
        then raise Not_found;

        if not (is_bottom1 && is_bottom2)
        then begin
            let loc_bits1 = loc_bytes_to_loc_bits rest_1 in
            let loc_bits2 = loc_bytes_to_loc_bits rest_2 in
            let single_base_ok =
              begin try
                (* If they are both in the same base and both almost valid,
                   it's also fine, but beware of empty rest for comparisons
                   to NULL, or of function pointers *)
                let extract_base is_bot loc =
                  if is_bot then Base.null
                  else begin
                    let base, offs = Location_Bits.find_lonely_key loc in
                    if Base.is_function base then
                      (if not (Ival.equal Ival.zero offs)
                       then raise Base.Not_valid_offset)
                    else
                      Base.is_valid_offset ~for_writing:false 
			Int.zero base offs;
                    base
                  end
                in
                let base_1 = extract_base is_bottom1 loc_bits1
                and base_2 = extract_base is_bottom2 loc_bits2
                in
                  is_bottom1 || is_bottom2 || (Base.equal base_1 base_2)
                with
                  Not_found -> false
              end
            in
            if not single_base_ok
            then begin
                if op = Eq || op = Ne
                then begin
                    (* If both addresses are valid, they can be compared 
		       for equality. *)
                    let loc1 = make_loc loc_bits1 Int_Base.one in
                    let loc2 = make_loc loc_bits2 Int_Base.one in
                    if (not (Locations.is_valid_or_function loc1)) ||
                      (not (Locations.is_valid_or_function loc2))
                    then raise Not_found;
                    (* But wait! literal strings can only be compared 
		       if their contents are recognizably different! 
		       (or the strings are physically the same) *)
                    Locations.Location_Bytes.iter_on_strings
                      ~skip:None
                      (fun base1 s1 offs1 len1 ->
                        Locations.Location_Bytes.iter_on_strings
                          ~skip:(Some base1)
                          (fun _ s2 offs2 len2 ->
                            let delta = offs1-offs2 in
                            begin
                              try
                                let start = if delta <= 0 then (-delta) else 0
                                in
                                for i = start to min len2 (len1 - delta)
                                do
(*                                Format.printf "%S %S %d %d@."
                                    s1 s2 i delta; *)
                                  if s2.[i] <> s1.[i + delta]
                                  then raise Distinguishable_strings;
                                done;
                                raise Not_found
                              with Distinguishable_strings -> ();
                            end)
                          rest_1)
                      rest_2
                  end
                else raise Not_found
              end
          end
      end;
    false, ev1, ev2
  with Not_found | Base.Not_valid_offset ->
    true, ev1, ev2

let do_cast ~with_alarms t expr =
  let treat inttype =
    match inttype with
    | TInt(kind,_) ->
        let size = Int.of_int (bitsSizeOf inttype) in
        let signed = isSigned kind in
        V.cast ~with_alarms ~signed ~size expr
    | TFloat (FFloat,_) ->
        let addresses, overflow, res = 
	  V.cast_float ~rounding_mode:(get_rounding_mode()) expr 
	in
        if addresses
        then warning_once_current
          "addresses in float";
        if overflow then CilE.warn_float_overflow with_alarms
          (fun () -> Pretty_utils.sfprintf "%a" V.pretty expr);
        res
    | TFloat (FDouble,_)
    | TFloat (FLongDouble,_) -> (* TODO: make behavior more symetrical with
                                   float case above *)
      expr
    | _ -> assert false
  in
  match unrollType t with
  | TInt _ | TFloat _  as t' ->
      treat t'
  | TPtr _ ->
      treat theMachine.upointType
  | TEnum ({ekind=k},_) -> treat (TInt(k,[]))
  | TComp _ -> expr (* see test [struct_call.c] *)
  | TBuiltin_va_list _ ->
      (match with_alarms.CilE.imprecision_tracing with
       | CilE.Aignore -> ()
       | CilE.Acall f -> f ()
       | CilE.Alog _ ->
           warning_once_current
             "cast to __builtin_va_list is not precisely implemented yet");
      V.topify_arith_origin expr
  | TFun _ -> expr
  | TNamed _ -> assert false
  | TVoid _ -> assert false
  | TArray _ -> assert false

let do_promotion ~with_alarms ~src_typ ~dest_type v e_src =
  match Cil.unrollType dest_type, Cil.unrollType src_typ with
  | TFloat _, TInt _ ->
      Cvalue.V.cast_int_to_float ~with_alarms (get_rounding_mode()) v
  | TInt (kind,_), TFloat _ ->
      let size = bitsSizeOf dest_type in
      let signed = isSigned kind in
      let alarm_use_as_float, alarm_overflow, r =
        Cvalue.V.cast_float_to_int ~signed ~size v
      in
      if alarm_use_as_float
      then begin
          warning_once_current
            "converting %a to float: assert(Ook)"
            !d_exp e_src;
        end;
      if alarm_overflow
      then
        warning_once_current
          "Overflow in cast of %a (%a) from floating-point to integer: assert(Ook)"
          !d_exp e_src
          Cvalue.V.pretty v;
      r
  | _, _ -> v

let handle_signed_overflow ~with_alarms typ interpreted_e =
  match unrollType typ with
    TInt(kind, _)
      when isSigned kind ->
          let size = bitsSizeOf typ in
          let mn, mx =
            let b = Int.power_two (size-1) in
            Int.neg b, Int.pred b
          in
          let mn64 = Int.to_int64 mn in
          let mx64 = Int.to_int64 mx in
          let warn_under, warn_over =
            try
              let i = V.project_ival interpreted_e in
              let imn, imx = Ival.min_and_max i in
              let u =
                match imn with
                  Some bound when Int.ge bound mn -> None
                | _ -> Some mn64
              in
              let o =
                match imx with
                  Some bound when Int.le bound mx -> None
                | _ -> Some mx64
              in
              u, o
            with V.Not_based_on_null ->
              Some mn64, Some mx64
          in
	  ( match warn_under, warn_over with
		None, None ->
		  interpreted_e
	      | _ ->
		  if Value_parameters.SignedOverflow.get()
		  then 
		      let all_values =
			Cvalue.V.inject_ival 
			  (Ival.inject_range (Some mn) (Some mx))
		      in
		      CilE.warn_signed_overflow
			with_alarms
			warn_under
			warn_over;
		      V.narrow all_values interpreted_e 
		  else 
		    ( warning_once_current 
			"2's complement assumed for overflow";
		      interpreted_e ) )
  | _ -> interpreted_e

let warn_imprecise_lval_read ~with_alarms lv loc contents =
  if with_alarms.CilE.imprecision_tracing != CilE.Aignore
  then
  let pretty_param fmt param =
    match param with
    | Location_Bits.Top_Param.Top -> Format.fprintf fmt "is imprecise"
    | Location_Bits.Top_Param.Set _s ->
        Format.fprintf fmt "is a garbled mix of %a"
          Location_Bits.Top_Param.pretty param
  in
  let pretty_param_b fmt param =
    match param with
    | Location_Bytes.Top_Param.Top ->
        Format.fprintf fmt "The contents@ are imprecise"
    | Location_Bytes.Top_Param.Set _s ->
          Format.fprintf fmt "It contains@ a garbled@ mix@ of@ %a"
            Location_Bytes.Top_Param.pretty param
  in
  let something_to_warn =
    match loc.loc with Location_Bits.Top _ -> true
      | Location_Bits.Map _ ->
          match contents with
          | Location_Bytes.Top _ -> true
          | Location_Bytes.Map _ -> false
  in
  match something_to_warn, with_alarms.CilE.imprecision_tracing with
    | false, _ | true, CilE.Aignore -> ()
    | true, CilE.Acall f -> f ()
    | true, CilE.Alog _ ->
    Value_parameters.result ~current:true ~once:true
      "@[<v>@[Reading left-value %a.@]@ %t%t%t@]"
      !Ast_printer.d_lval lv
      (fun fmt ->
         match lv with
         | Mem _, _ ->
             (match loc.loc with
             | Location_Bits.Top (param,o) when Origin.equal o Origin.top  ->
                 Format.fprintf fmt "@[The location %a.@]@ "
                   pretty_param param
             | Location_Bits.Top (param,orig) ->
                 Format.fprintf fmt "@[The location @[%a@]@ because of@ %a.@]@ "
                   pretty_param param
                   Origin.pretty orig
             | Location_Bits.Map _ ->
                 Format.fprintf fmt "@[The location is @[%a@].@]@ "
                   Location_Bits.pretty loc.loc)
         | Var _, _ -> ())
      (fun fmt ->
         match contents with
         | Location_Bytes.Top (param,o) when Origin.equal o Origin.top ->
                 Format.fprintf fmt "@[%a.@]"
                   pretty_param_b param
         | Location_Bytes.Top (param,orig) ->
             Format.fprintf fmt "@[%a@ because of@ %a.@]"
               pretty_param_b param
               Origin.pretty orig
         | Location_Bytes.Map _ -> ())
      pp_callstack

let eval_binop_float ~with_alarms ev1 op ev2 = 
  try
    let conv v = 
      try Ival.project_float (V.project_ival v)
      with
        | V.Not_based_on_null
        | Ival.Float_abstract.Nan_or_infinite ->
            warning_once_current "converting value to float: assert(Ook)";
            Ival.Float_abstract.top
    in
    let f1 = conv ev1
    and f2 = conv ev2
    in
    let binary_float_floats (_name: string) f =
      try
        let alarm, f = f (get_rounding_mode ()) f1 f2 in
        if alarm then CilE.warn_result_nan_infinite with_alarms;
        V.inject_ival (Ival.inject_float f)
      with
        | Ival.Float_abstract.Nan_or_infinite ->
            CilE.warn_result_nan_infinite with_alarms ;
            V.top_float
        | Ival.Float_abstract.Bottom ->
            CilE.warn_result_nan_infinite with_alarms ;
            V.bottom
    in
    begin match op with
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
      | _ -> raise V.Not_based_on_null
    end
  with V.Not_based_on_null | Ival.F.Nan_or_infinite ->
    assert false (* because reading from a float lvalue 
	can only make float values *)

(* eval some operations on location_bytes. This function is more low-level
   than eval_binop, that evaluates the expressions in the given state. Here,
   we suppose someone else has done the evaluation, and combine the results.
   The expressions are passed in order to have the types of the expressions
   at hand; hopefully they are correct. [typ] is optional. If it is not
   passed, the function must behave as if it was acting on unbounded integers *)
let eval_binop_int ~with_alarms ?typ ~te1 ev1 op ev2 =
  match op with
    | PlusPI | IndexPI -> V.add_untyped (osizeof_pointed te1) ev1 ev2
    | MinusPI -> V.add_untyped (Int_Base.neg (osizeof_pointed te1)) ev1 ev2
    | PlusA ->  V.add_untyped (Int_Base.one) ev1 ev2
    | MinusA -> V.add_untyped Int_Base.minus_one ev1 ev2
    | MinusPP ->
      let minus_val = V.add_untyped Int_Base.minus_one ev1 ev2 in
      begin
        try
          let size = Int_Base.project (sizeof_pointed te1) in
          let size = Int.div size Int.eight in
          if Int.is_one size then
            minus_val
          else
            let minus_val = Cvalue.V.project_ival minus_val in
            Cvalue.V.inject_ival (Ival.scale_div ~pos:true size minus_val)
        with
          | Int_Base.Error_Top
          | Cvalue.V.Not_based_on_null
          | Not_found ->
            V.join (V.topify_arith_origin ev1) (V.topify_arith_origin ev2)
      end
    | Mod -> V.c_rem ~with_alarms ev1 ev2
    | Div -> V.div ~with_alarms ev1 ev2
    | Mult -> V.arithmetic_function ~with_alarms "*" Ival.mul ev1 ev2
    | BXor ->
	V.oper_on_values ~with_alarms "^" Int.logxor ev1 ev2
    | BOr ->
        V.bitwise_or ~size:(bitsSizeOf te1) ev1 ev2
    | BAnd ->
      (try
         let size = bitsSizeOf te1 in
         let signed = is_signed_int_enum_pointer te1 in
         V.bitwise_and ~size ~signed ev1 ev2
       with SizeOfError _ ->
	 assert false) (* an expression is supposed to have a type with a size *)

    | Eq | Ne | Ge | Le | Gt | Lt ->
      let warn, ev1, ev2 = check_comparable op ev1 ev2 in
      if warn then CilE.warn_pointer_comparison with_alarms;
      if warn && Value_parameters.UndefinedPointerComparisonPropagateAll.get ()
      then V.zero_or_one
      else
        let signed = is_signed_int_enum_pointer (unrollType te1) in
        let f = match op with
          | Eq -> V.check_equal true
          | Ne -> V.check_equal false
          | Ge -> V.comparisons ">=" ~signed V.do_ge
          | Le -> V.comparisons "<=" ~signed V.do_le
          | Gt -> V.comparisons ">" ~signed V.do_gt
          | Lt -> V.comparisons "<" ~signed V.do_lt
          | _ -> assert false
        in
        f ev1 ev2
    | Shiftrt | Shiftlt ->
	begin
	  let f = 
	    if op = Shiftlt then V.shift_left else V.shift_right
	  in
	  try
            let size = 
	      match typ with
		None -> None
	      | Some t -> 
		  let t = unrollType t in
		  let warn_negative = 
		    Value_parameters.LeftShiftNegative.get() &&
		    is_signed_int_enum_pointer t
		  in
		  Some (warn_negative, bitsSizeOf t)
	    in
            f ~with_alarms ?size ev1 ev2
          with SizeOfError _ -> assert false	  
	end

    (* Strict evaluation. The caller of this function is supposed to take
       into account the lazyness of those operators itself *)
    | LOr -> V.interp_boolean
        ~contains_zero:(V.contains_zero ev1 && V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 || V.contains_non_zero ev2)
    | LAnd -> V.interp_boolean
        ~contains_zero: (V.contains_zero ev1 || V.contains_zero ev2)
        ~contains_non_zero:(V.contains_non_zero ev1 && V.contains_non_zero ev2)


(* TODO: A version that does not create bigints would be better *)
let bitfield_size_lv lv = sizeof (typeOfLval lv)
let bitfield_size_bf lv = Bit_utils.sizeof_lval lv
let is_bitfield lv ?(sizelv=bitfield_size_lv lv) ?(sizebf=(bitfield_size_bf lv)) () =
  not (Int_Base.equal sizelv sizebf)

let rec lval_to_loc ~with_alarms state lv =
  snd (lval_to_loc_state ~with_alarms state lv)

and lval_to_loc_state ~with_alarms state lv =
  let state,_,r =
    lval_to_loc_deps_option
      ~with_alarms
      ~deps:None
      ~reduce_valid_index:(Kernel.SafeArrays.get ())
      state
      lv
  in
  state, r

and lval_to_loc_deps_option
    ~with_alarms ~deps state ~reduce_valid_index (base,offset as lv)
    =
  if not (Cvalue.Model.is_reachable state) then
    state, deps, loc_bottom
  else
    let typ = match base with
    | Var host -> host.vtype
    | Mem x -> typeOf x
    in
    try
      let state, deps, offs =
        eval_offset
          ~reduce_valid_index
          ~with_alarms deps typ state offset
      in
      base_to_loc ~with_alarms ?deps state lv base offs
    with Offset_not_based_on_Null(deps,offset) ->
      let state, deps, loc_if_there_wasnt_offset =
        base_to_loc ~with_alarms ?deps state lv base Ival.zero
      in
      state, deps,
      loc_bits_to_loc lv
        (Location_Bits.join
            (loc_bytes_to_loc_bits offset)
            loc_if_there_wasnt_offset.loc)

(* pc says: only called in addrOf *)
and lval_to_loc_with_offset_deps_only ~deps state v =
  lval_to_loc_with_offset_deps_only_option ~deps:(Some deps) state v

and lval_to_loc_with_deps ~deps state lv ~with_alarms =
  lval_to_loc_deps_option ~with_alarms ~deps:(Some deps) state lv

(* pc says: only called in addrOf *)
and lval_to_loc_with_offset_deps_only_option
    ~with_alarms ~deps (state:Cvalue.Model.t) (_base, _offset as v)
    =
  lval_to_loc_deps_option
    ~with_alarms ~deps (state:Cvalue.Model.t) (v)
    ~reduce_valid_index:false


(** Detects if an expression can be considered as a lvalue even though
    it is hidden by a cast that does not change the lvalue.
    Raises [exn] if it is not an lvalue.

    TODO: When the goal is to recognize the form (cast)l-value == expr,
    it would be better and more powerful to have chains of inverse functions *)

and pass_cast ~with_alarms state exn typ e =
  try
    let typeofe = typeOf e in
    (* Any volatile attribute may have an effect on the expression value *)
    if hasAttribute "volatile" (typeAttrs typeofe)
      || hasAttribute  "volatile" (typeAttrs typ)
    then raise exn;
    let sztyp = sizeof typ in
    let szexpr = sizeof typeofe in
    let styp, sexpr =
      match sztyp,szexpr with
        | Int_Base.Value styp, Int_Base.Value sexpr -> styp, sexpr
        | _ -> raise exn
    in
    let sityp = is_signed_int_enum_pointer typ in
    let sisexpr = is_signed_int_enum_pointer typeofe in
    if (Int.ge styp sexpr && sityp = sisexpr) (* larger and same signedness *)
      || (Int.gt styp sexpr && sityp) (* strictly larger and signed *)	
    then ()
    else (* try to ignore the cast if it acts as identity on the value [e] *)
      let size = bitsSizeOf typ in
      if 
	not (V.is_included 
		(eval_expr ~with_alarms state e) 
		(V.create_all_values ~size ~signed:sityp ~modu:My_bigint.one))
      then raise exn
  with Neither_Int_Nor_Enum_Nor_Pointer -> raise exn

and find_lv ~with_alarms (state:Cvalue.Model.t) ee =
  (* [BM] Do not recognize an lval whenever a volatile is involved to
     prevent copy/paste optimization. IS THIS THE RIGHTPLACE PC ?*)
  if hasAttribute "volatile" (typeAttrs (typeOf ee)) then
    raise Cannot_find_lv;
  match ee.enode with
  | Lval lv -> lv
  | CastE (typ,e) ->
      ( match unrollType typ, unrollType (typeOf e) with
        TFloat (FDouble,_), TFloat _ -> find_lv ~with_alarms state e
          (* see remark at pass_cast about inverse functions *)
      | _ ->
          pass_cast ~with_alarms state Cannot_find_lv typ e;
          find_lv ~with_alarms state e)
  | _ -> raise Cannot_find_lv

(** If possible, decomposes [e] into [lval+offset]; where [lval] is a Cil
    expression, and [offset] is an Ival.t, in bytes.

    @raises Cannot_find_lv if the expression cannot be decomposed *)
and find_lv_plus_offset ~with_alarms state e =
  let acc = ref None in
  let rec aux e current_offs =
    try
      let lv = find_lv ~with_alarms state e in
      if not (hasAttribute "volatile" (typeAttrs (Cil.typeOfLval lv)))
      then acc := Some (lv,current_offs)
    with Cannot_find_lv ->
      match e.enode with
      | BinOp((MinusPI|PlusPI|IndexPI as op), p, offs, typ) ->
          let offs = eval_expr ~with_alarms state offs in
          (try
              let offs = V.project_ival offs in
              let offs =
                Ival.scale (Int_Base.project (osizeof_pointed typ)) offs in
              let offs = if op = MinusPI then Ival.neg offs else offs in
              aux p (Ival.add_int current_offs offs)
            with V.Not_based_on_null | Int_Base.Error_Top-> ());
      | CastE(typ,e) ->
          (try
              pass_cast ~with_alarms state Cannot_find_lv typ e;
              aux e current_offs
            with Cannot_find_lv -> ())
      | _ -> ()
  in
  aux e Ival.zero;
  (* Extlib.may
     (fun (lv,ival) -> Format.printf "find_lv_plus %a=%a+%a\n"
     !d_exp e !d_lval lv Ival.pretty ival
     ) !acc; *)
  match !acc with
  | None -> raise Cannot_find_lv
  | Some (lv, offs) -> lv, offs

and base_to_loc ~with_alarms ?deps state lv base offs =
  if Ival.is_bottom offs
  then begin
      Cvalue.Model.bottom,
    (Some Zone.bottom),
    loc_bits_to_loc lv Location_Bits.bottom
    end
  else
    let result = match base with
    | Var host ->
        let base = Base.find host in
        state, deps,
        loc_bits_to_loc lv (Location_Bits.inject base offs)
    | Mem x ->
        let state, deps, loc_lv =
          eval_expr_with_deps_state ~with_alarms deps state x
        in
        let loc_bits =
          Location_Bits.location_shift
            offs
            (loc_bytes_to_loc_bits loc_lv)
        in
        state, deps, loc_bits_to_loc lv loc_bits
    in
    CilE.set_syntactic_context (CilE.SyMem lv);
    result

and eval_expr ~with_alarms state e =
  snd (eval_expr_with_deps ~with_alarms None state e)

and get_influential_vars ~with_alarms state cond =
  (*  Format.printf "get_influential cond:%a@.state:%a@."
      !d_exp cond
      Cvalue.Model.pretty state; *)
  let rec get_vars acc cond =
    let eval_offset off t =
      try
        let _, _, offset =
          eval_offset ~reduce_valid_index:true ~with_alarms None t state off
        in
        offset
      with Offset_not_based_on_Null _ -> Ival.top
    in
    match cond.enode with
    | Lval (Var v, off as lv) ->
        let offset = eval_offset off v.vtype in
        if Ival.cardinal_zero_or_one offset
        then
          (* no variable in offset can be influential *)
          let varid = Base.create_varinfo v in
          let loc =
            Locations.make_loc
              (Locations.Location_Bits.inject varid offset)
              (sizeof_lval lv)
          in
          let contents =
            Cvalue.Model.find ~conflate_bottom:true state ~with_alarms loc
          in
          if Location_Bytes.cardinal_zero_or_one contents
          then acc (* it's not influential *)
          else loc :: acc
        else
          (* a variable in offset can be influential *)
          get_vars_offset acc off
    | Lval (Mem e, off as lv) ->
        let t = typeOf_pointed (typeOf e) in
        let offset = eval_offset off t in
        if Ival.cardinal_zero_or_one offset then
          let v = eval_expr ~with_alarms state e in
          if Location_Bytes.cardinal_zero_or_one v then
            let locbi = loc_bytes_to_loc_bits v in
            let locbi' = Location_Bits.location_shift offset locbi in
            let loc = Locations.make_loc locbi' (sizeof_lval lv) in
            loc :: acc
          else get_vars acc e
        else
          (* variables in expr or offset can be influential *)
          get_vars_offset (get_vars acc e) off
    | BinOp(_,v1,v2,_) ->
        get_vars (get_vars acc v1) v2
    | UnOp(_,v1,_) ->
        get_vars acc v1
    | CastE (_typ,exp) ->
        get_vars acc exp
    | _ -> acc
  and get_vars_offset acc offset =
    match offset with
      NoOffset -> acc
    | Field (_,off) -> get_vars_offset acc off
    | Index (ind,off) -> get_vars (get_vars_offset acc off) ind
  in
  get_vars [] cond

and reduce_by_valid_loc ~positive ~for_writing loc typ state =
  try
    let value = Cvalue.Model.find ~with_alarms:CilE.warn_none_mode
      ~conflate_bottom:true state loc
    in
    if Cvalue.V.is_imprecise value then
      (* we won't reduce anything anyway, and we may lose information if loc
         contains misaligned data *)
      raise Cannot_find_lv;
    let value_as_loc =
      make_loc (loc_bytes_to_loc_bits value) (sizeof_pointed typ)
    in
    let reduced_value =
      loc_to_loc_without_size
        (if positive
          then valid_part ~for_writing value_as_loc
          else invalid_part value_as_loc )
    in
    if Location_Bytes.equal value reduced_value
    then state
    else begin
	if Location_Bytes.equal Location_Bytes.bottom reduced_value
	then Cvalue.Model.bottom
	else
          Cvalue.Model.reduce_previous_binding ~with_alarms:CilE.warn_none_mode
            state loc reduced_value
      end
  with Cannot_find_lv -> state

and reduce_by_initialized_defined f (typ, loc) state =
  let loc_bits = loc_bytes_to_loc_bits loc in
  let size = match unrollType typ with
  | TPtr (t, _) -> Int.of_int (bitsSizeOf t)
  | _ -> assert false
  in
  Cvalue.Model.reduce_by_initialized_defined_loc f loc_bits size state

and eval_BinOp ~with_alarms e deps state =
  match e.enode with
  | BinOp (op, e1, e2, typ) ->
      let state, deps, ev1 =
        eval_expr_with_deps_state ~with_alarms deps state e1
      in
      if V.is_bottom ev1
      then Cvalue.Model.bottom, (Some Zone.bottom) ,V.bottom
      else
        let state, deps, ev2 =
          eval_expr_with_deps_state ~with_alarms deps state e2
        in
        if V.is_bottom ev2
        then Cvalue.Model.bottom, (Some Zone.bottom) ,V.bottom
        else begin
            begin match unrollType (typeOf e1) with
            | TFloat _ ->
                CilE.set_syntactic_context (CilE.SyUnOp e);
                let r = eval_binop_float ~with_alarms ev1 op ev2 in
                state, deps, r
            | TInt _ | TPtr (_, _) | _ as te1 ->
		CilE.set_syntactic_context (CilE.SyBinOp (op, e1, e2));
		let v = eval_binop_int
                  ~with_alarms ~typ ~te1 ev1 op ev2 in
		(* Warn if overflow in a signed int binop *)
		let v = match op with
                | Shiftlt | Mult | MinusPP | MinusPI | IndexPI | PlusPI
                | PlusA | Div | Mod | MinusA ->
                    handle_signed_overflow ~with_alarms typ v
                | _ -> v
		in
		state, deps, v
            end
          end
  | _ -> assert false

and eval_expr_with_deps ~with_alarms deps (state : Cvalue.Model.t) e =
  let _,deps,r = eval_expr_with_deps_state ~with_alarms deps state e in
  deps, r

and eval_expr_with_deps_state ~with_alarms deps state e =
  let state, deps, r =
    let orig_expr = Cil.stripInfo e in
    match orig_expr.enode with
    | Info _ -> assert false
    | Const v ->
        let r =
          begin match v with
          | CInt64 (i,_k,_s) ->
              V.inject_int i
          | CChr c ->
              (match charConstToInt c with
              | CInt64 (i,_,_) -> V.inject_int i
              | _ -> assert false)
          | CReal (f, _fsize, _) ->
              let f = Ival.F.of_float f in
              let overflow, af =
                try
                  let o, af = Ival.Float_abstract.inject_r f f in
                  o, V.inject_ival (Ival.inject_float af)
                with Ival.Float_abstract.Bottom ->
                  Value_parameters.result ~current:true
                    "Floating-point literal (or constant expression) is not finite. This path is assumed to be dead.";
                  true, V.bottom
              in
              if overflow
              then Value_parameters.result "overflow in constant: assert(Ook);";
              af
          | CWStr _ | CStr _ ->
              V.inject (Base.create_string orig_expr) Ival.zero
          | CEnum {eival = e} ->
              eval_expr ~with_alarms state e
          end
        in
        state, deps, r
    | BinOp _  ->
        eval_BinOp ~with_alarms orig_expr deps state
    | Lval lv ->
        eval_lval_and_convert ~with_alarms deps state (lv, e)
    | AddrOf v | StartOf v ->
        let state, deps, r =
          lval_to_loc_with_offset_deps_only_option ~with_alarms ?deps state v
        in
        state, deps, loc_to_loc_without_size r

    | CastE (typ, e) ->
        let state, deps, evaled_expr =
          eval_expr_with_deps_state ~with_alarms deps state e
        in
        let r = do_promotion ~with_alarms
          ~dest_type:typ ~src_typ:(typeOf e) evaled_expr e in
        state, deps, r

    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
        let e = Cil.constFold true orig_expr in
        let r = match e.enode with
        | Const (CInt64 (v, _, _)) -> Cvalue.V.inject_int v
        | _ ->
            Value_parameters.error ~current:true
              "cannot interpret sizeof or alignof (incomplete type)";
            V.top_int
        in
        state, deps, r

    | UnOp (op, e, _t_res) ->
        let t = unrollType (typeOf e) in
        let state, deps, expr =
          eval_expr_with_deps_state ~with_alarms deps state e in
        let syntactic_context = match op with
        | Neg -> CilE.SyUnOp orig_expr (* Can overflow *)
        | BNot -> CilE.SyUnOp orig_expr (* does in fact never raise an alarm*)
        | LNot -> CilE.SyBinOp (Eq, Cil.zero ~loc:e.eloc, e)
            (* Can raise a pointer comparison. CilE needs a binop there *)
        in
        CilE.set_syntactic_context syntactic_context;
        let result = eval_unop ~check_overflow:true ~with_alarms expr t op in
        state, deps, result
  in
  let r =
    if hasAttribute "volatile" (typeAttrs (typeOf e))
      && not (Cvalue.V.is_bottom r)
    then V.top_int
    else
      r
  in
  let typ = typeOf e in
  CilE.set_syntactic_context (CilE.SyUnOp e);
  let rr = do_cast ~with_alarms typ r in
  (*  ( match typ with
      TInt _ when not (V.equal r rr || V.is_topint r) ->
      warning_once_current
      "downcast %a -> %a@." V.pretty r V.pretty rr
      | _ -> ()); *)
  (if Cvalue.V.is_bottom rr then Cvalue.Model.bottom else state), deps, rr

(* This function evaluates a unary minus, but does _not_ check for overflows.
   This is left to the caller *)
and eval_uneg_exp ~with_alarms expr t =
  match unrollType t with
  | TFloat _ ->
      (try
          let v = V.project_ival expr in
          let f = Ival.project_float v in
          V.inject_ival
            (Ival.inject_float (Ival.Float_abstract.neg_float f))
        with
          V.Not_based_on_null ->
            begin match with_alarms.CilE.others with
            | CilE.Aignore -> ()
            | CilE.Acall f -> f()
            | CilE.Alog _ ->
                warning_once_current
                  "converting address to float: assert(TODO)"
            end;
            V.topify_arith_origin expr
	| Ival.Float_abstract.Nan_or_infinite ->
            begin match with_alarms.CilE.others with
            | CilE.Aignore -> ()
            | CilE.Acall f -> f()
            | CilE.Alog _ ->
                warning_once_current
                  "converting value to float: assert (TODO)"
            end;
            V.top_float
      )
  | _ ->
      try
        let v = V.project_ival expr in
        V.inject_ival (Ival.neg v)
      with V.Not_based_on_null -> V.topify_arith_origin expr

and eval_unop ~check_overflow ~with_alarms expr t op =
  match op with
  | Neg ->
      let r = eval_uneg_exp ~with_alarms expr t in
      if check_overflow
      then handle_signed_overflow ~with_alarms t r
      else r
  | BNot ->
      (try
          let v = V.project_ival expr in
          V.inject_ival
            (Ival.apply_set_unary "~" Int.lognot v)
        with V.Not_based_on_null -> V.topify_arith_origin expr)

  | LNot ->
      (* TODO:  on float, LNot is equivalent to == 0.0 *)
      let warn, _, expr = check_comparable Eq V.singleton_zero expr in
      if warn then CilE.warn_pointer_comparison with_alarms;
      if (warn &&
             Value_parameters.UndefinedPointerComparisonPropagateAll.get ())
        || not (isIntegralType t || isPointerType t)
      then
        V.zero_or_one
      else
        V.interp_boolean
          ~contains_zero:(V.contains_non_zero expr)
          ~contains_non_zero:(V.is_included V.singleton_zero expr)

and eval_expr_with_deps_state_subdiv ~with_alarms deps state e =
  let (state_without_subdiv, deps_without_subdiv, result_without_subdiv as r) =
    eval_expr_with_deps_state ~with_alarms deps state e
  in
  let subdivnb = Value_parameters.Subdivide_float_in_expr.get() in
  if subdivnb=0
  then r
  else if not (Location_Bytes.is_included result_without_subdiv Location_Bytes.top_int)
  then begin
      Value_parameters.debug ~level:2
        "subdivfloatvar: expression has an address result";
      r
    end
  else
    let compare_min, compare_max =
      if Location_Bytes.is_included result_without_subdiv Locations.Location_Bytes.top_float
      then begin
          Value_parameters.debug ~level:2
            "subdivfloatvar: optimizing floating-point expression %a=%a"
            !d_exp e
            Locations.Location_Bytes.pretty result_without_subdiv;
          Cvalue.V.compare_min_float, Cvalue.V.compare_max_float
        end
      else begin
          Value_parameters.debug ~level:2
            "subdivfloatvar: optimizing integer expression %a=%a"
            !d_exp e
            Locations.Location_Bytes.pretty result_without_subdiv;
          Cvalue.V.compare_min_int, Cvalue.V.compare_max_int
        end
    in
    let vars =
      get_influential_vars ~with_alarms:CilE.warn_none_mode state e
    in
    Value_parameters.debug ~level:2 "subdivfloatvar: variable list=%a"
      (Pretty_utils.pp_list Locations.pretty)
      vars;
    let rec try_sub vars =
      match vars with
      | [] | [ _ ] -> r
      | v :: tail ->
          try
            if not (List.exists (fun x -> Locations.loc_equal v x) tail)
            then raise Too_linear;
            let v_value =
              Cvalue.Model.find
                ~conflate_bottom:true
                ~with_alarms:CilE.warn_none_mode
                state
                v
            in
	    (*        Value_parameters.result ~current:true
		      "subdivfloatvar: considering optimizing variable %a (value %a)"
		      Locations.pretty v Cvalue.V.pretty v_value; *)
            if not (Locations.Location_Bytes.is_included
                       v_value
                       Locations.Location_Bytes.top_float)
            then raise Too_linear;

            let working_list = ref [ (v_value, result_without_subdiv) ] in
	    let bound1, bound2 = Cvalue.V.min_and_max_float v_value in
	    let compute subvalue =
	      let substate =
                Cvalue.Model.add_binding
                  ~with_alarms:CilE.warn_none_mode
                  ~exact:true
                  state
                  v
                  subvalue
              in
	      let subexpr = 
		eval_expr ~with_alarms substate e 
	      in
(*	      Value_parameters.debug ~current:true
		"subdivfloatvar: computed var=%a expr=%a"
		V.pretty subvalue
		V.pretty subexpr; *)
	      subexpr
	    in
	    let r1 = compute (Cvalue.V.inject_float bound1) in
	    let r2 = compute (Cvalue.V.inject_float bound2) in
	    let wont_find_better =
	      ref
		(if compare_min r2 r1 >= 0
		then r1
		else r2)
	    in
(*	    Value_parameters.debug ~current:true
	      "subdivfloatvar: wont initial %a"
	      V.pretty !wont_find_better; *)
            let had_bottom = ref false in
            let size =
              if Value_parameters.AllRoundingModes.get ()
              then 0
              else Int.to_int (Int_Base.project v.Locations.size)
            in
            let subdiv_for_bound better_bound =
              let insert_subvalue_in_list (_, exp_value as p) l =
		let wont = !wont_find_better in
		let bound_to_test =
		  if better_bound exp_value wont <= 0
		  then exp_value
		  else wont
		in
		let rec aux l =
                  match l with
                    [] -> [p]
                  | (_, exp_value1 as p1) :: tail ->
                      if better_bound exp_value1 bound_to_test >= 0
                      then p :: l
                      else p1 :: (aux tail)
		in
		aux l
              in
              let exp_subvalue subvalue l =
		let subexpr = compute subvalue in
                if Cvalue.V.is_bottom subexpr
                then begin
                    had_bottom := true;
                    l
                  end
                else 
                  insert_subvalue_in_list (subvalue, subexpr) l
              in
              let subdiv l =
                match l with
                  [] ->
(*                    Value_parameters.debug
                      "subdivfloatvar: all reduced to bottom!!"; *)
                    raise Ival.Can_not_subdiv
                | (value, exp_value) :: tail ->
                    let subvalue1, subvalue2 =
                      Cvalue.V.subdiv_float_interval ~size value
                    in
(*
		    let rmid = compute middlepoint1 in
		    if better_bound !wont_find_better rmid > 0
		    then begin
			wont_find_better := rmid;
                 (* Value_parameters.debug ~current:true
			  "subdivfloatvar: improved wont %a"
			  V.pretty !wont_find_better; *)
		      end;		                              *)
		    if better_bound !wont_find_better exp_value = 0
		    then begin
(*			Value_parameters.debug ~current:true
			  "subdivfloatvar: optimum reached"; *)
			raise Ival.Can_not_subdiv			
		      end;
                    let s = exp_subvalue subvalue1 tail in
                    exp_subvalue subvalue2 s
              in
              try
                for _i = 1 to subdivnb do
                  working_list := subdiv !working_list;
                done
              with Ival.Can_not_subdiv -> ()
            in
            subdiv_for_bound compare_min ;
            (* Now sort working_list in decreasing order
               on the upper bounds of exp_value *)
            let comp_exp_value (_value1,exp_value1) (_value2,exp_value2) =
              compare_max exp_value1 exp_value2
            in
            working_list := List.sort comp_exp_value !working_list ;
	    wont_find_better :=
	      if compare_max r2 r1 >= 0
	      then r1
	      else r2;	    
(*            if Value_parameters.debug_atleast 2 then
              List.iter
                (function (x, e) ->
                  Value_parameters.debug
                    "subdivfloatvar: elements of list max %a %a"
                    V.pretty x V.pretty e)
                !working_list;
	    Value_parameters.debug "subdivfloatvar: wont %a"
	      V.pretty !wont_find_better; *)
            subdiv_for_bound compare_max ;
            let working_list = !working_list in
(*            if Value_parameters.debug_atleast 2 then
              List.iter
                (function (x, e) ->
                  Value_parameters.debug
                    "subdivfloatvar: elements of final list %a %a"
                    V.pretty x V.pretty e)
                working_list; *)
            let reduced_state, optimized_exp_value =
              if !had_bottom
              then
                let reduced_var, optimized_exp_value =
                  List.fold_left
                    (fun (accv,acce) (value, exp_value)  ->
                      Cvalue.V.join value accv,
                      Cvalue.V.join exp_value acce)
                    (Cvalue.V.bottom,
                    Cvalue.V.bottom)
                    working_list
                in
                Cvalue.Model.add_binding
                  ~with_alarms:CilE.warn_none_mode
                  ~exact:true
                  state
                  v
                  reduced_var,
              optimized_exp_value
              else
                state_without_subdiv,
              List.fold_left
                (fun acc (_value, exp_value)  ->
                  Cvalue.V.join exp_value acc)
                Cvalue.V.bottom
                working_list
            in
            reduced_state, deps_without_subdiv, optimized_exp_value
          with Not_less_than | Too_linear ->
            try_sub tail
    in
    try_sub vars

(* TODO. Cf also [Eval_stmts.do_assign_abstract_value_to_loc]. Should we check
   that the value is an int, and topify otherwise ? *)
and cast_lval_bitfield lv size v =
  let signed = signof_typeof_lval lv in
  Cvalue.V.cast ~with_alarms:CilE.warn_none_mode ~size ~signed v

and cast_lval_when_bitfield lv ?(sizelv=bitfield_size_lv lv) ?(sizebf=(bitfield_size_bf lv)) v =
  match sizebf with
  | Int_Base.Value size when is_bitfield lv ~sizelv ~sizebf () ->
      cast_lval_bitfield lv size v
  | _ -> v

(* [loc] is the location pointed to by [lv]. If [lv] is precise enough, we
   reduce it to the parts of [loc] that are valid for a read/write operation *)
and reduce_by_accessed_loc ~with_alarms ~for_writing state loc lv =
  if not (Locations.is_valid ~for_writing loc)
  then begin
      CilE.set_syntactic_context (CilE.SyMem lv);
      (if for_writing then CilE.warn_mem_write else CilE.warn_mem_read)
	with_alarms ;
    end;
  (* It is not possible to factor the calls to is_valid and valid_part, becauses
     of bases with validity Unknown *)
  let valid_loc = (Locations.valid_part ~for_writing loc).loc in
  if Location_Bits.equal loc.loc valid_loc
  then state
  else begin
      match lv with
      | Mem (exp_mem), offs ->
	  let state =
	    if Cil.isConstantOffset offs 
	    then
              (* offset coming from [offs] *)
              let offs = match offs with
              | NoOffset -> Ival.zero
              | _ ->
                  let typ_exp = match offs with
                  | NoOffset -> assert false
                  | Index _ -> Cil.typeOf_pointed (typeOf exp_mem)
                  | Field _ -> Cil.typeOf exp_mem
                  in
                  let offs_bytes = fst (Cil.bitsOffset typ_exp offs) / 8 in
                  Ival.inject_singleton (Int.of_int offs_bytes)
              in
              (try
		  (* Decompose [exp_mem] into a base lvalue and an offset *)
		  let lv_mem, plus = find_lv_plus_offset
		    ~with_alarms:CilE.warn_none_mode state exp_mem in
		  (* Total offset, still in bytes *)
		  let plus = Ival.add_int plus offs in
		  let state, loc_mem =
		    lval_to_loc_state ~with_alarms:CilE.warn_none_mode state lv_mem
		  in
		  let loc_mem = Locations.valid_part ~for_writing loc_mem in
		  if Location_Bits.is_relationable loc_mem.Locations.loc
		  then
		    (* is_relationable guarantees that [loc_mem] is a single binding,
                       that can be safely reduced. The valid (reduced) value
                       the original location shifted by [-plus] *)
		    let new_val =
                      Location_Bytes.location_shift
			(Ival.neg plus) (loc_bits_to_loc_bytes valid_loc)
		    in
		    (* [new_val] is not necessarily included in previous binding,
	               use [reduce_binding] *)
		    Cvalue.Model.reduce_binding 
		      ~with_alarms:CilE.warn_none_mode
                      state loc_mem new_val
		  else state
		with Cannot_find_lv (* find_lval_plus_offset *) -> 
		  state)
	    else state
	  in
	  let rec aux e =
	    ( match e.enode with
	    | BinOp((MinusPI|PlusPI|IndexPI as _op), p, 
		   exp_index , _typ) ->
		let base_pointer = eval_expr ~with_alarms:CilE.warn_none_mode state p in
		if Cvalue.V.cardinal_zero_or_one base_pointer
		then begin
		    let lv_index = 
		      find_lv ~with_alarms:CilE.warn_none_mode
			state 
			exp_index 
		    in
		    let loc_index = lval_to_loc state ~with_alarms:CilE.warn_none_mode lv_index in
		    if Location_Bits.is_relationable loc_index.Locations.loc
		    then
		      let old_index_val = 
			Cvalue.Model.find
			  ~conflate_bottom:true
			  ~with_alarms:CilE.warn_none_mode
			  state loc_index
		      in
		      if Cvalue.V.is_included old_index_val Cvalue.V.top_int
		      then 
			let old_index_ival = 
			  Cvalue.V.project_ival 
			    old_index_val
			in
			let accessed_loc = 
			  Location_Bytes.location_shift 
			    old_index_ival
			    base_pointer
			in
			let size_pointed =
			  Int.of_int ((bitsSizeOf (Cil.typeOf_pointed _typ)))
			in
			let size_pointed_bytes =
			  Int.div size_pointed (Bit_utils.sizeofchar())
			in
			let accessed_loc =
			  Locations.make_loc
			    (loc_bytes_to_loc_bits accessed_loc)
			    (Int_Base.inject size_pointed)
			in
			let valid_accessed_loc = 
			  Locations.valid_part ~for_writing accessed_loc 
			in
			if valid_accessed_loc != accessed_loc
			then
                          if Location_Bits.equal Location_Bits.bottom valid_accessed_loc.loc then
                            Cvalue.Model.bottom
                          else
			  let new_index_val = (* in bytes *)
			    V.add_untyped Int_Base.minus_one 
			      (loc_bits_to_loc_bytes valid_accessed_loc.Locations.loc)
			      base_pointer
			  in
			  let new_index_val =
			    ( try
				let i = Cvalue.V.project_ival new_index_val in
				let mi, ma = Ival.min_and_max i in
				let mi = match mi with
				  None -> None
				| Some mi -> 
				    Some (Int.pos_div
					     (Int.add mi (Int.pred size_pointed_bytes))
					 size_pointed_bytes)
				in
				let ma = match ma with
				  None -> None
				| Some ma -> 
				    Some (Int.pos_div
					     ma
					     size_pointed_bytes)
				in
				Ival.inject_range mi ma
			      with Cvalue.V.Not_based_on_null ->
                            Value_parameters.fatal ~current:true
                              "REDUCE by ACCESSED LOC: loc %a, lv %a, \
                                for_writing: %b,state@ %a, new_index_val %a"
                              Locations.pretty loc Cil.d_lval lv for_writing
                              Cvalue.Model.pretty state
                              Cvalue.V.pretty new_index_val;
                            )
			  in
			  let new_index_val = Cvalue.V.inject_ival new_index_val in
			  Cvalue.Model.reduce_previous_binding 
			    ~with_alarms:CilE.warn_none_mode
			    state loc_index new_index_val
			else
			  state

		      else state
		    else state
		  end
		else state

	    | CastE(typ,e) ->
		pass_cast ~with_alarms:CilE.warn_none_mode state Cannot_find_lv typ e;
		aux e
	    | _ -> state)
	  in
	  if offs = NoOffset (* TODO: improve *)
	  then
	    ( try		    
		aux exp_mem
	      with Cannot_find_lv -> state)
	  else state
      | _ -> state
 end

 and eval_lval ~conflate_bottom ~with_alarms deps state lv =
    let state,deps,loc =
      lval_to_loc_deps_option ~with_alarms ?deps state lv
	~reduce_valid_index:(Kernel.SafeArrays.get ())
    in
    CilE.set_syntactic_context (CilE.SyMem lv);
    let state, result =
      if conflate_bottom
      then
	Cvalue.Model.find_and_reduce_indeterminate ~with_alarms
	  state loc
      else 
	state, 
      (Cvalue.Model.find ~conflate_bottom ~with_alarms state loc)
    in
    (*  Format.printf "lval %a before %a@."
	!d_lval lv
	Cvalue.V.pretty result; *)
    let result = cast_lval_when_bitfield lv ~sizebf:loc.Locations.size result in
    (*  Format.printf "lval %a after %a@."
	!d_lval lv
	Cvalue.V.pretty result; *)

    let state = 
      reduce_by_accessed_loc ~with_alarms ~for_writing:false state loc lv 
    in

    warn_imprecise_lval_read ~with_alarms lv loc result;

    let new_deps =
      match deps with
      | None -> None
      | Some deps ->
          Some (Zone.join deps (valid_enumerate_bits ~for_writing:false loc))
    in
    state, new_deps, result

 and eval_lval_and_convert ~with_alarms deps state (lv, e) =
    let state, deps, result =
      eval_lval ~conflate_bottom:true ~with_alarms deps state lv
    in
    let state, result_conv =
      match unrollType (Cil.typeOfLval lv) with
      | TFloat (FFloat, _) ->
          let adresses, overflow, r = 
	    Cvalue.V.cast_float ~rounding_mode:(get_rounding_mode()) result
	  in
          if overflow 
	  then begin
              CilE.set_syntactic_context (CilE.SyUnOp e);
              CilE.warn_float_overflow with_alarms
		(fun () -> Pretty_utils.sfprintf "%a" V.pretty result);
              try
		let loc, _v, _ = eval_as_exact_loc ~with_alarms state e in
		let state =
		  if adresses
		  then state
		  else
                    Cvalue.Model.reduce_previous_binding 
		      ~with_alarms:CilE.warn_none_mode
	              state loc r
		in
		state, r
              with Not_an_exact_loc -> state, r
            end
          else state, r
      | _ -> state, result (* TODO: catch overflow in bigger float types *)
    in
    state, deps, result_conv

 and eval_offset ~reduce_valid_index ~with_alarms deps typ state offset =
    match offset with
    | NoOffset ->
	state, deps, Ival.singleton_zero
    | Index (exp,remaining) ->
	let typ_pointed,array_size = match (unrollType typ) with
	| TArray (t,size,_,_) -> t, size
	| TPtr(t,_) ->
            (match unrollType t with
            | TArray (t,size,_,_) -> t,size (* pointer to start of an array *)
            | _ ->
		Value_parameters.error ~current:true
                  "Got type '%a'" !Ast_printer.d_type t;
		assert false)
	| t ->
            Value_parameters.error ~current:true
              "Got type '%a'" !Ast_printer.d_type t;
            assert false
	in
	let state, deps, current =
          eval_expr_with_deps_state ~with_alarms deps state exp
	in
	if V.is_bottom current
	then Cvalue.Model.bottom, (Some Zone.bottom), Ival.bottom
	else
          let state, offset =
            try
              let v = V.project_ival current in
              let state, v =
		if reduce_valid_index then
                  try
                    let array_siz = lenOfArray64 array_size in
                    let new_v =
                      Ival.narrow
			(Ival.inject_range
                            (Some Int.zero)
                            (Some (My_bigint.pred array_siz)))
			v
                    in
                    let new_state =
                      if Ival.equal new_v v
                      then state
                      else begin
			  begin
                            match with_alarms.CilE.others with
                            | CilE.Aignore -> ()
                            | CilE.Acall f -> f ()
                            | CilE.Alog _ ->
				let range = Pretty_utils.sfprintf "%a"
                                  V.pretty current in
				let positive = match Ival.min_int v with
				| None -> false
				| Some min -> Int.ge min Int.zero
				in
				CilE.set_syntactic_context
                                  (CilE.SyBinOp
                                      (IndexPI,
                                      exp,
                                      Extlib.the array_size));
				CilE.warn_index with_alarms ~positive ~range;
			  end;
			  begin try
                              let loc,_,_= eval_as_exact_loc ~with_alarms state exp in
                              Cvalue.Model.reduce_previous_binding
				~with_alarms:CilE.warn_none_mode
				state loc (V.inject_ival new_v)
			    with Not_an_exact_loc  -> state
			  end
			end
                    in
                    new_state, new_v
                  with LenOfArray -> state, v
		else state, v
              in
              state, v
            with V.Not_based_on_null ->
              let deps, offset =
		topify_offset
                  ~with_alarms
                  deps
                  state
                  (Cvalue.V.topify_arith_origin current)
                  remaining
              in
              raise (Offset_not_based_on_Null (deps,offset))
          in
          let state, deps, r =
            eval_offset ~reduce_valid_index ~with_alarms
              deps typ_pointed state remaining
          in
          let offset = Ival.scale_int64base (sizeof typ_pointed) offset in
          state, deps, Ival.add_int offset r
    | Field (fi,remaining) ->
	let current,_ = bitsOffset typ (Field(fi,NoOffset)) in
	let state, deps, r =
          eval_offset ~reduce_valid_index ~with_alarms
            deps
            fi.ftype
            state
            remaining
	in
	state, deps, Ival.add_int (Ival.of_int current) r
 and topify_offset ~with_alarms deps state acc offset =
    match offset with
    | NoOffset -> deps,acc
    | Field (_fi,remaining) -> topify_offset ~with_alarms deps state acc remaining
    | Index (exp,remaining) ->
	let deps, loc_index = eval_expr_with_deps ~with_alarms deps state exp in
	let acc = Location_Bytes.join
          (Cvalue.V.topify_arith_origin loc_index)
          acc
	in
	topify_offset ~with_alarms deps state acc remaining

 and eval_as_exact_loc ~with_alarms state e =
    try
      let lv = find_lv ~with_alarms state e in
      let loc = lval_to_loc ~with_alarms state lv in
      if not (valid_cardinal_zero_or_one ~for_writing:false loc)
      then raise Not_an_exact_loc;
      let typ = typeOfLval lv in
      let value_for_loc =
	Cvalue.Model.find ~conflate_bottom:true ~with_alarms state loc in
      (* Using (typeOf e) caused imprecisions with the condition
	 char c; ... if (c>0) being transformed in if (((int)c)>0) by Cil. *)
      CilE.set_syntactic_context (CilE.SyUnOp e);
      let value_for_loc2 = do_cast ~with_alarms typ value_for_loc in
      let value_for_loc2 =
	cast_lval_when_bitfield lv ~sizebf:loc.size value_for_loc2
      in
      if Cvalue.V.has_sign_problems value_for_loc &&
	not (Cvalue.V.equal value_for_loc value_for_loc2)
      then raise Not_an_exact_loc;
      loc, value_for_loc2, typ
    with Cannot_find_lv ->
      raise Not_an_exact_loc


let reduce_rel_symetric_int positive binop cond_expr value =
  match positive,binop with
    | false, Eq | true,  Ne -> V.diff_if_one value cond_expr
    | true,  Eq | false, Ne -> V.narrow value cond_expr
    | _,_ -> value

let reduce_rel_symetric_float = reduce_rel_symetric_int

let reduce_rel_antisymetric_int ~typ_loc:_ positive binop cond_expr value =
  try match positive,binop with
    | true,  Le | false, Gt -> V.filter_le value ~cond_expr
    | true,  Ge | false, Lt -> V.filter_ge value ~cond_expr
    | false, Le | true,  Gt -> V.filter_gt value ~cond_expr
    | false, Ge | true,  Lt -> V.filter_lt value ~cond_expr
    | _,_ -> value
  with V.Error_Bottom -> V.bottom

let reduce_rel_antisymetric_float round ~typ_loc positive binop cond_expr value =
  try let r = match positive,binop with
    | true, Le | false, Gt -> V.filter_le_float round ~typ_loc value ~cond_expr
    | true, Ge | false, Lt -> V.filter_ge_float round ~typ_loc value ~cond_expr
    | false, Le | true, Gt -> V.filter_gt_float round ~typ_loc value ~cond_expr
    | false, Ge | true, Lt -> V.filter_lt_float round ~typ_loc value ~cond_expr
    | _,_ -> value
  in
  r
  with V.Error_Bottom -> V.bottom


type reduce_rel_int_float = {
  reduce_rel_symetric: bool -> binop -> V.t -> V.t -> V.t;
  reduce_rel_antisymetric: typ_loc:typ -> bool -> binop -> V.t -> V.t -> V.t;
}

let reduce_rel_int = {
  reduce_rel_symetric = reduce_rel_symetric_int;
  reduce_rel_antisymetric = reduce_rel_antisymetric_int;
}

let reduce_rel_float round = {
  reduce_rel_symetric = reduce_rel_symetric_float;
  reduce_rel_antisymetric = reduce_rel_antisymetric_float round;
}

let reduce_rel_from_type t =
  if isIntegralType t || isPointerType t
  then reduce_rel_int
  else reduce_rel_float (Value_parameters.AllRoundingModes.get ())

(** Reduce the state for comparisons of the form 'v Rel k', where v
    evaluates to a location, and k to some value *)
let reduce_by_left_comparison_abstract ~with_alarms eval pos expl binop expr state =
  try
    let loc, val_for_loc, invert, val_compared, typ_compared = 
      try
	let loc, value, typ =
	  eval_as_exact_loc ~with_alarms state expl 
	in
	loc, value, (fun x -> x), value, typ
      with
	Not_an_exact_loc ->
	  let invert_cast e1 typ_compared =
	      let loc, val_for_loc, typ_for_loc =
		eval_as_exact_loc ~with_alarms state e1
	      in
	      ( match Cil.unrollType typ_for_loc with
	      | TFloat ((FDouble|FFloat) as fk, _) ->
		  let single_precision = fk = FFloat in
		  let size = bitsSizeOf typ_compared in
		  let signed = isSignedInteger typ_compared in
		  let _, _, val_compared = V.cast_float_to_int ~signed ~size val_for_loc in
		  loc, val_for_loc, 
		  (V.cast_float_to_int_inverse ~single_precision), 
		  val_compared, typ_compared
	      | _ -> raise Not_an_exact_loc)
	  in
	  ( match expl.enode with
	  | CastE (typ_larger, { enode = CastE(typ_compared,e1) } )
	      when isIntegralType typ_compared && isIntegralType typ_larger &&
(		bitsSizeOf typ_larger > bitsSizeOf typ_compared &&
		isSignedInteger typ_compared ) (* FIXME : type inclusion; see Rte *)
		->
	      invert_cast e1 typ_compared
	  | CastE (typ_compared, e1) when isIntegralType typ_compared ->
	      invert_cast e1 typ_compared
	  | _ -> raise Not_an_exact_loc)
	    
    in    

    let cond_v = expr in
    let v_sym = eval.reduce_rel_symetric pos binop cond_v val_compared in
    let v_asym = eval.reduce_rel_antisymetric ~typ_loc:typ_compared 
      pos binop cond_v v_sym 
    in
(*    Format.printf "reduce_by_left %a -> %a -> %a@." 
      Cvalue.V.pretty val_for_loc
      Cvalue.V.pretty val_compared
      Cvalue.V.pretty v_asym; *)
    if V.equal v_asym V.bottom then raise Reduce_to_bottom;
    if V.equal v_asym val_compared
    then state
    else (
	let new_val_for_loc = invert v_asym in
	let new_val_for_loc = V.narrow new_val_for_loc val_for_loc in
	if V.equal new_val_for_loc val_for_loc
	then state
	else begin
(*	    Format.printf "reduce_by_left %a -> %a -> %a -> %a@." 
	      Cvalue.V.pretty val_for_loc
	      Cvalue.V.pretty val_compared
	      Cvalue.V.pretty v_asym
	      Cvalue.V.pretty new_val_for_loc;  *)
	    Cvalue.Model.reduce_previous_binding
	      ~with_alarms:CilE.warn_none_mode
	      state loc new_val_for_loc
	  end )
  with Not_an_exact_loc  -> state

let reduce_by_left_comparison ~with_alarms eval pos expl binop expr state =
  let expr = eval_expr ~with_alarms state expr in
  reduce_by_left_comparison_abstract ~with_alarms eval pos expl binop expr state

(** Reduce the state for comparisons of the form
    'v Rel k', 'k Rel v' or 'v = w' *)
let reduce_by_comparison ~with_alarms reduce_rel pos exp1 binop exp2 state =
(*  Format.printf "red_by_comparison  %a@." Cvalue.Model.pretty state; *)
  let state = reduce_by_left_comparison ~with_alarms reduce_rel
    pos exp1 binop exp2 state
  in
  let inv_binop = match binop with
    | Gt -> Lt | Lt -> Gt | Le -> Ge | Ge -> Le
    | _ -> binop
  in
  reduce_by_left_comparison ~with_alarms reduce_rel
    pos exp2 inv_binop exp1 state


(** raises [Reduce_to_bottom] and never returns [Cvalue.Model.bottom]*)
let reduce_by_cond ~with_alarms state cond =
  (* Do not reduce anything if the cond is volatile.
     (This test is dumb because the cond may contain volatile l-values
     without the "volatile" attribute appearing at toplevel. pc 2007/11) *)
  if hasAttribute "volatile" (typeAttr (typeOf cond.exp)) then state
  else
    let rec aux cond state =
      (*Format.printf "eval_cond_aux %B %a@." cond.positive (!d_exp) cond.exp;*)
      match cond.positive,cond.exp.enode with
      | _positive, BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), exp1, exp2, _typ) ->
        let reduce_rel = reduce_rel_from_type (unrollType (typeOf exp1)) in
        reduce_by_comparison ~with_alarms reduce_rel
          cond.positive exp1 binop exp2 state

      | true, BinOp (LAnd, exp1, exp2, _)
      | false, BinOp (LOr, exp1, exp2, _) ->
          let new_state = aux {cond with exp = exp1} state in
          let result = aux {cond with exp = exp2} new_state in
          result
      | false, BinOp (LAnd, exp1, exp2, _)
      | true, BinOp (LOr, exp1, exp2, _) ->
          let new_v1 = try aux {cond with exp = exp1} state
            with Reduce_to_bottom -> Cvalue.Model.bottom
          in let new_v2 = try aux {cond with exp = exp2} state
            with Reduce_to_bottom -> Cvalue.Model.bottom
          in let r = Cvalue.Model.join new_v1 new_v2 in
          if Db.Value.is_reachable r then r else raise Reduce_to_bottom

      | _, UnOp(LNot,exp,_) ->
          aux { positive = not cond.positive; exp = exp; } state

      | _, CastE (typ, e) ->
        (try
           pass_cast ~with_alarms state Exit typ e;
           aux { cond with exp = e} state
         with Exit -> 
	   if  isIntegralType typ || isPointerType typ
	   then 
	     reduce_by_left_comparison_abstract ~with_alarms 
	       reduce_rel_int cond.positive cond.exp Ne V.singleton_zero state
	   else state)
      | _, Lval _ when (let t = typeOf cond.exp in
                        isIntegralType t || isPointerType t)
          -> (* "if (c)" is equivalent to "if(!(c==0))" *)
	  reduce_by_left_comparison_abstract ~with_alarms 
	    reduce_rel_int cond.positive cond.exp Ne V.singleton_zero state	    
      | _ -> state
    in
    let result = aux cond state in
    let contains_zero =
      if Value_parameters.UndefinedPointerComparisonPropagateAll.get()
      then V.contains_zero
      else (fun x -> V.is_included V.singleton_zero x)
    in
    let condition_may_still_be_true_in_state state =
      let cond_interp = eval_expr ~with_alarms state cond.exp in
      (not cond.positive || V.contains_non_zero cond_interp) &&
        (cond.positive || contains_zero cond_interp)
    in
    if (not (Cvalue.Model.equal result state)) &&
      (not (condition_may_still_be_true_in_state result))
    then raise Reduce_to_bottom;
    let is_enumerable v =
      let v_interp =
        Cvalue.Model.find ~conflate_bottom:true ~with_alarms result v
      in
      ignore (Location_Bytes.cardinal_less_than v_interp 7);
      v_interp
    in
    let rec enumerate_one_var l =
      match l with
      | [] -> raise Not_found
      | v::t ->
          try
            let v_interp = is_enumerable v in
            v,v_interp,t
          with Abstract_interp.Not_less_than ->
            enumerate_one_var t
    in
    let invert_cond vl =
      try
        let v1,v_interp1, _tail = enumerate_one_var vl in
(*      Format.printf "enumerate %a %a@."
          Locations.pretty v1
          Cvalue.V.pretty v_interp1; *)
        let f one_val acc =
          (* interpret cond in an environment where v -> one_val
          *)
          let env =
            Cvalue.Model.reduce_previous_binding
	      ~with_alarms:CilE.warn_none_mode
              result v1 one_val
          in
          let stays = condition_may_still_be_true_in_state env in
(*        Format.printf "enumerate %a stays:%B@."
            Cvalue.V.pretty one_val
            stays; *)
          if stays
          then begin
              Location_Bytes.join one_val acc
            end
          else begin
              acc
            end
        in
        let new_v_interp =
          Location_Bytes.fold_enum
            ~split_non_enumerable:2
            f v_interp1 Location_Bytes.bottom
        in
        let state_value =
          if V.is_bottom new_v_interp
          then raise Reduce_to_bottom
          else if V.equal new_v_interp v_interp1
	  then result
	  else
	    Cvalue.Model.reduce_previous_binding
	      ~with_alarms:CilE.warn_none_mode
	      result v1 new_v_interp
        in
        state_value
      with Not_found -> result
    in
    let result =
      invert_cond (get_influential_vars ~with_alarms result cond.exp)
    in
    if not (Cvalue.Model.is_reachable result)
    then raise Reduce_to_bottom
    else result

(* Test that two functions types are compatible; used to verify that a call
   through a function pointer is ok. In theory, we could only check that
   both types are compatible as defined by C99, 6.2.7. However, some industrial
   codes do not strictly follow the norm, and we must be more lenient.
   Thus, we emit a warning on undefined code, but we also return true
   if Value can ignore more or less safely the incompatibleness in the types. *)
let compatible_functions ~with_alarms vi typ_pointer typ_fun =
  try
    ignore (Cabs2cil.compatibleTypes typ_pointer typ_fun); true
  with Failure _ ->
    if with_alarms.CilE.others != CilE.Aignore then
      warning_once_current
        "@[Function pointer@ and@ pointed function@ '%a' @ have@ incompatible \
            types:@ %a@ vs.@ %a.@ assert(function type matches)@]"
        Cil.d_var vi Cil.d_type typ_pointer Cil.d_type typ_fun;
    match Cil.unrollType typ_pointer, Cil.unrollType typ_fun with
      | TFun (ret1, args1, var1, _), TFun (ret2, args2, var2, _) ->
          (* Either both functions are variadic, or none. Otherwise, it
             will be too complicated to make the argument match *)
          var1 = var2 &&
          (* Both functions return something of the same size, or nothing*)
          (match Cil.unrollType ret1, Cil.unrollType ret2 with
             | TVoid _, TVoid _ -> true (* let's avoid relying on the size
                                           of void *)
             | TVoid _, _ | _, TVoid _ -> false
             | t1, t2 -> bitsSizeOf t1 = bitsSizeOf t2
          ) &&
          (* Argument lists of the same length, with compatible sizes between
             the arguments, or unspecified argument lists *)
          (match args1, args2 with
             | None, None | None, Some _ | Some _, None -> true
             | Some l1, Some l2 ->
                 List.length l1 = List.length l2 &&
                 List.for_all2
                   (fun (_, t1, _) (_, t2, _) -> bitsSizeOf t1 = bitsSizeOf t2)
                   l1 l2
          )
      | _ -> false

let resolv_func_vinfo ~with_alarms deps state funcexp =
  let warning_once_current fmt =
    match with_alarms.CilE.defined_logic with
      | CilE.Aignore -> Format.ifprintf Format.std_formatter fmt
      | CilE.Alog _ -> warning_once_current fmt
      | CilE.Acall f -> f (); Format.ifprintf Format.std_formatter fmt
  in
  match funcexp.enode with
  | Lval (Var vinfo,NoOffset) ->
      Kernel_function.Hptset.singleton (Globals.Functions.get vinfo), deps
  | Lval (Mem v,NoOffset) ->
      let deps, loc = eval_expr_with_deps ~with_alarms deps state v in
      let typ_pointer = typeOf funcexp in
      let pp_assert fmt =
        Format.fprintf fmt "assert(\\valid_fun_pointer(%a))" Cil.d_exp funcexp
      in
      let fundecs = match loc with
        | Location_Bytes.Map _ ->
          Location_Bytes.fold_i
            (fun base offs acc ->
              match base with
                | Base.String (_,_) ->
                  warning_once_current
                    "Function pointer call at string position in memory: \
                       ignoring this particular value: %t" pp_assert;
                  acc
                | Base.Null ->
                  warning_once_current
                    "Function pointer call at absolute position in memory: \
                       ignoring this particular value: %t" pp_assert;
                  acc
                | Base.Var (v,_) | Base.Initialized_Var (v,_) ->
                  if Cil.isFunctionType v.vtype then (
                    if Ival.contains_non_zero offs then
                      warning_once_current
                        "Function pointer evaluates to function address plus \
                          offset: ignoring this particular value: %t" pp_assert;
                    if Ival.contains_zero offs then (
                      if compatible_functions ~with_alarms v typ_pointer v.vtype
                      then
                        Kernel_function.Hptset.add (Globals.Functions.get v) acc
                      else acc
                    )
                    else acc
                  ) else (
                    warning_once_current
                      "Function pointer evaluates to non-function: \
                    ignoring this particular value: %t" pp_assert;
                    acc)
            )
            loc Kernel_function.Hptset.empty
        | Location_Bytes.Top (set, _) ->
            warning_once_current
              "Function pointer for call is imprecise: %t" pp_assert;
            Location_Bytes.Top_Param.fold
              (fun b acc -> match b with
                | Base.Var (v,_) | Base.Initialized_Var (v,_)
                      when Cil.isFunctionType v.vtype ->
                    if compatible_functions ~with_alarms v typ_pointer v.vtype
                    then
                      Kernel_function.Hptset.add (Globals.Functions.get v) acc
                    else acc
                | _ -> acc
              ) set Kernel_function.Hptset.empty

      in
      fundecs, deps
  | _ ->
      assert false

let offsetmap_of_lv ~with_alarms state lv =
  CilE.set_syntactic_context (CilE.SyMem lv);
  let state, loc_to_read = lval_to_loc_state ~with_alarms state lv in
  state,
  Cvalue.Model.copy_offsetmap
    ~with_alarms:CilE.warn_none_mode
    loc_to_read
    state



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
