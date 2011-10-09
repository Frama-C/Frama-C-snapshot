(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

exception Leaf (* raised when nothing is known for a function :
                  no source nor specification *)

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
        let addresses, overflow, res = V.cast_float expr in
        if addresses
        then warning_once_current
          "addresses in float";
        if overflow then warning_once_current
          "overflow in float: %a -> %a. assert(Ook)"
          V.pretty expr V.pretty res;
        res
    | TFloat (FDouble,_)
    | TFloat (FLongDouble,_) ->
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
  match dest_type, src_typ with
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

let warn_lval_read lv loc contents =
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
  if something_to_warn then
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
    warning_once_current
      "float operation on address.";
    V.join
      (V.topify_arith_origin ev1)
      (V.topify_arith_origin ev2)

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
    | LOr ->
      assert false
                (* This code makes a strict evaluation: V.interp_boolean
                   ~contains_zero: (V.contains_zero ev1 &&
                   V.contains_zero ev2) ~contains_non_zero:
                   (V.contains_non_zero ev1 || V.contains_non_zero
                   ev2)*)
    | LAnd ->
      assert false
                (* This code makes a strict evaluation:
                   V.interp_boolean ~contains_zero: (V.contains_zero
                   ev1 || V.contains_zero ev2) ~contains_non_zero:
                   (V.contains_non_zero ev1 && V.contains_non_zero
                   ev2)*)
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
         V.join (V.topify_arith_origin ev1) (V.topify_arith_origin ev2))

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
            let size = Extlib.opt_map bitsSizeOf typ in
            f ~with_alarms ?size ev1 ev2
          with SizeOfError _ -> assert false	  
	end

(* TODO: A version that does not create bigints would be better *)
let bitfield_size_lv lv = sizeof (typeOfLval lv)
let bitfield_size_bf lv = Bit_utils.sizeof_lval lv
let is_bitfield lv ?(sizelv=bitfield_size_lv lv) ?(sizebf=(bitfield_size_bf lv)) () =
  not (Int_Base.equal sizelv sizebf)

let rec lval_to_loc ~with_alarms state lv =
  let _,_,r =
    lval_to_loc_deps_option
      ~with_alarms
      ~deps:None
      ~reduce_valid_index:(Kernel.SafeArrays.get ())
      state
      lv
  in
  r

and exp_lval_to_loc state exp =
  let lv =
    match exp.enode with
      | Lval lv -> lv
      | _ -> raise Cannot_find_lv
  in
    (* TODO: utiliser find_lv_plus pour traiter plus d'expressions *)
  lv, lval_to_loc ~with_alarms:CilE.warn_none_mode state lv

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
  (* type might be the same but their attributes.
     But volatile attribute cannot be skipped *)
  if not (Cilutil.equals
             (typeSigWithAttrs (filterAttributes "volatile") typ)
             (typeSigWithAttrs (filterAttributes "volatile") (typeOf e)))
  then
    (try
        let typeofe = typeOf e in
        (* Any volatile attribute may have an effect on the expression value *)
        if hasAttribute "volatile" (typeAttrs typeofe)
          || hasAttribute  "volatile" (typeAttrs typ)
        then raise exn;
        let sztyp = sizeof typ in
        let szexpr = sizeof typeofe in
        let typ_ge_typeofe =
          match sztyp,szexpr with
            Int_Base.Value styp, Int_Base.Value sexpr -> Int.ge styp sexpr
          | _ -> false
        in
        if not typ_ge_typeofe then raise exn;
        let sityp = is_signed_int_enum_pointer typ in
        let sisexpr = is_signed_int_enum_pointer (typeOf e) in
        if sityp = sisexpr then ()
          (* destination type is larger and has the same sign as
             the original type *)
        else begin (* try to ignore the cast if it acts as identity
                      on the value [e] even if signed/unsigned
                      conflict. *)
            match unrollType typ with
            | TInt _ | TEnum _ ->
                let size = Int.of_int (bitsSizeOf typ) in
                let signed = sityp in
                (try
                    let old_ival = V.project_ival
                      (eval_expr ~with_alarms state e)
                    in
                    if (Ival.equal
                           old_ival
                           (Ival.cast ~size ~signed ~value:old_ival))
                    then () (* [e] is not sensitive to cast *)
                    else raise exn
                  with
                  | Not_found
                  | V.Not_based_on_null ->
                      raise exn)
                  (* this is not always injective, thus cannot be
                     easily reverted. *)
            | _ -> raise exn
          end
      with Neither_Int_Nor_Enum_Nor_Pointer
        -> raise exn)

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

and find_lv_plus ~with_alarms state e =
  let acc = ref [] in
  let rec find_lv_plus_rec e current_offs =
    try
      let lv = find_lv ~with_alarms state e in
      if not (hasAttribute "volatile" (typeAttrs (Cil.typeOfLval lv)))
      then acc := (lv,current_offs) :: !acc
    with Cannot_find_lv ->
      match e.enode with
        BinOp(op, e1, e2, typ) ->
          begin
            match unrollType typ with
              TFloat _ -> ()
            | _ -> begin
                  match op with
                    PlusA ->
                      let ev1 = eval_expr ~with_alarms state e1 in
                      let ev2 = eval_expr ~with_alarms state e2 in
                      ( try
                          let ival1 = V.project_ival ev1 in
                          find_lv_plus_rec e2 (Ival.add current_offs ival1)
                        with V.Not_based_on_null -> ());
                      ( try
                          let ival2 = V.project_ival ev2 in
                          find_lv_plus_rec e1 (Ival.add current_offs ival2)
                        with V.Not_based_on_null -> ());
                  | (MinusA|MinusPI|PlusPI|IndexPI as b) ->
                      let ev2 = eval_expr ~with_alarms state e2 in
                      ( try
                          let ival2 = V.project_ival ev2 in
                          let ival2 =
                            if b = MinusA
                            then ival2
                            else
                              let ival2 =
                                Ival.scale
                                  (Int_Base.project (osizeof_pointed typ))
                                  ival2
                              in
                              if b = MinusPI
                              then ival2
                              else Ival.neg ival2
                          in
                          find_lv_plus_rec e1 (Ival.sub current_offs ival2)
                        with V.Not_based_on_null | Int_Base.Error_Top-> ());
                  | _ -> ()
              end
          end
      | CastE(typ,e) ->
          ( try
              pass_cast ~with_alarms  state Cannot_find_lv typ e;
            find_lv_plus_rec e current_offs
            with Cannot_find_lv -> ())
      | _ -> ()
  in
  find_lv_plus_rec e Ival.singleton_zero;
  (*List.iter
    (fun (lv,ival) ->
    ignore (Pretty.printf "find_lv_plus %a : %s\n"
    d_lval lv
    (pretty_to_string Ival.pretty ival)))
    !acc;*)
  !acc

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
    match cond.enode with
    | Lval (Var v, off as lv) ->
        let offset =
          try
            let _, _, offset =
              eval_offset ~reduce_valid_index:true ~with_alarms None
                v.vtype state off
            in
            offset
          with Offset_not_based_on_Null _ ->
            Ival.top
        in
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
            Cvalue.Model.find ~conflate_bottom:true
              state ~with_alarms loc
          in
          if Location_Bytes.cardinal_zero_or_one contents
          then (
              (* Format.printf "cond:%a@.var contents:%a@.state:%a@."
                 !d_exp cond
                 Location_Bytes.pretty contents
                 Cvalue.Model.pretty state; *)
              acc (* it's not influential *)
            )
          else loc :: acc
        else
          (* a variable in offset can be influential *)
          get_vars_offset acc off
    | Lval (Mem e, off) ->
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

and reduce_by_valid_expr ~positive ~for_writing exp state =
  try
    let lv, loc = exp_lval_to_loc state exp in
    if not (Locations.valid_cardinal_zero_or_one ~for_writing:false loc)
    then state
    else reduce_by_valid_loc ~positive ~for_writing loc (typeOfLval lv) state
  with Cannot_find_lv -> state

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
        Cvalue.Model.reduce_binding ~with_alarms:CilE.warn_none_mode
          state loc reduced_value
    end
  with Cannot_find_lv -> state

and reduce_by_initialized_loc ~with_alarms:_ ~positive (typ, loc) state =
  try
    let locbi = loc_bytes_to_loc_bits loc in
    let size = match unrollType typ with
      | TPtr (t, _) -> bitsSizeOf t
      | _ -> assert false
    in
    let loc = make_loc locbi (Int_Base.inject (Int.of_int size)) in
    if not (Locations.valid_cardinal_zero_or_one ~for_writing:false loc)
    then state
    else
      let value = Cvalue.Model.find_unspecified
        ~with_alarms:CilE.warn_none_mode state loc
      in
      (match value with
      | Cvalue.V_Or_Uninitialized.C_uninit_esc (Location_Bytes.Top _)
      | Cvalue.V_Or_Uninitialized.C_uninit_noesc (Location_Bytes.Top _)
      | Cvalue.V_Or_Uninitialized.C_init_esc (Location_Bytes.Top _)
      | Cvalue.V_Or_Uninitialized.C_init_noesc (Location_Bytes.Top _) ->
          (* we won't reduce anything anyway,
             and we may lose information if loc contains misaligned data *)
          raise Cannot_find_lv
      | _ -> () );
      let reduced_value =
        Cvalue.V_Or_Uninitialized.change_initialized positive value
      in
      if Cvalue.V_Or_Uninitialized.equal value reduced_value
      then state
      else begin
          if Cvalue.V_Or_Uninitialized.equal
            Cvalue.V_Or_Uninitialized.bottom reduced_value
          then Cvalue.Model.bottom
          else
            Cvalue.Model.add_binding_unspecified
              state
              loc
              reduced_value
        end
  with Cannot_find_lv -> state

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
              Value_parameters.result ~once:true
                "float support is experimental";
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
        eval_lval ~conflate_bottom:true ~with_alarms deps state lv
    | AddrOf v | StartOf v ->
        let state, deps, r =
          lval_to_loc_with_offset_deps_only_option ~with_alarms ?deps state v
        in
        state, deps, loc_to_loc_without_size r

    | CastE (typ, e) ->
        let deps, evaled_expr =
          eval_expr_with_deps ~with_alarms deps state e
        in
        let src_typ = unrollType (typeOf e) in
        let dest_type = unrollType typ in
        let r = do_promotion ~with_alarms ~dest_type ~src_typ evaled_expr e in
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
        let deps, expr = eval_expr_with_deps ~with_alarms deps state e in
        let syntactic_context = match op with
          | Neg -> CilE.SyUnOp orig_expr (* Can overflow *)
          | BNot -> CilE.SyUnOp orig_expr (* does in fact never raise an alarm*)
          | LNot -> CilE.SyBinOp (Eq, Cil.zero ~loc:e.eloc, e)
              (* Can raise a pointer comparison. CilE needs a binop there *)
        in
        CilE.set_syntactic_context syntactic_context;
        let result = eval_unop ~with_alarms expr t op in
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
  let rr = do_cast ~with_alarms typ r in
(*  ( match typ with
    TInt _ when not (V.equal r rr || V.is_topint r) ->
	warning_once_current
	  "downcast %a -> %a@." V.pretty r V.pretty rr
  | _ -> ()); *)
  state, deps, rr

and eval_unop ~with_alarms expr t op =
  match op with
    | Neg ->
        let t = unrollType t in
        (match t with TFloat _ ->
           (try
              let v = V.project_ival expr in
              let f = Ival.project_float v in
              V.inject_ival
                (Ival.inject_float (Ival.Float_abstract.neg_float f))
            with
              V.Not_based_on_null ->
                begin match with_alarms.CilE.others with
                  CilE.Aignore -> ()
                | CilE.Acall f -> f()
                | CilE.Alog _ ->
                    warning_once_current
                      "converting address to float: assert(TODO)"
                end;
                V.topify_arith_origin expr
            | Ival.Float_abstract.Nan_or_infinite ->
                begin match with_alarms.CilE.others with
                  CilE.Aignore -> ()
                | CilE.Acall f -> f()
                | CilE.Alog _ ->
                    warning_once_current
                      "converting value to float: assert (TODO)"
                end;
                V.top_float
           )
        | _ ->
            let result =
              try
                let v = V.project_ival expr in
                V.inject_ival (Ival.neg v)
              with V.Not_based_on_null -> V.topify_arith_origin expr
            in
            handle_signed_overflow ~with_alarms t result
        )

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
  else if not (Locations.Location_Bytes.is_included result_without_subdiv Locations.Location_Bytes.top_int)
  then begin
      Value_parameters.debug ~level:2
        "subdivfloatvar: expression has an address result";
      r
    end
  else
    let compare_min, compare_max =
      if Locations.Location_Bytes.is_included result_without_subdiv Locations.Location_Bytes.top_float
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
            let had_bottom = ref false in
            let subdiv_for_bound better_bound =
              let rec insert_subvalue_in_list (_, exp_value as p) l =
                match l with
                  [] -> [p]
                | (_, exp_value1 as p1) :: tail ->
                    if better_bound exp_value1 exp_value >= 0
                    then p :: l
                    else p1 :: (insert_subvalue_in_list p tail)
              in
              let exp_subvalue subvalue l =
                let substate =
                  (* FIXME: should be relation-aware primitive *)
                  Cvalue.Model.add_binding
                    ~with_alarms:CilE.warn_none_mode
                    ~exact:true
                    state
                    v
                    subvalue
                in
                let subexpr = eval_expr ~with_alarms substate e in
(*                Value_parameters.result ~current:true
                  "subdivfloatvar: computed var=%a expr=%a"
                  V.pretty subvalue
                  V.pretty subexpr; *)
                if Cvalue.V.is_bottom subexpr
                then begin
                    had_bottom := true;
                    l
                  end
                else
                  insert_subvalue_in_list (subvalue, subexpr) l
              in
              let size =
                if Value_parameters.AllRoundingModes.get ()
                then 0
                else Int.to_int (Int_Base.project v.Locations.size)
              in
              let subdiv l =
                match l with
                  [] ->
                    Value_parameters.debug
                      "subdivfloatvar: all reduced to bottom!!";
                    raise Ival.Can_not_subdiv
                | (value, _exp_value) :: tail ->
                    let (subvalue1, subvalue2) =
                      Cvalue.V.subdiv_float_interval ~size value
                    in
                    let s = exp_subvalue subvalue1 tail
                    in
                    exp_subvalue subvalue2 s
              in
              try
                for i = 1 to subdivnb do
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
            if Value_parameters.debug_atleast 2 then
              List.iter
                (function (x, e) ->
                  Value_parameters.debug
                    "subdivfloatvar: elements of list max %a %a"
                    V.pretty x V.pretty e)
                !working_list;
            subdiv_for_bound compare_max ;
            let working_list = !working_list in
            if Value_parameters.debug_atleast 2 then
              List.iter
                (function (x, e) ->
                  Value_parameters.debug
                    "subdivfloatvar: elements of final list %a %a"
                    V.pretty x V.pretty e)
                working_list;
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
                (* FIXME: should be relation-aware primitive *)
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

and eval_lval_using_main_memory ~conflate_bottom ~with_alarms deps state lv =
  let state,deps,loc =
    lval_to_loc_deps_option ~with_alarms ?deps state lv
      ~reduce_valid_index:(Kernel.SafeArrays.get ())
  in
  CilE.set_syntactic_context (CilE.SyMem lv);
  let result =
    Cvalue.Model.find ~conflate_bottom ~with_alarms state loc
  in
(*  Format.printf "lval %a before %a@."
    !d_lval lv
    Cvalue.V.pretty result; *)
  let result = cast_lval_when_bitfield lv ~sizebf:loc.Locations.size result in
(*  Format.printf "lval %a after %a@."
    !d_lval lv
    Cvalue.V.pretty result; *)
  (* TODO: move into Model.find *)
  let valid_loc = Locations.valid_part ~for_writing:false loc in
  let state =
    if Location_Bits.equal loc.Locations.loc valid_loc.Locations.loc
    then state
    else begin
        match lv with
          Mem (exp_mem),NoOffset ->
            let lv_mem_plus_list =
              find_lv_plus ~with_alarms:CilE.warn_none_mode state exp_mem
            in
            let treat_lv_mem_plus (lv_mem, plus) state =
              let loc_mem =
                lval_to_loc ~with_alarms:CilE.warn_none_mode state lv_mem
              in
              if Location_Bits.is_relationable loc_mem.Locations.loc
              then
                let new_val =
                  Location_Bytes.location_shift
                    (Ival.neg plus)
                    (loc_bits_to_loc_bytes valid_loc.loc)
                in
                Cvalue.Model.reduce_binding
		  ~with_alarms:CilE.warn_none_mode
                  state loc_mem new_val
              else state
            in
            List.fold_right treat_lv_mem_plus lv_mem_plus_list state
        | _ -> state
      end
  in
  (match with_alarms.CilE.imprecision_tracing with
  | CilE.Aignore -> ()
  | CilE.Acall f -> f ()
  | CilE.Alog _ -> warn_lval_read lv loc result);
  let new_deps =
    match deps with
    | None -> None
    | Some deps ->
        Some (Zone.join deps (valid_enumerate_bits ~for_writing:false loc))
  in
  state, new_deps, result

and eval_lval ~conflate_bottom ~with_alarms deps state lv =
  let state, deps, result =
    eval_lval_using_main_memory ~conflate_bottom ~with_alarms deps state lv
  in
  let result_conv =
    (*    match unrollType (Cil.typeOfLval lv) with
          TFloat (FDouble|FFloat as kind, _) ->
          let f, r = Cvalue.V.force_float kind result in
          if f then Format.printf "TODO: assert@.";
          r
          | _ -> *) result
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
                              CilE.set_syntactic_context
                                (CilE.SyBinOp
                                    (IndexPI,
                                    exp,
                                    Cilutil.out_some array_size));
                              CilE.warn_index with_alarms "accessing"
                                (Pretty_utils.sfprintf "%a" V.pretty current);
                        end;
                        state (* TODO : if the index is a variable, reduce *)
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
        state, deps, Ival.add offset r
  | Field (fi,remaining) ->
      let current,_ = bitsOffset typ (Field(fi,NoOffset)) in
      let state, deps, r =
        eval_offset ~reduce_valid_index ~with_alarms
          deps
          fi.ftype
          state
          remaining
      in
      state, deps, Ival.add (Ival.of_int current) r
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


let eval_as_exact_loc ~with_alarms state e =
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

let eval_symetric_int positive binop cond_expr value =
  match positive,binop with
    | false, Eq | true,  Ne -> V.diff_if_one value cond_expr
    | true,  Eq | false, Ne -> V.narrow value cond_expr
    | _,_ -> value

let eval_symetric_float = eval_symetric_int

let eval_antisymetric_int ~typ_loc:_ positive binop cond_expr value =
  try match positive,binop with
    | true,  Le | false, Gt -> V.filter_le value ~cond_expr
    | true,  Ge | false, Lt -> V.filter_ge value ~cond_expr
    | false, Le | true,  Gt -> V.filter_gt value ~cond_expr
    | false, Ge | true,  Lt -> V.filter_lt value ~cond_expr
    | _,_ -> value
  with V.Error_Bottom -> V.bottom

let eval_antisymetric_float round ~typ_loc positive binop cond_expr value =
  try let r = match positive,binop with
    | true, Le | false, Gt -> V.filter_le_float round ~typ_loc value ~cond_expr
    | true, Ge | false, Lt -> V.filter_ge_float round ~typ_loc value ~cond_expr
    | false, Le | true, Gt -> V.filter_gt_float round ~typ_loc value ~cond_expr
    | false, Ge | true, Lt -> V.filter_lt_float round ~typ_loc value ~cond_expr
    | _,_ -> value
  in
  r
  with V.Error_Bottom -> V.bottom


type eval_int_float = {
  eval_symetric: bool -> binop -> V.t -> V.t -> V.t;
  eval_antisymetric: typ_loc:typ -> bool -> binop -> V.t -> V.t -> V.t;
}

let eval_int = {
  eval_symetric = eval_symetric_int;
  eval_antisymetric = eval_antisymetric_int;
}

let eval_float round = {
  eval_symetric = eval_symetric_float;
  eval_antisymetric = eval_antisymetric_float round;
}

let eval_from_type t round =
  if isIntegralType t || isPointerType t
  then eval_int
  else eval_float round

(** Reduce the state for comparisons of the form 'v Rel k', where v
    evaluates to a location, and k to some value *)
let reduce_by_left_comparison ~with_alarms eval pos expl binop expr state =
  try
    let loc,value_for_loc,typ_loc = eval_as_exact_loc ~with_alarms state expl in
(*    Format.printf "red_by_left1  %a %a %a@." 
      Locations.pretty loc
      Cvalue.V.pretty value_for_loc
      !d_type typ_loc; *)
    let cond_v = eval_expr ~with_alarms state expr in
    let v_sym = eval.eval_symetric pos binop cond_v value_for_loc in
    let v_asym = eval.eval_antisymetric ~typ_loc pos binop cond_v v_sym in
(*    Format.printf "red_by_left  %a@." Cvalue.V.pretty v_asym; *)
    if V.equal v_asym V.bottom then raise Reduce_to_bottom;
    if V.equal v_asym value_for_loc
    then state, Some loc
    else 
      (  Cvalue.Model.reduce_binding
	  ~with_alarms:CilE.warn_none_mode
	  state loc v_asym, 
      Some loc )
  with Not_an_exact_loc  -> state, None

(** Reduce the state for comparisons of the form
    'v Rel k', 'k Rel v' or 'v = w' *)
let reduce_by_comparison ~with_alarms eval pos exp1 binop exp2 state =
(*  Format.printf "red_by  %a@." Cvalue.Model.pretty state; *)
  let state, _loc1 = reduce_by_left_comparison ~with_alarms eval
    pos exp1 binop exp2 state
  in
  let inv_binop = match binop with
    | Gt -> Lt | Lt -> Gt | Le -> Ge | Ge -> Le
    | _ -> binop
  in
  let state, _loc2 = reduce_by_left_comparison ~with_alarms eval
    pos exp2 inv_binop exp1 state
  in
(*  Format.printf "red_by1 %a@." Cvalue.Model.pretty state; *)
  (* Without relations, this is now the identity
  begin match (pos, binop), loc1, loc2 with
    |  ((true, Eq) | (false, Ne)), Some left_loc , Some right_loc ->
      Cvalue.Model.reduce_equality state left_loc right_loc
    | _ -> state
  end
  *)
  state

(** raises [Reduce_to_bottom] and never returns [Cvalue.Model.bottom]*)
let reduce_by_cond ~with_alarms state cond =
  (* Do not reduce anything if the cond is volatile.
     (This test is dumb because the cond may contain volatile l-values
     without the "volatile" attribute appearing at toplevel. pc 2007/11) *)
(*  Format.printf "eval_cond %B %a@." cond.positive (!d_exp) cond.exp; *)
  if hasAttribute "volatile" (typeAttr (typeOf cond.exp)) then state
  else
    let rec aux cond state =
      match cond.positive,cond.exp.enode with
      | _positive, BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), exp1, exp2, _typ) ->
        let eval = eval_from_type (unrollType (typeOf exp1))
          (Value_parameters.AllRoundingModes.get ())
        in
        reduce_by_comparison ~with_alarms eval
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
      | _, Lval _ when (let t = typeOf cond.exp in
                        isIntegralType t || isPointerType t)
          -> (* "if (c)" is equivalent to "if(!(c==0))" *)
        (try
           let loc,value_for_loc,_ =
             eval_as_exact_loc ~with_alarms state cond.exp in
           let new_value = eval_symetric_int
             (not cond.positive) Eq V.singleton_zero value_for_loc
           in
           if V.equal new_value V.bottom then raise Reduce_to_bottom
           else 
	     Cvalue.Model.reduce_binding 
	       ~with_alarms:CilE.warn_none_mode state loc new_value
         with Not_an_exact_loc -> state)
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
            Cvalue.Model.reduce_binding
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
          if V.equal new_v_interp V.bottom
          then raise Reduce_to_bottom
          else 
	    Cvalue.Model.reduce_binding
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


let resolv_func_vinfo ~with_alarms deps state funcexp =
  match funcexp.enode with
  | Lval (Var vinfo,NoOffset) ->
      Kernel_function.Hptset.singleton (Globals.Functions.get vinfo), deps
  | Lval (Mem v,NoOffset) ->
      let deps, loc = eval_expr_with_deps ~with_alarms deps state v in
      let fundecs = List.fold_left
        (fun acc varid ->
           match varid with
           | Base.String (_,_) ->
               warning_once_current
                 "Function pointer call at string position in memory: \
                    ignoring this particular value: assert(TODO)";
               acc
           | Base.Null ->
               warning_once_current
                 "Function pointer call at absolute position in memory: \
                    ignoring this particular value: assert(TODO)";
               acc
           | Base.Var (v,_) | Base.Initialized_Var (v,_) ->
               Kernel_function.Hptset.add (Globals.Functions.get v) acc
        )
        Kernel_function.Hptset.empty
        (try  Location_Bytes.get_keys_exclusive Ival.zero loc
         with Location_Bytes.Not_all_keys ->
           warning_once_current
             "Function pointer call is completely unknown: \
                assuming no effects: assert(TODO)";
           raise Leaf)
      in
      fundecs, deps
  | _ ->
      assert false


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
