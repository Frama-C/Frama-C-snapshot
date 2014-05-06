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

open Cil_types
open Cil
open Abstract_interp
open Locations
open Cvalue
open Bit_utils
open Value_util


exception Not_an_exact_loc
let not_an_exact_loc = Not_an_exact_loc

exception Reduce_to_bottom
let reduce_to_bottom = Reduce_to_bottom

exception Offset_not_based_on_Null of
          Locations.Zone.t option * Location_Bytes.t * typ

exception Cannot_find_lv
let cannot_find_lv = Cannot_find_lv

exception Too_linear
let too_linear = Too_linear

type cond =
    { exp: exp; (* The condition of the branch*)
      positive: bool; (* true: normal false: negated *)}


let do_promotion_c ~with_alarms ~src_typ ~dst_typ v e_src =
  let rounding_mode = get_rounding_mode() in
  let msg fmt =
    Format.fprintf fmt "%a (%a)" Printer.pp_exp e_src Cvalue.V.pretty v
  in
  CilE.set_syntactic_context (CilE.SyUnOp e_src);
  Eval_op.do_promotion ~with_alarms rounding_mode ~src_typ ~dst_typ v msg


let rec lval_to_loc ~with_alarms state lv =
  let _, r, _typ = lval_to_loc_state ~with_alarms state lv in
  r

and lval_to_precise_loc ~with_alarms state lv =
  let _, r, _typ = lval_to_precise_loc_state ~with_alarms state lv in
  r

and lval_to_loc_state ~with_alarms state lv =
  let state,_,r, typ =
    lval_to_loc_deps_state
      ~with_alarms
      ~deps:None
      ~reduce_valid_index:(Kernel.SafeArrays.get ())
      state
      lv
  in
  state, r, typ

and lval_to_precise_loc_state ~with_alarms state lv =
  let state,_,r, typ =
    lval_to_precise_loc_deps_state
      ~with_alarms
      ~deps:None
      ~reduce_valid_index:(Kernel.SafeArrays.get ())
      state
      lv
  in
  state, r, typ

and lval_to_loc_deps_state ~with_alarms ~deps state ~reduce_valid_index lv =
  let state, deps, pl, typ =
    lval_to_precise_loc_deps_state
      ~with_alarms ~deps state ~reduce_valid_index lv
  in
  (*match fst pl with
    | PLVarOffset _ | PLLocOffset _ ->
      Value_parameters.result ~current:true "##Precise %a@." pretty_loc (fst pl)
    | _ -> ()*)
  state, deps, Precise_locs.imprecise_location pl, typ

and lval_to_precise_loc_deps_state ~with_alarms ~deps state ~reduce_valid_index (host,offset as lv) =
  if not (Cvalue.Model.is_reachable state) then
    state, deps, Precise_locs.loc_bottom, typeOfLval lv
  else
    let typ = match host with
    | Var host -> host.vtype
    | Mem x -> typeOf_pointed (typeOf x)
    in
    try
      let state, deps, offs, typ_offs =
        eval_offset ~with_alarms ~reduce_valid_index deps typ state offset
      in
      let state, deps, loc = eval_host ~with_alarms ~deps state host offs in
      let size = Eval_op.sizeof_lval_typ typ_offs in
      let loc = Precise_locs.make_precise_loc loc ~size in
      state, deps, loc, typ_offs
    with Offset_not_based_on_Null(deps,offset,typ_offs) ->
      let state, deps, loc_no_offset =
        eval_host ~with_alarms ~deps state host Precise_locs.offset_zero
      in
      let imprecise = Precise_locs.imprecise_location_bits loc_no_offset in
      let loc = Location_Bits.join (loc_bytes_to_loc_bits offset) imprecise in
      let size = Eval_op.sizeof_lval_typ typ_offs in
      let loc =
        Precise_locs.(make_precise_loc (inject_location_bits loc) ~size)
      in
      state, deps, loc, typ_offs

(* Combination of the evaluation of the right part of an lval (an host) with
   an offset, to obtain a location *)
and eval_host ~with_alarms ~deps state host offs =
  if Precise_locs.is_bottom_offset offs
  then
    Cvalue.Model.bottom, deps, Precise_locs.bottom_location_bits
  else (
    match host with
    | Var host ->
        let base = Base.of_varinfo host in
        state, deps, Precise_locs.combine_base_precise_offset base offs
    | Mem x ->
        let state, deps, loc_lv =
          eval_expr_with_deps_state ~with_alarms deps state x
        in
        let loc_bits = loc_bytes_to_loc_bits loc_lv in
        let loc_p = Precise_locs.combine_loc_precise_offset loc_bits offs in
        state, deps, loc_p
  )

(** Detects if an expression can be considered as a lvalue even though it is
    hidden by a cast that does not change the lvalue. Raises [exn] if it cannot.

    TODO: When the goal is to recognize the form (cast)l-value == expr,
    it would be better and more powerful to have chains of inverse functions *)
and pass_cast state exn typ e =
  let typeofe = typeOf e in
  (* Any volatile attribute may have an effect on the expression value *)
  if hasAttribute "volatile" (typeAttrs typeofe)
    || hasAttribute  "volatile" (typeAttrs typ)
  then raise exn;
(*  Format.printf "pass_cast %a as %a@." Printer.pp_exp e Printer.pp_typ typ; *)
  match unrollType typ, unrollType typeofe with
    | (TInt _ | TEnum _), (TInt _ | TEnum _) ->
        let sztyp = sizeof typ in
        let szexpr = sizeof typeofe in
        let styp, sexpr =
          match sztyp, szexpr with
            | Int_Base.Value styp, Int_Base.Value sexpr -> styp, sexpr
            | _ -> raise exn
        in
        let sityp = is_signed_int_enum_pointer typ in
        let sisexpr = is_signed_int_enum_pointer typeofe in
        if (Int.ge styp sexpr && sityp = sisexpr) (* larger, same signedness *)
          || (Int.gt styp sexpr && sityp) (* strictly larger and signed *)	
        then ()
        else
          (* try to ignore the cast if it acts as identity on the value [e] *)
          let size = bitsSizeOf typ in
          let all_values =
            V.create_all_values ~size ~signed:sityp ~modu:Integer.one
          in
          let with_alarms = Value_util.with_alarms_raise_exn exn in
          if not (V.is_included (eval_expr ~with_alarms state e) all_values)
          then raise exn

    | TFloat (f1,_), TFloat (f2, _) ->
        if Cil.frank f1 < Cil.frank f2
        then raise exn (* TODO: check value inclusion as in the integer case *)

    | _ -> raise exn (* Not a scalar type *)

and find_lv ~with_alarms (state:Cvalue.Model.t) ee =
  (* [BM] Do not recognize an lval whenever a volatile is involved to
     prevent copy/paste optimization. IS THIS THE RIGHTPLACE PC ?*)
  if hasAttribute "volatile" (typeAttrs (typeOf ee)) then
    raise cannot_find_lv;
  match ee.enode with
  | Lval lv -> lv
  | CastE (typ,e) ->
      pass_cast state cannot_find_lv typ e;
      find_lv ~with_alarms state e
  | _ -> raise cannot_find_lv

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
      | _ -> ()
  in
  aux e Ival.zero;
  (* Extlib.may
     (fun (lv,ival) -> Format.printf "find_lv_plus %a=%a+%a\n"
     Printer.pp_exp e !d_lval lv Ival.pretty ival
     ) !acc; *)
  match !acc with
  | None -> raise cannot_find_lv
  | Some (lv, offs) -> lv, offs

(* Find locations on which it is interesting to proceed by case disjunction
   to evaluate the expression *)
and get_influential_vars state exp =
  let with_alarms = CilE.warn_none_mode in
  (*  Format.printf "get_influential cond:%a@.state:%a@."
      Printer.pp_exp cond
      Cvalue.Model.pretty state; *)
  let rec get_vars acc exp =
    let eval_offset off t =
      try
        let _, _, offset, _ =
          eval_offset ~reduce_valid_index:true ~with_alarms None t state off
        in
        Precise_locs.imprecise_offset offset
      with Offset_not_based_on_Null _ -> Ival.top
    in
    match exp.enode with
    | Lval (Var v, off as lv) ->
        let offset = eval_offset off v.vtype in
        if Ival.cardinal_zero_or_one offset
        then
          (* no variable in offset can be influential. Check the
             contents of the location, on which we might want to enumerate  *)
          let varid = Base.of_varinfo v in
          let loc =
            Locations.make_loc
              (Locations.Location_Bits.inject varid offset)
              (sizeof_lval lv)
          in
          let contents =
            Cvalue.Model.find ~conflate_bottom:true state ~with_alarms loc
          in
          if Location_Bytes.cardinal_zero_or_one contents
          then acc (* small cardinal: not influential *)
          else loc :: acc
        else
          (* A variable in offset may be influential. The contents themselves
             are not influential, because we would need to split both by
             offset and by content in sync. *)
          get_vars_offset acc off
    | Lval (Mem e, off as lv) ->
        let t = typeOf_pointed (typeOf e) in
        let offset = eval_offset off t in
        if Ival.cardinal_zero_or_one offset then
          let v = eval_expr ~with_alarms state e in
          if Location_Bytes.cardinal_zero_or_one v then
            let locbi = loc_bytes_to_loc_bits v in
            let locbi' = Location_Bits.shift offset locbi in
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
  get_vars [] exp

and reduce_by_valid_loc ~positive ~for_writing loc typ state =
  try
    let value = Cvalue.Model.find ~with_alarms:CilE.warn_none_mode
      ~conflate_bottom:true state loc
    in
    if Cvalue.V.is_imprecise value then
      (* we won't reduce anything anyway, and we may lose information if loc
         contains misaligned data *)
      raise cannot_find_lv;
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
        Cvalue.Model.reduce_previous_binding state loc reduced_value
    end
  with Cannot_find_lv -> state

and eval_binop ~with_alarms e deps state =
  match e.enode with
  | BinOp (op, e1, e2, typ) ->
    let state, deps, ev1 =
      eval_expr_with_deps_state ~with_alarms deps state e1
    in
    if V.is_bottom ev1 then
      Cvalue.Model.bottom, deps, V.bottom
    else
      let state, deps, ev2 =
        eval_expr_with_deps_state ~with_alarms deps state e2
      in
      if V.is_bottom ev2 then
        Cvalue.Model.bottom, deps, V.bottom
      else begin
        match unrollType (typeOf e1) with
            | TFloat (fkind, _) ->
                CilE.set_syntactic_context (CilE.SyUnOp e);
                let r = Eval_op.eval_binop_float ~with_alarms
                  (get_rounding_mode ()) (Some fkind)
                   ev1 op ev2
                in
                state, deps, r
            | TInt _ | TPtr (_, _) | _ as te1 ->
	      CilE.set_syntactic_context (CilE.SyBinOp(e, op, e1, e2));
		let v =
                  Eval_op.eval_binop_int ~with_alarms ~typ ~te1 ev1 op ev2
                in
		(* Warn if overflow during a non-bitwise operation *)
		let v = match op with
                | Shiftlt | Mult | MinusPP | MinusPI | IndexPI | PlusPI
                | PlusA | Div | Mod | MinusA ->
                  let warn_unsigned = op <> Shiftlt in
                  Eval_op.handle_overflow ~with_alarms ~warn_unsigned typ v
                | _ -> v
		in
		state, deps, v
      end
  | _ -> assert false

and eval_expr ~with_alarms state e =
  let _, _, r = eval_expr_with_deps_state ~with_alarms None state e in r

and eval_expr_with_deps_state ~with_alarms deps state e =
  let state, deps, r =
    let orig_expr = Cil.stripInfo e in
    match orig_expr.enode with
    | Info _ -> assert false
    | Const v ->
        let r =
          begin match v with
          | CInt64 (i,_k,_s) ->
              V.inject_int i (* TODO: missing checks for overflow *)
          | CChr c ->
              (match charConstToInt c with
              | CInt64 (i,_,_) -> V.inject_int i
              | _ -> assert false)
          | CReal (f, fkind, fstring) ->
              CilE.set_syntactic_context (CilE.SyUnOp e);
              Eval_op.eval_float_constant ~with_alarms f fkind fstring
          | CWStr _ | CStr _ ->
              V.inject (Base.of_string_exp e) Ival.zero
          | CEnum {eival = e} ->
              eval_expr ~with_alarms state e
          end
        in
        state, deps, r
    | BinOp _  ->
        eval_binop ~with_alarms orig_expr deps state
    | Lval lv ->
        eval_lval_and_convert ~with_alarms deps state (e, lv)
    | AddrOf v | StartOf v ->
        let state, deps, r, _ =
          lval_to_loc_deps_state ~with_alarms
            ~deps state v ~reduce_valid_index:false
        in
        state, deps, loc_to_loc_without_size r

    | CastE (typ, e) ->
        let state, deps, evaled_expr =
          eval_expr_with_deps_state ~with_alarms deps state e
        in
        let r = do_promotion_c ~with_alarms
          ~dst_typ:typ ~src_typ:(typeOf e) evaled_expr e in
        state, deps, r

    | SizeOf _ | SizeOfE _ | SizeOfStr _ | AlignOf _ | AlignOfE _ ->
        let e = Cil.constFold true orig_expr in
        let r = match e.enode with
        | Const (CInt64 (v, _, _)) -> Cvalue.V.inject_int v
        | _ ->
            CilE.do_warn with_alarms.CilE.imprecision_tracing
              (fun _ ->
                 Value_parameters.result ~current:true
                   "cannot interpret sizeof or alignof (incomplete type)"
              );
            V.top_int
        in
        state, deps, r

    | UnOp (op, e, _t_res) ->
        let state, deps, expr =
          eval_expr_with_deps_state ~with_alarms deps state e in
        let syntactic_context = match op with
        | Neg -> CilE.SyUnOp orig_expr (* Can overflow *)
        | BNot -> CilE.SyUnOp orig_expr (* does in fact never raise an alarm*)
        | LNot -> CilE.SyUnOp e
	(* Can raise a pointer comparison. CilE needs [e] there *)
        in
        let t = unrollType (typeOf e) in
        CilE.set_syntactic_context syntactic_context;
        let result =
          Eval_op.eval_unop ~check_overflow:true ~with_alarms expr t op
        in
        state, deps, result
  in
  let r =
    if hasAttribute "volatile" (typeAttrs (typeOf e))
      && not (Cvalue.V.is_bottom r)
    then Eval_op.light_topify r
    else r
  in
  let typ = typeOf e in
  CilE.set_syntactic_context (CilE.SyUnOp e);
  (* TODO: the functions called above should respect the destination type.
     Calling reinterpret should be useless *)
  let rr = Eval_op.reinterpret ~with_alarms typ r in
  (if Cvalue.V.is_bottom rr then Cvalue.Model.bottom else state), deps, rr

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
            Printer.pp_exp e
            Locations.Location_Bytes.pretty result_without_subdiv;
          Cvalue.V.compare_min_float, Cvalue.V.compare_max_float
        end
      else begin
          Value_parameters.debug ~level:2
            "subdivfloatvar: optimizing integer expression %a=%a"
            Printer.pp_exp e
            Locations.Location_Bytes.pretty result_without_subdiv;
          Cvalue.V.compare_min_int, Cvalue.V.compare_max_int
        end
    in
    let vars = get_influential_vars state e in
    Value_parameters.debug ~level:2 "subdivfloatvar: variable list=%a"
      (Pretty_utils.pp_list Locations.pretty)
      vars;
    let rec try_sub vars =
      match vars with
      | [] | [ _ ] -> r
      | v :: tail ->
          try
            if not (List.exists (fun x -> Locations.loc_equal v x) tail)
            then raise too_linear;
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
            then raise too_linear;

            let working_list = ref [ (v_value, result_without_subdiv) ] in
	    let bound1, bound2 = Cvalue.V.min_and_max_float v_value in
	    let compute subvalue =
	      let substate =
                Cvalue.Model.reduce_previous_binding state v subvalue
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
                Cvalue.Model.reduce_previous_binding state v reduced_var,
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

(* [loc] is the location pointed to by [lv]. If [lv] is precise enough, we
   reduce it to the parts of [loc] that are valid for a read/write operation *)
and reduce_by_accessed_loc ~for_writing state lv loc =
  let with_alarms = CilE.warn_none_mode in
  let valid_loc = Locations.valid_part ~for_writing loc in
  let state =
    if Location_Bits.equal loc.loc valid_loc.loc
    then state
    else try
      match lv with
      | Mem (exp_mem), offs ->
	  let state =
	    if Cil.isConstantOffset offs 
	    then
              (* offset coming from [offs] *)
              let offs = match offs with
              | NoOffset -> Ival.zero
              | _ ->
                  let typ_exp = Cil.typeOf_pointed (typeOf exp_mem) in
                  let offs_bytes = fst (Cil.bitsOffset typ_exp offs) / 8 in
                  Ival.inject_singleton (Int.of_int offs_bytes)
              in
              (try
		  (* Decompose [exp_mem] into a base lvalue and an offset *)
		  let lv_mem, plus = find_lv_plus_offset
		    ~with_alarms state exp_mem in
		  (* Total offset, still in bytes *)
		  let plus = Ival.add_int plus offs in
		  let state, loc_mem, _typ_plus =
                    lval_to_loc_state ~with_alarms state lv_mem
		  in
		  let loc_mem = Locations.valid_part ~for_writing loc_mem in
		  if Location_Bits.is_relationable loc_mem.Locations.loc
		  then
		    (* is_relationable guarantees that [loc_mem] is a single binding,
                       that can be safely reduced. The valid (reduced) value
                       the original location shifted by [-plus] *)
		    let new_val =
                      Location_Bytes.shift
			(Ival.neg plus) (loc_bits_to_loc_bytes valid_loc.loc)
		    in
		    (* [new_val] is not necessarily included in previous
                       binding, use [reduce_binding] *)
		    Cvalue.Model.reduce_binding state loc_mem new_val
		  else state
		with Cannot_find_lv (* find_lval_plus_offset *) -> 
		  state)
	    else state
	  in
	  let rec aux e =
	    ( match e.enode with
	    | BinOp((PlusPI|IndexPI), p,
		   exp_index , typ) ->
		let base_pointer = eval_expr ~with_alarms state p in
		if Cvalue.V.cardinal_zero_or_one base_pointer
		then begin
		    let lv_index = find_lv ~with_alarms state exp_index in
		    let loc_index = lval_to_loc state ~with_alarms lv_index in
		    if Location_Bits.is_relationable loc_index.Locations.loc
		    then
		      let old_index_val = 
			Cvalue.Model.find
			  ~conflate_bottom:true
			  ~with_alarms
			  state loc_index
		      in
		      if Cvalue.V.is_included old_index_val Cvalue.V.top_int
		      then 
			let size_pointed =
			  Int.of_int ((bitsSizeOf (Cil.typeOf_pointed typ)))
			in
			let size_pointed_bytes =
			  Int.div size_pointed (Bit_utils.sizeofchar())
			in
			let old_index_ival = 
			  Cvalue.V.project_ival 
			    old_index_val
			in
                        let old_index_ival = Ival.scale size_pointed_bytes old_index_ival in
			let accessed_loc =
                          Location_Bytes.shift old_index_ival base_pointer
			in
			let accessed_loc =
			  Locations.make_loc
			    (loc_bytes_to_loc_bits accessed_loc)
			    (Int_Base.inject size_pointed)
			in
			let valid_accessed_loc = 
			  Locations.valid_part ~for_writing accessed_loc 
			in
			if not (Location_Bits.equal valid_accessed_loc.loc accessed_loc.loc)
			then
                          if Locations.is_bottom_loc valid_accessed_loc then
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
                              Locations.pretty loc Printer.pp_lval lv for_writing
                              Cvalue.Model.pretty state
                              Cvalue.V.pretty new_index_val;
                            )
			  in
			  let new_index_val = Cvalue.V.inject_ival new_index_val in
			  Cvalue.Model.reduce_previous_binding 
			    state loc_index new_index_val
			else
			  state

		      else state
		    else state
		  end
		else state

	    | CastE(typ,e) ->
		pass_cast state cannot_find_lv typ e;
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
    with Cil.SizeOfError _ (* from Cil.bits... and others *) -> state
  in state, valid_loc

 (* Auxiliary function for [eval_lval] below. We are evaluating the location
    [loc] that resulted from the evaluation of [lv]. *)
 and eval_lval_one_loc ~conflate_bottom ~with_alarms deps state lv typ_lv loc =
    CilE.set_syntactic_context (CilE.SyMem lv);
    let state, result =
      if conflate_bottom
      then
	Cvalue.Model.find_and_reduce_indeterminate ~with_alarms state loc
      else 
	state, 
        (Cvalue.Model.find ~conflate_bottom ~with_alarms state loc)
    in
    let result = Eval_op.cast_lval_bitfield typ_lv loc.size result in
    let state, loc =
      warn_reduce_by_accessed_loc ~with_alarms ~for_writing:false state loc lv
    in
    Warn.warn_imprecise_lval_read ~with_alarms lv loc result;
    let new_deps =
      match deps with
      | None -> None
      | Some deps ->
          Some (Zone.join deps (enumerate_valid_bits ~for_writing:false loc))
    in
    state, new_deps, result

 and eval_lval ~conflate_bottom ~with_alarms deps state lv =
    let state, deps, precise_loc, typ_lv =
      lval_to_precise_loc_deps_state ~with_alarms
        ~deps state lv ~reduce_valid_index:(Kernel.SafeArrays.get ())
    in
    if Precise_locs.is_bottom_loc precise_loc then
      Model.bottom, deps, V.bottom, typ_lv
    else
      let aux loc (res_state, res_deps, res_result) =
        let state', deps, res' =
          eval_lval_one_loc ~with_alarms ~conflate_bottom
            res_deps state lv typ_lv loc
        in
        Model.join res_state state', deps, V.join res' res_result
      in
      let state, deps, res =
        Precise_locs.fold aux precise_loc (Model.bottom, deps, V.bottom)
      in
      state, deps, res, typ_lv

 and eval_lval_and_convert ~with_alarms deps state (e, lv) =
    let state, deps, oldv, typ =
      eval_lval ~conflate_bottom:true ~with_alarms deps state lv
    in
    CilE.set_syntactic_context (CilE.SyUnOp e);
    let newv = Eval_op.reinterpret ~with_alarms typ oldv in
    (* Reduce if the conversion has really improved the result; in particular
       float that are top_int are reduced there. On the other hand, we do not
       want to take into account conversions unsigned -> signed, etc. *)
    let state' =
      (* Currently, we only store the reduction infinite float -> finite. *)
      if V.equal oldv V.top_int && isFloatingType typ then
        try
          let loc, _v, _ = eval_as_exact_loc ~with_alarms ~locv:false state e in
          Cvalue.Model.reduce_previous_binding state loc newv
        with Not_an_exact_loc -> state
      else state
    in
    state', deps, newv

 (** We are accessing an array of size [array_size] at indexes [index] in state
     [state]. If index causes an out-of-bounds access, emit an informative
     alarm,  reduce [index], and if possible reduce [index_exp] in [state]. *)
 and reduce_index ~with_alarms array_size_exp array_size index_exp index state =
  let array_range =
    Ival.inject_range (Some Int.zero) (Some (Integer.pred array_size))
  in
  let new_index = Ival.narrow index array_range in
  if Ival.equal new_index index
  then state, index
  else begin
    CilE.do_warn with_alarms.CilE.others
      (fun _ ->
	let range = Pretty_utils.to_string Ival.pretty index in
	let positive = match Ival.min_int index with
	  | None -> false
	  | Some min -> Int.ge min Int.zero
	in
        let size = Extlib.the array_size_exp (* array_size exists *) in
        (* first [index_exp] is unused *)
        let sc = CilE.SyBinOp (index_exp, IndexPI, index_exp, size) in
	CilE.set_syntactic_context sc;
	CilE.warn_index with_alarms ~positive ~range
      );
    begin
      try
        let loc,_,_=
          eval_as_exact_loc ~with_alarms ~locv:false state index_exp
        in
        let ival_new_index = V.inject_ival new_index in
        let new_state =
          Cvalue.Model.reduce_previous_binding state loc ival_new_index
        in
        new_state, new_index
      with Not_an_exact_loc  -> state, new_index
    end
  end

 and eval_offset ~with_alarms ~reduce_valid_index deps typ state offset =
    match offset with
    | NoOffset ->
	state, deps, Precise_locs.offset_zero, typ
    | Index (exp,remaining) ->
	let typ_pointed,array_size = match (unrollType typ) with
	| TArray (t,size,_,_) -> t, size
	| t ->
           Value_parameters.fatal ~current:true "Got type '%a'" Printer.pp_typ t
	in
	let state, deps, index =
          eval_expr_with_deps_state ~with_alarms deps state exp
	in
	if V.is_bottom index
	then
          let typ_offset = typeOffset typ_pointed remaining in
          Cvalue.Model.bottom, deps, Precise_locs.offset_bottom, typ_offset
	else begin
          try
            let index_i = V.project_ival index in
            let state, index_i =
              try
	        if reduce_valid_index then
                  let array_size_i = lenOfArray64 array_size in
                  reduce_index ~with_alarms
                    array_size array_size_i exp index_i state
	        else state, index_i
              with LenOfArray -> state, index_i (* unknown array size *)
            in
            (* Index offsets expressed in terms of the array elements size *)
            let index_i = Ival.scale_int_base (sizeof typ_pointed) index_i in
            (* offset(s) for each cell *)
            let state, deps, roffset, typ_offs =
              eval_offset ~reduce_valid_index ~with_alarms
                deps typ_pointed state remaining
            in
            (* Combine the two offsets *)
            state,deps, Precise_locs.shift_offset index_i roffset, typ_offs
          with V.Not_based_on_null ->
            (* result will be a garbled mix: collect all the bases involved in
               the evaluation of [offset], and raise an exception *)
            let bases_index = Cvalue.V.topify_arith_origin index in
            let deps, bases =
              topify_offset ~with_alarms deps state bases_index remaining
            in
            let typ_offset = typeOffset typ_pointed remaining in
            raise (Offset_not_based_on_Null (deps, bases, typ_offset))
        end
    | Field (fi,remaining) ->
        let attrs = filter_qualifier_attributes (typeAttr typ)  in
        let typ_fi = typeAddAttributes attrs fi.ftype in
	let state, deps, r, typ =
          eval_offset ~with_alarms
            ~reduce_valid_index deps typ_fi state remaining
	in
        let off =
          try
            let field = fst (bitsOffset typ (Field(fi,NoOffset))) in
            Precise_locs.shift_offset_by_singleton (Int.of_int field) r
          with Cil.SizeOfError _ -> Precise_locs.offset_top
        in
	state, deps, off, typ
 and topify_offset ~with_alarms deps state acc offset =
    match offset with
    | NoOffset -> deps,acc
    | Field (_fi,remaining) ->
        topify_offset ~with_alarms deps state acc remaining
    | Index (exp,remaining) ->
	let _, deps, loc_index =
          eval_expr_with_deps_state ~with_alarms deps state exp
        in
	let acc = Location_Bytes.join
          (Cvalue.V.topify_arith_origin loc_index)
          acc
	in
	topify_offset ~with_alarms deps state acc remaining

 and eval_as_exact_loc ~with_alarms ?(locv=true) state e =
    try
      let lv = find_lv ~with_alarms state e in
      let _, loc, typ = lval_to_loc_state ~with_alarms state lv in
      let loc = Locations.valid_part ~for_writing:false loc in
      if not (cardinal_zero_or_one loc) then raise not_an_exact_loc;
      let v =
        if locv then begin
          CilE.set_syntactic_context (CilE.SyMem lv);
          let v = Cvalue.Model.find ~with_alarms
            ~conflate_bottom:true state loc
          in
          CilE.set_syntactic_context (CilE.SyUnOp e);
          let v' = Eval_op.reinterpret ~with_alarms typ v in
          let v' = Eval_op.cast_lval_bitfield typ loc.size v' in
          v'
        end else
          V.bottom
      in
      loc, v, typ
    with Cannot_find_lv ->
      raise not_an_exact_loc

and warn_reduce_by_accessed_loc ~with_alarms ~for_writing state loc lv =
  let warn = not (Locations.is_valid ~for_writing loc) in
  if warn then begin
    CilE.set_syntactic_context (CilE.SyMem lv);
    (if for_writing then CilE.warn_mem_write else CilE.warn_mem_read)
      with_alarms;
    (* The calls to [is_valid] and to [reduce_by_accessed_loc] below cannot be
       fused because of bases with validity unkwnown *)
    reduce_by_accessed_loc ~for_writing state lv loc
  end
  else
    state, loc


let reduce_rel_from_type t =
  if isIntegralType t || isPointerType t
  then Eval_op.reduce_rel_int
  else Eval_op.reduce_rel_float (Value_parameters.AllRoundingModes.get ())

(** Reduce the state for comparisons of the form 'v Rel k', where v
    evaluates to a location, and k to some value *)
let reduce_by_left_comparison_abstract eval pos expl binop expr state =
  let with_alarms = CilE.warn_none_mode in
  try
    let loc, val_for_loc, invert, val_compared, typ_loc = 
      try
	let loc, value, typ =
	  eval_as_exact_loc ~with_alarms state expl 
	in
	loc, value, (fun x -> x), value, typ
      with
	Not_an_exact_loc ->
	  let invert_cast e1 typ_loc =
	      let loc, val_for_loc, typ_for_loc =
		eval_as_exact_loc ~with_alarms state e1
	      in
	      ( match Cil.unrollType typ_for_loc with
	      | TFloat ((FDouble|FFloat) as fk, _) ->
		  let single_precision = fk = FFloat in
		  let size = bitsSizeOf typ_loc in
		  let signed = isSignedInteger typ_loc in
		  let _, _, _, val_compared =
                    V.cast_float_to_int ~signed ~size val_for_loc
                  in
		  loc, val_for_loc, 
		  (V.cast_float_to_int_inverse ~single_precision), 
		  val_compared, typ_loc
	      | _ -> raise not_an_exact_loc)
	  in
	  ( match expl.enode with
	  | CastE (typ_larger, { enode = CastE(typ_loc,e1) } )
	      when isIntegralType typ_loc && isIntegralType typ_larger &&
(		bitsSizeOf typ_larger > bitsSizeOf typ_loc &&
		isSignedInteger typ_loc ) (* TODOBY: this should be
                                                  implemented using pass_cast *)
		->
	      invert_cast e1 typ_loc
	  | CastE (typ_loc, e1) when isIntegralType typ_loc ->
	      invert_cast e1 typ_loc
	  | _ -> raise not_an_exact_loc)
	    
    in
    let cond_v = expr in
    let v_sym =
      eval.Eval_op.reduce_rel_symmetric pos binop cond_v val_compared in
    let v_asym =
      eval.Eval_op.reduce_rel_antisymmetric ~typ_loc pos binop cond_v v_sym in
(*    Format.printf "reduce_by_left %a -> %a -> %a@." 
      Cvalue.V.pretty val_for_loc
      Cvalue.V.pretty val_compared
      Cvalue.V.pretty v_asym; *)
    if V.equal v_asym V.bottom then raise reduce_to_bottom;
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
	    Cvalue.Model.reduce_previous_binding state loc new_val_for_loc
	  end )
  with
  | Not_an_exact_loc | Cil.SizeOfError _ -> state

let reduce_by_left_comparison eval pos expl binop expr state =
  let expr = eval_expr ~with_alarms:CilE.warn_none_mode state expr in
  reduce_by_left_comparison_abstract eval pos expl binop expr state

(** Reduce the state for comparisons of the form
    'v Rel k', 'k Rel v' or 'v = w' *)
let reduce_by_comparison reduce_rel pos exp1 binop exp2 state =
(*  Format.printf "red_by_comparison  %a@." Cvalue.Model.pretty state; *)
  let state = reduce_by_left_comparison reduce_rel pos exp1 binop exp2 state in
  let inv_binop = match binop with
    | Gt -> Lt | Lt -> Gt | Le -> Ge | Ge -> Le
    | _ -> binop
  in
  reduce_by_left_comparison reduce_rel pos exp2 inv_binop exp1 state


(* Try to make the condition true by evaluating important locations, proceeding
   by case disjunction on them, and removing values that make the condition
   false. Raises [Reduce_to_bottom] instead of returning [Model.bottom] *)
let reduce_by_cond_enumerate state cond locs =
  let with_alarms = CilE.warn_none_mode in
  let condition_may_still_be_true_in_state state =
    let vcond = eval_expr ~with_alarms state cond.exp in
    if cond.positive
    then V.contains_non_zero vcond
    else
      if Value_parameters.UndefinedPointerComparisonPropagateAll.get()
      then V.contains_zero vcond
      else V.is_included V.singleton_zero vcond
  in
  let is_enumerable loc =
    let v = Cvalue.Model.find ~conflate_bottom:true ~with_alarms state loc in
    let upto = succ (Ival.get_small_cardinal()) in
    ignore (Location_Bytes.cardinal_less_than v upto);
    v
  in
  let rec enumerate_one_var l =
    match l with
      | [] -> raise Not_found
      | loc :: q ->
          try
            let v = is_enumerable loc in
            loc, v, q
          with Abstract_interp.Not_less_than -> enumerate_one_var q
  in
  try
    let loc, vloc, _tail = enumerate_one_var locs in
    (* Format.printf "enumerate %a %a@."  Location.pretty loc V.pretty vloc;*)
    let f one_val acc =
      (* interpret cond in an environment where v -> one_val *)
      let env =
        Cvalue.Model.reduce_previous_binding state loc one_val
      in
      let stays = condition_may_still_be_true_in_state env in
      (* Format.printf "enumerate %a stays:%B@." V.pretty one_val stays; *)
      if stays then Location_Bytes.join one_val acc else acc
    in
    let newv =
      Location_Bytes.fold_enum f vloc Location_Bytes.bottom
    in
    if V.is_bottom newv
    then raise reduce_to_bottom
    else if V.equal newv vloc
    then state
    else
      Cvalue.Model.reduce_previous_binding state loc newv
  with Not_found -> state


(** raises [Reduce_to_bottom] and never returns [Cvalue.Model.bottom]*)
let reduce_by_cond state cond =
  (* Do not reduce anything if the cond is volatile.
     (This test is dumb because the cond may contain volatile lvalues
     without the "volatile" attribute appearing at toplevel. pc 2007/11) *)
  if hasAttribute "volatile" (typeAttr (typeOf cond.exp)) then state
  else
    let rec aux cond state =
      (*Format.printf "eval_cond_aux %B %a@." cond.positive 
	Printer.pp_exp cond.exp;*)
      match cond.positive,cond.exp.enode with
      | _, (BinOp ((Eq | Ne) as eqop, ({enode = BinOp (Mod, ({enode = Lval _ } as expl), 
					exp12, typ1)} as exp1), 
		   exp2, _)) 
      | _, (BinOp ((Eq | Ne) as eqop, exp2, 
		   ({enode = BinOp (Mod, ({enode = Lval _ } as expl),
				    exp12, typ1)} as exp1), _))  ->
        let reduce_rel = reduce_rel_from_type (unrollType typ1) in
	( try
	    let with_alarms = CilE.warn_none_mode in
	    let a12 = V.project_ival (eval_expr ~with_alarms state exp12) in
	    let modu = Ival.project_int a12 in
	    let a2 = V.project_ival (eval_expr ~with_alarms state exp2) in
	    let i2 = Ival.project_int a2 in
	    let loc, value, _ = eval_as_exact_loc ~with_alarms state expl in
(*	    Format.printf "loc:%a value:%a == %a %% %a\n"
	      Locations.pretty loc
	      V.pretty value
	      Int.pretty i2
	      Int.pretty modu; *)
	    let av = V.project_ival value in
	    ( match av with
	      Ival.Top _ | Ival.Set _ ->
		let r = i2 in
		if Int.le modu Int.zero then raise not_an_exact_loc; (* TODO *)
		  let min, max, r = 
		    if (eqop = Ne) = cond.positive (* really not equal *)
		    then begin
		      if Int.equal modu Int.two && Int.is_zero r
		      then None, None, Int.one
		      else raise not_an_exact_loc 
		    end
		    else
		      if Int.is_zero r then
			None, None, r
		      else if Int.gt r Int.zero
		      then		    
			Some (Int.round_up_to_r ~min:Int.zero ~r ~modu), None, r
		      else
		    (* None, (Int.round_down_to_r ~max:Int.zero ~r ~modu), (r *)
			raise not_an_exact_loc (* TODO *)
		in
		if Int.ge (Int.abs r) modu 
		then Model.bottom
		else
		  let reducer = Ival.inject_top min max r modu in
		  let reduced_value = 
		    Ival.meet (* exact here *)
		      reducer
		      av
		  in
		  let state = 
		    Model.reduce_binding state loc (V.inject_ival reduced_value)
		  in
		  reduce_by_comparison reduce_rel cond.positive exp1 eqop exp2 
		    state
	    | Ival.Float _ -> raise not_an_exact_loc
	    )
	  with Not_an_exact_loc | V.Not_based_on_null | Ival.Not_Singleton_Int 
	    -> 
            reduce_by_comparison reduce_rel cond.positive exp1 eqop exp2 state)
      | _positive, BinOp ((Le|Ne|Eq|Gt|Lt|Ge as binop), exp1, exp2, _typ) ->
        let reduce_rel = reduce_rel_from_type (unrollType (typeOf exp1)) in
        reduce_by_comparison reduce_rel cond.positive exp1 binop exp2 state

      (* Strict or lazy operators can be handled uniformly here: there are
         no side effects inside expressions, and alarms should have been emitted
         prior to reducing *)
      | true,
        ( BinOp (LAnd, exp1, exp2, _)
        | BinOp (BAnd, (* 'cond1 & cond2' can be treated as 'e1 && e2' *)
                 ({ enode = BinOp ((Le|Ne|Eq|Gt|Lt|Ge), _, _, _)} as exp1),
                 ({ enode = BinOp ((Le|Ne|Eq|Gt|Lt|Ge), _, _, _)} as exp2),
                 _))
      | false,
        ( BinOp (LOr, exp1, exp2, _)
        | BinOp (BOr, (* '!(cond1 | cond2)' can be treated as '!(e1 || e2)' *)
                 ({ enode = BinOp ((Le|Ne|Eq|Gt|Lt|Ge), _, _, _)} as exp1),
                 ({ enode = BinOp ((Le|Ne|Eq|Gt|Lt|Ge), _, _, _)} as exp2),
                 _))
          ->
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
          if Db.Value.is_reachable r then r else raise reduce_to_bottom

      | _, UnOp(LNot,exp,_) ->
          aux { positive = not cond.positive; exp = exp; } state

      | _, CastE (typ, e) ->
        (try
           pass_cast state Exit typ e;
           aux { cond with exp = e} state
         with Exit -> 
	   if  isIntegralType typ || isPointerType typ
	   then 
	     reduce_by_left_comparison_abstract 
	       Eval_op.reduce_rel_int
               cond.positive cond.exp Ne V.singleton_zero state
	   else state)
      | _, Lval _ when (let t = typeOf cond.exp in
                        isIntegralType t || isPointerType t)
          -> (* "if (c)" is equivalent to "if(!(c==0))" *)
	  reduce_by_left_comparison_abstract 
	    Eval_op.reduce_rel_int
            cond.positive cond.exp Ne V.singleton_zero state	    
      | _ -> state
    in
    let result = aux cond state in
    (* If the condition does not evaluate exactly to true (or false if [cond] is
       negative), we reduce more agressively by splitting on some variables *)
    let evaled = eval_expr ~with_alarms:CilE.warn_none_mode result cond.exp in
    let reduce_more =
      if cond.positive
      then V.contains_zero evaled
      else V.contains_non_zero evaled
    in
    if reduce_more then
      let split_on = get_influential_vars result cond.exp in
      reduce_by_cond_enumerate result cond split_on
    else
      result


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
    if with_alarms.CilE.others.CilE.a_log <> None then
      warning_once_current
        "@[Function pointer@ and@ pointed function@ '%a' @ have@ incompatible \
            types:@ %a@ vs.@ %a.@ assert(function type matches)@]"
        Printer.pp_varinfo vi Printer.pp_typ typ_pointer Printer.pp_typ typ_fun;
    let compatible_sizes t1 t2 =
      try bitsSizeOf t1 = bitsSizeOf t2
      with Cil.SizeOfError _ -> false
    in
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
             | t1, t2 -> compatible_sizes t1 t2
          ) &&
          (* Argument lists of the same length, with compatible sizes between
             the arguments, or unspecified argument lists *)
          (match args1, args2 with
             | None, None | None, Some _ | Some _, None -> true
             | Some l1, Some l2 ->
                 List.length l1 = List.length l2 &&
                 List.for_all2
                   (fun (_, t1, _) (_, t2, _) -> compatible_sizes t1 t2)
                   l1 l2
          )
      | _ -> false

let resolv_func_vinfo ~with_alarms deps state funcexp =
  let warning_once_current fmt =
    let w = with_alarms.CilE.defined_logic in
    w.CilE.a_call ();
    match w.CilE.a_log with 
      | None -> Format.ifprintf Format.std_formatter fmt
      | Some _ -> warning_once_current fmt
  in
  match funcexp.enode with
  | Lval (Var vinfo,NoOffset) ->
      Kernel_function.Hptset.singleton (Globals.Functions.get vinfo), deps
  | Lval (Mem v,NoOffset) ->
      let _, deps, loc = eval_expr_with_deps_state ~with_alarms deps state v in
      let typ_pointer = typeOf funcexp in
      let pp_assert fmt =
        Format.fprintf fmt "assert(\\valid_fun_pointer(%a))" Printer.pp_exp funcexp
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
                | Base.CLogic_Var _ -> 
                  warning_once_current
                    "Function pointer call to a logic variable: \
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
        | Location_Bytes.Top (set, o) ->
          warning_once_current
            "Function pointer for call is imprecise: %t" pp_assert;
          try
            Base.SetLattice.fold
              (fun b acc -> match b with
                | Base.Var (v,_) | Base.Initialized_Var (v,_)
                      when Cil.isFunctionType v.vtype ->
                    if compatible_functions ~with_alarms v typ_pointer v.vtype
                    then
                      Kernel_function.Hptset.add (Globals.Functions.get v) acc
                    else acc
                | _ -> acc
              ) set Kernel_function.Hptset.empty
          with Base.SetLattice.Error_Top ->
            if Mark_noresults.no_memoization_enabled () then
              Value_parameters.abort ~current:true
                "Function pointer evaluates to anything. Try deactivating \
                 option(s) -no-results, -no-results-function and \
                 -obviously-terminates@."
            else
              Value_parameters.fatal ~current:true
                "Function pointer evaluates to anything. origin %a, function %a"
                Origin.pretty o Printer.pp_exp v
      in
      fundecs, deps
  | _ ->
      assert false

let offsetmap_of_lv ~with_alarms state lv =
  let state, loc_to_read, _typ =
    lval_to_precise_loc_state ~with_alarms state lv
  in
  CilE.set_syntactic_context (CilE.SyMem lv);
  let aux loc offsm_res =
    let copy = Cvalue.Model.copy_offsetmap ~with_alarms loc state in
    match copy, offsm_res with
      | None as r, None | (Some _ as r), None | None, (Some _ as r) -> r
      | Some r1, Some r2 -> Some (V_Offsetmap.join r1 r2)
  in
  loc_to_read,
  state,
  Precise_locs.fold aux loc_to_read None
  


(* -------------------------------------------------------------------------- *)
(* --- Registration inside Db                                             --- *)
(* -------------------------------------------------------------------------- *)

let () =
  Db.Value.find_lv_plus :=
    (fun ~with_alarms state e ->
      try [find_lv_plus_offset ~with_alarms state e]
      with Cannot_find_lv -> []);
;;


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
