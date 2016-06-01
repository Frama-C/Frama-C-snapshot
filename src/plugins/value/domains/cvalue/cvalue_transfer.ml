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
open Eval

open Cvalue.Model

type value = Main_values.CVal.t
type location = Main_locations.PLoc.location


let unbottomize = function
  | `Bottom -> Cvalue.V.bottom
  | `Value v -> v


(* ---------------------------------------------------------------------- *)
(*                       Functions on Offsetmap                           *)
(* ---------------------------------------------------------------------- *)

let make_right_value cvalue =
  let open Cvalue.V_Or_Uninitialized in
  let value = get_v cvalue in
  let v = if Cvalue.V.is_bottom value then `Bottom else `Value value in
  { v; initialized = is_initialized cvalue; escaping = not (is_noesc cvalue) }

let offsetmap_to_value typ offsm =
  let size = Integer.of_int (Cil.bitsSizeOf typ) in
  let validity = Base.validity_from_size size in
  let offsets = Ival.zero in
  let _, value = Cvalue.V_Offsetmap.find ~validity ~offsets ~size offsm in
  value

let find_right_value typ offsm = make_right_value (offsetmap_to_value typ offsm)

let offsetmap_of_v ~typ v =
  let size = Integer.of_int (Cil.bitsSizeOf typ) in
  let v = Cvalue.V_Or_Uninitialized.initialized v in
  Cvalue.V_Offsetmap.create ~size v ~size_v:size

exception Got_imprecise of Cvalue.V.t
let offsetmap_contains_imprecision offs =
  try
    Cvalue.V_Offsetmap.iter_on_values
      (fun v ->
         match Cvalue.V_Or_Uninitialized.get_v v with
         | Locations.Location_Bytes.Map _ -> ()
         | Locations.Location_Bytes.Top _ as v -> raise (Got_imprecise v)
      ) offs;
    None
  with Got_imprecise v -> Some v

exception Bottom
let remove_indeterminateness =
  let process_itv itv (v, size, offs) offsm =
    let open Cvalue.V_Or_Uninitialized in
    match v with
    | C_init_noesc _ -> offsm
    | C_init_esc v'  | C_uninit_esc v' | C_uninit_noesc v' ->
      if Cvalue.V.is_bottom v' then raise Bottom;
      Cvalue.V_Offsetmap.add itv (C_init_noesc v', size, offs) offsm
  in
  fun typ offsm ->
    if Cil.isArithmeticOrPointerType typ then
      try `Value (Cvalue.V_Offsetmap.fold process_itv offsm offsm)
      with Bottom -> `Bottom
    else
      `Value offsm

let offsetmap_from_location location state =
  let aux loc offsm_res =
    let size = Int_Base.project loc.Locations.size in
    let _, copy = Cvalue.Model.copy_offsetmap loc.Locations.loc size state in
    Cvalue.V_Offsetmap.join_top_bottom copy offsm_res
  in
  Precise_locs.fold aux location `Bottom


module Transfer
    (Valuation: Abstract_domain.Valuation with type value = value
                                           and type origin = bool
                                           and type loc = location)
= struct

  type state = Cvalue.Model.t
  type summary = Cvalue.V_Offsetmap.t option

  (* ---------------------------------------------------------------------- *)
  (*                               Assumptions                              *)
  (* ---------------------------------------------------------------------- *)

  let reduce valuation lval value t =
    let typ = Cil.typeOfLval lval in
    if Cil.typeHasQualifier "volatile" typ
    then t
    else
      match Valuation.find_loc valuation lval with
      | `Value record ->
        let loc = Precise_locs.imprecise_location record.loc in
        if Locations.cardinal_zero_or_one loc
        then reduce_indeterminate_binding t loc value
        else t
      | `Top -> t (* Cannot reduce without the location of the lvalue. *)

  (* Update the state according to a Valuation. *)
  let update valuation t =
    let process exp record t =
      match exp.enode with
      | Lval lv ->
        if record.reductness = Reduced || record.origin = Some true
        then
          let {v; initialized; escaping} = record.value in
          let v = unbottomize v in
          let value = Cvalue.V_Or_Uninitialized.make ~initialized ~escaping v in
          reduce valuation lv value t
        else t
      | _ -> t
    in
    let s = Valuation.fold process valuation t in
    s

  let assume _stmt _expr _positive valuation state = `Value (update valuation state)


  (* ---------------------------------------------------------------------- *)
  (*                              Assignments                               *)
  (* ---------------------------------------------------------------------- *)

  let with_alarms = Value_util.warn_all_quiet_mode ()

  let warn_imprecise_lval_read lval loc v =
    Warn.warn_imprecise_lval_read ~with_alarms lval loc v

  let warn_right_imprecision lval loc v =
    Warn.warn_right_exp_imprecision ~with_alarms lval loc v

  let write_abstract_value state (lval, loc, typ) assigned_value =
    let {v; initialized; escaping} = assigned_value in
    let value = unbottomize v in
    warn_right_imprecision lval loc value;
    let value =
      if Cil.typeHasQualifier "volatile" typ then
        (* Do not cast further, the offsetmap layer prefers this form. *)
        Cvalue_forward.make_volatile value
      else
        Cvalue_forward.cast_lval_if_bitfield typ value
    in
    match loc.Locations.loc with
    | Locations.Location_Bits.Top (Base.SetLattice.Top, orig) ->
      Value_parameters.result
        "State before degeneration:@\n======%a@\n======="
        Cvalue.Model.pretty state;
      Value_util.warning_once_current
        "writing at a completely unknown address@[%a@].@\nAborting."
        Origin.pretty_as_reason orig;
      raise Db.Value.Aborted
    | _ ->
      let exact = Locations.cardinal_zero_or_one loc in
      let value =
        Cvalue.V_Or_Uninitialized.make ~initialized ~escaping value in
      (* let value = Cvalue.V_Or_Uninitialized.initialized value in *)
      snd (add_unsafe_binding ~exact state loc value)

  exception Do_assign_imprecise_copy

  let copy_one_loc state left_lv right_lv determinize =
    let left_lval, left_loc, left_typ = left_lv
    and right_lval, right_loc, right_typ = right_lv in
    (* Warn if right_loc is imprecise *)
    warn_imprecise_lval_read right_lval right_loc Cvalue.V.bottom;
    (* top size is tested before this function is called, in which case
       the imprecise copy mode is used. *)
    let size = Int_Base.project right_loc.Locations.size in
    let offsetmap = snd (copy_offsetmap right_loc.Locations.loc size state) in
    let make_volatile =
      Cil.typeHasQualifier "volatile" left_typ ||
      Cil.typeHasQualifier "volatile" right_typ
    in
    match offsetmap with
    | `Bottom -> `Bottom
    | `Top ->
      Value_parameters.abort ~current:true ~once:true
        "completely imprecise state during evaluation. Aborting."
    | `Map offsm ->
      (* TODO: this is the good place to handle partially volatile
         struct, whether as source or destination *)
      let offsetmap =
        if make_volatile then
          Cvalue.V_Offsetmap.map_on_values
            (Cvalue.V_Or_Uninitialized.map Cvalue_forward.make_volatile) offsm
        else offsm
      in
      if not (Cvalue_forward.offsetmap_matches_type left_typ offsetmap) then
        raise Do_assign_imprecise_copy;
      let offsetmap =
        if determinize
        then remove_indeterminateness left_typ offsetmap
        else `Value offsetmap
      in
      offsetmap >>-: fun offsetmap ->
      let () =
        match offsetmap_contains_imprecision offsetmap with
        | Some v -> warn_right_imprecision left_lval left_loc v
        | _ -> ()
      in
      snd (paste_offsetmap ~reducing:false ~exact:true
             ~from:offsetmap ~dst_loc:left_loc.Locations.loc ~size state)

  let define value = { v = `Value value;
                       initialized = true;
                       escaping = false }

  let copy_right_lval state valuation left_lv right_lval copied_value =
    let (lval, loc, typ) = left_lv in
    let right_loc, right_typ =
      match Valuation.find_loc valuation right_lval with
      | `Value record -> record.loc, record.typ
      | `Top -> raise Do_assign_imprecise_copy
    in
    (* Size mismatch between left and right size, or imprecise size.
       This cannot be done by copies, but require a conversion *)
    let right_size = Main_locations.PLoc.size right_loc
    and left_size = Main_locations.PLoc.size loc in
    if not (Int_Base.equal left_size right_size)
    || Int_Base.is_top right_size
    || Eval_typ.is_bitfield typ
    then
      let assigned_value = match copied_value with
        | Exact right_value -> right_value
        | Determinate value -> define value.v
      in
      fun loc -> write_abstract_value state (lval, loc, typ) assigned_value
    else
      let determinize = match copied_value with
        | Exact _       -> false
        | Determinate _ -> true
      in
      fun loc ->
        try
          let process right_loc acc =
            let left_lv = lval, loc, typ
            and right_lv = right_lval, right_loc, right_typ in
            match copy_one_loc state left_lv right_lv determinize with
            | `Bottom -> acc
            | `Value state -> join acc state
          in
          Precise_locs.fold process right_loc bottom
        with
          Do_assign_imprecise_copy ->
          let assigned_value = match copied_value with
            | Exact right_value -> right_value
            | Determinate value -> define value.v
          in
          write_abstract_value state (lval, loc, typ) assigned_value

  let assign _stmt { lval; ltyp; lloc } _expr assigned valuation state =
    let state = update valuation state in
    let assign_one_loc =
      match assigned with
      | Assign value ->
        let assigned_value = define value in
        fun loc -> write_abstract_value state (lval, loc, ltyp) assigned_value
      | Copy (right_lval, copied_value) ->
        copy_right_lval state valuation (lval, lloc, ltyp) right_lval copied_value
    in
    let aux_loc loc acc_state =
      let s = assign_one_loc loc in
      join acc_state s
    in
    let state = Precise_locs.fold aux_loc lloc bottom in
    if not (is_reachable state)
    then `Bottom
    else `Value state


  (* ---------------------------------------------------------------------- *)
  (*                               Builtins                                 *)
  (* ---------------------------------------------------------------------- *)

  let add_binding ~exact state loc value =
    let value = Cvalue.V_Or_Uninitialized.initialized value in
    let _alarm, state = add_binding_unspecified ~exact state loc value in
    state

  let va_start valuation state args =
    match args with
    | [{enode = Lval lv}, _, _] ->
      let loc = match Valuation.find_loc valuation lv with
        | `Value record -> Precise_locs.imprecise_location record.loc
        | `Top -> assert false
      in
      let state = add_binding ~exact:true state loc Cvalue.V.top_int in
      { Value_types.c_values = [ None, state] ;
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None;
      }
    | _ -> assert false

  let va_arg _valuation state args =
    match args with
    | [_; (_, vsize, _); (_, locbytes, _)] ->
      let size =
        try
          let i = Cvalue.V.project_ival vsize in
          let i = Ival.project_int i in
          let ibytes = Integer.mul i (Bit_utils.sizeofchar ()) in
          Int_Base.inject ibytes
        with Cvalue.V.Not_based_on_null | Ival.Not_Singleton_Int ->
          Int_Base.top
      in
      let locbits = Locations.loc_bytes_to_loc_bits locbytes in
      let loc = Locations.make_loc locbits size in
      let state = add_binding ~exact:true state loc Cvalue.V.top_int in
      { Value_types.c_values = [ None, state] ;
        c_clobbered = Base.SetLattice.bottom;
        c_cacheable = Value_types.Cacheable;
        c_from = None;
      }
    | _ -> assert false

  let identify_builtin kf name =
    (* Advanced builtins which override a Cil function with a Caml one, but
       use the Cil one as backup if the Caml one fails. (None by default) *)
    try
      let name = Value_parameters.BuiltinsOverrides.find kf in
      (* This is an interesting C function. Mark it as called, otherwise
         it would get skipped, eg. from the Gui. *)
      Value_results.mark_kf_as_called kf;
      name, true
    with Not_found -> name, false

  (* Apply standard builtins with constant names, e.g. Frama_C_cos *)
  let apply_builtin override builtin state actuals =
    try Some (builtin state actuals)
    with
    | Builtins.Invalid_nb_of_args n ->
      Value_parameters.error ~current:true
        "Invalid number of arguments for builtin %s: %d expected, %d found"
        name n (List.length actuals);
      raise Db.Value.Aborted
    | Db.Value.Outside_builtin_possibilities ->
      if override then None
      else (
        Value_parameters.warning ~once:true ~current:true
          "Call to builtin %s failed, aborting." name;
        raise Db.Value.Aborted
      )

  (* Apply special builtins, such as Frama_C_show_each_foo *)
  let apply_special_builtins valuation name state actuals =
    if Ast_info.can_be_cea_function name then
      (* A few special functions that are not registered in the builtin table *)
      if Ast_info.is_cea_dump_function name then
        Some (Builtins.dump_state state actuals)
      else if Ast_info.is_cea_function name then
        Some (Builtins.dump_args name state actuals)
      else if Ast_info.is_cea_dump_file_function name then
        Some (Builtins.dump_state_file name state actuals)
      else
        None
    else if name = "__builtin_va_start" || name = "__builtin_va_end" then
      Some (va_start valuation state actuals)
    else if name = "__builtin_va_arg" then
      Some (va_arg valuation state actuals)
    else
      None

  (* Compute a call to a possible builtin [kf] in state [state]. [actuals] are
     the arguments of [kf], and have not been bound to its formals. Returns
     [None] if the call must be computed using the Cil function for [kf]. *)
  let compute_maybe_builtin call valuation state actuals rest =
    let actuals = actuals @ rest in
    let name = Kernel_function.get_name call.kf in
    try
      let name, override = identify_builtin call.kf name in
      let abstract_function = Builtins.find_builtin name in
      apply_builtin override abstract_function state actuals
    with Not_found ->
      apply_special_builtins valuation name state actuals


  (* ---------------------------------------------------------------------- *)
  (*                             Function Calls                             *)
  (* ---------------------------------------------------------------------- *)

  let warn_bottom kind lval =
    if with_alarms.CilE.imprecision_tracing.CilE.a_log then
      Value_parameters.result ~current:true ~once:true
        "completely invalid@ %s in evaluation of@ argument %a"
        kind Printer.pp_lval lval;
    (* TODO: bottom case should be avoided by the generic transfer function. *)
    raise InvalidCall

  let offsetmap_of_formal valuation state typ = function
    | Copy (lval, copy)
      when not (Eval_typ.is_bitfield (Cil.typeOfLval lval)) ->
      let record = match Valuation.find_loc valuation lval with
        | `Value record -> record
        | `Top -> assert false
      in
      let aux loc offsm_res =
        let open Locations in
        try
          let size = Int_Base.project loc.size in
          let _, copy = Cvalue.Model.copy_offsetmap loc.loc size state in
          Cvalue.V_Offsetmap.join_top_bottom copy offsm_res
        with
          Int_Base.Error_Top ->
          (* Subsumed by check_arg_size? *)
          Value_parameters.abort ~current:true
            "Function argument %a has unknown size. Aborting"
            Printer.pp_lval lval;
      in
      let o = Precise_locs.fold aux record.loc `Bottom in
      let offsm, _ =
        match o with
        | `Map offsm ->
          let typ_lv = Cil.typeOfLval lval in
          let offsm =
            if (match copy with Exact _ -> false | _ -> true) then
              match remove_indeterminateness typ_lv offsm with
              | `Bottom -> warn_bottom "value" lval
              | `Value r -> r
            else offsm
          in
          begin match offsetmap_contains_imprecision offsm with
            | Some v ->
              let loc = Precise_locs.imprecise_location record.loc in
              warn_imprecise_lval_read lval loc v
            | None -> ()
          end;
          offsm, state
        | `Bottom -> warn_bottom "location" lval
        | `Top ->
          Value_parameters.abort ~current:true ~once:true
            "completely imprecise state during evaluation. Aborting."
      in
      offsm
    | Copy (_lval, copy) ->
      let value = match copy with
        | Determinate v -> v.v
        | Exact _ -> assert false (* TODO *)
      in
      let offsm = offsetmap_of_v ~typ value in
      offsm
    | Assign value ->
      let offsm = offsetmap_of_v ~typ value in
      offsm

  let actualize_formals valuation state arguments rest =
    let compute expr assigned =
      let typ = Cil.typeOf expr in
      let offsm = offsetmap_of_formal valuation state typ assigned
      and value = unbottomize (Eval.value_assigned assigned) in
      offsm, value
    in
    let treat_one_formal arg (acc_state, acc_list) =
      let offsm, value = compute arg.concrete arg.avalue in
      Cvalue.Model.add_base (Base.of_varinfo arg.formal) offsm acc_state,
      (arg.concrete, value, offsm) :: acc_list
    in
    let treat_one_rest (exp, v) acc_list =
      let offsm, value = compute exp v in (exp, value, offsm) :: acc_list
    in
    let state, list = List.fold_right treat_one_formal arguments (state, []) in
    let rest = List.fold_right treat_one_rest rest [] in
    state, list, rest

  let call_action _stmt call valuation state =
    let state = update valuation state in
    let with_formals, list, rest =
      actualize_formals valuation state call.arguments call.rest
    in
    (* Store the initial state, but do not called mark_as_called. Uninteresting
       Value builtins are intentionally skipped *)
    Db.Value.merge_initial_state (Value_util.call_stack ()) with_formals;
    let stack_with_call = Value_util.call_stack () in
    Db.Value.Call_Value_Callbacks.apply (with_formals, stack_with_call);
    match compute_maybe_builtin call valuation state list rest with
    | None -> Compute (Continue with_formals, true), Base.SetLattice.bottom
    | Some res ->
      Db.Value.Call_Type_Value_Callbacks.apply
        (`Builtin res, with_formals, stack_with_call);
      let process_one_return acc (offsm, post_state) =
        if is_reachable post_state then
          let typ = Kernel_function.get_return_type call.kf in
          let returned_value = Extlib.opt_map (find_right_value typ) offsm in
          let result = { post_state ; summary = offsm; returned_value } in
          result :: acc
        else
          acc
      in
      let list = List.fold_left process_one_return [] res.Value_types.c_values in
      Result (Bottom.bot_of_list list, res.Value_types.c_cacheable),
      res.Value_types.c_clobbered


  let rec find_actual_varinfo e = match e.enode with
    | Lval (Var vi, NoOffset) ->
      if not vi.vaddrof && not (Cil.typeHasQualifier "volatile" vi.vtype)
      then Some vi else None
    | CastE (typ, e') -> begin
        match find_actual_varinfo e' with
        | None -> None
        | Some vi as ovi ->
          (* we can ignore casts, but only if they have no effect on the
             abstract value *)
          match Cil.unrollType typ, Cil.unrollType vi.vtype with
          | (TInt (ik, _) | TEnum ({ekind = ik}, _)),
            (TInt (ik', _) | TEnum ({ekind = ik'}, _)) ->
            if Cil.bytesSizeOfInt ik = Cil.bytesSizeOfInt ik' &&
               Cil.isSigned ik = Cil.isSigned ik'
            then ovi else None
          | TPtr _, TPtr _ -> ovi
          | TFloat (fk, _), TFloat (fk', _) ->
            if fk = fk' then ovi else None
          | _ -> None
      end
    | _ -> None

  (* Extract the content of \result from the state. *)
  let summarize kf _stmt ~returned state =
    let extract_return {ltyp; lloc} determinize state =
      let oret =
        try offsetmap_from_location lloc state
        with Int_Base.Error_Top ->
          Value_parameters.abort ~current:true
            "Function %a returns a value of unknown size. Aborting"
            Kernel_function.pretty kf
      in
      match oret with
      | `Bottom   -> assert false;
      | `Top      ->
        Value_parameters.abort ~current:true ~once:true
          "completely imprecise state during evaluation. Aborting."
      | `Map oret ->
        let offsetmap =
          if determinize
          then remove_indeterminateness ltyp oret
          else `Value oret
        in
        match offsetmap with
        | `Bottom -> None (* Completely indeterminate return *)
        | `Value (ret_val) -> Some ret_val
    in
    let return_offsm = match returned with
      | None -> None
      | Some (lv, copied) ->
        let determinize = match copied with
          | Exact _ -> false
          | Determinate _ -> true
        in
        extract_return lv determinize state
    in
    if Cvalue.Model.is_reachable state
    then `Value (return_offsm, state)
    else `Bottom

  (* This functions stores the result of call, represented by offsetmap
     [return], into [lv]. It is not trivial because we must handle the
     possibility of casts between the type of the result [rettyp] and the type
     of [lv]. With option [-no-collapse-call-cast], we only need the first part
     of the function. This function handles one possible location in [lv]. *)
  let assign_return_to_lv_one_loc rettype (lv, loc, lvtyp) return copied_value state =
    if not (Eval_typ.is_bitfield lvtyp) &&
       not (Eval_typ.need_cast lvtyp rettype)
    then
      (* Direct paste *)
      let size = Int_Base.project loc.Locations.size in
      snd (paste_offsetmap ~reducing:false
             ~from:return ~dst_loc:loc.Locations.loc ~size ~exact:true state)
    else
      (* Size mismatch. We use the returned value. *)
      let assigned_value = match copied_value with
        | Exact right_value -> right_value
        | Determinate value -> define value.v
      in
      write_abstract_value state (lv, loc, lvtyp) assigned_value

  (* Same as function above, but for multiple locations. *)
  let assign_return_to_lv rettype {lval; ltyp; lloc} return copied_value state =
    let aux loc acc_state =
      let state =
        assign_return_to_lv_one_loc rettype (lval, loc, ltyp) return copied_value state
      in
      join acc_state state
    in
    Precise_locs.fold aux lloc bottom

  (* This function unbinds [formal] in [state], which is assumed to be the
     post-state of a call. When possible, it also reduces the corresponding
     actual [actual] to the value of [formal] in [state]. This is possible
     only if [formal] has never been written during the call, an information
     supplied by [written]. *)
  let remove_formal formal actual written state =
    let base = Base.of_varinfo formal in
    let reduced_state =
      match written, find_actual_varinfo actual with
      | false, Some vi -> begin
          (* [vi] has not been written during the call. Replace it by [b].
             Indeed, either [b] is equal to [vi], or it has been reduced during
             the call (in which case it is useful to reduce [vi]). *)
          try
            match find_base base state with
            | `Bottom | `Top -> state
            | `Map offsm -> add_base (Base.of_varinfo vi) offsm state
          with Not_found ->
            (* formals are not bound for functions without a body... *)
            state
        end
      | false, None (* TODO: do a backward propagation *) | true, _ -> state
    in
    Cvalue.Model.remove_base base reduced_state

  let resolve_call _stmt call ~assigned valuation ~pre:_ ~post =
    let return, state = post in
    let state = update valuation state in
    let written_formals = Value_util.written_formals call.kf in
    let handle_argument acc argument =
      let formal = argument.formal in
      let written = Cil_datatype.Varinfo.Set.mem formal written_formals in
      remove_formal formal argument.concrete written acc
    in
    let state = List.fold_left handle_argument state call.arguments in
    let state = match assigned with
      | None -> `Value state
      | Some (left_loc, copied_value) ->
        let return =
          ( match return with
              None ->
              Format.printf "Function %a@." Kernel_function.pretty call.kf;
              Value_parameters.abort ~current:true ~once:true
                "Return value expected but none present. \
                 Did you misuse a builtin?"
            | Some return -> return )
        in
        let rettype = Kernel_function.get_return_type call.kf in
        let determinize = match copied_value with
          | Exact _           -> false
          | Determinate value -> (not value.initialized) || value.escaping
        in
        let return =
          if determinize
          then remove_indeterminateness left_loc.ltyp return
          else `Value return
        in
        return >>-: fun return ->
        assign_return_to_lv rettype left_loc return copied_value state
    in
    state >>- fun state ->
    if is_reachable state
    then `Value state
    else `Bottom

  let default_call _stmt _call _t = assert false

end
