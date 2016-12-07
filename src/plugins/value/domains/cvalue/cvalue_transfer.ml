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
  let v = Cvalue.V.anisotropic_cast ~size v in
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

let offsetmap_from_location location state =
  let aux loc offsm_res =
    let size = Int_Base.project loc.Locations.size in
    let _, copy = Cvalue.Model.copy_offsetmap loc.Locations.loc size state in
    Bottom.join Cvalue.V_Offsetmap.join copy offsm_res
  in
  Precise_locs.fold aux location `Bottom


module Transfer
    (Valuation: Abstract_domain.Valuation with type value = value
                                           and type origin = bool
                                           and type loc = location)
= struct

  type state = Cvalue.Model.t
  type return = Cvalue.V_Offsetmap.t option

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
      if Cil.typeHasQualifier "volatile" typ
      then Cvalue_forward.make_volatile value
      else value
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

  let copy_one_loc state left_lv right_lv =
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
    | `Value offsm ->
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
      let () =
        match offsetmap_contains_imprecision offsetmap with
        | Some v -> warn_right_imprecision left_lval left_loc v
        | _ -> ()
      in
      `Value
        (snd (paste_offsetmap ~reducing:false ~exact:true
                ~from:offsetmap ~dst_loc:left_loc.Locations.loc ~size state))

  let make_determinate value =
    { v = `Value value; initialized = true; escaping = false }

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
    if not (Int_Base.equal left_size right_size) || Int_Base.is_top right_size
    then
      fun loc -> write_abstract_value state (lval, loc, typ) copied_value
    else
      fun loc ->
        try
          let process right_loc acc =
            let left_lv = lval, loc, typ
            and right_lv = right_lval, right_loc, right_typ in
            match copy_one_loc state left_lv right_lv with
            | `Bottom -> acc
            | `Value state -> join acc state
          in
          Precise_locs.fold process right_loc bottom
        with
          Do_assign_imprecise_copy ->
          write_abstract_value state (lval, loc, typ) copied_value

  let assign _stmt { lval; ltyp; lloc } _expr assigned valuation state =
    let state = update valuation state in
    let assign_one_loc =
      match assigned with
      | Assign value ->
        let assigned_value = make_determinate value in
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

  let apply_abstract_builtin builtin state actuals =
    try Some (builtin state actuals)
    with
    | Builtins.Invalid_nb_of_args n ->
      Value_parameters.error ~current:true
        "Invalid number of arguments for builtin %s: %d expected, %d found"
        name n (List.length actuals);
      raise Db.Value.Aborted
    | Db.Value.Outside_builtin_possibilities ->
      Value_parameters.warning ~once:true ~current:true
        "Call to builtin %s failed, aborting." name;
      raise Db.Value.Aborted

  (* Apply special builtins, such as Frama_C_show_each_foo *)
  let apply_special_builtins valuation name state actuals =
    if Ast_info.can_be_cea_function name then
      (* A few special functions that are not registered in the builtin table *)
      if Ast_info.is_cea_dump_function name then
        Some (Builtins_misc.dump_state state actuals)
      else if Ast_info.is_cea_function name then
        Some (Builtins_misc.dump_args name state actuals)
      else if Ast_info.is_cea_dump_file_function name then
        Some (Builtins_misc.dump_state_file name state actuals)
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
    match Builtins.find_builtin_override call.kf with
    | Some abstract_function ->
      (* This is an interesting C function. Mark it as called, otherwise
         it would get skipped, eg. from the Gui. *)
      Value_results.mark_kf_as_called call.kf;
      apply_abstract_builtin abstract_function state actuals
    | None ->
      apply_special_builtins valuation name state actuals


  (* ---------------------------------------------------------------------- *)
  (*                             Function Calls                             *)
  (* ---------------------------------------------------------------------- *)

  let offsetmap_of_formal valuation state typ = function
    | Copy (lval, _value) ->
      let record = match Valuation.find_loc valuation lval with
        | `Value record -> record
        | `Top -> assert false
      in
      let aux loc offsm_res =
        let open Locations in
        try
          let size = Int_Base.project loc.size in
          let _, copy = Cvalue.Model.copy_offsetmap loc.loc size state in
          Bottom.join Cvalue.V_Offsetmap.join copy offsm_res
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
        | `Value offsm ->
          begin match offsetmap_contains_imprecision offsm with
            | Some v ->
              let loc = Precise_locs.imprecise_location record.loc in
              warn_imprecise_lval_read lval loc v
            | None -> ()
          end;
          offsm, state
        | `Bottom -> raise InvalidCall
      in
      offsm
    | Assign value -> offsetmap_of_v ~typ value

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

  let start_call _stmt call valuation state =
    let state = update valuation state in
    let with_formals, list, rest =
      actualize_formals valuation state call.arguments call.rest
    in
    let stack_with_call = Value_util.call_stack () in
    Db.Value.Call_Value_Callbacks.apply (with_formals, stack_with_call);
    match compute_maybe_builtin call valuation state list rest with
    | None -> Compute (Continue with_formals, true), Base.SetLattice.bottom
    | Some res ->
      (* Store the initial state, but do not called mark_as_called. Uninteresting
         Value builtins are intentionally skipped *)
      Db.Value.merge_initial_state (Value_util.call_stack ()) with_formals;
      Db.Value.Call_Type_Value_Callbacks.apply
        (`Builtin res, with_formals, stack_with_call);
      let process_one_return acc (offsm, post_state) =
        if is_reachable post_state then
          let typ = Kernel_function.get_return_type call.kf in
          let return = match offsm with
            | None -> None
            | Some offsm -> Some (find_right_value typ offsm, Some offsm)
          in
          let result = { post_state; return; } in
          result :: acc
        else
          acc
      in
      let list = List.fold_left process_one_return [] res.Value_types.c_values in
      Result (Bottom.bot_of_list list, res.Value_types.c_cacheable),
      res.Value_types.c_clobbered

  (* Extract the content of \result from the state. *)
  let make_return kf _stmt assign valuation state =
    try
      match assign with
      | Assign v ->
        let typ = Kernel_function.get_return_type kf in
        let size = Integer.of_int (Cil.bitsSizeOf typ) in
        let v = Cvalue.V.anisotropic_cast ~size v in
        let offsm = Eval_op.offsetmap_of_v ~typ v  in
        Some offsm
      | Copy (lval, _copied) ->
        match Valuation.find_loc valuation lval with
        | `Top -> assert false
        | `Value record ->
          let oret = offsetmap_from_location record.loc state in
          match oret with
          | `Bottom     -> assert false;
          | `Value oret -> Some oret
    with Int_Base.Error_Top | Cil.SizeOfError _ ->
      Value_parameters.abort ~current:true
        "Function %a returns a value of unknown size. Aborting"
        Kernel_function.pretty kf


  (* This functions stores the result of call, represented by offsetmap
     [return], into [lv]. It is not trivial because we must handle the
     possibility of casts between the type of the result [rettyp] and the type
     of [lv]. With option [-no-collapse-call-cast], we only need the first part
     of the function. This function handles one possible location in [lv]. *)
  let assign_return_to_lv_one_loc (lv, loc, lvtyp) return value state =
    match value, return with
    | Assign _, Some offsm ->
      let v = Extlib.the (Cvalue.V_Offsetmap.single_interval_value offsm) in
      let v = Cvalue.V_Or_Uninitialized.get_v v in
      (* TODO: perform a conversion here would make sense when the type
         changes between the callee and the caller *)
      (* let v = Cvalue_forward.unsafe_reinterpret lvtyp v in *)
      let v = make_determinate v in
      write_abstract_value state (lv, loc, lvtyp) v
    | Copy _, Some offsm ->
      (* Direct paste *)
      let size = Int_Base.project loc.Locations.size in
      snd (paste_offsetmap ~reducing:false
             ~from:offsm ~dst_loc:loc.Locations.loc ~size ~exact:true state)
    | _ , None -> assert false

  (* Same as function above, but for multiple locations. *)
  let assign_return _stmt {lval; ltyp; lloc} _kf return value valuation state =
    let state = update valuation state in
    let aux loc acc_state =
      let state =
        assign_return_to_lv_one_loc (lval, loc, ltyp) return value state
      in
      join acc_state state
    in
    let state = Precise_locs.fold aux lloc bottom in
    if is_reachable state
    then `Value state
    else `Bottom

  let finalize_call _stmt _call ~pre:_ ~post:state = `Value state

  let default_call _stmt _call _t = assert false

  let enter_loop _ state = state
  let incr_loop_counter _ state = state
  let leave_loop _ state = state
end
