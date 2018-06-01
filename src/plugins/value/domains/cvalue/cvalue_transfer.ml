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

let eval_precond =
  ref (fun _kf _stmt state -> state)

module Transfer
    (Valuation: Abstract_domain.Valuation with type value = value
                                           and type origin = value option
                                           and type loc = location)
= struct

  type state = Cvalue.Model.t

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

  let is_smaller_value typ v1 v2 =
    let size = Integer.of_int (Cil.bitsSizeOf typ) in
    let card1 = Cvalue.V.cardinal_estimate v1 ~size
    and card2 = Cvalue.V.cardinal_estimate v2 ~size in
    Integer.lt card1 card2

  (* Update the state according to a Valuation. *)
  let update valuation t =
    let process exp record t =
      match exp.enode with
      | Lval lv ->
        if record.reductness = Reduced
        then
          let {v; initialized; escaping} = record.value in
          let v = unbottomize v in
          let v =
            (* The origin contains the value already stored in the state, when
               its type is incompatible with the lvalue [lv]. The precision of
               this previous value and [v] are then incomparable (none is
               included in the other). We use some notion of cardinality of
               abstract values to choose the best value to keep. *)
            match record.origin with
            | Some (Some previous_v) ->
              let typ = Cil.typeOfLval lv in
              if is_smaller_value typ v previous_v then v else previous_v
            | _ -> v
          in
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

  let write_abstract_value state (lval, loc, typ) assigned_value =
    let {v; initialized; escaping} = assigned_value in
    let value = unbottomize v in
    Warn.warn_right_exp_imprecision lval loc value;
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
      add_indeterminate_binding ~exact state loc value

  exception Do_assign_imprecise_copy

  let copy_one_loc state left_lv right_lv =
    let left_lval, left_loc, left_typ = left_lv
    and right_lval, right_loc, right_typ = right_lv in
    (* Warn if right_loc is imprecise *)
    Warn.warn_imprecise_lval_read right_lval right_loc Cvalue.V.bottom;
    (* top size is tested before this function is called, in which case
       the imprecise copy mode is used. *)
    let size = Int_Base.project right_loc.Locations.size in
    let offsetmap = copy_offsetmap right_loc.Locations.loc size state in
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
      if not (Eval_typ.offsetmap_matches_type left_typ offsetmap) then
        raise Do_assign_imprecise_copy;
      let () =
        match offsetmap_contains_imprecision offsetmap with
        | Some v -> Warn.warn_right_exp_imprecision left_lval left_loc v
        | _ -> ()
      in
      `Value
        (paste_offsetmap ~exact:true
           ~from:offsetmap ~dst_loc:left_loc.Locations.loc ~size state)

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

  let apply_abstract_builtin builtin name (state: Cvalue.Model.t) actuals =
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

  (* Compute a call to a possible builtin [kf] in state [state]. [actuals] are
     the arguments of [kf], and have not been bound to its formals. Returns
     [None] if the call must be computed using the Cil function for [kf]. *)
  let compute_maybe_builtin call stmt state actuals rest =
    let actuals = actuals @ rest in
    let name = Kernel_function.get_name call.kf in
    match Builtins.find_builtin_override call.kf with
    | Some abstract_function ->
      (* This is an interesting C function. Mark it as called, otherwise
         it would get skipped, eg. from the Gui. *)
      Value_results.mark_kf_as_called call.kf;
      let state = !eval_precond call.kf stmt state in
      (* If the preconditions lead to bottom, do *not* call the Caml builtin.
         Most assume that their arguments are not bottom. *)
      if Cvalue.Model.(equal state bottom) then
        let bot_res = {
          Value_types.c_values = [];
          c_clobbered = Base.SetLattice.empty;
          c_cacheable = Value_types.Cacheable;
          c_from = None;
        }
        in
        Some bot_res
      else
        apply_abstract_builtin abstract_function name state actuals
    | None -> None


  (* ---------------------------------------------------------------------- *)
  (*                             Function Calls                             *)
  (* ---------------------------------------------------------------------- *)

  let warn_if_imprecise lval loc offsm =
    match offsetmap_contains_imprecision offsm with
    | Some v ->
      let loc = Precise_locs.imprecise_location loc in
      Warn.warn_imprecise_lval_read lval loc v
    | None -> ()

  let offsetmap_of_lval valuation state lval =
    let record = match Valuation.find_loc valuation lval with
      | `Value record -> record
      | `Top -> assert false
    in
    let offsm =
      try Bottom.non_bottom (Eval_op.offsetmap_of_loc record.loc state)
      with Abstract_interp.Error_Top ->
        (* Subsumed by check_arg_size? *)
        Value_parameters.abort ~current:true
          "Function argument %a has unknown size. Aborting"
          Printer.pp_lval lval;
    in
    warn_if_imprecise lval record.loc offsm;
    offsm

  let offsetmap_of_formal valuation state typ = function
    | Copy (lval, _value) -> offsetmap_of_lval valuation state lval
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

  let start_call stmt call valuation state =
    let state = update valuation state in
    let with_formals, list, rest =
      actualize_formals valuation state call.arguments call.rest
    in
    let stack_with_call = Value_util.call_stack () in
    Db.Value.Call_Value_Callbacks.apply (with_formals, stack_with_call);
    match compute_maybe_builtin call stmt with_formals list rest with
    | None -> Compute with_formals, Base.SetLattice.bottom
    | Some res ->
      (* Store the initial state, but do not called mark_as_called. Uninteresting
         Value builtins are intentionally skipped *)
      Db.Value.merge_initial_state (Value_util.call_stack ()) with_formals;
      Db.Value.Call_Type_Value_Callbacks.apply
        (`Builtin res, with_formals, stack_with_call);
      let process_one_return acc (ret, post_state) =
        if is_reachable post_state then
          match ret, call.return with
          | Some offsm_ret, Some vi_ret ->
            let b_ret = Base.of_varinfo vi_ret in
            let with_ret = Cvalue.Model.add_base b_ret offsm_ret post_state in
            with_ret :: acc
          | _, _ -> post_state :: acc
        else
          acc
      in
      let list = List.fold_left process_one_return [] res.Value_types.c_values in
      Result (Bottom.bot_of_list list, res.Value_types.c_cacheable),
      res.Value_types.c_clobbered

  let finalize_call stmt call ~pre:_ ~post:state =
    (* Deallocate memory allocated via alloca().
       To minimize computations, only do it for function definitions. *)
    let state' =
      if Kernel_function.is_definition call.kf then
        let stack = (call.kf, Kstmt stmt) :: (Value_util.call_stack ()) in
        Builtins_malloc.free_automatic_bases stack state
      else state
    in
    `Value state'

  let approximate_call _stmt _call _t = assert false

  let show_expr valuation state fmt expr =
    match expr.enode with
    | Lval lval ->
      let offsm = offsetmap_of_lval valuation state lval in
      let typ = Cil.typeOf expr in
      Eval_op.pretty_offsetmap typ fmt offsm
    | _ -> Format.fprintf fmt "%s" (Unicode.top_string ())
end
