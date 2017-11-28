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

open Eval

let counter = ref 0

let product_category = Value_parameters.register_category "domain_product"

module Make
    (Value: Abstract_value.S)
    (Left:  Abstract_domain.Internal with type value = Value.t)
    (Right: Abstract_domain.Internal with type value = Left.value
                                      and type location = Left.location)
= struct

  type value = Left.value
  type location = Left.location

  type origin = {
    left:  reductness * Left.origin;
    right: reductness * Right.origin;
  }

  let () = incr counter
  let name = Left.name ^ "*" ^ Right.name ^
             "(" ^ string_of_int !counter ^ ")"

  include Datatype.Pair_with_collections
      (Left)
      (Right)
      (struct let module_name = name end)
  type state = t

  let structure = Abstract_domain.Node (Left.structure, Right.structure)

  let log_category = product_category

  let top = Left.top, Right.top
  let is_included (left1, right1) (left2, right2) =
    Left.is_included left1 left2 && Right.is_included right1 right2
  let join (left1, right1) (left2, right2) =
    Left.join left1 left2, Right.join right1 right2
  let widen kf stmt (left1, right1) (left2, right2) =
    Left.widen kf stmt left1 left2, Right.widen kf stmt right1 right2

  let narrow (left1, right1) (left2, right2) =
    Left.narrow left1 left2 >>- fun left ->
    Right.narrow right1 right2 >>-: fun right ->
    (left, right)


  let merge (eval1, alarms1) (eval2, alarms2) =
    match Alarmset.inter alarms1 alarms2 with
    | `Inconsistent ->
      Value_parameters.abort ~current:true ~once:true
        "Inconsistent status of alarms: unsound states."
    | `Value alarms ->
      let value =
        eval1 >>- fun (v1, o1) ->
        eval2 >>- fun (v2, o2) ->
        Value.narrow v1 v2 >>-: fun value ->
        let left =
          if Value.equal value v1 then Unreduced
          else if Value.equal v1 Value.top then Created else Reduced
        and right =
          if Value.equal value v2 then Unreduced
          else if Value.equal v2 Value.top then Created else Reduced
        in
        let origin = {left = left, o1; right = right, o2} in
        value, origin
      in
      value, alarms

  let extract_expr oracle (left, right) expr =
    merge
      (Left.extract_expr oracle left expr)
      (Right.extract_expr oracle right expr)

  let extract_lval oracle (left, right) lval typ location =
    merge
      (Left.extract_lval oracle left lval typ location)
      (Right.extract_lval oracle right lval typ location)

  let backward_location (left, right) lval typ loc value =
    (* TODO: Loc.narrow *)
    Left.backward_location left lval typ loc value >>- fun (loc, value1) ->
    Right.backward_location right lval typ loc value >>- fun (loc, value2) ->
    Value.narrow value1 value2 >>-: fun value ->
    loc, value

  let reduce_further (left, right) expr value =
    List.append
      (Left.reduce_further left expr value)
      (Right.reduce_further right expr value)

  (* TODO: this function does a cartesian product, which is pretty terrible. *)
  let merge_results _kf left_list right_list =
    let list =
      List.fold_left
        (fun acc left ->
           List.fold_left
             (fun acc right -> (left, right) :: acc)
             acc right_list)
        [] left_list
    in
    List.rev list


  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type origin = origin
                                             and type loc = location)
  = struct

    module type Lift = sig
      type o
      val side : origin -> reductness * o
    end

    module Lift_Valuation (Lift: Lift) = struct
      type t = Valuation.t
      type value = Value.t
      type origin = Lift.o
      type loc = Valuation.loc

      let lift_record record =
        let origin = Extlib.opt_map Lift.side record.origin in
        let reductness =
          match record.reductness, origin with
          | Unreduced, Some (reduced, _) -> reduced
          | Unreduced, None -> Unreduced (* This case should not happen. *)
          | Reduced, Some (Created, _) -> Created
          | _ as x, _ -> x
        in
        let origin = Extlib.opt_map snd origin in
        { record with origin; reductness }

      let find valuation expr = match Valuation.find valuation expr with
        | `Value record -> `Value (lift_record record)
        | `Top -> `Top

      let fold f valuation acc =
        Valuation.fold
          (fun exp record acc -> f exp (lift_record record) acc)
          valuation acc

      let find_loc = Valuation.find_loc
    end

    module Left_Valuation =
      Lift_Valuation (struct type o = Left.origin let side o = o.left end)
    module Right_Valuation =
      Lift_Valuation (struct type o = Right.origin let side o = o.right end)

    module Left_Transfer = Left.Transfer (Left_Valuation)
    module Right_Transfer = Right.Transfer (Right_Valuation)

    let update valuation (left, right) =
      Left_Transfer.update valuation left,
      Right_Transfer.update valuation right

    let assign stmt lv expr value valuation (left, right) =
      Left_Transfer.assign stmt lv expr value valuation left >>- fun left ->
      Right_Transfer.assign stmt lv expr value valuation right >>-: fun right ->
      left, right

    let assume stmt expr positive valuation (left, right) =
      Left_Transfer.assume stmt expr positive valuation left >>- fun left ->
      Right_Transfer.assume stmt expr positive valuation right >>-: fun right ->
      left, right

    let finalize_call stmt call ~pre ~post =
      let pre_left, pre_right = pre
      and left_state, right_state = post in
      Left_Transfer.finalize_call stmt call ~pre:pre_left ~post:left_state
      >>- fun left ->
      Right_Transfer.finalize_call stmt call ~pre:pre_right ~post:right_state
      >>-: fun right ->
      left, right

    let approximate_call stmt call (left, right) =
      Left_Transfer.approximate_call stmt call left >>- fun left_result ->
      Right_Transfer.approximate_call stmt call right >>-: fun right_result ->
      merge_results call.kf left_result right_result

    let start_call stmt call valuation (left, right) =
      let left_action = Left_Transfer.start_call stmt call valuation left
      and right_action = Right_Transfer.start_call stmt call valuation right in
      match left_action, right_action with
      | Compute left, Compute right -> Compute (left, right)
      | Result (left_result, c1), Result (right_result, _c2) ->
        let result =
          left_result >>- fun left_result ->
          right_result >>-: fun right_result ->
          merge_results call.kf left_result right_result
        in
        Result (result, c1) (* TODO: c1 *)
      | Result (left_result, c1), _ ->
        let result =
          Right_Transfer.approximate_call stmt call right >>- fun right_result ->
          left_result >>-: fun left_result ->
          merge_results call.kf left_result right_result
        in
        Result (result, c1)
      | _, Result (right_result, c2) ->
        let result =
          Left_Transfer.approximate_call stmt call left >>- fun left_result ->
          right_result >>-: fun right_result ->
          merge_results call.kf left_result right_result
        in
        Result (result, c2)

    let show_expr =
      let (|-) f g = fun fmt exp -> f fmt exp; g fmt exp in
      let show_expr_one_side category name show_expr = fun fmt exp ->
        if Value_parameters.is_debug_key_enabled category
        then Format.fprintf fmt "@,@]@[<v># %s: @[<hov>%a@]" name show_expr exp
      in
      let right_log = Right.log_category
      and left_log = Left.log_category in
      match left_log = product_category,
            right_log = product_category with
      | true, true ->
        (fun valuation (left, right) ->
           Left_Transfer.show_expr valuation left |-
           Right_Transfer.show_expr valuation right)
      | true, false ->
        (fun valuation (left, right) ->
           Left_Transfer.show_expr valuation left |-
           show_expr_one_side right_log Right.name
             (Right_Transfer.show_expr valuation right))
      | false, true ->
        (fun valuation (left, right) ->
           show_expr_one_side left_log Left.name
             (Left_Transfer.show_expr valuation left) |-
           Right_Transfer.show_expr valuation right)
      | false, false ->
        (fun valuation (left, right) ->
           show_expr_one_side left_log Left.name
             (Left_Transfer.show_expr valuation left) |-
           show_expr_one_side right_log Right.name
             (Right_Transfer.show_expr valuation right))
  end

  let pretty =
    let print_one_side fmt category name dump state =
      if Value_parameters.is_debug_key_enabled category
      then Format.fprintf fmt "# %s:@ @[<hv>%a@]@ " name dump state
    in
    let right_log = Right.log_category
    and left_log = Left.log_category in
    match left_log = product_category,
          right_log = product_category with
    | true, true ->
      (fun fmt (left, right) ->
         Left.pretty fmt left;
         Right.pretty fmt right)
    | true, false ->
      (fun fmt (left, right) ->
         Left.pretty fmt left;
         print_one_side fmt right_log Right.name Right.pretty right)
    | false, true ->
      (fun fmt (left, right) ->
         print_one_side fmt left_log Left.name Left.pretty left;
         Right.pretty fmt right)
    | false, false ->
      (fun fmt (left, right) ->
         print_one_side fmt left_log Left.name Left.pretty left;
         print_one_side fmt right_log Right.name Right.pretty right)


  let logic_assign assign location ~pre:(left_pre, right_pre) (left, right) =
    Left.logic_assign assign location ~pre:left_pre left,
    Right.logic_assign assign location ~pre:right_pre right

  let lift_logic_env f logic_env =
    Abstract_domain.{ states = (fun label -> f (logic_env.states label));
                      result = logic_env.result; }

  let split_logic_env logic_env =
    lift_logic_env fst logic_env, lift_logic_env snd logic_env

  let evaluate_predicate logic_environment (left, right) pred =
    let left_env, right_env = split_logic_env logic_environment in
    let left_status = Left.evaluate_predicate left_env left pred
    and right_status = Right.evaluate_predicate right_env right pred in
    match Alarmset.Status.inter left_status right_status with
    | `Inconsistent ->
      Value_parameters.abort ~current:true ~once:true
        "Inconsistent status of alarms: unsound states."
    | `Value status -> status

  let reduce_by_predicate logic_environment (left, right) pred positive =
    let left_env, right_env = split_logic_env logic_environment in
    Left.reduce_by_predicate left_env left pred positive >>- fun left ->
    Right.reduce_by_predicate right_env right pred positive >>-: fun right ->
    left, right

  let enter_scope kf vars (left, right) =
    Left.enter_scope kf vars left, Right.enter_scope kf vars right
  let leave_scope kf vars (left, right) =
    Left.leave_scope kf vars left, Right.leave_scope kf vars right

  let enter_loop stmt (left, right) =
    Left.enter_loop stmt left, Right.enter_loop stmt right
  let incr_loop_counter stmt (left, right) =
    Left.incr_loop_counter stmt left, Right.incr_loop_counter stmt right
  let leave_loop stmt (left, right) =
    Left.leave_loop stmt left, Right.leave_loop stmt right

  let empty () = Left.empty (), Right.empty ()
  let introduce_globals vars (left, right) =
    Left.introduce_globals vars left, Right.introduce_globals vars right
  let initialize_variable lval loc ~initialized init_value (left, right) =
    Left.initialize_variable lval loc ~initialized init_value left,
    Right.initialize_variable lval loc ~initialized init_value right
  let initialize_variable_using_type kind varinfo (left, right) =
    Left.initialize_variable_using_type kind varinfo left,
    Right.initialize_variable_using_type kind varinfo right


  let filter_by_bases bases (left, right) =
    Left.filter_by_bases bases left, Right.filter_by_bases bases right
  let reuse ~current_input ~previous_output =
    Left.reuse
      ~current_input:(fst current_input) ~previous_output:(fst previous_output),
    Right.reuse
      ~current_input:(snd current_input) ~previous_output:(snd previous_output)


  let merge_tbl left_tbl right_tbl =
    let open Value_types in
    let tbl = Callstack.Hashtbl.create 7 in
    let merge callstack left =
      try
        let right = Callstack.Hashtbl.find right_tbl callstack in
        Callstack.Hashtbl.replace tbl callstack (left, right)
      with
        Not_found -> ()
    in
    Callstack.Hashtbl.iter merge left_tbl;
    if Callstack.Hashtbl.length tbl > 0 then `Value tbl else `Bottom

  let lift_tbl f tbl =
    let open Value_types in
    let new_tbl = Callstack.Hashtbl.create 7 in
    let lift cs t = Callstack.Hashtbl.replace new_tbl cs (f t) in
    Callstack.Hashtbl.iter lift tbl;
    `Value new_tbl

  let merge_callstack_tbl left right =
    match left, right with
    | `Top, `Top -> `Top
    | `Value left, `Value right -> merge_tbl left right
    | `Top, `Value right -> lift_tbl (fun t -> Left.top, t) right
    | `Value left, `Top -> lift_tbl (fun t -> t, Right.top) left
    | `Bottom, _ | _, `Bottom -> `Bottom

  module Store = struct
    let register_global_state state =
      Left.Store.register_global_state (state >>-: fst);
      Right.Store.register_global_state (state >>-: snd)
    let register_initial_state callstack (left, right) =
      Left.Store.register_initial_state callstack left;
      Right.Store.register_initial_state callstack right
    let register_state_before_stmt callstack stmt (left, right) =
      Left.Store.register_state_before_stmt callstack stmt left;
      Right.Store.register_state_before_stmt callstack stmt right
    let register_state_after_stmt callstack stmt (left, right) =
      Left.Store.register_state_after_stmt callstack stmt left;
      Right.Store.register_state_after_stmt callstack stmt right

    let get_global_state () =
      Left.Store.get_global_state () >>- fun left ->
      Right.Store.get_global_state () >>-: fun right ->
      left, right
    let get_initial_state kf =
      Left.Store.get_initial_state kf >>- fun left ->
      Right.Store.get_initial_state kf >>-: fun right ->
      left, right
    let get_initial_state_by_callstack kf =
      let left_tbl = Left.Store.get_initial_state_by_callstack kf
      and right_tbl = Right.Store.get_initial_state_by_callstack kf in
      merge_callstack_tbl left_tbl right_tbl

    let get_stmt_state stmt =
      Left.Store.get_stmt_state stmt >>- fun left ->
      Right.Store.get_stmt_state stmt >>-: fun right ->
      left, right
    let get_stmt_state_by_callstack ~after stmt =
      let left_tbl = Left.Store.get_stmt_state_by_callstack ~after stmt
      and right_tbl = Right.Store.get_stmt_state_by_callstack ~after stmt in
      merge_callstack_tbl left_tbl right_tbl

  end

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
