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

  let pretty fmt (left, right) =
    Format.fprintf fmt
      "@[<v>(@[%a@]@ ,@  @[%a@])@]" Left.pretty left Right.pretty right

  let structure = Abstract_domain.Node (Left.structure, Right.structure)

  let top = Left.top, Right.top
  let is_included (left1, right1) (left2, right2) =
    Left.is_included left1 left2 && Right.is_included right1 right2
  let join (left1, right1) (left2, right2) =
    Left.join left1 left2, Right.join right1 right2
  let join_and_is_included (left1, right1) (left2, right2) =
    let left, b1 = Left.join_and_is_included left1 left2
    and right, b2 = Right.join_and_is_included right1 right2 in
    (left, right), b1 && b2
  let widen kf stmt (left1, right1) (left2, right2) =
    Left.widen kf stmt left1 left2, Right.widen kf stmt right1 right2


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

  let merge_init left right =
    match left, right with
    | Default, Default -> Default
    | Continue left, Continue right -> Continue (left, right)
    | Default, Continue right       -> Continue (Left.top, right)
    | Continue left, Default        -> Continue (left, Right.top)
    | _, _ -> assert false (* TODO! *)

  (* TODO: this function does a cartesian product, which is pretty terrible. *)
  let merge_results _kf left_list right_list =
    List.fold_left
      (fun acc left ->
         List.fold_left
           (fun acc right -> (left, right) :: acc)
           acc right_list)
      [] left_list


  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type origin = origin
                                             and type loc = location)
  = struct

    type state = t
    type value = Value.t
    type location = Left.location
    type valuation = Valuation.t

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
      | Compute (left_init, b), Compute (right_init, b') ->
        Compute (merge_init left_init right_init, b && b')
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

  end


  (* TODO *)
  let compute_using_specification kinstr call spec (left, right) =
    Left.compute_using_specification kinstr call spec left >>- fun left ->
    Right.compute_using_specification kinstr call spec right >>-: fun right ->
    merge_results call.kf left right


  type eval_env = Left.eval_env * Right.eval_env

  let env_current_state (left, right) =
    Left.env_current_state left >>- fun left_env ->
    Right.env_current_state right >>-: fun right_env ->
    left_env, right_env

  let env_annot ~pre ~here () =
    Left.env_annot ~pre:(fst pre) ~here:(fst here) (),
    Right.env_annot ~pre:(snd pre) ~here:(snd here) ()

  let env_pre_f ~pre () =
    Left.env_pre_f ~pre:(fst pre) (), Right.env_pre_f ~pre:(snd pre) ()

  let env_post_f ~pre ~post ~result () =
    Left.env_post_f ~pre:(fst pre) ~post:(fst post) ~result (),
    Right.env_post_f ~pre:(snd pre) ~post:(snd post) ~result ()

  let eval_predicate (left, right) pred =
    let status =
      Alarmset.Status.inter
        (Left.eval_predicate left pred) (Right.eval_predicate right pred)
    in
    match status with
    | `Inconsistent ->
      Value_parameters.abort ~current:true ~once:true
        "Inconsistent status of alarms: unsound states."
    | `Value status -> status

  let reduce_by_predicate (left, right) positive pred =
    Left.reduce_by_predicate left positive pred,
    Right.reduce_by_predicate right positive pred

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
  let initialize_var (left, right) lval loc value =
    Left.initialize_var left lval loc value,
    Right.initialize_var right lval loc value
  let initialize_var_using_type (left, right) varinfo =
    Left.initialize_var_using_type left varinfo,
    Right.initialize_var_using_type right varinfo
  let global_state () =
    match Left.global_state (), Right.global_state () with
    | None, None -> None
    | None, Some s -> Some (s >>-: fun s -> Left.top, s)
    | Some s, None -> Some (s >>-: fun s -> s, Right.top)
    | Some l, Some r -> Some (l >>- fun l -> r >>-: fun r -> l, r)


  let filter_by_bases bases (left, right) =
    Left.filter_by_bases bases left, Right.filter_by_bases bases right
  let reuse ~current_input ~previous_output =
    Left.reuse
      ~current_input:(fst current_input) ~previous_output:(fst previous_output),
    Right.reuse
      ~current_input:(snd current_input) ~previous_output:(snd previous_output)


  let merge_callstack_tbl left_tbl right_tbl =
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
    Some tbl

  module Store = struct
    let register_initial_state callstack (left, right) =
      Left.Store.register_initial_state callstack left;
      Right.Store.register_initial_state callstack right
    let register_state_before_stmt callstack stmt (left, right) =
      Left.Store.register_state_before_stmt callstack stmt left;
      Right.Store.register_state_before_stmt callstack stmt right
    let register_state_after_stmt callstack stmt (left, right) =
      Left.Store.register_state_after_stmt callstack stmt left;
      Right.Store.register_state_after_stmt callstack stmt right

    let get_initial_state kf =
      Left.Store.get_initial_state kf >>- fun left ->
      Right.Store.get_initial_state kf >>-: fun right ->
      left, right
    let get_initial_state_by_callstack kf =
      let left_tbl = Left.Store.get_initial_state_by_callstack kf
      and right_tbl = Right.Store.get_initial_state_by_callstack kf in
      match left_tbl, right_tbl with
      | Some left, Some right -> merge_callstack_tbl left right
      | _, _ -> None

    let get_stmt_state stmt =
      Left.Store.get_stmt_state stmt >>- fun left ->
      Right.Store.get_stmt_state stmt >>-: fun right ->
      left, right
    let get_stmt_state_by_callstack ~after stmt =
      let left_tbl = Left.Store.get_stmt_state_by_callstack ~after stmt
      and right_tbl = Right.Store.get_stmt_state_by_callstack ~after stmt in
      match left_tbl, right_tbl with
      | Some left, Some right -> merge_callstack_tbl left right
      | _, _ -> None

  end

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
