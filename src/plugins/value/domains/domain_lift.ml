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

module type Conversion = sig
  type extended_value
  type extended_location
  type internal_value
  type internal_location

  val extend_val : internal_value -> extended_value
  val restrict_val : extended_value -> internal_value

  val extend_loc : internal_location -> extended_location
  val restrict_loc : extended_location -> internal_location
end


module Make
    (Domain: Abstract_domain.Internal)
    (Convert : Conversion with type internal_value := Domain.value
                           and type internal_location := Domain.location)
= struct

  include (Domain : Datatype.S_with_collections with type t = Domain.t)
  include (Domain : Abstract_domain.Lattice with type state = Domain.state)

  let structure = Domain.structure
  let log_category = Domain.log_category

  type value = Convert.extended_value
  type location = Convert.extended_location

  type origin = Domain.origin

  let extract_expr oracle state exp =
    let oracle exp = oracle exp >>=: Convert.restrict_val in
    Domain.extract_expr oracle state exp >>=: fun (value, origin) ->
    Convert.extend_val value, origin

  let extract_lval oracle state lval typ loc =
    let oracle exp = oracle exp >>=: Convert.restrict_val in
    let loc = Convert.restrict_loc loc in
    Domain.extract_lval oracle state lval typ loc >>=: fun (value, origin) ->
    Convert.extend_val value, origin

  let backward_location state lval typ loc value =
    Domain.backward_location
      state lval typ (Convert.restrict_loc loc) (Convert.restrict_val value)
    >>-: fun (loc, value) ->
    Convert.extend_loc loc, Convert.extend_val value

  let reduce_further state expr value =
    let list = Domain.reduce_further state expr (Convert.restrict_val value) in
    List.map (fun (e, v) -> e, Convert.extend_val v) list


  let lift_left left = { left with lloc = Convert.restrict_loc left.lloc }
  let lift_flagged_value value =
    { value with v = value.v >>-: Convert.restrict_val }
  let lift_assigned = function
    | Assign value -> Assign (Convert.restrict_val value)
    | Copy (lval, value) -> Copy (lval, lift_flagged_value value)

  let lift_argument arg = { arg with avalue = lift_assigned arg.avalue }

  let lift_call call =
    let arguments = List.map lift_argument call.arguments in
    let rest =
      List.map (fun (exp, assigned) -> exp, lift_assigned assigned) call.rest
    in
    { call with arguments; rest }

  module Transfer
      (Valuation:
         Abstract_domain.Valuation with type value = Convert.extended_value
                                    and type origin = Domain.origin
                                    and type loc = Convert.extended_location)
  = struct

    module Internal_Valuation = struct
      type t = Valuation.t
      type value = Domain.value
      type origin = Domain.origin
      type loc = Domain.location

      let lift_record record =
        { record with value = lift_flagged_value record.value }

      let find valuation expr = match Valuation.find valuation expr with
        | `Value record -> `Value (lift_record record)
        | `Top          -> `Top

      let fold f valuation acc =
        Valuation.fold
          (fun exp record acc -> f exp (lift_record record) acc)
          valuation acc

      let find_loc valuation loc = match Valuation.find_loc valuation loc with
        | `Value r -> `Value {r with loc = Convert.restrict_loc r.loc}
        | `Top     -> `Top

    end

    module Internal_Transfer = Domain.Transfer (Internal_Valuation)

    let update = Internal_Transfer.update

    let assign stmt lv expr value valuation state =
      Internal_Transfer.assign stmt
        (lift_left lv) expr (lift_assigned value) valuation state

    let assume = Internal_Transfer.assume

    let start_call stmt call valuation state =
      let call = lift_call call in
      Internal_Transfer.start_call stmt call valuation state

    let finalize_call stmt call ~pre ~post =
      let call = lift_call call in
      Internal_Transfer.finalize_call stmt call ~pre ~post

    let approximate_call stmt call state =
      Internal_Transfer.approximate_call stmt (lift_call call) state

    let show_expr = Internal_Transfer.show_expr
  end

  let logic_assign assigns location ~pre state =
    Domain.logic_assign assigns (Convert.restrict_loc location) ~pre state

  let evaluate_predicate = Domain.evaluate_predicate
  let reduce_by_predicate = Domain.reduce_by_predicate

  let enter_scope = Domain.enter_scope
  let leave_scope = Domain.leave_scope

  let enter_loop = Domain.enter_loop
  let incr_loop_counter = Domain.incr_loop_counter
  let leave_loop = Domain.leave_loop

  let empty = Domain.empty
  let introduce_globals = Domain.introduce_globals
  let initialize_variable lval loc ~initialized init_value state =
    let loc = Convert.restrict_loc loc in
    Domain.initialize_variable lval loc ~initialized init_value state

  let initialize_variable_using_type = Domain.initialize_variable_using_type

  let filter_by_bases = Domain.filter_by_bases
  let reuse = Domain.reuse

  module Store = Domain.Store

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
