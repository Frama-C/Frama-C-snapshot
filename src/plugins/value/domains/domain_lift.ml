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

  module Summary = Domain.Summary
  type summary = Domain.summary



  let lift_left left = { left with lloc = Convert.restrict_loc left.lloc }
  let lift_flagged_value value =
    { value with v = value.v >>-: Convert.restrict_val }
  let lift_copied = function
    | Determinate right ->
      Determinate {right with v = Convert.restrict_val right.v }
    | Exact right ->
      Exact (lift_flagged_value right)
  let lift_copy (left, copied) = lift_left left, lift_copied copied
  let lift_assigned = function
    | Assign value -> Assign (Convert.restrict_val value)
    | Copy (lval, copied) -> Copy (lval, lift_copied copied)

  let lift_argument arg = { arg with avalue = lift_assigned arg.avalue }

  let lift_call call =
    let arguments = List.map lift_argument call.arguments in
    let rest =
      List.map (fun (exp, assigned) -> exp, lift_assigned assigned) call.rest
    in
    { call with arguments; rest }

  let extend_return return =
    let extend value =
      { value with v = value.v >>-: Convert.extend_val }
    in
    let returned_value = Extlib.opt_map extend return.returned_value in
    { return with returned_value }

  let extend_call_result res = res >>-: List.map extend_return

  let extend_action = function
    | Compute _
    | Recall _ as a -> a
    | Result (res, cacheable) -> Result (extend_call_result res, cacheable)


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

    type state = Domain.state
    type summary = Domain.summary
    type value = Convert.extended_value
    type location = Convert.extended_location
    type valuation = Valuation.t

    let update = Internal_Transfer.update

    let assign stmt lv expr value valuation state =
      Internal_Transfer.assign stmt
        (lift_left lv) expr (lift_assigned value) valuation state

    let assume = Internal_Transfer.assume

    let call_action stmt call valuation state =
      let call = lift_call call in
      extend_action (Internal_Transfer.call_action stmt call valuation state)

    let summarize kf stmt ~returned state =
      let returned = Extlib.opt_map lift_copy returned in
      Internal_Transfer.summarize kf stmt ~returned state

    let resolve_call stmt call ~assigned valuation ~pre ~post =
      let call = lift_call call in
      let assigned = Extlib.opt_map lift_copy assigned in
      Internal_Transfer.resolve_call stmt call ~assigned valuation ~pre ~post

    let default_call stmt call state =
      let result = Internal_Transfer.default_call stmt (lift_call call) state in
      extend_call_result result
  end

  let compute_using_specification kinstr kf state =
    extend_call_result (Domain.compute_using_specification kinstr kf state)

  include (Domain : Abstract_domain.Logic with type state := t)


  let close_block = Domain.close_block
  let open_block = Domain.open_block

  let empty = Domain.empty
  let initialize_var state lval loc value =
    let loc = Convert.restrict_loc loc in
    let value = value >>-: fun (v, b) -> Convert.restrict_val v, b in
    Domain.initialize_var state lval loc value

  let initialize_var_using_type = Domain.initialize_var_using_type
  let global_state = Domain.global_state

  let filter_by_bases = Domain.filter_by_bases
  let reuse = Domain.reuse

end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
