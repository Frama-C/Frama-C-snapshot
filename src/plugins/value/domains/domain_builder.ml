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


module type InputDomain = sig
  include Abstract_domain.S_with_Structure
  val storage: unit -> bool
end

module Complete
    (Domain: InputDomain)
= struct

  include Domain
  module Store = Domain_store.Make (Domain)

end

open Simpler_domains

let simplify_argument argument =
  { formal = argument.Eval.formal;
    concrete = argument.Eval.concrete }

let simplify_call call =
  { kf = call.Eval.kf;
    arguments = List.map simplify_argument call.Eval.arguments;
    rest = List.map fst call.Eval.rest;
    return = call.Eval.return;
    recursive = call.Eval.recursive }

module Make_Minimal
    (Value: Abstract_value.S)
    (Location: Abstract_location.S)
    (Domain: Simpler_domains.Minimal)
= struct

  include Domain

  let structure = Abstract_domain.Void
  let log_category = Value_parameters.register_category ("d-" ^ name)

  type value = Value.t
  type location = Location.location
  type state = Domain.t
  type origin = unit

  let narrow x _y = `Value x

  let top_answer = `Value (Value.top, ()), Alarmset.all
  let extract_expr _oracle _state _expr = top_answer
  let extract_lval _oracle _state _lval _typ _location = top_answer
  let backward_location _state _lval _typ location value = `Value (location, value)
  let reduce_further _sttae _expr _value = []

  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type origin = origin
                                             and type loc = location)
  = struct

    let update _valuation state = state

    let assign kinstr lv expr _value _valuation state =
      Domain.assign kinstr lv.Eval.lval expr state

    let assume stmt expr positive _valuation state =
      Domain.assume stmt expr positive state

    let start_call stmt call _valuation state =
      Eval.Compute (Domain.start_call stmt (simplify_call call) state)

    let finalize_call stmt call ~pre ~post =
      Domain.finalize_call stmt (simplify_call call) ~pre ~post

    let approximate_call stmt call state =
      let call = simplify_call call in
      let name = Kernel_function.get_name call.kf in
      if Ast_info.is_frama_c_builtin name ||
         (name <> "free" && Eval_typ.kf_assigns_only_result_or_volatile call.kf)
      then `Value [ state ]
      else Domain.approximate_call stmt call state

    let show_expr _valuation = Domain.show_expr
  end

  let enter_loop _stmt state = state
  let incr_loop_counter _stmt state = state
  let leave_loop _stmt state = state

  let initialize_variable lval _location ~initialized value state =
    Domain.initialize_variable lval ~initialized value state

  let initialize_variable_using_type _kind varinfo state =
    let lval = Cil.var varinfo in
    let state = introduce_globals [varinfo] state in
    Domain.initialize_variable lval ~initialized:true Abstract_domain.Top state

  let logic_assign _assigns _location ~pre:_ _state = top
  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ t _ _ = `Value t

  let filter_by_bases _bases state = state
  let reuse ~current_input:_ ~previous_output = previous_output
end


module Complete_Minimal
    (Value: Abstract_value.S)
    (Location: Abstract_location.S)
    (Domain: Simpler_domains.Minimal)
= struct

  module D = struct
    include Make_Minimal (Value) (Location) (Domain)

    include
      (Datatype.Make_with_collections
         (struct
           include Datatype.Undefined
           type t = Domain.t
           let name = Domain.name
           let reprs = [ Domain.top ]
           let equal x y = Domain.compare x y = 0
           let compare = Domain.compare
           let hash = Domain.hash
           let pretty = Domain.pretty
           let mem_project = Datatype.never_any_project
         end)
       : Datatype.S_with_collections with type t := t)

    let storage () = false
  end

  include Complete (D)

end


module Complete_Minimal_with_datatype
    (Value: Abstract_value.S)
    (Location: Abstract_location.S)
    (Domain: Minimal_with_datatype)
= struct

  module D = struct

    include Make_Minimal (Value) (Location) (Domain)

    include
      (Datatype.With_collections
         (Domain) (struct let module_name = Domain.name end)
       : Datatype.S_with_collections with type t := t)

    let storage () = false
  end

  include Complete (D)

end

open Eval

module Complete_Simple_Cvalue (Domain: Simpler_domains.Simple_Cvalue)
= struct

  module D = struct
    include Domain

    include
      (Datatype.With_collections
         (Domain) (struct let module_name = Domain.name end)
       : Datatype.S_with_collections with type t := t)

    let structure = Abstract_domain.Void
    let log_category = Value_parameters.register_category ("d-" ^ name)

    type value = Cvalue.V.t
    type location = Precise_locs.precise_location
    type state = Domain.t
    type origin = unit

    let narrow x _y = `Value x

    let extract_expr _oracle state expr =
      let v = Domain.extract_expr state expr >>-: fun v -> v, () in
      v, Alarmset.all

    let extract_lval _oracle state lval typ location =
      let v = Domain.extract_lval state lval typ location >>-: fun v -> v, () in
      v, Alarmset.all

    let backward_location _state _lval _typ location value =
      `Value (location, value)

    let reduce_further _state _expr _value = []

    module Transfer
        (Valuation: Abstract_domain.Valuation with type value = value
                                               and type origin = origin
                                               and type loc = location)
    = struct

      let find valuation expr =
        match Valuation.find valuation expr with
        | `Top -> `Top
        | `Value record -> `Value record.value

      let find_loc valuation lval =
        match Valuation.find_loc valuation lval with
        | `Top -> `Top
        | `Value record -> `Value record.loc

      let record valuation = { find = find valuation;
                               find_loc = find_loc valuation; }

      let update _valuation state = state
      let assign kinstr lv expr value valuation state =
        Domain.assign kinstr lv expr value (record valuation) state
      let assume stmt expr positive valuation state =
        Domain.assume stmt expr positive (record valuation) state
      let start_call stmt call valuation state =
        Compute (Domain.start_call stmt call (record valuation) state)
      let finalize_call = Domain.finalize_call

      let approximate_call stmt call state =
        let name = Kernel_function.get_name call.kf in
        if Ast_info.is_frama_c_builtin name
        then `Value [ state ]
        else Domain.approximate_call stmt call state

      let show_expr _valuation = Domain.show_expr

    end

    let enter_loop _stmt state = state
    let incr_loop_counter _stmt state = state
    let leave_loop _stmt state = state

    let initialize_variable lval _location ~initialized value state =
      Domain.initialize_variable lval ~initialized value state

    let initialize_variable_using_type _kind varinfo state =
      let lval = Cil.var varinfo in
      let state = introduce_globals [varinfo] state in
      Domain.initialize_variable lval ~initialized:true Abstract_domain.Top state

    let logic_assign _assigns _location ~pre:_ _state = top
    let evaluate_predicate _ _ _ = Alarmset.Unknown
    let reduce_by_predicate _ t _ _ = `Value t

    let filter_by_bases _bases state = state
    let reuse ~current_input:_ ~previous_output = previous_output

    let storage () = false
  end

  include Complete (D)
end
