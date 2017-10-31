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

let log_key = Value_parameters.register_category "unit-domain"

module Static = struct
  module D = struct
    include Datatype.Unit
    type state = t

    let name = "Unit domain"
    let structure = Abstract_domain.Void
    let log_category = log_key

    let top = ()
    let is_included _ _ = true
    let join _ _ = ()
    let widen _ _ _ _ = ()
    let narrow _ _ = `Value ()

    let storage () = false
  end

  include D
  module Store = Domain_store.Make (D)
end

module Make
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S)
= struct

  include Static
  type value = Value.t
  type location = Loc.location

  type origin = unit

  let eval_top = `Value (Value.top, ()), Alarmset.all
  let extract_expr _ _ _ = eval_top
  let extract_lval _ _ _ _ _ = eval_top
  let backward_location _ _ _ loc value = `Value (loc, value)
  let reduce_further _ _ _  = []

  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type loc = location)
  = struct

    let update _ _ = ()
    let assign _ _ _ _ _ _ = `Value ()
    let assume _ _ _ _ _ = `Value ()
    let start_call _ _ _ _ = Compute ()
    let finalize_call _ _ ~pre:_ ~post:_ = `Value ()
    let approximate_call _ _ _ = `Value [ () ]
    let show_expr _ _ _ _ = ()
  end

  let logic_assign _ _ ~pre:_ _ = ()
  let evaluate_predicate _ _ _ = Alarmset.Unknown
  let reduce_by_predicate _ _ _ _ = `Value ()

  let enter_scope _ _ _ = ()
  let leave_scope _ _ _ = ()

  let enter_loop _ _ = ()
  let incr_loop_counter _ _ = ()
  let leave_loop _ _ = ()

  let empty () = ()
  let introduce_globals _ () = ()
  let initialize_variable _ _ ~initialized:_ _ _ = ()
  let initialize_variable_using_type _ _ _  = ()

  let filter_by_bases _ _ = ()
  let reuse ~current_input:_ ~previous_output:_ = ()
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
