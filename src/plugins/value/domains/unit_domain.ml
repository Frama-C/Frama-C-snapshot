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

module MakeInternal
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S)
= struct

  include Datatype.Unit
  type state = t
  type value = Value.t
  type location = Loc.location

  let structure = Abstract_domain.Void

  let top = ()
  let is_included _ _ = true
  let join _ _ = ()
  let join_and_is_included _ _ = (), true
  let widen _ _ _ _ = ()

  type origin = unit

  let eval_top = `Value (Value.top, ()), Alarmset.all
  let extract_expr _ _ _ = eval_top
  let extract_lval _ _ _ _ _ = eval_top
  let backward_location _ _ _ loc value = `Value (loc, value)
  let reduce_further _ _ _  = []

  let call_result = `Value [ () ]

  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type loc = location)
  = struct

    type state = t
    type value = Value.t
    type location = Loc.location
    type valuation = Valuation.t

    let update _ _ = ()
    let assign _ _ _ _ _ _ = `Value ()
    let assume _ _ _ _ _ = `Value ()
    let start_call _ _ _ _ = Compute (Continue (), true)
    let finalize_call _ _ ~pre:_ ~post:_ = `Value ()
    let approximate_call _ _ _ = call_result

  end

  type eval_env = unit
  let env_current_state _ = `Value ()
  let env_annot ~pre:_ ~here:_ () = ()
  let env_pre_f ~pre:_ () = ()
  let env_post_f ~pre:_ ~post:_ ~result:_ () = ()
  let eval_predicate _ _ = Alarmset.Unknown
  let reduce_by_predicate _ _ _ = ()

  let compute_using_specification _ _ _ _ = call_result

  let enter_scope _ _ _ = ()
  let leave_scope _ _ _ = ()

  let enter_loop _ _ = ()
  let incr_loop_counter _ _ = ()
  let leave_loop _ _ = ()

  let empty () = ()
  let initialize_var _ _ _ _ = ()
  let initialize_var_using_type _ _  = ()
  let global_state () = None

  let filter_by_bases _ _ = ()
  let reuse ~current_input:_ ~previous_output:_ = ()

  let storage () = false

end

module Make
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S)
= Domain_builder.Complete (MakeInternal (Value) (Loc))




(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
