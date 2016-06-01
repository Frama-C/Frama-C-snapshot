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

module Make
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

  module Summary = Datatype.Unit
  type summary = unit

  let call_return kf =
    let returned_value =
      try
        let stmt = Kernel_function.find_return kf in
        match stmt.Cil_types.skind with
        | Cil_types.Return (None, _) -> None
        | Cil_types.Return (Some _, _) ->
          Some { v = `Value Value.top;
                 initialized = false;
                 escaping = true;
               }
        | _ -> assert false
      with
        Kernel_function.No_Statement ->
        let vi = Kernel_function.get_vi kf in
        let return_type = Cil.getReturnType vi.Cil_types.vtype in
        let name = Kernel_function.get_name kf in
        if Cil.isVoidType return_type
        || (name >= "Frama_C_show" && name < "Frama_C_shox")
        || (name >= "Frama_C_dump" && name < "Frama_C_dumq")
        then None
        else Some { v = `Value Value.top;
                    initialized = false;
                    escaping = true;
                  }

    in
    let return = { post_state = (); summary = (); returned_value } in
    `Value [return]

  module Transfer
      (Valuation: Abstract_domain.Valuation with type value = value
                                             and type loc = location)
  = struct

    type state = t
    type summary = Summary.t
    type value = Value.t
    type location = Loc.location
    type valuation = Valuation.t

    let update _ _ = ()
    let assign _ _ _ _ _ _ = `Value ()
    let assume _ _ _ _ _ = `Value ()
    let call_action _ _ _ _ = Compute (Continue (), true)
    let summarize _ _ ~returned:_ _ = `Value ((), ())
    let resolve_call _ _ ~assigned:_ _ ~pre:_ ~post:_ = `Value ()
    let default_call _ call _ = call_return call.kf

  end

  type eval_env = unit
  let env_current_state _ = `Value ()
  let env_annot ~pre:_ ~here:_ () = ()
  let env_pre_f ~pre:_ () = ()
  let env_post_f ~pre:_ ~post:_ ~result:_ () = ()
  let eval_predicate _ _ = Alarmset.Unknown
  let reduce_by_predicate _ _ _ = ()

  let compute_using_specification _ (kf, _) _ = call_return kf

  let close_block _ _ ~body:_ _ = ()
  let open_block  _ _ ~body:_ _ = ()

  let empty () = ()
  let initialize_var _ _ _ _ = ()
  let initialize_var_using_type _ _  = ()
  let global_state () = None

  let filter_by_bases _ _ = ()
  let reuse ~current_input:_ ~previous_output:_ = ()
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
