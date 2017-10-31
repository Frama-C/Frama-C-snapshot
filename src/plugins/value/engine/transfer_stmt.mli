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

open Cil_types
open Eval

val current_kf_inout: unit -> Inout_type.t option

module type S = sig

  type state
  type value

  val assign: state -> kinstr -> lval -> exp -> state or_bottom

  val assume: state -> stmt -> exp -> bool -> state or_bottom

  val call:
    stmt -> lval option -> exp -> exp list -> state ->
    state list or_bottom * Value_types.cacheable

  val split_final_states:
    kernel_function -> exp -> Integer.t list -> state list -> state list list

  val check_unspecified_sequence:
    Cil_types.stmt ->
    state ->
    (* TODO *)
    (stmt * lval list * lval list * lval list * stmt ref list) list ->
    unit or_bottom

  type call_result = {
    states: state list or_bottom;
    cacheable: Value_types.cacheable;
  }

  val compute_call_ref: (kinstr -> value call -> state -> call_result) ref
end

module Make
    (Value: Abstract_value.External)
    (Location: Abstract_location.External)
    (Domain: Abstract_domain.External with type value = Value.t
                                       and type location = Location.location)
    (Eva: Evaluation.S with type state = Domain.state
                        and type value = Domain.value
                        and type loc = Domain.location
                        and type origin = Domain.origin)
  : S with type state = Domain.state
       and type value = Domain.value


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
