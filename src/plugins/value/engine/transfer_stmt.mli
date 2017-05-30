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

  val init: with_alarms:CilE.warn_mode ->
    state -> kernel_function -> stmt -> varinfo -> Cil_types.init ->
    state or_bottom

  val assign: with_alarms:CilE.warn_mode ->
    state -> kernel_function -> stmt -> lval -> exp -> state or_bottom

  val assume: with_alarms:CilE.warn_mode ->
    state -> stmt -> exp -> bool -> state or_bottom

  val call: with_alarms:CilE.warn_mode ->
    stmt -> lval option -> exp -> exp list -> state ->
    state list or_bottom * Value_types.cacheable

  val split_final_states:
    kernel_function -> exp -> Integer.t list -> state list -> state list list

  val check_unspecified_sequence:
    with_alarms:CilE.warn_mode ->
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

module type Domain = sig
  include Abstract_domain.Transfer
  val leave_scope: kernel_function -> varinfo list -> state -> state
  module Store: Abstract_domain.Store with type state := state
  include Datatype.S with type t = state
end

module Make
    (Value: Abstract_value.S)
    (Location: Abstract_location.External)
    (Domain: Domain with type value = Value.t
                     and type location = Location.location)
    (Init: Initialization.S with type state := Domain.state)
    (Eva: Evaluation.S with type state = Domain.state
                        and type value = Domain.value
                        and type loc = Domain.location
                        and type Valuation.t = Domain.valuation)
  : S with type state = Domain.state
       and type value = Domain.value


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
