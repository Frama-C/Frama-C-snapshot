(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

open Cil_types
open Eval

module type S = sig

  type state
  type value
  type summary

  val assign: with_alarms:CilE.warn_mode ->
    state -> kernel_function -> stmt -> lval -> exp -> state or_bottom

  val assume: with_alarms:CilE.warn_mode ->
    state -> stmt -> exp -> bool -> state or_bottom

  val call: with_alarms:CilE.warn_mode ->
    stmt -> lval option -> exp -> exp list -> state ->
    state list or_bottom * Value_types.cacheable

  val return: with_alarms:CilE.warn_mode ->
    kernel_function -> stmt -> lval option -> state ->
    (state, summary, value) return or_bottom

  val split_final_states:
    kernel_function -> exp -> Integer.t list -> state list -> state list list

  val check_unspecified_sequence:
    with_alarms:CilE.warn_mode ->
    state ->
    (* TODO *)
    (stmt * lval list * lval list * lval list * stmt ref list) list ->
    unit or_bottom

  type res = (state, summary, value) call_result * Value_types.cacheable

  val compute_call_ref: (kinstr -> value call -> state -> res) ref
end


module Make
    (Domain: Abstract_domain.Transfer)
    (Eva: Evaluation.S with type state = Domain.state
                        and type value = Domain.value
                        and type loc = Domain.location
                        and type Valuation.t = Domain.valuation)
  : S with type state = Domain.state
       and type value = Domain.value
       and type summary = Domain.summary


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
