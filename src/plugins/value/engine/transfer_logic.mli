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

module ActiveBehaviors : sig
  type t
  val is_active: t -> behavior -> Alarmset.status
  val active_behaviors: t -> behavior list
  val behavior_from_name: t -> string -> behavior
  val create: (predicate -> Alarmset.status) -> spec -> t
end

(* Marks all behaviors of the list as inactive. *)
val process_inactive_behaviors:
  kinstr -> kernel_function -> behavior list -> unit

module type S = sig
  type state
  type states

  val create: state -> kernel_function -> ActiveBehaviors.t
  val create_from_spec: state -> spec -> ActiveBehaviors.t

  val check_fct_preconditions_for_behaviors:
    kinstr -> kernel_function -> behavior list -> Alarmset.status ->
    states -> states

  val check_fct_preconditions:
    kinstr -> kernel_function -> ActiveBehaviors.t -> state -> states or_bottom

  val check_fct_postconditions_for_behaviors:
    kernel_function -> behavior list -> Alarmset.status ->
    pre_state:state -> post_states:states -> result:varinfo option -> states

  val check_fct_postconditions:
    kernel_function -> ActiveBehaviors.t -> termination_kind ->
    pre_state:state -> post_states:states -> result:varinfo option ->
    states or_bottom

  val evaluate_assumes_of_behavior: state -> behavior -> Alarmset.status

  val interp_annot:
    limit:int -> record:bool ->
    kernel_function -> ActiveBehaviors.t -> stmt -> code_annotation ->
    initial_state:state -> states -> states
end

module type LogicDomain = sig
  type t
  val top: t
  val equal: t -> t -> bool
  val evaluate_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> Alarmset.status
  val reduce_by_predicate:
    t Abstract_domain.logic_environment -> t -> predicate -> bool -> t or_bottom
end

module Make
    (Domain: LogicDomain)
    (States: Powerset.S with type state = Domain.t)
  : S with type state = Domain.t
       and type states = States.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
