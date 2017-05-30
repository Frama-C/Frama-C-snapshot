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

module type Domain = sig
  include Abstract_domain.Logic
  include Datatype.S with type t = state
end

module ActiveBehaviors : sig
  type t
  val is_active: t -> behavior -> Alarmset.status
  val active_behaviors: t -> behavior list
  val behavior_from_name: t -> string -> behavior
end

val process_inactive_behaviors:
  kernel_function -> kinstr -> ActiveBehaviors.t -> unit

val process_inactive_postconds:
  kernel_function -> behavior list -> unit


module type S = sig
  type state
  type states

  val create: state -> kernel_function -> ActiveBehaviors.t
  val create_from_spec: state -> spec -> ActiveBehaviors.t

  val check_fct_preconditions_for_behavior:
    kernel_function -> ActiveBehaviors.t -> per_behavior:bool -> kinstr ->
    states -> behavior -> states

  val check_fct_preconditions:
    kernel_function -> ActiveBehaviors.t -> kinstr -> state -> states or_bottom

  val check_fct_postconditions_for_behaviors:
    kernel_function -> ActiveBehaviors.t -> behavior list -> termination_kind ->
    per_behavior:bool ->
    pre_state:state -> post_states:states -> result:varinfo option -> states

  val check_fct_postconditions:
    kernel_function -> ActiveBehaviors.t -> termination_kind ->
    pre_state:state -> post_states:states -> result:varinfo option ->
    states or_bottom

  val reduce_by_assumes_of_behavior:
    kernel_function -> behavior -> states -> states

  val interp_annot:
    limit:int -> record:bool ->
    kernel_function -> ActiveBehaviors.t -> stmt -> code_annotation ->
    initial_state:state -> states -> states
end


module Make
    (Domain: Domain)
    (States: Powerset.S with type state = Domain.t)
  : S with type state = Domain.t
       and type states = States.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
