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

open Cil_types
open Eval

module type S = sig
  type state
  type states

  type active_behaviors
  val create: state -> kernel_function -> active_behaviors

  val check_fct_preconditions:
    kernel_function -> active_behaviors -> kinstr -> state -> states or_bottom

  val check_fct_postconditions:
    kernel_function -> active_behaviors -> termination_kind ->
    init_state:state -> post_states:states -> result:varinfo option ->
    states or_bottom

  val interp_annot:
    limit:int -> record:bool ->
    kernel_function -> active_behaviors -> stmt -> code_annotation ->
    initial_state:state -> states -> states
end


module Make
    (Domain: Abstract_domain.S)
    (States: Partitioning.StateSet with type state = Domain.t)
  : S with type state = Domain.t
       and type states = States.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
