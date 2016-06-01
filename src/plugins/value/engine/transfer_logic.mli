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
