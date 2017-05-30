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

(** Creation of the initial state of abstract domain. *)

open Cil_types
open Bottom.Type

module type S = sig
  type state

  (** Compute the initial state for an analysis. The initial state is generated
      according to the options of Value governing the shape of this state.
      All global variables are bound in the resulting abstract state. *)
  val initial_state : lib_entry:bool -> state or_bottom

  (** Compute the initial state for an analysis (as in {!initial_state}),
      but also bind the formal parameters of the function given as argument. *)
  val initial_state_with_formals :
    lib_entry:bool -> kernel_function -> state or_bottom

  (** add to the current state a varinfo with its initialization. Also used
      for initializing locals.
      @since Phosphorus-20170501-beta1
  *)
  val initialize_var: with_alarms:CilE.warn_mode ->
    stmt -> varinfo -> Cil_types.init -> state -> state or_bottom
end

module Make
    (Value: Abstract_value.S)
    (Loc: Abstract_location.S with type value = Value.t)
    (Domain: Abstract_domain.External with type value = Value.t
                                       and type location = Loc.location)
    (Eva: Evaluation.S with type state = Domain.state
                        and type value = Domain.value
                        and type origin = Domain.origin
                        and type loc = Domain.location)
  : S with type state := Domain.t


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
