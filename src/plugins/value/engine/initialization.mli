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

(** Creation of the inital state of abstract domain. *)

open Cil_types
open Bottom.Type

module type S = sig
  type state
  val initial_state : unit -> state or_bottom
  val initial_state_with_formals : kernel_function -> state or_bottom
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
