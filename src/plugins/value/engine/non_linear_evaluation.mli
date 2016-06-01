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

(** Evaluation of non-linear expressions. *)

open Cil_types
open Eval

module Make
    (Value : Abstract_value.External)
    (Eva: Evaluation.S with type value = Value.t)
  : sig

    (** Same functionality as Eva.evaluate.
        For expressions in which some l-values appear multiple times, proceed
        by disjunction on their abstract value, in order to gain precision. *)
    val evaluate :
      ?valuation:Eva.Valuation.t -> ?indeterminate:bool -> ?reduction:bool ->
      Eva.state -> exp -> (Eva.Valuation.t * Value.t) evaluated

  end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
