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
