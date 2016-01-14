(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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
open Locations


(** Same functionality as {!Eval_exprs.eval_expr_with_deps_state}. For
    expressions in which some l-values appear multiple times, proceed
    by disjunction on their abstract value, in order to gain precision. *)
val eval_expr_with_deps_state :
  with_alarms:CilE.warn_mode ->
  Zone.t option ->
  Cvalue.Model.t ->
  Cil_types.exp ->
  Cvalue.Model.t * Zone.t option * Location_Bytes.t

val compute_non_linear:
  exp -> (lval * Locations.location option) list

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
