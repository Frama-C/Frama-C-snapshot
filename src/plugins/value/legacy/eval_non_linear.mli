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
compile-command: "make -C ../../../.."
End:
*)
