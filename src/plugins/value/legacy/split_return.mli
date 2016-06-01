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

(** This module is used to merge together the final states of a function
    according to a given strategy. Default is to merge all states together *)

(** Join the given state_set. The strategy is defined according to
    the name of the function. *)
val join_final_states:
  Cil_types.kernel_function ->
  return_lv:Cil_types.lval option ->
  State_set.t ->
  Cvalue.Model.t list

val pretty_strategies: unit -> unit

val kf_strategy: Kernel_function.t -> Split_strategy.t

(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
