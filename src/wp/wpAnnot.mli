(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(** Every access to annotations have to go through here,
  * so this is the place where we decide what the computation
  * is allowed to use. *)

open Cil_types

(*----------------------------------------------------------------------------*)

(** splits a prop_id goals into prop_id parts for each sub-goals *)
val split : ( WpPropId.prop_id -> 'a -> unit ) -> WpPropId.prop_id -> 
  'a Bag.t -> unit

(** A proof accumulator for a set of related prop_id *)
type proof

val create_proof : WpPropId.prop_id -> proof
  (** to be used only once for one of the related prop_id *)

val add_proof : proof -> WpPropId.prop_id -> Property.t list -> unit
  (** accumulate int the proof the partial proof for this prop_id *)

val is_composed : proof -> bool
  (** whether a proof needs several lemma to be complete *)

val is_proved : proof -> bool
  (** wether all partial proofs have been accumulated or not *)

val target : proof -> Property.t
val dependencies : proof -> Property.t list
val missing_rte : kernel_function -> string list

(*----------------------------------------------------------------------------*)

val get_called_post_conditions : kernel_function -> Property.t list
val get_called_exit_conditions : kernel_function -> Property.t list
val get_called_assigns : kernel_function -> Property.t list
val get_called_preconditions_at : kernel_function -> stmt -> Property.t list
val lookup_called_preconditions_at : kernel_function -> stmt -> Property.t list

(*----------------------------------------------------------------------------*)

type asked_assigns = NoAssigns | OnlyAssigns | WithAssigns

(**
 * Defines how annotations are applied on the Cfg during the WP.
 * - [~behaviors:bhv] : only annotations related
 *   to [b] in [bhv] are taken into account.
 *   Otherwise all behaviors are included.
 *   The default behavior is one behavior among the others.
 * - [~property:pid] : only the mentioned property id is turned into a goal.
 *   Others are only used in hypothesis.
 *   Otherwise, annotations are generally turned into goals unless
 *   they already have a valid current status.
 * - [~assigns] : filter in or out the assigns clauses.
 *
 **)
(*
val get_strategies : Cil2cfg.t
  -> ?behaviors:string list
  -> ?property:asked_prop
  -> ?assigns:assigns
  -> unit -> strategy list
  *)

(** Compute the strategies to prove the selected property.  *)
val get_id_prop_strategies : 
  ?assigns:asked_assigns -> Property.t -> WpStrategy.strategy list

val get_call_pre_strategies : stmt -> WpStrategy.strategy list

(** Similar to [get_id_prop_strategies] but with a named property.
* (useful for command line option).
* The behavior list has to be the behaviors of the property.
* TODO: it should be removed when we will be able to compute it. *)
val get_prop_strategies : ?assigns:asked_assigns -> Kernel_function.t ->
  (string list * string) -> WpStrategy.strategy list

(** Compute the strategy to prove all the properties of the behavior.
* Notice that is a property is related to several behaviors,
* it might not be fully proved with this strategy.
* Can return more strategies than names in the input list
* because a name can be used for several (disjoint) statement spec.
* *)
val get_behavior_strategies :
  ?assigns:asked_assigns -> Kernel_function.t -> string list -> WpStrategy.strategy list

(** Compute the strategies to prove all the properties of the selected function.
* *)
val get_function_strategies : 
  ?assigns:asked_assigns -> Kernel_function.t -> WpStrategy.strategy list

(*----------------------------------------------------------------------------*)
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
(*----------------------------------------------------------------------------*)
