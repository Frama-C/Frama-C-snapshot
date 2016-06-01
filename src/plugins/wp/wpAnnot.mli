(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
(** accumulate in the proof the partial proof for this prop_id *)

val is_composed : proof -> bool
(** whether a proof needs several lemma to be complete *)

val is_proved : proof -> bool
(** wether all partial proofs have been accumulated or not *)

val target : proof -> Property.t
val dependencies : proof -> Property.t list
val missing_rte : kernel_function -> string list

val filter_status : WpPropId.prop_id -> bool

(*----------------------------------------------------------------------------*)

val get_called_preconditions_at : kernel_function -> stmt -> Property.t list
val get_called_post_conditions : kernel_function -> Property.t list
val get_called_exit_conditions : kernel_function -> Property.t list
val get_called_assigns : kernel_function -> Property.t list

(*----------------------------------------------------------------------------*)

type asked_assigns = NoAssigns | OnlyAssigns | WithAssigns

val get_id_prop_strategies :
  ?assigns:asked_assigns -> Property.t -> WpStrategy.strategy list

val get_call_pre_strategies : stmt -> WpStrategy.strategy list

val get_function_strategies :
  ?assigns:asked_assigns ->
  ?bhv:string list ->
  ?prop:string list ->
  Kernel_function.t -> WpStrategy.strategy list

(*----------------------------------------------------------------------------*)
