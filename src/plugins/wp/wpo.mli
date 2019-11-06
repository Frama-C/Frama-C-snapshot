(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open LogicUsage
open VCS
open Cil_types
open Cil_datatype
open WpPropId

type index =
  | Axiomatic of string option
  | Function of kernel_function * string option

(* ------------------------------------------------------------------------ *)
(**{1 Proof Obligations}                                                    *)
(* ------------------------------------------------------------------------ *)

module DISK :
sig
  val cache_log : pid:prop_id -> model:WpContext.model ->
    prover:prover -> result:result -> string
  val pretty : pid:prop_id -> model:WpContext.model ->
    prover:prover -> result:result -> Format.formatter -> unit
  val file_kf : kf:kernel_function -> model:WpContext.model -> prover:prover -> string
  val file_goal : pid:prop_id -> model:WpContext.model -> prover:prover -> string
  val file_logout : pid:prop_id -> model:WpContext.model -> prover:prover -> string
  val file_logerr : pid:prop_id -> model:WpContext.model -> prover:prover -> string
end

module GOAL :
sig
  type t
  open Lang
  val dummy : t
  val trivial : t
  val is_trivial : t -> bool
  val make : Conditions.sequent -> t
  val compute_proof : t -> F.pred
  val compute_descr : t -> Conditions.sequent
  val get_descr : t -> Conditions.sequent
  val compute : t -> unit
  val qed_time : t -> float
end

module VC_Lemma :
sig

  type t = {
    lemma : Definitions.dlemma ;
    depends : logic_lemma list ;
    mutable sequent : Conditions.sequent option ;
  }

  val is_trivial : t -> bool
  val cache_descr : t -> (prover * result) list -> string

end

module VC_Annot :
sig

  type t = {
    axioms : Definitions.axioms option ;
    goal : GOAL.t ;
    tags : Splitter.tag list ;
    warn : Warning.t list ;
    deps : Property.Set.t ;
    path : Stmt.Set.t ;
    effect : (stmt * effect_source) option ;
  }

  val resolve : t -> bool
  val is_trivial : t -> bool
  val cache_descr : pid:prop_id -> t -> (prover * result) list -> string

end

(* ------------------------------------------------------------------------ *)
(**{1 Proof Obligations}                                                    *)
(* ------------------------------------------------------------------------ *)

type formula =
  | GoalLemma of VC_Lemma.t
  | GoalAnnot of VC_Annot.t

type po = t and t = {
    po_gid   : string ;  (** goal identifier *)
    po_leg   : string ; (** legacy goal identifier *)
    po_sid   : string ;  (** goal short identifier (without model) *)
    po_name  : string ;  (** goal informal name *)
    po_idx   : index ;   (** goal index *)
    po_model : WpContext.model ;
    po_pid   : WpPropId.prop_id ; (* goal target property *)
    po_formula : formula ; (* proof obligation *)
  }

module S : Datatype.S_with_collections with type t = po
module Index : Map.OrderedType with type t = index
module Gmap : FCMap.S with type key = index

(** Dynamically exported
    @since Nitrogen-20111001
*)
val get_gid: t -> string

(** Dynamically exported
    @since Oxygen-20120901
*)
val get_property: t -> Property.t
val get_index : t -> index
val get_label : t -> string
val get_model : t -> WpContext.model
val get_scope : t -> WpContext.scope
val get_context : t -> WpContext.context
val get_file_logout : t -> prover -> string (** only filename, might not exists *)
val get_file_logerr : t -> prover -> string (** only filename, might not exists *)

val get_files : t -> (string * string) list

val qed_time : t -> float

val clear : unit -> unit
val remove : t -> unit
val on_remove : (t -> unit) -> unit

val add : t -> unit
val age : t -> int (* generation *)

val reduce : t -> bool (** tries simplification *)
val resolve : t -> bool (** tries simplification and set result if valid *)
val set_result : t -> prover -> result -> unit
val clear_results : t -> unit

val compute : t -> Definitions.axioms option * Conditions.sequent

val has_verdict : t -> prover -> bool
val get_result : t -> prover -> result
val get_results : t -> (prover * result) list
val get_proof : t -> bool * Property.t
val is_trivial : t -> bool (** do not tries simplification, do not check prover results *)
val is_proved : t -> bool (** do not tries simplification, check prover results *)
val is_unknown : t -> bool
val warnings : t -> Warning.t list

(** [true] if the result is valid. Dynamically exported.
    @since Nitrogen-20111001
*)
val is_valid: result -> bool

val get_time: result -> float
val get_steps: result -> int

val is_tactic : t -> bool

val iter :
  ?ip:Property.t ->
  ?index:index ->
  ?on_axiomatics:(string option -> unit) ->
  ?on_behavior:(kernel_function -> string option -> unit) ->
  ?on_goal:(t -> unit) ->
  unit -> unit

(** Dynamically exported.
    @since Nitrogen-20111001
*)
val iter_on_goals: (t -> unit) -> unit

(** All POs related to a given property.
    Dynamically exported
    @since Oxygen-20120901
*)
val goals_of_property: Property.t -> t list

val bar : string
val kf_context : index -> Description.kf
val pp_index : Format.formatter -> index -> unit
val pp_warnings : Format.formatter -> Warning.t list -> unit
val pp_depend : Format.formatter -> Property.t -> unit
val pp_dependency : Description.kf -> Format.formatter -> Property.t -> unit
val pp_dependencies : Description.kf -> Format.formatter -> Property.t list -> unit
val pp_goal : Format.formatter -> t -> unit
val pp_title : Format.formatter -> t -> unit
val pp_logfile : Format.formatter -> t -> prover -> unit

val pp_axiomatics : Format.formatter -> string option -> unit
val pp_function : Format.formatter -> Kernel_function.t -> string option -> unit
val pp_goal_flow : Format.formatter -> t -> unit

(** Dynamically exported. *)
val prover_of_name : string -> prover option
