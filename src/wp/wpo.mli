(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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

open LogicUsage
open VCS
open Cil_types
open Cil_datatype
open WpPropId

type index =
  | Lemma of string
  | Function of kernel_function * string option

(* ------------------------------------------------------------------------ *)
(**{1 Legacy Obligations}                                                   *)
(* ------------------------------------------------------------------------ *)

module VC_Legacy :
sig

  type t = {
    mid : string ;  (* model identifier *)
    env : string ;  (* goal environment identifier *)
    dep : Property.t list ; (* dependencies *)
    wrn : Warning.t list ; (* warnings *)
  }

  val file_for_ctxt  : env:string -> string
  val file_for_head  : gid:string -> string
  val file_for_body  : gid:string -> string
  val file_for_log_proof : gid:string -> prover -> string
  val file_for_log_check : gid:string -> language -> string
  val file_for_po    : gid:string -> language -> string
  val file_for_goal  : gid:string -> language -> string
  val file_for_env   : env:string -> language -> string
  val file_for_model : model:string -> language -> string
    
  val coq_for_env: env:string -> string
  val coq_for_model : model:string -> string
  val coqc_for_model : model:string -> string
  val coqlog_for_model : model:string -> string
  val coqlog_for_env : env:string -> string

  val pp_environment : Format.formatter -> env:string -> unit

end

(* ------------------------------------------------------------------------ *)
(**{1 New Obligations}                                                      *)
(* ------------------------------------------------------------------------ *)

module DISK :
sig
  val cache_log : pid:prop_id -> model:Model.t -> prover:prover -> result:result -> string
  val file_goal : pid:prop_id -> model:Model.t -> prover:prover -> string
  val file_logout : pid:prop_id -> model:Model.t -> prover:prover -> string
  val file_logerr : pid:prop_id -> model:Model.t -> prover:prover -> string
end

module GOAL :
sig
  type t
  open Lang
  val is_trivial : t -> bool
  val make : Hypotheses.t -> F.pred -> t
  val proof : t -> F.pred
  val descr : t -> Hypotheses.t * F.pred
end

module VC_Lemma :
sig

   type t = {
    model : Model.t ;
    lemma : Definitions.dlemma ;
    depends : logic_lemma list ; 
    (* list of axioms and lemma on which the proof depends on *)
  }

  val is_trivial : t -> bool
  val cache_descr : t -> (prover * result) list -> string

end

module VC_Annot :
sig

  type t = {
    model : Model.t ;
    goal : GOAL.t ;
    tags : Splitter.tag list ;
    warn : Warning.t list ;
    deps : Property.Set.t ;
    path : Stmt.Set.t ;
    effect : stmt option ;
  }

  val is_trivial : t -> bool
  val is_simplified : t -> bool

  val cache_descr : pid:prop_id -> t -> (prover * result) list -> string

end

(* ------------------------------------------------------------------------ *)
(**{1 Proof Obligations}                                                    *)
(* ------------------------------------------------------------------------ *)

type formula = 
  | Legacy of VC_Legacy.t 
  | GoalLemma of VC_Lemma.t
  | GoalAnnot of VC_Annot.t

(** Dynamically exported as ["Wpo.po"] *)
type t = {
  po_gid   : string ;  (* goal identifier *)
  po_name  : string ;  (* goal informal name *)
  po_idx   : index ;   (* goal index *)
  po_pid   : WpPropId.prop_id ; (* goal target property *)
  po_updater : Emitter.t ; (* property status updater *)
  po_formula : formula ; (* proof obligation *)
}

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
val get_model : t -> string

val get_files : t -> (string * string) list

val clear : unit -> unit

val gid : model:string -> propid:WpPropId.prop_id -> string
val add : t -> unit
val new_env : context:string -> string (** Generates a fresh environment name. *)
val release_env : env:string -> unit (** Releases the last generated environment name. *)
val set_result : t -> prover -> result -> unit

(** Dynamically exported. *)
val get_result : t -> prover -> result
val get_results : t -> (prover * result) list
val get_proof : t -> bool * Property.t

val is_trivial : t -> bool
val warnings : t -> Warning.t list

(** [true] if the result is valid. Dynamically exported.
    @since Nitrogen-20111001
*)
val is_valid: result -> bool

(** [true] if the result is meaningfull (Valid, Unknown or Timeout) *)
val is_verdict: result -> bool

val get_time: result -> float
val get_steps: result -> int

val iter :
  ?on_environment:(string -> unit) ->
  ?on_lemma:(string -> unit) ->
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

val pp_function : Format.formatter -> Kernel_function.t -> string option -> unit
val pp_goal_flow : Format.formatter -> t -> unit

(** Dynamically exported. *)
val prover_of_name : string -> prover option

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
