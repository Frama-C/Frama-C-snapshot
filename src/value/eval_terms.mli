(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

(** Evaluation of terms and predicates *)

open Cil_types
open Locations
open Cvalue

(** Evaluating a predicate. [Unknown] is the Top of the lattice *)
type predicate_status = True | False | Unknown
val pretty_predicate_status : Format.formatter -> predicate_status -> unit

val join_predicate_status :
  predicate_status -> predicate_status -> predicate_status
val join_list_predicate_status :
  predicate_status list -> predicate_status


(** Error during the evaluation of a term or a predicate *)
type logic_evaluation_error =
  | Unsupported of string
  | UnsupportedLogicVar of logic_var
  | AstError of string
  | NoEnv of logic_label
  | NoResult
  | CAlarm
val pretty_logic_evaluation_error :
  Format.formatter -> logic_evaluation_error -> unit

exception LogicEvalError of logic_evaluation_error

type labels_states = Cvalue.Model.t Cil_datatype.Logic_label.Map.t

(** Evaluation environment. Currently available are function Pre and Post, or
    the environment to evaluate an annotation *)
type eval_env
val env_pre_f :
  ?c_labels:labels_states -> init:Model.t -> unit -> eval_env
val env_annot :
  ?c_labels:labels_states -> pre:Model.t -> here:Model.t -> unit -> eval_env
val env_post_f :
  ?c_labels:labels_states -> pre:Model.t -> post:Model.t ->
  result:varinfo option -> unit -> eval_env
val env_assigns: init:Model.t -> eval_env
val env_here: Model.t -> eval_env

(** Dependencies needed to evaluate a term or a predicate *)
type logic_deps = Zone.t Cil_datatype.Logic_label.Map.t

type 'a eval_result = {
  etype: Cil_types.typ;
  evalue: 'a list;
  ldeps: logic_deps;
}

val eval_term :
  with_alarms:CilE.warn_mode ->
  eval_env -> term -> V.t eval_result

val eval_tlval :
  with_alarms:CilE.warn_mode ->
  eval_env -> term -> Location_Bits.t eval_result

val eval_tlval_as_location :
  with_alarms:CilE.warn_mode ->
  eval_env -> term -> location

val eval_tlval_as_locations :
  with_alarms:CilE.warn_mode ->
  eval_env -> term -> location list * logic_deps

val eval_tlval_as_zone :
  with_alarms:CilE.warn_mode ->
  for_writing:bool -> eval_env -> term -> Zone.t

exception Not_an_exact_loc
val eval_term_as_exact_loc :
  with_alarms:CilE.warn_mode ->
  eval_env -> term -> Cil_datatype.Typ.t * location

val eval_predicate :
  eval_env -> predicate named -> predicate_status

val predicate_deps: eval_env -> predicate named -> logic_deps

val reduce_by_predicate :
  eval_env -> bool -> predicate named -> eval_env

(** If [reduce] is true, reduce in all cases. Otherwise, reduce only
    when [p] is a disjunction, ie. split by this disjunction.
    The Property is the one in which is [p]. *)
val split_disjunction_and_reduce :
  reduce:bool ->
  env:eval_env ->
  (Cvalue.Model.t * Trace.t) ->
  slevel:int ->
  predicate named ->
  Property.t ->
  State_set.t
