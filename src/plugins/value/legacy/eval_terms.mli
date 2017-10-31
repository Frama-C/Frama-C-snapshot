(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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
(* val join_list_predicate_status :
    predicate_status list -> predicate_status *)


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

val make_env: Model.t Abstract_domain.logic_environment -> Model.t -> eval_env

val env_pre_f : pre:Model.t -> unit -> eval_env
val env_annot :
  ?c_labels:labels_states -> pre:Model.t -> here:Model.t -> unit -> eval_env
val env_post_f :
  ?c_labels:labels_states -> pre:Model.t -> post:Model.t ->
  result:varinfo option -> unit -> eval_env
val env_assigns: pre:Model.t -> eval_env

(** Used by auxiliary plugins, that do not supply the other states *)
val env_only_here: Model.t -> eval_env

val env_current_state: eval_env -> Model.t

(** Dependencies needed to evaluate a term or a predicate *)
type logic_deps = Zone.t Cil_datatype.Logic_label.Map.t

(** Three modes to handle the alarms when evaluating a logical term. *)
type alarm_mode =
  | Ignore             (* Ignores all alarms. *)
  | Fail               (* Raises a LogicEvalError when an alarm is encountered. *)
  | Track of bool ref  (* Tracks the possibility of an alarm in the boolean: the
                          boolean is set to true if an alarm is encountered. *)

(** Return a pair of (under-approximating, over-approximating) zones. *)
val eval_tlval_as_zone_under_over:
  alarm_mode:alarm_mode ->
  for_writing:bool -> eval_env -> term -> Zone.t * Zone.t

(* ML: Should not be exported. *)
type 'a eval_result = {
  etype: Cil_types.typ;
  eunder: 'a;
  eover: 'a;
  ldeps: logic_deps;
}


val eval_term :
  alarm_mode:alarm_mode ->
  eval_env -> term -> V.t eval_result

val eval_tlval_as_location :
  alarm_mode:alarm_mode ->
  eval_env -> term -> location

val eval_tlval_as_zone :
  alarm_mode:alarm_mode ->
  for_writing:bool -> eval_env -> term -> Zone.t

val eval_predicate :
  eval_env -> predicate -> predicate_status

val predicate_deps: eval_env -> predicate -> logic_deps

val reduce_by_predicate :
  eval_env -> bool -> predicate -> eval_env
