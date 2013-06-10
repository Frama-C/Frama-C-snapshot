(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

open Cil_types
open Locations
open Cvalue

(** Evaluating a predicate. [Unknown] is the Top of the lattice *)
type predicate_value = True | False | Unknown
val pretty_predicate_value : Format.formatter -> predicate_value -> unit

val join_predicate : predicate_value -> predicate_value -> predicate_value

val fold_join_predicate :
  ((predicate_value option -> 'a -> predicate_value option) ->
   'b option -> 'c -> predicate_value option) ->
  ('a -> predicate_value) -> 'c -> predicate_value


(** Error during the evaluation of a term or a predicate *)
type logic_evaluation_error =
  | Unsupported of string
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

(** Dependencies needed to evaluate a term or a predicate *)
type edeps = Zone.t Cil_datatype.Logic_label.Map.t

type 'a eval_result = {
  etype: Cil_types.typ;
  evalue: 'a list;
  edeps: edeps;
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
  eval_env -> term -> location list * edeps

val eval_tlval_as_zone :
  with_alarms:CilE.warn_mode ->
  for_writing:bool -> eval_env -> term -> Zone.t

exception Not_an_exact_loc
val eval_term_as_exact_loc :
  with_alarms:CilE.warn_mode ->
  eval_env -> term -> Cil_datatype.Typ.t * location

val eval_predicate :
  eval_env -> predicate named -> predicate_value

val reduce_by_predicate :
  eval_env -> bool -> predicate named -> eval_env

val reduce_by_disjunction :
  always:bool ->
  env:eval_env ->
  State_set.t ->
  int ->
  predicate named ->
  State_set.t
