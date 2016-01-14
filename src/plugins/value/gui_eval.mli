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

(** This module defines an abstraction to evaluate various things across
    multiple callstacks. Currently, l-values, NULL, expressions, term-lvalues,
    terms and predicates can be evaluated *)

open Cil_types
open Gui_types

(** Catch the fact that we are in a function for which [-no-results] or one
   of its variants is set. Without this check, we would display 
   much non-sensical information. *)
val results_kf_computed: kernel_function -> bool

(** This is the record that encapsulates all evaluation functions *)
type ('env, 'expr, 'v) evaluation_functions = {
  eval_and_warn: 'env -> 'expr -> 'v * bool;
  env: Cvalue.Model.t -> Value_types.callstack -> 'env;
  equal: 'v -> 'v -> bool;
  bottom: 'v;
  join: 'v -> 'v -> 'v;
  expr_to_gui_selection: 'expr -> gui_selection;
  res_to_gui_res: 'expr -> 'v -> gui_res;
}

val lval_ev:
  (Cvalue.Model.t, lval, Gui_types.gui_offsetmap_res) evaluation_functions

val lval_zone_ev:
  (Cvalue.Model.t, lval, Locations.Zone.t) evaluation_functions

val null_ev:
  (Cvalue.Model.t, unit, Gui_types.gui_offsetmap_res) evaluation_functions

val exp_ev:
  (Cvalue.Model.t, exp, Cvalue.V.t) evaluation_functions

(** Evaluation of logic-originating objects is parameterized by a location
    information, which is used to build the evaluation environment *)

val tlval_ev:
  Gui_types.gui_loc ->
  (Eval_terms.eval_env, term, Gui_types.gui_offsetmap_res) evaluation_functions

val tlval_zone_ev:
  Gui_types.gui_loc ->
  (Eval_terms.eval_env, term, Locations.Zone.t) evaluation_functions

val term_ev:
  Gui_types.gui_loc ->
  (Eval_terms.eval_env, term, Cvalue.V.t) evaluation_functions

val predicate_ev:
  Gui_types.gui_loc ->
  (Eval_terms.eval_env,
   predicate named,
   Eval_terms.predicate_status Abstract_interp.Bot.or_bottom
  ) evaluation_functions


type gui_selection_data = {
  alarm: bool;
  before: gui_res;
  before_string: string Lazy.t;
  after: gui_after;
  after_string: string Lazy.t;
}

val gui_selection_data_empty: gui_selection_data
(** Default value. All the fields contain empty or dummy values *)

val make_data_all_callstacks:
  (Gui_types.gui_selection -> Gui_types.gui_callstack -> gui_selection_data ->
   unit) ->
  ('a, 'b, 'c) evaluation_functions ->
  before:Cvalue.Model.t Value_types.Callstack.Hashtbl.t ->
  after:Cvalue.Model.t Value_types.Callstack.Hashtbl.t option ->
  'b ->
  exn list


(** State in which the predicate, found in the given function,
    should be evaluated *)
val classify_pre_post: kernel_function -> Property.t -> gui_loc option


(** Maps from callstacks to Value states before and after a GUI location.
    The 'after' map is not always available. *)
type states_by_callstack = {
  states_before: Cvalue.Model.t Value_types.Callstack.Hashtbl.t;
  states_after: Cvalue.Model.t Value_types.Callstack.Hashtbl.t option;
}

val callstacks_at_gui_loc: gui_loc -> states_by_callstack option
(** For statements: results are available only if the statement is reachable.
    After states are available only for instructions.

    For pre-states: results are available only if the function is called,
    and correspond to the states before reduction by any precondition.
    After states are not available

    For post-states: results are available only for functions with a body, for
    normal termination, and only when the function is called.
    After states are not available. *)

val gui_loc_logic_env: gui_loc -> Logic_typing.Lenv.t
(** Logic labels valid at the given location. C labels are _not_ added,
    even if the location is a statement. *)
