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

(** This module defines an abstraction to evaluate various things across
    multiple callstacks. Currently, l-values, NULL, expressions, term-lvalues,
    terms and predicates can be evaluated *)

open Cil_types
open Gui_types

(** Catch the fact that we are in a function for which [-no-results] or one
    of its variants is set. Without this check, we would display
    much non-sensical information. *)
val results_kf_computed: kernel_function -> bool

(** State in which the predicate, found in the given function,
    should be evaluated *)
val classify_pre_post: kernel_function -> Property.t -> gui_loc option

val gui_loc_logic_env: gui_loc -> Logic_typing.Lenv.t
(** Logic labels valid at the given location. C labels are _not_ added,
    even if the location is a statement. *)

type 'a gui_selection_data = {
  alarm: bool;
  before: 'a gui_res;
  before_string: string Lazy.t;
  after: 'a gui_after;
  after_string: string Lazy.t;
}

val gui_selection_data_empty: 'a gui_selection_data
(** Default value. All the fields contain empty or dummy values *)

(** The types and function below depend on the abstract domains and values
    currently available in EVA. *)
module type S = sig

  module Analysis : Analysis.S

  (** This is the record that encapsulates all evaluation functions *)
  type ('env, 'expr, 'v) evaluation_functions = {
    eval_and_warn: 'env -> 'expr -> 'v * bool;
    env: Analysis.Dom.t -> Value_types.callstack -> 'env;
    equal: 'v -> 'v -> bool;
    bottom: 'v;
    join: 'v -> 'v -> 'v;
    expr_to_gui_selection: 'expr -> gui_selection;
    res_to_gui_res: 'expr -> 'v -> Analysis.Val.t gui_res;
  }

  val lval_as_offsm_ev:
    (Analysis.Dom.t, lval, gui_offsetmap_res) evaluation_functions

  val lval_zone_ev:
    (Analysis.Dom.t, lval, Locations.Zone.t) evaluation_functions

  val null_ev:
    (Analysis.Dom.t, unit, gui_offsetmap_res) evaluation_functions

  val exp_ev:
    (Analysis.Dom.t, exp, Analysis.Val.t Bottom.or_bottom) evaluation_functions

  val lval_ev:
    (Analysis.Dom.t, lval, Analysis.Val.t Eval.flagged_value) evaluation_functions

  (** Evaluation of logic-originating objects is parameterized by a location
      information, which is used to build the evaluation environment *)

  val tlval_ev:
    gui_loc ->
    (Eval_terms.eval_env, term, gui_offsetmap_res) evaluation_functions

  val tlval_zone_ev:
    gui_loc ->
    (Eval_terms.eval_env, term, Locations.Zone.t) evaluation_functions

  val term_ev:
    gui_loc ->
    (Eval_terms.eval_env, term, Analysis.Val.t Bottom.or_bottom) evaluation_functions

  val predicate_ev:
    gui_loc ->
    (Eval_terms.eval_env,
     predicate,
     Eval_terms.predicate_status Bottom.or_bottom
    ) evaluation_functions

  val make_data_all_callstacks:
    ('a, 'b, 'c) evaluation_functions -> gui_loc -> 'b ->
    (gui_callstack * Analysis.Val.t gui_selection_data) list * exn list
end

module Make (X: Analysis.S) : S with module Analysis = X
