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

(** This module creates and manages the "Values" panel on the lower notebook
    of the GUI. It mainly displays the values computed by the analysis
    for selected expressions and lvalues, and thus depends on the value
    abstractions used for the analysis.
    It is able to display different values by callstacks in a table,
    in accordance with the callstacks focused by the user (handled in
    gui_callstacks_filters.ml). *)

open Gui_types

(* List associating callstacks and data related to them.
   Each element of such a list is intended to be displayed in a separate row. *)
type 'v data_by_callstack =
  (gui_callstack * 'v Gui_eval.gui_selection_data) list

(* Type of the function that displays some data by callstacks for a selection
   (expression, lvalue or term) at a location (statement, precondition or
   postcondition of a function).
   This is the main feature of the "Values" panel, returned by the [create]
   function below. *)
type 'v display_data_by_callstack =
  gui_loc -> gui_selection -> 'v data_by_callstack -> unit

(* Module needed to create the panel. *)
module type Input = sig
  include Gui_types.S

  val make_data_for_lvalue :
    Cil_types.lval -> gui_loc -> value data_by_callstack
end

(** Creates the panel, attaches it to the lower notebook, and returns the
    display_by_callstack function allowing to display data on it.
    If a previous panel was previously created through this function, the new
    panel replaces it. *)
val create:
  Design.main_window_extension_points ->
  (module Input with type value = 'v) ->
  'v display_data_by_callstack

(* Should be called when the main_ui is reset. *)
val reset: unit -> unit

(* Clear the 'default' tab of the panel, for example on selection change. *)
val clear_default: unit -> unit

(* Set focus on the 'Selection' tab of the panel. *)
val focus_selection_tab: unit -> unit
