(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** {1 Source code navigation history.} 
    @since Nitrogen-20111001 *)

type history_elt =
  | Global of Cil_types.global
  | Localizable of Pretty_source.localizable

val is_empty: unit -> bool
(** Does the history contain an event. *)

val can_go_back: unit -> bool
(** Are there past events in the history. *)

val can_go_forward: unit -> bool
(** Are there events to redo in the history. *)

val back: unit -> unit
(** If possible, go back one step in the history. *)

val forward: unit -> unit
(** If possible (ie. if [back] has been called), go forward one step
    in the history. *)

val push: history_elt -> unit
(** Add the element to the current history; clears the forward history,
    and push the old current element to the past history. *)

val set_forward: history_elt list -> unit
(** Replaces the forward history with the given elements. *)

val get_current: unit -> history_elt option
(** return the current history point, if available
    @since Sodium-20150201
*)

val show_current: unit -> unit
(** Redisplay the current history point, if available. Useful to
    refresh the gui. *)

val on_current_history: unit -> ((unit -> unit) -> unit)
(** [on_current_history ()] returns a closure [at] such that [at f]
    will execute [f] in a context in which the history will be the
    one relevant when [on_current_history] was executed. *)

val selected_localizable: unit -> Pretty_source.localizable option
(** [selected_localizable ()] returns the localizable currently
    selected, or [None] if nothing or an entire global is selected. *)

val translate_history_elt: history_elt -> history_elt option
(** try to translate the history_elt of one project to the current one
    @since Sodium-20150201
*)

(**/**)
val set_display_elt_callback: (history_elt -> unit) -> unit
val create_buttons: Menu_manager.menu_manager -> Menu_manager.item array

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
