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

(** The source viewer multi-tabs widget window. *)

type t

val selection_locked : bool ref
(** Prevents the filetree callback from resetting the selected line when it
    was selected via a click in the original source viewer. *)

val make: 
  ?tab_pos:Gtk.Tags.position -> ?packing:(GObj.widget -> unit) -> unit -> t

val load_file: 
  t -> ?title:string -> filename:string -> ?line:int ->
  click_cb:(Pretty_source.localizable option -> unit) -> unit -> unit
  (** If [line] is 0 then the last line of the text is shown.
      If [line] is less that 0 then no scrolling occurs (default).
      If [title] is not provided the page title is the filename.
      [click_cb] is a callback called whenever the user clicks on the
      original source code. This callback is given the localizable that the
      user clicked on, if any was found. This localizable is estimated from
      a reverse mapping from the original source to the Cil source, and not
      always exact. *)

val select_file: t -> string -> unit (** Selection by page filename *)
val select_name: t -> string -> unit (** Selection by page title *)
val get_current_source_view : t -> GSourceView2.source_view
(** Returns the source viewer for the currently displayed tab *)

val clear : t -> unit
  (** Remove all pages added by [load_file] *)
