(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

module Pango : sig
  val set_small_font : #GObj.widget -> unit (** makes the font smaller. *)
  val set_bold_font : #GObj.widget -> unit (** makes the font bold. *)
end

val get_toolbar_index: GButton.toolbar -> GButton.tool_item -> int

val window:
  ?kind:Gtk.Tags.window_type ->
  ?title:string ->
  ?decorated:bool ->
  ?deletable:bool ->
  ?focus_on_map:bool ->
  ?icon:GdkPixbuf.pixbuf ->
  ?icon_name:string ->
  ?modal:bool ->
  ?position:Gtk.Tags.window_position ->
  ?resizable:bool ->
  ?screen:Gdk.screen ->
  ?type_hint:Gdk.Tags.window_type_hint ->
  ?urgency_hint:bool ->
  ?wmclass:(string * string) ->
  ?border_width:int ->
  ?width:int -> ?height:int -> ?show:bool -> unit -> GWindow.window
