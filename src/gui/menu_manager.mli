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

(** Handle the menubar and the toolbar.
    @since Boron-20100401 *)

(** Where to put a new entry.
    @since Boron-20100401 *)
type where =
  | Toolbar of GtkStock.id * string * string (** Label then tooltip *)
  | Menubar of
      GtkStock.id option (** Stock used for the icon *) * string (** Label *)
  | ToolMenubar of GtkStock.id * string * string (** Label then tooltip *)

(** Callback for the buttons that can be in the menus. Standard buttons/menus
    have a callback with no argument. Buttons/menus with states are displayed
    with checkboxes in menus, or as toggle buttons in toolbars. They receive the
    after-click state as argument. The state of the button with the second
    argument of [Bool_callback]. Currently checks menus cannot have images in
    Gtk, sor the [GtkStock.id] fields of [where] are ignored.

    @since Nitrogen-20111001 *)
type callback_state =
  | Unit_callback of (unit -> unit)
  | Bool_callback of (bool -> unit) * (unit -> bool)

(** @since Boron-20100401
    @modify Nitrogen-20111001 *)
type entry = private {
  e_where: where;
  e_callback: callback_state (** callback called when the button is clicked *);
  e_sensitive: unit -> bool (** should the button be activated when the gui
                              is refreshed *);
}

(** {2 Smart constructors for menu entries.}

    If not supplied, the [active] parameter is the function that always returns
    [true].
    @since Nitrogen-20111001 *)

val toolbar:
 ?sensitive:(unit -> bool) ->
  icon:GtkStock.id ->
  label:string ->
  ?tooltip:string ->
  callback_state ->
  entry

val menubar:
 ?sensitive:(unit -> bool) -> ?icon:GtkStock.id -> string -> callback_state ->
  entry

val toolmenubar:
 ?sensitive:(unit -> bool) ->
  icon:GtkStock.id ->
  label:string ->
  ?tooltip:string ->
  callback_state ->
  entry

(** The item type corresponding to an entry.
    @since Boron-20100401 *)
class type item = object

  method menu_item: GMenu.menu_item option
    (** @since Boron-20100401 *)

  method check_menu_item: GMenu.check_menu_item option
    (** @since Nitrogen-20111001 *)

  method menu_item_skel: GMenu.menu_item_skel option
    (** @since Nitrogen-20111001 *)

  method menu: GMenu.menu option
    (** Return the menu in which the item has been inserted, if meaningful
        @since Nitrogen-20111001 *)

  method add_accelerator: Gdk.Tags.modifier -> char -> unit
    (** Add an accelerator iff there is a menu item.
        @since Boron-20100401 *)


  method tool_button: GButton.tool_button option
    (** @since Boron-20100401 *)

  method toggle_tool_button: GButton.toggle_tool_button option
    (** @since Nitrogen-20111001 *)

  method tool_button_skel: GButton.tool_button_skel option
    (** @since Nitrogen-20111001 *)

end

(** How to handle a Frama-C menu. 
    @since Boron-20100401 *)
class menu_manager: ?packing:(GObj.widget -> unit) -> host:Gtk_helper.host ->
object

  (** {2 API for plug-ins} *)

  method add_plugin: ?title:string -> entry list -> item array
    (** Add entries dedicated to a plug-in.
        If [title] is specified, then the entries are added in a dedicated
        sub-menu of name [title].
        The elements of the returned array are in the same order that the ones
        in the input list.
        @since Boron-20100401 *)

  method add_debug:
    ?title:string -> ?show:(unit -> bool) -> entry list -> item array
    (** Add entries to the menu dedicated to debugging tools.
        If [title] is specified, then the entries are added in a dedicated
        sub-menu of name [title].
        If [show] is specified, then the entries are only shown when this
        function returns [true] (it returns [true] by default).
        The elements of the returned array are in the same order that the ones
        in the input list.
        @since Boron-20100401 *)

  (** {2 High-level API} *)

  method add_menu: ?pos:int -> string -> GMenu.menu_item * GMenu.menu
    (** @since Boron-20100401 *)

  method add_entries:
    ?title:string -> ?pos:int -> GMenu.menu -> entry list -> item array
    (** Add entries in the given menu. If [title] is specified, then the
        entries are added in a dedicated sub-menu of name [title].
        The elements of the returned array are in the same order that the ones
        in the input list.
        @since Boron-20100401 *)

  method set_sensitive: bool -> unit
    (** Set the sensitive property of all the entries.
        @since Boron-20100401 *)

  (** {2 Low-level API} *)

  method factory: GMenu.menu_shell GMenu.factory
    (** @since Boron-20100401 *)

  method menubar: GMenu.menu_shell
    (** @since Boron-20100401 *)

  method toolbar: GButton.toolbar
    (** @since Boron-20100401 *)

  method refresh: unit -> unit
    (** Reset the activation state of the buttons
        @since Nitrogen-20111001 *)
end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
