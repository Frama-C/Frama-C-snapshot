(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
  | Toolbar of GtkStock.id * string (** Tooltip *)
  | Menubar of
      GtkStock.id option (** Stock used for the icon *) * string (** Label *)
  | ToolMenubar of GtkStock.id * string (** Label and tooltip *)

(** @since Boron-20100401 *)
type entry = where * (unit -> unit) (** callback *)

(** The item type corresponding to an entry.
    @since Boron-20100401 *)
class type item = object

  method menu_item: GMenu.menu_item option
    (** @since Boron-20100401 *)

  method tool_button: GButton.tool_button option
    (** @since Boron-20100401 *)

  method add_accelerator: Gdk.Tags.modifier -> char -> unit
    (** Add an accelerator iff there is a menu item.
	@since Boron-20100401 *)

end

(** @since Boron-20100401 *)
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

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
