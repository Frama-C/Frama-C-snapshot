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

(** The extensible GUI.
    @plugin development guide *)

open Cil_types

(** This is the type of source code buffers that can react to global
    selections and highlighters.
    @since Beryllium-20090901 *)
class type reactive_buffer = object
  inherit Gtk_helper.error_manager
  method buffer : GSourceView2.source_buffer
  method locs : Pretty_source.Locs.state option
  method rehighlight : unit
  method redisplay : unit
end

(** This class type lists all the methods available to navigate the
    source code through the GUI *)
class type view_code = object

  (** {3 Pretty-printed code} *)

  method scroll : Pretty_source.localizable -> unit
    (** Move the pretty-printed source viewer to the given localizable
        if possible. Return a boolean indicating whether the operation
        succeeded

        @modify Nitrogen-20111001  Now indicates whether the
        operation succeeded. *)

  method display_globals : global list -> unit
    (** Display the given globals in the pretty-printed source viewer. *)


  (** {3 Original code} *)

  method view_original_stmt : stmt -> location
    (** Display the given [stmt] in the original source viewer *)

  method view_original : location -> unit
    (** Display the given location in the original_source_viewer *)


  (** {3 Both pretty-printed and original code} *)

  method view_stmt : stmt -> unit
    (** Display the given [stmt] in the [source_viewer] and in the
        [original_source_viewer]. Equivalent to two successive
        calls to [scroll] and [view_original_stmt]
      @since Carbon-20101201 *)

  method select_or_display_global : global -> unit
    (** This function tries to select the global in the treeview. If
        this fails, for example because the global is not shown in the
        treeview because of filters, it falls back to displaying the
        global by hand.

        @since Nitrogen-20111001 *)
end

class protected_menu_factory:
  Gtk_helper.host -> GMenu.menu -> [ GMenu.menu ] GMenu.factory

(** This is the type of extension points for the GUI.
    @modify Boron-20100401 new way of handling the menu and the toolbar
    @plugin development guide *)
class type main_window_extension_points = object
  inherit view_code

  (** {3 Main Components} *)

  method toplevel : main_window_extension_points
    (** The whole GUI aka self *)

  method menu_manager: unit -> Menu_manager.menu_manager
    (** The object managing the menubar and the toolbar.
        @since Boron-20100401 *)

  method file_tree : Filetree.t
    (** The tree containing the list of files and functions *)

  method file_tree_view : GTree.view
    (** The tree view containing the list of files and functions *)

  method main_window : GWindow.window
    (** The main window *)

  method annot_window : GText.view
    (** The information panel.
        The text is automatically cleared whenever the selection is changed.
        You should not directly use the buffer contained in the annot_window
        to add text. Use the method [pretty_information].
    *)
  method pretty_information : 'a.  ('a, Format.formatter, unit) format -> 'a
    (** Pretty print a message in the [annot_window]. *)

  method lower_notebook : GPack.notebook
    (** The lower notebook with messages tabs *)

  (** {4 Source viewers}  *)

  method source_viewer : GSourceView2.source_view
  (** The [GText.view] showing the AST. *)

  method reactive_buffer: reactive_buffer option
  (** The buffer containing the AST.
      @since Carbon-20101201 *)

  method original_source_viewer : Source_manager.t
    (** The multi-tab source file display widget containing the
        original source. *)


  (** {3 Dialog Boxes} *)

  method launcher : unit -> unit
    (** Display the analysis configuration dialog and offer the
        opportunity to launch to the user *)

  method error :
    'a. ?parent:GWindow.window_skel -> ('a, Format.formatter, unit) format -> 'a
    (** Popup a modal dialog displaying an error message *)

  (** {3 Extension Points} *)

  method register_source_selector :
    (GMenu.menu GMenu.factory
     -> main_window_extension_points
       -> button:int -> Pretty_source.localizable -> unit) -> unit
    (** register an action to perform when button is released on a given
        localizable.
        If the button 3 is released, the first argument is popped as a
        contextual menu. 
	@plugin development guide *)

  method register_source_highlighter :
    (GSourceView2.source_buffer -> Pretty_source.localizable ->
       start:int -> stop:int -> unit)
    -> unit
    (** register an highlighting function to run on a given localizable
        between start and stop in the given buffer.
        Priority of [Gtext.tags] is used to decide which tag is rendered on
        top of the other. *)

  method register_panel :
    (main_window_extension_points->(string*GObj.widget*(unit-> unit) option))
    -> unit
    (** [register_panel (name, widget, refresh)] registers a panel in GUI.
        The arguments are the name of the panel to create,
        the widget containing the panel and a function to be called on
        refresh. *)

  (** {3 General features} *)

  method reset : unit -> unit
    (** Reset the GUI and its extensions to its initial state *)

  method rehighlight : unit -> unit
    (** Force to rehilight the current displayed buffer.
        Plugins should call this method whenever they have changed the states
        on which the function given to [register_source_highlighter] have been
        updated. *)

  method redisplay : unit -> unit
    (** @since Nitrogen-20111001
        Force to redisplay the current displayed buffer.
        Plugins should call this method whenever they have changed the globals.
        For example whenever a plugin adds an annotation, the buffers need
        to be redisplayed. *)

  method protect :
    cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> unit) -> unit
    (** Lock the GUI ; run the funtion ; catch all exceptions ; Unlock GUI
        The parent window must be set if this method is not called directly
        by the main window: it will ensure that error dialogs are transient
        for the right window.

        Set cancelable to [true] if the protected action should be cancellable
        by the user through button `Stop'. *)

  method full_protect :
    'a . cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> 'a) ->
    'a option
    (** Lock the GUI ; run the funtion ; catch all exceptions ; Unlock GUI ;
        returns [f ()].
        The parent window must be set if this method is not called directly
        by the main window: it will ensure that error dialogs are transient
        for the right window.

        Set cancelable to [true] if the protected action should be cancellable
        by the user through button `Stop'. *)

  method push_info : 'a. ('a, Format.formatter, unit) format -> 'a
    (** Pretty print a temporary information in the status bar *)

  method pop_info : unit -> unit
    (** Remove last temporary information in the status bar *)

  method help_message : 'a 'b.
    (<event : GObj.event_ops ; .. > as 'a) ->
    ('b, Format.formatter, unit) format ->
    'b
    (** Help message displayed when entering the widget *)

end

class main_window : unit -> main_window_extension_points

val register_extension : (main_window_extension_points -> unit) -> unit
  (** Register an extension to the main GUI. It will be invoked at
      initialization time.
      @plugin development guide *)

val register_reset_extension : (main_window_extension_points -> unit) -> unit
  (** Register a function to be called whenever the main GUI reset method is
      called. *)

val reactive_buffer : main_window_extension_points ->
  ?parent_window:GWindow.window -> global list -> reactive_buffer
  (** This function creates a reactive buffer for the given list of globals.
      These buffers are cached and sensitive to selections and highlighters.
      @since Beryllium-20090901 *)

(** Bullets at left-margins
    @since Nitrogen-20111001 *)
module Feedback :
sig

  val mark : GSourceView2.source_buffer 
    -> start:int -> stop:int 
    -> Property_status.Feedback.t -> unit

  val update: reactive_buffer -> Property.t -> unit
end



(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
