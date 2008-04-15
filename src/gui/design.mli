(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: design.mli,v 1.37 2008/11/18 12:13:41 uid568 Exp $ *)

(** The extensible GUI.
    @plugin development guide *)

open Db_types
open Cil_types

(** This is the type of extension points for the GUI.
    @plugin development guide *)
class type main_window_extension_points = object
  (** Use this to add menu entries.
      The default layout to enhance is
      "<ui>
      <menubar name='MenuBar'>
       <menu action='FileMenu'></menu>
       <menu action='ViewMenu'></menu>
      </menubar>

      <toolbar name='ToolBar'>
       <toolitem action='Load'/>
       <toolitem action='Quit'/>
      </toolbar>

      <popup name='Contextual'>
      </popup>

      </ui>"
      
      Here is an example to add a menu entry "Dummy Entry" under the 
      File menu in object o:
      [
      (* First create the action associated to the entry ... *)
      GAction.add_action "DummyAction" ~label:"Dumm_y Entry" 
      ?stock ?tooltip ?accel
      ?callback 
      o#actions;
      (* ... and merge the menu entries at the right location *)
      ui_manager#add_ui_from_string 
      "<ui><menubar name='MenuBar'> <menu name='FileMenu'> 
      <menuitem action='DummyAction'/> </menu> </menubar></ui>" 
]
      
  *)
  method ui_manager : GAction.ui_manager
  method actions : GAction.action_group

  method file_tree : Filetree.t
    (** The tree containing the list of files and functions *)

  method file_tree_view : GTree.view
    (** The tree view containing the list of files and functions *)

  method toplevel : main_window_extension_points
    (** The whole GUI aka self *)

  method main_window : GWindow.window
    (** The main window *)

  method annot_window : GText.view
    (** The information pannel. 
        The text is cleared whenever the selection is changed. *)

  method source_viewer : GSourceView.source_view
    (** The [GText.view] showing the AST. 
	@plugin development guide *)

  method display_globals : global list -> GSourceView.source_buffer
    (** Display globals in a memoized buffer [b]. 
        Use [main_ui#source_viexer#set_buffer b] to display it
        in the general source view. *)

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
    (GSourceView.source_buffer -> Pretty_source.localizable -> 
       start:int -> stop:int -> unit)
    -> unit
    (** register an highlighting function to run on a given localizable 
        between start and stop in the given buffer. 
        Priority of [Gtext.tags] is used to decide which tag is rendered on 
        top of the other. 
	@plugin development guide *)

  method register_launcher :
    (unit -> (string*GObj.widget option*(unit-> unit) option*(unit-> unit) option)) -> unit
    (** [register_launcher f] registers a configurator and launcher.
        [f] shall return a 4-uple looking like
        ("Configuration panel name",
        widget containing a configuration panel,
        function to call when configurations are updated
        function to call to launch the functionality)
        
    *)

  method register_panel :
    (main_window_extension_points -> (string * GObj.widget *(unit-> unit) option)) -> unit
    (** [register_panel f] registers a panel in GUI.
        [f self] returns the name of the panel to create, 
        the widget containing the panel and a function to be called on refresh.
    *)

  method rehighlight : unit -> unit
    (** Force to rehilight the current displayed buffer. 
        Plugins should call this method whenever they have changed the states on
        which the function given to [register_source_highlighter] have been updated.
    *)

  method scroll : Pretty_source.localizable -> unit
    (** Scroll to the given localizable in the current buffer if possible. *)

  (** {3 The original source viewer}  *)

  method original_source_viewer : Source_manager.t
    (** The multi-tab source file display widget. *)

  method view_original : location -> unit
    (** Display the given [location] in the [original_source_viewer] *)

  method view_original_stmt : stmt -> location
    (** Display the given [stmt] in the [original_source_viewer] *)

  method view_original : location -> unit
    (** Display the given [location] in the [original_source_viewer] *)

  (** {3 General features} *)
  
  method reset : unit -> unit
    (** reset the GUI and its extensions to its initial state *)

  method lock : unit -> unit
    (** Block almost all user interaction. 
        Provide user feedback on the fact that something is happening *)

  method unlock : unit -> unit
    (** Unlock user interaction *)

  method monospace : Pango.font_description
    (** The monospace font to be used by all plugins *)

  method general : Pango.font_description
    (** The general font to be used by all plugins *)

  method error : 'a. ?parent:GWindow.window_skel -> ('a, Format.formatter, unit) format -> 'a
    (** Popup a modal dialog displaying an error message *)

  method push_info : 'a. ('a, Format.formatter, unit) format -> 'a
    (** Pretty print a temporary information in the status bar *)
  method pop_info : unit -> unit
    (** Remove last temporary information in the status bar *)

end

class main_window : unit -> main_window_extension_points

val register_extension : (main_window_extension_points -> unit) -> unit
  (** Register an extension to the main GUI. It will be invoked at
      initialization time.
      @plugin development guide *)

val register_reset_extension : (main_window_extension_points -> unit) -> unit
  (** Register a function to be called whenever the main GUI reset method is
      called. *)


val apply_on_selected : (Pretty_source.localizable -> unit) -> unit
  (** [apply_on_selected f] applies [f] to the currently selected 
      [Pretty_source.localizable]. Does nothing if nothing is selected. *)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
