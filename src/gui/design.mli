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

(** The extensible GUI *)
open Db_types
open Cil_types

(** This is the type of extension points for the GUI *)
class type main_window_extension_points = object
  (** Use this to add menu entries.
      The default layout to enhance is
      "<ui>
      <menubar name='MenuBar'>
       <menu action='FileMenu'>
       </menu>
      </menubar>

      <toolbar name='ToolBar'>
       <toolitem action='Load'/>
       <toolitem action='Quit'/>
      </toolbar>

      <popup name='Contextual'>
      </popup>

      </ui>"
      
      Here is an example to add a menu entry "Dummy Entry" under the File menu in object o:
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

  method toplevel : main_window_extension_points
    (** The whole GUI aka self *)

  method main_window : GWindow.window
    (** The main window *)

  method annot_window : GText.view
    (** The information pannel. 
        The text is cleared whenever the selection is changed. *)

  method source_viewer : GText.view
    (** The [GText.view] showing the AST. *)

  method display_globals : global list -> GText.buffer
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
        contextual menu. *)

  method register_source_highlighter : 
    (GText.buffer -> Pretty_source.localizable -> start:int -> stop:int -> unit) -> unit
    (** register an highlighting function to run on a given localizable 
        between start and stop in the given buffer. 
        Priority of [Gtext.tags] is used to decide which tag is rendered on 
        top of the other. *)

  method highlight : 
    scroll:bool -> GText.tag -> Pretty_source.localizable -> unit
    (** Manually Highlight [ki] with then given [tag] in source_viewer#buffer *)


  (** The original source viewer.  *)

  method original_source_viewer : Source_manager.t
    (** The multi-tab source file display widget. *)

  method view_original : location -> unit
    (** Display the given [location] in the [original_source_viewer] *)

  method view_original_stmt : stmt -> location
    (** Display the given [stmt] in the [original_source_viewer] *)

  method view_original : location -> unit
    (** Display the given [location] in the [original_source_viewer] *)



  (** General features *)
  
  method reset : unit -> unit
    (** reset the GUI and its extensions to its initial state *)

  method monospace : Pango.font_description
    (** The monospace font to be used by all plugins *)

  method general : Pango.font_description
    (** The general font to be used by all plugins *)

  method info : 'a. ('a, Format.formatter, unit) format -> 'a
    (** Pretty print a temporary information in the status bar *)
end

class main_window : unit -> main_window_extension_points

val register_extension : (main_window_extension_points -> unit) -> unit
  (** Register an extension to the main GUI. It will be invoked at
      initialization time.*)

val register_reset_extension : (main_window_extension_points -> unit) -> unit
  (** Register a function to be called whenever the main GUI reset method is
      called. *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
