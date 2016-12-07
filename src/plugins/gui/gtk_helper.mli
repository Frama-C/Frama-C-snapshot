(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

(** Generic Gtk helpers. *)

val framac_logo: GdkPixbuf.pixbuf option
(** @since Boron-20100401 *)

val framac_icon: GdkPixbuf.pixbuf option
(** @since Boron-20100401 *)

(** Some generic icon management tools.
    @since Carbon-20101201 *)
module Icon: sig

  type kind = Frama_C | Unmark
            | Custom of string
            | Feedback of Property_status.Feedback.t
  (** Generic icons available in every proper install of Frama-C.
      To be able to use [Custom s] you must have called
      [register ~name:s ~file] orelse you will get an generic icon
      placeholder. *)

  val register: name:string -> file:string -> unit
  (** [register ~name ~file] registers the kind [Custom name] associated
      to the filename [file].
      [$FRAMAC_SHARE/f] should point to an existing file containing
      an image loadable by GdkPixbuf. *)

  val get: kind -> GdkPixbuf.pixbuf
  (** @return the pixbuf associated to the given kind.
      If the given kind is [Custom s] and no one ever called
      [register ~name:s ~file] where [file] is such that
      [$(FRAMAC_SHARE)/f] is not a real image file loadable by GdkPixbuf,
      a generic icon placeholder is returned. *)

  val default: unit -> GdkPixbuf.pixbuf

end

(** Configuration module for the GUI: all magic visual constants should
    use this mechanism (window width, ratios, ...).
    @since Carbon-20101201 *)
module Configuration: sig
  type configData =
      ConfInt of int
    | ConfBool of bool
    | ConfFloat of float
    | ConfString of string
    | ConfList of configData list

  val load : unit -> unit
  val save : unit -> unit

  val set : string -> configData -> unit
  (** Set a configuration element, with a key. Overwrites the previous values *)

  val find: string -> configData
  (** Find a configuration elements, given a key. Raises Not_found if it cannot
      find it *)

  val find_int: ?default:int -> string -> int
  (** Like find but extracts the integer.
      Raises Not_found if the key is found but is not an integer.
      Raises Not_found if no default is given and the key is not found.
      If a default is given and the key is not found then the default value
      is stored for the given key and returned. *)

  val use_int: string -> (int -> unit) -> unit
  (** Looks for an integer configuration element, and if it is found, it is
      given to the given function. Otherwise, does nothing *)

  val set_int: string -> int -> unit
  (** Sets a ConfigInt *)
  
  val find_bool : ?default:bool -> string -> bool
  (** Same as {find_int}. *)

  val use_bool: string -> (bool -> unit) -> unit
  (** Same as {!use_int}. *)

  val set_bool: string -> bool -> unit
  (** Sets a ConfigBool *)

  val find_float : ?default:float -> string -> float
  (** Same as {!find_int}. *)

  val use_float: string -> (float -> unit) -> unit
  (** Same as {!use_int}. *)

  val set_float: string -> float -> unit
  (** Sets a ConfigFloat *)

  val find_string: ?default:string -> string -> string
  (** Same as {!find_int}. *)

  val use_string: string -> (string -> unit) -> unit
  (** Same as {!use_int}. *)

  val find_list: string -> configData list
  val use_list: string -> (configData list -> unit) -> unit
  val set_list: string -> configData list -> unit

  (** Helpers to connect widgets to configuration values.
      The configuration value is first pushed to the widget
      using method [#set], or the [~default] value is used instead.
      
      Then, a callback is registered
      into the widget via [#connect] such that subsequent
      values from user's action are saved back into the 
      configuration file. *)

  (** Abstract interface to the connected widget. 
      This API is consistent with the [Widget] ones. *)
  class type ['a] selector =
    object
      method set : 'a -> unit
      (** Set's widget value to given one. *)
      method connect : ('a -> unit) -> unit
      (** Register a callback invoked by the widget each time the value is edited. *)
    end
    
  val config_int : key:string -> default:int -> int #selector -> unit
  val config_bool : key:string -> default:bool -> bool #selector -> unit
  val config_string : key:string -> default:string -> string #selector -> unit
  val config_values : key:string -> default:'a ->
    values:('a * string) list -> 'a #selector -> unit
  (** The [values] field is used as a dictionnary of available values. 
      They are compared with [Pervasives.(=)]. *)
  
end

(* ************************************************************************** *)
(** {2 Tags} *)
(* ************************************************************************** *)

val make_tag :
  < tag_table : Gtk.text_tag_table;
    create_tag : ?name:string -> GText.tag_property list -> GText.tag ; .. >
  -> name:string -> GText.tag_property list -> GText.tag

val apply_tag : GSourceView2.source_buffer -> GText.tag -> int -> int -> unit
val remove_tag : GSourceView2.source_buffer -> GText.tag -> int -> int -> unit
val cleanup_tag : GSourceView2.source_buffer -> GText.tag -> unit

val cleanup_all_tags : GSourceView2.source_buffer -> unit

(* ************************************************************************** *)
(** {2 Channels} *)
(* ************************************************************************** *)

val make_formatter: ?flush:(unit -> unit) -> #GText.buffer -> Format.formatter
(** Build a formatter that redirects its output to the given buffer.
    [flush] is called whenever the formatter is flushed. *)

val channel_redirector:  Unix.file_descr -> (string -> bool) -> unit
(** Redirects all strings written to the file descriptor
    and call the given function on each. *)

val log_redirector: ?flush:(unit->unit) -> (string -> unit) -> unit
(** Redirects all strings written to the terminal and call the given function
    on each. *)

val redirect : Format.formatter -> #GText.buffer -> unit
(** Redirect the given formatter to the given buffer *)

(* ************************************************************************** *)
(** {2 Asynchronous command execution} *)
(* ************************************************************************** *)

val spawn_command:
  ?timeout:int ->
  ?stdout:Buffer.t ->
  ?stderr:Buffer.t ->
  string -> string array ->
  (Unix.process_status -> unit) ->
  unit
(** Launches the given command and calls the given
    function when the process terminates.
    If timeout is > 0 (the default) then the process will be killed if it does
    not end before timeout seconds.
    In this case the returned process status will be
    [Unix.WSIGNALED Sys.sigalrm]. *)

(* ************************************************************************** *)
(** {2 Locks} *)
(* ************************************************************************** *)

val gui_unlocked: bool ref
(** This is a mutex you may use to prevent running some code while the GUI
    is locked. *)

val register_locking_machinery:
  ?lock_last:bool -> lock:(bool -> unit) -> unlock:(unit -> unit) -> unit ->
  unit
(** Add hooks to the locking mechanism of the GUI.
    [lock_last] must be set to true if [lock] must be executed after all the
    others locking actions and [unlock] must be executed before all the others
    unlocking actions. Default is [false]. At least one "lock_last" action is
    allowed.
    @since Beryllium-20090901
    @modify Boron-20100401 new optional argument [lock_last] and new
    argument [()] *)

(* ************************************************************************** *)
(** 2 Tooltips *)
(* ************************************************************************** *)

val do_tooltip: ?tooltip:string -> < coerce: GObj.widget; .. > -> unit
(** Add the given tooltip to the given widget.
    It has no effect if no tooltip is given. *)

(* ************************************************************************** *)
(** {2 Chooser} *)
(* ************************************************************************** *)

type 'a chooser =
  GPack.box -> string -> (unit -> 'a) -> ('a -> unit) -> (unit -> unit)
(** The created widget is packed in the box.
    The two following functions are supposed to be accessors(get and set)
    for the value to be displayed.
    The returned closure may be called to resynchronize the value in the widget
    from the get function. *)

val on_bool: ?tooltip:string -> ?use_markup:bool -> bool chooser
(** Pack a check button *)

val range_selector:
  ?tooltip:string ->
  ?use_markup:bool ->
  GPack.box ->
  label:string ->
  lower:int -> upper:int -> (int -> unit) -> (unit -> int) -> unit -> unit

val on_int:
  ?tooltip:string -> ?use_markup:bool -> ?lower:int -> ?upper:int ->
  ?sensitive:(unit -> bool) -> ?width:int ->
  int chooser
(** Pack a spin button.
    By default, sensitivity is set to true when this function is called. *)

val on_string:
  ?tooltip:string -> ?use_markup:bool -> ?validator:(string -> bool) ->
  ?width:int
  -> string chooser
(** Pack a string chooser *)

val on_string_set: ?tooltip:string -> ?use_markup:bool -> ?width:int
  -> string chooser
(** Pack a string-set chooser *)

val on_string_completion:
  ?tooltip:string -> ?use_markup:bool -> ?validator:(string -> bool)
  -> string list -> string chooser

val on_combo:
  string list -> ?tooltip:string -> ?use_markup:bool -> ?width:int
  -> string chooser
(** Pack a string-selector *)

(* ************************************************************************** *)
(** {2 Error manager} *)
(* ************************************************************************** *)

(**  A utility class to catch exceptions and report proper error messages.
     See the documentations of the methods in
     {!Design.main_window_extension_points}, which inherits from this
     class. *)
class type host = object
  method error:
    'a. ?parent:GWindow.window_skel -> ?reset:bool -> ('a, Format.formatter, unit) format -> 'a
  method full_protect :
    'a. cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> 'a) ->
    'a option
  method protect :
    cancelable:bool -> ?parent:GWindow.window_skel -> (unit -> unit) -> unit
  method private set_reset: (unit -> unit) -> unit
end

(**  A utility class to catch exceptions and report proper error messages.
     The error dialog will be transient for the [GWindow.window_skel] argument.
     @since Beryllium-20090901 *)
class error_manager : ?reset:(unit -> unit) -> GWindow.window_skel -> host

(* ************************************************************************** *)
(** {2 Source files chooser} *)
(* ************************************************************************** *)

(** @since Boron-20100401 *)
class type source_files_chooser_host = object
  inherit host
  method main_window: GWindow.window_skel
  method reset: unit -> unit
end

(** Open a dialog box for choosing C source files and performing an action on
    them.
    @since Boron-20100401 *)
val source_files_chooser:
  source_files_chooser_host ->
  string list (** list of default selected files *) ->
  (string list -> unit) ->
  unit

(* ************************************************************************** *)
(** {2 Miscellaneous} *)
(* ************************************************************************** *)

val refresh_gui: unit -> unit
(** Process some pending events in the main Glib loop.
    This is intended to be called only when [!gui_unlocked == false].
    @since Beryllium-20090901 *)

val string_selector: string list -> (GObj.widget -> unit) -> GEdit.entry

val expand_to_path : GTree.view -> Gtk.tree_path -> unit

val make_string_list: packing:(GObj.widget -> unit)
  -> (string -> unit)* (unit -> unit)*(unit -> string list)
(** @return (add, remove_selected, get_elements) *)

val place_paned: GPack.paned -> float -> unit
(** Sets the position of the paned widget to the given ratio *)

val save_paned_ratio: string -> GPack.paned -> unit
(** Saves the current ratio of the panel associated to the given key. *)

val old_gtk_compat: ('a -> unit) -> 'a -> unit
(** Catch exception [Not_found] and do nothing *)

val trace_event: GObj.event_ops -> unit
(** Trace all events on stderr for the given object.
    This is a debugging function: it should not be called during normal
    execution. *)

val make_text_page:
  ?pos:int -> GPack.notebook -> string -> (GPack.notebook -> unit) * GText.view
(** Insert a GText.view in a new page of the notebook with the given title,
    at position [pos] if specified, or last if not.
    It returns a new GText.view together with a function to reparent the
    inserted page in another notebook.
    The tab label of the created page will be highlighted whenever its
    contents changes.
    @since Beryllium-20090901 *)

val open_in_external_viewer : ?line:int -> string -> unit
(** Opens [file] in an external viewer, optionally centered on line [line]
    (if supported by the viewer). The viewer is executed in the
    background (i.e. it does not block the execution of Frama-C).
    The external viewer is given by the environment variable [EDITOR].
    If undefined, a default viewer is used.
    @since Magnesium-20151001 *)

(** A functor to build custom Gtk lists.
    You'll probably prefer to use the highlevel custom models in
    the next module named Custom.List.
    It may be part of a future lablgtk release.
    Do not change anything without changing lablgtk svn.*)
module MAKE_CUSTOM_LIST(A : sig type t end)
  : sig
    type custom_list = { finfo : A.t; fidx : int; }
    val inbound : int -> 'a array -> bool
    class custom_list_class : GTree.column_list ->
      object
        inherit
          [custom_list,custom_list,unit,unit] GTree.custom_tree_model
        method custom_decode_iter :
          custom_list -> unit -> unit -> custom_list
        method custom_encode_iter :
          custom_list -> custom_list * unit * unit
        method custom_get_iter : Gtk.tree_path -> custom_list option
        method custom_get_path : custom_list -> Gtk.tree_path
        method custom_iter_children :
          custom_list option -> custom_list option
        method custom_iter_has_child : custom_list -> bool
        method custom_iter_n_children : custom_list option -> int
        method custom_iter_next : custom_list -> custom_list option
        method custom_iter_nth_child :
          custom_list option -> int -> custom_list option
        method custom_iter_parent : custom_list -> custom_list option
        method custom_value :
          Gobject.g_type -> custom_list -> column:int -> Gobject.basic
        method insert : A.t -> unit
        method clear : unit -> unit
      end
    val custom_list : unit -> custom_list_class
    val make_view_column :
      custom_list_class -> ('b,'a) #GTree.cell_renderer_skel ->
      (A.t -> 'a list) ->
      title:string ->
      GTree.view_column
  end

(** Create a new window displaying a graph.
    @plugin development guide *)
val graph_window:
  parent: GWindow.window ->
  title:string ->
  (packing:(GObj.widget -> unit) -> unit ->
   <adapt_zoom: unit -> unit; ..>) ->
  unit

(** Create a new window displaying a graph, by printing dot commands. *)
val graph_window_through_dot:
  parent: GWindow.window ->
  title:string ->
  (Format.formatter -> unit) ->
  unit

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
