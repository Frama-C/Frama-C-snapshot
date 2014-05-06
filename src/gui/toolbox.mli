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

(** GUI Toolbox.
    
    This module implements GUI helpers upon [LablGtk].
    It uses nothing from [Gtk_helper] but the [UTF-8] 
    conversion facilities.
*)

(** {1 Concepts} *)

class type widget = 
object
  method set_enabled : bool -> unit
  method coerce : GObj.widget
end
	  
class ['a] signal :
object
  method fire : 'a -> unit
  method set_enabled : bool -> unit
  method connect : ('a -> unit) -> unit
  method on_check : 'a -> (bool -> unit) -> unit
  method on_value : 'a -> (unit -> unit) -> unit
  method on_event : (unit -> unit) -> unit
end

class ['a] selector : 'a ->
object
  inherit ['a] signal
  method set : 'a -> unit
  method get : 'a
  method send : ('a -> unit) -> unit -> unit
end

(** {1 Icons} *)

type icon = [ GtkStock.id | `Share of string ]

(** {1 Simple Widgets} *)

type align = [`Left | `Right | `Center]
type style = [`Label | `Descr | `Title ]

(** Title label *)
class label : ?style:style -> ?text:string -> ?align:align -> unit ->
object
  method coerce : GObj.widget
  method set_text : string -> unit
end

class button : ?label:string -> ?icon:icon -> ?tooltip:string -> unit ->
object
  inherit widget
  inherit [unit] signal
  method set_icon : icon option -> unit
  method set_label : string -> unit
  method set_relief : bool -> unit
  method default : unit -> unit
end

class toggle : ?label:string -> ?icon:icon -> ?tooltip:string -> unit ->
object
  inherit widget
  inherit [bool] selector
  method set_icon : icon option -> unit
  method set_label : string -> unit
  method set_relief : bool -> unit
end
  
class checkbox : label:string -> ?tooltip:string -> unit ->
object
  inherit widget
  inherit [bool] selector
end

class switchbox : ?tooltip:string -> unit ->
object
  inherit widget
  inherit [bool] selector
end

class spinner : 
  ?min:int -> ?max:int -> ?step:int -> value:int -> 
  ?tooltip:string -> unit ->
object
  inherit widget
  inherit [int] selector
end

class ['a] menulist : 
  default:'a -> render:('a -> string) -> ?items:'a list -> unit ->
object
  inherit widget
  inherit ['a] selector
  method set_items : 'a list -> unit
  method get_items : 'a list
end

(** Conceptual selector, to create toggles and radio buttons. *)  
class ['a] switch : 'a -> 
object
  inherit ['a] selector
  method add_toggle : ?label:string -> ?icon:icon -> ?tooltip:string -> 
    value:'a -> unit -> widget
  method add_radio : label:string -> ?tooltip:string -> 
    value:'a -> unit -> widget
end

(** {1 File Choosers} *)

type filekind = [ `FILE | `DIR ]

(** Dialog for choosing a file.
    The default file type is [`FILE]. *)
class filechooser_dialog :
  ?kind:filekind ->
  ?title:string ->
  ?select:string ->
  ?parent:GWindow.window ->
  unit ->
object
  inherit [string] signal
  method filter : descr:string -> patterns:string list -> unit
  method select : ?dir:string -> ?file:string -> unit -> unit
end

class filechooser_button : 
  ?kind:filekind ->
  ?title:string ->
  ?select:string ->
  ?tooltip:string ->
  ?parent:GWindow.window ->
  unit ->
object
  inherit widget
  inherit filechooser_dialog
  inherit [string] selector (** Holds the filename *)
  method tooltip : (string -> string) -> unit
    (** Set the pretty-printer for tooptip. *)
  method display : (string -> string) -> unit 
    (** Set the pretty-printer for button. *)
end

(** {1 Contextual Menus} *)

class popup : unit ->
object
  method clear : unit
    (** Remove all items *)
  method add_item : label:string -> callback:(unit -> unit) -> unit
    (** Adds an item. *)
  method add_separator : unit
    (** Inserts a separator. 
	Consecutives and trailing separators are eliminated. *)
  method popup : unit -> unit
    (** Run the menu. *)
end

(** {1 Forms and Toolbar Layouts} *)

(** The expansible attribute of a field. *)
type field = 
    [ `Compact (** Fixed size. Does not expand. *)
    | `Field   (** Single line field. Expands to the left. *)
    | `Editor  (** Multiline field. Expands to both left and bottom. *)
    ]

(** A simple button-rack horizontal box. *)
class rack : widget list -> widget

(** A form with various field types.

    The form consists of two columns, with one entry per line.
    Left columns is reserved for (optional) labels.
    Fields take place in right column.
    It is also possible to add widget that spans over the two columns.

    The form can be horizontaly devided into sections.

    Elements must be added from left-to-right, top-to-down order.
*)

class form : unit ->
object
  inherit widget

  method add_newline : unit
    (** Inserts an emty line. *)

  method add_section : string -> unit
    (** Starts a new section. *)

  method add_label : string -> unit 
    (** Inserts a field name. Moves to right column. *)

  method add_label_widget : GObj.widget -> unit 
    (** Inserts a small (fixed) widget in place of a label. 
	Moves to right column. *)

  method add_field : ?label:string -> ?field:field -> GObj.widget -> unit
    (** Inserts an entry in the form. 
	Optional label is inserted in right column is specified.
	Default [field] is [`Field].
	Moves to next line. *)

  method add_row : ?field:field -> GObj.widget -> unit
    (** Inserts a wide entry in the form, spanning the two columns.
	Default [field] is [`Field]. 
	Moves to next line. *)
end

(** An editable list of items. *)

class ['a] listbox : 
  render:('a -> string) -> ?width:int -> ?height:int -> unit ->
object
  inherit widget
  inherit ['a list] selector
  method get : 'a list 
  method set : 'a list -> unit
  method insert : int -> 'a -> unit
  method on_insert_request : (int -> unit) -> unit
end

(** {1 Layouts} *)  

class ['a] notebook : ?tabs:Gtk.Tags.position -> default:'a -> unit ->
object
  inherit widget
  inherit ['a] selector
  method add : ?label:string -> 'a -> GObj.widget -> unit
  method on_focus : 'a -> (bool -> unit) -> unit
end

class type entry = 
object
  method widget : GObj.widget  (** Returns the widget *)
  method update : unit -> unit (** Signal *)
  method delete : unit -> unit (** When removed *)
end

class ['a] warray : 
  ?dir:Gtk.Tags.orientation ->
  unit ->
object
  inherit widget
  method create : ('a -> entry) -> unit
  method set : 'a list -> unit
  method get : 'a list
  method mem : 'a -> bool
  method append : 'a -> unit
  method insert : ?after:'a -> 'a -> unit
  method remove : 'a -> unit
  method update : unit -> unit
end

(** {1 Dialog} *)

(** Dialog exit button categories *)

type 'a action = 
    [ 
    | `CANCEL (** Cancel choice (same as closing the dialog or `MAIN `CANCEL) *)
    | `APPLY (** Default choice (same as `DEFAULT `APPLY) *)
    | `DEFAULT of 'a (** Default choice (right, small, default button) *)
    | `SELECT of 'a (** Secondary choice (right, small button) *)
    | `ALT of 'a (** Alternative choice (left, large button) *)
    | `ACTION of (unit -> unit) (** Button (left, large button) *)
    ]

(** Dialog Window.
    
    Dialog window are asynchroneous and modal.  To open the dialog,
    invoke [run]. The method returns immediately.  When running, the
    main window is no more sensitive (dialog is modal).  When an
    action-button is pressed, or the method [select] is invoked, the
    associated signal is emitted, and the dialog is dismissed. Then
    focus goes back to the main window, and the dialog can be re-emitted.
*)

class ['a] dialog : 
  title:string -> window:GWindow.window -> ?resize:bool -> unit ->
object
  constraint 'a = [> `CANCEL | `APPLY]
  method add_row : GObj.widget -> unit
  method add_block : GObj.widget -> unit
  method button : 
    action:'a action ->
    ?label:string -> ?icon:icon -> ?tooltip:string ->
    unit -> unit (** Closes the dialog. *)
  method select : 'a -> unit (** Closes the dialog. *)
  method run : unit -> unit (** Opens the dialog (asynchroneously). *)
  inherit ['a] signal (** Emitted when the dialog is closed. *)
end

(** {1 Rich Text Renderer} *)
  
class text : unit ->
object
  inherit widget
  method clear : unit
  method fmt : Format.formatter (** The formatter used by [printf] method. *)
  method printf : 'a. ('a,Format.formatter,unit) format -> 'a
    (** Append material to the text buffer.
	The underlying formatter (method [fmt]) recognizes the following tags:
	- ["bf"] bold face
	- ["it"] italic style
	- ["ul"] underlined
	- ["st"] striked through
	- ["red"], ["orange"], ["green"] foreground color
	- ["fg:<color name>"] foreground color
	- ["bg:<color name>"] background color
	- ["link:<link name>"] clickable link, see method [on_link].
	- ["mark:<mark name>"] named text range, see methods [show], [set_properties].
	
	Properties for any tag (except link and mark) can be modified
	with method [set_tag_properties].
	
	[t#printf] is a shortcut for [Format.fprintf t#fmt]. *)
    
  method highlight : mark:string -> GText.tag_property list -> unit
  method focus : mark:string -> unit
  method on_link : (string -> unit) -> unit
end
