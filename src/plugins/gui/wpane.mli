(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Panels *)

open Widget

(** {2 Form} *)

(** The expansible attribute of a field. *)
type field =
  [ `Compact (** Fixed size. Does not expand. *)
  | `Field   (** Single line field. Expands to the left. *)
  | `Panel   (** Multiline field. Expands to both left and bottom. *)
  ]

(** A form with various field types.

    The form consists of two columns, with one entry per line.
    Left columns is reserved for (optional) labels.
    Fields take place in right column.
    It is also possible to add widget that spans over the two columns.

    The form can be horizontally divided into sections.

    Elements must be added in left-to-right, top-down order.
*)

class form : unit ->
  object
    inherit widget

    method add_newline : unit
    (** Inserts an empty line. *)

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
      
    method add_row :
      ?field:field ->
      ?xpadding:int ->
      ?ypadding:int -> GObj.widget -> unit
    (** Inserts a wide entry in the form, spanning the two columns.
        Default [field] is [`Field].
        Moves to next line. *)
  end

(** {2 Tabbed-pane} *)

class ['a] notebook : ?tabs:Gtk.Tags.position -> default:'a -> unit ->
  object
    inherit widget
    inherit ['a] selector
    method add : ?label:string -> 'a -> GObj.widget -> unit
    method on_focus : 'a -> (bool -> unit) -> unit
  end

(** {2 Array of Widgets} *)

class type entry =
  object
    method widget : GObj.widget  (** Returns the widget *)
    method update : unit -> unit (** On array request *)
    method delete : unit -> unit (** When removed *)
  end

class ['a] warray :
  ?dir:Gtk.Tags.orientation ->
  ?entry:('a -> entry) ->
  unit ->
  object
    inherit widget
    (** Install the new-entry creator. *)
    method set_entry : ('a -> entry) -> unit
    method set : 'a list -> unit
    method get : 'a list
    method mem : 'a -> bool
    method append : 'a -> unit
    method insert : ?after:'a -> 'a -> unit
    method remove : 'a -> unit
    method update : unit -> unit
    (** Request update on each entry. *)
  end

(** {2 Dialogs} *)

(** Button for dialog options *)
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

    Dialog window are asynchronous and modal.  To open the dialog,
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
    method run : unit -> unit (** Opens the dialog (asynchronously). *)
    inherit ['a] signal (** Emitted when the dialog is closed. *)
  end
