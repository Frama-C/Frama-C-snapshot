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

(** {1 Simple Widgets} *)

class type widget =
  object
    method set_enabled : bool -> unit
    method coerce : GObj.widget
  end

class type ['a] signal =
  object
    method fire : 'a -> unit
    (** Emits the signal to all connected listeners, if enabled and not locked.
        The signal itself is locked during the execution of listeners, breaking
        any circularity. *)

    method set_enabled : bool -> unit
    (** Turns emission of signals {i on} or {i off}. *)

    method lock : (unit -> unit) -> unit
    (** If not locked,
        lock and execute the continutation and {i finally} release the lock. *)

    method connect : ('a -> unit) -> unit
    (** [connect f] calls [f s] on each signal [s]. *)

    method on_check : 'a -> (bool -> unit) -> unit
    (** [on_check v] fires a boolean signal [(s=v)] on signal [s]. *)

    method on_value : 'a -> (unit -> unit) -> unit
    (** [on_value v] fires a unit signal on signal [s] when [s=v]. *)

    method on_event : (unit -> unit) -> unit
    (** [on_event] fires a unit signal on any signal. *)
  end

class type ['a] selector =
  object
    inherit ['a] signal (** listen to all sets. *)
    method set : 'a -> unit
    method get : 'a
    method send : ('a -> unit) -> unit -> unit
    (* [send f] calls [f] with the current value {i via} the signal lock. *)
  end

(** {2 Labels} *)

type align = [`Left | `Right | `Center]
type style = [`Label | `Descr | `Title]

class label : ?style:style -> ?text:string -> ?align:align -> unit ->
  object
    inherit widget
    method set_text : string -> unit
  end

(** {2 Icons} *)

type icon = [ GtkStock.id | `Share of string ]

val default_icon : unit -> GdkPixbuf.pixbuf
val shared_icon : string -> GdkPixbuf.pixbuf
val gimage : icon -> GMisc.image

(** {2 Buttons} *)

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

class switch : ?tooltip:string -> unit ->
  object
    inherit widget
    inherit [bool] selector
  end

(** {2 Groups} *)

(** A group is not a widget ; it creates interconnected toggle or radio buttons,
    each switching to a peculiar value. *)
class ['a] group : 'a ->
  object
    inherit ['a] selector
    method add_toggle : ?label:string -> ?icon:icon -> ?tooltip:string ->
      value:'a -> unit -> widget
    method add_radio : label:string -> ?tooltip:string ->
      value:'a -> unit -> widget
  end

(** Compact vertical box, typically used for packing radio buttons from a {!group}. *)
class vbox : widget list -> widget

(** Compact horizontal box, typically used for packing toggle buttons from a {!group}. *)
class hbox : widget list -> widget

(** {2 Selectors} *)

class spinner :
  ?min:int -> ?max:int -> ?step:int -> value:int ->
  ?tooltip:string -> unit ->
  object
    inherit widget
    inherit [int] selector
  end

class ['a] menu :
  default:'a -> render:('a -> string) -> ?items:'a list -> unit ->
  object
    inherit widget
    inherit ['a] selector
    method set_items : 'a list -> unit
    method get_items : 'a list
  end

(** Contextual menu.
    The [#popup] method can be called inside a right-click callback. For instance:
    [widget#on_right_click menu#popup]. *)
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
