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

(** {1 Simple Widgets} *)

class type widget =
  object
    method set_visible : bool -> unit
    method set_enabled : bool -> unit
    method coerce : GObj.widget
    method widget : widget
  end

class type action =
  object
    inherit widget
    method set_tooltip : string -> unit
  end

class type t = widget

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
        lock and execute the continuation and {i finally} release the lock. *)

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
type style = [`Label | `Descr | `Code | `Title]
type color = [ GDraw.color | `NORMAL ]

(** Default: [~style:`Label ~align:`Left] *)
class label : ?style:style -> ?align:align -> ?width:int -> ?text:string -> unit ->
  object
    inherit widget
    method set_text : string -> unit
    method set_tooltip : string -> unit
    method set_fg : color -> unit
    method set_bg : color -> unit
  end

(** {2 Icons} *)

type icon = [ GtkStock.id | `Share of string | `None ]

val default_icon : unit -> GdkPixbuf.pixbuf
val shared_icon : string -> GdkPixbuf.pixbuf
val gimage : icon -> GMisc.image

class image : icon ->
  object
    inherit widget
    method set_icon : icon -> unit
  end

(** {2 Buttons} *)

class button : ?align:align -> ?icon:icon -> ?label:string -> ?border:bool -> ?tooltip:string -> unit ->
  object
    inherit action
    inherit [unit] signal
    method set_icon : icon -> unit
    method set_label : string -> unit
    method set_border : bool -> unit
    method default : unit -> unit
  end

class toggle : ?align:align -> ?icon:icon -> ?label:string -> ?border:bool -> ?tooltip:string ->
  unit ->
  object
    inherit action
    inherit [bool] selector
    method set_icon : icon -> unit
    method set_label : string -> unit
    method set_border : bool -> unit
  end

class checkbox : label:string -> ?tooltip:string -> unit ->
  object
    inherit action
    inherit [bool] selector
  end

class switch : ?tooltip:string -> unit ->
  object
    inherit action
    inherit [bool] selector
  end

(** {2 Groups} *)

(** A group is not a widget ; it creates interconnected toggle or radio buttons,
    each switching to a peculiar value.

    Use [Wbox.hgroup] and [Wbox.vgroup] to pack several buttons into a dongle.
*)
class ['a] group : 'a ->
  object
    inherit ['a] selector
    method add_toggle : ?label:string -> ?icon:icon -> ?tooltip:string ->
      value:'a -> unit -> widget
    method add_radio : label:string -> ?tooltip:string ->
      value:'a -> unit -> widget
  end

(** {2 Selectors} *)

class spinner :
  ?min:int -> ?max:int -> ?step:int -> value:int ->
  ?tooltip:string -> unit ->
  object
    inherit action
    inherit [int] selector
    method set_min : int -> unit
    method set_max : int -> unit
  end

class ['a] menu :
  default:'a ->
  ?options:('a * string) list ->
  ?render:('a -> string) ->
  ?items:'a list ->
  unit ->
  object
    inherit action
    inherit ['a] selector
    method set_options : ('a * string) list -> unit
    method set_render : ('a -> string) -> unit
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
        Consecutive and trailing separators are eliminated. *)
    method run : unit -> unit
    (** Run the menu. *)
  end
