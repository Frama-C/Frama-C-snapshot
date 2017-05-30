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

(** {1 Rich Text Renderer} *)

type 'a entry = int * int * 'a

class type ['a] marker =
  object
    method set_style : GText.tag_property list -> unit
    (** The style of added entries. Defaults to empty.

        {b Warning} must be set before any entry is added. *)
    method set_hover : GText.tag_property list -> unit
    (** The style of hovered entries. Defaults to background green.

        {b Warning} must be set before any entry is added. *)

    method connect : (GdkEvent.Button.t -> 'a entry -> unit) -> unit
    method on_click : ('a entry -> unit) -> unit
    method on_double_click : ('a entry -> unit) -> unit
    method on_right_click : ('a entry -> unit) -> unit
    method on_shift_click : ('a entry -> unit) -> unit
    method on_add : ('a entry -> unit) -> unit

    method wrap : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit
    (** Register with [#add] an entry around its pretty-print. *)

    method mark : 'b. 'a -> (Format.formatter -> 'b -> unit) -> Format.formatter -> 'b -> unit
    (** Register the entry around the pretty-printed material. *)

    method add : 'a entry -> unit
    (** Register an entry *)
    
  end

class text : ?autoscroll:bool -> ?width:int -> ?indent:int -> unit ->
  object
    inherit Widget.widget
    method clear : unit
    method fmt : Format.formatter (** The formatter used by [printf] method. *)

    method hrule : unit (** Print an horizontal rule. Consecutive rules are collapsed. *)
    method printf : 'a. ?scroll:bool -> ('a,Format.formatter,unit) format -> 'a
    (** Append material to the text buffer, optionally scrolling it to the
        beginning of the message (defaults to autoscrolling setting).

       	The underlying formatter (method [fmt]) recognizes the following default
        tags:
        - ["bf"] bold face
        - ["it"] italic style
        - ["ul"] underlined
        - ["st"] striked through
        - ["blue"], ["red"], ["orange"], ["green"] predefined foreground color
        - ["hover"] background green (default for highlighter)
        - ["link"] underlined blue
        - ["fg:<color name>"] foreground color
        - ["bg:<color name>"] background color
        - ["link:<name>"] add a link marker
        
        Properties for any tag (except parametric and mark) can be modified
        with method [set_tag_style].
        
        [t#printf] is a shortcut for [Format.fprintf t#fmt] followed
        by flushing and optional scrolling. *)

    method set_monospace : unit
    (** Set a monospace font *)

    method set_font: string -> unit
    (** Set the font used to display the text. *)

    method set_css : (string * GText.tag_property list) list -> unit
    (** Override properties for the given tag name.
        Must be used before the tag is used. *)

    method set_style : string -> int -> int -> unit
    method remove_style : string -> int -> int -> unit
    method remove_all : string list -> unit

    method offset : int
    method lines : int
    method set_width : int -> unit
    method set_indent : int -> unit

    method wrap :
      (int -> int -> unit) ->
      (Format.formatter -> unit) ->
      Format.formatter -> unit
    (** Print with a callback to the actual bounds of the printed material
        after boxing. *)

    method marker : 'a. 'a marker
    method links : string marker

    method on_link : (string -> unit) -> unit
    (** Short cut to [links#on_click] (callback without range). *)

    method set_autoscroll: bool -> unit
    (** Automatically scroll to lastly inserted text (console behavior). *)

    method scroll : ?line:int -> unit -> unit
    (** Default line is last one. *)

    method select : ?scroll:bool -> int -> int -> unit
    (** When [scroll:false] (default), only minimal scrolling is performed to make
        the selection visible. Otherwise, the window is scrolled to center the selection
        at screen. *)

    method get_view: GText.view
    (** Returns the viewer object (and the buffer it contains). *)

  end
