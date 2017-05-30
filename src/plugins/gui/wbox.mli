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

(** Box Layouts. *)

open Widget

type box (** A packed widget with its layout directives *)

(** Expansion Modes. *)
type expand =
  | W (** Stay at widget's size. *)
  | H (** Expands horizontally. Typically, a text-field. *)
  | V (** Expands vertically. Typically a side-bar. *)
  | HV (** Expands in both directions. Typically, a text editor. *)

(** Generic packing. Without [~widget], a space is created instead.

    Defaults: [~expand:W ~padding:0]. *)
val box : ?expand:expand -> ?padding:int -> ?widget:#widget -> unit -> box

(** Helper to [box] for packing a [GObj.widget]. Same defaults than [box]. *)
val g : ?expand:expand -> ?padding:int -> #GObj.widget -> box

(** Helper to [box] for packing a [widget]. Same defaults than [box]. *)
val w : ?expand:expand -> ?padding:int -> #widget -> box
val h : ?padding:int -> #widget -> box (** [w ~expand:H] *)
val v : ?padding:int -> #widget -> box (** [w ~expand:V] *)
val hv : ?padding:int -> #widget -> box (** [w ~expand:HV] *)

val label : ?fill:bool -> ?style:style -> ?align:align -> ?padding:int -> string -> box
(** Helper to pack a [Widget.label] widget using [box].

    Default: [~fill:false ~style:`Label ~align:`Left ~padding:0] *)

(** [hbox] and [vbox] can be used to created nested boxes. 
    Typically, local scope opening can be used, typically: 
    [Wbox.(hbox [ w A ; w B ; w C ])],
    where [A], [B] and [C] are widgets, or boxes. 

    Notice that nested boxes can {i generally} be packed using default 
    [W] mode, even if they contains horizontal or vertical widgets. *)

val hbox : box list -> widget (** Pack a list of boxes horizontally. *)
val vbox : box list -> widget (** Pack a list of boxes vertically. *)

(** Pack a list of widgets horizontally, with all widgets stuck to the same width *)
val hgroup : widget list -> widget

(** Pack a list of widgets vertically, with all widgets stuck to the same width *)
val vgroup : widget list -> widget

(** The first list is packed to the left side of the toolbar. 
    The second list is packed to the right side of the toolbar. *)
val toolbar : box list -> box list -> widget

(** The first list is packed to the top of the sidebar. 
    The second list is packed to the bottom of the sidebar. *)
val sidebar : box list -> box list -> widget

(** Helper to create a full featured window:
    [~top] is layout as a toolbar, [left] and [right] as sidebars, and [bottom] as a status bar. 
    The main (non-optional) widget is centered with full expansion in both directions. *)
val panel : ?top:widget -> ?left:widget -> ?right:widget -> ?bottom:widget -> #widget -> widget

