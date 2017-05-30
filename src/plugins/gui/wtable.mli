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

(** {1 Table Views} *)

type ('a,'b) column =
  ?title:string -> 'b list -> ('a -> 'b list) -> GTree.view_column

class type ['a] columns =
  object
    method view : GTree.view (** the tree *)
    method scroll : GBin.scrolled_window (** scrolled tree (build on demand) *)
    method coerce : GObj.widget (** widget of the scroll *)
    method pack : (GObj.widget -> unit) -> unit (** packs the scroll *)
    method reload : unit (** Structure has changed *)
    method update_all : unit (** (only) Content of rows has changed *)
    method update_row : 'a -> unit
    method insert_row : 'a -> unit
    method set_focus : 'a -> GTree.view_column -> unit
    method on_click : ('a -> GTree.view_column -> unit) -> unit
    method on_right_click : ('a -> GTree.view_column -> unit) -> unit
    method on_double_click : ('a -> GTree.view_column -> unit) -> unit
    method set_selection_mode : Gtk.Tags.selection_mode -> unit
    method on_selection : (unit -> unit) -> unit
    method count_selected : int
    method iter_selected : ('a -> unit) -> unit
    method is_selected : 'a -> bool
    method add_column_text   : ('a,GTree.cell_properties_text) column
    method add_column_pixbuf : ('a,GTree.cell_properties_pixbuf) column
    method add_column_toggle : ('a,GTree.cell_properties_toggle) column
    method add_column_empty : GTree.view_column
    (** Add an empty column that always appears after the columns created
        by the other [add_column] methods. *)
  end

class type ['a] listmodel =
  object
    method reload : unit
    method size : int
    method index : 'a -> int
    method get : int -> 'a
  end

class ['a] list : ?packing:(GObj.widget->unit) ->
  ?width:int -> ?height:int ->
  ?headers:bool -> ?rules:bool ->
  'a listmodel ->
  object
    inherit ['a] columns
  end

class type ['a] treemodel =
  object
    method reload : unit
    method has_child : 'a -> bool
    method children : 'a option -> int
    method child_at : 'a option -> int -> 'a
    method parent : 'a -> 'a option
    method index : 'a -> int
  end

class ['a] tree : ?packing:(GObj.widget->unit) ->
  ?width:int -> ?height:int ->
  ?headers:bool -> ?rules:bool ->
  'a treemodel ->
  object
    inherit ['a] columns
  end
