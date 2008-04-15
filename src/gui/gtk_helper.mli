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

(* $Id: gtk_helper.mli,v 1.25 2008/12/16 10:02:59 uid526 Exp $ *)

(** Generic Gtk helpers. 
    @plugin development guide *)

val apply_tag : GSourceView.source_buffer -> GText.tag -> int -> int -> unit
  (** @plugin development guide *)

val remove_tag : GSourceView.source_buffer -> GText.tag -> int -> int -> unit

val cleanup_tag : GSourceView.source_buffer -> GText.tag -> unit
  (** @plugin development guide *)

val cleanup_all_tags : GSourceView.source_buffer -> unit

val expand_to_path : GTree.view -> Gtk.tree_path -> unit

val make_tag : 
  < tag_table : Gtk.text_tag_table;
    create_tag : ?name:string -> GText.tag_property list -> GText.tag ; .. > 
  -> name:string -> GText.tag_property list -> GText.tag
  (** @plugin development guide *)

val make_formatter : ?flush:(unit -> unit) -> #GText.buffer -> Format.formatter

val channel_redirector :  Unix.file_descr -> (string -> bool) -> unit

val redirect : Format.formatter -> #GText.buffer -> unit

(** This is a mutex you may use to prevent running some code while the GUI
    is locked. *)
val gui_unlocked: bool ref

val string_selector: string list -> (GObj.widget -> unit) -> GEdit.entry

(** @return a function to be called to enforce synchronization *)
(*val range_selector: 
 GPack.box -> label:string -> lower:int -> upper:int 
 -> (int -> unit) -> (unit -> int) -> (unit -> unit)
*)
(** returns (add, remove_selected, get_elements) *)
val make_string_list: packing:(GObj.widget -> unit) 
  -> (string -> unit)* (unit -> unit)*(unit -> string list)

type 'a chooser = GPack.box -> string -> (unit -> 'a) -> ('a -> unit) -> (unit -> unit)
(** Pack a check button with the given getter and setter. *)
val on_bool: bool chooser
val on_bool_radio: GPack.box -> string -> string 
  -> (unit -> bool) -> (bool -> unit) -> (unit -> unit)
  
val on_int: ?lower:int -> ?upper:int -> ?sensitive:(unit -> bool) ->  int chooser
(** Returns a function usable for refresfring purpose.
    By default, sensitivity is set to true when this function is called. *)
  
val on_string: ?validator:(string -> bool) -> string chooser
val on_string_set: string chooser
val on_string_completion: ?validator:(string -> bool) -> string list -> string chooser

val place_paned: GPack.paned -> float -> unit
  (** Sets the position of the paned widget to the given ratio *)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
