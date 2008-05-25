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

(* $Id: gtk_helper.mli,v 1.11 2008/05/23 14:35:10 uid528 Exp $ *)

(** Generic Gtk helpers. *)

val apply_tag : GText.buffer -> GText.tag -> int -> int -> unit
val remove_tag : GText.buffer -> GText.tag -> int -> int -> unit
val cleanup_tag : GText.buffer -> GText.tag -> unit
val expand_to_path : GTree.view -> Gtk.tree_path -> unit
val make_tag : 
  GText.buffer -> name:string -> GText.tag_property list -> GText.tag

val make_formatter : GText.buffer -> Format.formatter

val channel_redirector :  Unix.file_descr -> (string -> bool) -> unit

val redirect : Format.formatter -> GText.buffer -> unit

(** This is a mutex you may use to prevent running some code while the GUI
    is locked. *)
val gui_unlocked: bool ref

val string_selector: string list -> (GObj.widget -> unit) -> GEdit.entry

(** returns (add, remove_selected, get_elements) *)
val make_string_list: packing:(GObj.widget -> unit) 
  -> (string -> unit)* (unit -> unit)*(unit -> string list)
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
