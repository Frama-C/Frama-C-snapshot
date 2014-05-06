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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] Yet useless for the Frama-C platform. It seems to be only
   used by a CEA private plug-in (AP via LC). 
   To the authors/users of this module: please document it. *)

type t

val make: 
  ?tab_pos:Gtk.Tags.position -> ?packing:(GObj.widget -> unit) -> unit -> t

val get_notebook: t -> GPack.notebook

val append_source_tab : t -> string -> GSourceView2.source_view

val prepend_source_tab : t -> string -> GSourceView2.source_view

val get_nth_page: t -> int -> GObj.widget

val current_page: t -> int

val last_page: t -> int

val set_current_view: t -> int -> unit

val get_current_view: t -> GSourceView2.source_view

val get_current_index: t -> int

val delete_current_view: t -> unit

val delete_view: t -> int -> unit

val delete_all_views: t -> unit

val append_view: t -> GSourceView2.source_view -> unit

val get_nth_view:  t -> int -> GSourceView2.source_view

val enable_popup : t -> bool -> unit

val set_scrollable : t -> bool -> unit

val length: t -> int

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
