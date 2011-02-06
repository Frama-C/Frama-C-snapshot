(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** This file contains the source viewer multi-tabs widget window *)

type t

val make : packing:(GObj.widget -> unit) -> t

val make_unpacked : unit -> t

val load_file : ?title:string -> t -> filename:string -> line:int -> unit
  (** If [line] is 0 then the last line of the text is shown.
      If [line] is less that 0 then no scrolling occurs.
      If [title] is not provided the page title is the filename.
  *)

(** Lowlevel interface *)

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
  (** Delete all pages in the object *)

val append_view: t -> GSourceView2.source_view -> unit

val get_nth_view:  t -> int -> GSourceView2.source_view

val enable_popup : t -> bool -> unit

val set_scrollable : t -> bool -> unit

val length: t -> int

