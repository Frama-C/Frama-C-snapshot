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

(** Utilities to pretty print source with located elements in a Gtk TextBuffer. *)

open Cil_types
open Db_types

(** The kind of object that can be selected in the source viewer *)
type localizable =
  | PStmt of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PTermLval of (kernel_function option * kinstr * term_lval)
  | PVDecl of (kernel_function option * varinfo)

module Locs:sig
  type tbl
  val locs : tbl ref
end

val display_source :  
  global list ->
  GText.buffer ->
  ((unit ->unit)*(unit ->unit)*(unit ->unit)) ->
  highlighter:(localizable -> start:int -> stop:int -> unit) ->
  selector:(button:int -> localizable -> unit) -> unit
(** This will set a fresh Locs.tbl in Locs.locs *)


val locate_localizable : localizable -> (int*int) option
  (** @returns Some (start,stop) in offset from start of buffer if the
      given localizable has been displayed according to [Locs.locs] *)
val localizable_from_locs : file:string -> line:int -> localizable list

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
