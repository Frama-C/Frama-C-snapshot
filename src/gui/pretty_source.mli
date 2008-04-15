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

(* $Id: pretty_source.mli,v 1.19 2008/11/18 12:13:41 uid568 Exp $ *)

(** Utilities to pretty print source with located elements in a Gtk
    TextBuffer. 
    @plugin development guide *)

open Cil_types
open Db_types

(** The kind of object that can be selected in the source viewer.
    @plugin development guide *)
type localizable =
  | PStmt of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PTermLval of (kernel_function option * kinstr * term_lval)
  | PVDecl of (kernel_function option * varinfo)

module Localizable_Datatype : Project.Datatype.S with type t = localizable

module Locs:sig
  type state
  val locs : state ref
end

val display_source :  
  global list ->
  GSourceView.source_buffer ->
  ((unit -> unit) * (unit -> unit) * (unit -> unit)) ->
  highlighter:(localizable -> start:int -> stop:int -> unit) ->
  selector:(button:int -> localizable -> unit) -> unit
  (** This will set a fresh Locs.state in Locs.locs. *)

val hilite : unit -> unit

val locate_localizable : localizable -> (int*int) option
  (** @return Some (start,stop) in offset from start of buffer if the
      given localizable has been displayed according to [Locs.locs].
      @plugin development guide *)

val localizable_from_locs : file:string -> line:int -> localizable list

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
