(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

(* $Id: pretty_source.mli,v 1.22 2008-12-22 17:29:17 uid528 Exp $ *)

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
  | PCodeAnnot of (kernel_function * stmt * code_annotation)
  | PGlobal of global (* all globals but variable declarations and function 
			 definitions. *)
  | PBehavior of (kernel_function*funbehavior)
  | PPredicate of (kernel_function option*kinstr*identified_predicate)

module Localizable_Datatype : Project.Datatype.S with type t = localizable

module Locs:sig
  type state
end

val display_source :  
  global list ->
  GSourceView.source_buffer ->
  host:Gtk_helper.host -> 
  highlighter:(localizable -> start:int -> stop:int -> unit) ->
  selector:(button:int -> localizable -> unit) -> Locs.state
  (** The selector and the highlighter are always host#protected.
      The selector will not be called when [not !Gtk_helper.gui_unlocked].
      This returns a [state] to pass to the functions defined hereafter. *)

val hilite : Locs.state -> unit

val locate_localizable : Locs.state -> localizable -> (int*int) option
  (** @return Some (start,stop) in offset from start of buffer if the
      given localizable has been displayed according to [Locs.locs].
      @plugin development guide *)

val localizable_from_locs : Locs.state -> file:string -> line:int -> localizable list
  (** Returns the lists of localizable in [file] at [line] 
      visible in the current [Locs.state].
      This function is inefficient as it iterates on all the current
      [Locs.state]. *)
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
