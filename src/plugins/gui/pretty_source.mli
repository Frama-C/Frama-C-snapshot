(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

(** Utilities to pretty print source with located elements in a Gtk
    TextBuffer. *)

open Cil_types

(** The kind of object that can be selected in the source viewer. *)
type localizable =
  | PStmt of (kernel_function * stmt)
  | PLval of (kernel_function option * kinstr * lval)
  | PExp of (kernel_function option * kinstr * exp)
  | PTermLval of (kernel_function option * kinstr * Property.t * term_lval)
  | PVDecl of (kernel_function option * kinstr * varinfo)
  (** Declaration and definition of variables and function. Check the type
      of the varinfo to distinguish between the various possibilities.
      If the varinfo is a global or a local, the kernel_function is the
      one in which the variable is declared. The [kinstr] argument is given
      for local variables with an explicit initializer. *)
  | PGlobal of global (** all globals but variable declarations and function
                          definitions. *)
  | PIP of Property.t

module Localizable: Datatype.S with type t = localizable

module Locs: sig
  type state
  (** To call when the source buffer is about to be discarded *)
  val create: unit -> state
  val clear: state -> unit
end

(* Folds or unfolds the preconditions at callsite [stmt]. *)
val fold_preconds_at_callsite: stmt -> unit

(* Are the preconditions unfolded at statement [stmt]?
   Used to know which folding or unfolding icon to display at [stmt]. *)
val are_preconds_unfolded: stmt -> bool

val display_source :
  global list ->
  GSourceView2.source_buffer ->
  host:Gtk_helper.host ->
  highlighter:(localizable -> start:int -> stop:int -> unit) ->
  selector:(button:int -> localizable -> unit) ->
  Locs.state ->
  unit
(** The selector and the highlighter are always host#protected.
    The selector will not be called when [not !Gtk_helper.gui_unlocked].
    This clears the [Locs.state] passed as argument, then fills it. *)

val hilite : Locs.state -> unit

val stmt_start: Locs.state -> stmt -> int
(** Offset at which the current statement starts in the buffer
    corresponding to [state], _without_ ACSL assertions/contracts, etc. *)

val locate_localizable : Locs.state -> localizable -> (int*int) option
(** @return Some (start,stop) in offset from start of buffer if the
    given localizable has been displayed according to [Locs.locs]. *)


val kf_of_localizable : localizable -> kernel_function option
val ki_of_localizable : localizable -> kinstr
val varinfo_of_localizable : localizable -> varinfo option

val localizable_from_locs :
  Locs.state -> file:string -> line:int -> localizable list
(** Returns the lists of localizable in [file] at [line]
    visible in the current [Locs.state].
    This function is inefficient as it iterates on all the current
    [Locs.state]. *)

val loc_to_localizable: ?precise_col:bool -> Lexing.position -> localizable option
(** return the (hopefully) most precise localizable that contains the given
    Lexing.position. If [precise_col] is [true], takes the column number into
    account (possibly a more precise, but costly, result).
    @since Nitrogen-20111001 *)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
