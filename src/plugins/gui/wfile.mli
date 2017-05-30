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

open Widget

(** {1 File Choosers} *)

type filekind = [ `FILE | `DIR ]

(** Dialog for choosing a file.
    The default file type is [`FILE]. *)
class dialog :
  ?kind:filekind ->
  ?title:string ->
  ?select:string ->
  ?parent:GWindow.window ->
  unit ->
  object
    inherit [string] signal
    method add_filter : descr:string -> patterns:string list -> unit
    method select : ?dir:string -> ?file:string -> unit -> unit
    (** Open the dialog. The selected file is signaled {i via}
        the connected listeners. *)
  end

(** A button associated with a dialog to select the file. *)
class button :
  ?kind:filekind ->
  ?title:string ->
  ?select:string ->
  ?tooltip:string ->
  ?parent:GWindow.window ->
  unit ->
  object
    inherit widget
    inherit dialog
    inherit [string] selector (** Holds the selected filename, [""] by default. *)
    method set_tooltip : (string -> string) -> unit
    (** Set the pretty-printer for tooltip. *)
    method set_display : (string -> string) -> unit
    (** Set the pretty-printer for button. *)
  end
