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

(** The Frama-C launcher.
    That is the dialog box for configuring and running Frama-C with new
    parameter values. *)

(** Subtype of {!Design.main_window_extension_points} which is required to show
    the launcher. *)
class type basic_main = object
  inherit Gtk_helper.host
  method main_window: GWindow.window
  method reset: unit -> unit
end

val show: ?height:int -> ?width:int -> host:basic_main -> unit -> unit
  (** Display the Frama-C launcher. *)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
