(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
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
compile-command: "make -C ../../.."
End:
*)
