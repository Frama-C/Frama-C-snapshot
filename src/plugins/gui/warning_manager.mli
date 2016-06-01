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

(** Handle Frama-C warnings in the GUI. *)

type t
(** Type of the widget containing the warnings. *)

val make :
  packing:(GObj.widget -> unit) -> 
  callback:(Log.event -> GTree.view_column -> unit) -> t
(** Build a new widget for storing the warnings. *)

val append: t -> Log.event -> unit
(** Append a new message warning. *)

val clear: t -> unit
(** Clear all the stored warnigns. *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
