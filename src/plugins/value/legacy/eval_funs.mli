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

(** Value analysis of entire functions, using the legacy engine. *)

val force_compute : unit -> unit
(** Perform a full analysis, starting from the [main] function. *)


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
