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

(** GUI as a plug-in. *)

include Plugin.S

module Project_name: Parameter_sig.String
(** Option -gui-project. *)

module Undo: Parameter_sig.Bool
(** Option -undo. *)

module Theme: Parameter_sig.String
(** Option -gui-theme. *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
