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

(* ************************************************************************* *)
(** {2 Security parameters} *)
(* ************************************************************************* *)

include Plugin.S

module Slicing: Parameter_sig.Bool
  (** Perform the security slicing pre-analysis. *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
