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

include Plugin.S

module Analysis: Parameter_sig.Bool
(** Whether to perform spare code detection or not. *)

module Annot : Parameter_sig.Bool
(** keep more things to keep all reachable annotations. *)

module GlobDecl : Parameter_sig.Bool
    (** remove unused global types and variables *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
