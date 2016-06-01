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

(** Option -deps *)
module ForceDeps: Parameter_sig.With_output

(** Option -calldeps.
    @plugin development guide *)
module ForceCallDeps: Parameter_sig.With_output

(** Option -show-indirect-deps *)
module ShowIndirectDeps: Parameter_sig.Bool

(** Option -from-verify-assigns. *)
module VerifyAssigns: Parameter_sig.Bool

(** Option -experimental-path-deps *)
module PathDeps: Parameter_sig.Bool

(** Option -experimental-mem-deps *)
module MemDeps: Parameter_sig.Bool


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
