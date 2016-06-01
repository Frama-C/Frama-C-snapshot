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

module BuildAll: Parameter_sig.With_output

module BuildFct: Parameter_sig.Kernel_function_set

module PrintBw: Parameter_sig.Bool

module DotBasename: Parameter_sig.String

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
