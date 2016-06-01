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

module SemanticConstFolding: Parameter_sig.Bool
module SemanticConstFold: Parameter_sig.Fundec_set
module CastIntro: Parameter_sig.Bool
module ExpandLogicContext: Parameter_sig.Bool
module Project_name: Parameter_sig.String

include Log.Messages

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
