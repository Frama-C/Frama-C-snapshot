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

module Run: Parameter_sig.Bool
module Dictionary: Parameter_sig.String
module Literal_string: Parameter_sig.String

val states: State.t list

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
