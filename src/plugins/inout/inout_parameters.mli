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

module ForceAccessPath: Parameter_sig.Bool
module ForceOut: Parameter_sig.Bool
module ForceExternalOut: Parameter_sig.Bool
module ForceInput: Parameter_sig.Bool
module ForceInputWithFormals: Parameter_sig.Bool
module ForceInout: Parameter_sig.Bool
module ForceCallwiseInout: Parameter_sig.Bool
module ForceInoutExternalWithFormals: Parameter_sig.Bool
module ForceDeref: Parameter_sig.Bool

module Output: Parameter_sig.Bool

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
