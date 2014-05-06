(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
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
compile-command: "make -C ../.."
End:
*)
