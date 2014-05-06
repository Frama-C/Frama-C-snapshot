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

module Enabled: Parameter_sig.Bool

module DoAll: Parameter_sig.Bool
module DoShift : Parameter_sig.Bool
module DoDivMod : Parameter_sig.Bool
module DoFloatToInt : Parameter_sig.Bool
module DoMemAccess : Parameter_sig.Bool
module DoCalledPrecond : Parameter_sig.Bool

module Trivial : Parameter_sig.Bool
module Warn : Parameter_sig.Bool
module FunctionSelection : Parameter_sig.String_set

val warn: ?source:Lexing.position -> ('a, Format.formatter, unit) format -> 'a

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
