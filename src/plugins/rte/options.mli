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

module Enabled: Parameter_sig.Bool

module DoAll: Parameter_sig.Bool
module DoShift : Parameter_sig.Bool
module DoDivMod : Parameter_sig.Bool
module DoFloatToInt : Parameter_sig.Bool
module DoMemAccess : Parameter_sig.Bool
module DoCalledPrecond : Parameter_sig.Bool

module Trivial : Parameter_sig.Bool
module Warn : Parameter_sig.Bool
module FunctionSelection: Parameter_sig.Kernel_function_set

val warn: ?source:Lexing.position -> ('a, Format.formatter, unit) format -> 'a

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
