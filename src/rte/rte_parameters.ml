(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

let help_msg = "generates annotations for runtime error checking and \
preconditions at call sites"

include Plugin.Register
  (struct
     let name = "rte annotation"
     let shortname = "rte"
     let module_name = "RteGen"
     let help = help_msg
   end)

(* enabling/disabling plugin *)
module Enabled =
  False
    (struct
       let option_name = "-rte"
       let help = "when on (off by default), " ^ help_msg
       let kind = `Tuning
     end)

(* if true: generate assertions for both signed and unsigned overflows.
   only unsigned overflows are "true" runtime errors.
   see DoSignerOverflow parameter
*)
module DoUnsignedOverflow =
  False
    (struct
       let option_name = "-rte-unsigned-ov"
       let help = "when on (off by default), annotate for unsigned overflows"
       let kind = `Correctness
     end)

(* if DoAll is true: all other options become true, except for DoUnsignedOverflow
   and "PreConds"
    <=> all "true" runtime error assertions are generated *)
module DoAll =
  True
    (struct
       let option_name = "-rte-all"
       let help = "when on (by default), generate everything (supersedes all -rte-no-*)"
       let kind = `Correctness
     end)

module Print =
  False
    (struct
       let option_name = "-rte-print"
       let help = "when on (off by default), pretty print the annotated code"
       let kind = `Tuning
     end)

module DoSignedOverflow =
  False
    (struct
       let option_name = "-rte-signed"
       let help = "when on (off by default), annotate for signed overflow"
       let kind = `Correctness
     end)

module DoDownCast =
  False
    (struct
       let option_name = "-rte-downcast"
       let help = "when on (off by default), annotate signed integer downcast"
       let kind = `Correctness
     end)

module DoDivMod =
  False
    (struct
       let option_name = "-rte-div"
       let help = "when on (off by default), annotate for division by zero"
       let kind = `Correctness
     end)

module DoMemAccess =
  False
    (struct
       let option_name = "-rte-mem"
       let help = "when on (off by default), annotate for valid pointer or array access"
       let kind = `Correctness
     end)

module ConstFold =
  True
    (struct
       let option_name = "-rte-const"
       let help = "when on (by default), simplify assertions involving constants"
         (* if on, evaluates constants in order to check if assertions
            are trivially true / false *)
       let kind = `Tuning
     end)

module DoCalledPrecond =
  False
    (struct
       let option_name = "-rte-precond"
       let help = "when on (off by default), generate assertions on function calls based on contracts"
       let kind = `Correctness
     end)

module Warn =
  True
    (struct
       let option_name = "-rte-warn"
       let help = "when on (default), emits warning on broken asserts"
       let kind = `Tuning
     end)

module FunctionSelection =
  StringSet
    (struct
       let option_name = "-rte-select"
       let arg_name = "fun"
       let help = "select <fun> for analysis (default all functions)"
       let kind = `Correctness
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
