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

include Plugin.Register
    (struct
       let name = "metrics"
       let shortname = "metrics"
       let help = "syntactic metrics"
     end)

module Enabled =
  WithOutput
    (struct
      let option_name = "-metrics"
      let help = "activate metrics computation"
      let output_by_default = true
     end)

module ByFunction =
  WithOutput
    (struct
      let option_name = "-metrics-by-function"
      let help = "also compute metrics on a per-function basis"
      let output_by_default = true
     end)

module OutputFile =
  EmptyString
    (struct
      let option_name = "-metrics-output"
      let arg_name = "filename"
      let help = "print some metrics into the specified file; \
                  the output format is recognized through the extension."
     end)

module ValueCoverage =
  WithOutput (
    struct
      let option_name = "-metrics-value-cover"
      let help = "estimate value analysis coverage w.r.t. \
                  to reachable syntactic definitions"
      let output_by_default = true
    end)

module AstType =
  String
    (struct
      let option_name = "-metrics-ast"
      let arg_name = "[cabs | cil | acsl]"
      let help = "apply metrics to Cabs or CIL AST, or to ACSL specs"
      let default = "cil"
     end
    )

let () = AstType.set_possible_values ["cil"; "cabs"; "acsl"]

module SyntacticallyReachable =
  StringSet
    (struct
      let option_name = "-metrics-cover"
      let arg_name = "f1,..,fn"
      let help = "compute an overapproximation of the functions reachable from \
                  f1,..,fn."
     end
    )

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
