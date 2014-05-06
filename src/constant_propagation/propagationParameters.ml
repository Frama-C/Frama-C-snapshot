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

(** Constant Propagation *)
include Plugin.Register
  (struct
     let name = "semantic constant folding"
     let shortname = "scf"
     let help = "propagates constants semantically"
   end)

module SemanticConstFolding =
  False
      (struct
         let option_name = "-scf"
         let help = "pretty print a version of the source code where each constant expression is replaced by its value"
       end)
let () = SemanticConstFolding.add_aliases ["-semantic-const-folding"]

module SemanticConstFold =
  StringSet
    (struct
       let option_name = "-scf-fct"
       let arg_name = "f1, ..., fn"
       let help = "propagate constants only into functions f1,...,fn"
       end)
let () = SemanticConstFold.add_aliases ["-semantic-const-fold"]

module CastIntro =
    False
      (struct
        let option_name = "-scf-allow-cast"
        let help = "replace expressions by constants even when doing so \
requires a pointer cast"
       end)
let () = CastIntro.add_aliases ["-cast-from-constant"]

module ExpandLogicContext =
  False
      (struct
        let option_name = "-scf-logic"
        let help = "replace values from logical context and create corresponding variables (HIGHLY EXPERIMENTAL)"
       end)
let () = ExpandLogicContext.add_aliases ["-semantic-const-fold-logic"]

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
