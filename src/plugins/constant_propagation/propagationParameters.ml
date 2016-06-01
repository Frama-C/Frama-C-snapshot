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
  Fundec_set
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

module Project_name =
  String
    (struct
      let option_name = "-scf-project-name"
      let default = "propagated"
      let arg_name = ""
      let help = "name of the generated project (default is `propagated`)"
     end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
