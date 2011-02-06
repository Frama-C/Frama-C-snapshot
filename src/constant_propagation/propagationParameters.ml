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
         let option_name = "-semantic-const-folding"
	 let help = "pretty print a version of the source code where each constant expression is replaced by its value"
       let kind = `Tuning
       end)

module SemanticConstFold =
  StringSet
    (struct
       let option_name = "-semantic-const-fold"
       let arg_name = "f1, ..., fn"
       let help = "propagate constants only into functions f1,...,fn"
       let kind = `Tuning
       end)

module CastIntro =
    False
      (struct
	let option_name = "-cast-from-constant"
	let help = "replace expressions by constants even when doing so \
requires a pointer cast"
        let kind = `Tuning
       end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
