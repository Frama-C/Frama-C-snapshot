(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
     let descr = "propagates constants semantically"
   end)

module SemanticConstFolding =
  False
      (struct
         let option_name = "-semantic-const-folding"
	 let descr = "pretty print a version of the source code where each constant expression is replaced by its value"
       end)
module SemanticConstFold =
  StringSet
    (struct
       let option_name = "-semantic-const-fold"
       let arg_name = "f1, ..., fn"
       let descr = "propagate constants only into functions f1,...,fn"
       end)
module CastIntro =
    False(struct
	    let option_name = "-cast-from-constant"
	    let descr = "replace expressions by constants even when doing so requires a pointer cast"
	  end)


(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
