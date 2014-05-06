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
     let name = "sparecode"
     let shortname = "sparecode"
     let help = "code cleaner"
   end)

module Analysis =
  False(struct
          let option_name = "-sparecode"
          let help = "perform a spare code analysis"
        end)
let () = Analysis.add_aliases ["-sparecode-analysis"]

module Annot =
  True(struct
         let option_name = "-sparecode-annot"
         let help = "select more things to keep every reachable annotation"
       end)

module GlobDecl =
  False(struct
          let option_name = "-rm-unused-globals"
          let help = ("only remove unused global types and variables "^
                       "(automatically done by -sparecode-analysis)")
        end)
 

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
