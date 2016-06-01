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
compile-command: "make -C ../../.."
End:
*)
