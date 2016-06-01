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
     let name = "security-slicing"
     let shortname = "security-slicing"
     let help = "security slicing (experimental, undocumented)"
   end)

module Slicing =
  False
    (struct
       let option_name = "-security-slicing"
       let help = "perfom the security slicing analysis"
     end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
