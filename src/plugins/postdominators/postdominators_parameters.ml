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
     let name = "postdominators"
     let shortname = "postdominators"
     let help = "computing postdominators of statements"
   end)

module DotPostdomBasename =
  Empty_string
    (struct
       let option_name = "-dot-postdom"
       let arg_name = "f"
       let help = "put the postdominators of function <f> in basename.f.dot"
     end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
