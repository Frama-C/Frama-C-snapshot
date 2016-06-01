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
     let name = "occurrence"
     let shortname = "occurrence"
     let help = "automatically computes where variables are used"
   end)

module Print =
  False
    (struct
       let option_name = "-occurrence"
       let help = "print results of occurrence analysis"
     end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
