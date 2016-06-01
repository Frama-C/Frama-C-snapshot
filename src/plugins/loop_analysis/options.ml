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
      let name = "loop"
      let shortname = "loop"
      let help = "Find number of iterations in loops, and slevel value"
    end)

module Run = False
    (struct
      let option_name = "-loop"
      let help = "Launch loop analysis"
    end)

module MaxIterations = Int
    (struct
      let option_name = "-loop-max-iterations"
      let arg_name = "num"
      let default = 1000
      let help = "If slevel is found to be higher than this number in a loop"
                 ^ "force the use of merge-after-loop (default: 1000)"
    end)

module MaxSlevel = Int
    (struct
      let option_name = "-loop-max-slevel"
      let arg_name = "num"
      let default = 10000
      let help = "If slevel is found to be higher than this number,"
                 ^ "set slevel to 0 instead (default: 10000)"
    end)
