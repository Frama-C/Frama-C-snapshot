(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

module NoBranches = False
    (struct
      let option_name = "-loop-no-branches"
      let help = "Modify the algorithm use to estimate the slevel: ignore \
                  branching due to ifs and always merge after loops"
    end)
