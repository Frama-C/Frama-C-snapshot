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
     let name = "impact"
     let shortname = "impact"
     let help = "impact analysis (experimental)"
   end)

module Pragma =
  StringSet
    (struct
       let option_name = "-impact-pragma"
       let arg_name = "f1, ..., fn"
       let help = "use the impact pragmas in the code of functions f1,...,fn"
     end)

module Print =
  False
    (struct
       let option_name = "-impact-print"
       let help = "print the impacted stmt"
     end)

module Reason =
  False
    (struct
       let option_name = "-impact-graph"
       let help = "build a graph that explains why a statement is in the set \
                   of impacted nodes"
     end)

module Slicing =
  False
    (struct
       let option_name = "-impact-slicing"
       let help = "slice from the impacted stmt"
     end)

module Skip =
  StringSet
    (struct
       let arg_name = "v1,...,vn"
       let help = "consider that those variables are not impacted"
       let option_name = "-impact-skip"
     end)

let () = Parameter_customize.set_negative_option_name "-impact-not-in-callers"
module Upward =
  True
    (struct
       let  option_name = "-impact-in-callers"
       let help = "compute compute impact in callers as well as in callees"
     end)

let is_on () = not (Pragma.is_empty ())

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
