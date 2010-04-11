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

include Plugin.Register
  (struct
     let name = "impact"
     let shortname = "impact"
     let descr = "impact analysis (experimental)"
   end)

module Pragma =
  StringSet
    (struct
       let option_name = "-impact-pragma"
       let arg_name = "f1, ..., fn"
       let descr = "use the impact pragmas in the code of functions f1,...,fn"
     end)

module Print =
  False
    (struct
       let option_name = "-impact-print"
       let descr = "print the impacted stmt"
     end)

module Slicing =
  False
    (struct
       let option_name = "-impact-slicing"
       let descr = "slice from the impacted stmt"
     end)

let is_on () = not (Pragma.is_empty ())

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
