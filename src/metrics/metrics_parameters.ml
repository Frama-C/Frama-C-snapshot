(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
       let name = "metrics"
       let shortname = "metrics"
       let help = "syntactic metrics"
     end)

module Print =
  False
    (struct
       let option_name = "-metrics"
       let help = " print some metrics on stdout"
       let kind = `Tuning
     end)

module Dump =
  EmptyString
    (struct
       let option_name = "-metrics-dump"
       let arg_name = ""
       let help = "print some metrics into the specified file"
       let kind = `Tuning
     end)

let is_on () = Print.get () || not (Dump.is_default ())

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
