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

let name = "semantic callgraph"

include
  Plugin.Register
    (struct
       let name = name
       let shortname = "scg"
       let help = "semantic stratified callgraph"
     end)

module Filename =
  EmptyString
    (struct
       let option_name = "-scg"
       let arg_name = "filename"
       let help = "dump the semantic stratified callgraph to the file \
<filename> in dot format"
     end)

module InitFunc =
  StringSet
    (struct
       let option_name = "-scg-init-func"
       let arg_name = ""
       let help = "use the given functions as a root service for the scg \
(you can add as many comma-separated functions as you want; if no function is \
declared, then root services are initialized with functions with no callers)"
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
