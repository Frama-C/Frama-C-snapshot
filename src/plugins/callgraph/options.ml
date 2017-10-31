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

let name = "callgraph"

include
  Plugin.Register
    (struct
      let name = name
      let shortname = "cg"
      let help = "automatically compute the callgraph of the program. \
                  Using Eva might improve the precision of this plug-in"
    end)

module Filename =
  Empty_string
    (struct
      let option_name = "-cg"
      let arg_name = "filename"
      let help = "dump the callgraph to the file \
                  <filename> in dot format"
    end)

module Services =
  True
    (struct
      let option_name = "-cg-services"
      let help = "compute and display services (groups of related \
                  functions which seem to provide common functionalities) \
                  from the callgraph"
    end)

module Roots =
  Kernel_function_set
    (struct
      let option_name = "-cg-roots"
      let arg_name = ""
      let help = "if not empty, display only the functions of the callgraph \
                  reachable from the given functions"
    end)

module Service_roots =
  Kernel_function_set
    (struct
      let option_name = "-cg-service-roots"
      let arg_name = ""
      let help = "when computing callgraph services (see " ^
                 Services.option_name ^
                 "), use the given functions (and their immediate children) \
                  as service roots. If none, use the main function if any; \
                  else use every uncalled function"
    end)

module Function_pointers =
  True
    (struct
      let option_name = "-cg-function-pointers"
      let help = "when Eva has not been computed, safely over-approximate \
                  callees in presence of function pointers; \
                  always done when Eva has been previously computed. \
                  WARNING: this option is unsound"
    end)

module Uncalled =
  True
    (struct
      let option_name = "-cg-uncalled"
      let help = "add the uncalled functions to the callgraph \
                  (the main function is always added anyway)"
    end)

module Uncalled_leaf =
  False
    (struct
      let option_name = "-cg-uncalled-leaf"
      let help = "add to the callgraph the uncalled functions that, \
                  themselves, do not call any function"
    end)

let dump output g =
  let file = Filename.get () in
  feedback ~level:2 "dumping the graph into file %s" file;
  try
    let cout = open_out file in
    output cout g;
    close_out cout
  with e ->
    error
      "error while dumping the syntactic callgraph: %s"
      (Printexc.to_string e)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
