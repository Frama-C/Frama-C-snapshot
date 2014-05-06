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
     let name = "obfuscator"
     let shortname = "obfuscator"
     let help = "objuscator for confidential code"
   end)

module Run =
  False
    (struct
       let option_name = "-obfuscate"
       let help = "print an obfuscated version of the input files and exit.\n\
Disable any other Frama-C analysis."
     end)

module Dictionary =
  EmptyString
    (struct
      let option_name = "-obfuscator-dictionary"
      let arg_name = "f"
      let help = "generate the dictionary into file <f> (on stdout by default)"
     end)

module Literal_string =
  EmptyString
    (struct
      let option_name = "-obfuscator-string-dictionary"
      let arg_name = "f"
      let help = "generate the dictionary of literal strings into file <f> \
(in the same place than the code by default)"
     end)

let states = [ Run.self; Dictionary.self; Literal_string.self ]

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
