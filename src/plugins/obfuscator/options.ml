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
  Empty_string
    (struct
      let option_name = "-obfuscator-dictionary"
      let arg_name = "f"
      let help = "generate the dictionary into file <f> (on stdout by default)"
     end)

module Literal_string =
  Empty_string
    (struct
      let option_name = "-obfuscator-string-dictionary"
      let arg_name = "f"
      let help = "generate the dictionary of literal strings into file <f> \
(in the same place than the code by default)"
     end)

let states = [ Run.self; Dictionary.self; Literal_string.self ]

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
