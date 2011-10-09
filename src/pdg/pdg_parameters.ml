(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

include Plugin.Register
  (struct
     let name = "pdg"
     let shortname = "pdg"
     let help = "Program Dependence Graph"
   end)

let output = add_group "Output"

module BuildAll =
  WithOutput
    (struct
       let option_name = "-pdg"
       let help =
         "build the dependence graph of each function"
       let kind = `Tuning
       let output_by_default = false
     end)

module BuildFct =
  StringSet
    (struct
       let option_name = "-fct-pdg"
       let arg_name = ""
       let help = "build the dependence graph for the specified function"
       let kind = `Tuning
     end)


let () = Plugin.set_group output
module PrintBw =
  False(struct
          let option_name = "-codpds"
          let help = "print the co-dependencies rather than the dependencies"
          let kind = `Tuning
        end)

let () = Plugin.set_group output
module DotBasename =
  EmptyString
    (struct
       let option_name = "-dot-pdg"
       let arg_name = "basename"
       let help = "put the PDG of function <f> in basename.f.dot"
       let kind = `Tuning
     end)
