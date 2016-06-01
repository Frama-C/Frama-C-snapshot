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
       let output_by_default = false
     end)

module BuildFct =
  Kernel_function_set
    (struct
       let option_name = "-fct-pdg"
       let arg_name = ""
       let help = "build the dependence graph for the specified function"
     end)

let () = Parameter_customize.set_group output
module PrintBw =
  False(struct
          let option_name = "-codpds"
          let help = "force option -pdg-print to show the co-dependencies rather than the dependencies"
        end)

let () = Parameter_customize.set_group output
module DotBasename =
  Empty_string
    (struct
       let option_name = "-pdg-dot"
       let arg_name = "basename"
       let help = "put the PDG of function <f> in basename.f.dot"
     end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
