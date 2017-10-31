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

let () = Plugin.is_share_visible ()
module P = Plugin.Register
  (struct
     let name = "E-ACSL"
     let shortname = "e-acsl"
     let help = "Executable ANSI/ISO C Specification Language --- runtime \
assertion checker generator"
  end)
module PP = P (* [PP] required to avoid an ocamldoc error in OCaml 4.02 *)
include PP

module Check =
  False
    (struct
      let option_name = "-e-acsl-check"
      let help = "only type check E-ACSL annotated program"
     end)

module Run =
  False
    (struct
      let option_name = "-e-acsl"
      let help = "generate a new project where E-ACSL annotations are \
translated to executable C code"
     end)

module Project_name =
  String
    (struct
      let option_name = "-e-acsl-project"
      let help = "the name of the generated project is <prj> \
(default to \"e-acsl\")"
      let default = "e-acsl"
      let arg_name = "prj"
     end)

module Valid =
  False
    (struct
      let option_name = "-e-acsl-valid"
      let help = "translate annotation which have been proven valid"
     end)

module Prepare =
  False
    (struct
      let option_name = "-e-acsl-prepare"
      let help = "prepare the AST to be directly usable by E-ACSL"
     end)

module Gmp_only =
  False
    (struct
      let option_name = "-e-acsl-gmp-only"
      let help = "always use GMP integers instead of C integral types"
     end)

module Temporal_validity =
  False
    (struct
      let option_name = "-e-acsl-temporal-validity"
      let help = "enable temporal analysis in valid annotations"
     end)

module Full_mmodel =
  False
    (struct
      let option_name = "-e-acsl-full-mmodel"
      let help = "maximal memory-related instrumentation"
     end)

module Builtins =
  String_set
    (struct
      let option_name = "-e-acsl-builtins"
      let arg_name = ""
      let help = "C functions which can be used in the E-ACSL specifications"
     end)

let () = Parameter_customize.set_group help
module Version =
  False
    (struct
      let option_name = "-e-acsl-version"
      let help = "version of plug-in E-ACSL"
     end)

let version () =
  if Version.get () then begin
    Log.print_on_output 
      (fun fmt -> 
	Format.fprintf 
	  fmt
	  "Version of plug-in E-ACSL: %s@?"
	  Local_config.version);
    raise Cmdline.Exit
  end
let () = Cmdline.run_after_configuring_stage version

let must_visit () = Run.get () || Check.get ()

let dkey_analysis = register_category "analysis"
let dkey_dup = register_category "duplication"
let dkey_translation = register_category "translation"
let dkey_typing = register_category "typing"

(*
Local Variables:
compile-command: "make"
End:
*)
