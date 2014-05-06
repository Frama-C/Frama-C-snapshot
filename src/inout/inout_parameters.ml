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
     let name = "inout"
     let shortname = "inout"
     let help = "operational, imperative and all kinds of inputs/outputs"
   end)


module ForceDeref =
  False
    (struct
       let option_name = "-deref"
       let help = "force deref computation (undocumented)"
     end)

module ForceAccessPath =
  False
    (struct
       let option_name = "-access-path"
       let help = "force the access path information to be computed"
     end)

module ForceOut =
  False
    (struct
       let option_name = "-out"
       let help = "Compute internal out. Those are an over-approximation of the set of written locations"
     end)

module ForceExternalOut =
  False
    (struct
       let option_name = "-out-external"
       let help = "Compute external out. Those are an over-approximation of the set of written locations, excluding locals"
     end)

module ForceInput =
  False
     (struct
        let option_name = "-input"
        let help = "Compute imperative inputs. Locals and function parameters are not displayed"
      end)

module ForceInputWithFormals =
  False
    (struct
       let option_name = "-input-with-formals"
       let help = "Compute imperative inputs. Function parameters are displayed, locals are not"
     end)

module ForceInout =
  False
    (struct
       let option_name = "-inout"
       let help = "Compute operational inputs, an over-approximation of the set of locations whose initial value is used; and the sure outputs, an under-approximation of the set of the certainly written locations"
     end)

module ForceCallwiseInout =
  False
    (struct
       let option_name = "-inout-callwise"
       let help = "Compute callsite-wide operational inputs; this results in more precise results for -inout and -out options"
     end)

module ForceInoutExternalWithFormals =
  False
    (struct
       let option_name = "-inout-with-formals"
       let help = "same as -inout but without local variables and with function parameters"
     end)

let () = Parameter_customize.set_group messages
module Output =
  True(struct
    let option_name = "-inout-print"
    let help = "print the results of all the analyzes"
  end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
