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
     let name = "inout"
     let shortname = "inout"
     let help = "operational, imperative and all kinds of inputs/outputs"
   end)


module ForceDeref =
  Action
    (struct
       let option_name = "-deref"
       let help = "force deref computation (undocumented)"
       let kind = `Tuning
     end)

module ForceAccessPath =
  Action
    (struct
       let option_name = "-access-path"
       let help = "force the access path information to be computed"
       let kind = `Tuning
     end)

module ForceOut =
  Action
    (struct
       let option_name = "-out"
       let help = "internal out display; this is an over-approximation of the set of written tsets"
       let kind = `Tuning
     end)

module ForceExternalOut =
  Action
    (struct
       let option_name = "-out-external"
       let help = "external out display; this is an over-approximation of the set of written tsets excluding locals"
       let kind = `Tuning
     end)

module ForceInput =
   Action
     (struct
	let option_name = "-input"
	let help = "display imperative inputs. Locals and function parameters are not displayed"
        let kind = `Tuning
      end)

module ForceInputWithFormals =
  Action
    (struct
       let option_name = "-input-with-formals"
       let help = "display imperative inputs. Function parameters are displayed, locals are not"
       let kind = `Tuning
     end)

module ForceInout =
  Action
    (struct
       let option_name = "-inout"
       let help = "display operational inputs, an over-approximation of the set of locations whose initial value is used; and the sure outputs, an under-approximation of the set of the certainly written locations"
       let kind = `Tuning
     end)

module ForceInoutExternalWithFormals =
  Action
    (struct
       let option_name = "-inout-with-formals"
       let help = "same as -inout but without local variables and with function parameters"
       let kind = `Tuning
     end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
