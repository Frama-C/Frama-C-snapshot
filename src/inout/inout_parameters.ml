(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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
     let descr = "operational, imperative and all kinds of inputs/outputs"
   end)


module ForceDeref =
  Action
    (struct
       let option_name = "-deref"
       let descr = "force deref computation (undocumented)"
     end)

module ForceAccessPath =
  Action
    (struct
       let option_name = "-access-path"
       let descr = "force the access path information to be computed"
     end)

module ForceOut =
  Action
    (struct
       let option_name = "-out"
       let descr = "force internal out display; this is an over-approximation of the set of written tsets"
     end)

module ForceExternalOut =
  Action
    (struct
       let option_name = "-out-external"
       let descr = "force external out display; this is an over-approximation of the set of written tsets excluding locals"
     end)
let () = ForceExternalOut.do_not_save ()

module ForceInput =
   Action
     (struct
	let option_name = "-input"
	let descr = "force display of imperative inputs computed in a linear pass. Locals and function parameters are not displayed"
      end)

module ForceInputWithFormals =
  Action
    (struct
       let option_name = "-input-with-formals"
       let descr = "force display of imperative inputs computed in a linear pass. Function parameters are displayed, locals are not"
     end)

module ForceInout =
  Action
    (struct
       let option_name = "-inout"
       let descr = "display operational inputs, an over-approximation of the set of locations whose initial value is used; and the sure outputs, which are undocumented"
     end)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
