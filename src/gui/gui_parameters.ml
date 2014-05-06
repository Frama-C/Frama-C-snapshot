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

let () = Plugin.is_config_visible ()
include Plugin.Register
  (struct
     let name = "GUI"
     let shortname = "gui"
     let help = "Graphical User Interface"
   end)

(* Used mainly for debugging purposes. No need to show it to the user *)
let () = Parameter_customize.is_invisible ()
module Undo =
  True
    (struct
      let option_name = "-gui-undo"
      let help = "possible to click on the `undo' button (set by default)"
     end)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
