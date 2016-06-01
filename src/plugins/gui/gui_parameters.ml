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

let () = Plugin.is_config_visible ()
include Plugin.Register
    (struct
      let name = "GUI"
      let shortname = "gui"
      let help = "Graphical User Interface"
    end)

let () = Parameter_customize.do_not_projectify ()
module Project_name =
  Empty_string
    (struct
      let option_name = "-gui-project"
      let arg_name = "p"
      let help = "run the GUI on project <p> after applying the \
                  command line actions (by default, it is run on the default project"
    end)

(* Used mainly for debugging purposes. No need to show it to the user *)
let () = Parameter_customize.is_invisible ()
module Undo =
  True
    (struct
      let option_name = "-gui-undo"
      let help = "possible to click on the `undo' button (set by default)"
    end)

module Theme =
  String
    (struct
      let option_name = "-gui-theme"
      let arg_name = "s"
      let help =
        "choose the theme <s> of the GUI (available: 'default', 'colorblind')"
      let default = "default"
    end)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
