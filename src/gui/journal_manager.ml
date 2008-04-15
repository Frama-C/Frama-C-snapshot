(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

let start_stop_callback toggle =
  if toggle#get_active then Journal.start ()
  else Journal.stop ()

let ui_info = "<ui>
  <menubar name='MenuBar'>
    <menu action='JournalMenu'>
      <menuitem action='IsActive' />
    </menu>
  </menubar>
</ui>"

let () = Design.register_extension
  (fun window ->
     GAction.add_actions window#actions
       [ GAction.add_action "JournalMenu" ~label:"_Journal";
	 GAction.add_toggle_action
	   "IsActive"
	   ~label:"_Is active"
	   ~active:(not (Cmdline.Journal.Disable.get ()))
	   ~callback:start_stop_callback ];
     ignore (window#ui_manager#add_ui_from_string ui_info))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../../ -j"
End:
*)
