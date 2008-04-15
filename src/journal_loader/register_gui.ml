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

open Type

let load_journal_file (main_ui:Design.main_window_extension_points) =
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Open journal"
    ~parent:main_ui#main_window 
    () 
  in
  dialog#add_button_stock `CANCEL `CANCEL;
  dialog#add_button_stock `OPEN `OPEN;
  let load = begin match dialog#run () with
    | `OPEN -> dialog#filename
    | `DELETE_EVENT | `CANCEL -> None
  end
  in
  dialog#destroy ();
  match load with
  | Some filename -> 
      main_ui#reset ();
      (try 
	 Dynamic.apply 
	   "Journal_loader.load" 
	   (func string unit) 
	   filename
       with 
       | FunTbl.Not_Registered fun_name -> 
	   Format.printf "Not registered: %s@." fun_name;
	   assert false
       | Journal.LoadingError msg ->
	   Format.eprintf "Error while loading journal: %s@." msg)
  | None -> 
      ()

let ui_info = "<ui>
  <menubar name='MenuBar'>
    <menu action='JournalMenu'>
      <menuitem action='LoadJournal' />
    </menu>
  </menubar>
</ui>"

let () = Design.register_extension
  (fun window ->
     GAction.add_actions window#actions
       [ GAction.add_action
	   "LoadJournal"
	   ~label:"_Load journal"
	   ~accel:"<control>J"
	   ~stock:`OPEN
	   ~callback:(fun _ -> load_journal_file window) ];
     ignore (window#ui_manager#add_ui_from_string ui_info))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../../ -j"
End:
*)
