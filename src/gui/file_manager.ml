(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

let add_files host_window =
  Gtk_helper.source_files_chooser
    (host_window :> Gtk_helper.source_files_chooser_host)
    (Parameters.Files.get ())
    (fun filenames ->
       Parameters.Files.set filenames;
       if Ast.is_computed () then 
	 Gui_parameters.warning "Input files unchanged. Ignored."
       else begin
	 File.init_from_cmdline ();
	 host_window#reset ()
       end)

let filename: string option ref = ref None
  (* [None] for opening the 'save as' dialog box;
     [Some f] for saving in file [f] *)

let save_in (host_window: Design.main_window_extension_points) parent name =
  try
    Project.save_all name;
    filename := Some name
  with Project.IOError s ->
    host_window#error ~parent "Cannot save: %s" s

(** Save a project file. Choose a filename *)
let save_file_as (host_window: Design.main_window_extension_points) =
  let dialog = 
    GWindow.file_chooser_dialog
      ~action:`SAVE
      ~title:"Save the current session"
      ~parent:host_window#main_window () 
  in
  (*dialog#set_do_overwrite_confirmation true ; only in later lablgtk2 *)
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `SAVE `SAVE ;
  host_window#protect ~cancelable:true ~parent:(dialog :> GWindow.window_skel)
    (fun () ->
       match dialog#run () with
       | `SAVE ->
	   Extlib.may
	     (save_in host_window (dialog :> GWindow.window_skel))
	     dialog#filename
       | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy ()

let save_file (host_window: Design.main_window_extension_points) =
  match !filename with
  | None -> save_file_as host_window
  | Some f -> 
      save_in host_window (host_window#main_window :> GWindow.window_skel) f

(** Load a project file *)
let load_file (host_window: Design.main_window_extension_points) =
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Load a saved session"
    ~parent:host_window#main_window () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  host_window#protect ~cancelable:true ~parent:(dialog:>GWindow.window_skel)
    (fun () -> match dialog#run () with
     | `OPEN ->
	 begin match dialog#filename with
	 | None -> ()
	 | Some f ->
             Project.load_all f
	 end
     | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy ()

let insert (host_window: Design.main_window_extension_points) =
  let _, filemenu = host_window#menu_manager#add_menu "_File" in
  let file_items =
    host_window#menu_manager#add_entries
      filemenu
      [ 
	Menu_manager.ToolMenubar(`FILE, "Set C source files"),
	(fun () -> add_files host_window);
	Menu_manager.ToolMenubar(`SAVE, "Save session"), 
	(fun () -> save_file host_window);
	Menu_manager.ToolMenubar(`SAVE_AS, "Save session as"),
	(fun () -> save_file_as host_window);
	Menu_manager.ToolMenubar(`REVERT_TO_SAVED, "Load session"),
	(fun () -> load_file host_window) 
      ]
  in
  file_items.(1)#add_accelerator `CONTROL 's';
  file_items.(3)#add_accelerator `CONTROL 'l';
  let stock = `QUIT in
  let quit_item =
    host_window#menu_manager#add_entries
      filemenu
      [ Menu_manager.Menubar(Some stock, "Exit Frama-C"), Cmdline.bail_out ]
  in
  quit_item.(0)#add_accelerator `CONTROL 'q';
  ignore
    (host_window#menu_manager#add_entries
       filemenu 
       ~pos:0
       [ Menu_manager.Toolbar(stock, "Exit Frama-C"), Cmdline.bail_out ])

(** Register this dialog in main window menu bar *)
let () = Design.register_extension insert

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
