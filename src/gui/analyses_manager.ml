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

let filter name extension =
  let f = GFile.filter ~name () in
  f#add_pattern ("*" ^ extension);
  f

let run title filter_name extension loader
    (host_window: Design.main_window_extension_points)
    =
  let dialog =
    GWindow.file_chooser_dialog
      ~action:`OPEN
      ~title
      ~parent:host_window#main_window ()
  in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `EXECUTE `EXECUTE ;
  dialog#add_filter (filter filter_name extension);
  host_window#protect ~cancelable:true ~parent:(dialog :> GWindow.window_skel)
    (fun () ->
       match dialog#run () with
       | `EXECUTE ->
           let run f =
             loader f;
             !Db.Main.play ();
             host_window#reset ()
           in
           Extlib.may run dialog#filename;
       | `DELETE_EVENT | `CANCEL ->
           ());
  dialog#destroy ()

let run_script =
  run "Execute an OCaml script" "OCaml sources" ".ml" Dynamic.load_script

let run_module =
  run "Load an OCaml object file" "OCaml objects" Dynamic.object_file_extension
    Dynamic.load_module

let insert (main_ui: Design.main_window_extension_points) =
  let menu_manager = main_ui#menu_manager () in
  let stop = ref (fun () -> assert false) (* delayed *) in
  let stop_sensitive = ref false (* can the stop button be clicked? *) in
  let default_analyses_items =
    menu_manager#add_plugin
      [
        Menu_manager.toolmenubar ~icon:`PROPERTIES
          ~label:"Analyses" ~tooltip:"Configure and run analyses"
          (Menu_manager.Unit_callback main_ui#launcher);
        Menu_manager.menubar ~icon:`EXECUTE "Compile and run an OCaml Script"
          (Menu_manager.Unit_callback (fun () -> run_script main_ui));
        Menu_manager.menubar "Load and run an OCaml Module"
          (Menu_manager.Unit_callback (fun () -> run_module main_ui));
        Menu_manager.toolbar ~sensitive:(fun () -> !stop_sensitive) ~icon:`STOP
          ~label:"Stop" ~tooltip:"Stop currently running analyses"
          (Menu_manager.Unit_callback (fun () -> !stop ()));
      ]
  in
  default_analyses_items.(0)#add_accelerator `CONTROL 'r';
  let stop_button = Extlib.the default_analyses_items.(3)#tool_button in
  let old_progress = ref !Db.progress in
  stop :=
    (fun () ->
       Db.progress :=
         (fun () ->
            Db.progress := !old_progress;
            raise Db.Cancel));

  Gtk_helper.register_locking_machinery
    ~lock_last:true
    ~lock:(fun cancelable ->
             if !stop_sensitive then Gui_parameters.warning
               "Inconsistent state for stop button. Ignoring.";
             old_progress := !Db.progress;
             menu_manager#set_sensitive false;
             if cancelable then (stop_button#misc#set_sensitive true;
                                 stop_sensitive := true);
          )
    ~unlock:(fun () ->
               Db.progress := !old_progress;
               menu_manager#set_sensitive true;
               stop_button#misc#set_sensitive false;
               stop_sensitive := false;
            )
    ()

let () = Design.register_extension insert

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
