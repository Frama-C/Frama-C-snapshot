(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

let add_files (host_window: Design.main_window_extension_points) =
  Gtk_helper.source_files_chooser
    (host_window :> Gtk_helper.source_files_chooser_host)
    (Kernel.Files.get ())
    (fun filenames ->
       Kernel.Files.set filenames;
       if Ast.is_computed () then
         Gui_parameters.warning "Input files unchanged. Ignored."
       else begin
         File.init_from_cmdline ();
         host_window#reset ()
       end)

let filename: string option ref = ref None
(* [None] for opening the 'save as' dialog box;
   [Some f] for saving in file [f] *)

let reparse (host_window: Design.main_window_extension_points) =
  let old_helt = History.get_current () in
  let old_scroll =
    let adj = host_window#source_viewer_scroll#vadjustment in
    (adj#value -. adj#lower ) /. (adj#upper -. adj#lower)
  in
  let succeeded = host_window#full_protect ~cancelable:true
      (fun () ->
         let files = Kernel.Files.get () in
         Kernel.Files.set [];
         Kernel.Files.set files;
         Ast.compute ();
         !Db.Main.play ();
         Source_manager.clear host_window#original_source_viewer)
  in
  begin match old_helt, succeeded with
    | None, _ -> (** no history available before reparsing *)
        host_window#reset ()
    | _, None -> (** the user stopped or an error occurred  *)
        host_window#reset ()
    | Some old_helt, Some () ->
        let new_helt = History.translate_history_elt old_helt in
        Extlib.may History.push new_helt;
        host_window#reset ();
        (** The buffer is not ready yet, modification of its vadjustement
            is unreliable *)
        let set () =
          let adj = host_window#source_viewer_scroll#vadjustment in
          adj#set_value (old_scroll *. (adj#upper-.adj#lower) +. adj#lower)
        in
        Wutil.later set
  end

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

(** Open the Preferences dialog *)
let preferences (host_window: Design.main_window_extension_points) =
  let dialog =
    GWindow.dialog ~modal:true
      ~border_width:8 ~title:"Preferences" ~parent:host_window#main_window ()
  in
  let main_box = dialog#vbox in
  main_box#set_spacing 10;
  let theme_frame = GBin.frame ~label:"Property bullets theme" () in
  main_box#pack theme_frame#coerce;
  let theme_box = GPack.vbox ~spacing:2 ~border_width:10 () in
  theme_frame#add theme_box#coerce;
  (* Themes are directories in share/theme. *)
  let themes_path = !Wutil.share ^ "/theme/" in
  let themes = Array.to_list (Sys.readdir themes_path) in
  let is_theme_directory name = Sys.is_directory (themes_path ^ name) in
  let themes = List.filter is_theme_directory themes in
  (* The current theme is kept in the configuration file. *)
  let active_theme =
    Gtk_helper.Configuration.find_string ~default:"default" "theme"
  in
  let theme_group = new Widget.group "" in
  let build_theme_button name =
    let label = Transitioning.String.capitalize_ascii name in
    let widget = theme_group#add_radio ~label ~value:name () in
    theme_box#add widget#coerce
  in
  (* Builds the theme buttons, and sets the active theme. *)
  List.iter build_theme_button themes;
  theme_group#set active_theme;
  (* External editor command. *)
  let default = "emacs +%d %s" in
  let editor = Gtk_helper.Configuration.find_string ~default "editor" in
  let editor_frame = GBin.frame ~label:"Editor command" () in
  main_box#pack editor_frame#coerce;
  let editor_box = GPack.vbox ~spacing:5 ~border_width:10 () in
  editor_frame#add editor_box#coerce;
  let text = "Command to open an external editor \
              on Ctrl-click in the original source code. \n\
              Use %s for file name and %d for line number."
  in
  let label = GMisc.label ~xalign:0. ~line_wrap:true ~text () in
  editor_box#pack label#coerce;
  let editor_input = GEdit.entry ~width_chars:30 ~text:editor () in
  editor_box#pack editor_input#coerce ~expand:true;
  (* Save and cancel buttons. *)
  let hbox_buttons = dialog#action_area in
  let packing = hbox_buttons#pack ~expand:true ~padding:3 in
  let wb_ok = GButton.button ~label:"Save" ~packing () in
  let wb_cancel = GButton.button ~label:"Cancel" ~packing () in
  wb_ok#grab_default ();
  let f_ok () =
    (* retrieve chosen preferences from dialog *)
    Gui_parameters.debug "saving preferences";
    Gtk_helper.Configuration.set "theme"
      (Gtk_helper.Configuration.ConfString theme_group#get);
    Gtk_helper.Configuration.set "editor"
      (Gtk_helper.Configuration.ConfString editor_input#text);
    Gtk_helper.Configuration.save ();
    dialog#destroy ();
    (* Reloads the icons from the theme, and resets the icons used as property
       status bullets.*)
    Gtk_helper.Icon.clear ();
    Design.Feedback.declare_markers host_window#source_viewer;
  in
  let f_cancel () =
    Gui_parameters.debug "canceled, preferences not saved";
    dialog#destroy ()
  in
  ignore (wb_ok#connect#clicked f_ok);
  ignore (wb_cancel#connect#clicked f_cancel);
  (* the enter key is linked to the ok action *)
  (* the escape key is linked to the cancel action *)
  dialog#misc#grab_focus ();
  dialog#show ()

let insert (host_window: Design.main_window_extension_points) =
  let menu_manager = host_window#menu_manager () in
  let _, filemenu = menu_manager#add_menu "_File" in
  let file_items =
    menu_manager#add_entries
      filemenu
      [
        Menu_manager.toolmenubar
          ~icon:`FILE ~label:"Source files"
          ~tooltip:"Create a new session from existing C files"
          (Menu_manager.Unit_callback (fun () -> add_files host_window));
        Menu_manager.toolmenubar
          ~icon:`REFRESH ~label:"Reparse"
          ~tooltip:"Reparse source files, and replay analyses"
          (Menu_manager.Unit_callback (fun () -> reparse host_window));
        Menu_manager.toolmenubar `REVERT_TO_SAVED "Load session"
          (Menu_manager.Unit_callback (fun () -> load_file host_window));
        Menu_manager.toolmenubar `SAVE "Save session"
          (Menu_manager.Unit_callback (fun () -> save_file host_window));
        Menu_manager.menubar ~icon:`SAVE_AS "Save session as"
          (Menu_manager.Unit_callback (fun () -> save_file_as host_window));
        Menu_manager.menubar ~icon:`PREFERENCES "Preferences"
          (Menu_manager.Unit_callback (fun () -> preferences host_window));
      ]
  in
  file_items.(5)#add_accelerator `CONTROL 'p';
  file_items.(3)#add_accelerator `CONTROL 's';
  file_items.(2)#add_accelerator `CONTROL 'l';
  let stock = `QUIT in
  let quit_item =
    menu_manager#add_entries
      filemenu
      [ Menu_manager.menubar ~icon:stock "Exit Frama-C"
          (Menu_manager.Unit_callback Cmdline.bail_out) ]
  in
  quit_item.(0)#add_accelerator `CONTROL 'q';
  ignore
    (menu_manager#add_entries
       filemenu
       ~pos:0
       [ Menu_manager.toolbar ~icon:stock ~label:"Exit" ~tooltip:"Exit Frama-C"
           (Menu_manager.Unit_callback Cmdline.bail_out)])

(** Register this dialog in main window menu bar *)
let () = Design.register_extension insert

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
