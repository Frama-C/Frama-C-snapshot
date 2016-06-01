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
    | _, None -> (** the user stopped or an error occured  *)
        host_window#reset ()
    | Some old_helt, Some () ->
        let new_helt = History.translate_history_elt old_helt in
        Extlib.may History.push new_helt;
        host_window#reset ();
        (** The buffer is not ready yet, modification of its vadjustement
            is unrealiable *)
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
      ]
  in
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
