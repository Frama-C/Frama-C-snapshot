(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

let compare_prj (_p1, n1) (_p2, n2) =
  String.compare n1 n2

let projects_list ?(filter=fun _ -> true) () =
  let projects =
    Project.fold_on_projects
      (fun acc p ->
         if filter p then ((p, Project.get_unique_name p) :: acc) else acc)
      []
  in
  List.sort compare_prj projects

(* use the same order than the projects list.
   is not possible with an hashtbl.
   So we use a reference over a set of couple *)
module PrjRadiosSet =
  FCSet.Make
    (struct
      type t = (Project.t * string) * GButton.radio_button * GMenu.menu_item
      let compare (p1, _, _) (p2, _, _) = compare_prj p1 p2
    end)

let project_radios : PrjRadiosSet.t ref = ref PrjRadiosSet.empty

(** Create a new project *)
let new_project main_ui =
  Gtk_helper.source_files_chooser
    (main_ui :> Gtk_helper.source_files_chooser_host)
    []
    (fun filenames ->
       let project = Project.create "interactive" in
       let init () =
         Kernel.Files.set filenames;
         File.init_from_cmdline ()
       in
       Project.on project init ();
       Project.set_current project)

let delete_project project =
  let name = Project.get_unique_name project in
  let ok =
    GToolbox.question_box
      ~title:(Format.sprintf "Deleting project %S" name)
      ~buttons:[ "Confirm"; "Cancel" ]
      (Format.sprintf "Do you want to destroy project %S?" name)
  in
  if ok = 1 then begin
    (try
       Project.remove ~project ()
     with Project.Cannot_remove _ ->
       let p = Project.create "default" in
       Project.on p File.init_from_cmdline ();
       try Project.remove () with Project.Cannot_remove _ -> assert false)
  end

module Filenames = Hashtbl.Make(Project)
let filenames : string Filenames.t = Filenames.create 7

let save_in
    (host_window: Design.main_window_extension_points) parent project name =
  try
    Project.save ~project name;
    Filenames.replace filenames project name
  with Project.IOError s ->
    host_window#error ~parent "Cannot save: %s" s

let save_project_as (main_ui: Design.main_window_extension_points) project =
  let dialog =
    GWindow.file_chooser_dialog
      ~action:`SAVE
      ~title:("Save project " ^ Project.get_unique_name project)
      ~parent:main_ui#main_window ()
  in
  (*dialog#set_do_overwrite_confirmation true ; only in later lablgtk2 *)
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `SAVE `SAVE ;
  main_ui#protect ~cancelable:true ~parent:(dialog :> GWindow.window_skel)
    (fun () ->
       match dialog#run () with
       | `SAVE ->
           Extlib.may
             (save_in main_ui (dialog :> GWindow.window_skel) project)
             dialog#filename
       | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy ()

let save_project (host_window: Design.main_window_extension_points) project =
  try
    save_in
      host_window
      (host_window#main_window :> GWindow.window_skel)
      project
      (Filenames.find filenames project)
  with Not_found ->
    save_project_as host_window project

let load_project (host_window: Design.main_window_extension_points) =
  let dialog = GWindow.file_chooser_dialog
      ~action:`OPEN
      ~title:"Load a saved project"
      ~parent:host_window#main_window ()
  in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  host_window#protect ~cancelable:true ~parent:(dialog:>GWindow.window_skel)
    (fun () -> match dialog#run () with
       | `OPEN ->
           begin match dialog#filename with
             | None -> ()
             | Some f ->
                 (try ignore (Project.load f)
                  with Project.IOError s | Failure s ->
                    host_window#error
                      ~reset:true ~parent:(dialog:>GWindow.window_skel)
                      "Cannot load: %s" s)
           end
       | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy ()

let mk_project_markup p =
  let name = Project.get_unique_name p in
  if Project.is_current p then "<b>" ^ name ^ "</b>" else name

let reset ?filter (menu: GMenu.menu) =
  (* Do not reset all if there is no change. *)
  let pl = projects_list ?filter () in
  let same_projects =
    (* use that project_radios and pl are sorted in the same way *)
    try
      let rest =
        PrjRadiosSet.fold
          (fun (p1, _, _) acc ->
             match acc with
             | [] -> raise Exit
             | p2 :: acc ->
                 if compare_prj p1 p2 = 0 then acc else raise Exit)
          !project_radios
          pl
      in
      rest = []
    with Exit ->
      false
  in
  if same_projects then begin
    (* update the item status according to the current project anyway *)
    PrjRadiosSet.iter
      (fun ((p, _), r, i) ->
         r#set_active (Project.is_current p);
         let widgets = i#children in
         match widgets with
         | [ w ] ->
           (try
              let label = GMisc.label_cast w in
              label#set_label (mk_project_markup p);
              label#set_use_markup true
            with Gobject.Cannot_cast (t1,t2) ->
              Gui_parameters.warning
                "Child of project menu item of kind %s while %s was expected"
                t1 t2)
         | [] -> Gui_parameters.warning "Project menu item without child"
         | _ -> Gui_parameters.warning "Project menu item with %d child"
                  (List.length widgets)
      )
      !project_radios;
    false
  end else begin
    PrjRadiosSet.iter (fun (_, _, i) -> menu#remove i) !project_radios;
    project_radios := PrjRadiosSet.empty;
    true
  end

let duplicate_project project =
  ignore
    (Project.create_by_copy ~last:false ~src:project (Project.get_name project))

let rec rename_project
    (main_ui: Design.main_window_extension_points) menu project
  =
  let old = Project.get_unique_name project in
  let s =
    Gtk_helper.input_string
      ~parent:main_ui#main_window
      ~title:"Renaming project"
      (Format.sprintf "New name for project %S:" old)
  in
  (match s with
   | None -> ()
   | Some s ->
     try
       ignore (Project.from_unique_name s);
       main_ui#error "Project of name %S already exists" s
     with Project.Unknown_project ->
       Project.set_name project s);
  recompute main_ui menu

and mk_project_entry window menu ?group p =
  let pname = Project.get_unique_name p in
  let markup = mk_project_markup p in
  let item = GMenu.menu_item ~packing:menu#append () in
  let _label = GMisc.label ~markup ~xalign:0. ~packing:item#add () in
  let submenu = GMenu.menu ~packing:item#set_submenu () in
  let current = GMenu.menu_item ~packing:submenu#append () in
  let p_item = GButton.radio_button
      ?group
      ~active:(Project.is_current p)
      ~packing:current#add
      ~label:"Set current"
      ()
  in
  let callback () = Project.set_current p in
  ignore (current#connect#activate ~callback);
  project_radios := PrjRadiosSet.add ((p, pname), p_item, item) !project_radios;
  let add_action stock text callback =
    let image = GMisc.image ~xalign:0. ~stock () in
    let image = image#coerce in
    let item =
      Gtk_helper.image_menu_item ~image ~text ~packing:submenu#append
    in
    ignore (item#connect#activate ~callback)
  in
  add_action `COPY "Duplicate project"
    (fun () -> duplicate_project p);
  add_action `DELETE "Delete project" (fun () -> delete_project p);
  add_action `SAVE "Save project" (fun () -> save_project window p);
  add_action `SAVE_AS "Save project as" (fun () -> save_project_as window p);
  add_action
    `SELECT_FONT "Rename project" (fun () -> rename_project window menu p);
  p_item

and make_project_entries ?filter window menu =
  match projects_list ?filter () with
  | [] -> assert (filter <> None)
  | (pa, _name) :: tl ->
      let mk = mk_project_entry window menu in
      let pa_item = mk pa in
      let group = pa_item#group in
      List.iter (fun (pa, _) -> ignore (mk ~group pa)) tl

and recompute ?filter window menu =
  let is_reset = reset ?filter menu in
  if is_reset then make_project_entries ?filter window menu

open Menu_manager

(** Register this dialog in main window menu bar *)
let () =
  Design.register_extension
    (fun window ->
       let menu_manager = window#menu_manager () in
       let _item, menu = menu_manager#add_menu "_Project" in
       let constant_items =
         menu_manager#add_entries
           menu
           [
             menubar ~icon:`NEW "New project"
               (Unit_callback (fun () -> new_project window));
             menubar ~icon:`REVERT_TO_SAVED "Load project"
               (Unit_callback (fun () -> load_project window));
             menubar ~icon:`COPY "Duplicate current project"
               (Unit_callback
                  (fun () -> duplicate_project (Project.current())));
             menubar ~icon:`DELETE "Delete current project"
               (Unit_callback (fun () -> delete_project (Project.current ())));
             menubar ~icon:`SELECT_FONT "Rename current project"
               (Unit_callback
                  (fun () -> rename_project window menu (Project.current ())));
           ]
       in
       let new_item = constant_items.(0) in
       new_item#add_accelerator `CONTROL 'n';
       constant_items.(3)#add_accelerator `CONTROL 'd';
       ignore (GMenu.separator_item ~packing:menu#append ());
       let callback_prj _p = recompute window menu in
       let callback_rm_prj p =
         let filter p' = not (Project.equal p p') in
         recompute ~filter window menu
       in
       let hook () = recompute window menu in
       Project.register_create_hook callback_prj;
       Project.register_after_set_current_hook ~user_only:true callback_prj;
       Project.register_before_remove_hook callback_rm_prj;
       Project.register_after_load_hook hook;
       Project.register_after_global_load_hook hook;
       recompute window menu)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
