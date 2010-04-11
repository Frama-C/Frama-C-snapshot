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

open Cilutil

let compare_prj p1 p2 =
  let n = String.compare (Project.name p1) (Project.name p2) in
  if n = 0 then Project.compare p1 p2 else n

let projects_list () =
  let projects = Project.fold_on_projects (fun acc p -> p :: acc) [] in
  List.sort compare_prj projects

(* use the same order than the projects list.
   is not possible with an hashtbl. 
   So we use a reference over a set of couple *)
module PrjRadiosSet = 
  Set.Make
    (struct 
       type t = Project.t * GMenu.radio_menu_item 
       let compare (p1, _) (p2, _) = compare_prj p1 p2 
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
	 Parameters.Files.set filenames;
	 File.init_from_cmdline ()
       in
       Project.on project init ();
       Project.set_current project)

let delete_project project =
  let name = Project.unique_name project in
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
      ~title:("Save project %S" ^ Project.unique_name project)
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
		host_window#error ~parent:(dialog:>GWindow.window_skel)
		  "Cannot load: %s" s)
	 end
     | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy ()

let rename_project (main_ui: Design.main_window_extension_points) project =
  let old = Project.unique_name project in
  let s = 
    GToolbox.input_string
      ~title:"Renaming project"
      (Format.sprintf "New name for project %S:" old)
  in
  match s with
  | None -> ()
  | Some s ->
      try 
	ignore (Project.from_unique_name s);
	main_ui#error "Project of name %S already exists" s
      with Not_found ->
	Project.set_name project s
  
let reset (menu: GMenu.menu) =
  (* Do not reset all if there is no changes. *)
  let pl = projects_list () in
  let same_projects =
    (* use that project_radios and pl are sorted in the same way *)
    try 
      let rest =
	PrjRadiosSet.fold
	  (fun (p1, _) acc ->
	     match acc with
	     | [] -> raise Exit
	     | p2 :: acc -> 
		 if Project.compare p1 p2 = 0 then acc else raise Exit)
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
      (fun (p, r) -> r#set_active (Project.is_current p))
      !project_radios;
    false
  end else begin
    PrjRadiosSet.iter
      (fun (_, r) -> menu#remove (r :> GMenu.menu_item))
      !project_radios;
    project_radios := PrjRadiosSet.empty;
    true
  end

let rec duplicate_project window menu project =
  let new_p = Project.create_by_copy ~src:project (Project.name project) in
  (* update the GUI *)
  let group = 
    let _, i =
      try PrjRadiosSet.choose !project_radios with Not_found -> assert false
    in
    i#group
  in
  ignore (mk_project_entry window menu ~group new_p)

(* let is_reset = reset menu in
  assert is_reset;
  make_project_entries window menu*)

and mk_project_entry window menu ?group p =
  let p_item = GMenu.radio_menu_item
    ?group
    ~active:(Project.is_current p)
    ~packing:menu#append 
    ()
  in
  ignore
    (p_item#connect#toggled
       ~callback:(fun () -> 
		    if p_item#active && not (Project.is_current p) then begin
		      Project.set_current p
		    end));
  project_radios := PrjRadiosSet.add (p, p_item) !project_radios;
  let box = GPack.hbox ~packing:p_item#add () in  
  ignore (GMisc.label ~text:(Project.unique_name p) ~packing:box#pack ());
  let buttons_box = GPack.hbox ~packing:(box#pack ~from:`END) () in  
  let tooltips = GData.tooltips () in
  let add_action stock text callback =
    let item = GButton.button ~packing:buttons_box#pack () in
    tooltips#set_tip item#coerce ~text;
    item#set_relief `NONE;
    let image = GMisc.image ~stock () in
    item#set_image image#coerce;
    image#set_icon_size `MENU;
    ignore (item#connect#clicked ~callback)
  in
  add_action `COPY "Duplicate project" 
    (fun () -> duplicate_project window menu p);
  add_action `DELETE "Delete project" (fun () -> delete_project p);
  add_action `SAVE "Save project" (fun () -> save_project window p);
  add_action `SAVE_AS "Save project as" (fun () -> save_project_as window p);
  add_action `SPELL_CHECK "Rename project" (fun () -> rename_project window p);
  p_item

let make_project_entries window menu =
  match projects_list () with
  | [] -> assert false
  | pa :: tl ->
      let mk = mk_project_entry window menu in
      let pa_item = mk pa in
      let group = pa_item#group in
      List.iter (fun pa -> ignore (mk ~group pa)) tl

(** Register this dialog in main window menu bar *)
let () =
  Design.register_extension
    (fun window ->
       let item, menu = window#menu_manager#add_menu "_Project" in
       let constant_items =
	 window#menu_manager#add_entries
	   menu
	   [ 
	     Menu_manager.ToolMenubar(`NEW, "New project"),
	     (fun () -> new_project window);
	     Menu_manager.Menubar(Some `REVERT_TO_SAVED, "Load project"),
	     (fun () -> load_project window);
	     Menu_manager.ToolMenubar(`COPY, "Duplicate current project"),
	     (fun () -> duplicate_project window menu (Project.current ()));
	     Menu_manager.ToolMenubar(`DELETE, "Delete current project"),
	     (fun () -> delete_project (Project.current ()));
	   ]
       in
       let new_item = constant_items.(0) in
       new_item#add_accelerator `CONTROL 'n';
       constant_items.(3)#add_accelerator `CONTROL 'd';
       ignore (GMenu.separator_item ~packing:menu#append ());
       let callback () = 
	 let is_reset = reset menu in
	 if is_reset then make_project_entries window menu
       in
       ignore (item#connect#activate ~callback))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
