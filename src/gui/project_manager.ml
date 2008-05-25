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

open Cilutil

let list_project () =
  let result = ref [] in
  Project.iter_on_projects 
    (fun p -> result := (p, Project.is_current p) :: !result);
  List.sort
    (fun (p1,_) (p2,_) -> 
       let n = 
	 String.compare (Project.name p1) (Project.name p2) 
       in
       if n = 0 then Project.compare p1 p2 else n)
  !result

let reset_project_menu = ref (fun () -> ())

let make_project_entries window =
  let actions = GAction.action_group ~name:"project_manager" () in
  let ui = window#ui_manager in
  ui#insert_action_group actions 0;
  let merge_id = ref None in
  let demerge () =
    Extlib.may ui#remove_ui !merge_id;
    ui#remove_action_group actions;
    ui#ensure_update()
  in
  reset_project_menu := demerge;
  let switcher = list_project () in
  let idx = ref (-1) in
  let active_idx = ref (-1) in
  let configure_for_action (project,active) = 
    incr idx;
    if active then active_idx := !idx;
    let name = Project.unique_name project in
    project,name,!idx,Pretty_utils.escape_underscores name
  in
  let switcher = List.map configure_for_action switcher in
  let ra =
    List.map 
     (fun (_,name,idx,label) act -> 
	GAction.add_radio_action name idx ~label act)
      switcher
  in
  GAction.add_actions (actions:GAction.action_group)
    [GAction.group_radio_actions 
       ~init_value:!active_idx
       ~callback:(fun i -> 
		    let project,_,_,_ = List.nth switcher i in
                    Project.set_current project;
                    window#reset ())
       ra];
  let location =
    Format.sprintf
      "<ui><menubar name='MenuBar'>
          <menu action='ProjectMenu'>
          %s
          </menu>
          </menubar></ui>"
      (List.fold_right
         (fun (_,name,_,_) acc ->
            (Format.sprintf "<menuitem action='%s'/>" name) ^ acc)
         switcher
         "")
  in
  merge_id := Some (ui#add_ui_from_string location);
  ui#ensure_update ()

(** Register this dialog in main window menu bar *)
let () =
  Design.register_extension
    (fun window ->
       GAction.add_actions window#actions
         [ GAction.add_action
             "ProjectMenu"
             ~label:"_Change project"
             ~callback:
	     (fun _ -> !reset_project_menu (); make_project_entries window) ];
       (window#actions#get_action "ProjectMenu")#set_hide_if_empty false;
       let location =
         "<ui><menubar name='MenuBar'>
          <menu action='ProjectMenu'>
          </menu>
          </menubar></ui>"
       in
       ignore (window#ui_manager#add_ui_from_string location))

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
