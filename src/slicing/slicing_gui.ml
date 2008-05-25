(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA   (Commissariat à l'Énergie Atomique)                           *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: slicing_gui.ml,v 1.19 2008/04/07 14:51:04 uid568 Exp $ i*)
open Pretty_source
open Cil_types
open Db
open Gtk_helper

let slicing_selector (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) ~button localizable = 
  if Db.Value.is_computed () then
    let annot = main_ui#annot_window#buffer in
    if button = 1 then
      begin match localizable with
      | PStmt (kf,ki) -> 
          if Cmdline.Slicing.Mode.Verbose.get () > 0
          then begin
            (* use -slicing-debug -verbose to get slicing mark information *)
            let projects = Slicing.Project.get_all () in                
 	    match projects with
	    | [project] ->
                let slices = !Slicing.Slice.get_all project kf in
                begin
                  match slices with
                  | [] -> (* No slice for this kf *)
                      if !Slicing.Project.is_called project kf
                      then (* but the source function is called *)
                        annot#insert ("Slicing mark: <src><--->\n")
                      else
                        annot#insert ("Slicing mark: <   ><   >\n")
                  | slices ->
                      if !Slicing.Project.is_called project kf
                      then (* The source function is also called *)
                        annot#insert ("Slicing mark: <src><--->\n");
                      let mark_slice slice =
                        let mark =
                          !Slicing.Slice.get_mark_from_stmt slice ki
                        in
                        annot#insert (Format.sprintf "Slicing mark: %s\n"
                                        (Cil.fprintf_to_string "%a" !Slicing.Mark.pretty mark))
                      in List.iter mark_slice slices
                end
	    | [] -> ()
	    | _ -> ()
          end
      | _ -> ()
      end 
    else if button = 3 then
      begin match localizable with
      | PLval (Some kf,(Kstmt stmt as ki),lv) ->
          if not (Cil.isFunctionType (Cil.typeOfLval lv)) then 
            let callback () = 
              let tag_defs = 
                Gtk_helper.make_tag 
                  main_ui#source_viewer#buffer 
                  ~name:"slicing_defs"
                  [`BACKGROUND "yellow"]
              in
              Gtk_helper.cleanup_tag main_ui#source_viewer#buffer tag_defs;
              try 
                let pdg = !Pdg.get kf in
                let zone= !Value.lval_to_zone ki ~with_alarms:CilE.warn_none_mode lv in 
                let nodes, _undef = 
                  !Pdg.find_location_nodes_at_stmt 
                    pdg 
                    stmt
                    ~before:true 
                    zone
                in
                (* TODO : what about undef ? *)
                List.iter (* select corresponding stmts *)
                  (fun node -> 
                     Extlib.may 
                       (fun st -> 
                          main_ui#highlight ~scroll:false tag_defs
                            (PStmt(kf,st)))
                       (PdgIndex.Key.stmt (!Pdg.node_key node)))
                  nodes
              with Pdg.Bottom | Pdg.Top | Pdg.NotFound -> ()
            in
            ignore (popup_factory#add_item "_Show defs" ~callback)

      | _  -> ()
      end

let slicing_highlighter  (buffer:GText.buffer) localizable ~start ~stop =
  if Db.Value.is_computed () then
    let ki = match localizable with 
    | PStmt (_,stmt) -> Kstmt stmt
    | PLval (_,ki,_) | PTermLval(_,ki,_) -> ki
    | PVDecl _ -> Kglobal
    in
    if Value.is_accessible ki then 
      let unused_code_area = 
	make_tag buffer ~name:"slicing_unused" [`STRIKETHROUGH true ] 
      in
      let spare_code_area = 
	make_tag buffer ~name:"slicing_spare" [`UNDERLINE `LOW] 
      in
      let necessary_code_area = 
	make_tag buffer ~name:"slicing_necessary" [`BACKGROUND "green"] 
      in
      let projects = Slicing.Project.get_all () in
      let apply_on_one_project_and_merge_slices kf pb pe mark_of_slice =
        (* JS: do not display slicing results if security is on *)
        if not (Cmdline.Security.is_on ()) then
	  match projects with
	  | [project] ->
              let slices = !Slicing.Slice.get_all project kf in
              begin
                match slices with
                | [] ->
                    (* No slice for this kf *)
                    if !Slicing.Project.is_called project kf
                    then
                      apply_tag buffer necessary_code_area pb pe
                    else
                      apply_tag buffer unused_code_area pb pe
                | slices ->
                    if Cmdline.Debug.get () > 0 then begin
                      let l = List.length slices in
                      if l >=2 then
                        Format.printf "Got %d slices@."
			  (List.length slices)
                    end;
                    let mark_slice slice =
                      let mark =
                        mark_of_slice project slice
                      in
                      if Cmdline.Debug.get () > 0 then
                        Format.printf "Got mark: %a@." !Slicing.Mark.pretty mark;
                      if !Slicing.Mark.is_bottom mark then
                        apply_tag buffer unused_code_area pb pe;
                      if !Slicing.Mark.is_spare mark then
                        apply_tag buffer spare_code_area pb pe;
                      if (!Slicing.Mark.is_ctrl mark
			  || !Slicing.Mark.is_data mark
			  || !Slicing.Mark.is_addr mark)
                      then
                        apply_tag buffer necessary_code_area pb pe
                    in List.iter mark_slice slices
              end
	  | [] -> ()
	  | l -> CilE.warn_once "%d slicing projects ? Disabling slicing display."
              (List.length l)
      in
      let tag_stmt kf stmt pb pe =
        if Db.Value.is_accessible (Kstmt stmt) then
	  apply_on_one_project_and_merge_slices
	    kf
	    pb
	    pe
	    (fun _ slice -> !Slicing.Slice.get_mark_from_stmt slice stmt)
      in
      let tag_vdecl kf vi pb pe =
        if not vi.vglob then
	  apply_on_one_project_and_merge_slices
	    kf
	    pb
	    pe
	    (fun _ slice -> !Slicing.Slice.get_mark_from_local_var slice vi)
      in
      match localizable with 
      | PStmt (kf,stmt) -> tag_stmt kf stmt start stop
      | PVDecl (Some kf,vi) -> tag_vdecl kf vi start stop 
      | PVDecl (None,_) | PLval _ | PTermLval _ -> ()

            
let main (main_ui:Design.main_window_extension_points) =
  main_ui#register_source_highlighter slicing_highlighter;
  main_ui#register_source_selector slicing_selector


let () = 
  Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)


