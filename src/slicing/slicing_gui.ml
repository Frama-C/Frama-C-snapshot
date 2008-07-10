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

(*i $Id: slicing_gui.ml,v 1.35 2008/07/09 11:26:38 uid530 Exp $ i*)
open Pretty_source
open Cil_types
open Db
open Gtk_helper
open Cilutil 

let state_highlighter_defs = ref StmtSet.empty

let slicing_selector (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) ~button localizable = 
  if Db.Value.is_computed () then
    let reset_highlight = main_ui#rehighlight (* main_ui#reset *) in
    let add_annot txt = main_ui#annot_window#buffer#insert (txt ^ "\n") in
    let slicing_project = Slicing.Project.get_project () in
    if button = 1 then
      begin let slicing_view project =
        add_annot ("Highlighting for " ^ (!Slicing.Project.get_name project))
      in 
      Extlib.may slicing_view slicing_project;
      if Cmdline.Slicing.Mode.Verbose.get () > 0 then begin
        let slicing_mark project =
          let slicing_mark kf get_mark =
            (* use -slicing-debug -verbose to get slicing mark information *)
            let add_mark_info txt = add_annot ("Tag: " ^ txt) in
            let slices = !Slicing.Slice.get_all project kf in
            match slices with
            | [] -> (* No slice for this kf *)
                add_mark_info (if !Slicing.Project.is_called project kf
                               then (* but the source function is called *)
                                 "<src><--->"
                               else
                                 "<   ><   >")
            | slices ->
                if !Slicing.Project.is_called project kf
                then (* The source function is also called *)
                  add_mark_info "<src><--->";
                let mark_slice slice =
                  add_mark_info (Cil.fprintf_to_string "%a" !Slicing.Mark.pretty (get_mark slice))
                in List.iter mark_slice slices
          in match localizable with
          | PStmt (kf,ki) ->  slicing_mark kf (fun slice -> !Slicing.Slice.get_mark_from_stmt slice ki)
          | PVDecl (Some kf,vi) -> slicing_mark kf (fun slice -> !Slicing.Slice.get_mark_from_local_var slice vi)
          | PVDecl (None,_) | PLval _ | PTermLval _ -> ()
        in Extlib.may slicing_mark slicing_project
      end 
      end 
    else if button = 3 then begin
      let add_item  (factory:GMenu.menu GMenu.factory) name arg_opt callback =
        match arg_opt with
        | None ->
            let item = factory#add_item name ~callback: (fun () -> ())
            in item#misc#set_sensitive false 
        | Some arg -> ignore (factory#add_item name ~callback: (fun () -> callback arg)) in
      (* definitions for slicing plug-in *)
      let submenu = popup_factory#add_submenu "Slicing" in
      let slicing_factory = new GMenu.factory submenu in
      let add_slicing_item name = add_item slicing_factory name in
      let mk_selection fselect = fselect (!Db.Slicing.Select.empty_selects ()) in             
      let mk_selection_cad fselect =
        mk_selection fselect (!Db.Slicing.Mark.make ~ctrl:true ~addr:true ~data:true) in          
      let mk_selection_all fselect =
        mk_selection fselect ~spare:false in
      let mk_slice selection =
        let n = string_of_int (1 + List.length (Slicing.Project.get_all ())) in
        let project = Slicing.Project.mk_project ("Slicing "^ n) in
        !Db.Slicing.Request.add_persistent_selection project selection ;
	!Db.Slicing.Request.apply_all_internal project;          
        if Cmdline.Slicing.Mode.Callers.get () then
          !Db.Slicing.Slice.remove_uncalled project;
	let new_project = !Db.Slicing.Project.extract ((!Db.Slicing.Project.get_name project)^ " export") project in
        add_annot ("Slice exported to project: " ^ (Project.name new_project));
        Slicing.Project.set_project (Some (project)) ;
        reset_highlight ()
      in
      let add_slice_menu kf_opt kf_ki_opt =
        add_slicing_item "Slice calls to"
          kf_opt
          (fun kf -> 
             let selection = (mk_selection_all !Db.Slicing.Select.select_func_calls_to) kf in
             mk_slice selection);
        add_slicing_item "Slice calls into"
          kf_opt
          (fun kf -> 
             let selection = (mk_selection_all !Db.Slicing.Select.select_func_calls_into) kf in
             mk_slice selection);
        add_slicing_item "Slice stmt"
          kf_ki_opt
          (fun (kf, ki) ->
             let selection = (mk_selection_all !Db.Slicing.Select.select_stmt) ki kf in
             mk_slice selection);
        add_slicing_item "Slice expr"
          kf_ki_opt
          (fun (kf, ki) ->
             let do_with_txt txt =
               try
                 let lval_str = Cilutil.StringSet.add txt Cilutil.StringSet.empty in
                 let selection = (mk_selection_cad !Db.Slicing.Select.select_stmt_lval) lval_str ~before:true ki kf in
                 mk_slice selection
               with e -> main_ui#info "invalid expression: %s" (Printexc.to_string e)
             in
             let txt =
               GToolbox.input_string
                 ~title:"Input a pure C expression to slice before current statement"
                 ""
             in Extlib.may do_with_txt txt)
      in
      let some_kf_from_vi vi = 
	try Some (Globals.Functions.get vi)
        with Not_found -> None in
      let some_kf_from_lv  lv = 
        match lv with
        | Var vi,_ -> some_kf_from_vi vi
        | _ -> None in
      (* definitions for scope plug-in *)
      let add_scope_item name = add_item popup_factory name in
      let add_scope_menu kf_stmt_ki_lv_opt =
        add_scope_item "_Show defs"
          kf_stmt_ki_lv_opt
          (fun (kf, stmt, ki, lv) ->
             try 
               let pdg = !Pdg.get kf in
               let zone= 
                 !Value.lval_to_zone
                   ki 
                   ~with_alarms:CilE.warn_none_mode lv
               in 
               let nodes, _undef = 
                 !Pdg.find_location_nodes_at_stmt 
                   pdg 
                   stmt
                   ~before:true 
                   zone
               in
               (* TODO : what about undef ? *)
               let nodes = List.map (fun (n,_z) -> n) nodes in
               state_highlighter_defs := (* select corresponding stmts *)
                 List.fold_left 
                   (fun acc node -> 
                      match (PdgIndex.Key.stmt (!Pdg.node_key node)) with 
                      | None -> acc
                      | Some s -> StmtSet.add s acc)
                   StmtSet.empty
                   nodes;
               main_ui#rehighlight ()
             with Pdg.Bottom | Pdg.Top | Pdg.NotFound -> 
               state_highlighter_defs := StmtSet.empty;
               main_ui#rehighlight ())
      in begin  (* add menu for slicing and scope plug-in *)
        match localizable with
        | PVDecl (_,vi) ->
            add_scope_menu None ;
            add_slice_menu (some_kf_from_vi vi) None
        | PStmt (kf,ki) ->
            add_scope_menu None ;
            add_slice_menu None (Some (kf, ki))
        | PLval (Some kf,(Kstmt stmt as ki),lv) ->
            add_scope_menu (if not (Cil.isFunctionType (Cil.typeOfLval lv)) then 
                              Some (kf, stmt, ki, lv)
                            else None) ;
            add_slice_menu (some_kf_from_lv lv) (Some (kf, stmt))
        | _  ->
            add_scope_menu None ;
            add_slice_menu None None
      end;
      let projects = Slicing.Project.get_all() in
      ignore (slicing_factory#add_separator ());
      add_slicing_item "_Unhighlight" (Some ())
        (fun () ->  Slicing.Project.set_project None ;
           reset_highlight ()) ;
      List.iter
        (fun proj ->
           let add_highlight_menu sensitive =
             add_slicing_item ("Highlight " ^
		                 (Pretty_utils.escape_underscores (!Slicing.Project.get_name proj)))
               sensitive
               (fun () ->  Slicing.Project.set_project (Some proj) ;
                  reset_highlight ())
           in match slicing_project with
           | Some project -> add_highlight_menu (if (proj == project) then None else Some ())
           | None -> add_highlight_menu None)
        projects;
    end
        
let slicing_highlighter  (buffer:GSourceView.source_buffer) localizable ~start ~stop =
  (* First highlight the 'Show defs' *)
  begin match localizable with
  | PStmt (_,stmt) when StmtSet.mem stmt !state_highlighter_defs ->
      let tag = 
        make_tag 
          buffer 
          ~name:"slicing_defs"
          [`BACKGROUND "yellow"]
      in
      apply_tag buffer tag start stop
  | _ -> () end;
  
  let highlight project =
    let ki = match localizable with 
      | PStmt (_,stmt) -> Kstmt stmt
      | PLval (_,ki,_) | PTermLval(_,ki,_) -> ki
      | PVDecl _ -> Kglobal
    in
    if Value.is_accessible ki then 
      let unused_code_area = 
	make_tag buffer ~name:"slicing_unused" [`STRIKETHROUGH true ] in
      let spare_code_area = 
	make_tag buffer ~name:"slicing_spare" [`UNDERLINE `LOW] in
      let necessary_code_area = 
	make_tag buffer ~name:"slicing_necessary" [`BACKGROUND "green"] in
      let apply_on_one_project_and_merge_slices kf pb pe mark_of_slice =
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
                let mark = mark_of_slice project slice in
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
  in
  if Db.Value.is_computed () then
    let slicing_project = Slicing.Project.get_project () in
    Extlib.may highlight slicing_project         
            
let main (main_ui:Design.main_window_extension_points) =
  main_ui#register_source_selector slicing_selector;
  main_ui#register_source_highlighter slicing_highlighter


let () = 
  Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)


