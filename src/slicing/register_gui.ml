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

(*i $Id: register_gui.ml,v 1.35 2008/12/16 10:02:59 uid526 Exp $ i*)
open Cil_types
open Cilutil 

module Enable = 
  Computation.Ref
    (struct include Datatype.Bool
            let default () = false 
     end)
    (struct 
       let name = "Slicing_gui.State"
       let dependencies = []
     end)
    
   
(* for slicing callback *)
let mk_selection fselect = fselect (!Db.Slicing.Select.empty_selects ())
  
(* for slicing callback *)
let mk_selection_cad fselect =
  mk_selection fselect (!Db.Slicing.Mark.make ~ctrl:true ~addr:true ~data:true)
    
(* for slicing callback *)
let mk_selection_all fselect =
  mk_selection fselect ~spare:false
    
(* for slicing callback *)
let mk_slice selection =
  let n = string_of_int (1 + List.length (!Db.Slicing.Project.get_all ())) in
  let project = !Db.Slicing.Project.mk_project ("Slicing "^ n) in
    !Db.Slicing.Request.add_persistent_selection project selection ;
    !Db.Slicing.Request.apply_all_internal project;          
    if Cmdline.Slicing.Mode.Callers.get () then
      !Db.Slicing.Slice.remove_uncalled project;
    let new_project = !Db.Slicing.Project.extract ((!Db.Slicing.Project.get_name project)^ " export") project in
      !Db.Slicing.Project.set_project (Some (project)) ;
      new_project

(* To add a sensitive/unsensitive menu item to a [factory] *)
let add_item (factory:GMenu.menu GMenu.factory) ~callback name arg_opt =
    match arg_opt with
      | None -> (* add the menu item, but it isn't sensitive *)
          let item = factory#add_item name ~callback:(fun () -> ())
          in item#misc#set_sensitive false
      | Some arg -> (* add the menu item with its callback *)
          ignore (factory#add_item name ~callback:(fun () -> callback arg))

(* To inform the user about a status. *)       
let gui_annot_info (main_ui:Design.main_window_extension_points) ~level txt =
  if (Cmdline.Slicing.Mode.Verbose.get () >= level) then
    begin
      main_ui#annot_window#buffer#insert ((txt ()) ^ ".\n")
    end
      
(* To inform the user about an action. *)       
let gui_annot_action (main_ui:Design.main_window_extension_points) txt =
  if (Cmdline.Slicing.Mode.Verbose.get () >= 0) then
    begin
      let tag_style_italic = Gtk_helper.make_tag main_ui#annot_window#buffer ~name:"slicing:style italic" [`STYLE `ITALIC] in
        main_ui#annot_window#buffer#insert ~tags:[tag_style_italic] ((txt ()) ^ "\n")
    end
      
(* To inform the user about an error. *)       
let gui_annot_error (main_ui:Design.main_window_extension_points) txt =
  let tag_style_italic = Gtk_helper.make_tag main_ui#annot_window#buffer ~name:"slicing:style italic" [`STYLE `OBLIQUE  ; ] in
    main_ui#annot_window#buffer#insert ~tags:[tag_style_italic] (txt ^ ".\n")
      
let gui_mk_slice main_ui selection ~info =
  let already_locked =
    gui_annot_action main_ui info;
    main_ui#lock () in (* lock the gui while ... *)
  let new_project = mk_slice selection in (* ... slicing computation *)
    main_ui#unlock already_locked ;
    gui_annot_action main_ui (fun () -> "Slice exported to project: " ^ (Project.name new_project));
    main_ui#rehighlight ()
        
let gui_compute_values (main_ui:Design.main_window_extension_points) =
  if not (Db.Value.is_computed ()) then
    begin
      gui_annot_action main_ui (fun () -> "Activating slicing plugin by running value analysis first");
      let already_locked = main_ui#lock () in
        (try !Db.Value.compute ();
         with Globals.No_such_entry_point msg -> gui_annot_error main_ui msg);
        main_ui#unlock already_locked
    end

(* To do an action and inform the user. *)
let gui_apply_action (main_ui:Design.main_window_extension_points) f x ~info =
  f x ;
  gui_annot_action main_ui info

let get_setting_option_text txt = "Setting option " ^ txt ^ " for the current project"
  
let gui_toggle_slice_undef (main_ui:Design.main_window_extension_points) =
  let slice_undef = not (Cmdline.Slicing.Mode.SliceUndef.get ()) in
    gui_apply_action main_ui Cmdline.Slicing.Mode.SliceUndef.set slice_undef
      ~info:(fun () ->
               
               if slice_undef then (get_setting_option_text "-slice-undef-functions" )^
                 ". Allow the use of the slicing level for calls to undefined functions"
               else (get_setting_option_text "-no-slice-undef-functions") ^
                 ". Forbid the slicing of prototypes of undefined functions")
    
let gui_set_project (main_ui:Design.main_window_extension_points) proj_opt =
  gui_apply_action main_ui !Db.Slicing.Project.set_project proj_opt
    ~info:(fun () ->
             Extlib.may_map ~dft:"Clear slicing highlighting"
               (fun project -> ("Highlighting for " ^ (!Db.Slicing.Project.get_name project)))
               proj_opt) ;
  main_ui#rehighlight ()
      
let slicing_selector (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) ~button localizable = 
  if not (Db.Value.is_computed ()) then
    ignore (popup_factory#add_item "Activate _Slicing"
	      ~callback:(fun () -> gui_compute_values main_ui ))
  else if not (Enable.get ()) then
    ignore (popup_factory#add_item "Enable _Slicing menu"
	      ~callback:(fun () -> Enable.set true))
  else
    let slicing_project = !Db.Slicing.Project.get_project () in
      if button = 1 then
        begin let level = 0 in
        let slicing_view project =
          gui_annot_info main_ui ~level (fun () -> "Highlighting for " ^ (!Db.Slicing.Project.get_name project))
        in 
          Extlib.may slicing_view slicing_project;
          if Cmdline.Slicing.Mode.Verbose.get () > level then begin
            let slicing_mark project =
              let slicing_mark kf get_mark =
                (* use -slicing-debug -verbose to get slicing mark information *)
                let add_mark_info txt = gui_annot_info ~level main_ui (fun () -> "Tag: " ^ (txt ())) in
                let slices = !Db.Slicing.Slice.get_all project kf in
                  match slices with
                    | [] -> (* No slice for this kf *)
                        add_mark_info (fun () ->
                                         if !Db.Slicing.Project.is_called project kf
                                         then (* but the source function is called *)
                                           (Cil.fprintf_to_string "<src>%a"
                                              !Db.Slicing.Mark.pretty (!Db.Slicing.Mark.get_from_src_func project kf))
                                         else
                                           "<   ><   >")
                    | slices ->
                        if !Db.Slicing.Project.is_called project kf
                        then begin (* The source function is also called *)
                          assert (not (kf == fst (Globals.entry_point ()))) ;
                          add_mark_info (fun () ->
                                           Cil.fprintf_to_string "<src>%a"
                                             !Db.Slicing.Mark.pretty (!Db.Slicing.Mark.get_from_src_func project kf))
                        end ;
                        let mark_slice slice =
                          add_mark_info (fun () -> Cil.fprintf_to_string "%a" !Db.Slicing.Mark.pretty (get_mark slice))
                        in List.iter mark_slice slices
              in match localizable with
                | Pretty_source.PTermLval(Some kf,(Kstmt ki),_) (* as for the statement *)
                | Pretty_source.PLval (Some kf,(Kstmt ki),_) (* as for the statement *)
                | Pretty_source.PStmt (kf,ki) -> slicing_mark kf (fun slice -> !Db.Slicing.Slice.get_mark_from_stmt slice ki)
                | Pretty_source.PVDecl (Some kf,vi) -> slicing_mark kf (fun slice -> !Db.Slicing.Slice.get_mark_from_local_var slice vi)
                | _ -> ()
            in Extlib.may slicing_mark slicing_project
          end 
        end 
      else if button = 3 then begin
        let submenu = popup_factory#add_submenu "Slicing" in
        let slicing_factory = new GMenu.factory submenu in
          (* definitions for slicing plug-in *)        
        let add_slicing_item name = add_item slicing_factory name in
        let mk_slice = gui_mk_slice main_ui in
        let add_slice_menu kf_opt kf_ki_opt =
          
          add_slicing_item "Slice calls to"
            kf_opt
            ~callback:(fun kf ->
                         mk_slice
                           ~info:(fun () -> Cil.fprintf_to_string "Request for slicing effects of function %a"
                                    Kernel_function.pretty_name kf)
                           ((mk_selection_all !Db.Slicing.Select.select_func_calls_to) kf));
          add_slicing_item "Slice calls into"
            kf_opt
            ~callback:(fun kf -> 
                         mk_slice
                           ~info:(fun () -> Cil.fprintf_to_string "Request for slicing entrance into function %a"
                                    Kernel_function.pretty_name kf)
                           ((mk_selection_all !Db.Slicing.Select.select_func_calls_into) kf));
          add_slicing_item "Slice result"
            (Extlib.opt_filter (fun kf ->
                                  let is_not_void_kf x =
                                    match x.Cil_types.vtype with
                                      | Cil_types.TFun (Cil_types.TVoid (_),_,_,_) -> false
                                      | _ -> true
                                  in is_not_void_kf (Kernel_function.get_vi kf))
               kf_opt)
            ~callback:(fun kf ->
                         mk_slice
                           ~info:(fun () -> Cil.fprintf_to_string "Request for returned value of function %a"
                                    Kernel_function.pretty_name kf)
                           ((mk_selection_all !Db.Slicing.Select.select_func_return) kf));
          add_slicing_item "Slice stmt"
            kf_ki_opt
            ~callback:(fun (kf, ki) ->
                         mk_slice
                           ~info:(fun () -> Cil.fprintf_to_string "Request for slicing effects of statement %d"
                                    ki.sid)
                           ((mk_selection_all !Db.Slicing.Select.select_stmt) ki kf));
          add_slicing_item "Slice lval"
            kf_ki_opt
            ~callback:(fun (kf, ki) ->
                         let do_with_txt txt =
                           try
                             let lval_str = Cilutil.StringSet.add txt Cilutil.StringSet.empty in
                               mk_slice
                                 ~info:(fun () -> Cil.fprintf_to_string "Request for slicing Lvalue %s before statement %d"
                                          txt
                                          ki.sid)
                                 ((mk_selection_cad !Db.Slicing.Select.select_stmt_lval)
                                    lval_str ~before:true ki ~scope:ki ~eval:ki kf)
                           with e -> main_ui#error "Invalid expression: %s" (Printexc.to_string e)
                         in
                         let txt =
                           GToolbox.input_string
                             ~title:"Input a pure Lvalue expression to slice before current statement"
                             ""
                         in Extlib.may do_with_txt txt);
          add_slicing_item "Slice rd"
            kf_ki_opt
            ~callback:(fun (kf, ki) ->
                         let do_with_txt txt =
                           try
                             let lval_str = Cilutil.StringSet.add txt Cilutil.StringSet.empty in
                               mk_slice
                                 ~info:(fun () -> Cil.fprintf_to_string "Request for slicing read accesses to Lvalue %s"
                                          txt)
                                 ((mk_selection_cad !Db.Slicing.Select.select_func_lval_rw)
                                    ~rd:lval_str ~wr:Cilutil.StringSet.empty ~scope:ki ~eval:ki kf)
                           with e -> main_ui#error "Invalid expression: %s" (Printexc.to_string e)
                         in
                         let txt =
                           GToolbox.input_string
                             ~title:"Input a pure Lvalue expression to slice read accesses"
                             ""
                         in Extlib.may do_with_txt txt);
          add_slicing_item "Slice wr"
            kf_ki_opt
            ~callback:(fun (kf, ki) ->
                         let do_with_txt txt =
                           try
                             let lval_str = Cilutil.StringSet.add txt Cilutil.StringSet.empty in
                               mk_slice
                                 ~info:(fun () -> Cil.fprintf_to_string "Request for slicing writen accesses to Lvalue %s"
                                          txt)
                                 ((mk_selection_cad !Db.Slicing.Select.select_func_lval_rw)
                                    ~rd:Cilutil.StringSet.empty ~wr:lval_str ~scope:ki ~eval:ki kf)
                           with e -> main_ui#error "Invalid expression: %s" (Printexc.to_string e)
                         in
                         let txt =
                           GToolbox.input_string
                             ~title:"Input a pure Lvalue expression to slice read accesses"
                             ""
                         in Extlib.may do_with_txt txt);
          add_slicing_item "Slice ctrl"
            kf_ki_opt
            ~callback:(fun (kf, ki) ->
                         mk_slice
                           ~info:(fun () -> Cil.fprintf_to_string "Request for slicing accessibility to statement %d"
                                    ki.sid)
                           ((mk_selection_all !Db.Slicing.Select.select_stmt_ctrl) ki kf))
        in
        let some_kf_from_vi vi = 
	  try let kf = Globals.Functions.get vi in
          if Enable.get () && !Db.Value.is_called kf then Some kf else None
          with Not_found -> None in
        let some_kf_from_lv  lv = 
          match lv with
            | Var vi,_ -> some_kf_from_vi vi
            | _ -> None in
        let some_kf_ki kf ki = 
	  if Enable.get ()
            && !Db.Value.is_called kf
            && Db.Value.is_accessible (Cil_types.Kstmt ki)
          then Some (kf, ki) else None in
          begin  (* add menu for slicing and scope plug-in *)
            match localizable with
              | Pretty_source.PLval (Some kf,(Kstmt stmt),lv) ->
                  add_slice_menu (some_kf_from_lv lv) (some_kf_ki kf stmt)
              | Pretty_source.PTermLval(Some kf,(Kstmt ki),_) (* as for the statement *)
              | Pretty_source.PStmt (kf,ki) ->
                  add_slice_menu None (some_kf_ki kf ki)
              | Pretty_source.PVDecl (_,vi) ->
                  add_slice_menu (some_kf_from_vi vi) None
              | _  ->
                  add_slice_menu None None
          end;
          let projects = !Db.Slicing.Project.get_all() in
            ignore (slicing_factory#add_separator ());
            add_slicing_item "_Disable"
              (Some ())
              ~callback:(fun () -> Enable.set false);
            add_slicing_item "_Clear"
              (if slicing_project = None then None else Some ())
              ~callback:(fun () -> gui_set_project main_ui None) ;
            List.iter
              (fun proj ->
                 let add_highlight_menu sensitive =
                   add_slicing_item
                     ("Highlight " ^ (Pretty_utils.escape_underscores (!Db.Slicing.Project.get_name proj)))
                     sensitive
                     ~callback:(fun () -> gui_set_project main_ui (Some proj))
                 in match slicing_project with
                   | Some project -> add_highlight_menu (if (proj == project) then None else Some ())
                   | None -> add_highlight_menu (Some()))
              projects;
      end
        
let slicing_highlighter 
    (buffer:GSourceView.source_buffer) localizable ~start ~stop =
  if Enable.get () then begin
    (* Definition for highlight 'Slicing' *)
    let highlight project =
      let ki = match localizable with 
        | Pretty_source.PStmt (_,stmt) -> Kstmt stmt
        | Pretty_source.PLval (_,ki,_) | Pretty_source.PTermLval(_,ki,_) -> ki
        | Pretty_source.PVDecl _ -> Kglobal
      in
      if Db.Value.is_accessible ki then 
        let unused_code_area = 
	  Gtk_helper.make_tag buffer ~name:"slicing_unused" [`STRIKETHROUGH true ] in
        let spare_code_area = 
	  Gtk_helper.make_tag buffer ~name:"slicing_spare" [`UNDERLINE `LOW] in
        let necessary_code_area = 
	  Gtk_helper.make_tag buffer ~name:"slicing_necessary" [`BACKGROUND "green"] in
        let apply_on_one_project_and_merge_slices kf pb pe mark_of_slice =
          let apply_mark mark =
            if Cmdline.Debug.get () > 0 then
              Format.printf "Got mark: %a@." !Db.Slicing.Mark.pretty mark;
            if !Db.Slicing.Mark.is_bottom mark then
              Gtk_helper.apply_tag buffer unused_code_area pb pe;
            if !Db.Slicing.Mark.is_spare mark then
              Gtk_helper.apply_tag buffer spare_code_area pb pe;
            if (!Db.Slicing.Mark.is_ctrl mark
	        || !Db.Slicing.Mark.is_data mark
	        || !Db.Slicing.Mark.is_addr mark)
            then
              Gtk_helper.apply_tag buffer necessary_code_area pb pe
          in
          let slices = !Db.Slicing.Slice.get_all project kf in
          begin
            match slices with
            | [] ->
                (* No slice for this kf *)
                if !Db.Slicing.Project.is_called project kf
                then begin
                  if Cmdline.Debug.get () > 0 then 
                    Format.printf "Got source code@." ;
                  apply_mark (!Db.Slicing.Mark.get_from_src_func project kf)
                end
                else
                  Gtk_helper.apply_tag buffer unused_code_area pb pe
            | slices ->
                if !Db.Slicing.Project.is_called project kf
                then begin
                  assert (not (kf == fst (Globals.entry_point ()))) ;
                  if Cmdline.Debug.get () > 0 then 
                    Format.printf "Got source code@." ;
                  apply_mark (!Db.Slicing.Mark.get_from_src_func project kf)
                end ;
                if Cmdline.Debug.get () > 0 then begin
                  let l = List.length slices in
                  if l >=2 then
                    Format.printf "Got %d slices@."
		      (List.length slices)
                end;
                let mark_slice slice =
                  let mark = mark_of_slice project slice in
                  apply_mark mark
                in List.iter mark_slice slices
          end
        in
        let tag_stmt kf stmt pb pe =
          assert (Db.Value.is_accessible (Kstmt stmt)) ;
          apply_on_one_project_and_merge_slices
	    kf
	    pb
	    pe
	    (fun _ slice -> !Db.Slicing.Slice.get_mark_from_stmt slice stmt)
        in
        let tag_vdecl kf vi pb pe =
          if not vi.vglob then
 	    apply_on_one_project_and_merge_slices
	      kf
	      pb
	      pe
	      (fun _ slice -> !Db.Slicing.Slice.get_mark_from_local_var slice vi)
        in
        match localizable with 
        | Pretty_source.PStmt (kf,stmt) -> tag_stmt kf stmt start stop
        | Pretty_source.PVDecl (Some kf,vi) -> tag_vdecl kf vi start stop 
        | Pretty_source.PVDecl (None,_)
        | Pretty_source.PLval _
        | Pretty_source.PTermLval _ -> ()
    in 
    let slicing_project = !Db.Slicing.Project.get_project () in
    (* 2. Highlights the 'Slicing' *)
    Extlib.may highlight slicing_project         
  end

let none_text = "<i>None</i>"

let rebuild_model ((_, (model, _column)) as combo_box_text) = 
  model#clear ();
  GEdit.text_combo_add combo_box_text none_text;
  List.iter 
    (fun p -> GEdit.text_combo_add combo_box_text (!Db.Slicing.Project.get_name p)) 
    (List.rev (!Db.Slicing.Project.get_all()))

let refresh_combo_box ((combo_box, (model, _column)) as combo_box_text) 
    slicing_project sensitive =
  let nb_combo_elts = model#iter_n_children None in
  let projects = List.rev (!Db.Slicing.Project.get_all()) in
  if nb_combo_elts<>(1+(List.length projects))
  then rebuild_model combo_box_text;
  (* Reset the active project as active in the combo box *)
  let nth_proj = ref 0 in
  let i = ref 1 in
  List.iter (fun proj ->
                 Extlib.may (fun slicing_proj ->
                               if proj == slicing_proj then nth_proj := !i)
                   slicing_project;
                 incr i)
      projects;
  combo_box#set_active !nth_proj;
  combo_box#misc#set_sensitive sensitive

    
let gui_set_slicing_debug (main_ui:Design.main_window_extension_points) v =
  let old = Cmdline.Slicing.Mode.Verbose.get () in
    if v != old then (* Otherwise set is done at every refreshing *)
      gui_apply_action main_ui Cmdline.Slicing.Mode.Verbose.set v
        ~info:(fun () -> get_setting_option_text ("-slicing-debug \"-debug " ^ (string_of_int v) ^ "\""))
    
let gui_set_slicing_level (main_ui:Design.main_window_extension_points) v =
  let old = Cmdline.Slicing.Mode.Calls.get () in
    if v != old then (* Otherwise set is done at every refreshing *)
      gui_apply_action main_ui Cmdline.Slicing.Mode.Calls.set v
        ~info:(fun () -> get_setting_option_text ("-slicing-level " ^ (string_of_int v)))
        
let gui_set_slicing_undef_functions (main_ui:Design.main_window_extension_points) v =
  let old = Cmdline.Slicing.Mode.SliceUndef.get () in
    if v != old then (* Otherwise set is done at every refreshing *)
      gui_apply_action main_ui Cmdline.Slicing.Mode.SliceUndef.set v
        ~info:(fun () -> get_setting_option_text (if v then "-slicing-undef-functions" else "-no-slice-undef-functions"))
        
let slicing_panel main_ui =
  let w = GPack.vbox  () in
  let hbox1 = GPack.hbox
    ~packing:w#pack () in
  let activate_button = GButton.button ~label:"Activate"
    ~packing:hbox1#pack () in
  let ((combo_box, (_model, column)) as combo_box_text) =
    GEdit.combo_box_text ~strings:[ none_text ] ~wrap_width:3 ~use_markup:true
      ~packing:(hbox1#pack ~expand:true ~fill:true) () in

  let table = GPack.table ~columns:2 ~rows:2 ~homogeneous:true ~packing:w#pack () in

  let hbox2 = GPack.hbox ~packing:(table#attach ~left:1 ~top:0) () in

    (* [enabled_button] to give slicing menu available *)    
  let enable_refresh () = 
    gui_compute_values main_ui ;
    main_ui#rehighlight ()
  in 
  let enabled_button =
    GButton.check_button 
      ~label:"Enable"
      ~active:(Enable.get ())
      ~packing:(table#attach ~left:0 ~top:0) () in

  ignore (enabled_button#connect#toggled 
            ~callback:(fun () -> 
                         Enable.set enabled_button#active;
                         enable_refresh ()));

  let verbose_refresh = Gtk_helper.on_int ~lower:0 ~upper:3 
    hbox2
    "Verbosity"
        ~sensitive:Enable.get
        Cmdline.Slicing.Mode.Verbose.get
        (gui_set_slicing_debug main_ui)
  in

  let hbox3 = GPack.hbox ~packing:(table#attach ~left:1 ~top:1) () in
    (* [slice_undef_button] related to -slice-undef option *)    
  let slice_undef_button =
    GButton.check_button 
      ~label:"Libraries"
      ~active:(Enable.get ())
      ~packing:(table#attach ~left:0 ~top:1) () in

   let level_refresh = Gtk_helper.on_int ~lower:0 ~upper:3
    hbox3 
    "Level"
    ~sensitive:Enable.get
    Cmdline.Slicing.Mode.Calls.get 
    (gui_set_slicing_level main_ui)
   in

    combo_box#set_active 0 ;
    ignore (combo_box#connect#changed
              (fun () ->
                 match combo_box#active_iter with
                   | None -> ()
                   | Some row -> 
                       let slicing_project_name =
                         (* get the text entry related to the current slicing project *)
                         Extlib.may_map !Db.Slicing.Project.get_name ~dft:none_text (!Db.Slicing.Project.get_project ())
                       and selected_name = combo_box#model#get ~row ~column in
                         if (selected_name != slicing_project_name) then
                           let proj_opt =
                             try Some (List.find (fun proj -> selected_name = !Db.Slicing.Project.get_name proj) (!Db.Slicing.Project.get_all ()))
                             with Not_found -> None
                           in
                             gui_set_project main_ui proj_opt));
    ignore (activate_button#connect#pressed
              (fun () -> gui_compute_values main_ui ));

    ignore (slice_undef_button#connect#toggled
              (fun () ->
                 gui_set_slicing_undef_functions main_ui slice_undef_button#active));
    
    Project.register_after_set_current_hook
      ~user_only:true
      (fun () -> rebuild_model combo_box_text);

    let refresh () =
      let value_is_computed = Db.Value.is_computed () in
      let slicing_project = !Db.Slicing.Project.get_project () in
      let enabled = Enable.get () in
      activate_button#misc#set_sensitive (not value_is_computed) ;
      enabled_button#misc#set_sensitive value_is_computed ;
      slice_undef_button#misc#set_sensitive enabled ;
      
      verbose_refresh ();
      level_refresh ();

      if enabled_button#active <> enabled then 
	begin
          enabled_button#set_active enabled ;
          enable_refresh ();
	end;

      slice_undef_button#set_active (Cmdline.Slicing.Mode.SliceUndef.get());
  
      ignore (refresh_combo_box combo_box_text slicing_project (enabled && value_is_computed))
    in 
    refresh () ;
    "Slicing",w#coerce,Some refresh
        
let file_tree_decorate (file_tree:Filetree.t) = 
  file_tree#append_pixbuf_column 
    "Slicing"  
    (fun globs -> 
       Extlib.may_map
         (fun project ->
            if (List.exists
                  (fun glob -> match glob with 
                     | GFun ({svar = vi},_ ) ->
                         begin
                           try
                             let kf = Globals.Functions.get vi
                             in (!Db.Slicing.Project.is_called project kf)
                                || ( [] != (!Db.Slicing.Slice.get_all project kf))
                           with Not_found -> false
                         end
                     |  _ -> false) 
                  globs) then
              [`STOCK_ID "gtk-yes"]
            else
              [`STOCK_ID ""])
         ~dft:[`STOCK_ID ""]
         (!Db.Slicing.Project.get_project ()))

let main (main_ui:Design.main_window_extension_points) =
  main_ui#register_source_selector slicing_selector;
  main_ui#register_source_highlighter slicing_highlighter;
  main_ui#register_panel slicing_panel;
  file_tree_decorate main_ui#file_tree

let () = Design.register_extension main

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)


