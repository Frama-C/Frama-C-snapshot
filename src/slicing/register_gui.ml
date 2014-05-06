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

open Cil_types

(* Update the 'Slicing' column of the gui filetree. *)
let update_column  = ref (fun _ -> ())

(* Are results shown? *)
module Enabled = struct
  include State_builder.Ref
    (Datatype.Bool)
    (struct
       let name = "Slicing_gui.State"
       let dependencies = [!Db.Slicing.self]
       let default () = false
     end)
end

(* for slicing callback *)
let mk_selection fselect = fselect Db.Slicing.Select.empty_selects

(* for slicing callback *)
let mk_selection_cad fselect =
  mk_selection fselect (!Db.Slicing.Mark.make ~ctrl:true ~addr:true ~data:true)

(* for slicing callback *)
let mk_selection_all fselect =
  mk_selection fselect ~spare:false

(* for slicing callback *)
let mk_slice selection =
  let n = string_of_int (1 + List.length (!Db.Slicing.Project.get_all ())) in
  let project_name = SlicingParameters.ProjectName.get () ^ n in
  let project = !Db.Slicing.Project.mk_project project_name in
  !Db.Slicing.Request.add_persistent_selection project selection ;
  !Db.Slicing.Request.apply_all_internal project;
  if SlicingParameters.Mode.Callers.get () then
    !Db.Slicing.Slice.remove_uncalled project;
  let sliced_project_name =
    project_name ^ SlicingParameters.ExportedProjectPostfix.get ()
  in
  let new_project = !Db.Slicing.Project.extract sliced_project_name project in
  !Db.Slicing.Project.set_project (Some project);
  new_project

(* To add a sensitive/unsensitive menu item to a [factory] *)
let add_item (factory:GMenu.menu GMenu.factory) ~callback name arg_opt =
  match arg_opt with
  | None ->
    (* add the menu item, but it isn't sensitive *)
    let item = factory#add_item name ~callback:(fun () -> ()) in
    item#misc#set_sensitive false
  | Some arg ->
    (* add the menu item with its callback *)
    ignore (factory#add_item name ~callback:(fun () -> callback arg))

(* To inform the user about a status. *)
let gui_annot_info (main_ui:Design.main_window_extension_points) ~level txt =
  if (SlicingParameters.verbose_atleast level) then begin
    main_ui#annot_window#buffer#insert ((txt ()) ^ ".\n")
  end

(* To inform the user about an action. *)
let gui_annot_action (main_ui:Design.main_window_extension_points) txt =
  if SlicingParameters.verbose_atleast 2 then
    let tag_style_italic =
      Gtk_helper.make_tag
        main_ui#annot_window#buffer
        ~name:"slicing:style italic"
        [`STYLE `ITALIC]
    in
    main_ui#annot_window#buffer#insert
      ~tags:[tag_style_italic]
      ((txt ())^"\n")

(* To inform the user about an error. *)
let gui_mk_slice (main_ui:Design.main_window_extension_points) selection ~info =
  gui_annot_action main_ui info;
  let new_project = mk_slice selection in (* ... slicing computation *)
  gui_annot_action main_ui
    (fun () -> "Slice exported to project: " ^ (Project.get_name new_project));
  main_ui#rehighlight ()

let _msg_appl_compute_values =
  "Activating Slicing Plug-in by running Value Analysis first"
let msg_help_compute_values =
  "Activates Slicing Plug-in by running Value Analysis first."
let msg_help_enable_gui = "Enables/Disables the Slicing GUI."
let msg_help_libraries =
  "Allows/Disallows the use of the -slicing-level option for calls to \
undefined functions."

let gui_compute_values (main_ui:Design.main_window_extension_points) =
  if not (Db.Value.is_computed ()) then begin
    let tag_style_oblique =
      Gtk_helper.make_tag main_ui#annot_window#buffer
        ~name:"slicing:style oblique" [`STYLE `OBLIQUE  ; ]
    in
    main_ui#annot_window#buffer#insert
      "[Slicing] activation requires an execution of a ";
    main_ui#annot_window#buffer#insert
      ~tags:[tag_style_oblique] "value analysis";
    main_ui#annot_window#buffer#insert ". Selects ";
    main_ui#annot_window#buffer#insert ~tags:[tag_style_oblique] "-val";
    main_ui#annot_window#buffer#insert
      " option and sets parameters before pushing " ;
    main_ui#annot_window#buffer#insert ~tags:[tag_style_oblique] "Execute" ;
    main_ui#annot_window#buffer#insert " button.\n" ;
    main_ui#launcher ()
  end

(* To do an action and inform the user. *)
let gui_apply_action (main_ui:Design.main_window_extension_points) f x ~info =
  f x ;
  gui_annot_action main_ui info

let get_setting_option_text txt =
  "Setting option " ^ txt ^ " for the current project"

let gui_set_project (main_ui:Design.main_window_extension_points) proj_opt =
  gui_apply_action main_ui !Db.Slicing.Project.set_project proj_opt
    ~info:(fun () ->
             Extlib.may_map ~dft:"Clear slicing highlighting"
               (fun project -> ("Highlighting for " ^ (!Db.Slicing.Project.get_name project)))
               proj_opt) ;
  main_ui#rehighlight ()

let slicing_selector (popup_factory:GMenu.menu GMenu.factory)
    (main_ui:Design.main_window_extension_points) ~button localizable =
  if (not (Db.Value.is_computed ())) || not (Enabled.get ())
  then
    ignore
      (popup_factory#add_item "Enable _slicing"
         ~callback:
         (fun () ->
            if (not (Db.Value.is_computed ()))
            then gui_compute_values main_ui ;
            if Db.Value.is_computed ()
            then (Enabled.set true;
                  !update_column `Visibility)
         ))
  else
    let slicing_project = !Db.Slicing.Project.get_project () in
    if button = 1 then
      begin let level = 1 in
      let slicing_view project =
        gui_annot_info main_ui ~level (fun () -> "Highlighting for " ^ (!Db.Slicing.Project.get_name project))
      in
      Extlib.may slicing_view slicing_project;
      if SlicingParameters.verbose_atleast level then begin
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
                                   (Pretty_utils.sfprintf "<src>%a"
                                      !Db.Slicing.Mark.pretty (!Db.Slicing.Mark.get_from_src_func project kf))
                                 else
                                   "<   ><   >")
            | slices ->
                if !Db.Slicing.Project.is_called project kf
                then begin (* The source function is also called *)
                  assert (not (kf == fst (Globals.entry_point ()))) ;
                  add_mark_info (fun () ->
                                   Pretty_utils.sfprintf "<src>%a"
                                     !Db.Slicing.Mark.pretty (!Db.Slicing.Mark.get_from_src_func project kf))
                end ;
                let mark_slice slice =
                  add_mark_info (fun () -> Pretty_utils.sfprintf "%a" !Db.Slicing.Mark.pretty (get_mark slice))
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
      let slicing_factory =
        new Design.protected_menu_factory (main_ui:>Gtk_helper.host) submenu
      in
      (* definitions for slicing plug-in *)
      let add_slicing_item name ~callback v =
        let callback v =
          callback v;
          !update_column `Contents
        in
        add_item slicing_factory name ~callback v
        in
      let mk_slice = gui_mk_slice main_ui in
      let add_slice_menu kf_opt kf_ki_opt =
        (let callback kf =
           mk_slice
             ~info:(fun () ->
               Pretty_utils.sfprintf
                 "Request for slicing effects of function %a"
                 Kernel_function.pretty kf)
             (mk_selection_all !Db.Slicing.Select.select_func_calls_to kf)
         in
         add_slicing_item "Slice calls to" kf_opt ~callback);

        (let callback kf =
           mk_slice
             ~info:(fun () ->
               Pretty_utils.sfprintf
                 "Request for slicing entrance into function %a"
                 Kernel_function.pretty kf)
             (mk_selection_all !Db.Slicing.Select.select_func_calls_into kf)
         in
        add_slicing_item "Slice calls into" kf_opt ~callback);

        (let callback kf =
           mk_slice
             ~info:(fun () ->
               Pretty_utils.sfprintf
                 "Request for returned value of function %a"
                 Kernel_function.pretty kf)
             (mk_selection_all !Db.Slicing.Select.select_func_return kf)
         in
         add_slicing_item "Slice result"
           (Extlib.opt_filter
              (fun kf ->
                let is_not_void_kf x =
                  match x.Cil_types.vtype with
                  | Cil_types.TFun (Cil_types.TVoid (_),_,_,_) -> false
                  | _ -> true
                in is_not_void_kf (Kernel_function.get_vi kf))
              kf_opt)
           ~callback);

        (let callback (kf, ki) =
           mk_slice
             ~info:(fun () ->
               Pretty_utils.sfprintf
                 "Request for slicing effects of statement %d"
                 ki.sid)
             (mk_selection_all !Db.Slicing.Select.select_stmt ki kf)
         in
         add_slicing_item "Slice stmt" kf_ki_opt ~callback);

        (let callback (kf, ki) =
          let do_with_txt txt =
            try
              let lval_str =
                Datatype.String.Set.add txt Datatype.String.Set.empty
              in
              mk_slice
                ~info:(fun () ->
                  Pretty_utils.sfprintf
                    "Request for slicing Lvalue %s before statement %d"
                    txt
                    ki.sid)
                (mk_selection_cad !Db.Slicing.Select.select_stmt_lval
                   lval_str ~before:true ki ~scope:ki ~eval:ki kf)
            with e ->
              main_ui#error "Invalid expression: %s" (Printexc.to_string e)
          in
          let txt =
            GToolbox.input_string
              ~title:"Input a pure Lvalue expression to slice before current \
statement"
              ""
          in
          Extlib.may do_with_txt txt
        in
        add_slicing_item "Slice lval" kf_ki_opt ~callback);

        (let callback (kf, ki) =
           let do_with_txt txt =
             try
               let lval_str =
                 Datatype.String.Set.add txt Datatype.String.Set.empty
               in
               mk_slice
                 ~info:(fun () ->
                   Pretty_utils.sfprintf
                     "Request for slicing read accesses to Lvalue %s"
                     txt)
                 (mk_selection_cad
                    !Db.Slicing.Select.select_func_lval_rw
                    ~rd:lval_str
                    ~wr:Datatype.String.Set.empty
                    ~scope:ki
                    ~eval:ki kf)
             with e ->
               main_ui#error "Invalid expression: %s" (Printexc.to_string e)
           in
           let txt =
             GToolbox.input_string
               ~title:"Input a pure Lvalue expression to slice read accesses"
               ""
           in
           Extlib.may do_with_txt txt
         in
         add_slicing_item "Slice rd" kf_ki_opt ~callback);

        (let callback (kf, ki) =
           let do_with_txt txt =
             try
               let lval_str =
                 Datatype.String.Set.add txt Datatype.String.Set.empty
               in
               mk_slice
                 ~info:(fun () ->
                   Pretty_utils.sfprintf
                     "Request for slicing writen accesses to Lvalue %s"
                     txt)
                 (mk_selection_cad
                    !Db.Slicing.Select.select_func_lval_rw
                    ~rd:Datatype.String.Set.empty
                    ~wr:lval_str
                    ~scope:ki
                    ~eval:ki kf)
             with e ->
               main_ui#error "Invalid expression: %s" (Printexc.to_string e)
           in
           let txt =
             GToolbox.input_string
               ~title:"Input a pure Lvalue expression to slice read accesses"
               ""
           in
           Extlib.may do_with_txt txt
         in
        add_slicing_item "Slice wr" kf_ki_opt ~callback);

        let callback (kf, ki) =
          mk_slice
            ~info:(fun () ->
              Pretty_utils.sfprintf
                "Request for slicing accessibility to statement %d"
                ki.sid)
            (mk_selection_all !Db.Slicing.Select.select_stmt_ctrl ki kf)
        in
        add_slicing_item "Slice ctrl" kf_ki_opt ~callback
      in
      let some_kf_from_vi vi =
        try let kf = Globals.Functions.get vi in
        if Enabled.get () && !Db.Value.is_called kf then Some kf else None
        with Not_found -> None in
      let some_kf_from_lv  lv =
        match lv with
        | Var vi,_ -> some_kf_from_vi vi
        | _ -> None in
      let some_kf_ki kf stmt =
        if Enabled.get ()
          && !Db.Value.is_called kf
          && Db.Value.is_reachable_stmt stmt
        then Some (kf, stmt) else None in
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
        ~callback:(fun () -> Enabled.set false);
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
    (buffer:GSourceView2.source_buffer) localizable ~start ~stop =
  if Enabled.get () then begin
    (* Definition for highlight 'Slicing' *)
    let highlight project =
      let ki = Pretty_source.ki_of_localizable localizable in
      if Db.Value.is_accessible ki then
        let unused_code_area =
          Gtk_helper.make_tag buffer
            ~name:"slicing_unused" [`STRIKETHROUGH true ]
        in
        let spare_code_area =
          Gtk_helper.make_tag buffer ~name:"slicing_spare" [`UNDERLINE `LOW] in
        let necessary_code_area =
          Gtk_helper.make_tag buffer
            ~name:"slicing_necessary" [`BACKGROUND "green"]
        in
        let apply_on_one_project_and_merge_slices kf pb pe mark_of_slice =
          let apply_mark mark =
            if SlicingParameters.debug_atleast 1 then
              SlicingParameters.debug "Got mark: %a"
                !Db.Slicing.Mark.pretty mark;
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
                  SlicingParameters.debug "Got source code@." ;
                  apply_mark (!Db.Slicing.Mark.get_from_src_func project kf)
                end
                else
                  Gtk_helper.apply_tag buffer unused_code_area pb pe
            | slices ->
                if !Db.Slicing.Project.is_called project kf
                then begin
                  assert (not (kf == fst (Globals.entry_point ()))) ;
                  SlicingParameters.debug "Got source code" ;
                  apply_mark (!Db.Slicing.Mark.get_from_src_func project kf)
                end ;
                if SlicingParameters.debug_atleast 1 then begin
                  let l = List.length slices in
                  if l >=2 then
                    SlicingParameters.debug "Got %d slices" (List.length slices)
                end;
                let mark_slice slice =
                  let mark = mark_of_slice project slice in
                  apply_mark mark
                in List.iter mark_slice slices
          end
        in
        let tag_stmt kf stmt pb pe =
          assert (Db.Value.is_reachable_stmt stmt) ;
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
        | Pretty_source.PTermLval _
        | Pretty_source.PGlobal _
        | Pretty_source.PIP _ -> ()
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
  let old = SlicingParameters.Verbose.get () in
    if v <> old then (* Otherwise set is done at every refreshing *)
      gui_apply_action main_ui SlicingParameters.Verbose.set v
        ~info:(fun () -> get_setting_option_text ("-slicing-debug \"-debug " ^ (string_of_int v) ^ "\""))

let gui_set_slicing_level (main_ui:Design.main_window_extension_points) v =
  let old = SlicingParameters.Mode.Calls.get () in
    if v != old then (* Otherwise set is done at every refreshing *)
      gui_apply_action main_ui SlicingParameters.Mode.Calls.set v
        ~info:(fun () -> get_setting_option_text ("-slicing-level " ^ (string_of_int v)))

let gui_set_slicing_undef_functions (main_ui:Design.main_window_extension_points) v =
  let old = SlicingParameters.Mode.SliceUndef.get () in
    if v != old then (* Otherwise set is done at every refreshing *)
      gui_apply_action main_ui SlicingParameters.Mode.SliceUndef.set v
        ~info:(fun () -> get_setting_option_text (if v then "-slicing-undef-functions" else "-no-slice-undef-functions"))

let slicing_panel (main_ui:Design.main_window_extension_points) =
  let w = GPack.vbox  () in
  let hbox1 = GPack.hbox
    ~packing:w#pack () in
  let activate_button =
    let b = GButton.button ~label:"Activate"
      ~packing:hbox1#pack () in
      main_ui#help_message b "%s" msg_help_compute_values ;
      ignore (b#connect#pressed
                (fun () -> gui_compute_values main_ui ));

      b
  in
  let combo_box_text =
    let ((combo_box, (_model, column)) as combo_box_text) =
      GEdit.combo_box_text ~strings:[ none_text ] ~wrap_width:3 ~use_markup:true
        ~packing:(hbox1#pack ~expand:true ~fill:true) () in
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
      combo_box_text
  in
  let table = GPack.table ~columns:2 ~rows:2 ~homogeneous:true ~packing:w#pack () in

  let hbox2 = GPack.hbox ~packing:(table#attach ~left:1 ~top:0) () in

  (* [enabled_button] to give slicing menu available *)
  let do_refresh to_enable =
    if to_enable then gui_compute_values main_ui;
    !update_column `Visibility;
    main_ui#rehighlight ();
  in
  let enabled_button =
    let b = GButton.check_button
      ~label:"Enable"
      ~active:(Enabled.get ())
      ~packing:(table#attach ~left:0 ~top:0) () in
      main_ui#help_message b "%s" msg_help_enable_gui ;
      ignore (b#connect#toggled
                ~callback:(fun () ->
                             Enabled.set b#active;
                             do_refresh b#active));
      b
  in
  let verbose_refresh = Gtk_helper.on_int ~lower:0 ~upper:3
    hbox2
    "Verbosity"
     ~sensitive:Enabled.get
    SlicingParameters.Verbose.get
    (gui_set_slicing_debug main_ui)
  in
  let hbox3 = GPack.hbox ~packing:(table#attach ~left:1 ~top:1) () in
    (* [slice_undef_button] related to -slice-undef option *)
  let slice_undef_button =
    let b = GButton.check_button
      ~label:"Libraries"
      ~active:(Enabled.get ())
      ~packing:(table#attach ~left:0 ~top:1) () in
      main_ui#help_message b "%s" msg_help_libraries ;
      ignore (b#connect#toggled
                (fun () ->
                   gui_set_slicing_undef_functions main_ui b#active));
      b
  in
  let level_refresh = Gtk_helper.on_int ~lower:0 ~upper:3
    hbox3
    "Level"
    ~sensitive:Enabled.get
    SlicingParameters.Mode.Calls.get
    (gui_set_slicing_level main_ui)
  in
    Project.register_after_set_current_hook
      ~user_only:true
      (fun _ -> rebuild_model combo_box_text);

    let refresh () =
      let value_is_computed = Db.Value.is_computed () in
      let slicing_project = !Db.Slicing.Project.get_project () in
      let enabled = Enabled.get () in
      activate_button#misc#set_sensitive (not value_is_computed) ;
      enabled_button#misc#set_sensitive value_is_computed ;
      slice_undef_button#misc#set_sensitive enabled ;
      verbose_refresh ();
      level_refresh ();
      if Enabled.get () <> enabled_button#active then (
        enabled_button#set_active (Enabled.get ());
        !update_column `Contents;
      );
      slice_undef_button#set_active (SlicingParameters.Mode.SliceUndef.get());
      refresh_combo_box combo_box_text slicing_project
        (enabled && value_is_computed)
    in
    refresh () ;
    "Slicing",w#coerce,Some refresh

let file_tree_decorate (file_tree:Filetree.t) =
  update_column :=
    file_tree#append_pixbuf_column
      ~title:"Slicing"
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
              [`STOCK_ID "gtk-apply"]
            else
              [`STOCK_ID ""])
          ~dft:[`STOCK_ID ""]
          (!Db.Slicing.Project.get_project ()))
      (fun () -> Enabled.get ());
  !update_column `Visibility

let main (main_ui:Design.main_window_extension_points) =
  main_ui#register_source_selector slicing_selector;
  main_ui#register_source_highlighter slicing_highlighter;
  main_ui#register_panel slicing_panel;
  file_tree_decorate main_ui#file_tree

let () =
  Design.register_extension main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
