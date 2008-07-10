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

(** Main GUI skeleton *)
open Cil_types
open Cil
open Db_types
open Db
open Pretty_source
open Gtk_helper

let all_files () =
  let f = GFile.filter ~name:"Source files" () in
  f#add_pattern "*.c" ;
  f#add_pattern "*.h" ;
  f#add_pattern "*.i" ;
  f#add_pattern "*.C" ;
  f#add_pattern "*.cpp" ;
  f#add_pattern "*.CPP" ;
  f

module Configure_Hook = Hook.Make(struct end)

let run_configure_dialog main_ui = 
  Cmdline.clear_selected_options ();
  let d = 
    GWindow.dialog 
      ~position:`CENTER 
      ~modal:true 
      ~height:480 
      ~width:480 
      ~title:"Configure analysis" ~show:true () 
  in
  let nb = GPack.notebook ~packing:d#vbox#add () in

  let sw_base = 
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:
      (fun w -> ignore (nb#prepend_page
                          ~tab_label:(GMisc.label ~text:"Basic" ())#coerce w))
      ()
  in
  let basic_options_container = GPack.box `VERTICAL ~packing:sw_base#add_with_viewport () in

  let sw_experts = 
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:
      (fun w -> ignore (nb#append_page
                          ~tab_label:(GMisc.label ~text:"Experts" ())#coerce w))
      ()
  in
  let experts_options_container = GPack.box `VERTICAL ~packing:sw_experts#add_with_viewport () 
  in

  d#add_button_stock `OK `OK;
  d#add_button_stock `CANCEL `CANCEL;
  let all_function_names = 
    List.sort
      Pervasives.compare
      (Globals.Functions.fold 
	 (fun kf acc -> (Kernel_function.get_name kf)::acc)
	 [])
  in
  (*  d#set_default_response `CLOSE;*)
  let entrypoint_name, is_lib = 
    try 
      let kf, is_lib = Globals.entry_point () in
      Kernel_function.get_name kf, is_lib
    with Globals.No_such_entry_point _ -> 
      "", Cmdline.LibEntry.get ()
  in
  let entry_box = GPack.hbox ~packing:basic_options_container#pack ~spacing:5 () in
  ignore (GMisc.label ~packing:entry_box#pack ~text:"Entry point:" ());
  let entry = Gtk_helper.string_selector all_function_names entry_box#pack in
  let () = entry#set_text entrypoint_name in
  let islib_button = 
    GButton.check_button 
      ~label:"Library" ~packing:entry_box#pack ~active:is_lib ()
  in
  let set_entry () = 
    try Globals.set_entry_point entry#text islib_button#active 
    with Not_found -> ()
  in
  ignore 
    (entry#event#connect#focus_out 
       (fun _ -> Configure_Hook.extend (fun () -> set_entry ());false));
  ignore 
    (islib_button#connect#toggled 
       (fun () -> Configure_Hook.extend (fun () -> set_entry ())));
  let check_response () = 
    let r = entry#text in
    if r <> "" then 
      try 
        ignore (Globals.Functions.find_def_by_name r)
      with Not_found -> 
        Format.printf "%s is not a function definition.@." r
  in
  let on_bool (container:GPack.box) label get set = 
    let button = GButton.check_button 
      ~label
      ~packing:container#pack
      ~active:(get ())
      ()
    in
    ignore 
      (button#connect#toggled 
	 ~callback:
	 (fun () -> Configure_Hook.extend (fun () -> set button#active)))
  in
  let on_int (container:GPack.box) label get set = 
    let make_spin ~label ~value =
      let container = GPack.hbox ~packing:container#pack () in
      let x = GEdit.spin_button ~digits:0 ~packing:container#pack () in
      x#adjustment#set_bounds ~lower:0. ~upper:max_float ~step_incr:1. ();
      x#adjustment#set_value (float value);
      ignore 
	(x#connect#value_changed 
	   ~callback:
           (fun () -> Configure_Hook.extend (fun () -> set x#value_as_int)));
      ignore (GMisc.label ~text:label ~xalign:0. ~packing:container#pack ())
    in
    make_spin ~label ~value:(get ()) 
  in
  let on_string (container:GPack.box) label get set = 
    let container = GPack.hbox ~packing:container#pack () in
    let entry = GEdit.entry ~packing:container#pack ~text:(get ()) () in
    let callback _ = Configure_Hook.extend (fun () -> set entry#text); false in
    ignore (entry#event#connect#focus_out ~callback);
    ignore (GMisc.label ~packing:container#pack ~text:label ())
  in
  let on_string_set (container:GPack.box) label get set = 
    let container = GPack.hbox ~packing:container#pack () in
    let entry = GEdit.entry ~packing:container#pack ~text:(get()) () in
    let callback _ = 
      Configure_Hook.extend (fun () -> set (entry#text)); false 
    in
    ignore (entry#event#connect#focus_out ~callback);
    ignore (GMisc.label ~packing:container#pack ~text:(label^" (list)") ())
  in
  on_bool basic_options_container "Values" 
    Cmdline.ForceValues.get Cmdline.ForceValues.set;
  on_bool basic_options_container "Functional dependencies" 
    Cmdline.ForceDeps.get Cmdline.ForceDeps.set;
  on_bool basic_options_container "Call-site wise functional dependencies " 
    Cmdline.ForceCallDeps.get Cmdline.ForceCallDeps.set;
  Cmdline.iter_on_options 
    (fun (k, s) -> match k with
     | Cmdline.Bool ({Cmdline.get=get; set= set}) -> 
	 on_bool experts_options_container s get set
     | Cmdline.Int ({Cmdline.get=get; set= set}) -> 
	 on_int experts_options_container s get set
     | Cmdline.String ({Cmdline.get=get; set= set}) -> 
         on_string experts_options_container s get set
     | Cmdline.StringSet ({Cmdline.get=get; set= set}) -> 
         on_string_set experts_options_container s get set);
  match d#run () with 
  | `OK ->
      check_response ();
      Configure_Hook.apply ();
      main_ui#reset ();
      d#destroy ();
      Configure_Hook.clear ()
  | `DELETE_EVENT | `CANCEL -> 
      Configure_Hook.clear ();
      d#destroy ()
    
let use_external_viewer = false

let highlight_range ~scroll tag (v:GSourceView.source_view) pb pe =
  let b = v#source_buffer in
  let start = b#get_iter (`OFFSET pb) in
  let stop = b#get_iter (`OFFSET pe) in
  b#apply_tag tag start stop;
  if scroll then begin
    b#place_cursor start;
    ignore (v#scroll_to_mark `INSERT)
  end

(** The initial menus and toolbar *)
let initial_ui_info = "<ui>
  <menubar name='MenuBar'>
    <menu action='FileMenu'>
      <separator/>
    </menu>
    <menu action='AnalysesMenu'>
      <separator/>
    </menu>
  </menubar>
  <toolbar name='ToolBar'>
  </toolbar>
</ui>"

let basic_ui_info = "<ui>
  <menubar name='MenuBar'>
    <menu action='FileMenu'>
      <menuitem action='New'/>
      <menuitem action='Load'/>
      <menuitem action='Save'/>
      <menuitem action='Quit'/>
    </menu>
    <menu action='AnalysesMenu'>
      <menuitem action='Configure'/>
      <menuitem action='Run'/>
    </menu>
  </menubar>

  <toolbar name='ToolBar'>
      <toolitem action='New'/>
      <toolitem action='Configure'/>
      <toolitem action='Run'/>
      <toolitem action='Quit'/>
  </toolbar>
</ui>"

(** *)
class type main_window_extension_points = object
  method ui_manager : GAction.ui_manager
  method actions : GAction.action_group

  method file_tree : Filetree.t
  method toplevel : main_window_extension_points
  method main_window : GWindow.window
  method annot_window : GText.view

  method source_viewer : GSourceView.source_view
  method display_globals : global list -> GSourceView.source_buffer

  method register_source_selector : 
    (GMenu.menu GMenu.factory -> main_window_extension_points -> button:int
       -> Pretty_source.localizable -> unit) 
    -> unit
    
  method register_source_highlighter : 
    (GSourceView.source_buffer -> localizable -> start:int -> stop:int -> unit)
    -> unit

  method rehighlight : unit -> unit
  method scroll : localizable -> unit

  method original_source_viewer : Source_manager.t
  method view_original_stmt : stmt -> location
  method view_original : location -> unit

  method reset : unit -> unit

  method lock : unit -> unit
  method unlock : unit -> unit

  method monospace : Pango.font_description
  method general : Pango.font_description

  method info : 'a. ('a, Format.formatter, unit) format -> 'a
end

(** Create a new project *)
let new_project (main_ui:main_window_extension_points) =
  let dialog = GWindow.dialog 
    ~width:800
    ~height:400
    ~modal:true 
    ~title:"Create Project" 
    ~parent:main_ui#main_window
    ~destroy_with_parent:true
    ()
  in
    dialog#add_button_stock `CLOSE `CANCEL ;
    dialog#add_button_stock `NEW `OPEN;
    let hbox = GPack.box `HORIZONTAL ~packing:dialog#vbox#add () in
    let filechooser = GFile.chooser_widget
      ~action:`OPEN
      ~packing:(hbox#pack ~expand:true ~fill:true)
      ()
    in
    filechooser#set_select_multiple true;
    filechooser#add_filter (all_files ());

    let bbox = 
      GPack.button_box 
	~layout:`START
	`VERTICAL ~packing:(hbox#pack ~expand:false 
						      ~fill:false) () in
    let add_button = GButton.button ~stock:`ADD ~packing:bbox#add () in
    let remove_button = GButton.button ~stock:`REMOVE ~packing:bbox#add () in
    let w = 
      GBin.scrolled_window
	~vpolicy:`AUTOMATIC
	~hpolicy:`AUTOMATIC
	~packing:(hbox#pack ~expand:true ~fill:true)
	()
    in
    let add,remove,get_all = 
      Gtk_helper.make_string_list ~packing:w#add 
    in
    let add_selected_files () = 
      let add_file f = add f in
      let f = filechooser#get_filenames in
	List.iter add_file f	
    in
    ignore (add_button#connect#pressed ~callback:add_selected_files);
    ignore (remove_button#connect#pressed ~callback:remove);
    ignore (filechooser#connect#file_activated ~callback:add_selected_files);
    begin match dialog#run () with
      | `OPEN ->
	  let files = 
	    List.map File.from_filename (get_all ())
	  in
          let project = Project.create "interactive" in
          Project.set_current project;
	  File.init_from_c_files files;
	  main_ui#reset ()
      | `DELETE_EVENT | `CANCEL -> ()
    end ;
    dialog#destroy ()
    
(** Load a project file *)
let load_file (main_ui:main_window_extension_points) =
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Open state"
    ~parent:main_ui#main_window () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  begin match dialog#run () with
  | `OPEN ->
      begin match dialog#filename with
      | None -> ()
      | Some f ->
          (try Project.load_all f
           with Sys_error s -> main_ui#info "Cannot load: %s" s);
          main_ui#reset ()
      end
  | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ()

(** Load a project file *)
let save_file (main_ui:main_window_extension_points) =
  let dialog = GWindow.file_chooser_dialog
    ~action:`SAVE
    ~title:"Save state"
    ~parent:main_ui#main_window () in
  (*dialog#set_do_overwrite_confirmation true ; only in latter lablgtk2 *)
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `SAVE `SAVE ;
  begin match dialog#run () with
  | `SAVE ->
      begin match dialog#filename with
      | None -> ()
      | Some f ->
          try Project.save_all f
          with Sys_error s -> main_ui#info "Cannot save: %s" s
      end
  | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ()

(** The list of registered extension *)
let (handlers:(main_window_extension_points -> unit) list ref) = ref []
(** Insert an extension *)
let register_extension f =
  handlers := f::!handlers
(** Apply all extensions *)
let process_extensions window =
  List.iter (fun f -> f window) (List.rev !handlers)

(** The list of reset extensions. 
    Such extensions are used for example when the current project
    is changed. *)
let (reset_handlers:(main_window_extension_points -> unit) list ref) = ref []

(** Insert a reset extension *)
let register_reset_extension f =
  reset_handlers := f::!reset_handlers

(** Apply all reset extensions *)
let reset_extensions window =
  List.iter (fun f -> f window) (List.rev !reset_handlers)


(** Memoization of displayed globals *)
module Globals_GUI = 
struct 
  include Hashtbl.Make(struct 
                         type t = global list
                         let equal = (==)
                         let hash = Hashtbl.hash
                       end)
  let tbl = create 17
  let find = find tbl
  let add = add tbl
  let clear () = clear tbl
end

let filetree_selector 
    (main_ui:main_window_extension_points) 
    ~was_activated
    ~activating
    globals 
    =
  if not was_activated && activating then begin
    let source = main_ui#source_viewer in
    let b = main_ui#display_globals globals in
    source#set_buffer (b:>GText.buffer);
    source#scroll_to_mark ~use_align:true ~xalign:1.0 ~yalign:0.5 `INSERT;
    let print_one_global (v,loc) = 
      main_ui#lock ();
      begin 
        main_ui#view_original loc;
        try 
          let kf = Globals.Functions.get v in
          let text = fprintf_to_string  
            "Function '%a'\n" Kernel_function.pretty_name kf
          in
            main_ui#annot_window#buffer#insert text
        with Not_found -> 
          main_ui#annot_window#buffer#insert "No such function: please report\n"
      end;
      main_ui#unlock ()
    in
    begin match globals with
    | [] -> main_ui#annot_window#buffer#set_text "No globals in this file\n"
    | [GFun ({svar=v},loc)] -> 
	main_ui#annot_window#buffer#set_text "";
	print_one_global (v,loc)
    | l -> 
	let first_global = ref None in
          main_ui#annot_window#buffer#set_text 
	    ("Defined globals: ");
	  List.iter 
	    (function 
	       | GFun ({svar=v},loc) -> 
		   (match !first_global with 
		      | None -> first_global:=Some (v,loc)
		      | Some _ -> ());
		   main_ui#annot_window#buffer#insert 
		     (fprintf_to_string "%a{f} " Ast_info.pretty_vname v)
	       | GType ({tname=name},_) -> 
		   main_ui#annot_window#buffer#insert 
		     (fprintf_to_string "%s{t} " name)
	       | GCompTagDecl ({cname=name},_) | GCompTag ({cname=name},_)-> 
		   main_ui#annot_window#buffer#insert 
		     (fprintf_to_string "%s{c} " name)
		     
	       | GEnumTagDecl ({ename=name},_) | GEnumTag ({ename=name},_)-> 
		   main_ui#annot_window#buffer#insert 
		     (fprintf_to_string "%s{e} " name)
	       | GVarDecl (_,v,_) -> 
		   main_ui#annot_window#buffer#insert 
		     (fprintf_to_string "%a{d} " Ast_info.pretty_vname v)

	       | GVar (v,_,_) -> 
		   main_ui#annot_window#buffer#insert 
		     (fprintf_to_string "%a{v} " Ast_info.pretty_vname v)
	       | GAsm _ -> main_ui#annot_window#buffer#insert "{asm} "
	       | GPragma _ -> main_ui#annot_window#buffer#insert "{pragma} "
	       | GText _ -> main_ui#annot_window#buffer#insert "{comment} " 
	       | GAnnot _ -> main_ui#annot_window#buffer#insert "{logic} ")
	    l;
	  main_ui#annot_window#buffer#insert "\n";
	  Extlib.may print_one_global !first_global
    end
  end
    
let rec to_do_on_select
    (menu_factory:GMenu.menu GMenu.factory)
    (main_ui:main_window_extension_points)
    ~button
    selected
    =
  let tree_view = main_ui#file_tree in
  let annot = main_ui#annot_window#buffer in
  if button = 1 then
    begin match selected with
    | PStmt (kf,ki) -> begin
        (* Find kinstr and kf *)
	try
          let loc = main_ui#view_original_stmt ki in
          let skind =
            if (Cmdline.Debug.get ()> 0) then
              match ki with
              | {skind=Block _} -> "Block "
              | {skind=Instr (Skip _)} -> "Skip "
              | _ -> ""
            else ""
          in
	  let n =
            Format.sprintf
              "Function: %s@\n%sStatement: %d (line %d)@\n"
              (fprintf_to_string "%a" Kernel_function.pretty_name kf)
              skind
              ki.sid
              (fst loc).Lexing.pos_lnum
          in
	  annot#set_text n;

          (* Code annotations for this statement *)
	  List.iter
	    (fun a ->
	       let pos,a = match a with
	       | Before a -> "Before", a
	       | After a -> "After", a
	       in
	       let user,s = match a with
	       | WP (a,_) ->
		   "WP",
		   begin
                     try
                       let a = List.find
                         (function Fol.Goal _ -> true | _ -> false)
                         a
                       in
                       let a = match a with
                       | Fol.Goal (_,a) -> a
                       | _ -> assert false
                       in
                       fprintf_to_string "%a" Why_output.predicate a
                     with Not_found -> "no such goal"
                   end
	       | User a ->
		   "user",
		   fprintf_to_string "%a" !Ast_printer.d_code_annotation a
	       | AI (_,a) ->
                   "alarm",
		   fprintf_to_string "%a" !Ast_printer.d_code_annotation a
	       in
	       annot#insert (Format.sprintf "%s(%s): %s@\n" pos user s))
	    (Annotations.get ki);

	with e ->
	  annot#insert ("error in Db.KernelFunction.find ("
                        ^ (Printexc.to_string e)
                        ^ ")")
      end
    | PLval (kf, ki,lv) ->
        begin try
          begin
            match ki with Kglobal -> ()
            | Kstmt ki -> ignore (main_ui#view_original_stmt ki)
          end;
          let ty = typeOfLval lv in
          if isFunctionType ty then
            annot#set_text "This is a C function\n"
          else begin
            let sid = Ast_info.get_sid ki in
	    let n =
              Format.sprintf
                "Function: %s@\nStatement: %d@\n"
                (match kf with None -> "<none>"
                 | Some kf ->
		     fprintf_to_string "%a" Kernel_function.pretty_name kf)
                sid
            in
	    annot#set_text n;
            let vars = extract_varinfos_from_lval lv in
            VarinfoSet.iter
              (fun vi ->
                 annot#insert
                   (Format.sprintf
                      "Variable %s has type \"%s\".\nIt is a %s variable.\n%sIt is %sreferenced and its address is %staken.\n"
                      (fprintf_to_string "%a" Ast_info.pretty_vname vi)
                      (fprintf_to_string "%a" !Ast_printer.d_type vi.vtype)
                      (if vi.vglob then "global" else "local")
                      (match vi.vdescr with None -> ""
                       | Some s ->
                           Format.sprintf "This is a temporary variable for \"%s\".\n" s)
                      (if vi.vreferenced then "" else "not ")
                      (if vi.vaddrof then "" else "not ")))
              vars
          end
        with Not_found ->
          annot#insert "Error in lval Db.KernelFunction.find\n"
        end
    | PTermLval _ ->
	() (* JS: TODO (?) *)
    | PVDecl (kf,vi) ->
        main_ui#view_original vi.vdecl;
        if vi.vglob
        then
          annot#set_text
            (Format.sprintf "This is the declaration of global %s\nIt is %sreferenced and its address is %staken.\n"
               (fprintf_to_string "%a" Ast_info.pretty_vname vi)
               (if vi.vreferenced then "" else "not ")
               (if vi.vaddrof then "" else "not "))
        else
          annot#set_text
            (Format.sprintf "This is the declaration of local %s in function %s\n%s"
               (fprintf_to_string "%a" Ast_info.pretty_vname vi)
               (fprintf_to_string "%a"
		  Kernel_function.pretty_name (Cilutil.out_some kf))
               (match vi.vdescr with None -> ""
                | Some s ->  Format.sprintf "This is a temporary variable for \"%s\".\n" s))
    end
  else if button = 3 then begin
    match selected with
    | PVDecl _ -> ()
    | PStmt (kf,ki) ->
        let add_assert_after _ =
          let txt =
            GToolbox.input_string
              ~title:"Insert an assertion after"
              ""
          in
          match txt with
          | None -> ()
          | Some txt -> try
              Db.Properties.add_assert
		kf ki ~before:false
		("assert " ^ txt ^ ";");
              to_do_on_select menu_factory main_ui ~button:1 selected
	    with e ->
	      annot#set_text (Printexc.to_string e)
        in
        let add_assert_before _ =
          let txt =
            GToolbox.input_string
              ~title:"Insert an assertion before"
              ""
          in
          match txt with
          | None -> ()
          | Some txt -> try
              Db.Properties.add_assert
		kf ki ~before:true ("assert " ^ txt ^ ";");
              to_do_on_select menu_factory main_ui ~button:1 selected
	    with e ->
	      annot#set_text (Printexc.to_string e)
        in
        begin
          ignore (menu_factory#add_item
		    "Add assert _after" ~callback:add_assert_after);
          ignore (menu_factory#add_item
		    "Add assert _before" ~callback:add_assert_before)
        end
    | PLval (_kf, _ki, lv) ->
        let ty = typeOfLval lv in
        (* Do special actions for functions *)
        (* popup a menu to jump the definitions of the given varinfos *)
        let do_menu l =
          match l with
          | [] -> ()
          | _ ->
              try
                List.iter
                  (fun v ->
                     ignore
                       (menu_factory#add_item
                          ("Go to definition of " ^
                             (Pretty_utils.escape_underscores
				(fprintf_to_string "%a"
				   Ast_info.pretty_vname v)))
                          ~callback:
                          (fun () ->
                             tree_view#select_global v)))
                  l
              with Not_found -> ()
        in
        (match lv with
         | Var v,NoOffset when isFunctionType ty ->
             (* only simple literal calls can be resolved syntactically *)
             do_menu [v]
         | _  -> ())
    | PTermLval _ ->
	() (* JS: TODO (?) *)

  end

(** The main application window *)
class main_window () : main_window_extension_points =
  let main_window =
    GWindow.window
      ~title:"Frama-C"
      ~width:1024 ~height:768
      ~allow_shrink:true
      ~allow_grow:true
      ~show:false
      ()
  in

  let watch_cursor = Gdk.Cursor.create `WATCH in
  let arrow_cursor = Gdk.Cursor.create `ARROW in
  (* On top one finds the menubar *)
  let toplevel_vbox = GPack.box `VERTICAL ~packing:main_window#add ()
  in

  (* Build the main menubar *)
  (* The menu thing with one default action to quit *)
  let actions = GAction.action_group ~name:"Actions" () in
  (* Make the ui manager thing *)
  let ui_manager = GAction.ui_manager () in
  (* Connect accelerators *)
  let () = main_window#add_accel_group ui_manager#get_accel_group in

  let () = 
    GAction.add_actions actions 
      [ GAction.add_action "FileMenu" ~label:"_File";
        GAction.add_action "AnalysesMenu" ~label:"_Analyses" ;
      ]
  in
  
  (* Register actions for menubar *)
  let () =
    ui_manager#insert_action_group actions 0;
    ignore (ui_manager#add_ui_from_string initial_ui_info )
  in

  (* And now: the menubar *)
  let menubar = new GMenu.menu_shell
    (GtkMenu.MenuBar.cast (ui_manager#get_widget "ui/MenuBar")#as_widget)
  in
  (* Pack the menu bar on top of gui *)
  let () = toplevel_vbox#pack ~fill:false menubar#coerce
  in

  (* And the toolbar *)
  let toolbar = new GButton.toolbar
    (GtkButton.Toolbar.cast (ui_manager#get_widget "ui/ToolBar")#as_widget)
  in
  (* Pack the menu bar on top of gui *)
  let () = toplevel_vbox#pack ~fill:false toolbar#coerce
  in

  (* Split below the bars *)
  let toplevel_hpaned = GPack.paned `HORIZONTAL ~packing:toplevel_vbox#add () in

  (* The left filetree inside an automatic scrolled window and a nice frame *)
  let filetree_frame = GBin.frame ~shadow_type:`ETCHED_OUT ~packing:toplevel_hpaned#add1 () in
  let filetree_scrolled_window =
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`NEVER
      ~packing:filetree_frame#add ()
  in
  let file_tree = GTree.view ~packing:filetree_scrolled_window#add () in
  let () = file_tree#selection#set_mode `NONE in
  let _ = main_window#misc#connect#after#show (fun _ -> file_tree#selection#set_mode `BROWSE) in
  let () = file_tree#set_rules_hint true in
  let file_tree = Filetree.make file_tree in

  (* bottom hbox *)
  let bottom_hbox = GPack.box `HORIZONTAL ~packing:toplevel_vbox#pack ()
  in

  (* status bar (at bottom) *)
  let statusbar = 
    GMisc.statusbar ~has_resize_grip:false ~packing:bottom_hbox#add () 
  in
  let status_context = statusbar#new_context "messages" in
  let _ = status_context#push "Ready" in
  let _ = status_context#push "Ready" in

  (* progress bar (at bottom) *)
  let progress_bar = 
    GRange.progress_bar 
      ~pulse_step:0.01 
      ~packing:(bottom_hbox#pack 
		  ~fill:false) 
      () 
  in
  let lock_gui lock = 
    actions#set_sensitive (not lock);
    Gtk_helper.gui_unlocked:=not lock;
    if lock then 
      ignore (Glib.Timeout.add ~ms:25 
		~callback:(fun () -> 
			     progress_bar#pulse (); 
			     not !Gtk_helper.gui_unlocked));
    Gdk.Window.set_cursor 
      main_window#misc#window 
      (if lock then watch_cursor else arrow_cursor);
    if lock then (progress_bar#misc#show ();
                  ignore (status_context#push "Computing")) else 
      (status_context#pop();
       progress_bar#misc#hide ())
  in
  let lock,unlock,update_gui = 
    (fun () -> lock_gui true),
    (fun () -> lock_gui false),
    (fun () -> while Glib.Main.iteration false do () done)
  in
  (* splits between messages and sources *)
  let vb_message_sources =
    GPack.paned `VERTICAL  ~border_width:3 ~packing:toplevel_hpaned#add2 ()
  in

  (* splits between messages and sources *)
  let hb_sources =
    GPack.paned `HORIZONTAL  ~border_width:3 ~packing:vb_message_sources#add1 ()
  in

  (* Set the relative position for all paned whenever the main window is resized *)
  let () =
    ignore (main_window#misc#connect#size_allocate
	      (let old_w = ref 0
	       and old_h = ref 0 in
	       fun {Gtk.width=w;Gtk.height=h} ->
	         if !old_w <> w or !old_h <> h then
		   begin
		     old_h := h;
		     old_w := w;
                     hb_sources#set_position
                       ((hb_sources#max_position - hb_sources#min_position)/2);
                     vb_message_sources#set_position
                       ((vb_message_sources#max_position - vb_message_sources#min_position)*5/7);
                     (* toplevel_hpaned#set_position
                        ((toplevel_hpaned#max_position - toplevel_hpaned#min_position)*3/10);*)
		   end))
  in

  (* lower notebook *)
  let fr2 = GBin.frame ~shadow_type:`ETCHED_OUT ~packing:vb_message_sources#add2 () in
  let lower_notebook =
    GPack.notebook ~scrollable:true ~show_tabs:true ~packing:fr2#add ()
  in
  (*  let count = ref 0 in
      let _id = Glib.Timeout.add
      ~ms:10000
      ~callback:(fun () ->
      Warning_manager.append
      warning_manager
      { Warning_manager.m_file = "Here";
      m_line = (incr count; !count);
      m_msg = "Watch me!" ;
      m_severity = `Warn}
      ~on_select:(fun s -> assert false);
      true(*repeat ad lib*)
      )
      in
  *)
  (* lower text view and its scroll view: annotations and messages *)
  let sw = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC
    ~hpolicy:`AUTOMATIC
    ~packing:
    (fun w -> ignore (lower_notebook#prepend_page
                        ~tab_label:(GMisc.label ~text:"Informations" ())#coerce w))
    ()
  in

  let annot_window = GText.view ~packing:sw#add () in

  let _ = annot_window#set_editable false in

  (* upper text view: source code *)
  let fr1 = GBin.frame ~shadow_type:`ETCHED_OUT ~packing:hb_sources#add1 () in
  let sw = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC
    ~hpolicy:`AUTOMATIC
    ~packing:fr1#add ()
  in

  let source_viewer = Source_viewer.make ~packing:sw#add in

  let original_source_viewer = Source_manager.make ~packing:hb_sources#add2 in

  (* Remove default pango menu for textviews *)
  let () =
    ignore (source_viewer#event#connect#button_press ~callback:
	      (fun ev -> GdkEvent.Button.button ev = 3));
    ignore (annot_window#event#connect#button_press ~callback:
	      (fun ev -> GdkEvent.Button.button ev = 3));
    (* startup configuration *)
    source_viewer#buffer#place_cursor
      ~where:source_viewer#buffer#start_iter
  in
object (self:#main_window_extension_points)
  val mutable selector = []
  val mutable highlighter = []

  method lock = lock
  method unlock = unlock 

  method monospace = Pango.Font.from_string 
    (Cmdline.MonospaceFontName.get ())
  method general = Pango.Font.from_string 
    (Cmdline.GeneralFontName.get ())

  method toplevel = (self:>main_window_extension_points)
  method main_window = main_window
  method ui_manager = ui_manager
  method actions = actions
  val mutable file_tree = file_tree
  method file_tree = file_tree
  method annot_window = annot_window
  method source_viewer = source_viewer
  method register_source_selector f = selector <- f::selector
  method register_source_highlighter f = highlighter <- f::highlighter

  method original_source_viewer = original_source_viewer
  method display_globals globs = 
    let f buffer =
      Pretty_source.display_source
        globs
        buffer
        (lock,unlock,update_gui)
        ~highlighter:(fun localizable ~start ~stop ->
                        List.iter 
			  (fun f -> f buffer localizable ~start ~stop)
			  highlighter)
        ~selector:(fun ~button localizable ->
                     let popup_factory = new GMenu.factory (GMenu.menu ()) in
                     List.iter
                       (fun f ->
                          try
			    f popup_factory
                              self#toplevel ~button
			      localizable
			  with e ->
			    self#info
			      "Selector got exception %s; please report."
			      (Printexc.to_string e))
                       selector;
                     if button = 3 && popup_factory#menu#children <> [] then
                       popup_factory#menu#popup
                         ~button
                         ~time:(GtkMain.Main.get_current_event_time ()))
    in
    let buffer = 
      try 
        let buffer,locs = Globals_GUI.find globs in
        Pretty_source.Locs.locs := locs;
        self#rehighlight ();
        buffer
      with Not_found ->
        let buffer = Source_viewer.buffer () in
        f buffer;
        Globals_GUI.add globs (buffer,!Pretty_source.Locs.locs);
        buffer
    in
    buffer
  method rehighlight () = 
    Pretty_source.hilite ()

  method scroll loc = 
    match Pretty_source.locate_localizable loc with
    | None ->
        if Cmdline.Debug.get () > 2 then
          self#info "Could not scroll in GUI."
    | Some (b,_) ->
        self#source_viewer#buffer#place_cursor (self#source_viewer#buffer#get_iter (`OFFSET b));
        ignore (self#source_viewer#scroll_to_mark `INSERT)
    
(*  method highlight ~scroll tag loc =
    match Pretty_source.locate_localizable loc with
    | None ->
        if Cmdline.Debug.get () > 0 then
          self#info "Could not highlight in GUI."
    | Some (b,e) ->
        highlight_range ~scroll tag self#source_viewer b e
*)
  method view_original loc =
    if loc <> locUnknown then
      Source_manager.load_file
        self#monospace
        self#original_source_viewer
        (fst loc).Lexing.pos_fname
        (fst loc).Lexing.pos_lnum

  method view_original_stmt st =
    let loc = get_stmtLoc st.skind in
    if use_external_viewer
    then
      (if loc <> locUnknown then
         let args_for_emacs =
	   Format.sprintf "emacsclient -n +%d %s" 
             (fst loc).Lexing.pos_lnum (fst loc).Lexing.pos_fname
             (*          Format.sprintf "mate -a -l %d %s" line file  *)
         in
         if Cmdline.Debug.get () > 0 then
	   self#info "Running %s" args_for_emacs;
         ignore (Sys.command args_for_emacs))
    else self#view_original loc;
    loc

  method private info_string s = 
    status_context#pop ();
    ignore (status_context#push s)

  method info fmt =
    let b = Buffer.create 80 in
    let bfmt = Format.formatter_of_buffer b in
    Format.kfprintf
      (function fmt ->
	 Format.pp_print_flush fmt ();
	 let content = Buffer.contents b in
         self#info_string content)
      bfmt 
      fmt

  (*  flash_info := !display_info (* fun s -> status_context#flash ~delay:10 s *);*)

  (* These private method might be exported when necessary *)
  method private toplevel_vbox = toplevel_vbox
  method private toplevel_hpaned = toplevel_hpaned
  method private statusbar = statusbar
  method private menubar = menubar
  method private reparent parent =
    toplevel_vbox#misc#reparent parent;
    main_window#destroy ()
  method reset () =
    Globals_GUI.clear ();
    source_viewer#buffer#set_text "Please select a file in the left panel or start a new project.";
    file_tree <- self#file_tree#reset ();
    reset_extensions self#toplevel
  initializer
    (*    Format.eprintf "initializing gui...@.";*)
    main_window#misc#modify_font self#general;
    ignore (main_window#connect#destroy ~callback:(fun () -> exit 0));
    self#annot_window#misc#modify_font self#general;
    Gtk_helper.channel_redirector Unix.stdout (fun s -> self#annot_window#buffer#insert s;true);
    Gtk_helper.channel_redirector Unix.stderr (fun s -> self#annot_window#buffer#insert s;true);
    statusbar#misc#modify_font self#general;
    file_tree#add_select_function (filetree_selector self#toplevel);
    process_extensions self#toplevel;
    self#register_source_selector to_do_on_select;
    let () = GAction.add_actions actions
      [ 
        GAction.add_action "New" ~stock:`NEW ~tooltip:"Create a new project"
          ~accel:"<control>N"
	  ~callback:(fun _ -> new_project self#toplevel);
        GAction.add_action "Load" ~stock:`OPEN ~tooltip:"Load a saved state"
          ~accel:"<control>O"
	  ~callback:(fun _ -> load_file self#toplevel) ;
        GAction.add_action "Save" ~stock:`SAVE ~tooltip:"Save state"
          ~accel:"<control>S"
	  ~callback:(fun _ -> save_file self#toplevel) ;
        
        GAction.add_action "Configure" ~stock:`PREFERENCES ~tooltip:"Configure analyses"
	  ~callback:(fun _ -> run_configure_dialog self);
        GAction.add_action "Run" ~stock:`EXECUTE ~tooltip:"Run analyses"
          ~accel:"<control>R"
	  ~callback:(fun _ ->
                       (*let old = Project.current () in
			 let project = Project.create "interactive" in
			 Project.set_current project;
			 Project.copy ~src:old ~only:(Cmdline.get_selection ()) project;*)
		       annot_window#buffer#set_text "Running analysis\n";
		       self#lock ();
                       (try !Db.Toplevel.run_all_plugins 
			  (Gtk_helper.make_formatter annot_window#buffer)
                        with e -> 
                          Format.eprintf 
                            "Uncaught exception escaped from a plugin. Please report: %s@." 
                            (Printexc.to_string e));
		       self#unlock ();
                       self#reset ()) ;
        GAction.add_action "Quit" ~stock:`QUIT ~tooltip:"Quit" ~accel:"<control>Q"
	  ~callback:(fun _ -> exit 0);
      ]
    in
    ignore (ui_manager#add_ui_from_string basic_ui_info);

    main_window#show ();
    let warning_manager =
      Warning_manager.make
        ~packing:(fun w ->
                    ignore 
		      (lower_notebook#append_page
                         ~tab_label:(GMisc.label ~text:"Messages" ())#coerce w))
        ~callback:(fun s d ->
                     let locs = localizable_from_locs s d in
                     match locs with
                     | [] -> 
			 let loc = { Lexing.dummy_pos with 
			               Lexing.pos_lnum=d; Lexing.pos_fname=s;
			           } in
			 self#view_original (loc,loc)
                     | loc::_ ->
                         to_do_on_select
                           (new GMenu.factory (GMenu.menu ()))
                           ~button:1
                           self#toplevel
                           loc)
    in
    let display_warnings () =
      Warning_manager.clear warning_manager;
      Messages_manager.iter
	(fun _k mess ->
           Warning_manager.append
             warning_manager
             mess
             ~on_select:(fun _ -> assert false))
    in
    display_warnings ();
    register_reset_extension (fun _ -> display_warnings ());
    self#source_viewer#buffer#set_text "Please select a file in the left panel or start a new project.";

end

let toplevel_init () =
  Cilutil.flush_all ();
  (*print_endline ("BOOT: " ^ (Glib.Main.setlocale `ALL None));*)
  let (_:string) = GtkMain.Main.init ~setlocale:false () in
  (*print_endline ("START: " ^ (Glib.Main.setlocale `ALL None));*)
  if !Errormsg.hadErrors then 
    (GToolbox.message_box ~title:"Fatal error" 
      ~ok:"Close" 
      "Could not start Frama-C.\nRead the standard error and standard output messages to understand the problem.";
     exit 1)
  else ignore (new main_window ());
  GMain.Main.main ()

let () = Messages_manager.enable_collect ()
let () = 
  Db.progress:=(fun () -> while Glib.Main.iteration false do () done)

let () = Options.add_plugin
  ~name:"ValViewer"
  ~descr:"gui to display values"
  ~toplevel_init
  ["-monospace-font",
   Arg.String Cmdline.MonospaceFontName.set,
   Format.sprintf "f : use font f as monospace font (defaults to %s)"
     (Cmdline.MonospaceFontName.get ());
   "-general-font",
   Arg.String Cmdline.GeneralFontName.set,
   Format.sprintf "f : use font f as general font (defaults to %s)"
     (Cmdline.GeneralFontName.get ())
  ]

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
