(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

module LastSelected =
  Computation.OptionRef
    (Localizable_Datatype)
     (struct
       let name = "Design.LastSelected"
       let dependencies = [Ast.self]
     end)

let apply_on_selected = LastSelected.may

let all_files () =
  let f = GFile.filter ~name:"Source files" () in
  f#add_pattern "*.c" ;
  f#add_pattern "*.h" ;
  f#add_pattern "*.i" ;
  f#add_pattern "*.C" ;
  f#add_pattern "*.cpp" ;
  f#add_pattern "*.CPP" ;
  f

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
    <menu action='ViewMenu'>
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
      <menuitem action='Save as'/>
      <menuitem action='Quit'/>
    </menu>
    <menu action='AnalysesMenu'>
      <menuitem action='Analyze'/>
    </menu>
  </menubar>

  <toolbar name='ToolBar'>
      <toolitem action='New'/>
      <toolitem action='Analyze'/>
      <toolitem action='Quit'/>
  </toolbar>
</ui>"

class type main_window_extension_points = object
  inherit Launcher.basic_main
  method ui_manager : GAction.ui_manager
  method actions : GAction.action_group

  method file_tree : Filetree.t
  method file_tree_view : GTree.view
  method toplevel : main_window_extension_points
  method annot_window : GText.view
  method launcher : unit -> unit
  method source_viewer : GSourceView.source_view
  method display_globals : global list -> unit

  method register_source_selector :
    (GMenu.menu GMenu.factory -> main_window_extension_points -> button:int
       -> Pretty_source.localizable -> unit)
    -> unit

  method register_source_highlighter :
    (GSourceView.source_buffer -> localizable -> start:int -> stop:int -> unit)
    -> unit

  method register_panel :
    (main_window_extension_points ->
       (string * GObj.widget *(unit-> unit) option)) -> unit

  method rehighlight : unit -> unit
  method scroll : localizable -> unit

  method original_source_viewer : Source_manager.t
  method view_original_stmt : stmt -> location
  method view_original : location -> unit

  method reset : unit -> unit

  method error :
    'a. ?parent:GWindow.window_skel -> ('a, Format.formatter, unit) format
    -> 'a

  method push_info : 'a. ('a, Format.formatter, unit) format -> 'a
  method pop_info : unit -> unit
  method help_message : 'a 'b.
    (<event : GObj.event_ops ; .. > as 'a)
    -> ('b, Format.formatter, unit) format
    -> 'b
  method lower_notebook : GPack.notebook
end

(** Create a new project *)
let new_project (main_ui:main_window_extension_points) =
  let dialog = GWindow.dialog
    ~width:800
    ~height:400
    ~modal:true
    ~title:"Select source files"
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
      `VERTICAL ~packing:(hbox#pack ~expand:false ~fill:false) () in
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
      main_ui#protect ~parent:(dialog:>GWindow.window_skel)
	( fun () -> let files = List.map File.from_filename (get_all ()) in
	  let project = Project.create "interactive" in
          Project.on project File.init_from_c_files files;
          Project.set_current project;
	  main_ui#reset ())
  | `DELETE_EVENT | `CANCEL -> ()
  end;
  dialog#destroy ()
(*
(** Delete the current project *)
let delete_project (main_ui:main_window_extension_points) =
  match GToolbox.question_box ~title:"Remove current project"
    ~buttons:["Confirm"; "Cancel"]
    "Do you want to destroy the current project?"
  with
  | 1 ->
      (try
        Project.remove ()
      with Project.Cannot_remove _ ->
	(* doesn't work (see bts#209) *)
        let p = Project.create "default" in
        Project.on p File.init_from_cmdline ();
	Project.remove ()
      );
      main_ui#reset ()
  | _ -> ()
*)
(** Load a project file *)
let load_file (main_ui:main_window_extension_points) =
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Load state"
    ~parent:main_ui#main_window () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  main_ui#protect ~parent:(dialog:>GWindow.window_skel)
    (fun () -> match dialog#run () with
     | `OPEN ->
	 begin match dialog#filename with
	 | None -> ()
	 | Some f ->
             let f () =
               (try Project.load_all f
	        with Project.IOError s | Failure s ->
		  main_ui#error ~parent:(dialog:>GWindow.window_skel)
		    "Cannot load: %s" s);
	       main_ui#reset ()
             in
             f()
(*             ; ignore (Glib.Timeout.add ~ms:500
	       ~callback:(fun () ->
               f () ; true))*)
	 end
     | `DELETE_EVENT | `CANCEL -> ());
  dialog#destroy ()

let filename = ref ""

let save_in (main_ui:main_window_extension_points) parent name =
  try
    Project.save_all name;
    filename := name
  with Project.IOError s ->
    main_ui#error ~parent "Cannot save: %s" s

(** Save a project file. Choose a filename *)
let save_file_as (main_ui:main_window_extension_points) =
  let dialog = GWindow.file_chooser_dialog
    ~action:`SAVE
    ~title:"Save state"
    ~parent:main_ui#main_window () in
    (*dialog#set_do_overwrite_confirmation true ; only in later lablgtk2 *)
    dialog#add_button_stock `CANCEL `CANCEL ;
    dialog#add_select_button_stock `SAVE `SAVE ;
    main_ui#protect ~parent:(dialog :> GWindow.window_skel)
      (fun () ->
	 match dialog#run () with
	 | `SAVE ->
	     Extlib.may
	       (save_in main_ui (dialog :> GWindow.window_skel))
	       dialog#filename
	   | `DELETE_EVENT | `CANCEL -> ());
    dialog#destroy ()

let save_file (main_ui:main_window_extension_points) =
  if !filename = "" then save_file_as main_ui
  else save_in main_ui (main_ui#main_window :> GWindow.window_skel) !filename

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
module Globals_GUI = struct
  include Hashtbl.Make
    (struct
       type t = global list
       let equal = List.for_all2 (==)
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
    main_ui#display_globals globals;
    source#scroll_to_mark ~use_align:true ~xalign:1.0 ~yalign:0.5 `INSERT;
    let print_one_global (v,loc) =
      main_ui#protect
	(fun () ->
           main_ui#view_original loc;
           try
             let kf = Globals.Functions.get v in
             let text = Pretty_utils.sfprintf
               "Function '%a'\n" Kernel_function.pretty_name kf
             in
               main_ui#annot_window#buffer#insert ~iter:main_ui#annot_window#buffer#end_iter text
           with Not_found -> main_ui#error "No such function: please report\n")
    in
    begin match globals with
    | [] -> main_ui#annot_window#buffer#set_text "No globals in this file\n"
    | [GFun ({svar=v},loc)] ->
	main_ui#annot_window#buffer#set_text "";
	print_one_global (v,loc)
    | _ ->
	let first_global = ref None in
        main_ui#annot_window#buffer#set_text "";
	let (gfun,gtype,gcomp,genum,gvardecl,gvar) =
	  List.fold_right
	    (fun g (gfun,gtype,gcomp,genum,gvardecl,gvar) ->
	      match g with
		GFun ({svar=v},loc) ->
		  (match !first_global with
		  | None -> first_global:=Some (v,loc)
		  | Some _ -> ());
		  (g::gfun,gtype,gcomp,genum,gvardecl,gvar)
	      | GType _ -> (gfun,g::gtype,gcomp,genum,gvardecl,gvar)
	      | GCompTagDecl _ -> (gfun,gtype,g::gcomp,genum,gvardecl,gvar)
	      | GEnumTagDecl _ -> (gfun,gtype,gcomp,g::genum,gvardecl,gvar)
	      | GVarDecl _ -> (gfun,gtype,gcomp,genum,g::gvardecl,gvar)
	      | GVar _ -> (gfun,gtype,gcomp,genum,gvardecl,g::gvar)
	      | _ ->
		  (* PC does this happen? *)
		  (gfun,gtype,gcomp,genum,gvardecl,gvar))
	    globals
	    ([],[],[],[],[],[])
	in
	let buffer = main_ui#annot_window#buffer in
	let printing head f l =
	  if l <> []
	  then begin
	      buffer#insert ~iter:buffer#end_iter head;
	      List.iter
		(function g -> buffer#insert ~iter:buffer#end_iter (f g))
		l;
	      buffer#insert ~iter:buffer#end_iter "\n"
	    end;
	in
	printing
	  "Functions:"
	  (function GFun ({svar=v},_) ->
	    Pretty_utils.sfprintf " %a" Ast_info.pretty_vname v
	    | _ -> assert false)
	  gfun;
	printing
	  "Types:"
	  (function GType ({tname=name},_) ->
	    (Pretty_utils.sfprintf " %s" name)
	    | _ -> assert false)
	  gtype;
	printing
	  "Composite types:"
	  (function  GCompTagDecl ({cname=name},_) |GCompTag ({cname=name},_)->
	    (Pretty_utils.sfprintf " %s" name)
	    | _ -> assert false)
	  gcomp;
	printing
	  "Enums:"
	   (function GEnumTagDecl ({ename=name},_) | GEnumTag ({ename=name},_)->
	       (Pretty_utils.sfprintf " %s" name)
	     |_ -> assert false)
	  genum;
	printing
	  "Declared variables:"
	  (function GVarDecl (_,v,_) ->
	      (Pretty_utils.sfprintf " %a" Ast_info.pretty_vname v)
	    | _ -> assert false)
	  gvardecl;
	printing
	  "Variables:"
	  (function GVar(v,_,_) ->
	      (Pretty_utils.sfprintf " %a" Ast_info.pretty_vname v)
	    | _ -> assert false)
	  gvar;

(*	  Extlib.may print_one_global !first_global *)
    end
  end

let rec to_do_on_select
    (menu_factory:GMenu.menu GMenu.factory)
    (main_ui:main_window_extension_points)
    ~button
    selected
    =
  let current_statement_msg kf stmt =
    match stmt with
      Kglobal ->
	Format.sprintf
	  "Function: %s@\n@\n"
	  (match kf with
	     None -> "<none>"
	   | Some kf ->
	       Pretty_utils.sfprintf "%a" Kernel_function.pretty_name kf)
    | Kstmt ki ->
	let loc = main_ui#view_original_stmt ki in
	let skind =
	  if (Parameters.Debug.get ()> 0) then
            match ki with
            | {skind=Block _} -> "Block "
            | {skind=Instr (Skip _)} -> "Skip "
            | _ -> ""
	  else ""
	in
	Format.sprintf
	  "Function: %s@\n%sStatement: %d (line %d in %s)@\n"
	  (match kf with
	     None -> "<none>"
	   | Some kf ->
	       Pretty_utils.sfprintf "%a" Kernel_function.pretty_name kf)
	  skind
	  ki.sid
	  (fst loc).Lexing.pos_lnum
	  (fst loc).Lexing.pos_fname
  in
  LastSelected.set selected;
  let tree_view = main_ui#file_tree in
  let annot = main_ui#annot_window#buffer in
  if button = 1 then
    begin match selected with
    | PStmt (kf, ki) ->
	main_ui#protect
	  (fun () ->
	     let n = current_statement_msg (Some kf) (Kstmt ki) in
	     annot#set_text n;

             (* Code annotations for this statement *)
	     List.iter
	       (fun a ->
		  let pos,a = match a with
		  | Before a -> "Before", a
		  | After a -> "After", a
		  in
		  let user,s,status = match a with
		  | User a ->
		      "user",
		      Pretty_utils.sfprintf "%a\n" 
                        !Ast_printer.d_code_annotation a,
		      Pretty_utils.sfprintf "Status: %a" 
                        d_annotation_status 
                        (Db.Properties.Status.CodeAnnotation.get a)
		  | AI (_,a) ->
		      "alarm",
		      Pretty_utils.sfprintf "%a\n" 
                        !Ast_printer.d_code_annotation a,
		      Pretty_utils.sfprintf "Status: %a" 
                        d_annotation_status 
                        (Db.Properties.Status.CodeAnnotation.get a)
		  in
		  annot#insert ~iter:annot#end_iter
                    (Format.sprintf "%s(%s): %s%s" pos user s status))
	       (Annotations.get ki))
    | PCodeAnnot (kf,stmt,ca) ->
        let n = current_statement_msg (Some kf) (Kstmt stmt) in
	annot#set_text n;
        annot#insert ~iter:annot#end_iter
          (Pretty_utils.sfprintf "Code annotation id: %d\n" ca.annot_id);
        annot#insert ~iter:annot#end_iter
          (Pretty_utils.sfprintf "Status: %a@." 
            (Pretty_utils.pp_list ~sep:";" d_annotation_status) 
            (Db.Properties.Status.CodeAnnotation.get_all ca))
    | PBehavior (_,{b_name=n} as b) ->
        let msg = 
          Pretty_utils.sfprintf "Function behavior %s\nStatus: %a\n"
            n
            (Pretty_utils.pp_list ~sep:";" d_annotation_status) 
            (Db.Properties.Status.Behavior.get_all b)
        in
        annot#set_text msg

    | PPredicate (_,_,({ip_id=n} as p)) ->
        let msg = 
          Pretty_utils.sfprintf "Predicate %d\nStatus: %a\n"
            n
            (Pretty_utils.pp_list ~sep:";" d_annotation_status) 
            (Db.Properties.Status.Predicate.get_all p)
            
        in
        annot#set_text msg
          
    | PGlobal _g ->
	annot#set_text "This is a global\n";
    | PLval (kf, ki,lv) ->
        begin try
	  let n = current_statement_msg kf ki in

          let ty = typeOfLval lv in
          if isFunctionType ty
	  then
            annot#set_text (n ^ "This is a C function\n")
          else begin
	    annot#set_text n;
            let vars = extract_varinfos_from_lval lv in
            Cilutil.VarinfoSet.iter
              (fun vi ->
                 annot#insert
                   ~iter:annot#end_iter
                   (Format.sprintf
                      "Variable %s has type \"%s\".\nIt is a %s variable.\n%sIt is %sreferenced and its address is %staken.\n"
                      (Pretty_utils.sfprintf "%a" Ast_info.pretty_vname vi)
                      (Pretty_utils.sfprintf "%a" !Ast_printer.d_type vi.vtype)
                      (if vi.vglob then "global" else "local")
                      (match vi.vdescr with None -> ""
                       | Some s ->
                           Format.sprintf "This is a temporary variable for \"%s\".\n" s)
                      (if vi.vreferenced then "" else "not ")
                      (if vi.vaddrof then "" else "not ")))
              vars
          end
        with Not_found ->
          main_ui#error "Error in lval Db.KernelFunction.find"
        end
    | PTermLval _ ->
	annot#set_text "This is a logical left-value.\n"
    | PVDecl (kf,vi) ->
        main_ui#view_original vi.vdecl;
        if vi.vglob
        then
          annot#set_text
            (Format.sprintf "This is the declaration of global %s\nIt is %sreferenced and its address is %staken.\n"
               (Pretty_utils.sfprintf "%a" Ast_info.pretty_vname vi)
               (if vi.vreferenced then "" else "not ")
               (if vi.vaddrof then "" else "not "))
        else
          annot#set_text
            (Format.sprintf "This is the declaration of local %s in function %s\n%s"
               (Pretty_utils.sfprintf "%a" Ast_info.pretty_vname vi)
               (Pretty_utils.sfprintf "%a"
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
          | Some txt ->
	      main_ui#protect
		(fun () ->
		   Db.Properties.add_assert
		     kf ki ~before:false ("assert " ^ txt ^ ";");
		   to_do_on_select menu_factory main_ui ~button:1 selected)
        in
        let add_assert_before _ =
          let txt =
            GToolbox.input_string
              ~title:"Insert an assertion before"
              ""
          in
          match txt with
          | None -> ()
          | Some txt ->
	      main_ui#protect
                (fun () -> Db.Properties.add_assert
		   kf ki ~before:true ("assert " ^ txt ^ ";");
		   to_do_on_select menu_factory main_ui ~button:1 selected)
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
				(Pretty_utils.sfprintf "%a"
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
    | PTermLval _ | PCodeAnnot _ | PGlobal _ 
    | PBehavior _ | PPredicate _ -> ()
  end
(*
external sync_menu :  [>`menushell] Gobject.obj -> unit = "ml_ige_mac_menu_set_menu_bar"
external ige_mac_menu_set_global_key_handler_enabled : bool -> unit =
      "ml_ige_mac_menu_set_global_key_handler_enabled"
type app_menu_group
external ige_mac_menu_add_app_menu_group : unit -> app_menu_group =
    "ml_ige_mac_menu_add_app_menu_group"
external ige_mac_menu_add_app_menu_item : app_menu_group -> [>`menuitem] Gobject.obj -> unit =
    "ml_ige_mac_menu_add_app_menu_item"
*)

(** Global selectors and highlighters *)
let highlighter = ref []
let (selector: (GMenu.menu GMenu.factory ->
         main_window_extension_points ->
         button:int -> Pretty_source.localizable -> unit) list ref) = ref []

class type reactive_buffer = object
  inherit error_manager
  method buffer : GSourceView.source_buffer
  method locs : Pretty_source.Locs.state option
  method rehighlight : unit
end




class protected_menu_factory (host:Gtk_helper.host) (menu:GMenu.menu) = object
  inherit [GMenu.menu] GMenu.factory menu as super

  method add_item ?key ?callback ?submenu string =
    let callback = match callback with
      None -> None
    | Some cb -> Some (fun () -> ignore (host#full_protect cb))
    in
    super#add_item ?key ?callback ?submenu string

  method add_check_item ?active ?key ?callback string =
    let callback = match callback with
      None -> None
    | Some cb -> Some (fun b -> ignore (host#full_protect (fun () -> cb b)))
    in
    super#add_check_item ?active ?key ?callback string

end

class reactive_buffer_cl (main_ui:main_window_extension_points)
  ?(parent_window=main_ui#main_window)
  globs :reactive_buffer  =
object(self)
  inherit error_manager (parent_window:>GWindow.window_skel) as super
  val buffer = Source_viewer.buffer ()
  val mutable locs = None
  method buffer = buffer
  method locs = locs
  method rehighlight = Extlib.may Pretty_source.hilite locs
  method private init = locs <- Some(
    Pretty_source.display_source
      globs
      buffer
      ~host:(self:>Gtk_helper.host)
      ~highlighter:(fun localizable ~start ~stop ->
                      List.iter
			(fun f -> f buffer localizable ~start ~stop)
			!highlighter)
      ~selector:(fun ~button localizable ->
                   let popup_factory =
                     new protected_menu_factory (self:>Gtk_helper.host)
                       (GMenu.menu ())
                   in
                   List.iter
		     (fun f -> f popup_factory main_ui ~button localizable)
		     !selector;
                   if button = 3 && popup_factory#menu#children <> [] then
		     popup_factory#menu#popup
                       ~button
                       ~time:(GtkMain.Main.get_current_event_time ())));
  initializer
    self#init;
    Globals_GUI.add globs (self:> reactive_buffer)
end

let reactive_buffer main_ui ?parent_window globs  =
  try
    Globals_GUI.find globs
  with Not_found ->
    new reactive_buffer_cl main_ui ?parent_window globs


(** The main application window *)
class main_window () : main_window_extension_points =
  let width = (Gdk.Screen.width ())*7/8 in
  let height = (Gdk.Screen.height ())*7/8 in
  let max_width = (* maximum width for this height *)
    height * 8 / 5 (* 16/10 ratio *)
  in
  let width, height =
    if width > max_width
    then max_width, height
    else
      let max_height = width * 3 / 4 in
      let new_height = min height max_height in
      width, new_height
  in
  let main_window =
    GWindow.window
      ~title:"Frama-C"
      ~width
      ~height
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
        GAction.add_action "ViewMenu" ~label:"_View";
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
    (GtkMenu.MenuShell.cast (ui_manager#get_widget "ui/MenuBar")#as_widget)
  in
  (* Pack the menu bar on top of gui *)
  let () = toplevel_vbox#pack ~fill:false menubar#coerce
  in
  (* And the toolbar *)
  let toolbar = new GButton.toolbar
    (GtkButton.Toolbar.cast (ui_manager#get_widget "ui/ToolBar")#as_widget)
  in
  let stop_button = GButton.tool_button ~stock:`STOP ~packing:toolbar#insert ()
  in
  let _ = stop_button#connect#clicked
    (fun _ ->
       let old_progress = !Db.progress in
       Db.progress := (fun () ->
                         Db.progress := old_progress ;
                         stop_button#misc#set_sensitive false;
                         raise Db.Cancel))
  in
  let () = stop_button#misc#set_sensitive false
  in

(*
	GAction.add_action "Stop" ~stock:`STOP ~tooltip:"Stop analysis"
	  ~callback:(fun _ -> Db.progress := (fun () -> raise Db.Cancel));
*)

  (* Pack the menu bar on top of gui *)
  let () = toplevel_vbox#pack ~fill:false toolbar#coerce
  in

  (* Split below the bars *)
  let toplevel_hpaned = GPack.paned `HORIZONTAL ~packing:toplevel_vbox#add () in
  let filetree_panel_vpaned =
    GPack.paned `VERTICAL ~packing:(toplevel_hpaned#add1) ()
  in

  (* The left filetree inside an automatic scrolled window and a nice frame *)
  let filetree_frame =
    GBin.frame ~shadow_type:`ETCHED_OUT ~packing:filetree_panel_vpaned#add1 ()
  in
  let filetree_scrolled_window =
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:filetree_frame#add ()
  in
  let file_tree_view = GTree.view ~packing:filetree_scrolled_window#add () in
  let () = file_tree_view#selection#set_mode `NONE in
  let _ = main_window#misc#connect#after#show
    (fun _ ->
       file_tree_view#selection#set_mode `BROWSE)
  in
  let () = file_tree_view#set_rules_hint true in

  (* bottom hbox *)
  let bottom_hbox = GPack.box `HORIZONTAL ~packing:toplevel_vbox#pack ()
  in

  (* status bar (at bottom) *)
  let statusbar =
    GMisc.statusbar ~has_resize_grip:false ~packing:bottom_hbox#add ()
  in
  let status_context = statusbar#new_context "messages" in

  (* progress bar (at bottom) *)
  let progress_bar =
    GRange.progress_bar
      ~pulse_step:0.01
      ~packing:(bottom_hbox#pack
		  ~fill:false)
      ()
  in
  let lock_gui lock =
    (* lock left part of the GUI. *)
    filetree_panel_vpaned#misc#set_sensitive (not lock);
    (* lock all menus *)
    actions#set_sensitive (not lock);
    stop_button#misc#set_sensitive lock;
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
  let () = register_locking_machinery
    ~lock:(fun () -> lock_gui true) ~unlock:(fun () -> lock_gui false)
  in
  (* splits between messages and sources *)
  let vb_message_sources =
    GPack.paned `VERTICAL  ~border_width:3 ~packing:toplevel_hpaned#add2 ()
  in

  (* splits between messages and sources *)
  let hb_sources =
    GPack.paned `HORIZONTAL  ~border_width:3 ~packing:vb_message_sources#add1 ()
  in

  (* lower notebook *)
  let fr2 =
    GBin.frame ~shadow_type:`ETCHED_OUT ~packing:vb_message_sources#add2 ()
  in
  let lower_notebook =
    GPack.notebook ~scrollable:true ~show_tabs:true ~packing:fr2#add ()
  in

  (* lower text view and its scroll view: annotations and messages *)
  let _,annot_window = Gtk_helper.make_text_page lower_notebook "Information" in


  (* upper text view: source code *)
  let fr1 = GBin.frame ~shadow_type:`ETCHED_OUT ~packing:hb_sources#add1 () in
  let sw = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC
    ~hpolicy:`AUTOMATIC
    ~packing:fr1#add ()
  in

  let source_viewer = Source_viewer.make ~packing:sw#add in
  let () = source_viewer#set_show_line_numbers false in
  let original_source_viewer = Source_manager.make ~packing:hb_sources#add2 in

  (* Remove default pango menu for textviews *)
  let () =
    ignore (source_viewer#event#connect#button_press ~callback:
	      (fun ev -> GdkEvent.Button.button ev = 3));
    ignore (annot_window#event#connect#button_press ~callback:
	      (fun ev -> GdkEvent.Button.button ev = 3));
    (* startup configuration *)
    source_viewer#buffer#place_cursor
      ~where:source_viewer#buffer#start_iter in
  (* for syncing the Carbon menu if applicable: *)
  let () = ()
    (* let analyze_item =
       new GMenu.menu_item
       (GtkMenu.MenuItem.cast (ui_manager#get_widget "ui/MenuBar/AnalysesMenu")#as_widget)
       in
       let group = ige_mac_menu_add_app_menu_group () in
       ige_mac_menu_add_app_menu_item group (GtkMenu.MenuItem.cast analyze_item#as_widget) *)
  in
object (self:#main_window_extension_points)
  val mutable launcher = []
  val mutable panel = []
  val mutable main_window_metrics = {Gtk.width=0;height=0;x=0; y=0}

  method toplevel = (self:>main_window_extension_points)
  method main_window = main_window
  method ui_manager = ui_manager
  method actions = actions
  val mutable file_tree = None
  method file_tree = Extlib.the file_tree
  method file_tree_view = file_tree_view
  method annot_window = annot_window
  method source_viewer = source_viewer
  method register_source_selector f = selector := f::!selector
  method register_source_highlighter f = highlighter := f::!highlighter
  method register_panel f = panel <- f::panel

  method private initialize_panels () =
    let to_refresh = ref [] in
    let sw =
      GBin.scrolled_window
	~vpolicy:`AUTOMATIC
	~hpolicy:`AUTOMATIC
	~packing:filetree_panel_vpaned#add2
	()
    in
    let vbox = GPack.vbox ~packing:sw#add_with_viewport () in
    List.iter
      (fun f ->
	 let text,widget,refresh = f (self:>main_window_extension_points) in
	 let buffer = GText.buffer ~text () in
	 let tag =
	   buffer#create_tag
	     [ `BACKGROUND_GDK (vbox#misc#style#bg `PRELIGHT); `WEIGHT `BOLD ]
	 in
	 buffer#apply_tag tag ~start:buffer#start_iter ~stop:buffer#end_iter;
	 let expander = GBin.expander ~expanded:true ~packing:vbox#pack () in
	 expander#set_label_widget (GText.view ~buffer () :> GObj.widget);
	 let frame = GBin.frame ~packing:expander#add () in
	 frame#add widget;
	 Extlib.may
	   (fun refresh ->
	      to_refresh:=
		(fun ()-> if expander#expanded then refresh ())::!to_refresh)
	   refresh)
      panel;
    let refresh_all _ = (List.iter (fun f -> f ()) !to_refresh;true) in
    ignore (Glib.Timeout.add ~ms:500 ~callback:refresh_all)

  method launcher () =
    Launcher.show
      ~width:(main_window_metrics.Gtk.width/2)
      ~height:(2*main_window_metrics.Gtk.height/3)
      ~host:(self:>Launcher.basic_main)
      ()

  method original_source_viewer = original_source_viewer

  val mutable current_buffer_state : reactive_buffer option = None

  method display_globals globs =
    Parameters.debug "display_globals";
    let buff = reactive_buffer self#toplevel globs in
    current_buffer_state <- Some buff;
    self#source_viewer#set_buffer (buff#buffer:>GText.buffer)

  method rehighlight () =
    Extlib.may (fun f -> f#rehighlight) current_buffer_state;
    self#file_tree#model#foreach
      (fun p i -> self#file_tree#model#row_changed p i;false)

  method scroll loc =
    Extlib.may
      (fun state ->
         match Pretty_source.locate_localizable (Extlib.the state#locs) loc with
         | None ->
             if Parameters.Debug.get () > 2 then
               self#error "Could not scroll in GUI."
         | Some (b,_) ->
             self#source_viewer#buffer#place_cursor
               (self#source_viewer#buffer#get_iter (`OFFSET b));
             ignore (self#source_viewer#scroll_to_mark `INSERT))
      current_buffer_state

  method view_original loc =
    if loc <> Cilutil.locUnknown then
      Source_manager.load_file
        self#original_source_viewer
        (fst loc).Lexing.pos_fname
        (fst loc).Lexing.pos_lnum

  method view_original_stmt st =
    let loc = Cilutil.get_stmtLoc st.skind in
    if use_external_viewer
    then
      (if loc <> Cilutil.locUnknown then
         let args_for_emacs =
	   Format.sprintf "emacsclient -n +%d %s"
             (fst loc).Lexing.pos_lnum (fst loc).Lexing.pos_fname
             (*          Format.sprintf "mate -a -l %d %s" line file  *)
         in
         if Parameters.Debug.get () > 0 then
	   self#push_info "Running %s" args_for_emacs;
         ignore (Sys.command args_for_emacs);
         if Parameters.Debug.get () > 0 then
	   self#pop_info ();)
    else self#view_original loc;
    loc

  method private info_string s =
    ignore (status_context#push s)

  method pop_info ()  =
    status_context#pop ();

  method private push_info_buffer :
    'a. ?buffer:Buffer.t -> ('a, Format.formatter, unit) format -> 'a =
    fun ?buffer fmt ->
      let b = match buffer with
      | None -> Buffer.create 80
      | Some b -> b
      in
      let bfmt = Format.formatter_of_buffer b  in
      Format.kfprintf
	(function fmt ->
	   Format.pp_print_flush fmt ();
	   let content = Buffer.contents b in
           self#info_string content)
	bfmt
	fmt

  method push_info fmt = self#push_info_buffer fmt

  method help_message w fmt =
    let buffer = Buffer.create 80 in
    let bfmt = Format.formatter_of_buffer buffer  in
    Format.kfprintf
      (function _ ->
         ignore (w#event#connect#leave_notify
                   (fun _ -> self#pop_info ();true));
         ignore (w#event#connect#enter_notify
                   (fun _ ->
		      Format.pp_print_flush bfmt ();
		      self#push_info_buffer ~buffer "" ;false)))
      bfmt
      fmt

  inherit error_manager (main_window:>GWindow.window_skel)

  (*  flash_info := !display_info (* fun s -> status_context#flash ~delay:10 s *);*)

  (* These private method might be exported when necessary *)
  method private toplevel_vbox = toplevel_vbox
  method private toplevel_hpaned = toplevel_hpaned
  method private statusbar = statusbar
  method private menubar = menubar
  method private reparent parent =
    toplevel_vbox#misc#reparent parent;
    main_window#destroy ()
  method lower_notebook = lower_notebook
  method reset () =
    Globals_GUI.clear ();
    source_viewer#buffer#set_text "Please select a file in the left panel\nor start a new project.";
    self#file_tree#reset ();
    reset_extensions self#toplevel

  initializer
  let init_all () =
    ignore (main_window#connect#destroy
	      ~callback:(fun () -> exit 0));
    (* Set the relative position for all paned whenever the main window is
       resized *)
    ignore (main_window#misc#connect#size_allocate
	      (fun ({Gtk.width=w;Gtk.height=h} as rect) ->
	         if main_window_metrics.Gtk.width <> w
                   || main_window_metrics.Gtk.height <> h then
		     begin
                       place_paned hb_sources 0.5;
                       place_paned vb_message_sources 0.71;
                       place_paned filetree_panel_vpaned 0.5;
                       place_paned toplevel_hpaned 0.18
		     end;
                 main_window_metrics <- rect));

    file_tree <- Some (Filetree.make file_tree_view);

    self#file_tree#add_select_function (filetree_selector self#toplevel);

    process_extensions self#toplevel;

    self#register_source_selector to_do_on_select;
    let () = GAction.add_actions actions
      [
	GAction.add_action "New" ~stock:`NEW ~tooltip:"Create a new project"
          ~accel:"<control>N"
	  ~callback:(fun _ -> new_project self#toplevel);
(*	GAction.add_action "Delete" ~stock:`DELETE
	  ~tooltip:"Delete the current project" ~accel:"<control>D"
	  ~callback:(fun _ -> delete_project self#toplevel);*)
	GAction.add_action "Load" ~stock:`OPEN ~tooltip:"Load a saved state"
          ~accel:"<control>L"
	  ~callback:(fun _ -> load_file self#toplevel) ;
	GAction.add_action "Save" ~stock:`SAVE ~tooltip:"Save state"
          ~accel:"<control>S"
	  ~callback:(fun _ -> save_file self#toplevel) ;
	GAction.add_action "Save as" ~stock:`SAVE_AS ~tooltip:"Save state as"
	  ~callback:(fun _ -> save_file_as self#toplevel) ;

	GAction.add_action "Analyze" ~stock:`EXECUTE
	  ~tooltip:"Configure and run analyses"
	  ~callback:(fun _ -> self#launcher ());

	GAction.add_action "Quit" ~stock:`QUIT ~tooltip:"Quit"
	  ~accel:"<control>Q"
	  ~callback:(fun _ -> exit 0);
      ]
    in
    ignore (ui_manager#add_ui_from_string basic_ui_info);
    self#initialize_panels ();
    main_window#show ();
    Gdk.Window.set_cursor main_window#misc#window arrow_cursor;

    let warning_manager =
      Warning_manager.make
	~packing:(fun w ->
                    ignore
		      (lower_notebook#append_page
			 ~tab_label:(GMisc.label ~text:"Messages" ())#coerce w))
	~callback:(fun s d ->
                     Extlib.may
                       ( fun state ->
                           let locs = localizable_from_locs (Extlib.the state#locs) s d in
		           match locs with
		           | [] ->
			       let loc = { Lexing.dummy_pos with
				             Lexing.pos_lnum=d; Lexing.pos_fname=s;
				         } in
			       self#view_original (loc,loc)
		           | loc::_ ->
			       to_do_on_select
			         (new protected_menu_factory
                                    (self:>Gtk_helper.host)
                                    (GMenu.menu ()))
			         ~button:1
			         self#toplevel
			         loc)
                       current_buffer_state)
    in
    let display_warnings () =
      Warning_manager.clear warning_manager;
      Messages.iter
	(fun _ event ->
           Warning_manager.append warning_manager event
             ~on_select:(fun _ -> assert false))
    in
    display_warnings ();
    register_reset_extension (fun _ -> display_warnings ());
    self#source_viewer#buffer#set_text
      "Please select a file in the left panel\nor start a new project."
  in
  init_all ()
end

let make_splash () =
  GMain.Rc.add_default_file (Config.datadir ^"/frama-c.rc");
  GMain.Rc.add_default_file (Config.datadir ^"/frama-c-user.rc");
  (*print_endline ("BOOT: " ^ (Glib.Main.setlocale `ALL None));*)
  let (_:string) = GtkMain.Main.init ~setlocale:false () in
  (*print_endline ("START: " ^ (Glib.Main.setlocale `ALL None));*)

  let w =
    GWindow.window
      ~title:"Splash" ~width:640 ~height:480 ~position:`CENTER_ALWAYS
      ~show:false ()
  in
  let _ = w#event#connect#delete ~callback:(fun _ -> exit 0) in
  let tid =
    Glib.Timeout.add ~ms:500 ~callback:(fun () -> w#present (); false)
  in
  let bx = GPack.vbox ~packing:w#add () in
  let notebook = GPack.notebook ~packing:bx#add () in
  let close_button =
    GButton.button
      ~packing:(bx#pack ~expand:false ~fill:false) ~stock:`CANCEL ()
  in
  ignore (close_button#connect#released (fun () -> exit 0));
  let reparent,stdout = Gtk_helper.make_text_page notebook "Console" in
  if Parameters.Debug.get () = 0 then begin
    Gtk_helper.log_redirector (fun s -> stdout#buffer#insert ~iter:stdout#buffer#end_iter s);

    let it = make_tag stdout#buffer "italic" [`STYLE `OBLIQUE] in
    let tag_stack = Stack.create () in
    let open_tag s =
      (match s with
      | "i" ->
          Stack.push (it,(stdout#buffer#get_iter `INSERT)#offset) tag_stack;
      | _ -> ());
      ""
    in
    let close_tag s =
      (match s with
       | "i" ->
           let stop = stdout#buffer#end_iter in
           let tag,start_offset = Stack.pop tag_stack in
           let start = stdout#buffer#get_iter (`OFFSET start_offset) in
           (* Format.printf "start:%d stop:%d@." start#offset stop#offset; *)
           stdout#buffer#apply_tag tag ~start ~stop
       | _ -> ());
      ""
    in
    Kernel.register_tag_handlers (open_tag,close_tag)
  end;
  tid,stdout,w,reparent

let toplevel play =
  Db.progress := Gtk_helper.refresh_gui;
  let in_idle () =
    let tid,_splash_out,splash_w,reparent_console = make_splash () in
    let error_manager = new Gtk_helper.error_manager (splash_w:>GWindow.window_skel) in
    error_manager#protect
      (fun () ->
         Messages.enable_collect ();
         play ();
         Ast.compute ();
         let main_ui =  new main_window () in
         Glib.Timeout.remove tid;
         reparent_console main_ui#lower_notebook;
         splash_w#destroy ())
  in
  ignore (Glib.Idle.add (fun () -> in_idle (); false));
  GMain.Main.main ()

let () = Db.Toplevel.run := toplevel

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
