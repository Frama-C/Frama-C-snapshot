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

(* This is the panel to control the status of properties. *)
open Design
open Cil_types
open Db_types

type property = {module_name:string;
                 function_name:string;
                 kind:Project.Computation.t;
		 status_states: Project.Computation.t list;
                 status_name:string;
                 visible:bool;}

let graph_window main_window title states =
  if states <> [] then
    let state_dependency_graph ~packing =
      let dot_file =
	Extlib.temp_file_cleanup_at_exit
	  "framac_partial_state_dependency_graph" "dot"
      in
      let only =
	List.fold_left
	  (fun acc s ->
	     Project.Selection.add s Kind.Select_Dependencies acc)
	  Project.Selection.empty
	  states
      in
      Project.Computation.dump_dynamic_dependencies ~only dot_file;
      let model = Dgraph.DGraphModel.read_dot dot_file in
      let view = Dgraph.DGraphView.view ~aa:true ~packing model in
      view#connect_highlighting_event ();
      view
    in
    let height = int_of_float (float main_window#default_height *. 3. /. 4.) in
    let width = int_of_float (float main_window#default_width *. 3. /. 4.) in
    let window =
      GWindow.window
	~width ~height ~title ~allow_shrink:true ~allow_grow:true
	~position:`CENTER ()
    in
    let scroll =
      GBin.scrolled_window
	~packing:window#add ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ()
    in
    let view = state_dependency_graph ~packing:scroll#add in
    ignore (view#set_center_scroll_region true);
    window#show ();
    view#adapt_zoom ()

let make_panel
    (main_ui:main_window_extension_points)
    :(string*GObj.widget*(unit-> unit) option) =
  let container = GPack.vbox ()
  in
  let module L = struct
    type t = property
    let column_list = new GTree.column_list
    let module_name_col = column_list#add Gobject.Data.string
    let function_name_col = column_list#add Gobject.Data.string
    let kind_name_col = column_list#add Gobject.Data.string
    let status_name_col = column_list#add Gobject.Data.string
    let custom_value (_:Gobject.g_type) t ~column : Gobject.basic =
      match column with
	| 0 -> (* module_name *)   `STRING (Some t.module_name)
	| 1 -> (* function_name *)  `STRING (Some t.function_name)
	| 2 -> (* kind *) `STRING (Some (Project.Computation.name t.kind))
	| 3 -> (* status *) `STRING (Some t.status_name)
	| _ -> assert false
  end
  in
  let module MODEL =  Gtk_helper.MAKE_CUSTOM_LIST(L) in
  let model = MODEL.custom_list () in
  let append m = if m.visible then model#insert m in
  let clear () = model#clear () in
  let sc =
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:(container#pack ~expand:true ~fill:true)
      ()
  in let view = GTree.view
    ~rules_hint:true
    ~headers_visible:true
    ~packing:sc#add ()
  in
  ignore
    (view#connect#row_activated
       ~callback:(fun path _col ->
		    match model#custom_get_iter path with
		      | Some {MODEL.finfo={status_states=l}} ->
			  graph_window main_ui#main_window "Dependencies" l
		      | None -> ()));
  let top = `YALIGN 0.0 in

  (* Module name column viewer *)
  let module_name_renderer = GTree.cell_renderer_text [top] in
  let m_module_name_renderer renderer (lmodel:GTree.model) iter =
    let (path:Gtk.tree_path) = lmodel#get_path iter  in
    match model#custom_get_iter path with
      | Some {MODEL.finfo={module_name=m}} ->
          renderer#set_properties [`TEXT m]
      | None -> ()
  in
  let module_cview = GTree.view_column
    ~title:"Module" ~renderer:(module_name_renderer,[]) ()
  in
  module_cview#set_cell_data_func
    module_name_renderer (m_module_name_renderer module_name_renderer);
  ignore (view#append_column module_cview);

  (* Function name column viewer *)
  let function_name_renderer = GTree.cell_renderer_text [top] in
  let m_function_name_renderer renderer (lmodel:GTree.model) iter =
    let (path:Gtk.tree_path) = lmodel#get_path iter  in
    match model#custom_get_iter path with
      | Some {MODEL.finfo={function_name=m}} ->
          renderer#set_properties [`TEXT m]
      | None -> ()
  in
  let function_cview = GTree.view_column
    ~title:"Function" ~renderer:(function_name_renderer,[]) ()
  in
  function_cview#set_cell_data_func
    function_name_renderer (m_function_name_renderer function_name_renderer);
  ignore (view#append_column function_cview);

  (* Kind name column viewer *)
  let kind_name_renderer = GTree.cell_renderer_text [top] in
  let m_kind_name_renderer renderer (lmodel:GTree.model) iter =
    let (path:Gtk.tree_path) = lmodel#get_path iter  in
    match model#custom_get_iter path with
      | Some {MODEL.finfo={kind=k}} ->
          renderer#set_properties [`TEXT (Project.Computation.name k)]
      | None -> ()
  in
  let kind_cview = GTree.view_column
    ~title:"Kind" ~renderer:(kind_name_renderer,[]) ()
  in
  kind_cview#set_cell_data_func
    kind_name_renderer (m_kind_name_renderer kind_name_renderer);
  ignore (view#append_column kind_cview);

  (* Status name column viewer *)
  let status_name_renderer = GTree.cell_renderer_text [top] in
  let m_status_name_renderer renderer (lmodel:GTree.model) iter =
    let (path:Gtk.tree_path) = lmodel#get_path iter  in
    match model#custom_get_iter path with
      | Some {MODEL.finfo={status_name=m}} ->
          renderer#set_properties [`TEXT m]
      | None -> ()
  in
  let status_cview = GTree.view_column
    ~title:"Status" ~renderer:(status_name_renderer,[]) ()
  in
  status_cview#set_cell_data_func
    status_name_renderer (m_status_name_renderer status_name_renderer);
  ignore (view#append_column status_cview);

  view#set_model (Some model#coerce);

  let hb = GPack.hbox  ~packing:container#pack () in

  (* Function to be called during the idle time of the GUI *)
  let refreshers = ref [] in
  let add_refresher f = refreshers := f::!refreshers in
  let module Add_Refresher(X: sig val name: string end) = struct
    include Computation.Ref
      (struct include Datatype.Bool let default () = true end)
      (struct let name = "show " ^ X.name let dependencies = [] end)
    let () = add_refresher (Gtk_helper.on_bool hb X.name get set)
  end
  in
  let module Ensures = Add_Refresher(struct let name = "ensures" end) in
  let module RTE = Add_Refresher(struct let name = "RTE" end) in
  let module Preconditions =
    Add_Refresher(struct let name = "preconditions" end)
  in
  let module Behaviors = Add_Refresher(struct let name = "behaviors" end) in
  let module Assigns = Add_Refresher(struct let name = "assigns" end) in
  let module Assert = Add_Refresher(struct let name = "assert" end) in

  let status_string status state =
    match status with
      | Unknown -> Pretty_utils.sfprintf "%a" Cil.d_annotation_status status
      | Checked _ ->
	  Pretty_utils.sfprintf "%a" Cil.d_annotation_status
	    status ^ " (" ^ Project.Computation.name state ^ ")"
  in
  let fill_model () =
    let files = Globals.FileIndex.get_files () in
    List.iter
      (fun file ->
	 let kfs = Globals.FileIndex.get_functions file in
         let file_base = Filename.basename file in
	 List.iter
           (fun kf -> if Kernel_function.is_definition kf then begin
	      let kf_name = Kernel_function.get_name kf in
	      let spec = Kernel_function.get_spec kf in
	      List.iter
		(fun (rte_status, status_states, rte_status_get) ->

		   append {module_name=file_base;
			   function_name= kf_name;
			   kind=rte_status; (* "Runtime Errors"; *)
			   status_states = [ status_states kf ];
			   status_name =
		       if rte_status_get kf then "Generated"
		       else "not Generated";
			   visible= RTE.get ()} )
		(Properties_status.get_all_status ()) (* rte_status_name_get *) ;
	      (*
		append {module_name=file_base;
		function_name= kf_name;
		kind_name="Preconditions";
		status =
                if Properties_status.Called_Precond_Generated.get kf then
		"Generated"
                else "not Generated";
		visible= Preconditions.get ()};
	      *)
	      List.iter
                (fun behavior ->
                   let function_name = kf_name^": behavior "^behavior.b_name in
		   let behavior_id = kf,Kglobal,behavior in
                   let status, state =
                     Properties_status.Behavior.strongest behavior_id
                   in
                   append {module_name=file_base;
			   function_name= function_name;
			   kind=Properties_status.Behavior.self;
			   status_states=
		       Properties_status.Behavior.get_all_states behavior_id;
			   status_name=status_string status state;
                           visible=Behaviors.get ()};
		   if behavior.b_assigns <> [] then begin
		     let assigns_id =
		       kf,Kglobal, Some behavior,behavior.b_assigns
		     in
                     let status, state =
		       Properties_status.Assigns.strongest assigns_id
                     in
                     append {module_name=file_base;
			     function_name= function_name;
		             kind=Properties_status.Assigns.self;
			     status_states=
			 Properties_status.Assigns.get_all_states assigns_id;
		             status_name= status_string status state;
                             visible=Assigns.get ()};
		   end;
                   List.iter
                     (fun (_,post) ->
                        let status, state =
                          Properties_status.Predicate.strongest post
                        in
                        append {module_name=file_base;
			        function_name= function_name;
			        kind=Properties_status.Predicate.self;
			        status_name=status_string status state;
				status_states=
			    Properties_status.Predicate.get_all_states post;
                                visible=Ensures.get ()})
                     behavior.b_post_cond;)
                spec.spec_behavior;
              List.iter
                (fun (_stmt,(Before(User ca|AI(_,ca))|After(User ca|AI(_,ca)))) ->
                   match ca.annot_content with
                     | AAssert (_labels,_predicate,_) ->
                         let status, state =
			   Properties_status.CodeAnnotation.strongest ca
                         in
                         append {module_name=file_base;
			         function_name= kf_name;
			         kind=Properties_status.CodeAnnotation.self;
			         status_name=status_string status state;
				 status_states=
			     Properties_status.CodeAnnotation.get_all_states ca;
                                 visible=Assert.get ()}
                     |APragma _|AAssigns (_, _)|AVariant _
                     |AInvariant (_, _, _)|AStmtSpec _
                                    -> ())
                (Kernel_function.code_annotations kf)
	    end)
	   kfs)
      (List.sort Pervasives.compare files)
  in
  let refresh_button = GButton.button ~label:"Refresh" ~packing:hb#add () in
  let (_:GtkSignal.id) = refresh_button#connect#released
    ~callback:(fun _ -> clear (); fill_model ())
  in
  (* To fill at startup:
     let (_:GtkSignal.id) = view#misc#connect#after#realize fill_model in *)
  "Properties (I'm not there)",container#coerce,
  Some (fun () -> List.iter (fun f -> f()) !refreshers)


(* Graphical markers in text showing the status of properties. *)
let highlighter (main_ui:Design.main_window_extension_points) =
  let make_marker name stock =
    let pixbuf =
      main_ui#source_viewer#misc#render_icon ~size:`MENU stock
    in
    main_ui#source_viewer#set_mark_category_pixbuf ~category:name
      (Some pixbuf)
  in
  make_marker "true" `YES;
  make_marker "false" `NO;
  make_marker "maybe" `DIALOG_QUESTION;
  make_marker "nottried" `INFO;
  main_ui#source_viewer#set_show_line_marks true;
  let mark_with_status (buffer:GSourceView2.source_buffer) start status =
    match fst status with
    | Unknown ->
        ignore(buffer#create_source_mark ~category:"nottried"
		 (buffer#get_iter_at_char start))
    | Checked {valid = v} ->
        ignore(buffer#create_source_mark
		 ~category: (match v with | True -> "true"
			     | False -> "false"
			     | Maybe -> "maybe")
		 (buffer#get_iter_at_char start))
  in
  fun (buffer:GSourceView2.source_buffer) localizable ~start ~stop:_ ->
    buffer#remove_source_marks
      (buffer#get_iter_at_char start)
      (buffer#get_iter_at_char start)
      () ;
  match localizable with
  | Pretty_source.PCodeAnnot (_kf,_stmt,annot) ->
      mark_with_status buffer
        start
        (Properties_status.CodeAnnotation.strongest annot)
  | Pretty_source.PBehavior kb ->
      mark_with_status buffer
	start
	(Properties_status.Behavior.strongest kb)
  | Pretty_source.PPredicate (_,_,p)
  | Pretty_source.PPost_cond (_,_,_,(_,p))
      (*    | Pretty_source.PAssumes (_,_,_,p) *)
  | Pretty_source.PRequires (_,_,_,p)
  | Pretty_source.PTerminates (_,_,p) ->
      mark_with_status buffer
        start
        (Properties_status.Predicate.strongest p)
  | Pretty_source.PAssigns a ->
      mark_with_status buffer
        start
        (Properties_status.Assigns.strongest a)
  | Pretty_source.PDisjoint_behaviors b ->
      mark_with_status buffer
        start
        (Properties_status.Disjoint.strongest b)
  | Pretty_source.PComplete_behaviors b ->
      mark_with_status buffer
        start
        (Properties_status.Complete.strongest b)
  | Pretty_source.PVariant _
  | Pretty_source.PGlobal _| Pretty_source.PVDecl _
  | Pretty_source.PTermLval _| Pretty_source.PLval _
  | Pretty_source.PStmt _
  | Pretty_source.PAssumes _ -> ()

let extend (main_ui:main_window_extension_points) =
  main_ui#register_panel make_panel;
  main_ui#register_source_highlighter (highlighter main_ui)

let () = Design.register_extension extend

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
