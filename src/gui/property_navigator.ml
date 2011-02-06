(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Properties_status
open Design
open Cil_types
open Db_types

type property = {
  consolidated_tree : Consolidation_tree.t;
  module_name:string;
  function_name:string;
  kind:string;
  status_states: State.t list;
  status_name:string;
  status_bg:Gdk.color;
  status_icon:string;
  visible:bool;
  ip: Property.t;
}

module M = struct
  open Properties_status.Consolidation_tree
  exception No_more_valid_status
  exception Not_valid
  let rec relies_on_valid_hyps_only t =
    (* the consolidated status of [t] is valid iff at least one valid status of
       [t] relies on valid hypothesis only. *)
    let status =
      (* put the valid status first *)
      List.sort
	(fun s1 s2 -> match s1.value, s2.value with
	| (Checked { valid = True }, _), _ -> -1
	| _, (Checked { valid = True }, _) -> 1
	| _, _ -> 0)
	t.status
    in
    let is_valid_status s =
      (* return [true] iff each hypothesis of [s] is valid and relies itself
	 on valid hypothesis *)
      try
	List.iter
	  (fun h ->
	    match Properties_status.strongest h.property with
	    | Checked { valid = True }, _ ->
	      if not (relies_on_valid_hyps_only h) then raise Not_valid
	    | (Unknown | Checked { valid = False | Maybe }), _ ->
	      raise Not_valid)
	  s.hypothesis;
	(* [TODO JS 2010/11/09] introduce cluster handling here *)
	true
      with Not_valid ->
	false
    in
    (* The following assumes that valid status come first. *)
    try
      List.iter
	(fun s -> match s.value with
	| Checked { valid = True }, _ -> if is_valid_status s then raise Exit
	| (Unknown | Checked { valid = False | Maybe }), _ ->
	  raise No_more_valid_status)
	status;
      false
    with
    | No_more_valid_status -> false
    | Exit -> true
end
let relies_on_valid_hyps_only = M.relies_on_valid_hyps_only

let rec make_property forest ~ip ~status_states ~status_name
    ~module_name ~function_name =
  try
    let ctree =
      List.find
	(fun x -> Property.equal ip x.Consolidation_tree.property)
	forest
    in
    let function_name = match Property.get_kf ip with
      | None -> (* Blob properties have lost this information*) function_name
      | Some kf -> Pretty_utils.sfprintf "%a" Kernel_function.pretty_name kf
    in
    let status_states = List.map
      (fun s -> snd s.Consolidation_tree.value)
      ctree.Consolidation_tree.status
    in
    let kind = State.get_name ctree.Consolidation_tree.state in
    let status_bg= GDraw.color
      (`NAME (if relies_on_valid_hyps_only ctree then "green" else "orange"))
    in
    let status_icon =
      match Properties_status.strongest ip with
	| Checked { valid = True }, _ -> "gtk-yes"
	| Checked { valid = False }, _ -> "gtk-no"
	| Checked { valid = Maybe }, _ -> "gtk-dialog-question"
	| Unknown, _ -> "gtk-info"
    in
    { consolidated_tree = ctree;
      module_name = module_name;
      function_name = function_name;
      visible = true;
      ip=ip;
      kind=kind;
      status_states = status_states;
      status_name = status_name;
      status_bg=status_bg;
      status_icon=status_icon}
  with Not_found ->
    make_property [ Consolidation_tree.get ip ]
      ~ip ~status_states ~status_name
      ~module_name ~function_name

let graph_window main_window title states ip =
  if states <> [] then
    let state_dependency_graph ~packing =
      let f =
	Extlib.temp_file_cleanup_at_exit
	  "framac_property_status_navigator_graph" "dot"
      in
      Properties_status.Consolidation_tree.dump
	(Properties_status.Consolidation_tree.get_graph ip)
	f;
      snd (Dgraph.DGraphContainer.Dot.from_dot_with_commands ~packing f)
    in
    let height = int_of_float (float main_window#default_height *. 3. /. 4.) in
    let width = int_of_float (float main_window#default_width *. 3. /. 4.) in
    let window =
      GWindow.window
	~width ~height ~title ~allow_shrink:true ~allow_grow:true
	~position:`CENTER ()
    in
    let view = state_dependency_graph ~packing:window#add in
    window#show ();
    view#adapt_zoom ()

module Refreshers:
sig
  module Ensures: State_builder.Ref with type data = bool
  module RTE: State_builder.Ref with type data = bool
  module Preconditions: State_builder.Ref with type data = bool
  module Behaviors: State_builder.Ref with type data = bool
  module Assigns: State_builder.Ref with type data = bool
  module Assert: State_builder.Ref with type data = bool
  module Invariant: State_builder.Ref with type data = bool
  module Variant: State_builder.Ref with type data = bool
  module StmtSpec: State_builder.Ref with type data = bool
  module OnlyCurrent: State_builder.Ref with type data = bool

  val pack: GPack.box -> unit
  val apply: unit -> unit
end
=
struct
  (* Function to be called during the idle time of the GUI *)
  let refreshers = ref []
  let add_refresher f = refreshers := f::!refreshers

  module Add(X: sig val name: string end) = struct
    include State_builder.Ref
      (Datatype.Bool)
      (struct
	let name = "show " ^ X.name
	let dependencies = []
	let kind = `Internal
	let default () = true
       end)
    let add hb = add_refresher (Gtk_helper.on_bool hb X.name get set)
  end

  let apply () = List.iter (fun f -> f ()) !refreshers

  module Preconditions = Add(struct let name = "preconditions" end)
  module Ensures = Add(struct let name = "postconditions" end)
  module RTE = Add(struct let name = "RTE" end)
  module Behaviors = Add(struct let name = "behaviors" end)
  module Assigns = Add(struct let name = "assigns" end)
  module Assert = Add(struct let name = "assert" end)
  module Invariant = Add(struct let name = "invariant" end)
  module Variant = Add(struct let name = "variant" end)
  module StmtSpec = Add(struct let name = "stmt contract" end)
  module OnlyCurrent = Add(struct let name = "only selected" end)

  let pack hb =
    Preconditions.add hb;
    Ensures.add hb;
    RTE.add hb;
    Behaviors.add hb;
    Assigns.add hb;
    Assert.add hb;
    Invariant.add hb;
    Variant.add hb;
    StmtSpec.add hb;
    OnlyCurrent.add hb
end

open Refreshers

let make_panel (main_ui:main_window_extension_points) =
  let container = GPack.vbox ()
  in
  let module L = struct
    type t = property
    let column_list = new GTree.column_list
    let custom_value (_:Gobject.g_type) _t ~column:_ : Gobject.basic =
      assert false
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
  in
  let view = GTree.view
    ~rules_hint:true
    ~headers_visible:true
    ~packing:sc#add ()
  in
  ignore
    (view#connect#row_activated
       ~callback:(fun path _col ->
		    match model#custom_get_iter path with
		    | Some {MODEL.finfo={status_states=l; ip = ip;}} ->
			graph_window main_ui#main_window "Dependencies" l ip
		    | None -> ()));
  view#selection#set_select_function
    (fun path currently_selected ->
       if not currently_selected then
         begin match model#custom_get_iter path with
         | Some {MODEL.finfo={ip = ip;}} ->
             (match Property.get_kf ip with
             | Some kf ->
               main_ui#file_tree#select_global (Kernel_function.get_vi kf)
             (*TODO: select the Property.get_kinstr *)
             | None -> ())
         | None -> ()
         end;
       true);

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
  module_cview#set_resizable true;
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
  function_cview#set_resizable true;
  ignore (view#append_column function_cview);

  (* Kind name column viewer *)
  let kind_name_renderer = GTree.cell_renderer_text [top] in
  let m_kind_name_renderer renderer (lmodel:GTree.model) iter =
    let (path:Gtk.tree_path) = lmodel#get_path iter  in
    match model#custom_get_iter path with
    | Some {MODEL.finfo={kind=k}} ->
        renderer#set_properties [ `TEXT k ]
    | None -> ()
  in
  let kind_cview = GTree.view_column
    ~title:"Kind" ~renderer:(kind_name_renderer,[]) ()
  in
  kind_cview#set_cell_data_func
    kind_name_renderer (m_kind_name_renderer kind_name_renderer);
  kind_cview#set_resizable true;
  ignore (view#append_column kind_cview);

  (* Status colored column viewer *)
  let status_color_renderer = GTree.cell_renderer_pixbuf [top] in
  let m_status_color_renderer renderer (lmodel:GTree.model) iter =
    let (path:Gtk.tree_path) = lmodel#get_path iter  in
    match model#custom_get_iter path with
    | Some {MODEL.finfo={status_bg=color;status_icon=status_icon}} ->
        renderer#set_properties [`CELL_BACKGROUND_GDK color;
				 `STOCK_ID status_icon]
    | None -> ()
  in
  let status_color_cview = GTree.view_column
    ~title:"Status" ~renderer:(status_color_renderer,[]) ()
  in
  status_color_cview#set_cell_data_func
    status_color_renderer (m_status_color_renderer status_color_renderer);
  status_color_cview#set_resizable true;
  ignore (view#append_column status_color_cview);

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
    ~title:"Textual Status" ~renderer:(status_name_renderer,[]) ()
  in
  status_cview#set_cell_data_func
    status_name_renderer (m_status_name_renderer status_name_renderer);
  status_cview#set_resizable true;
  ignore (view#append_column status_cview);
  view#set_model (Some model#coerce);

  let hb = GPack.hbox  ~packing:container#pack () in
  Refreshers.pack hb;

  (* [VP 2011-01-29] seems like some ip do not have an associated option to
     let them be visible. *)
  let visible ip =
    match ip with
        Property.IPBlob _ -> false
      | Property.IPPredicate(Property.PKRequires _,_,Kglobal,_) -> 
        Preconditions.get ()
      | Property.IPPredicate(Property.PKRequires _,_,Kstmt _,_) ->
        Preconditions.get () && StmtSpec.get ()
      | Property.IPPredicate(Property.PKAssumes _,_,_,_) -> false
      | Property.IPPredicate(Property.PKEnsures _,_,Kglobal,_) -> Ensures.get ()
      | Property.IPPredicate(Property.PKEnsures _,_,Kstmt _,_) -> 
        Ensures.get() && StmtSpec.get()
      | Property.IPPredicate(Property.PKTerminates,_,_,_) -> false
      | Property.IPAxiom _ -> false
      | Property.IPComplete _ -> false
      | Property.IPDisjoint _ -> false
      | Property.IPCodeAnnot(_,_,{annot_content = AAssert _}) -> Assert.get ()
      | Property.IPCodeAnnot(_,_,{annot_content = AInvariant _}) -> 
        Invariant.get ()
      | Property.IPCodeAnnot(_,_,{annot_content = APragma _}) -> false
      | Property.IPCodeAnnot _ -> assert false
      | Property.IPBehavior (_,Kglobal,_) -> Behaviors.get ()
      | Property.IPBehavior (_,Kstmt _,_) -> Behaviors.get () && StmtSpec.get ()
      | Property.IPAssigns (_,Kglobal,_,_) -> Assigns.get ()
      | Property.IPAssigns (_,Kstmt _,Property.Id_code_annot _,_) -> 
        Assigns.get ()
      | Property.IPAssigns (_,Kstmt _,Property.Id_behavior _,_) -> 
        Assigns.get() && StmtSpec.get()
      | Property.IPFrom _ -> false
      | Property.IPDecrease _ -> Variant.get ()
  in
  let fill_model () =
    let status_string status (state: State.t option)=
      match status with
      | Unknown -> Pretty_utils.sfprintf "%a" Cil.d_annotation_status status
      | Checked _ ->
          assert (not (state = None));
          Pretty_utils.sfprintf "%a" Cil.d_annotation_status status
    in
    let get_states = function
      | None -> []
      | Some s -> [ s ]
    in
    let forest= Properties_status.Consolidation_tree.get_all () in
    let files = Globals.FileIndex.get_files () in
    (* We only display the name of the file *)
    let files = List.map
      (fun file->(Globals.FileIndex.get_functions file,Filename.basename file))
      files
    in
    List.iter
      (fun (kfs, file_base) ->
	let add_ip ip =
          let status, state = Properties_status.strongest ip in
          let function_name =
            (Extlib.may_map
               (fun f -> Kernel_function.get_name f ^ ": ") ~dft:"" 
               (Property.get_kf ip))
            ^ (Extlib.may_map 
                 (fun b -> "behavior " ^ b.b_name ^ ": ") ~dft:""
                 (Property.get_behavior ip))
          in
          if visible ip then
            append
	      (make_property
	         forest
	         ~module_name:file_base
	         ~function_name
	         ~status_states:(get_states state)
	         ~status_name:(status_string status state)
                 ~ip)
	in
	 List.iter
           (fun kf ->
              if Kernel_function.is_definition kf
                && (not (OnlyCurrent.get ()) ||
                      let kfvi = Kernel_function.get_vi kf in
                      List.exists
                        (fun g -> match g with
                         | GFun (f,_) -> Cil_datatype.Varinfo.equal f.svar kfvi
                         | _ -> false)
                        main_ui#file_tree#selected_globals)
              then begin
		let rte_get_all_status = !Db.RteGen.get_all_status in
	        let kf_name = Kernel_function.get_name kf in
	        List.iter
		  (fun (rte_status, status_states, rte_status_get) ->
                     if RTE.get () then
		       append
		         (make_property forest
			    ~module_name:file_base
			    ~function_name:kf_name
			    ~status_states:[ status_states kf ]
			    ~status_name:(if rte_status_get kf then "Generated"
				          else "not Generated")
                            ~ip:(Property.ip_blob rte_status)))
		  (rte_get_all_status ());
                let add_spec spec code_annotations =
                  let ip_spec = Property.ip_of_spec kf Kglobal spec in
                  let ip_annot = 
                    List.fold_right
                      (fun (stmt,loc_ca) acc -> 
                        let ca = 
                          match loc_ca with
                            | Before(User ca|AI(_,ca))
			    | After(User ca | AI(_,ca)) -> ca
                        in
                        Property.ip_of_code_annot kf stmt ca @ acc)
                      code_annotations []
                  in
                  List.iter add_ip ip_spec;
                  List.iter add_ip ip_annot;
                in
	        add_spec
                  (Kernel_function.get_spec kf)
                  (Kernel_function.code_annotations kf)
	      end)
	   kfs)
      (List.sort (fun (_, f1) (_, f2) -> String.compare f1 f2) files)
  in
  let refresh_button = GButton.button ~label:"Refresh" ~packing:hb#add () in
  ignore
    (let callback _ =
      main_ui#protect ~cancelable:false
        (fun () ->
	  clear ();
          Refreshers.apply ();
          fill_model ())
    in
    refresh_button#connect#released ~callback);
  (* To fill at startup:
     let (_:GtkSignal.id) = view#misc#connect#after#realize fill_model in *)
  let (_:int) = main_ui#lower_notebook#append_page
    ~tab_label:(GMisc.label ~text:"Properties" ())#coerce
    (container#coerce)
  in
  register_reset_extension (fun _ -> Refreshers.apply ())


(* Graphical markers in text showing the status of properties.
   Aka. "bullets" in left margin *)
let highlighter (main_ui:Design.main_window_extension_points) =
  let _pixbuf_from_stock stock =
    main_ui#source_viewer#misc#render_icon ~size:`MENU stock 
  in
  let make_marker name pixbuf =
    main_ui#source_viewer#set_mark_category_pixbuf ~category:name (Some pixbuf)
  in
  make_marker "true" (Gtk_helper.Icon.get Gtk_helper.Icon.Check);
  (* "true" used to be (pixbuf_from_stock `YES) *)
  make_marker "implied" (Gtk_helper.Icon.get 
                           Gtk_helper.Icon.Relies_on_valid_hyp);
  make_marker "false" (Gtk_helper.Icon.get Gtk_helper.Icon.Failed);
  make_marker "maybe" (Gtk_helper.Icon.get Gtk_helper.Icon.Maybe);
  make_marker "nottried" (Gtk_helper.Icon.get Gtk_helper.Icon.Attach);
  main_ui#source_viewer#set_show_line_marks true;
  let mark_with_status (buffer:GSourceView2.source_buffer) start ip =
    let status = Properties_status.strongest ip in
    match fst status with
    | Unknown ->
        ignore(buffer#create_source_mark ~category:"nottried"
		 (buffer#get_iter_at_char start))
    | Checked {valid = v} ->
        ignore(buffer#create_source_mark
		 ~category: (match v with 
                 | True -> if
                     relies_on_valid_hyps_only (Consolidation_tree.get ip)
                   then       
                     "true"
                   else
                     "implied"
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
  | Pretty_source.PIP (Property.IPPredicate (Property.PKAssumes _,_,_,_)) ->
      (* Assumes clause do not get a bullet*)
      ()
  | Pretty_source.PIP ip ->
    (*Format.printf "MARK again:%d (STRONGEST='%a' ALL='%a')@." start 
      Cil.d_annotation_status
      (fst (Properties_status.strongest ip))
      Properties_status.pretty_all ip;*)
    mark_with_status buffer start ip
  | Pretty_source.PGlobal _| Pretty_source.PVDecl _
  | Pretty_source.PTermLval _| Pretty_source.PLval _
  | Pretty_source.PStmt _ -> ()

let extend (main_ui:main_window_extension_points) =
  make_panel main_ui;
  main_ui#register_source_highlighter (highlighter main_ui)

let () = Design.register_extension extend

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
