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

(* This is the panel to control the status of properties. 
   [JS 2011/07/28] TODO: move it in the plug-in `report' *)

open Design
open Cil_types
open Property_status

type property = {
  module_name:string;
  function_name:string;
  kind:string;
  status_name:string;
  consolidated_status_name:string;
  status_icon:Gtk_helper.Icon.kind;
  visible:bool;
  ip: Property.t;
}

let rec make_property ~ip ~status_name ~module_name ~function_name =
  try
    let status = Consolidation.get ip in
    let function_name = match Property.get_kf ip with
      | None -> 
	(* [JS 2011/07/28] TODO: is it still possible?
	   some properties may have lost this information*) 
	function_name
      | Some kf -> Pretty_utils.sfprintf "%a" Kernel_function.pretty kf
    in
    let kind = 
      Pretty_utils.sfprintf "@[<hov>%a@]" Property.pretty ip 
    in
    let consolidated_status_name = 
      Pretty_utils.sfprintf "%a" Consolidation.pretty status
    in
    let status_icon = Gtk_helper.Icon.Feedback (Feedback.get ip) in
    { 
      module_name = module_name;
      function_name = function_name;
      visible = true;
      ip=ip; kind=kind;
      status_name = status_name ;
      consolidated_status_name = consolidated_status_name ;
      status_icon = status_icon ;
    }
  with Not_found ->
    make_property ~ip ~status_name ~module_name ~function_name

let graph_window main_window title ip = 
  let state_dependency_graph ~packing =
    let f =
      try
        Extlib.temp_file_cleanup_at_exit
          "framac_property_status_navigator_graph" "dot"
      with Extlib.Temp_file_error s ->
        Gui_parameters.abort "cannot create temporary file: %s" s
    in
    Property_status.Consolidation_graph.dump
      (Property_status.Consolidation_graph.get ip)
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

module Refreshers: sig
  module OnlyCurrent: State_builder.Ref with type data = bool

  module Ensures: State_builder.Ref with type data = bool
  module Preconditions: State_builder.Ref with type data = bool
  module Behaviors: State_builder.Ref with type data = bool
  module Assigns: State_builder.Ref with type data = bool
  module From: State_builder.Ref with type data = bool
  module Assert: State_builder.Ref with type data = bool
  module Invariant: State_builder.Ref with type data = bool
  module Variant: State_builder.Ref with type data = bool
  module Terminates: State_builder.Ref with type data = bool
  module StmtSpec: State_builder.Ref with type data = bool
  module Unreachable: State_builder.Ref with type data = bool
  module Other: State_builder.Ref with type data = bool
  module Axiomatic: State_builder.Ref with type data = bool
(*module Pragma: State_builder.Ref with type data = bool*)
  module RteNotGenerated: State_builder.Ref with type data = bool
  module RteGenerated: State_builder.Ref with type data = bool

  val pack: GPack.box -> unit
  val apply: unit -> unit
end
=
struct
  (* Function to be called during the idle time of the GUI *)
  let refreshers = ref []
  let add_refresher f = refreshers := f::!refreshers

  module Add (X: sig val name: string val hint: string end) =
  struct
    open Gtk_helper
    let key_name =
      Configuration.load ();
      let s = String.copy X.name in
      for i = 0 to String.length s - 1 do
        let c = s.[i] in
        if c < 'A' || c > 'z' || (c > 'Z' && c < 'a') then
          s.[i] <- '_'
      done;
      "property_panel." ^ s

    include State_builder.Ref
      (Datatype.Bool)
      (struct
        let name = "show " ^ X.name
        let dependencies = []
        let kind = `Internal
        let default () =
          let v = Configuration.find_bool ~default:true key_name in
          v
       end)
    let set v = Configuration.set key_name (Configuration.ConfBool v);
      set v
    let add hb = add_refresher
      (Gtk_helper.on_bool ~tooltip:X.hint hb X.name get set)
  end

  let apply () = List.iter (fun f -> f ()) !refreshers

  module OnlyCurrent = Add(
    struct let name = "Current function"
           let hint = "Restrict properties to those of current function" end)
  module Preconditions = Add(
    struct let name = "Preconditions"
           let hint = "Show functions preconditions" end)
  module Ensures = Add(
    struct let name = "Postconditions"
           let hint = "Show functions postconditions" end)
  module Behaviors = Add(
    struct let name = "Behaviors"
           let hint = "Show functions behaviors" end)
  module Assigns = Add(
    struct let name = "Assigns"
           let hint = "Show functions assigns" end)
  module From = Add(
    struct let name = "From"
           let hint = "Show functional dependencies in functions assigns" end)
  module Assert = Add(
    struct let name = "Assert"
           let hint = "Show assertions" end)
  module Invariant = Add(
    struct let name = "Invariant"
           let hint = "Show loop invariants" end)
  module Variant = Add(
    struct let name = "Variant"
           let hint = "Show loop termination argument" end)
  module Terminates = Add(
    struct let name = "Terminates"
           let hint = "Show functions termination clauses" end)
  module StmtSpec = Add(
    struct let name = "Stmt contract"
           let hint = "Show statements contracts" end)
  module Axiomatic = Add(
    struct let name = "Axiomatic"
           let hint = "Show global axiomatics" end)
  module Unreachable = Add(
    struct let name = "Unreachable"
           let hint = "Show 'unreachable' hypotheses" end)
  module Other = Add(
    struct let name = "Other"
           let hint = "Show other properties" end)
  (*module Pragma = Add(struct let name = "pragma" end) *)
  module RteNotGenerated = Add(
    struct let name = "Non generated"
           let hint = "Show RTEs assertions that remain to generate" end)
  module RteGenerated = Add(
    struct let name = "Generated"
           let hint = "Show RTEs assertions that have been generated" end)

  let pack hb =
    OnlyCurrent.add hb;
    Preconditions.add hb;
    Ensures.add hb;
    Behaviors.add hb;
    Assigns.add hb;
    From.add hb;
    Assert.add hb;
    Invariant.add hb;
    Variant.add hb;
    Terminates.add hb;
    Unreachable.add hb;
    StmtSpec.add hb;
    Axiomatic.add hb;
    Other.add hb;
  (*Pragma.add hb;*)
    RteNotGenerated.add hb;
    RteGenerated.add hb;
end

open Refreshers

let make_panel (main_ui:main_window_extension_points) =
  let container = GPack.hbox () in

  let sc_buttons =
    GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`NEVER ()
  in
  let vb = GPack.vbox () in
  let refresh_button = GButton.button ~label:"Refresh" ~packing:vb#pack () in
  Refreshers.pack vb;
  sc_buttons#add_with_viewport vb#coerce;
  container#pack sc_buttons#coerce;

  let module MODEL =  
	Gtk_helper.MAKE_CUSTOM_LIST(struct type t = property end)
  in
  let model = MODEL.custom_list () in
  let append m = if m.visible then model#insert m in
  let clear () = model#clear () in
  (* TOOD: this avoids some problems when changing projects, where
     the property navigator displays outdated information. A better solution
     would be to projectify what is being displayed *)
  Design.register_reset_extension (fun _ -> clear ());
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
	 | Some { MODEL.finfo = { ip = ip } } ->
	   graph_window main_ui#main_window "Dependencies" ip
	 | None -> ()));
  view#selection#set_select_function
    (fun path currently_selected ->
      if not currently_selected then
        begin match model#custom_get_iter path with
        | Some {MODEL.finfo={ip = ip;}} ->
          ignore (main_ui#scroll (Pretty_source.PIP ip))
        | None -> ()
        end;
      true);

  let top = `YALIGN 0.0 in

  let make_view_column renderer properties ~title =
    let cview = MODEL.make_view_column model renderer properties ~title in
    cview#set_resizable true;
    ignore (view#append_column cview)
  in

  (* Module name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{module_name=m} -> [`TEXT m])
    ~title:"Module";

  (* Function name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{function_name=m} -> [`TEXT m])
    ~title:"Function";

  (* Kind name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{kind=k} -> [`TEXT k])
    ~title:"Kind";

  (* Status colored column viewer *)
  make_view_column (GTree.cell_renderer_pixbuf [top])
    (function {status_icon=status_icon} ->
      [`PIXBUF (Gtk_helper.Icon.get status_icon)])
    ~title:"Status";

  (* (Local) status name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{status_name=k}-> [`TEXT k])
    ~title:"Local Status";

  (* Consolidated status name column viewer *)
  make_view_column (GTree.cell_renderer_text [top])
    (function{consolidated_status_name=k}-> [`TEXT k])
    ~title:"Consolidated Status";

  view#set_model (Some model#coerce);

  (* [JS 2011-08-29] Be careful: that it is incorrect to mask some properties
     when they are the only not-valid ones. In such a case, all is green in the
     property panel implying that the verification task successfully ended,
     while that is not true actually.
     [BY 2011-09-31] JS: I'm not sure what you mean. Do you want a check
     that supersedes all the other settings, and displays everything which
     is not green?
  *)
  let visible ip = match ip with
    | Property.IPOther _ -> Other.get ()
    | Property.IPUnreachable _ -> Unreachable.get ()
    | Property.IPBehavior (_,Kglobal,_) -> Behaviors.get ()
    | Property.IPBehavior (_,Kstmt _,_) -> Behaviors.get () && StmtSpec.get ()
    | Property.IPPredicate(Property.PKRequires _,_,Kglobal,_) ->
        Preconditions.get ()
    | Property.IPPredicate(Property.PKRequires _,_,Kstmt _,_) ->
        Preconditions.get () && StmtSpec.get ()
    | Property.IPPredicate(Property.PKAssumes _,_,_,_) -> false
    | Property.IPPredicate(Property.PKEnsures _,_,Kglobal,_) -> Ensures.get ()
    | Property.IPPredicate(Property.PKEnsures _,_,Kstmt _,_) ->
        Ensures.get() && StmtSpec.get()
    | Property.IPPredicate(Property.PKTerminates,_,_,_) -> Terminates.get ()
    | Property.IPAxiom _ -> false
    | Property.IPAxiomatic _ -> Axiomatic.get () && not (OnlyCurrent.get ())
    | Property.IPLemma _ -> Axiomatic.get ()
    | Property.IPComplete _ -> Behaviors.get ()
    | Property.IPDisjoint _ -> Behaviors.get ()
    | Property.IPCodeAnnot(_,_,{annot_content = AAssert _}) -> Assert.get ()
    | Property.IPCodeAnnot(_,_,{annot_content = AInvariant _}) ->
        Invariant.get ()
    | Property.IPCodeAnnot(_,_,{annot_content = APragma p}) ->
        Logic_utils.is_property_pragma p (* currently always false. *)
    | Property.IPCodeAnnot(_, _, _) -> assert false
    | Property.IPAssigns (_,Kglobal,_,_) -> Assigns.get ()
    | Property.IPAssigns (_,Kstmt _,Property.Id_code_annot _,_) ->
        Assigns.get ()
    | Property.IPAssigns (_,Kstmt _,Property.Id_behavior _,_) ->
        Assigns.get() && StmtSpec.get()
    | Property.IPFrom _ -> From.get ()
    | Property.IPDecrease _ -> Variant.get ()
  in
  let fill_model () =
    let status_string s = Pretty_utils.sfprintf "%a" Property_status.pretty s in
    let files = Globals.FileIndex.get_files () in
    (* add global annotations *)
    let annot_by_files =
      List.map
	(fun f -> 
	  Globals.FileIndex.get_global_annotations f, Filename.basename f)
	files
    in
    let add_ip module_name function_name ip =
      if visible ip then
	let status = Property_status.get ip in
	append
	  (make_property 
	     ~module_name
	     ~function_name
	     ~status_name:(status_string status)
	     ~ip)
    in
    List.iter
      (fun (l, f) -> 
	List.iter 
	  (fun a -> 
	    List.iter (add_ip f "") (Property.ip_of_global_annotation a))
	  l)
      annot_by_files;
    (* We only display the name of the file *)
    let files = List.map
      (fun file->(Globals.FileIndex.get_functions file,Filename.basename file))
      files
    in
    List.iter
      (fun (kfs, file_base) ->
        let add_ip ip = 
          let function_name =
            (Extlib.may_map
	       (fun f -> Kernel_function.get_name f ^ ": ") ~dft:""
	       (Property.get_kf ip))
            ^ (Extlib.may_map
                 (fun b -> "behavior " ^ b.b_name ^ ": ") ~dft:""
                 (Property.get_behavior ip))
          in
	  add_ip file_base function_name ip
	in
	List.iter
          (fun kf ->
            if (not (OnlyCurrent.get ()) ||
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
		(fun (rte_status, _, rte_status_get,_) ->
                  let st = rte_status_get kf in
                  match st, RteGenerated.get (), RteNotGenerated.get ()
                  with
                    | true, true, _
                    | false, _, true ->
		      append
		        (make_property
			   ~module_name:file_base
			   ~function_name:kf_name
			   ~status_name:(if st then "Generated"
			                 else "not Generated")
                           ~ip:(Property.ip_other
				  (State.get_name rte_status)
				  None Kglobal))
                    | true, false, _
                    | false, _, false -> ()
                )
		(rte_get_all_status ());
              let add_spec spec code_annotations =
                let ip_spec = Property.ip_of_spec kf Kglobal spec in
                let ip_annot =
                  List.fold_right
                    (fun (stmt,loc_ca) acc ->
                      let ca = match loc_ca with | User ca|AI(_,ca) -> ca in
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
let highlighter (buffer:GSourceView2.source_buffer) localizable ~start ~stop =
  match localizable with
  | Pretty_source.PIP (Property.IPPredicate (Property.PKAssumes _,_,_,_)) ->
    (* Assumes clause do not get a bullet: there is nothing
       to prove about them.*)
    ()
  | Pretty_source.PIP ppt ->
      Design.Feedback.mark buffer ~start ~stop (Property_status.Feedback.get ppt)
  | Pretty_source.PGlobal _| Pretty_source.PVDecl _
  | Pretty_source.PTermLval _| Pretty_source.PLval _
  | Pretty_source.PStmt _ -> ()

let extend (main_ui:main_window_extension_points) =
  make_panel main_ui;
  main_ui#register_source_highlighter highlighter

let () = Design.register_extension extend

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
