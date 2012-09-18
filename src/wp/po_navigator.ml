(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* This is the panel to control the status of proof obligations. *)

open Design
open VCS

type row = {
  wpo: Wpo.t;
  visible : bool
}

(* Contains the prover associated to the given column *)
module Prover_Column = struct
  let column_tbl = Hashtbl.create 7
  let get (col:GTree.view_column) =
    try Some (Hashtbl.find column_tbl col#misc#get_oid)
    with Not_found -> None
  let register col prover = Hashtbl.add column_tbl col#misc#get_oid prover
end

module SelectionHook=Hook.Build(struct type t = Wpo.t end)

let refresh_current_wpo = ref (None : (Wpo.t * (prover * result) list) option)
let refresh_panel_callback = ref (fun () -> ())
let refresh_status_callback = ref (fun () -> ())

let make_panel (main_ui:main_window_extension_points) =
  let container = GPack.vbox ~show:true () in
  let paned = GPack.paned `VERTICAL
    ~packing:(container#pack ~expand:true ~fill:true)
    ()
  in
  (* Save position of the vpaned *)
  let _ = paned#event#connect#button_release
    ~callback:(fun _ ->
      Gtk_helper.save_paned_ratio "po_navigator_paned" paned; false)
  in
  let upper_frame = GBin.frame ~packing:paned#add1 () in
  let lower_frame = GBin.frame ~packing:paned#add2 () in
  let module MODEL =  Gtk_helper.MAKE_CUSTOM_LIST(struct type t = row end) in
  let model = MODEL.custom_list () in

  let model_age = ref 0 in
  let append m =
    incr model_age;
    if m.visible then model#insert m
  in
  let clear () = incr model_age; model#clear () in
  let sc =
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:upper_frame#add
      ()
  in
  let view =
    GTree.view ~rules_hint:true ~headers_visible:true ~packing:sc#add ()
  in
  ignore
    (view#connect#row_activated
       ~callback:(fun path col ->
         match model#custom_get_iter path,Prover_Column.get col with
         | Some ({MODEL.finfo= {wpo=wpo}} as custom), Some prover when prover <> WP
           ->
             (* copy to prevent Gtk to free it too soon.
                Bug in all pre 2.14 versions of Lablgtk2 *)
           let path = GtkTree.TreePath.copy path in
           Wpo.set_result wpo prover VCS.computing ;
           model#custom_row_changed path custom ;
           let current_model_age = !model_age in
           let callback _wpo _prover _result =
             if current_model_age = !model_age then
               begin
                 model#custom_row_changed path custom;
                 Gui_parameters.debug "Custom row changed";
               end;
             main_ui#rehighlight ()
           in
           let task = Prover.prove wpo ~interactive:true ~callback prover in
           let server = ProverTask.server () in
           Task.spawn server (Task.job task) ;
           Task.launch server
         | _ -> ()));
  view#selection#set_select_function
    (fun path currently_selected ->
      if not currently_selected then
        begin match model#custom_get_iter path with
          | Some {MODEL.finfo = {wpo=wpo};} ->
              SelectionHook.apply wpo;
              let prop = Wpo.get_property wpo in
              main_ui#scroll (Pretty_source.PIP prop)
              
          | None -> ()
        end;
      true);

  let top = `YALIGN 0.0 in

     (* Generic function to add a textual column to the panel. *)
  let add_text_column ~title f =
    let cview = MODEL.make_view_column model
      (GTree.cell_renderer_text [top])
      (fun {wpo=wpo} -> [`TEXT (f wpo)])
      ~title
    in
    cview#set_resizable true;
    ignore (view#append_column cview)
  in

  add_text_column
    ~title:"Module"
    (fun wpo -> 
       match Wpo.get_index wpo with
	 | Wpo.Lemma s -> Printf.sprintf "Lemma '%s'" s
	 | Wpo.Function(kf,_) -> Printf.sprintf "Function '%s'" 
	     (Kernel_function.get_name kf)
    );

  add_text_column
    ~title:"Property"
    (fun wpo -> 
       let context = Wpo.kf_context (Wpo.get_index wpo) in
       let property = Wpo.get_property wpo in
       Pretty_utils.to_string 
	 (Description.pp_localized ~kf:context ~ki:false ~kloc:false) 
	 property) ;

  add_text_column ~title:"Kind" Wpo.get_label ;

  add_text_column ~title:"Model" Wpo.get_model ;

  let icon_of_result r = 
    match r.verdict with
      | VCS.NoResult -> ""
      | VCS.Valid -> "gtk-yes"
      | VCS.Failed -> "gtk-dialog-error"
      | VCS.Unknown -> "gtk-dialog-question"
      | VCS.Timeout -> "gtk-cut"
      | VCS.Stepout -> "gtk-cut"
      | VCS.Invalid -> "gtk-no"
      | VCS.Computing -> "gtk-execute"
  in

  (* Prover columns *)
  let make_prover_status prover =
    let cview = MODEL.make_view_column model
      (GTree.cell_renderer_pixbuf [top])
      (fun {wpo=wpo} ->
        match Wpo.get_result wpo prover with
          | { verdict=NoResult } when prover=WP ->
	      [ `PIXBUF(Gtk_helper.Icon.get Gtk_helper.Icon.Unmark) ] 
          | r -> [ `STOCK_ID (icon_of_result r) ]
      ) ~title:(VCS.name_of_prover prover)
    in
    cview#set_resizable true;
    cview#set_clickable true;
    ignore (cview#connect#clicked
              (fun () ->
                Gui_parameters.debug "Clicked on column %a" VCS.pp_prover prover)) ;
    ignore (view#append_column cview);
    Prover_Column.register cview prover
  in
  List.iter make_prover_status VCS.gui_provers ;

     (* Last column is empty and juste uses the extra white space *)
  let last_column = GTree.view_column ~title:"" () in
  ignore (view#append_column last_column);

  view#set_model (Some model#coerce);

  let information_window = 
    Source_manager.make ~packing:lower_frame#add ~tab_pos:`RIGHT () 
  in
  SelectionHook.extend
    (fun wpo ->
       let results = Wpo.get_results wpo in
       match !refresh_current_wpo with
	 | Some (wold,rold) when 
	     (Wpo.get_gid wold = Wpo.get_gid wpo) && 
	       (Pervasives.compare rold results = 0) -> ()
	 | _ ->
	     begin
	       refresh_current_wpo := Some (wpo,results) ;
	       Source_manager.clear information_window ;
	       let files = Wpo.get_files wpo in
	       List.iter
		 (fun (title,filename) ->
		    Source_manager.load_file information_window 
		      ~title ~filename ~line:1 ())
		 files ;
	       match files with
		 | [] -> ()
		 | (title,_)::_ ->
		     Source_manager.select_name information_window title ;
	     end
    ) ;

  let fill_model () =
    Wpo.iter ~on_goal:(fun wpo -> append {wpo=wpo; visible=true}) ()
  in

  refresh_panel_callback :=
    (fun () ->
       main_ui#protect ~cancelable:false
         (fun () ->
            clear ();
            if paned#position < 64 then paned#set_position 64 ;
            fill_model ())) ;

  refresh_status_callback :=
    (fun () ->
       model#foreach (fun p i -> model#row_changed p i;false) ;
       match !refresh_current_wpo with
         | None -> ()
         | Some (wpo,_) -> SelectionHook.apply wpo
    ) ;

  (*To position the panels at startup:*)
  let (_:GtkSignal.id) = view#misc#connect#after#realize
    (fun () ->
       !refresh_panel_callback () ;
       Gtk_helper.place_paned paned
         (Gtk_helper.Configuration.find_float
            ~default:0.60
            "po_navigator_paned"))
  in
  ignore (main_ui#lower_notebook#append_page
            ~tab_label:(GMisc.label ~text:"WP Proof Obligations" ())#coerce
            (container#coerce))

let extend (main_ui:main_window_extension_points) =
  make_panel main_ui

let () = Design.register_extension extend

let refresh_panel () = !refresh_panel_callback ()
let refresh_status () = !refresh_status_callback ()

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
