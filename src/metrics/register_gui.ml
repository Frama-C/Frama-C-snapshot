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

(** This module defines abstraction for Metrics use *)


let mk_bi_label (parent:GPack.box) l1 =
  let container = GPack.hbox ~packing:parent#pack () in
  let t = GMisc.label ~text:l1 ~xalign:0.0
    ~packing:(container#pack ~expand:false ~fill:true)
    ()
  in
  Gtk_helper.old_gtk_compat t#set_width_chars 7;
  let label = GMisc.label ~selectable:true ~xalign:0.0 ~text:""
    ~packing:(container#pack ~expand:true)
    ()
  in label




module HalsteadMetricsGUI = struct

  let compute = Metrics_cabs.compute_on_cabs 
  let name = "Halstead" 

  let display_result (parent_win:GPack.box) =
    let padder = GBin.alignment
      ~padding:(5, 5, 15, 15) ~packing:parent_win#pack () in
    let box = GPack.vbox ~homogeneous:false () in
    padder#add (box:>GObj.widget);
    ignore(GMisc.label ~markup:(Printf.sprintf "<b>%s</b>" name)
             ~justify:`LEFT ~packing:box#pack ());
    ignore(GMisc.separator `HORIZONTAL ~packing:box#pack ());
    let metrics = Metrics_cabs.Halstead.get_metrics () in
    let table_contents = Metrics_cabs.Halstead.to_list metrics in
    Metrics_gui.display_as_table table_contents box
  

  let register _ = Metrics_gui.register_metrics name display_result 
end

module CyclomaticMetricsGUI = struct
  open Metrics_base
  open Pretty_source
  open Visitor
  

  let name = "Cyclomatic"

  class cyclo_class (main_ui:Design.main_window_extension_points) = object(self)

    val mutable checked_fun = Kernel_function.dummy ()

    method get_data =
      let checker = (new Metrics_cilast.slocVisitor) in
      ignore (visitFramacGlobal (checker :> frama_c_visitor)
                (Kernel_function.get_global checked_fun));
      checker#get_metrics
    (* 2 becomes "2*checker#funcs" in the general case *)

    method do_value (main_ui:Design.main_window_extension_points) loc
      (total:int) (valeur:int) (percent:float) =
        match loc with
          | PVDecl (Some kf,_) ->
	    begin
	    (* Get the global of this function *)
	      let fname = Kernel_function.get_name kf in
	    (* create a small results window *)
	      let dialog = GWindow.window
		~title:(Format.sprintf "Value analysis statistics of %s" fname)
		~modal:false
		~position:`CENTER_ON_PARENT
		~border_width:3
		~resizable:true
		()
	      in
	      dialog#set_transient_for main_ui#main_window#as_window;
	      let padder = GBin.alignment
		~padding:(5, 0, 15, 15) ~packing:dialog#add () in
	      let vbox = GPack.vbox () in
	      padder#add (vbox:>GObj.widget);
	      ignore (dialog#event#connect#delete
			~callback:(fun _ -> dialog#misc#hide ();
			  true));
	      ignore(GMisc.label ~markup:(Printf.sprintf "<b>%s</b>" fname)
		       ~justify:`LEFT ~packing:vbox#pack ());
	      ignore(GMisc.separator `HORIZONTAL ~packing:vbox#pack ());
	      let metrics_data  = [["total stmts";(string_of_int total)]; 
				   ["stmts analyzed";(string_of_int valeur)];
				   ["percentage of stmts covered"; (string_of_float percent)]
				  ] in
	      Metrics_gui.display_as_table metrics_data vbox;
	      let close_button = GButton.button ~stock:`OK ~packing:vbox#pack () in
	      close_button#set_border_width 10;
	      ignore (close_button#connect#clicked ~callback:dialog#misc#hide);
	      dialog#show ()
	    end
          | _  ->  prerr_endline "no function"

    method do_cyclo (main_ui:Design.main_window_extension_points) =
      let fname = Kernel_function.get_name checked_fun in
    (* create a small results window *)
      let dialog = GWindow.window
        ~title:(Format.sprintf "Measures for %s" fname)
        ~modal:false
        ~position:`CENTER_ON_PARENT
        ~border_width:3
        ~resizable:true
        ()
      in
      dialog#set_transient_for main_ui#main_window#as_window;
      let padder = GBin.alignment
        ~padding:(5, 0, 15, 15) ~packing:dialog#add () in
      let vbox = GPack.vbox () in
      padder#add (vbox:>GObj.widget);
      ignore (dialog#event#connect#delete
                ~callback:(fun _ -> dialog#misc#hide ();
                  true));
      ignore(GMisc.label ~markup:(Printf.sprintf "<b>%s</b>" fname)
               ~justify:`LEFT ~packing:vbox#pack ());
      ignore(GMisc.separator `HORIZONTAL ~packing:vbox#pack ());
      let metrics_data  = BasicMetrics.to_list self#get_data in
      Metrics_gui.display_as_table metrics_data vbox;
      let close_button = GButton.button ~stock:`OK ~packing:vbox#pack () in
      close_button#set_border_width 10;
      ignore (close_button#connect#clicked ~callback:dialog#misc#hide);
      dialog#show ()

  (* callback of menu_item "Cyclo" *)
    method display_localizable localizable () =
      begin
        match localizable with
          | PVDecl (Some kf,_) -> (* Process only the function selected *)
              (* Get the global of this function *)
              checked_fun <- kf;
              self#do_cyclo main_ui;
          | _  -> ()
      end

    method cyclo_selector (popup_factory:GMenu.menu GMenu.factory) _main_ui ~button localizable =
      if button = 3 && Db.Value.is_computed () then
        match localizable with
          | PVDecl (Some kf, _) ->
            let callback1 () =
              Metrics_parameters.debug "cyclo_selector - callback";
              self#display_localizable localizable  ()
            in
	    let callback2 () =
	      (* function selected is kf *)
	       let semantic = (Metrics_coverage.compute_semantic ()) in
	       let l = (Metrics_coverage.compute_coverage_by_fun semantic) in
	       (* Got a list of (kf,value,total,percent).
		  Now let's scan this list *)
	       try 
		 let (_,valeur,total,percent) =
		   (List.find (fun (kf2,_,_,_) -> Kernel_function.equal kf kf2) l) in
		 self#do_value main_ui localizable valeur total percent
	       with Not_found -> ()
	    in
	    begin
              ignore (popup_factory#add_item "Cyclomatic metrics"
			~callback:callback1);
              ignore (popup_factory#add_item "Value metrics"
			~callback:callback2)
	    end
          | _ -> ()

    initializer
      main_ui#register_source_selector self#cyclo_selector
  end

  let compute () = Metrics_cilast.compute_on_cilast () 

  let display_result (parent_win:GPack.box) =
    let padder = GBin.alignment
      ~padding:(5, 5, 15, 15) ~packing:parent_win#pack () in
    let box = GPack.vbox ~homogeneous:false () in
    padder#add (box:>GObj.widget);
    ignore(GMisc.label ~markup:(Printf.sprintf "<b>%s</b>" name)
             ~justify:`LEFT ~packing:box#pack ());
    ignore(GMisc.separator `HORIZONTAL ~packing:box#pack ());
    let metrics = Metrics_cilast.get_metrics () in
    let table_contents = BasicMetrics.to_list metrics in
    Metrics_gui.display_as_table table_contents box

  let register main_ui =
    ignore (new cyclo_class main_ui);
    Metrics_gui.register_metrics name display_result
end

(** GUI hooks value coverage  *)
module ValueCoverageGUI = struct
  open Cil_datatype
  open Metrics_coverage
  open Gtk_helper

  let name = "Value coverage"

  let result = ref None 
  let highlight = ref true 

  (* TODO : Metrics data structure must be projectified ? *)
  let compute () =
    begin
      match !result with
        | None -> result := Some (fst (Metrics_coverage.compute ()))
        | Some _ -> ()
    end;
    Extlib.the !result
  

  (* Functions are highlighted using different colors according to the
     following scheme:
     - Both semantically and syntactically reachable functions are green;
     - Only syntactically reachable are yellow;
     - Unreachable (neither semantically nor syntactically) functions
     are in red (bad!)
  *)
  let highlighter buffer loc ~start ~stop =
    if !highlight then begin
      match !result with
        | None -> ()
        | Some metrics ->
          begin
            let pure_syntactic =
              Varinfo.Set.diff metrics.syntactic metrics.semantic in
            let hilit color =
              let tag = make_tag buffer "metrics" [`BACKGROUND color] in
              apply_tag buffer tag start stop
            in
            let syn_hilit () = hilit "yellow"
            and sem_hilit () = hilit "green"
            and unseen_hilit () = hilit "red"
            in
            match loc with
              | Pretty_source.PVDecl(_, vi) ->
                if Ast_info.is_function_type vi then begin
                  if Varinfo.Set.mem vi pure_syntactic then syn_hilit ()
                  else if Varinfo.Set.mem vi metrics.semantic then sem_hilit ()
                  else unseen_hilit ()
                end
              | _ -> ()
          end
    end

  let display_result main_ui (parent_win:GPack.box) =
    let padder = GBin.alignment
      ~padding:(5, 5, 15, 15) ~packing:parent_win#pack () in
    let box = GPack.vbox ~homogeneous:false () in
    padder#add (box:>GObj.widget);
    ignore(GMisc.label ~markup:(Printf.sprintf "<b>%s</b>" name)
             ~justify:`LEFT ~packing:box#pack ());
    ignore(GMisc.separator `HORIZONTAL ~packing:box#pack ());
    let metrics = compute () in
    let pcent = Metrics_coverage.percent_coverage metrics in
    let progress_bar = GRange.progress_bar ~packing:box#pack () in
    progress_bar#set_fraction (pcent /. 100.0);
    ignore(GMisc.label
             ~markup:(Format.sprintf "%s%% functions reached"
                        (Metrics_base.float_to_string pcent))
             ~justify:`LEFT ~packing:box#pack ());
    let _ = Gtk_helper.on_bool box "Highlight results" (fun () -> !highlight)
      (fun b -> highlight := b; main_ui#rehighlight ()) 
    in
    main_ui#rehighlight ()

  let register main_ui =
    Design.register_reset_extension (fun _ -> result := None);
    main_ui#register_source_highlighter highlighter;
    Metrics_gui.register_metrics name (display_result main_ui);
end
  
let register_final main_ui =
  let box = Metrics_gui.init_panel () in
  Design.register_reset_extension Metrics_gui.reset_panel;
  HalsteadMetricsGUI.register main_ui;
  CyclomaticMetricsGUI.register main_ui;
  ValueCoverageGUI.register main_ui;
  Metrics_gui.coerce_panel_to_ui box main_ui

let gui (main_ui:Design.main_window_extension_points) =
  main_ui#register_panel register_final

let () =
  Design.register_extension gui
