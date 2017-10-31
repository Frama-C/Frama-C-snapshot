(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

  let display_result (main_ui:Design.main_window_extension_points) (parent_win:GPack.box) =
    try
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
    with
    | Ast.NoUntypedAst ->
      main_ui#error "Cannot compute Halstead metrics: untyped AST not present.\n\
                     It has been removed either by user request or \
                     by some AST transformation."

  let register main_ui = Metrics_gui.register_metrics name (display_result main_ui)
end

module CyclomaticMetricsGUI = struct
  open Metrics_base
  open Pretty_source
  open Visitor
  

  let name = "Cyclomatic"

  class cyclo_class ~libc (main_ui:Design.main_window_extension_points) = object(self)

    val mutable checked_fun = Kernel_function.dummy ()

    method get_data =
      let checker = (new Metrics_cilast.slocVisitor ~libc) in
      ignore (visitFramacGlobal (checker :> frama_c_visitor)
                (Kernel_function.get_global checked_fun));
      checker#get_metrics
    (* 2 becomes "2*checker#funcs" in the general case *)

    method do_value (main_ui:Design.main_window_extension_points) loc
      (total:int) (valeur:int) (percent:float) =
        match loc with
          | PVDecl (Some kf,_,_) ->
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
          | PVDecl (Some kf,_,_) -> (* Process only the function selected *)
              (* Get the global of this function *)
              checked_fun <- kf;
              self#do_cyclo main_ui;
          | _  -> ()
      end

    method cyclo_selector (popup_factory:GMenu.menu GMenu.factory) main_ui ~button localizable =
      if button = 3 && Db.Value.is_computed () then
        match localizable with
          | PVDecl (Some kf, _,_) ->
            let callback1 () =
              Metrics_parameters.debug "cyclo_selector - callback";
              self#display_localizable localizable  ()
            in
	    let callback2 () =
	      (* function selected is kf *)
	       Metrics_coverage.compute_coverage_by_fun ();
	       (* Got a list of (kf,value,total,percent).
		  Now let's scan this list *)
	       try 
		 let valeur,total,percent = Metrics_coverage.get_coverage kf in
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

  let compute ~libc () = Metrics_cilast.compute_on_cilast ~libc

  let display_result ~libc (parent_win:GPack.box) =
    let padder = GBin.alignment
      ~padding:(5, 5, 15, 15) ~packing:parent_win#pack () in
    let box = GPack.vbox ~homogeneous:false () in
    padder#add (box:>GObj.widget);
    ignore(GMisc.label ~markup:(Printf.sprintf "<b>%s</b>" name)
             ~justify:`LEFT ~packing:box#pack ());
    ignore(GMisc.separator `HORIZONTAL ~packing:box#pack ());
    let metrics = Metrics_cilast.get_metrics ~libc in
    let table_contents = BasicMetrics.to_list metrics in
    Metrics_gui.display_as_table table_contents box

  let register ~libc main_ui =
    ignore (new cyclo_class ~libc main_ui);
    Metrics_gui.register_metrics name (display_result ~libc)
end

(** GUI hooks value coverage  *)
module ValueCoverageGUI = struct
  open Cil_datatype
  open Metrics_coverage
  open Gtk_helper

  let name = "Value coverage"

  let result = ref None 
  let highlight = ref false

  let update_filetree = ref (fun _ -> ())
  let filetree_enabled = ref true

  let filetree_visible () =
    !filetree_enabled && Metrics_coverage.is_computed_by_fun ()

  (* TODO : Metrics data structure must be projectified ? *)
  let compute ~libc =
    begin
      match !result with
        | None ->
          !Db.Value.compute ();
          result := Some (Metrics_coverage.compute ~libc)
        | Some _ -> ()
    end;
    Metrics_coverage.compute_coverage_by_fun ();
    !update_filetree `Contents;
    Extlib.the !result

  let decorate_filetree (main_ui: Design.main_window_extension_points) =
    let compute get = function
      | Cil_types.GFun ({Cil_types.svar = v }, _) ->
        begin
          try
            let kf = Globals.Functions.get v in
            get (Metrics_coverage.get_coverage kf)
          with Not_found -> -1
        end
      | _ -> -1
    in
    let percentage (_, _, pct_covered) = truncate (100. -. pct_covered) in
    let number (total, value, _) = total - value in
    let text get =
      fun g -> let i = compute get g in if i < 0 then "" else string_of_int i
    in
    let sort get =
      fun g h -> Datatype.Int.compare (compute get g) (compute get h)
    in
    let refresh_percentage =
      main_ui#file_tree#append_text_column
        ~title:"Dead code %"
        ~tooltip:"Percentage of dead code in each function"
        ~visible:filetree_visible
        ~text:(text percentage)
        ~sort:(sort percentage)
    in
    let refresh_number =
      main_ui#file_tree#append_text_column
        ~title:"Dead stmts"
        ~tooltip:"Number of dead statements in each function"
        ~visible:filetree_visible
        ~text:(text number)
        ~sort:(sort number)
    in
    let refresh x = refresh_percentage x; refresh_number x in
    update_filetree := refresh

  let () =
    Db.Value.Table_By_Callstack.add_hook_on_update
      (fun _ ->
         Metrics_coverage.clear_coverage_by_fun ();
        !update_filetree `Visibility)

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
              Varinfo.Set.diff metrics.syntactic metrics.semantic
            in
            let hilit color =
              let tag = make_tag buffer#buffer "metrics" [`BACKGROUND color] in
              apply_tag buffer#buffer tag start stop
            in
            let syn_hilit () = hilit "yellow"
            and sem_hilit () = hilit "green"
            and unseen_hilit () = hilit "red"
            in
            match loc with
              | Pretty_source.PVDecl(_, _, vi) ->
                if Ast_info.is_function_type vi then begin
                  if Varinfo.Set.mem vi pure_syntactic then syn_hilit ()
                  else if Varinfo.Set.mem vi metrics.semantic then sem_hilit ()
                  else unseen_hilit ()
                end
              | _ -> ()
          end
    end

  let display_result ~libc main_ui (parent_win:GPack.box) =
    let padder = GBin.alignment
      ~padding:(5, 5, 15, 15) ~packing:parent_win#pack () in
    let box = GPack.vbox ~homogeneous:false () in
    padder#add (box:>GObj.widget);
    ignore(GMisc.label ~markup:(Printf.sprintf "<b>%s</b>" name)
             ~justify:`LEFT ~packing:box#pack ());
    ignore(GMisc.separator `HORIZONTAL ~packing:box#pack ());
    let metrics = compute ~libc in
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
    let _ = Gtk_helper.on_bool box "Show columns"
        ~tooltip:"Shows the columns related to dead code in the filetree."
        (fun () -> !filetree_enabled)
        (fun b -> filetree_enabled := b; !update_filetree `Visibility)
    in
    main_ui#rehighlight ()

  let register ~libc main_ui =
    Design.register_reset_extension (fun _ -> result := None);
    main_ui#register_source_highlighter highlighter;
    let apply = Metrics_parameters.ValueCoverage.get () in
    Metrics_gui.register_metrics ~apply name (display_result ~libc main_ui);
end

let register_final ?(libc=Metrics_parameters.Libc.get ()) main_ui =
  let box = Metrics_gui.init_panel main_ui in
  Design.register_reset_extension Metrics_gui.reset_panel;
  HalsteadMetricsGUI.register main_ui;
  CyclomaticMetricsGUI.register ~libc main_ui;
  ValueCoverageGUI.register ~libc main_ui;
  Metrics_gui.coerce_panel_to_ui box main_ui

let gui (main_ui:Design.main_window_extension_points) =
  main_ui#register_panel register_final

let () =
  Design.register_extension gui;
  Design.register_extension ValueCoverageGUI.decorate_filetree
