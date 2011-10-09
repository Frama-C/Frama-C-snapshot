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

open Cil_types
open Visitor
open Pretty_source
open Kernel_function
open Metrics_base
;;

class cyclo_class (main_ui:Design.main_window_extension_points) = object(self)

  val mutable fct_to_check = (GText "")

  method get_data =
    let checker = (new Metrics_cilast.slocVisitor) in
    Metrics_parameters.Metrics.debug ~level:2 "Beginning of cyclo check@.";
    ignore (visitFramacGlobal (checker :> frama_c_visitor) fct_to_check);
    checker#get_metrics
    (* 2 becomes "2*checker#funcs" in the general case *)

  method insert_text (buffer: GText.buffer) =
    let iter = buffer#get_iter `START in
    let metrics_data  = self#get_data in
    buffer#insert ~iter (string_of_int metrics_data.cslocs);
    buffer#insert ~iter (string_of_int metrics_data.cifs);
    buffer#insert ~iter (string_of_int metrics_data.cassigns);
    buffer#insert ~iter (string_of_int metrics_data.cloops);
    buffer#insert ~iter (string_of_int metrics_data.ccalls);
    buffer#insert ~iter (string_of_int metrics_data.cgotos);
    buffer#insert ~iter (string_of_int metrics_data.cptrs);
    buffer#insert ~iter (string_of_int (cyclo metrics_data))

  method do_cyclo (main_ui:Design.main_window_extension_points) =
    Metrics_parameters.Metrics.debug "Cyclo";
    (* create a small results window *)
    let dialog = GWindow.window
      ~title:"Measure"
      ~modal:false
      ~position:`CENTER_ON_PARENT
      ~width:300 ~height:300 ~border_width:3
      ~resizable:true
      ()
    in
    dialog#set_transient_for main_ui#main_window#as_window;
    let a_vbox = GPack.vbox ~packing:dialog#add () in
    ignore (dialog#event#connect#delete
              ~callback:(fun _ -> dialog#misc#hide ();
                           true));
    let metrics_data  = self#get_data in
    let add_label msg n =
      let text = msg ^ string_of_int n in
      ignore (GMisc.label ~text ~packing:a_vbox#add ())
    in
    add_label "Lines of source code: " metrics_data.cslocs;
    add_label "# of if statements: " metrics_data.cifs;
    add_label "# of assignments: " metrics_data.cassigns;
    add_label "# of loops: " metrics_data.cloops;
    add_label "# of function calls: " metrics_data.ccalls;
    add_label "# of gotos: " metrics_data.cgotos;
    add_label "# of indirect memory accesses: " metrics_data.cptrs;
    add_label "Cyclomatic complexity: " (cyclo metrics_data);
    let close_button = GButton.button ~stock:`OK ~packing:a_vbox#add () in
    close_button#set_border_width 10;
    ignore (close_button#connect#clicked ~callback:dialog#misc#hide);
    dialog#show ()

  (* callback of menu_item "Cyclo" *)
  method display_localizable localizable () =
    begin
      match localizable with
        | PVDecl (Some kf,_) -> (* Process only the function selected *)
            begin
              (* Get the global of this function *)
              fct_to_check <- (get_global kf);
              self#do_cyclo main_ui;
            end
        | _  -> ()
    end

  method cyclo_selector
    (popup_factory:GMenu.menu GMenu.factory) _main_ui ~button localizable =
    Metrics_parameters.Metrics.debug "cyclo_selector";
    if button = 3 then
      match localizable with
        | PVDecl (Some _, _) ->
            let callback () =
              Metrics_parameters.Metrics.debug "cyclo_selector - callback";
              self#display_localizable localizable  ()
            in
            ignore (popup_factory#add_item "Metrics" ~callback:callback)
        | _ -> ()
  initializer
    main_ui#register_source_selector self#cyclo_selector
end

(* ABP end *)

let make_bi_label (parent:GPack.box) l1 =
  let container = GPack.hbox ~packing:parent#pack () in
  let t = GMisc.label ~text:l1 ~xalign:0.0
    ~packing:(container#pack ~expand:false ~fill:false)
    ()
  in
  Gtk_helper.old_gtk_compat t#set_width_chars 7;
  let label = GMisc.label ~selectable:true ~xalign:0.0 ~text:""
    ~packing:(container#pack ~expand:true)
    ()
  in label

let make_hbox (parent:GPack.box) =
  GPack.hbox ~homogeneous:true ~packing:parent#pack ()

module LastResult =
  State_builder.Option_ref
    (DatatypeMetrics)
    (struct
      let dependencies = [ Ast.self ]
      let name = name
      let kind = `Internal
     end)
;;

let make_panel _main_ui =
  let w = GPack.vbox ~width:120 () in
  let update_button =
    let w = make_hbox w in
      GButton.button (* ~stock:`REFRESH *) ~label:"Measure"
        ~packing:(w#pack ~fill:false ~expand:true)
        ()
  in

  let box = make_hbox w in
  (* Sloc *)
  let sloc_label = make_bi_label box "Slocs:" in
  (* Calls *)
  let calls_label =  make_bi_label box "Calls:" in


  let box = make_hbox w in
  (* If *)
  let if_label = make_bi_label box "If:" in
  (* while *)
  let loops_label = make_bi_label box "Loops:" in

  let box = make_hbox w in
  (* Goto *)
  let goto_label = make_bi_label box "Goto:" in
  (* assign *)
  let assign_label =  make_bi_label box "Assigns:" in

  let box = make_hbox w in
  (* Mem *)
  let mem_label = make_bi_label box "Ptr:" in
  let _placeholder1 = make_bi_label box "" in

  let box = make_hbox w in
  (* funcs *)
  let func_label = make_bi_label box "Fct:" in
  (* proto *)
  let proto_label = make_bi_label box "Proto:" in

  let box = make_hbox w in
    (* cyclomatic complexity *)
  let cyclo_label = make_bi_label box "Cyclo:" in
  let _placeholder2 = make_bi_label box "" in

  let init () =
      sloc_label#set_text "";
      calls_label#set_text "";
      if_label#set_text "";
      loops_label#set_text "";
      goto_label#set_text "";
      assign_label#set_text "";
      mem_label#set_text "";
      func_label#set_text "";
      proto_label#set_text "";
      cyclo_label#set_text "";
  in

  let fill () =
    try
      let {Db.Metrics.if_statements = ifs;
            mem_access = mem_access;
            loop_statements = loops;
            call_statements = calls;
            assign_statements = assigns;
            goto_statements = gotos;
            sloc =  sloc;
            functions_without_source =  fws;
            functions_with_source = fs;
            cyclos = cycl; }
          = LastResult.get ()
      in
      update_button#misc#set_sensitive false;
      sloc_label#set_text (string_of_int sloc);
      calls_label#set_text (string_of_int calls);
      if_label#set_text (string_of_int ifs);
      loops_label#set_text (string_of_int loops);
      goto_label#set_text (string_of_int gotos);
      assign_label#set_text (string_of_int assigns);
      mem_label#set_text (string_of_int mem_access);
      func_label#set_text
        (string_of_int (Metrics_base.map_cardinal_varinfomap fs));
      proto_label#set_text (string_of_int(map_cardinal_varinfomap fws));
      cyclo_label#set_text (string_of_int cycl)
    with Not_found ->
      update_button#misc#set_sensitive true ;
      init ()
  in
  ignore
    (update_button#connect#clicked
       (fun () -> LastResult.set (!Db.Metrics.compute ()); fill ()));
  "Metrics", w#coerce, Some fill

let gui (main_ui:Design.main_window_extension_points) =
  main_ui#register_panel make_panel;
  ignore (new cyclo_class main_ui)

let () =
  Design.register_extension gui
