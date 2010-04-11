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

(* ABP added *)
open Cil_types
open Visitor
open Pretty_source
open Kernel_function

class cyclo_class (main_ui:Design.main_window_extension_points) =
object(self)

  val mutable fct_to_check = (GText "")

  method get_data =
    let checker = (new Register.slocVisitor) in
    checker#set_standalone false;
    Metrics_parameters.debug "beginning of cyclo check";
    ignore (visitFramacGlobal
	      (checker :> frama_c_visitor)
	      fct_to_check);
    checker#sloc,
  checker#ifs,
  checker#assigns,
  checker#loops,
  checker#calls,
  checker#gotos,
  checker#mem_access,
  (checker#ifs + checker#loops) - checker#exits + 2
    (* 2 becomes "2*checker#funcs" in the general case *)

  method insert_text (buffer: GText.buffer) =
    let iter = buffer#get_iter `START in
    let data  = self#get_data in
    let (slocs,ifs,assigns,loops,calls, gotos, mems,cyclos) = data in
    buffer#insert ~iter (string_of_int slocs);
    buffer#insert ~iter (string_of_int ifs);
    buffer#insert ~iter (string_of_int assigns);
    buffer#insert ~iter (string_of_int loops);
    buffer#insert ~iter (string_of_int calls);
    buffer#insert ~iter (string_of_int gotos);
    buffer#insert ~iter (string_of_int mems);
    buffer#insert ~iter (string_of_int cyclos)

  method do_cyclo (main_ui:Design.main_window_extension_points) =
    Metrics_parameters.debug "Cyclo";
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
    let data  = self#get_data in
    let (slocs, ifs, assigns, loops, calls, gotos, mems, cyclos) = data in
    let add_label msg n =
      let text = msg ^ string_of_int n in
      ignore (GMisc.label ~text ~packing:a_vbox#add ())
    in
    add_label "#Lines of source code: " slocs;
    add_label "#Lines of if statements: " ifs;
    add_label "#Lines of assignments: " assigns;
    add_label "#Lines of loops: " loops;
    add_label "#Lines of function calls: " calls;
    add_label "#Lines of gotos: " gotos;
    add_label "#Lines of memory accesses: " mems;
    add_label "Cyclomatic complexity: " cyclos;
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
    Metrics_parameters.debug "cyclo_selector";
    if button = 3 then
      let callback () =
        Metrics_parameters.debug "cyclo_selector - callback";
        self#display_localizable localizable  ()
      in
      ignore (popup_factory#add_item "Metrics" ~callback:callback)

  initializer
    Metrics_parameters.debug "Cyclo init";
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
 let l = GMisc.label ~selectable:true ~xalign:0.0 ~text:""
             ~packing:(container#pack ~expand:true)
             ()
 in
 l

let make_hbox (parent:GPack.box) =
 GPack.hbox ~homogeneous:true ~packing:parent#pack ()

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
  let sloc_label = make_bi_label box "sloc:" in
  (* Calls *)
  let calls_label =  make_bi_label box "calls:" in


  let box = make_hbox w in
  (* If *)
  let if_label = make_bi_label box "if:" in
  (* while *)
  let loops_label = make_bi_label box "loops:" in

  let box = make_hbox w in
  (* Goto *)
  let goto_label = make_bi_label box "goto:" in
  (* assign *)
  let assign_label =  make_bi_label box "assigns:" in

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
    (* cyclomatric complexity *)
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
          = !Db.Metrics.last_result ()
      in
      update_button#misc#set_sensitive false;
      sloc_label#set_text (string_of_int sloc);
      calls_label#set_text (string_of_int calls);
      if_label#set_text (string_of_int ifs);
      loops_label#set_text (string_of_int loops);
      goto_label#set_text (string_of_int gotos);
      assign_label#set_text (string_of_int assigns);
      mem_label#set_text (string_of_int mem_access);
      func_label#set_text (string_of_int (Cilutil.VarinfoHashtbl.length fs));
      proto_label#set_text (string_of_int(Cilutil.VarinfoHashtbl.length fws));
      cyclo_label#set_text (string_of_int cycl)
    with Not_found ->
      update_button#misc#set_sensitive true ;
      init ()
  in
  ignore
    (update_button#connect#clicked
       (fun () -> !Db.Metrics.compute (); fill ()));
  "Metrics", w#coerce, Some fill

let gui (main_ui:Design.main_window_extension_points) =
  main_ui#register_panel make_panel;
  ignore (new cyclo_class main_ui)

let () =
  Design.register_extension gui
