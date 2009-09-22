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
  let _placeholder = make_bi_label box "" in

  let box = make_hbox w in
  (* funcs *)
  let func_label = make_bi_label box "Fct:" in
  (* proto *)
  let proto_label = make_bi_label box "Proto:" in

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
            functions_with_source = fs; }
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
      proto_label#set_text (string_of_int(Cilutil.VarinfoHashtbl.length fws))
    with Not_found ->
      update_button#misc#set_sensitive true ;
      init ()
  in
  ignore
    (update_button#connect#clicked 
       (fun () -> !Db.Metrics.compute (); fill ()));
  "Metrics", w#coerce, Some fill
   
let gui (main_ui:Design.main_window_extension_points) = 
  main_ui#register_panel make_panel

let () =
  Design.register_extension gui
