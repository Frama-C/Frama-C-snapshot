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

open Log

type t = 
    { widget: GTree.view;
      append : event -> unit;
      clear : unit -> unit;}

let make ~packing ~callback = 
  let module L = struct
    type t = event
    let column_list = new GTree.column_list
    let message_list_scope_col = column_list#add Gobject.Data.caml
    let message_list_channel_col = column_list#add Gobject.Data.string
    let message_list_message_col = column_list#add Gobject.Data.string
    let message_list_severity_col = column_list#add Gobject.Data.caml

    let scope = function
      | None -> "Global"
      | Some s -> Printf.sprintf "%s:%d" s.src_file s.src_line
	  
    let custom_value (_:Gobject.g_type) t ~column : Gobject.basic = 
      match column with
	| 0 -> (* scope *)   `CAML (Obj.repr t.evt_source)
	| 1 -> (* plugin *)  `STRING (Some (String.capitalize t.evt_plugin))
	| 2 -> (* message *) `STRING (Some t.evt_message)
	| 3 -> (* severity *) `CAML (Obj.repr t.evt_kind)
	| _ -> assert false
  end
  in  
  let module MODEL =  Gtk_helper.MAKE_CUSTOM_LIST(L) in
  let message_list_list_store = MODEL.custom_list () in
  let append m = message_list_list_store#insert m in
  let clear () = message_list_list_store#clear () in
  let sc =   
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing 
      ()
  in
  let view:GTree.view = GTree.view 
    ~rules_hint:true
    ~headers_visible:false
    ~packing:sc#add () in
  let model = message_list_list_store#coerce in
  let top = `YALIGN 0.0 in
  let severity_renderer = GTree.cell_renderer_pixbuf [top;`XALIGN 0.5] in
  let scope_renderer = GTree.cell_renderer_text [top] in
  let plugin_renderer = GTree.cell_renderer_text [top] in
  let message_renderer = GTree.cell_renderer_text [top] in
  let m_severity_renderer renderer (model:GTree.model) iter =
    let severity = model#get ~row:iter ~column:L.message_list_severity_col in
    renderer#set_properties (match severity with 
			       | Error -> [`STOCK_ID "gtk-dialog-error"]
			       | Warning -> [`STOCK_ID  "gtk-dialog-warning"]
			       | _ -> [`STOCK_ID "gtk-dialog-info"])
  in
  let m_scope_renderer renderer (model:GTree.model) iter =
    let src = model#get ~row:iter ~column:L.message_list_scope_col in
    renderer#set_properties [`TEXT (L.scope src)]
  in
  let m_plugin_renderer renderer (model:GTree.model) iter =
    let plugin = model#get ~row:iter ~column:L.message_list_channel_col in
    renderer#set_properties [`TEXT plugin]
  in
  let m_message_renderer renderer (model:GTree.model) iter =
    let message = model#get ~row:iter ~column:L.message_list_message_col in
    renderer#set_properties [`TEXT message]
  in
  let severity_col_view = GTree.view_column
    ~title:"" ~renderer:(severity_renderer, []) () in
  let scope_col_view = GTree.view_column
    ~title:"Source" ~renderer:(scope_renderer, []) () in
  let channel_col_view = GTree.view_column
    ~title:"Plugin" ~renderer:(plugin_renderer, []) () in
  let message_col_view = GTree.view_column
    ~title:"Message" ~renderer:(message_renderer, []) () in
  severity_col_view#set_cell_data_func
    severity_renderer (m_severity_renderer severity_renderer) ;
  scope_col_view#set_cell_data_func
    scope_renderer (m_scope_renderer scope_renderer) ;
  channel_col_view#set_cell_data_func
    plugin_renderer (m_plugin_renderer plugin_renderer) ;
  message_col_view#set_cell_data_func
    message_renderer (m_message_renderer message_renderer) ;
  
  ignore (view#append_column severity_col_view) ;
  ignore (view#append_column scope_col_view) ;
  ignore (view#append_column channel_col_view) ;
  ignore (view#append_column message_col_view) ;
  
  let on_message_activated (mess_view:GTree.view) tree_path _view_column =
    let model = mess_view#model in
    let row = model#get_iter tree_path in
    let src = model#get ~row ~column:L.message_list_scope_col in
    match src with
      | None -> ()
      | Some s -> callback s.src_file s.src_line 
  in
  ignore (view#connect#row_activated ~callback:(on_message_activated view)) ;
  view#set_model (Some model);

  {widget = view;
   append = append;
   clear = clear}

let append t message ~on_select:_  =
  t.append message

let clear t = t.clear ()

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
