(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
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

open Messages_manager
type t = { widget: GTree.view;
           append : message -> unit;
           clear : unit -> unit;}

let make ~packing ~callback = 
  let cols = new GTree.column_list in
  let message_list_file_col = cols#add Gobject.Data.string in
  let message_list_line_col = cols#add Gobject.Data.int in
  let message_list_message_col = cols#add Gobject.Data.string in
  let message_list_severity_col = cols#add Gobject.Data.caml in
  let message_list_list_store = GTree.list_store cols in
  let append message = 
    let row = message_list_list_store#append ()
    in
    message_list_list_store#set ~row ~column:message_list_file_col message.m_file ;
    message_list_list_store#set ~row ~column:message_list_line_col message.m_line ;
    message_list_list_store#set ~row ~column:message_list_message_col message.m_msg ;
    message_list_list_store#set ~row ~column:message_list_severity_col message.m_severity
  in
  let clear () = message_list_list_store#clear () in
  let sc =   
    GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing 
      ()
  in
  let view:GTree.view = GTree.view ~packing:sc#add () in
  let model = message_list_list_store#coerce in
  let file_renderer = GTree.cell_renderer_text [] in
  let line_renderer = GTree.cell_renderer_text [] in
  let message_renderer = GTree.cell_renderer_text [] in
  let m_file_renderer renderer (model:GTree.model) iter =
    let name = model#get ~row:iter ~column:message_list_file_col in
    renderer#set_properties [`TEXT name]
  in
  let m_line_renderer renderer (model:GTree.model) iter =
    let name = model#get ~row:iter ~column:message_list_line_col in
    renderer#set_properties [`TEXT (string_of_int name)]
  in
  let m_message_renderer renderer (model:GTree.model) iter =
    let name = model#get ~row:iter ~column:message_list_message_col in
    renderer#set_properties [`TEXT name]
  in
  let file_col_view = GTree.view_column
    ~title:"Filename"
    ~renderer:(file_renderer, []) () in
  let line_col_view = GTree.view_column
    ~title:"Line"
    ~renderer:(line_renderer, []) () in
  let message_col_view = GTree.view_column
    ~title:"Message"
    ~renderer:(message_renderer, []) () in
  file_col_view#set_cell_data_func
    file_renderer (m_file_renderer file_renderer) ;
  line_col_view#set_cell_data_func
    line_renderer (m_line_renderer line_renderer) ;
  message_col_view#set_cell_data_func
    message_renderer (m_message_renderer message_renderer) ;

  ignore (view#append_column file_col_view) ;
  ignore (view#append_column line_col_view) ;
  ignore (view#append_column message_col_view) ;

  let on_message_activated (mess_view:GTree.view) tree_path _view_column =
    let model = mess_view#model in
    let row = model#get_iter tree_path in
    let file = model#get ~row ~column:message_list_file_col in
    let line = model#get ~row ~column:message_list_line_col in
  callback file line 
  in
  ignore (view#connect#row_activated
            ~callback:(on_message_activated view)) ;
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
