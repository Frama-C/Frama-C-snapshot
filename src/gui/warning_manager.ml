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

open Log

let scope = function
  | None -> "Global"
  | Some s ->
    Printf.sprintf "%s:%d"
      (Filepath.pretty s.Lexing.pos_fname) s.Lexing.pos_lnum

module Legacy=struct
type t =
    { widget: GTree.view;
      append : event -> unit;
      clear : unit -> unit;}

let _make ~packing ~callback =
  let module MODEL =  Gtk_helper.MAKE_CUSTOM_LIST(struct type t = event end) in
  let model = MODEL.custom_list () in
  let append m = model#insert m in
  let clear () = model#clear () in
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
  let top = `YALIGN 0.0 in
  let severity_col_view = MODEL.make_view_column model
    (GTree.cell_renderer_pixbuf [top;`XALIGN 0.5])
     (fun e -> match e with
     | {evt_kind=Error} -> [`STOCK_ID "gtk-dialog-error"]
     | {evt_kind=Warning} -> [`STOCK_ID  "gtk-dialog-warning"]
     | _ -> [`STOCK_ID "gtk-dialog-info"])
    ~title:""
  in
  let scope_col_view = MODEL.make_view_column model
    (GTree.cell_renderer_text [top])
    (fun {evt_source=src} -> [`TEXT (scope src)])
    ~title:"Source"
  in
  let channel_col_view =
    MODEL.make_view_column model
      (GTree.cell_renderer_text [top])
      (fun {evt_plugin=m} -> [`TEXT m])
      ~title:"Plugin"
  in
  let message_col_view = MODEL.make_view_column model
    (GTree.cell_renderer_text [top])
    (fun {evt_message=m} -> [`TEXT m])
    ~title:"Message"
  in
  ignore (view#append_column severity_col_view) ;
  ignore (view#append_column scope_col_view) ;
  ignore (view#append_column channel_col_view) ;
  ignore (view#append_column message_col_view) ;

  let on_message_activated tree_path view_column =
    let v = model#custom_get_iter tree_path in
    match v with
    | None -> ()
    | Some {MODEL.finfo=e} ->
      callback e view_column
  in
  ignore (view#connect#row_activated ~callback:on_message_activated);
  view#set_model (Some model#coerce);

  {widget = view;
   append = append;
   clear = clear}

let _append t message =
  t.append message

let _clear t = t.clear ()
end

module New=struct
  type w = Log.event
  type t = 
      { widget: (int*w) Gtk_helper.Custom.columns;
	append : event -> unit;
	clear : unit -> unit;}

module Data = Indexer.Make(
  struct
    type t = int*w
    let compare (x,_) (y,_) = Pervasives.compare x y
  end)

let make ~packing ~callback =
  let model = object(self)
    val mutable m = Data.empty
    val mutable age = 0
    method data = m 
    method size =  Data.size m
    method index i = Data.index i m
    method get i = Data.get i m
    method add i = age<-age+1; m <- Data.add (age,i) m;age,i
    method reload = age<-0; m <- Data.empty
    method coerce = (self:> (int*w) Gtk_helper.Custom.List.model)
  end
  in
  let w = new Gtk_helper.Custom.List.view 
    ~packing ~headers:true ~rules:true model#coerce
  in
  let append e = w#insert_row (model#add e)
  in
  let clear () = 
    (* Post a reload request before clearing.
       The current model is used to know how many rows
       must be deleted. *)
    w#reload ;
  in    
  let _ = w#add_column_pixbuf ~title:"Kind" [`YALIGN 0.0;`XALIGN 0.5]
    (fun (_,e) -> match e with
     | {evt_kind=Error} -> [`STOCK_ID "gtk-dialog-error"]
     | {evt_kind=Warning} -> [`STOCK_ID  "gtk-dialog-warning"]
     | _ -> [`STOCK_ID "gtk-dialog-info"])
  in
  let _ = w#add_column_text ~title:"Source" [`YALIGN 0.0]
    (fun (_,{evt_source=src}) -> [`TEXT (scope src)])
  in
  let _ = w#add_column_text ~title:"Plugin" [`YALIGN 0.0]
    (fun (_,{evt_plugin=m}) -> [`TEXT m])
  in
  let _ = w#add_column_text ~title:"Message" [`YALIGN 0.0]
    (fun (_,{evt_message=m}) -> [`TEXT m])
  in
  w#on_click (fun (_,w) c -> callback w c);
  {widget=w;append=append;clear=clear}
   
let append t message = t.append message

let clear t = t.clear ()
end
include New
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
