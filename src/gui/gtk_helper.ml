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

(* $Id: gtk_helper.ml,v 1.12 2008/05/23 14:35:10 uid528 Exp $ *)

(** Generic Gtk helpers. *)

let apply_tag (b:GText.buffer) tag pb pe =
  let start = b#get_iter (`OFFSET pb) in
  let stop = b#get_iter (`OFFSET pe) in
  b#apply_tag ~start ~stop tag

let remove_tag (b:GText.buffer) tag pb pe =
  let start = b#get_iter (`OFFSET pb) in
  let stop = b#get_iter (`OFFSET pe) in
  b#remove_tag ~start ~stop tag

let cleanup_tag (b:GText.buffer) tag = 
  b#remove_tag tag ~start:b#start_iter ~stop:b#end_iter

let make_tag (buffer:GText.buffer) ~name l = 
  match GtkText.TagTable.lookup buffer#tag_table name with
  | None -> buffer#create_tag ~name l
  | Some t -> new GText.tag t


let expand_to_path (treeview:GTree.view) path =
  (* in the future when a recent lablgtk2 is installed it becomes:
     [treeview#expand_to_path path]
  *)
  let path = GTree.Path.copy path in
  let path_nb  = GTree.Path.get_indices path in
  let path = GTree.Path.create [] in
  for i = 0 to pred (Array.length path_nb) do
    GTree.Path.append_index path path_nb.(i);
    treeview#expand_row path
  done

let make_formatter (t:GText.buffer) = 
  let fmt_emit s start length =
    let subs = String.sub s start length in
    t#insert subs
  in
  let fmt_flush () = ()
  in
  Format.make_formatter fmt_emit fmt_flush

let redirect fmt (t:GText.buffer) = 
  let fmt_emit s start length =
    let subs = String.sub s start length in
    t#insert subs
  in
  let fmt_flush () = () in
  Format.pp_set_formatter_output_functions
    fmt
    fmt_emit fmt_flush

let channel_redirector channel callback = 
  let cout,cin = Unix.pipe () in
  Unix.dup2 cin channel ;
  let channel = Glib.Io.channel_of_descr cout in
  let len = 80 in
  let buf = String.create len in
  ignore (Glib.Io.add_watch channel ~prio:0 ~cond:[`IN; `HUP; `ERR] ~callback:
    begin fun cond -> 
      try if List.mem `IN cond then begin
	(* On Windows, you must use Io.read *)
	let len = Glib.Io.read channel ~buf ~pos:0 ~len in
	len >= 1 && (callback (String.sub buf 0 len)) 
      end
      else false
      with e -> 
        ignore (callback 
                  ("Channel redirector got an exception: " ^ (Printexc.to_string e))); 
       false
    end)

let gui_unlocked = ref true
  


  
let make_string_list ~packing = 
  let (model,column) = 
    GTree.store_of_list Gobject.Data.string [] in
  let insert s = 
    let row = model#append () in
    model#set ~row ~column s
  in
  let get_all () = 
    let l = ref [] in
    model#foreach (fun _ row -> 
		     l := model#get ~row ~column ::!l ;
		     true);
      !l
  in
  let view = GTree.view ~model ~reorderable:true ~packing () in
  let view_column = GTree.view_column ~title:"Source file" () in
  let str_renderer = GTree.cell_renderer_text [] in
    view_column#pack str_renderer;
    view_column#add_attribute str_renderer "text" column;
  let _ = view#append_column view_column in
  let remove_selected () = 
    let path_list = view#selection#get_selected_rows in
    let row_refs = List.map model#get_row_reference path_list in
    List.iter (fun rr -> ignore (model#remove rr#iter)) row_refs
  in
  insert,remove_selected, get_all


let model_of_list conv l =
  let cols = new GTree.column_list in
  let column = cols#add conv in
  let model = GTree.list_store cols in
  List.iter
    (fun data ->
      let row = model#append () in
      model#set ~row ~column data)
    l ;
  (model, column)

let string_selector completions packing =
  let (model, col) = model_of_list Gobject.Data.string completions in
  let entry = GEdit.entry ~packing () in
  let c = GEdit.entry_completion ~model ~entry () in
  c#set_text_column col ;
  entry
(*  (GEdit.combo ~popdown_strings:completions ~packing ())#entry *)

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
