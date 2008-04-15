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

(* $Id: gtk_helper.ml,v 1.26 2008/12/16 10:02:59 uid526 Exp $ *)

(** Generic Gtk helpers. *)

let apply_tag b tag pb pe =
  let b = (b:>GText.buffer) in
  let start = b#get_iter (`OFFSET pb) in
  let stop = b#get_iter (`OFFSET pe) in
  b#apply_tag ~start ~stop tag

let remove_tag b tag pb pe =
  let b = (b:>GText.buffer) in
  let start = b#get_iter (`OFFSET pb) in
  let stop = b#get_iter (`OFFSET pe) in
  b#remove_tag ~start ~stop tag

let cleanup_tag b tag = 
  let b = (b:>GText.buffer) in
  b#remove_tag tag ~start:b#start_iter ~stop:b#end_iter

(* this table shall not be projectified: it contains trans-project informations *)
module IntHashtbl = 
  Hashtbl.Make(struct 
                 type t = int 
                 let hash = Hashtbl.hash 
                 let equal = (=) 
               end)

let tag_names = IntHashtbl.create 17
let cleanup_all_tags b = 
  let b = (b:>GText.buffer) in
  let start = b#start_iter in
  let stop = b#end_iter in
  try 
  let tags = IntHashtbl.find tag_names (Oo.id b) in
  Cilutil.StringSet.iter (fun s -> b#remove_tag_by_name s ~start ~stop) tags
  with Not_found -> ()

let make_tag (buffer:< tag_table : Gtk.text_tag_table;
              create_tag : ?name:string -> GText.tag_property list -> GText.tag ; .. >)
    ~name l 
    = 
  match GtkText.TagTable.lookup buffer#tag_table name with
  | None -> 
      let oid = Oo.id buffer in
      let old_set = 
        try IntHashtbl.find tag_names oid 
        with Not_found -> Cilutil.StringSet.empty 
      in
      IntHashtbl.replace tag_names oid (Cilutil.StringSet.add name old_set);
      buffer#create_tag ~name l
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

let make_formatter ?(flush= fun () -> ()) t = 
  let t = (t:>GText.buffer) in
  let fmt_emit s start length =
    let subs = String.sub s start length in
    t#insert subs
  in
  Format.make_formatter fmt_emit flush

let redirect fmt (t:#GText.buffer) = 
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
		     false);
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

type 'a chooser = GPack.box -> string -> (unit -> 'a) -> ('a -> unit) -> (unit -> unit)

let on_bool (container:GPack.box) label get set = 
  let result = ref (get()) in
  let button = GButton.check_button 
    ~label
    ~packing:container#pack
    ~active:!result
    ()
  in
  ignore 
    (button#connect#toggled 
       ~callback:
       (fun () -> result := button#active));
  (fun () -> set !result)


let on_bool_radio (container:GPack.box) label_true label_false get set = 
  let result = ref (get()) in
  let container = GPack.hbox ~packing:container#pack () in
  let label_true = GButton.radio_button
    ~label:label_true
    ~packing:container#pack
    ~active:!result
    ()
  in
  let _label_false = GButton.radio_button
    ~group:label_true#group
    ~label:label_false
    ~packing:container#pack
    ~active:(not !result)
    ()
  in
  ignore 
    (label_true#connect#toggled 
       ~callback:
       (fun () -> result := label_true#active));
  (fun () -> set !result)

let range_selector (container:GPack.box) ~label ~lower ~upper set get =
    let container = GPack.hbox ~packing:container#pack () in
    let x = GEdit.spin_button ~digits:0 ~packing:(container#pack ~padding:10)() in
    x#adjustment#set_bounds ~lower:(float lower) ~upper:(float upper) ~step_incr:1. ();
    x#adjustment#set_value (float (get ()));
    ignore 
      (x#connect#value_changed 
	 ~callback:
         (fun () -> set x#value_as_int));
    ignore (GMisc.label ~line_wrap:true ~text:label ~xalign:0. ~packing:container#pack ());
    (fun () -> x#adjustment#set_value (float (get ())))

let on_int ?(lower=0) ?(upper=max_int) ?(sensitive=(fun () -> true)) (container:GPack.box) label  get set = 
  let result = ref (get()) in
  let make_spin ~label ~value =
    let container = GPack.hbox ~packing:container#pack () in
    let x = GEdit.spin_button ~digits:0 ~packing:container#pack () in
    x#adjustment#set_bounds ~lower:(float lower) ~upper:(float upper) ~step_incr:1. ();
    x#adjustment#set_value (float value);
    ignore 
      (x#connect#value_changed 
	 ~callback:
         (fun () -> result := x#value_as_int));
    let l = GMisc.label ~line_wrap:true ~text:label ~xalign:0. ~packing:container#pack ()
    in x,l  
  in
  let (x,l) = make_spin ~label ~value:!result
  in (fun () ->
        l#misc#set_sensitive (sensitive ()) ;
        x#misc#set_sensitive (sensitive ()) ;
        set !result)


let on_string ?(validator=(fun _ -> true)) (container:GPack.box) label get set = 
  let result = ref (get ()) in 
  let container = GPack.hbox ~packing:container#pack () in
  let entry = GEdit.entry ~packing:container#pack ~text:!result () in
  let callback _ = 
    let text =  entry#text in 
    if validator text
    then result := text
    else entry#set_text !result;
    false 
  in
  ignore (entry#event#connect#focus_out ~callback);
  ignore (GMisc.label ~line_wrap:true ~packing:container#pack ~text:label ());
  (fun () -> set !result)
      
let on_string_set (container:GPack.box) label get set = 
  let result = ref (get ()) in
  let container = GPack.hbox ~packing:container#pack () in
  let entry = GEdit.entry ~packing:container#pack ~text:!result () in
  let callback _ = result := entry#text; false in
  ignore (entry#event#connect#focus_out ~callback);
  ignore (GMisc.label ~line_wrap:true ~packing:container#pack ~text:(label^" (list)") ());
  (fun () -> set !result)

let on_string_completion 
    ?(validator=(fun _ -> true)) completions (container:GPack.box) label get set = 
  let result = ref (get()) in
  let box = GPack.hbox ~packing:container#pack () in
  let entry = string_selector completions box#pack in
  ignore (GMisc.label ~packing:box#pack ~text:label ());
  let () = entry#set_text !result in
  let callback _ = 
    let text =  entry#text in 
    if validator text
    then result := text
    else entry#set_text !result;
    false 
  in  
  ignore (entry#event#connect#focus_out ~callback);
  (fun () -> set !result)
  

let place_paned (paned:GPack.paned) factor = 
  paned#set_position (int_of_float (float (paned#max_position - paned#min_position)*.factor))
(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
