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

(* $Id: gtk_helper.ml,v 1.29 2009-01-28 14:34:54 uid568 Exp $ *)

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

let gui_unlocked = ref true
module Lock = Hook.Make(struct end)
module Unlock = Hook.Make(struct end)
let register_locking_machinery ~lock ~unlock = 
  Lock.extend lock;
  Unlock.extend unlock

exception Found of string * string
let splitting_for_utf8 s =
  let idx = ref 0 in
  let buggy_string = "(* not a valid utf8 string *)" in
  let len = String.length s in
  try
    try 
      for i = 1 to 6 do
	idx := i;
	if len = i then raise Exit;
	let pre = String.sub s 0 (len - i) in
	let suf = String.sub s (len - i) i in
	if Glib.Utf8.validate pre then raise (Found (pre, suf))
      done;
      buggy_string, ""
    with Exit -> 
      buggy_string, ""
  with Found(pre, suf) ->
    pre, suf

let channel_redirector channel callback = 
  let cout,cin = Unix.pipe () in
  Unix.dup2 cin channel ;
  let channel = Glib.Io.channel_of_descr cout in
  let len = 80 in
  let current_partial = ref "" in
  let buf = String.create len in
  ignore (Glib.Io.add_watch channel ~prio:0 ~cond:[`IN; `HUP; `ERR] ~callback:
    begin fun cond -> 
      try if List.mem `IN cond then begin
	(* On Windows, you must use Io.read *)
	let len = Glib.Io.read channel ~buf ~pos:0 ~len in
	len >= 1 && 
	  (let full_string = !current_partial ^ String.sub buf 0 len in
	   let to_emit, c = splitting_for_utf8 full_string in
	   current_partial := c;
	   callback to_emit)
      end
      else false
      with e -> 
        ignore 
	  (callback 
             ("Channel redirector got an exception: " 
	      ^ (Printexc.to_string e))); 
       false
    end)

let log_redirector ?(flush=fun () -> ()) emit_string = 
  let output s offset length = emit_string (String.sub s offset length) in
  Log.set_output output flush

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

let mk_label ?(use_markup=false) ?xalign (container:GPack.box) label =
  let l = 
    GMisc.label ~xpad:3 ~line_wrap:true ?xalign
      ~packing:(container#pack ~expand:true ~fill:true)
  in
  if use_markup then l ~markup:label () else l ~text:label ()

type 'a chooser = 
    GPack.box -> string -> (unit -> 'a) -> ('a -> unit) -> (unit -> unit)

let on_bool ?use_markup (container:GPack.box) label get set = 
  let result = ref (get()) in
  let container = GPack.hbox ~packing:container#pack () in
  let button = 
    GButton.check_button ~packing:container#pack ~active:!result ()
  in
  ignore (mk_label ?use_markup container ~xalign:0. label);
  ignore 
    (button#connect#toggled 
       ~callback:
       (fun () -> result := button#active));
  (fun () -> set !result)

let on_bool_radio ?use_markup 
    (container:GPack.box) label_true label_false get set = 
  let result = ref (get()) in
  let frame = GBin.frame ~border_width:5 ~packing:container#pack () in
  let container = GPack.hbox ~packing:frame#add () in
  let label_true_button = 
    GButton.radio_button ~packing:container#pack ~active:!result ()
  in
  ignore (mk_label ?use_markup container ~xalign:0. label_true);
  let _label_false_button = GButton.radio_button
    ~group:label_true_button#group
    ~packing:container#pack
    ~active:(not !result)
    ()
  in
  ignore (mk_label ?use_markup container ~xalign:0. label_false);
  ignore 
    (label_true_button#connect#toggled 
       ~callback:
       (fun () -> result := label_true_button#active));
  (fun () -> set !result)

let range_selector 
    ?use_markup (container:GPack.box) ~label ~lower ~upper 
    set get =
  let container = GPack.hbox ~packing:container#pack () in
  let x = 
    GEdit.spin_button ~digits:0 ~packing:(container#pack ~padding:10)() 
  in
  x#adjustment#set_bounds
    ~lower:(float lower) ~upper:(float upper) ~step_incr:1. ();
  x#adjustment#set_value (float (get ()));
  ignore 
    (x#connect#value_changed 
       ~callback:
       (fun () -> set x#value_as_int));
  ignore (mk_label ?use_markup ~xalign:0. container label);
  (fun () -> x#adjustment#set_value (float (get ())))

let on_int 
    ?use_markup ?(lower=0) ?(upper=max_int) 
    ?(sensitive=(fun () -> true)) (container:GPack.box) label get set = 
  let result = ref (get()) in
  let make_spin ~label ~value =
    let container = GPack.hbox ~packing:container#pack () in
    let x = GEdit.spin_button ~digits:0 ~packing:container#pack () in
    x#adjustment#set_bounds 
      ~lower:(float lower) ~upper:(float upper) ~step_incr:1. ();
    x#adjustment#set_value (float value);
    ignore 
      (x#connect#value_changed 
	 ~callback:
         (fun () -> result := x#value_as_int));
    let l = mk_label ?use_markup container label in
    x,l  
  in
  let (x,l) = make_spin ~label ~value:!result in 
  (fun () ->
     l#misc#set_sensitive (sensitive ()) ;
     x#misc#set_sensitive (sensitive ()) ;
     set !result)

let on_string ?use_markup ?(validator=(fun _ -> true)) 
    (container:GPack.box) label get set = 
  let result = ref (get ()) in 
  let container = GPack.hbox ~packing:container#pack () in
  let entry = GEdit.entry ~packing:container#pack ~text:!result () in
  let callback _ = 
    let text =  entry#text in 
    if validator text then result := text else entry#set_text !result;
    false 
  in
  ignore (entry#event#connect#focus_out ~callback);
  ignore (mk_label ?use_markup ~xalign:0. container label);
  (fun () -> set !result)
      
let on_string_set ?use_markup (container:GPack.box) label get set = 
  let result = ref (get ()) in
  let container = GPack.hbox ~packing:container#pack () in
  let entry = GEdit.entry ~packing:container#pack ~text:!result () in
  let callback _ = result := entry#text; false in
  ignore (entry#event#connect#focus_out ~callback);
  ignore (mk_label ?use_markup ~xalign:0. container (label^" (list)"));
  (fun () -> set !result)

let on_string_completion 
    ?use_markup ?(validator=(fun _ -> true)) completions (container:GPack.box) 
    label get set = 
  let result = ref (get()) in
  let box = GPack.hbox ~packing:container#pack () in
  let entry = string_selector completions box#pack in
  ignore (mk_label ?use_markup ~xalign:0. box label);
  let () = entry#set_text !result in
  let callback _ = 
    let text =  entry#text in 
    if validator text then result := text else entry#set_text !result;
    false 
  in  
  ignore (entry#event#connect#focus_out ~callback);
  (fun () -> set !result)
  
let place_paned (paned:GPack.paned) factor = 
  let paned_min_pos = paned#min_position in
  let offset = 
    int_of_float (float (paned#max_position - paned_min_pos)*.factor)
  in
  paned#set_position (paned_min_pos + offset)

let old_gtk_compat f x = try f x with Not_found -> ()

module MAKE_CUSTOM_LIST
  (A:sig
     type t 
     val custom_value: Gobject.g_type -> t -> column:int -> Gobject.basic
     val column_list:GTree.column_list
   end) = 
struct
  type custom_list =
      {finfo: A.t; 
       fidx: int (* invariant: root.(fidx)==myself *) }
        
  module H = Hashtbl

  let inbound i a = i>=0 && i<Array.length a
    
  (** The custom model itself *)
  class custom_list_class column_list =
  object (self)
    inherit 
      [custom_list,custom_list,unit,unit] GTree.custom_tree_model column_list

    method custom_encode_iter cr = cr, (), ()
    method custom_decode_iter cr () () = cr

    val mutable last_idx = 0
    val mutable roots : (int,custom_list) H.t = H.create 19
    method private find_opt i = 
      try Some (H.find roots i) with Not_found -> None
    method custom_flags = [`LIST_ONLY]
    method custom_get_iter (path:Gtk.tree_path) : custom_list option =
      let indices: int array  = GTree.Path.get_indices path in
      match indices with
      | [||] ->      
          None
      | [|i|] -> self#find_opt i
      | _ -> failwith "Invalid Path of depth > 1 in a list"

    method custom_get_path (row:custom_list) : Gtk.tree_path =
      GTree.Path.create [row.fidx]

    method custom_value (t:Gobject.g_type) (row:custom_list) ~column =
      A.custom_value t row.finfo ~column

    method custom_iter_next (row:custom_list) : custom_list option =
      let nidx = succ row.fidx in
	self#find_opt nidx
	  
    method custom_iter_children (rowopt:custom_list option) :custom_list option =
      match rowopt with
      | None -> self#find_opt 0
      | Some _ -> None

    method custom_iter_has_child (_:custom_list) : bool = false

    method custom_iter_n_children (rowopt:custom_list option) : int =
      match rowopt with
      | None -> H.length roots
      | Some _ -> assert false

    method custom_iter_nth_child (rowopt:custom_list option) (n:int) 
      : custom_list option =
      match rowopt with
      | None -> self#find_opt n
      | _ -> None 

    method custom_iter_parent (_:custom_list) : custom_list option = None

    method insert (t:A.t) =
      let e = {finfo=t; fidx= last_idx } in
      self#custom_row_inserted (GTree.Path.create [last_idx]) e;
      H.add roots last_idx e;
      last_idx <- last_idx+1;

    method clear () = 
      for i=last_idx-1 downto 0 do 
      self#custom_row_deleted (GTree.Path.create [i]);
      done;
      last_idx <- 0;
      H.clear roots;


  end

  let custom_list () = 
    new custom_list_class A.column_list
end


(**  A utility class to catch exceptions and report proper error messages. *)
class type host = object
  method error: 
    'a. ?parent:GWindow.window_skel -> ('a, Format.formatter, unit) format -> 'a
  method full_protect : 
    'a. ?parent:GWindow.window_skel -> (unit -> 'a) -> 'a option
  method protect : ?parent:GWindow.window_skel -> (unit -> unit) -> unit
end

class error_manager (parent:GWindow.window_skel) : host= 
object(self:#host)
  method private error_string ?parent message =
    let w = GWindow.message_dialog
      ~message
      ~message_type:`ERROR
      ?parent
      ~buttons:GWindow.Buttons.ok
      ~title:"Error"
      ~modal:false
      ()
    in
    ignore (w#run ());
    w#destroy ()
      
  method error ?parent fmt =
    let b = Buffer.create 80 in
    let bfmt = Format.formatter_of_buffer b in
    Format.kfprintf
      (function fmt ->
	 Format.pp_print_flush fmt ();
	 let content = Buffer.contents b in
         self#error_string ?parent content)
      bfmt
      fmt
  method protect  ?(parent:GWindow.window_skel option) f = 
    ignore (self#full_protect ?parent f)

  method full_protect ?(parent:GWindow.window_skel option) f =  
    try
      Project.Undo.breakpoint ();
      let old_gui_unlocked = !gui_unlocked in
      let res = 
        Extlib.try_finally 
          ~finally:(fun () -> if old_gui_unlocked then begin
                      Unlock.apply (); 
                      gui_unlocked := true
                    end)
          (fun () -> 
             if old_gui_unlocked then begin
               Lock.apply (); 
               gui_unlocked:=false; 
             end;
             f ())
          ()
      in
      Project.Undo.clear_breakpoint ();
      Some res
    with 
    | Cmdline.Exit -> 
        None
    | Sys.Break | Db.Cancel -> 
	Project.Undo.restore ();
	self#error ?parent "Stopping current computation on user request.";
        None
    | e ->
	self#error ?parent "%s" (Log.protect e) ;
        None

end

let make_text_page (notebook:GPack.notebook) title =
  let make_tab_label (notebook:GPack.notebook) = 
    let flash_title = Format.sprintf "<i>%s</i>" title in
    let tab_label = GMisc.label ~markup:title () in
    let sw = GBin.scrolled_window
      ~vpolicy:`AUTOMATIC
      ~hpolicy:`AUTOMATIC
      ~packing:
      (fun w -> ignore (notebook#append_page ~tab_label:tab_label#coerce w))
      ()
    in
    let flash b =  
    tab_label#set_text (if b then flash_title else title);
    ignore(tab_label#set_use_markup true)
    in
    flash, sw
  in
  let flash,sw = make_tab_label notebook in
  let flash_ref = ref flash in
  let w = GText.view ~packing:sw#add () in
  let _ = w#set_editable false in
  let _ = w#misc#connect#map (fun () -> !flash_ref false) in
  let _ = w#event#connect#focus_in (fun _ -> !flash_ref false; false) in
  let _ = w#buffer#connect#changed (fun () -> !flash_ref true)
  in
  let reparent_page (notebook:GPack.notebook) = 
    let flash, sw = make_tab_label notebook in
    flash_ref := flash;
    w#misc#reparent sw#coerce
  in
  reparent_page, w

let refresh_gui () = while Glib.Main.iteration false do () done

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
