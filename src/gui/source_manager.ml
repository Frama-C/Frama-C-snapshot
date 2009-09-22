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

(** This file contains the source viewer muli-tabs widget window *)

(* ABP added 1 line *)
open Pretty_source

type source_tab = {
  tab_name : string;
  select_line : int -> unit;
}


module Q = Qstack.Make(struct 
                         type t = GSourceView.source_view
                         let equal x y = x == y
                       end)


type t = {notebook : GPack.notebook;
          tbl : (string,source_tab) Hashtbl.t;
          views : Q.t; }

let get_notebook t = t.notebook


let set_current_view t n =
  if (n>=0) && (n < (Q.length t.views)) then t.notebook#goto_page n
      
let prepend_source_tab w titre =
  Parameters.debug "prepend_source_tab";
  (* insert one extra tab in the source window w, with label *)
  let label = GMisc.label ~text:titre () in
  let sw = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC
    ~hpolicy:`AUTOMATIC
    ~packing:(fun arg -> 
                ignore 
                  (w.notebook#prepend_page ~tab_label:label#coerce arg))
    ()
  in  
  let window = (Source_viewer.make ~packing:sw#add) in
    (* Remove default pango menu for textviews *)
    ignore (window#event#connect#button_press ~callback:
	      (fun ev -> GdkEvent.Button.button ev = 3));
    Q.add window w.views;
    w.notebook#goto_page 0;
    window

let get_nth_page (t:t) n = 
  let nb =  t.notebook in
    nb#get_nth_page n (* Deprecated *)

let current_page (t:t) =
  let nb =  t.notebook in
    nb#current_page

let last_page t = Q.length t.views - 1
	
(* ABP and methods to manage this memory *)
let get_current_view (t:t) =
  let nb =  t.notebook in
  let cp = nb#current_page in
  Parameters.debug "get_current_view: %d" cp;
  Q.nth cp t.views

let get_current_index (t:t) =
  let cp = t.notebook#current_page in
  Parameters.debug "get_current_index: %d" cp; 
  cp

let delete_view (t:t) cp =
  let nb =  t.notebook in
    Parameters.debug "delete_current_view - cur is page %d" cp; 
    Q.remove (Q.nth cp t.views) t.views; 
    nb#remove_page cp;
    let last = pred (Q.length t.views) in
    Parameters.debug "Going to page (delete_current_view) %d" last;
    nb#goto_page last

(* delete within w the tab that contains window win *)
let delete_view_and_loc w win () =
  Parameters.debug "delete_view_and_loc ";
  let idx = Q.idx win w.views in
  delete_view w idx 

let delete_current_view t =  delete_view t t.notebook#current_page

let delete_all_views (t:t) =
  Q.iter (fun _ -> t.notebook#remove_page 0) t.views;
  Q.clear t.views

let append_view (t:t) (v:GSourceView.source_view) =
  let nb =  t.notebook in
  let next =  Q.length t.views in
  let text = Printf.sprintf "Page %d" next in
  let label = GMisc.label ~text:text () in
  let sw = GBin.scrolled_window 
    ~vpolicy:`AUTOMATIC
    ~hpolicy:`AUTOMATIC
    ~packing:(fun arg ->
		ignore
		  (nb#append_page ~tab_label:label#coerce arg)) () in
  sw#add (v:>GObj.widget);
  nb#goto_page next;
  Parameters.debug "Going to page (append_view) %d" next;
  Q.add_at_end v t.views;
  Parameters.debug "append_view - nb pages is %d" (Q.length t.views);
  Parameters.debug "append_view - current nb page is %d" nb#current_page
     
let get_nth_view t (n:int) = Q.nth n t.views 

let enable_popup (t:t) (b:bool) =
  let nb =  t.notebook in
    nb#set_enable_popup b

let set_scrollable (t:t) (b:bool) =
  let nb =  t.notebook in
    nb#set_scrollable b

(* get length of the current source_views list *)
let length t = Q.length t.views


let append_source_tab w titre =
  Parameters.debug "append_source_tab";
  (* insert one extra tab in the source window w, with some title *)
  let composed_label = GPack.hbox  () in

  let _ = GMisc.label ~text:(titre) ~packing:composed_label#add () in

  let cbutton = GButton.button  ~packing:composed_label#add () in
    
    cbutton#set_use_stock false ;
    cbutton#set_label "X";
    cbutton#misc#set_size_request ~width:20 ~height:20 ();

  let sw = GBin.scrolled_window
    ~vpolicy:`AUTOMATIC
    ~hpolicy:`AUTOMATIC
    ~packing:(fun arg -> 
		ignore
		  (w.notebook#append_page ~tab_label:composed_label#coerce arg))
    (*
    ~packing:(fun arg -> 
                ignore 
                  (w.notebook#append_page ~tab_label:label#coerce arg)) *)
    ()
  in  
  let window = (Source_viewer.make ~packing:sw#add) in
    ignore 
	(cbutton#connect#clicked ~callback:(fun () -> delete_view_and_loc w window ()));
  (* Remove default pango menu for textviews *)
    ignore (window#event#connect#button_press ~callback:
	      (fun ev -> GdkEvent.Button.button ev = 3));
    Q.add_at_end window w.views;
    let last = pred (Q.length w.views) in
    w.notebook#goto_page last;  (* THIS CALLS THE SWITCH_PAGE CALLBACK IMMEDIATELY! *)
    window

(* ABP end of additions *)
  
let make_unpacked () = 
  { notebook = 
      (let nb = GPack.notebook ~scrollable:true ~show_tabs:true ()
       in
       nb#set_enable_popup true;nb);
    tbl = Hashtbl.create 7;
    views = Q.create ()
  }

let make ~packing = 
  { notebook = 
      (let nb = 
	 GPack.notebook ~scrollable:true ~show_tabs:true ~packing ()
       in
       nb#set_enable_popup true;nb);
    tbl = Hashtbl.create 7;
    views = Q.create ()}
    
(* Try to convert a source file either as UTF-8 or as locale. *)
let try_convert s =
  try
    if Glib.Utf8.validate s then s else
      Glib.Convert.locale_to_utf8 s
  with Glib.Convert.Error _ -> 
    try 
      Glib.Convert.convert_with_fallback 
        ~fallback:"#neither UTF-8 nor locale nor ISO-8859-15#"
        ~to_codeset:"UTF-8"
        ~from_codeset:"ISO_8859-15"
        s
    with Glib.Convert.Error _ as e -> Printexc.to_string e

let input_channel b ic =
  let buf = String.create 1024 and len = ref 0 in
  while len := input ic buf 0 1024; !len > 0 do
    Buffer.add_substring b buf 0 !len
  done


let with_file name ~f =
  try 
  let ic = open_in_gen [Open_rdonly] 0o644 name in
  try f ic; close_in ic with _exn ->
    close_in ic (*; !flash_info ("Error: "^Printexc.to_string exn)*)
 with _exn -> ()

let load_file w ~filename ~line =
  Parameters.debug  "Opening file %S line %d" filename line;
  let filename_info = 
    begin
      try Hashtbl.find w.tbl filename  
      with Not_found -> 
        let label = GMisc.label ~text:filename () in
        
        let sw = GBin.scrolled_window
	  ~vpolicy:`AUTOMATIC
	  ~hpolicy:`AUTOMATIC
          ~packing:(fun arg -> 
                      ignore 
                        (w.notebook#append_page ~tab_label:label#coerce arg))
          ()
        in  
        let window = ((Source_viewer.make 
                         ~packing:sw#add)
                      :> GText.view) 
        in
        (*
          ignore 
          (window#buffer#connect#mark_set 
          ~callback:(fun it mark -> 
          if Gobject.get_oid mark == Gobject.get_oid (GtkText.Buffer.get_insert window#buffer#as_buffer) then 
          Format.printf "File:%s Line:%d@."
          filename
          (succ it#line)
          ));*)
        let page_num = w.notebook#page_num sw#coerce in
        let b = Buffer.create 1024 in
        with_file filename ~f:(input_channel b);
        let s = try_convert (Buffer.contents b) in
        Buffer.reset b;
        let (buffer:GText.buffer) = window#buffer in
        buffer#set_text s;
        let select_line line = 
          w.notebook#goto_page page_num;
          let it = buffer#get_iter (`LINE (line-1)) in 
          buffer#place_cursor ~where:it;
          let y = if buffer#line_count < 20 then 0.23 else 0.3 in
          window#scroll_to_mark ~use_align:true ~yalign:y `INSERT
        in
        let result = { tab_name = filename; select_line = select_line;}
        in
        Hashtbl.add w.tbl filename result ;
        result
    end 
  in
  (* Runs this at idle priority to let the text be displayed before. *)
  ignore (Glib.Idle.add 
            (fun () -> filename_info.select_line line;false(*do it once only*)))
    

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
