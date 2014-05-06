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

module Q = Qstack.Make
  (struct
     type t = GSourceView2.source_view
     let equal x y = x == y
   end)

type t = {
  notebook : GPack.notebook ;
  views : Q.t ;
}

let make ?tab_pos ?packing () =
  let notebook = GPack.notebook
    ~scrollable:true ~show_tabs:true ?tab_pos ?packing ()
  in
  notebook#set_enable_popup true ;
  {
    notebook = notebook ;
    views = Q.create ();
  }

let get_notebook t = t.notebook


let set_current_view t n =
  if (n>=0) && (n < (Q.length t.views)) then t.notebook#goto_page n

let prepend_source_tab w titre =
  Gui_parameters.debug "prepend_source_tab";
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
  Gui_parameters.debug "get_current_view: %d" cp;
  Q.nth cp t.views

let get_current_index (t:t) =
  let cp = t.notebook#current_page in
  Gui_parameters.debug "get_current_index: %d" cp;
  cp

let delete_view (t:t) cp =
  let nb =  t.notebook in
    Gui_parameters.debug "delete_current_view - cur is page %d" cp;
    Q.remove (Q.nth cp t.views) t.views;
    nb#remove_page cp;
    let last = pred (Q.length t.views) in
    Gui_parameters.debug "Going to page (delete_current_view) %d" last;
    nb#goto_page last

(* delete within w the tab that contains window win *)
let delete_view_and_loc w win () =
  Gui_parameters.debug "delete_view_and_loc ";
  let idx = Q.idx win w.views in
  delete_view w idx

let delete_current_view t =  delete_view t t.notebook#current_page

let delete_all_views (t:t) =
  Q.iter (fun _ -> t.notebook#remove_page 0) t.views;
  Q.clear t.views

let append_view (t:t) (v:GSourceView2.source_view) =
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
  Gui_parameters.debug "Going to page (append_view) %d" next;
  Q.add_at_end v t.views;
  Gui_parameters.debug "append_view - nb pages is %d" (Q.length t.views);
  Gui_parameters.debug "append_view - current nb page is %d" nb#current_page

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
  Gui_parameters.debug "append_source_tab";
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
      (cbutton#connect#clicked 
	 ~callback:(fun () -> delete_view_and_loc w window ()));
  (* Remove default pango menu for textviews *)
    ignore (window#event#connect#button_press ~callback:
              (fun ev -> GdkEvent.Button.button ev = 3));
    Q.add_at_end window w.views;
    let last = pred (Q.length w.views) in
    (* THIS CALLS THE SWITCH_PAGE CALLBACK IMMEDIATELY! *)
    w.notebook#goto_page last;  
    window

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
