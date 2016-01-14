(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

type tab = {
  tab_name : string ;
  tab_file : string ;
  tab_page : int ;
  tab_select : line:int -> unit ;
  tab_source_view : GSourceView2.source_view;
}

type t = {
  notebook : GPack.notebook;
  file_index : (string,tab) Hashtbl.t;
  name_index : (string,tab) Hashtbl.t;
  page_index : (int,tab) Hashtbl.t;
  mutable pages : int ;
}

let make ?tab_pos ?packing () =
  let notebook = GPack.notebook
    ~scrollable:true ~show_tabs:true ?tab_pos ?packing ()
  in
  notebook#set_enable_popup true ;
  {
    notebook = notebook ;
    file_index = Hashtbl.create 7;
    name_index = Hashtbl.create 7;
    page_index = Hashtbl.create 7;
    pages = 0 ;
  }

(* Try to convert a source file either as UTF-8 or as locale. *)
let try_convert = Gtk_helper.to_utf8

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

let clear w =
  begin
    for _i=1 to w.pages do w.notebook#remove_page 0 done ;
    w.pages <- 0 ;
    Hashtbl.clear w.file_index ;
    Hashtbl.clear w.name_index ;
    Hashtbl.clear w.page_index ;
  end

let later f = ignore (Glib.Idle.add (fun () -> f () ; false))

let select_file w filename =
  try
    let tab = Hashtbl.find w.file_index filename in
    later (fun () -> w.notebook#goto_page tab.tab_page)
  with Not_found -> ()

let select_name w title =
  try
    let tab = Hashtbl.find w.name_index title in
    later (fun () -> w.notebook#goto_page tab.tab_page)
  with Not_found -> ()

let selection_locked = ref false

let load_file w ?title ~filename ?(line=(-1)) ~click_cb () =
  Gui_parameters.debug "Opening file %S line %d" filename line ;
  let tab =
    begin
      try Hashtbl.find w.file_index filename
      with Not_found ->
        let name = match title with
          | None -> Filepath.pretty filename
          | Some s -> s
        in
        let label = GMisc.label ~text:name () in
        let sw = GBin.scrolled_window
          ~vpolicy:`AUTOMATIC
          ~hpolicy:`AUTOMATIC
          ~packing:(fun arg ->
                      ignore
                        (w.notebook#append_page ~tab_label:label#coerce arg))
          () in
        let original_source_view = Source_viewer.make ~name:"original_source"
            ~packing:sw#add ()
        in
        let window = (original_source_view :> GText.view) in
        let page_num = w.notebook#page_num sw#coerce in
        let b = Buffer.create 1024 in
        with_file filename ~f:(input_channel b) ;
        let s = try_convert (Buffer.contents b) in
        Buffer.reset b;
        let (buffer:GText.buffer) = window#buffer in
        buffer#set_text s;
        let select_line ~line =
          if !selection_locked then
            (* ignore a single call and release the lock for the next one *)
            selection_locked := false
          else
            begin
              w.notebook#goto_page page_num;
              if line >= 0 then
                let it = buffer#get_iter (`LINE (line-1)) in
                buffer#place_cursor ~where:it;
                let y = if buffer#line_count < 20 then 0.23 else 0.3 in
                window#scroll_to_mark ~use_align:true ~yalign:y `INSERT
            end
        in
        (* Ctrl+click opens the external viewer at the current line and file. *)
        ignore (window#event#connect#button_press
          ~callback:
            (fun ev ->
               (if GdkEvent.Button.button ev = 1 &&
                   List.mem `CONTROL
                     (Gdk.Convert.modifier (GdkEvent.Button.state ev))
                then
                  Gtk_helper.later
                    (fun () ->
                       try
                         let cur_page = w.notebook#current_page in
                         let tab = Hashtbl.find w.page_index cur_page in
                         let file = tab.tab_file in
                         let iter = buffer#get_iter_at_mark `INSERT in
                         let line = iter#line + 1 in
                         Gtk_helper.open_in_external_viewer ~line file
                       with Not_found ->
                         failwith (Printf.sprintf "ctrl+click cb: invalid page %d"
                                     w.notebook#current_page)
                    );
                if GdkEvent.Button.button ev = 1 then
                  Gtk_helper.later
                    (fun () ->
                       try
                         let iter = buffer#get_iter_at_mark `INSERT in
                         let line = iter#line + 1 in
                         let col = iter#line_index in
                         let offset = iter#offset in
                         let pos = {Lexing.pos_fname = filename;
                                    Lexing.pos_lnum = line;
                                    Lexing.pos_bol = offset - col;
                                    Lexing.pos_cnum = offset;} in
                         let localz =
                           Pretty_source.loc_to_localizable ~precise_col:true pos
                         in
                         click_cb localz
                       with Not_found ->
                         failwith (Printf.sprintf "click cb: invalid page %d"
                                     w.notebook#current_page)
                    );
               );
               false (* other events are processed as usual *)
            ));
        let tab = {
          tab_file = filename ;
          tab_name = name ;
          tab_select = select_line ;
          tab_page = page_num ;
          tab_source_view = original_source_view;
        } in
        w.pages <- succ page_num ;
        Hashtbl.add w.file_index filename tab ;
        Hashtbl.add w.name_index name tab ;
        Hashtbl.add w.page_index page_num tab ;
        tab
    end
  in
  (* Runs this at idle priority to let the text be displayed before. *)
  later (fun () -> tab.tab_select ~line)

let get_current_source_view w =
  try
    let tab = Hashtbl.find w.page_index w.notebook#current_page in
    tab.tab_source_view
  with Not_found ->
    failwith (Printf.sprintf "get_source_view: invalid page %d"
                w.notebook#current_page)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
