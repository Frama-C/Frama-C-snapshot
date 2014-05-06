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

type tab = {
  tab_name : string ;
  tab_file : string ;
  tab_page : int ;
  tab_select : line:int -> unit ;
}

type t = {
  notebook : GPack.notebook;
  file_index : (string,tab) Hashtbl.t;
  name_index : (string,tab) Hashtbl.t;
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

let load_file w ?title ~filename ?(line=(-1)) () =
  Gui_parameters.debug "Opening file %S line %d" filename line ;
  let tab =
    begin
      try Hashtbl.find w.file_index filename
      with Not_found ->
        let name = match title with None -> filename | Some s -> s in
        let label = GMisc.label ~text:name () in
        let sw = GBin.scrolled_window
          ~vpolicy:`AUTOMATIC
          ~hpolicy:`AUTOMATIC
          ~packing:(fun arg ->
                      ignore
                        (w.notebook#append_page ~tab_label:label#coerce arg))
          () in
        let window = ((Source_viewer.make ~packing:sw#add) :> GText.view) in
        let page_num = w.notebook#page_num sw#coerce in
        let b = Buffer.create 1024 in
        with_file filename ~f:(input_channel b) ;
        let s = try_convert (Buffer.contents b) in
        Buffer.reset b;
        let (buffer:GText.buffer) = window#buffer in
        buffer#set_text s;
        let select_line ~line =
          w.notebook#goto_page page_num;
          if line >= 0 then
            let it = buffer#get_iter (`LINE (line-1)) in
            buffer#place_cursor ~where:it;
            let y = if buffer#line_count < 20 then 0.23 else 0.3 in
            window#scroll_to_mark ~use_align:true ~yalign:y `INSERT
        in
        let tab = {
          tab_file = filename ;
          tab_name = name ;
          tab_select = select_line ;
          tab_page = page_num ;
        } in
        w.pages <- succ page_num ;
        Hashtbl.add w.file_index filename tab ;
        Hashtbl.add w.name_index name tab ;
        tab
    end
  in
  (* Runs this at idle priority to let the text be displayed before. *)
  later (fun () -> tab.tab_select ~line)

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
