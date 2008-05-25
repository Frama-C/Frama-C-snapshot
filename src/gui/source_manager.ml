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

(** This file contains the source viewer muli-tabs widget window *)

type source_tab = {
  filename : string;
  select_line : int -> unit;
  modify_font : Pango.font_description -> unit;
}

type t = {notebook : GPack.notebook;
          tbl : (string,source_tab) Hashtbl.t}
  
let make ~packing = 
  { notebook = 
      (let nb = GPack.notebook ~scrollable:true ~show_tabs:true ~packing () in
       nb#set_enable_popup true;nb);
    tbl = Hashtbl.create 7;}
    
let set_font (t:t) fontname = 
  Hashtbl.iter (fun _ v -> v.modify_font fontname) t.tbl

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

let load_file fn w ~filename ~line =
  if Cmdline.Debug.get () > 0 then
    Format.printf "Opening file %S line %d@." filename line;
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
        let modify_font fn = window#misc#modify_font fn
        in
        modify_font fn;
        let select_line line = 
          w.notebook#goto_page page_num;
          let it = buffer#get_iter (`LINE (line-1)) in 
          buffer#place_cursor ~where:it;
          let y = if buffer#line_count < 20 then 0.23 else 0.3 in
          window#scroll_to_mark ~use_align:true ~yalign:y `INSERT
        in
        let result = { filename = filename;
                       select_line = select_line;
                       modify_font = modify_font;}
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
