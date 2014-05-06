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


(* Build a read only text view for C source code. *)

let set_language_to_C (buffer:GSourceView2.source_buffer)  =
  let original_source_language_manager =
    GSourceView2.source_language_manager ~default:true
  in
  let original_lang =
    original_source_language_manager#guess_language
      ~content_type:"text/x-csrc" ()
  in
  begin match original_lang with
  | Some _ -> buffer#set_language original_lang
  | None -> Gui_parameters.warning "Mime type 'text/x-csrc' not found"
  end;
  buffer#set_highlight_syntax true

let make ~packing =
(*  let d = GWindow.font_selection_dialog ~title:"tutu" ~show:true () in
  d#selection#set_preview_text
    (Format.sprintf "%s %s %s %s"
       Utf8_logic.forall Utf8_logic.exists Utf8_logic.eq Utf8_logic.neq) ;
*)
  let original_source_window =
    GSourceView2.source_view
      ~show_line_numbers:true
      ~editable:false
      ~packing
      ()
  in
(*  let pixbuf =
    original_source_window#misc#render_icon ~size:`MENU `DIALOG_WARNING
  in
  original_source_window#set_marker_pixbuf "warning" pixbuf; *)
  original_source_window#misc#modify_font_by_name "Monospace";
  original_source_window#misc#set_name "source";
  let original_source_buffer = original_source_window#source_buffer in
  set_language_to_C original_source_buffer;
(*
  ignore (original_source_buffer#create_marker ~typ:"warning" original_source_buffer#start_iter ) ;*)
  begin try
    original_source_window#set_highlight_current_line true
  with Not_found -> ()
    (* very old gtksourceview do not have this property. *)
  end;
  original_source_window


let buffer () =
  let original_source_buffer = GSourceView2.source_buffer ()  in
  set_language_to_C original_source_buffer;
  original_source_buffer
