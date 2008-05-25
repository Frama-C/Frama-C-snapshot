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




(* Build a read only text view for C source code. *)

let make ~packing =
  (IFDEF SOURCEVIEW THEN
   begin
    let original_source_window = GSourceView.source_view
     ~show_line_numbers:true
     ~show_line_markers:true
     ~editable:false
     ~packing
     ()
    in
(*    let pixbuf = 
      original_source_window#misc#render_icon ~size:`MENU `DIALOG_WARNING 
    in 
    original_source_window#set_marker_pixbuf "warning" pixbuf;*)
    let original_source_buffer = original_source_window#source_buffer in
    let original_source_language_manager = GSourceView.source_languages_manager () in
    let original_lang =
      original_source_language_manager#get_language_from_mime_type "text/x-csrc"
    in
    begin match original_lang with
    | Some lang -> original_source_buffer#set_language lang
    | None -> Format.printf "No such mime type@."
    end;
    original_source_buffer#set_highlight true;
    (*      ignore (original_source_buffer#create_marker ~typ:"warning" original_source_buffer#start_iter ) ;*)
    begin try
	original_source_window#set_highlight_current_line true
      with Not_found -> ()
        (* very old gtksourceview do not have this property. *)
    end;
    original_source_window
   end
  ELSE
    GText.view ~packing ()
  END
  :> GText.view)

let buffer () =
  (IFDEF SOURCEVIEW THEN
   begin
     let original_source_buffer = GSourceView.source_buffer ()  in
     let original_source_language_manager = GSourceView.source_languages_manager () in
     let original_lang =
       original_source_language_manager#get_language_from_mime_type "text/x-csrc"
     in
     begin match original_lang with
     | Some lang -> original_source_buffer#set_language lang
     | None -> Format.printf "No such mime type@."
     end;
     original_source_buffer#set_highlight true;
     original_source_buffer
   end
  ELSE
    GText.buffer ()
  END
  :> GText.buffer)
