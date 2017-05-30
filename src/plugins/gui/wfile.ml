(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* -------------------------------------------------------------------------- *)
(* ---  File Chooser                                                      --- *)
(* -------------------------------------------------------------------------- *)

type filekind = [ `FILE | `DIR ]

class dialog
    ?(kind=`FILE)
    ?(title="Select File")
    ?(select="Select")
    ?parent () =
  let dialog = GWindow.dialog ~title ?parent ~modal:true () in
  let packing = dialog#vbox#pack ~expand:true in
  let action = match kind with `FILE -> `SAVE | `DIR -> `CREATE_FOLDER in
  let chooser = GFile.chooser_widget ~action ~packing () in
  object

    inherit [string] Wutil.signal as signal

    initializer
      begin
        ignore (dialog#event#connect#delete (fun _ -> true)) ;
        dialog#add_button "Cancel" `DELETE_EVENT ;
        dialog#add_button select `SELECT ;
        ignore (GMisc.label ~packing:(dialog#action_area#pack ~expand:true) ()) ;
      end

    method add_filter ~descr ~patterns =
      if kind = `FILE then
        chooser#add_filter (GFile.filter ~name:descr ~patterns ())

    method select ?dir ?file () =
      begin
        match dir , file with
        | None , None -> ignore (chooser#set_filename "")
        | None , Some path -> ignore (chooser#set_filename path)
        | Some dir , None ->
            ignore (chooser#set_current_folder dir) ;
            ignore (chooser#set_current_name "")
        | Some dir , Some file ->
            ignore (chooser#set_current_folder dir) ;
            ignore (chooser#set_current_name file)
      end ;
      let result = dialog#run () in
      dialog#misc#hide () ;
      match result with
      | `DELETE_EVENT -> ()
      | `SELECT ->
          match chooser#get_filenames with | f::_ -> signal#fire f | _ -> ()

  end

class button ?kind ?title ?select ?tooltip ?parent () =
  let box = GPack.hbox ~homogeneous:false ~spacing:0 ~border_width:0 () in
  let fld = GMisc.label ~text:"(none)" ~xalign:0.0
      ~packing:(box#pack ~expand:true) () in
  let _ = GMisc.separator `VERTICAL
      ~packing:(box#pack ~expand:false ~padding:2) ~show:true () in
  let _ = GMisc.image  ~packing:(box#pack ~expand:false) ~stock:`OPEN () in
  let button = GButton.button () in
  let dialog = new dialog ?kind ?title ?select ?parent () in
  object(self)

    inherit Wutil.gobj_widget button
    inherit! [string] Wutil.selector "" as current

    val mutable disptip = fun f ->
      match tooltip , f with
      | None , "" -> "(none)"
      | None , _ -> f
      | Some d , "" -> d
      | Some d , f -> Printf.sprintf "%s: %s" d f

    val mutable display = function
      | "" -> "(none)"
      | path -> Filename.basename path

    initializer
      begin
        button#add box#coerce ;
        button#set_focus_on_click false ;
        ignore (button#connect#clicked self#select) ;
        dialog#connect current#set ;
        Wutil.set_tooltip button tooltip ;
        current#connect
          (fun f ->
             button#misc#set_tooltip_text (disptip f) ;
             fld#set_text (display f)) ;
      end

    method set_tooltip p = disptip <- p ; fld#misc#set_tooltip_text (p current#get)
    method set_display p = display <- p ; fld#set_text (p current#get)
    method add_filter = dialog#add_filter

    method select ?dir ?file () =
      let file = match file with None -> current#get | Some f -> f in
      dialog#select ?dir ~file ()

  end
