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

open Gtk_helper

module Parameters_hook = Hook.Make(struct end)

class type basic_main = object 
  inherit host
  method main_window: GWindow.window
  method reset: unit -> unit
end


let run (host:basic_main) dialog () =
  ignore (host#protect ~parent:(dialog :> GWindow.window_skel) 
    (fun () -> 
       dialog#destroy ();
       Parameters_hook.apply ();
       !Db.Main.play (); 
       host#reset ()));
  Parameters_hook.clear ()

let add_parameter (box:GPack.box) p = 
  let name = p.Plugin.o_name in
  let highlight s = "<span foreground=\"red\">" ^ s ^ "</span>" in
  let hname = highlight name in
  match p.Plugin.o_kind with
  | Plugin.Bool ({ Plugin.get = get; set = set; is_set = is_set }, None) ->
      let use_markup = is_set () in
      let name = if use_markup then hname else name in
      Parameters_hook.extend (on_bool ~use_markup box name get set);
      use_markup
  | Plugin.Bool ({ Plugin.get = get; set = set; is_set = is_set }, 
		 Some negative_name) ->
      let use_markup = is_set () in
      let name, negative_name = 
	if use_markup then hname, highlight negative_name 
	else name, negative_name
      in
      Parameters_hook.extend 
	(on_bool_radio ~use_markup box name negative_name get set);
      use_markup
  | Plugin.Int ({ Plugin.get = get; set = set; is_set = is_set }, range) ->
      let use_markup = is_set () in
      let name = if use_markup then hname else name in
      let lower, upper = range () in
      Parameters_hook.extend 
	(on_int ~use_markup ~lower ~upper box name get set);
      use_markup
  | Plugin.String({ Plugin.get = get; set = set; is_set = is_set }, 
		  possible_values) ->
      let use_markup = is_set () in
      let name = if use_markup then hname else name in
      (match possible_values () with
       | [] -> 
	   Parameters_hook.extend (on_string ~use_markup box name get set)
       | v ->
	   Parameters_hook.extend 
	     (on_string_completion 
		~use_markup ~validator:(fun s -> List.mem s v) 
		v box name get set));
      use_markup
  | Plugin.StringSet { Plugin.get = get; set = set; is_set = is_set } ->
      let use_markup = is_set () in
      let name = if use_markup then hname else name in
      Parameters_hook.extend (on_string_set ~use_markup box name get set);
      use_markup
  
let mk_text ~highlight box text =
  let buffer = GText.buffer ~text () in
  let ppt = `BACKGROUND_GDK (box#misc#style#bg `PRELIGHT) in
  let tag =
    if highlight then buffer#create_tag [ `FOREGROUND "red"; ppt ]
    else buffer#create_tag [ ppt ]
  in
  buffer#apply_tag tag ~start:buffer#start_iter ~stop:buffer#end_iter;
  (GText.view ~buffer () :> GObj.widget)

let set_expander_text box exp s highlight =
  let text = mk_text ~highlight box s in
  exp#set_label_widget text;
  exp#set_expanded highlight

let add_group (box:GPack.box) label options =
  let box, set_expander_text = 
    if label = "" then 
      box, fun _ -> () 
    else
      let expander = GBin.expander ~packing:box#pack () in
      let frame = GBin.frame ~border_width:5 ~packing:expander#add () in
      GPack.vbox ~packing:frame#add (), set_expander_text box expander label
  in
  let highlight = 
    List.fold_right
      (fun p b -> let is_set = add_parameter box p in b || is_set) 
      options 
      false 
  in
  set_expander_text highlight;
  highlight

let add_plugin (box:GPack.box) p = 
  let expander = GBin.expander ~packing:(box#pack ~padding:2) () in
  let frame = GBin.frame ~border_width:5 ~packing:expander#add () in
  let vbox = GPack.vbox ~packing:frame#add () in
  ignore (GMisc.label ~text:p.Plugin.p_descr ~packing:vbox#pack ());
  let highlight =
    Hashtbl.fold
      (fun l g b -> let is_set = add_group vbox l g in b || is_set) 
      p.Plugin.p_parameters
      false
  in
  set_expander_text vbox expander p.Plugin.p_name highlight

let show ?height ?width ~(host:basic_main) () = 
  let dialog =
    GWindow.dialog
      ~title:"Launching analysis"
      ~modal:true
      ~position:`CENTER_ON_PARENT
      ~allow_shrink:true
      ?width
      ?height
      ~parent:host#main_window
      ~allow_grow:true
      ()
  in
  let box = GPack.vbox () in
  let scrolling = 
    GBin.scrolled_window
      ~packing:(dialog#vbox#pack ~fill:true ~expand:true) 
      ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC () 
  in
  scrolling#add_with_viewport (box :> GObj.widget);
  ignore
    (GMisc.label
       ~text:"Customize parameters, then click on `Execute'"
       ~packing:box#pack
       ());
  (* Action buttons *)
  let buttons =
    GPack.button_box 
      `HORIZONTAL ~layout:`END ~packing:dialog#action_area#pack ()
  in
  let cancel = 
    GButton.button ~label:"Cancel" ~stock:`CANCEL ~packing:buttons#pack ()
  in
  ignore (cancel#connect#released dialog#destroy);
  let button_run = 
    GButton.button
      ~label:"Configure analysis" ~stock:`EXECUTE ~packing:buttons#pack ()
  in
  ignore (button_run#connect#released (run host dialog));
  Plugin.iter_on_plugins (add_plugin box);
  dialog#show ()

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
