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

open Gtk_helper

module Kernel_hook = Hook.Make(struct end)

class type basic_main = object
  inherit host
  method main_window: GWindow.window
  method reset: unit -> unit
end

let run (host:basic_main) dialog () =
  ignore (host#protect ~cancelable:true ~parent:(dialog :> GWindow.window_skel)
    (fun () ->
       dialog#destroy ();
       Kernel_hook.apply ();
       !Db.Main.play ()));
  (* Even if the above operation failed, we try to reset the gui, as the
     plugins might have done something before crashing *)
  ignore (host#protect ~cancelable:false ~parent:(dialog :> GWindow.window_skel)
            host#reset);
  Kernel_hook.clear ()

let add_parameter (box:GPack.box) p =
  let name = p.Typed_parameter.name in
  let tooltip = p.Typed_parameter.help in
  let is_set = p.Typed_parameter.is_set in
  let use_markup = is_set () in
  let highlight s = "<span foreground=\"blue\">" ^ s ^ "</span>" in
  let hname = highlight name in
  (match p.Typed_parameter.accessor with
  | Typed_parameter.Bool ({ Typed_parameter.get = get; set = set }, None) ->
    let name = if use_markup then hname else name in
    (* fix bts#510: a parameter [p] must be set if and only if it is set by the
       user in the launcher. In particular, it must not be reset to its old
       value if setting another parameter [p'] modifies [p] via hooking. *)
    let old = get () in
    let set r = if r <> old then set r in
    Kernel_hook.extend (on_bool ~tooltip ~use_markup box name get set);

  | Typed_parameter.Bool
      ({ Typed_parameter.get = get; set = set }, Some negative_name) ->
    let use_markup = is_set () in
    let name, _negative_name =
      if use_markup then hname, highlight negative_name
      else name, negative_name
    in
    let old = get () in
    let set r = if r <> old then set r in
    Kernel_hook.extend
      (on_bool ~tooltip ~use_markup box name (*negative_name*) get set);

  | Typed_parameter.Int ({ Typed_parameter.get = get; set = set }, range) ->
    let use_markup = is_set () in
    let name = if use_markup then hname else name in
    let lower, upper = range () in
    let old = get () in
    let set r = if r <> old then set r in
    Kernel_hook.extend
      (on_int ~tooltip ~use_markup ~lower ~upper ~width:120 box name get set);

  | Typed_parameter.String
      ({ Typed_parameter.get = get; set = set }, possible_values) ->
    let use_markup = is_set () in
    let hname = if use_markup then hname else name in
    let old = get () in
    let widget_value = ref old in
    let w_set r = widget_value := r in
    let w_get () = !widget_value in
    (match possible_values () with
    | [] ->
      let _refresh = 
	on_string ~tooltip ~use_markup ~width:250 box hname w_get w_set
      in
      Kernel_hook.extend 
	(fun () -> if !widget_value <> old then set !widget_value)

    | v ->
      let validator s =
	let b = List.mem s v in
	if not b then Gui_parameters.error "invalid input `%s' for %s" s name;
	b
      in
      let _refresh = 
	on_string_completion
	  ~tooltip ~use_markup ~validator v box hname w_get w_set
      in
      Kernel_hook.extend 
	   (fun () -> if !widget_value <> old then set !widget_value))

  | Typed_parameter.String_set { Typed_parameter.get = get; set = set }
  | Typed_parameter.String_list { Typed_parameter.get = get; set = set } ->
    let use_markup = is_set () in
    let name = if use_markup then hname else name in
    let old = get () in
    let widget_value = ref old in
    let w_set r = widget_value := r in
    let w_get () = !widget_value in
    let _refresh = 
      on_string_set ~tooltip ~use_markup ~width:400 box name w_get w_set
    in
    Kernel_hook.extend 
      (fun () -> if !widget_value <> old then set !widget_value)
  );
  use_markup

let mk_text ~highlight text =
  let markup =
    if highlight then Format.sprintf "<span foreground=\"blue\">%s</span>" text
    else text
  in
  let label = GMisc.label ~markup () in
  label#coerce

let set_expander_text exp s ~tooltip highlight =
  let text = mk_text ~highlight s in
  Gtk_helper.do_tooltip ?tooltip text;
  exp#set_label_widget text;
  exp#set_expanded highlight

let add_group (box:GPack.box) label options =
  let box, set_expander_text =
    if label = "" then
      box, fun _ -> ()
    else
      let expander = GBin.expander ~packing:box#pack () in
      let frame = GBin.frame ~border_width:5 ~packing:expander#add () in
      GPack.vbox ~packing:frame#add (),
      set_expander_text expander ~tooltip:None label
  in
  let highlight =
    List.fold_right
      (fun p b -> let is_set = add_parameter box p in b || is_set)
      options
      false
  in
  set_expander_text highlight;
  highlight

let box_plugin p =
  let frame = GBin.frame ~border_width:5 () in
  let vbox = GPack.vbox ~packing:frame#add () in
  let markup = "<span font_weight=\"bold\">" ^
    String.capitalize p.Plugin.p_help ^ "</span>" in
  ignore (GMisc.label ~markup ~packing:(vbox#pack ~padding:15) ());
  let sorted_groups =
    List.sort
      (fun (s1, _) (s2, _) -> String.compare s1 s2)
      (Hashtbl.fold
         (fun l g acc ->
	   if g = [] then acc else (String.capitalize l, g) :: acc)
         p.Plugin.p_parameters
         [])
  in
  let highlight =
    List.fold_left
      (fun b (l, g) -> let is_set = add_group vbox l g in b || is_set)
      false
      sorted_groups
  in
  frame, highlight

(* Sort plugins, kernel first *)
let compare_plugin_name n1 n2 = 
  if n1 = "Kernel" then
    if n2 = "Kernel" then 0 else -1
  else if n2 = "Kernel" then 1
  else String.compare n1 n2


(* -------------------------------------------------------------------------- *)
(* ---                                                                    --- *)
(* -------------------------------------------------------------------------- *)

type plugin_options =
    string (* plugin name *) * bool (* highlighted *) * GBin.frame

let listview_plugins ~(packing:?from:Gtk.Tags.pack_type ->
  ?expand:bool -> ?fill:bool -> ?padding:int -> GObj.widget -> unit) plugins =
  let module Data = Indexer.Make(
    struct
      type t = plugin_options
      let compare (x,_,_) (y,_,_) = compare_plugin_name x y
    end)
  in
  let model = object(self)
    val mutable m = Data.empty
    method data = m 
    method size =  Data.size m
    method index i = Data.index i m
    method get i = Data.get i m
    method add i = m <- Data.add i m; i
    method reload = m <- Data.empty
    method coerce = (self:> plugin_options Gtk_helper.Custom.List.model)
  end in

  let scrolling_list_plugins =
    GBin.scrolled_window
      ~packing:(packing ~expand:false ~padding:5) ~vpolicy:`AUTOMATIC ~hpolicy:`NEVER ()
  in

  let w = new Gtk_helper.Custom.List.view ~headers:false model#coerce in
  scrolling_list_plugins#add_with_viewport (w#view :> GObj.widget);


  let box = GPack.vbox () in
  let scrolling_right =
    GBin.scrolled_window
      ~packing:(packing ~expand:true ~padding:5)
      ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC ()
  in
  scrolling_right#add_with_viewport (box :> GObj.widget);

  let append e = w#insert_row (model#add e) in
  let _ = w#add_column_text (*~title:"Plugins"*) [`YALIGN 0.0]
    (fun (name, highlight, _expander) ->
       let bold = [`FOREGROUND (if highlight then "blue" else "black")] in
       `TEXT name :: bold )
  in
  w#on_click (fun (_, _, expander) _col ->
                List.iter box#remove (box#all_children);
                box#pack (expander :> GObj.widget));

(*  scrolling#add_with_viewport (hbox :> GObj.widget); *)
  List.iter (fun (pname, p) ->
               let frame, highlight = box_plugin p in
               append (pname, highlight, frame);
            ) plugins;

  (w#view#get_column 0)#set_sizing `AUTOSIZE
       
   

(* -------------------------------------------------------------------------- *)
(* ---                                                                    --- *)
(* -------------------------------------------------------------------------- *)


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
  ignore (dialog#misc#connect#size_allocate
            (fun ({Gtk.width=w;Gtk.height=h}) ->
              Configuration.set "launcher_width" (Configuration.ConfInt w);
              Configuration.set "launcher_height" (Configuration.ConfInt h)));
  ignore
    (GMisc.label
       ~text:"Customize parameters, then click on `Execute'"
       ~packing:(dialog#vbox#pack ~padding:10)
       ());
  let hbox = GPack.hbox ~packing:(dialog#vbox#pack ~fill:true ~expand:true) () in
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
  let plugins = ref [] in
  Plugin.iter_on_plugins
    (fun p -> plugins := (String.capitalize p.Plugin.p_name, p) :: !plugins);
  plugins :=
    List.sort (fun (n1, _) (n2, _) -> compare_plugin_name n1 n2)!plugins;
  listview_plugins ~packing:hbox#pack !plugins;
  dialog#show ()

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
