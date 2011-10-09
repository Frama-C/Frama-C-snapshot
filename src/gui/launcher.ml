(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
  let name = p.Parameter.name in
  let tooltip = p.Parameter.help in
  let is_set = p.Parameter.is_set in
  let highlight s = "<span foreground=\"red\">" ^ s ^ "</span>" in
  let hname = highlight name in
  match p.Parameter.accessor with
  | Parameter.Bool ({ Parameter.get = get; set = set }, None) ->
    let use_markup = is_set () in
    let name = if use_markup then hname else name in
    (* fix bts#510: a parameter [p] must be set if and only if it is set by the
       user in the launcher. In particular, it must not be reset to its old
       value if setting another parameter [p'] modifies [p] via hooking. *)
    let old = get () in
    let set r = if r <> old then set r in
    Kernel_hook.extend (on_bool ~tooltip ~use_markup box name get set);
    use_markup
  | Parameter.Bool ({ Parameter.get = get; set = set }, Some negative_name) ->
    let use_markup = is_set () in
    let name, negative_name =
      if use_markup then hname, highlight negative_name
      else name, negative_name
    in
    let old = get () in
    let set r = if r <> old then set r in
    Kernel_hook.extend
      (on_bool_radio ~tooltip ~use_markup box name negative_name get set);
    use_markup
  | Parameter.Int ({ Parameter.get = get; set = set }, range) ->
    let use_markup = is_set () in
    let name = if use_markup then hname else name in
    let lower, upper = range () in
    let old = get () in
    let set r = if r <> old then set r in
    Kernel_hook.extend
      (on_int ~tooltip ~use_markup ~lower ~upper box name get set);
    use_markup
  | Parameter.String({ Parameter.get = get; set = set }, possible_values) ->
    let use_markup = is_set () in
    let name = if use_markup then hname else name in
    let old = get () in
    let set r = if r <> old then set r in
    (match possible_values () with
    | [] ->
      Kernel_hook.extend (on_string ~tooltip ~use_markup box name get set)
    | v ->
      Kernel_hook.extend
        (on_string_completion
           ~tooltip ~use_markup ~validator:(fun s -> List.mem s v)
           v box name get set));
    use_markup
  | Parameter.String_set { Parameter.get = get; set = set }
  | Parameter.String_list { Parameter.get = get; set = set } ->
    let use_markup = is_set () in
    let name = if use_markup then hname else name in
    let old = get () in
    let set r = if r <> old then set r in
    Kernel_hook.extend (on_string_set ~tooltip ~use_markup box name get set);
    use_markup

let mk_text ~highlight text =
  let markup =
    if highlight then Format.sprintf "<span foreground=\"red\">%s</span>" text
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

let add_plugin (box:GPack.box) p =
  let expander = GBin.expander ~packing:(box#pack ~padding:2) () in
  let frame = GBin.frame ~border_width:5 ~packing:expander#add () in
  let vbox = GPack.vbox ~packing:frame#add () in
  let markup = "<b>" ^ p.Plugin.p_help ^ "</b>" in
  ignore (GMisc.label ~markup ~packing:(vbox#pack ~padding:4) ());
  let sorted_groups =
    List.sort
      (fun (s1, _) (s2, _) -> String.compare s1 s2)
      (Hashtbl.fold
         (fun l g acc -> if g = [] then acc else (l, g) :: acc)
         p.Plugin.p_parameters
         [])
  in
  let highlight =
    List.fold_left
      (fun b (l, g) -> let is_set = add_group vbox l g in b || is_set)
      false
      sorted_groups
  in
  set_expander_text
    expander
    p.Plugin.p_name
    ~tooltip:(Some p.Plugin.p_help)
    highlight

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
compile-command: "make -C ../.."
End:
*)
