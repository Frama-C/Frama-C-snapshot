(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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

type where =
  | Toolbar of GtkStock.id * string
  | Menubar of GtkStock.id option * string
  | ToolMenubar of GtkStock.id * string

type entry = where * (unit -> unit)

class item ?menu_item ?tool_button group = object
  method menu_item: GMenu.menu_item option = menu_item
  method tool_button: GButton.tool_button option = tool_button
  method add_accelerator modifier c =
    Extlib.may
      (fun i -> 
	 (* unfortunatly full type annotation required *)
	 let f: group:Gtk.accel_group ->
           ?modi:Gdk.Tags.modifier list ->
           ?flags:Gtk.Tags.accel_flag list ->
           Gdk.keysym -> unit = 
	   i#add_accelerator
	 in
	 f ~group ~flags:[ `VISIBLE ] ~modi:[ modifier ] (int_of_char c))
      menu_item

end

(* the analyses-menu will be at the last position of the menubar *)
let add_submenu container ~pos label =
  let item = 
    let packing item = container#insert item ~pos in
    GMenu.menu_item ~use_mnemonic:true ~packing ~label () 
  in
  let m = GMenu.menu () in
  item#set_submenu m;
  item, m

(*
external set_menu :  Obj.t -> unit = "ige_mac_menu_set_menu_bar"
*)

class menu_manager ?packing () = 
  let menubar = GMenu.menu_bar ?packing () in
(*  let () = set_menu (Obj.field (Obj.repr ((menubar)#as_widget)) 1) in *)
  let factory = new GMenu.factory menubar in
  let toolbar = GButton.toolbar ?packing () in
object (self)

  val mutable first_tool_separator = None

  val analyses_menu = snd (add_submenu menubar ~pos:(-1) "_Analyses")

  val debug_item_and_menu = add_submenu menubar ~pos:(-1) "_Debug"
  val mutable debug_actions = []

  (** {2 API for plug-ins} *)

  method add_plugin ?title = self#add_entries ?title analyses_menu

  method add_debug ?title ?(show=fun () -> true) entries = 
    let items = self#add_entries ?title (snd debug_item_and_menu) entries in
    let action item =
      if show () then begin
	Extlib.may (fun i -> i#misc#show ()) item#menu_item;
	Extlib.may (fun i -> i#misc#show ()) item#tool_button
      end else begin
	Extlib.may (fun i -> i#misc#hide ()) item#menu_item;
	Extlib.may (fun i -> i#misc#hide ()) item#tool_button
      end
    in
    let l = List.rev debug_actions in
    Array.iter
      (fun i -> 
	 action i; 
	 debug_actions <- (fun () -> action i) :: l) 
      items;
    items

  (** {2 High-level API} *)

  method add_menu ?(pos=List.length menubar#children - 2) s = 
    add_submenu ~pos factory#menu s

  method add_entries ?title ?pos container entries =
    (* Toolbar *)
    let toolbar_pos = 
      (* The first group will be at the end of the toolbar. 
	 By default, add all the others just before this very first group. *)
      ref (match pos, first_tool_separator with
	   | None, None -> 0
	   | None, Some sep -> max 0 (toolbar#get_item_index sep)
	   | Some p, _ -> p)
    in
    let toolbar_packing w = 
      toolbar#insert ~pos:!toolbar_pos w;
      incr toolbar_pos
    in
    let add_tool_separator () =
      if !toolbar_pos > 0 || first_tool_separator = None then begin
	let s = GButton.separator_tool_item ~packing:toolbar_packing () in
	match first_tool_separator with
	| None -> first_tool_separator <- Some s
	| Some _ -> ()
      end
    in
    let extra_tool_separator () = match pos with
      | Some 0 -> add_tool_separator ()
      | _ -> ()
    in
    let add_item_toolbar stock tooltip callback =
      let b = GButton.tool_button ~stock ~packing:toolbar_packing () in
      b#set_tooltip (GData.tooltips ()) tooltip "";
      ignore (b#connect#clicked ~callback);
      b
    in
    (* Menubar *)
    let menu_pos = ref (match pos with None -> -1 | Some p -> p) in
    let menubar_packing w = 
      let pos = !menu_pos in
      (match title with
       | None -> container#insert ~pos w
       | Some s -> (snd (add_submenu container ~pos s))#append w);
      if pos <> -1 then incr menu_pos
    in
    let add_menu_separator = 
      let first = ref true in
      fun () -> 
	if !menu_pos > 0 || (!menu_pos = -1 && container#children <> []) 
	then begin
	  ignore (GMenu.separator_item ~packing:menubar_packing ());
	  first := false
	end
    in
    let add_item_menu stock_opt label callback =
      let item = match stock_opt with
	| None -> GMenu.menu_item ~packing:menubar_packing ~label ()
	| Some stock -> 
	    let image = GMisc.image ~stock () in
	    (GMenu.image_menu_item ~image ~packing:menubar_packing ~label ()
	     :> GMenu.menu_item)
      in
      ignore (item#connect#activate callback);
      item
    in
    let extra_menu_separator () = match pos with
      | Some 0 -> add_menu_separator ()
      | _ -> ()
    in
    (* Entries *)
    let add_item (kind, callback) = 
      match kind with
      | Toolbar(stock, tooltip) -> 
	  let tool_button = add_item_toolbar stock tooltip callback in
	  new item ~tool_button factory#accel_group
      | Menubar(stock_opt, label) -> 
	  let menu_item = add_item_menu stock_opt label callback in
	  new item ~menu_item factory#accel_group
      | ToolMenubar(stock, label) -> 
	  let tool_button = add_item_toolbar stock label callback in
	  let menu_item = add_item_menu (Some stock) label callback in
	  new item ~menu_item ~tool_button factory#accel_group
    in
    let edit_menubar = 
      List.exists
	(function (Menubar _, _) | (ToolMenubar _, _) -> true | _ -> false)
	entries
    in
    let edit_toolbar = 
      List.exists
	(function (Toolbar _, _) | (ToolMenubar _, _) -> true | _ -> false)
	entries
    in
    if edit_menubar then add_menu_separator ();
    if edit_toolbar then add_tool_separator ();
    let entries = List.map add_item entries in
    if edit_menubar then extra_menu_separator ();
    if edit_toolbar then extra_tool_separator ();
    Array.of_list entries

  method set_sensitive b =
    List.iter (fun i -> i#misc#set_sensitive b) toolbar#children;
    List.iter (fun i -> i#misc#set_sensitive b) menubar#children

  (** {2 Low-level API} *)

  method factory = factory
  method menubar = menubar
  method toolbar = toolbar

  initializer
  let reset () =
    List.iter (fun f -> f ()) debug_actions;
    let debug_item = fst debug_item_and_menu in
    if !Plugin.positive_debug_ref > 0 then debug_item#misc#show ()
    else debug_item#misc#hide ()
  in
  reset ();
  Db.Main.extend reset

end

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
