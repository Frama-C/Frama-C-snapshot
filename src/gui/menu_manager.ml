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

type where =
  | Toolbar of GtkStock.id * string * string
  | Menubar of GtkStock.id option * string
  | ToolMenubar of GtkStock.id * string * string

type callback_state =
  | Unit_callback of (unit -> unit)
  | Bool_callback of (bool -> unit) * (unit -> bool)

type entry = 
    { e_where: where;
      e_callback: callback_state;
      e_sensitive: unit -> bool }

let toolbar 
    ?(sensitive=(fun _ -> true)) ~icon ~label ?(tooltip=label) callback = 
  { e_where = Toolbar (icon, label, tooltip);
    e_callback = callback;
    e_sensitive = sensitive }

let menubar ?(sensitive=(fun _ -> true)) ?icon text callback = 
  { e_where = Menubar (icon, text);
    e_callback = callback;
    e_sensitive = sensitive }

let toolmenubar 
    ?(sensitive=(fun _ -> true)) ~icon ~label ?(tooltip=label) callback = 
  { e_where = ToolMenubar (icon, label, tooltip);
    e_callback = callback;
    e_sensitive = sensitive }

type button_type =
  | BStandard of GButton.tool_button
  | BToggle of GButton.toggle_tool_button

let bt_type_as_skel = function
  | BStandard b -> (b :> GButton.tool_button_skel)
  | BToggle b -> (b :> GButton.tool_button_skel)

type menu_item_type =
  | MStandard of GMenu.menu_item
  | MCheck of GMenu.check_menu_item

let mitem_type_as_skel = function
  | MCheck m -> (m :> GMenu.menu_item_skel)
  | MStandard m -> (m :> GMenu.menu_item_skel)

class item ?menu ?menu_item ?button group = object (self)

  method menu_item =
    match menu_item with Some (MStandard m) -> Some m | _ -> None

  method check_menu_item =
    match menu_item with Some (MCheck m) -> Some m | _ -> None

  method menu_item_skel =
    match menu_item with Some m -> Some (mitem_type_as_skel m) | _ -> None

  method tool_button =
    match button with Some (BStandard b) -> Some b | _ -> None

  method toggle_tool_button =
    match button with Some (BToggle b) -> Some b | _ -> None

  method tool_button_skel =
    match button with Some b -> Some (bt_type_as_skel b) | None -> None

  method add_accelerator modifier c =
    Extlib.may
      (fun (i : GMenu.menu_item_skel) ->
        i#add_accelerator
          ~group ~flags:[ `VISIBLE ] ~modi:[ modifier ] (int_of_char c)) 
      self#menu_item_skel

  method menu: GMenu.menu option = menu

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

class menu_manager ?packing ~host:(_:Gtk_helper.host) =
  let menubar = GMenu.menu_bar ?packing () in
(*  let () = set_menu (Obj.field (Obj.repr ((menubar)#as_widget)) 1) in *)
  let factory = new GMenu.factory menubar in
  let toolbar = GButton.toolbar ?packing () in
object (self)

  val mutable first_tool_separator = None

  val analyses_menu = snd (add_submenu menubar ~pos:(-1) "_Analyses")

  val debug_item_and_menu = add_submenu menubar ~pos:(-1) "_Debug"
  val mutable debug_actions = []

  val mutable menubar_items = []
  val mutable toolbar_buttons = []
  val mutable set_active_states = []

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
    let add_item_toolbar stock label tooltip callback sensitive =
(*
      let tooltip =
        try
          if (GtkStock.Item.lookup stock).GtkStock.label = "" then Some tooltip
          else None
        with Not_found -> Some tooltip
      in
*)
      let b = match callback with
        | Unit_callback callback ->
            let b = GButton.tool_button
              ~label:tooltip ~stock ~packing:toolbar_packing ()
            in
            b#set_label label;
            ignore (b#connect#clicked ~callback);
            BStandard b
        | Bool_callback (callback, active) ->
            let b = GButton.toggle_tool_button
              ~active:(active ()) ~label:tooltip ~stock
              ~packing:toolbar_packing ()
            in
            b#set_label tooltip;
            ignore (b#connect#toggled
                      ~callback:(fun () -> callback b#get_active));
            set_active_states <-
              (fun () -> b#set_active (active ())) :: set_active_states;
            BToggle b
      in
      (bt_type_as_skel b)#set_tooltip (GData.tooltips ()) tooltip "";
      toolbar_buttons <- (b, sensitive) :: toolbar_buttons;
      b
    in
    (* Menubar *)
    let menu_pos = ref (match pos with None -> -1 | Some p -> p) in
    let container_packing w =
      container#insert ~pos:!menu_pos w;
      if !menu_pos <> -1 then incr menu_pos
    in
    let (!!) = Lazy.force in
    let menubar_packing, in_menu =
      let aux =
        lazy (* if [title] is not None, we want to create the submenu only once,
                and late enough *)
          (match title with
            | None -> container_packing, container
            | Some s ->
                let sub = snd (add_submenu container ~pos:!menu_pos s) in
                (fun w -> sub#append w), sub
          )
      in
      lazy (fst !!aux), lazy (snd !!aux)
    in
    let add_menu_separator =
      fun () ->
        if !menu_pos > 0 || (!menu_pos = -1 && container#children <> []) then
          ignore (GMenu.separator_item ~packing:container_packing ())
    in
    let add_item_menu stock_opt label callback sensitive =
      let item = match stock_opt, callback with
        | None, Unit_callback callback ->
            let mi = GMenu.menu_item ~packing:!!menubar_packing ~label () in
            ignore (mi#connect#activate callback);
            MStandard mi
        | Some stock, Unit_callback callback ->
            let image = GMisc.image ~stock () in
            let mi =
              (GMenu.image_menu_item
                 ~image ~packing:!!menubar_packing ~label ()
               :> GMenu.menu_item)
            in
            ignore (mi#connect#activate callback);
            MStandard mi
        | _, Bool_callback (callback, active) ->
            let mi = GMenu.check_menu_item
              ~packing:!!menubar_packing ~label ~active:(active ()) ()
            in
            ignore (mi#connect#activate (fun () -> callback mi#active));
            set_active_states <-
              (fun () -> mi#set_active (active ())) :: set_active_states;
            MCheck mi
      in
      menubar_items <- (item, sensitive) :: menubar_items;
      item
    in
    let extra_menu_separator () = match pos with
      | Some 0 -> add_menu_separator ()
      | _ -> ()
    in
    (* Entries *)
    let add_item { e_where = kind; e_callback = callback; e_sensitive = sensitive} =
      match kind with
      | Toolbar(stock, label, tooltip) ->
          let button = add_item_toolbar stock label tooltip callback sensitive in
          new item ~button factory#accel_group
      | Menubar(stock_opt, label) ->
          let menu_item = add_item_menu stock_opt label callback sensitive in
          new item ~menu:!!in_menu ~menu_item factory#accel_group
      | ToolMenubar(stock, label, tooltip) ->
          let button = add_item_toolbar stock label tooltip callback sensitive in
          let menu_item = add_item_menu (Some stock) label callback sensitive in
          new item ~menu:!!in_menu ~menu_item ~button factory#accel_group
    in
    let edit_menubar =
      List.exists
        (function { e_where = Menubar _ | ToolMenubar _ } -> true | _ -> false)
        entries
    in
    let edit_toolbar =
      List.exists
        (function { e_where = Toolbar _ | ToolMenubar _ } -> true | _ -> false)
        entries
    in
    if edit_menubar then add_menu_separator ();
    if edit_toolbar then add_tool_separator ();
    let entries = List.map add_item entries in
    if edit_menubar then extra_menu_separator ();
    if edit_toolbar then extra_tool_separator ();
    Array.of_list entries

  method set_sensitive b =
    List.iter
      (fun (i, f) -> (bt_type_as_skel i)#misc#set_sensitive (b && f ()))
      toolbar_buttons;
    List.iter
      (fun (i, f) -> (mitem_type_as_skel i)#misc#set_sensitive (b && f()))
      menubar_items

  (** {2 Low-level API} *)

  method factory = factory
  method menubar = menubar
  method toolbar = toolbar

  method refresh () =
    List.iter
      (fun (i, f) -> (bt_type_as_skel i)#misc#set_sensitive (f ()))
      toolbar_buttons;
    List.iter
      (fun (i, f) -> (mitem_type_as_skel i)#misc#set_sensitive (f()))
      menubar_items;
    List.iter (fun f -> f ()) set_active_states;


  initializer
  let reset () =
    self#refresh ();
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
