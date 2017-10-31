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

class type t = Wutil.widget
class type widget = Wutil.widget

class type action =
  object
    inherit widget
    method set_tooltip : string -> unit
  end

class type ['a] signal =
  object
    method fire : 'a -> unit
    method set_enabled : bool -> unit
    method lock : (unit -> unit) -> unit
    method connect : ('a -> unit) -> unit
    method on_check : 'a -> (bool -> unit) -> unit
    method on_value : 'a -> (unit -> unit) -> unit
    method on_event : (unit -> unit) -> unit
  end

class type ['a] selector =
  object
    inherit ['a] signal
    method set : 'a -> unit
    method get : 'a
    method send : ('a -> unit) -> unit -> unit
  end

(* -------------------------------------------------------------------------- *)
(* ---  Labels                                                            --- *)
(* -------------------------------------------------------------------------- *)

open Wutil

type align = [`Left | `Right | `Center]
type style = [`Label | `Descr | `Code | `Title]
type color = [ GDraw.color | `NORMAL ]

let xalign = function `Left -> 0.0 | `Right -> 1.0 | `Center -> 0.5

class label ?(style=`Label) ?(align=`Left) ?width ?text () =
  let w = GMisc.label ?text ~xalign:(xalign align) () in
  object
    inherit Wutil.gobj_widget w
    val mutable fg = None
    val mutable bg = None

    method set_fg (c : color) =
      match fg , c with
      | None , `NORMAL -> ()
      | Some c0 , `NORMAL ->
          w#misc#modify_fg [ `NORMAL , `COLOR c0 ]
      | None , (#GDraw.color as c) ->
          fg <- Some (w#misc#style#fg `NORMAL) ;
          w#misc#modify_fg [ `NORMAL , c ]
      | Some _ , (#GDraw.color as c) ->
          w#misc#modify_fg [ `NORMAL , c ]

    method set_bg (c : color) =
      match bg , c with
      | None , `NORMAL -> ()
      | Some c0 , `NORMAL ->
          w#misc#modify_bg [ `NORMAL , `COLOR c0 ]
      | None , (#GDraw.color as c) ->
          bg <- Some (w#misc#style#bg `NORMAL) ;
          w#misc#modify_bg [ `NORMAL , c ]
      | Some _ , (#GDraw.color as c) ->
          w#misc#modify_bg [ `NORMAL , c ]

    initializer
      Wutil.on width w#set_width_chars ;
      match style with
      | `Label -> ()
      | `Code -> set_monospace w
      | `Title -> set_bold_font w
      | `Descr ->
          w#set_single_line_mode false ;
          w#set_line_wrap true ;
          w#set_justify `LEFT ;
          set_small_font w

    method set_text = w#set_text
    method set_tooltip msg =
      Wutil.set_tooltip w (if msg = "" then None else Some msg)
  end

(* -------------------------------------------------------------------------- *)
(* ---  Icons                                                             --- *)
(* -------------------------------------------------------------------------- *)

type icon = [ GtkStock.id | `Share of string | `None ]

let default_icon =
  let xpm =
    [| "12 12 2 1";
       ". c #ffffff";
       "# c #000000";
       "############";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "#..........#";
       "############"|]
  in once (fun () -> GdkPixbuf.from_xpm_data xpm)

let pixbufs = Hashtbl.create 63
let shared_icon (f:string) =
  try Hashtbl.find pixbufs f
  with Not_found ->
    let file = Printf.sprintf "%s/%s" !Wutil.share f in
    let pixbuf =
      try GdkPixbuf.from_file file
      with Glib.GError _ ->
        Wutil.warning "Icon '%s' not found" file ;
        default_icon ()
    in Hashtbl.add pixbufs f pixbuf ; pixbuf

let gimage = function 
  | `None -> GMisc.image ()
  | `Share f -> GMisc.image ~pixbuf:(shared_icon f) ()
  | #GtkStock.id as stock -> GMisc.image ~stock ()

class image (icn:icon) =
  let img = gimage icn in
  object
    inherit gobj_widget img
    method set_icon (icn:icon) =
      match icn with
      | `None -> img#clear ()
      | `Share f -> img#set_pixbuf (shared_icon f)
      | #GtkStock.id as id -> img#set_stock id
  end

(* -------------------------------------------------------------------------- *)
(* ---  Buttons                                                           --- *)
(* -------------------------------------------------------------------------- *)

class button_skel ?align ?(icon=`None) ?tooltip (button:GButton.button_skel) =
  object(self)
    val mutable images = []
    initializer
      begin
        self#set_icon icon ;
        Wutil.on align (fun a -> button#set_xalign (xalign a)) ;
        set_tooltip button tooltip ;
        button#misc#set_can_focus false ;
        button#set_focus_on_click false ;
      end
    inherit gobj_action button
    method set_label = button#set_label
    method set_border e = button#set_relief (if e then `NORMAL else `NONE)
    method set_icon (i:icon) =
      match i with
      | `None -> button#unset_image ()
      | #icon as icn ->
          let image =
            try List.assoc icn images
            with Not_found ->
              let img = gimage icn in
              images <- (icn,img)::images ; img
          in button#set_image image#coerce
  end

class button ?align ?icon ?label ?(border=true) ?tooltip () =
  let relief = if border then `NORMAL else `NONE in
  let button = GButton.button ?label ~relief ~show:true () in
  object(self)
    inherit [unit] signal as s
    inherit! button_skel ?align ?icon ?tooltip (button :> GButton.button_skel) as b
    method! set_enabled e = s#set_enabled e ; b#set_enabled e
    method default = button#grab_default
    initializer
      ignore (button#connect#clicked self#fire)
  end

(* -------------------------------------------------------------------------- *)
(* ---  On/Off Buttons                                                    --- *)
(* -------------------------------------------------------------------------- *)

class checkbox ~label ?tooltip () =
  let button = GButton.check_button ~label ~show:true () in
  object
    inherit [bool] selector false as s
    inherit! gobj_action button as b
    method! set_enabled e = s#set_enabled e ; b#set_enabled e
    method! set a = s#set a ; button#set_active a
    initializer
      begin
        set_tooltip button tooltip ;
        ignore (button#connect#clicked (fun () -> s#set button#active)) ;
      end
  end

let toggle_icon_warning = ref true

class toggle ?align ?icon ?label ?(border=true) ?tooltip () =
  let relief = if border then `NORMAL else `NONE in
  let button = GButton.toggle_button ?label ~relief ~show:true () in
  object
    inherit [bool] selector false as s
    inherit! button_skel ?align ?icon ?tooltip (button :> GButton.button_skel) as b
    method! set_enabled e = s#set_enabled e ; b#set_enabled e
    method! set = button#set_active
    method! set_icon icn =
      if icn <> `None && !toggle_icon_warning then
        ( Wutil.warning "[Widget] Icon may not appear on toggle buttons" ;
          toggle_icon_warning := false ) ;
      b#set_icon icn
    initializer
      ignore (button#connect#clicked (fun () -> s#set button#active))
  end

class switch ?tooltip () =
  let pix_on = shared_icon "switch-on.png" in
  let pix_off = shared_icon "switch-off.png" in
  let evt = GBin.event_box () in
  let img = GMisc.image ~pixbuf:pix_on ~packing:evt#add () in
  object(self)
    inherit [bool] selector false as s
    inherit! gobj_action evt as b
    method! set_enabled e = s#set_enabled e ; b#set_enabled e
    method! set a = s#set a ; img#set_pixbuf (if a then pix_on else pix_off)
    initializer
      begin
        set_tooltip evt tooltip ;
        ignore (evt#event#connect#button_release
                  (fun _evt -> self#set (not s#get) ; false)) ;
      end
  end

(* -------------------------------------------------------------------------- *)
(* ---  Button Group                                                      --- *)
(* -------------------------------------------------------------------------- *)

(* only used inside groups -> not exported to API *)
class radio_group ~label ?tooltip () =
  let button = GButton.radio_button ~label ~show:true () in
  object
    inherit [bool] selector false as s
    inherit! gobj_action button
    method! set e = s#set e ; if e then button#set_active true
    method group = function
      | None -> Some button#group
      | (Some g) as sg -> button#set_group g ; sg
    initializer
      begin
        set_tooltip button tooltip ;
        ignore (button#connect#clicked (fun () -> s#set button#active)) ;
      end
  end

(* only used inside groups -> not exported to API *)
class toggle_group ?label ?icon ?tooltip () =
  let button = GButton.button ?label ~show:true ~relief:`NONE () in
  object
    inherit [bool] selector false as s
    inherit! button_skel ?icon ?tooltip (button :> GButton.button_skel) as b
    method! set_enabled e = s#set_enabled e ; b#set_enabled e
    method! set a = s#set a ; button#set_relief (if a then `NORMAL else `NONE)
    initializer ignore (button#connect#clicked (fun () -> s#set (not s#get)))
  end

class ['a] group (default : 'a) =
  object(self)
    inherit ['a] selector default
    val mutable cases : (bool selector * 'a) list = []
    val mutable group = None
    initializer self#connect
        (fun v -> List.iter
            (fun (w,v0) -> w#set (v=v0)) cases)

    method private add_case (w : bool selector) (v : 'a) =
      begin
        w#set ( v = self#get ) ;
        w#connect (fun e -> if e then self#set v) ;
        cases <- (w,v) :: cases ;
      end

    method add_toggle ?label ?icon ?tooltip ~value () =
      let toggle = new toggle_group ?label ?icon ?tooltip () in
      self#add_case (toggle :> bool selector) value ;
      (toggle :> widget)

    method add_radio ~label ?tooltip ~value () =
      let radio = new radio_group ~label ?tooltip () in
      self#add_case (radio :> bool selector) value ;
      group <- radio#group group ;
      (radio :> widget)

    method! set_enabled e =
      List.iter (fun (w,_) -> w#set_enabled e) cases

  end

(* -------------------------------------------------------------------------- *)
(* ---  Spinner                                                           --- *)
(* -------------------------------------------------------------------------- *)

class spinner ?min ?max ?(step=1) ~value ?tooltip () =
  let b = GEdit.spin_button ~digits:0 () in
  object
    inherit [int] selector value as s
    inherit! gobj_action b
    method! set_enabled e = s#set_enabled e ; b#misc#set_sensitive e
    method! set a = s#set a ; b#set_value (float value)
    method set_min n = b#adjustment#set_bounds ~lower:(float n) ()
    method set_max n = b#adjustment#set_bounds ~upper:(float n) ()
    initializer
      begin
        set_tooltip b tooltip ;
        let fmap v = function None -> v | Some x -> float x in
        b#adjustment#set_bounds
          ~lower:(fmap (float min_int) min) ~upper:(fmap (float max_int) max)
          ~step_incr:(float step) () ;
        b#set_value (float value) ;
        let callback () = s#set b#value_as_int in
        ignore (b#connect#value_changed ~callback) ;
      end
  end

(* -------------------------------------------------------------------------- *)
(* ---  PopDown                                                           --- *)
(* -------------------------------------------------------------------------- *)

let render_options opt a =
  try List.assoc a opt
  with Not_found -> "<unknown>"

class ['a] menu ~default ?(options=[]) ?render ?items () =
  let strings = List.map snd options in
  let (cmb,(model,_)) as combo = GEdit.combo_box_text ~strings ~wrap_width:1 () in
  object(self)

    inherit gobj_action cmb as widget
    inherit! ['a] selector default as select

    initializer
      begin
        on render self#set_render ;
        on items self#set_items ;
      end

    val mutable printer = render_options options
    val mutable values = Array.of_list (List.map fst options)

    method set_options opt =
      printer <- render_options opt ;
      self#set_items (List.map fst opt)

    method set_render p = printer <- p

    method! set_enabled e =
      select#set_enabled e ; widget#set_enabled e

    method get_items = Array.to_list values

    method set_items xs =
      begin
        values <- Array.of_list xs ; model#clear () ;
        Array.iter (fun x -> GEdit.text_combo_add combo (printer x)) values ;
        let e = select#get in
        self#lock (fun () ->
            Array.iteri (fun i x -> if x=e then cmb#set_active i) values) ;
      end

    method private clicked n =
      if 0 <= n && n < Array.length values then
        select#set values.(n)

    method! set x =
      begin
        select#set x ;
        Array.iteri (fun i e -> if x=e then cmb#set_active i) values ;
      end

    initializer
      ignore (cmb#connect#notify_active self#clicked)

  end

(* -------------------------------------------------------------------------- *)
(* ---  Popup Menu                                                        --- *)
(* -------------------------------------------------------------------------- *)

class popup () =
  let menu = GMenu.menu () in
  object

    val mutable empty = true
    val mutable separator = false

    method clear =
      begin
        List.iter menu#remove menu#children ;
        empty <- true ;
        separator <- false ;
      end

    method add_separator = separator <- true

    method add_item ~label ~callback =
      if not empty && separator then
        ignore (GMenu.separator_item ~packing:menu#append ()) ;
      let item = GMenu.menu_item ~label ~packing:menu#append () in
      ignore (item#connect#activate ~callback) ;
      empty <- false ; separator <- false

    method run () =
      if not empty then
        let time = GMain.Event.get_current_time () in
        menu#popup ~button:3 ~time

  end
