(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

class type widget =
  object
    method set_enabled : bool -> unit
    method coerce : GObj.widget
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
type style = [`Label | `Descr | `Title]

let xalign = function `Left -> 0.0 | `Right -> 1.0 | `Center -> 0.5

class label ?(style=`Label) ?text ?(align=`Left) () =
  let w = GMisc.label ?text ~xalign:(xalign align) () in
  object
    inherit Wutil.coerce w
    initializer match style with
      | `Label -> ()
      | `Descr -> w#set_line_wrap true ; set_small_font w
      | `Title -> set_bold_font w
    method set_text = w#set_text
  end

(* -------------------------------------------------------------------------- *)
(* ---  Icons                                                             --- *)
(* -------------------------------------------------------------------------- *)

type icon = [ GtkStock.id | `Share of string ]

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

let gimage (icon:icon) = match icon with
  | `Share f -> GMisc.image ~pixbuf:(shared_icon f) ()
  | #GtkStock.id as stock -> GMisc.image ~stock ()

(* -------------------------------------------------------------------------- *)
(* ---  Buttons                                                           --- *)
(* -------------------------------------------------------------------------- *)

class button_skel ?icon ?tooltip (button:GButton.button_skel) =
  object(self)
    val mutable images = []
    initializer
      begin
        self#set_icon icon ;
        set_tooltip button tooltip ;
        button#misc#set_can_focus false ;
        button#set_focus_on_click false ;
      end
    inherit coerce button
    method set_label = button#set_label
    method set_relief e = button#set_relief (if e then `NORMAL else `NONE)
    method set_icon = function
      | None -> button#unset_image ()
      | Some icn ->
          let image =
            try List.assoc icn images
            with Not_found ->
              let img = gimage icn in
              images <- (icn,img)::images ; img
          in button#set_image image#coerce
  end

class button ?label ?icon ?tooltip () =
  let button = GButton.button ?label ~show:true () in
  object(self)
    inherit [unit] signal as s
    inherit! button_skel ?icon ?tooltip (button :> GButton.button_skel) as b
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
    inherit! coerce button as b
    method! set_enabled e = s#set_enabled e ; b#set_enabled e
    method! set a = s#set a ; button#set_active a
    initializer
      begin
        set_tooltip button tooltip ;
        ignore (button#connect#clicked (fun () -> s#set button#active)) ;
      end
  end

(* only used inside groups -> not exported to API *)
class radio ~label ?tooltip () =
  let button = GButton.radio_button ~label ~show:true () in
  object
    inherit [bool] selector false as s
    inherit! coerce button
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

class toggle ?label ?icon ?tooltip () =
  let button = GButton.button ?label ~show:true ~relief:`NONE () in
  object
    inherit [bool] selector false as s
    inherit! button_skel ?icon ?tooltip (button :> GButton.button_skel) as b
    method! set_enabled e = s#set_enabled e ; b#set_enabled e
    method! set a = s#set a ; button#set_relief (if a then `NORMAL else `NONE)
    initializer ignore (button#connect#clicked (fun () -> s#set (not s#get)))
  end

class switch ?tooltip () =
  let pix_on = shared_icon "switch-on.png" in
  let pix_off = shared_icon "switch-off.png" in
  let evt = GBin.event_box () in
  let img = GMisc.image ~pixbuf:pix_on ~packing:evt#add () in
  object(self)
    inherit [bool] selector false as s
    inherit! coerce evt as b
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

class hbox (widgets : widget list) =
  let box = GPack.hbox ~homogeneous:true ~spacing:0 ~border_width:0 () in
  object
    initializer List.iter (fun w -> box#add w#coerce) widgets
    method set_enabled e = List.iter (fun w -> w#set_enabled e) widgets
    method coerce = box#coerce
  end

class vbox (widgets : widget list) =
  let box = GPack.vbox ~homogeneous:true ~spacing:0 ~border_width:0 () in
  object
    initializer List.iter (fun w -> box#add w#coerce) widgets
    method set_enabled e = List.iter (fun w -> w#set_enabled e) widgets
    method coerce = box#coerce
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
      let toggle = new toggle ?label ?icon ?tooltip () in
      self#add_case (toggle :> bool selector) value ;
      (toggle :> widget)

    method add_radio ~label ?tooltip ~value () =
      let radio = new radio ~label ?tooltip () in
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
    inherit! coerce b
    method! set_enabled e = s#set_enabled e ; b#misc#set_sensitive e
    method! set a = s#set a ; b#set_value (float value)
    initializer
      begin
        set_tooltip b tooltip ;
        let fmap = function None -> None | Some x -> Some (float x) in
        b#adjustment#set_bounds
          ?lower:(fmap min) ?upper:(fmap max)
          ~step_incr:(float step) () ;
        b#set_value (float value) ;
        let callback () = s#set b#value_as_int in
        ignore (b#connect#value_changed ~callback) ;
      end
  end

(* -------------------------------------------------------------------------- *)
(* ---  PopDown                                                           --- *)
(* -------------------------------------------------------------------------- *)

class ['a] menu ~default ~render ?(items=[]) () =
  let strings = List.map render items in
  let (cmb,(model,_)) as combo = GEdit.combo_box_text ~strings ~wrap_width:1 () in
  object(self)

    inherit coerce cmb as widget
    inherit! ['a] selector default as select

    val mutable items = Array.of_list items

    method! set_enabled e =
      select#set_enabled e ; widget#set_enabled e

    method get_items = Array.to_list items

    method set_items xs =
      begin
        items <- Array.of_list xs ; model#clear () ;
        Array.iter (fun x -> GEdit.text_combo_add combo (render x)) items ;
        let e = select#get in
        self#lock (fun () ->
            Array.iteri (fun i x -> if x=e then cmb#set_active i) items) ;
      end

    method private clicked () =
      let n = cmb#active in
      if 0 <= n && n < Array.length items then
        self#lock (fun () -> select#set items.(n))

    method! set x =
      begin
        select#set x ;
        Array.iteri (fun i e -> if x=e then cmb#set_active i) items ;
      end

    initializer
      ignore (cmb#connect#changed self#clicked) ;

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

    method popup () =
      let time = GMain.Event.get_current_time () in
      menu#popup ~button:3 ~time

  end
