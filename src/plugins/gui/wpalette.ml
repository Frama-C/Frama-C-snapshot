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

open Widget

class tool ?label ?tooltip ?content () =
  let status = new Widget.image `None in
  let toggle = new Widget.toggle ~align:`Left ?label ?tooltip ~border:false () in
  let action = new Widget.button ~icon:`MEDIA_PLAY ?tooltip () in
  object(self)

    initializer
      begin
        let color = `NAME "orange" in
        toggle#coerce#misc#modify_bg [ `PRELIGHT , color ; `ACTIVE , color ] ;
        toggle#connect self#toggle ;
        action#connect self#action ;
        self#toggle false ;
        action#set_border false ;
        Wutil.on content self#set_content ;
      end

    val mutable details = None
    val mutable tooltip = None
    val mutable callback = None
    val mutable view = None

    method private toggle a =
      match details with
      | None -> ()
      | Some w -> Wutil.set_visible w a

    method private action () =
      match callback with
      | None -> ()
      | Some f -> f ()

    method private render =
      let hbox = GPack.hbox ~show:true () in
      hbox#pack ~expand:false status#coerce ;
      hbox#pack ~expand:true ~fill:true ~padding:1 toggle#coerce ;
      hbox#pack ~expand:false action#coerce ;
      match details with
      | None -> hbox#coerce
      | Some w ->
          let vbox = GPack.vbox ~show:true () in
          vbox#pack ~expand:false hbox#coerce ;
          vbox#pack ~expand:true ~fill:false w#coerce ;
          vbox#coerce

    method tool = (self :> tool)

    method widget =
      match view with Some w -> w | None ->
        let w = new Wutil.gobj_widget self#render in
        view <- Some w ; w

    method coerce = self#widget#coerce
    method on_active = toggle#connect
    method is_active = toggle#get
    method set_active = toggle#set
    method has_action = callback != None
    method set_enabled e = self#widget#set_enabled e
    method set_visible v = self#widget#set_visible v

    method set_label = toggle#set_label
    method set_tooltip txt = toggle#set_tooltip txt
    method set_status = status#set_icon

    method clear_action =
      callback <- None ;
      action#set_visible false

    method set_action ?icon ?tooltip ?callback:cb () =
      begin
        callback <- cb ;
        action#set_visible true ;
        action#set_enabled (cb != None) ;
        Wutil.on icon action#set_icon ;
        Wutil.on tooltip action#set_tooltip ;
      end

    method set_content (w : widget) =
      assert ( details == None ) ;
      let frame = GBin.frame ~show:false () in
      let padds = GBin.alignment ~padding:(4,4,4,4) () in
      padds#add w#coerce ;
      frame#add padds#coerce ;
      details <- Some frame

  end

(* -------------------------------------------------------------------------- *)
(* --- Panel                                                              --- *)
(* -------------------------------------------------------------------------- *)

class panel () =
  let box = GPack.vbox ~show:true () in
  object(self)
    inherit Wutil.gobj_widget box

    val mutable lock = false
    val mutable tools = []
    
    method add_widget (w : GObj.widget) =
      box#pack ~expand:false w
    
    method add_tool (w : tool) =
      begin
        self#add_widget w#coerce ;
        w#on_active (self#active w) ;
        tools <- w :: tools ;
      end
      
    method private active w a =
      if a && not lock then
        try
          lock <- true ;
          List.iter (fun w0 -> if w0 <> w then w0#set_active false) tools ;
          lock <- false ;
        with e ->
          lock <- false ; raise e
    
  end
