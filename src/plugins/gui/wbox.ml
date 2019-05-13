(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

type expand = W | H | V | HV

let hdir = function W | V -> false | H | HV -> true
let vdir = function W | H -> false | V | HV -> true

type box =
  | Void
  | ToEnd
  | Pack of expand * int * widget

let box ?(expand=W) ?(padding=0) ?widget () =
  match widget with
  | None -> Void
  | Some w -> Pack(expand,padding,(w :> widget))
let g ?(expand=W) ?(padding=0) g = Pack(expand,padding,new Wutil.gobj_widget g)
let w ?(expand=W) ?(padding=0) a = Pack(expand,padding,(a :> widget))
let h ?(padding=0) a = Pack(H,padding,(a :> widget))
let v ?(padding=0) a = Pack(V,padding,(a :> widget))
let hv ?(padding=0) a = Pack(HV,padding,(a :> widget))

let label ?(fill=false) ?style ?align ?padding text =
  w ~expand:(if fill then H else W) ?padding
    (new Widget.label ~text ?align ?style ())

let rec populate dir (box : #GPack.box) from = function
  | [] -> ()
  | Pack(e,padding,w)::ws ->
      box#pack ~from ~expand:(dir e) ~padding w#coerce ;
      populate dir box from ws
  | Void::ws ->
      populate dir box from ws
  | ToEnd::ws ->
      if from = `START then
        populate dir box `END (List.rev ws)
      else
        populate dir box from ws

let hbox ws =
  let box = GPack.hbox ~show:true () in
  populate hdir box `START ws ;
  new Wutil.gobj_widget box

let vbox ws =
  let box = GPack.vbox ~show:true () in
  populate vdir box `START ws ;
  new Wutil.gobj_widget box

let hgroup (ws : widget list) =
  let box = GPack.hbox ~show:true ~homogeneous:true () in
  List.iter (fun w -> box#pack ~expand:false w#coerce) ws ;
  new Wutil.gobj_widget box

let vgroup (ws : widget list) =
  let box = GPack.vbox ~show:true ~homogeneous:true () in
  List.iter (fun w -> box#pack ~expand:false w#coerce) ws ;
  new Wutil.gobj_widget box

let (<|>) xs ys = if ys = [] then xs else (xs @ (ToEnd::ys))

let toolbar xs ys = hbox (xs <|> ys)
let sidebar xs ys = vbox (xs <|> ys)

let panel ?top ?left ?right ?bottom center =
  let middle =
    match left , right with
    | None , None -> (center :> widget)
    | Some a , Some b -> hbox [ v a ; hv ~padding:2 center ; v b ]
    | None , Some b -> hbox [ hv ~padding:2 center ; v b ]
    | Some a , None -> hbox [ v a ; hv ~padding:2 center ]
  in match top , bottom with
  | None , None -> middle
  | Some t , None -> vbox [ h t ; hv middle ]
  | None , Some t -> vbox [ hv middle ; w t ]
  | Some a , Some b -> vbox [ h a ; hv middle ; h b ]

class type splitter =
  object
    inherit Wutil.widget
    method get : float
    method set : float -> unit
    method connect : (float -> unit) -> unit
  end

let split ~dir w1 w2 =
  let pane = GPack.paned dir () in
  pane#add1 w1#coerce ;
  pane#add2 w2#coerce ;
  let splitter =
    object
      inherit (Wutil.gobj_widget pane)
      method get = Wutil.get_pane_ratio pane
      method set = Wutil.set_pane_ratio pane
      method connect f =
        let callback _ = f (Wutil.get_pane_ratio pane) ; false in
        ignore (pane#event#connect#button_release ~callback)
    end
  in (splitter :> splitter)

let scroll ?(hpolicy=`AUTOMATIC) ?(vpolicy=`AUTOMATIC) w =
  (* Explicit conversion needed for lablgtk3, as policy_type has been extended
     with another constructor but we still export the lablgtk2 type. *)
  let vpolicy = (vpolicy :> Gtk.Tags.policy_type) in
  let hpolicy = (hpolicy :> Gtk.Tags.policy_type) in
  let scrolled = GBin.scrolled_window ~vpolicy ~hpolicy () in
  scrolled#add_with_viewport w#coerce ;
  new Wutil.gobj_widget scrolled

let hscroll w = scroll ~vpolicy:`NEVER w
let vscroll w = scroll ~hpolicy:`NEVER w
