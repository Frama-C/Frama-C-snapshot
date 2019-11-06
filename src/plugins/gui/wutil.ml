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

(* -------------------------------------------------------------------------- *)
(* ---  Utils                                                             --- *)
(* -------------------------------------------------------------------------- *)

let on x f = match x with None -> () | Some x -> f x
let fire fs x = List.iter (fun f -> f x) fs
let once = Wutil_once.once

(* -------------------------------------------------------------------------- *)
(* ---  Pango Properties                                                  --- *)
(* -------------------------------------------------------------------------- *)

include Gtk_compat.Pango

let set_font w name = w#misc#modify_font_by_name name
let set_monospace w = set_font w "monospace"

(* -------------------------------------------------------------------------- *)
(* --- Misc                                                               --- *)
(* -------------------------------------------------------------------------- *)

let set_tooltip w m = on m w#misc#set_tooltip_text
let set_enabled (w : #GObj.widget) = w#misc#set_sensitive
let set_visible (w : #GObj.widget) e =
  let m = w#misc in if e then m#show () else m#hide ()

let share = ref "/usr/local/share"
let flush = ref prerr_endline
let warning msg =
  let buffer = Buffer.create 80 in
  Format.kfprintf
    (fun fmt ->
       Format.pp_print_flush fmt () ;
       !flush (Buffer.contents buffer))
    (Format.formatter_of_buffer buffer) msg

(* -------------------------------------------------------------------------- *)
(* ---  UTF-8                                                             --- *)
(* -------------------------------------------------------------------------- *)

let to_utf8 s =
  try
    if Glib.Utf8.validate s then s else Glib.Convert.locale_to_utf8 s
  with Glib.Convert.Error _ ->
  try
    Glib.Convert.convert_with_fallback
      ~fallback:"#neither UTF-8 nor locale nor ISO-8859-15#"
      ~to_codeset:"UTF-8"
      ~from_codeset:"ISO_8859-15"
      s
  with Glib.Convert.Error _ as e -> Printexc.to_string e

(* -------------------------------------------------------------------------- *)
(* ---  Timer                                                             --- *)
(* -------------------------------------------------------------------------- *)

let later f =
  let for_idle () = f () ; false in
  let prio = Glib.int_of_priority `LOW in
  ignore (Glib.Idle.add ~prio for_idle)

(* -------------------------------------------------------------------------- *)
(* ---  Ratio                                                             --- *)
(* -------------------------------------------------------------------------- *)

let get_pane_ratio (paned:GPack.paned) =
  let paned_min_pos = paned#min_position in
  let paned_max_pos = paned#max_position in
  let length = paned_max_pos - paned_min_pos in
  if length = 0 then 0.5
  else (float_of_int paned#position)/.(float_of_int length)

let set_pane_ratio (paned:GPack.paned) ratio =
  let paned_min_pos = paned#min_position in
  let offset =
    int_of_float (float (paned#max_position - paned_min_pos) *. ratio)
  in paned#set_position (paned_min_pos + offset)

(* -------------------------------------------------------------------------- *)
(* ---  Widget & Signals                                                  --- *)
(* -------------------------------------------------------------------------- *)

class type widget =
  object
    method set_visible : bool -> unit
    method set_enabled : bool -> unit
    method coerce : GObj.widget
    method widget : widget
  end

class gobj_widget obj =
  object(self)
    method set_visible = set_visible obj
    method set_enabled = set_enabled obj
    method coerce : GObj.widget = (obj#coerce)
    method widget = (self :> widget)
  end

class gobj_action obj =
  object
    inherit gobj_widget obj
    method set_tooltip txt = set_tooltip (obj :> GObj.widget) (Some txt)
  end

class layout =
  object(self)
    val mutable content : widget option = None
    method coerce =
      match content with
      | None -> raise (Invalid_argument "Wbox.layout")
      | Some w -> w#coerce
    method widget =
      match content with
      | None -> (self :> widget)
      | Some w -> w
    method set_visible v =
      match content with
      | None -> ()
      | Some w -> w#set_visible v
    method set_enabled e =
      match content with
      | None -> ()
      | Some w -> w#set_enabled e
    method populate : 'a. (#widget as 'a) -> unit =
      fun w -> content <- Some (w :> widget)
  end

class virtual ['a] handler =
  object(self)
    method virtual connect : ('a -> unit) -> unit
    method on_check v f = self#connect (fun e -> f (e=v))
    method on_value v f = self#connect (fun e -> if e=v then f ())
    method on_event f = self#connect (fun _ -> f ())
  end

class ['a] signal =
  object
    val mutable enabled = true
    val mutable lock = false
    val mutable demon = []
    inherit ['a] handler
    method fire (x:'a) =
      if enabled && not lock then
        try lock <- true ; fire demon x ; lock <- false
        with err -> lock <- false ; raise err
    method connect f = demon <- demon @ [f]
    method set_enabled e = enabled <- e
    method lock : (unit -> unit) -> unit =
      fun f ->
      if not lock then
        try lock <- true ; f () ; lock <- false
        with err -> lock <- false ; raise err
  end

class ['a] selector default =
  object(self)
    val mutable current : 'a = default
    inherit ['a] signal
    method get = current
    method set x = current <- x ; self#fire x
    method send f () = self#lock (fun () -> f current)
  end
