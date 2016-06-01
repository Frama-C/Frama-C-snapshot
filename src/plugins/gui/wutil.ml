(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(*                                                                        *)
(**************************************************************************)

(* -------------------------------------------------------------------------- *)
(* ---  Utils                                                             --- *)
(* -------------------------------------------------------------------------- *)

let on x f = match x with None -> () | Some x -> f x
let fire fs x = List.iter (fun f -> f x) fs

type ('a,'b) cell = Value of 'b | Fun of ('a -> 'b)
let get p x =
  match !p with
  | Value y -> y
  | Fun f -> let y = f x in p := Value y ; y
let once f = get (ref (Fun f))

(* -------------------------------------------------------------------------- *)
(* ---  Pango Properties                                                  --- *)
(* -------------------------------------------------------------------------- *)

let small_font =
  once (fun f ->
      let f = Pango.Font.copy f in
      let s = Pango.Font.get_size f in
      Pango.Font.set_size f (s-2) ; f)

let bold_font =
  once (fun f ->
      let f = Pango.Font.copy f in
      Pango.Font.set_weight f `BOLD ; f)

let modify_font phi widget =
  widget#misc#modify_font (phi widget#misc#pango_context#font_description)

let set_small_font w = modify_font small_font w
let set_bold_font w = modify_font bold_font w
let set_tooltip w m = on m w#misc#set_tooltip_text

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
(* ---  Widget & Signals                                                  --- *)
(* -------------------------------------------------------------------------- *)

class coerce obj =
  object
    method set_enabled (e : bool) : unit = obj#misc#set_sensitive e
    method coerce : GObj.widget = (obj#coerce)
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
