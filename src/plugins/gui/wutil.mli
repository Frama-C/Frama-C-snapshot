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

(** {1 Wtoolkit - Utilities} *)

val on : 'a option -> ('a -> unit) -> unit
val fire : ('a -> unit) list -> 'a -> unit
val once : ('a -> 'b) -> 'a -> 'b

(** {2 Settings & Console} *)

val share : string ref
val flush : (string -> unit) ref
val warning : ('a,Format.formatter,unit) format -> 'a

(** {2 Styling} *)

val set_enabled : #GObj.widget -> bool -> unit
val set_visible : #GObj.widget -> bool -> unit
val set_tooltip : #GObj.widget -> string option -> unit
val set_font : #GObj.widget -> string -> unit
val set_monospace : #GObj.widget -> unit
val set_small_font : #GObj.widget -> unit
val set_bold_font : #GObj.widget -> unit
val to_utf8 : string -> string

(** {2 Timing} *)

val later : (unit -> unit) -> unit
(** Post the action on next idle. *)

(** {2 Events} *)

(** Defines [on_xxx] in term of [connect]. *)
class virtual ['a] handler :
  object
    method virtual connect : ('a -> unit) -> unit
    method on_check : 'a -> (bool -> unit) -> unit
    (** [on_check v] emits boolean signal [(s=v)] on signal [s]. *)

    method on_value : 'a -> (unit -> unit) -> unit
    (** [on_value v] emits a unit signal on signal [s=v]. *)

    method on_event : (unit -> unit) -> unit
    (** [on_event] emits a unit signal on any signal [s]. *)

  end

(** Has type {!Widget.signal} *)
class ['a] signal :
  object
    method fire : 'a -> unit
    method set_enabled : bool -> unit
    method connect : ('a -> unit) -> unit
    method lock : (unit -> unit) -> unit
    inherit ['a] handler
  end

(** Has type {!Widget.selector} *)
class ['a] selector : 'a ->
  object
    inherit ['a] signal
    method set : 'a -> unit
    method get : 'a
    method send : ('a -> unit) -> unit -> unit
  end

class type widget =
  object
    method set_visible : bool -> unit
    method set_enabled : bool -> unit
    method coerce : GObj.widget
    method widget : widget
  end

class layout :
  object
    inherit widget
    method populate : #widget -> unit
  end

class gobj_widget : #GObj.widget -> widget
class gobj_action : #GObj.widget ->
  object
    inherit widget
    method set_tooltip : string -> unit
  end
