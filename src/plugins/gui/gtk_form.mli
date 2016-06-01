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

(** {b DEPRECATED.} Helpers around [Gtk_helper] to create side-panel widgets. 

    This module should not be used anymore.
    The provided helpers allow for synchronizing plugin options with
    predefined widgets.

    However, the widgets should be now created with [Toolbox]. The
    synchronization can be achieved using [Toolbox.signal] as a
    replacement for above [demon]. *)

(* ------------------------------------------------------------------------ *)
(* ---  Forms Factory                                                   --- *)
(* ------------------------------------------------------------------------ *)

type demon
val demon : unit -> demon
val register : demon -> (unit -> unit) -> unit
val refresh : demon -> (unit -> unit)

type 'a field =
  ?tooltip:string -> packing:(GObj.widget -> unit) ->
  (unit -> 'a) -> ('a -> unit) -> demon -> unit

val check : ?label:string -> bool field
val menu : (string * 'a) list -> ?width:int -> 'a field
val spinner : ?lower:int -> ?upper:int -> ?width:int -> int field
val label : text:string -> packing:(GObj.widget -> unit) -> unit -> unit
val button : 
  label:string -> ?tooltip:string -> callback:(unit -> unit) -> 
  packing:(GObj.widget -> unit) -> unit -> unit

class form : packing:(GObj.widget -> unit) -> object
    method label : string -> unit
    method item : GObj.widget -> unit
    method row : GObj.widget -> unit
  end
