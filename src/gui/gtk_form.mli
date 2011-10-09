(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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

(** Undocumented. 
    Do not use this module if you don't know what you are doing. *)

(* [JS 2011/10/03] To the authors/users of this module: please document it. *)

(* ------------------------------------------------------------------------ *)
(* ---  Forms Factory                                                   --- *)
(* ------------------------------------------------------------------------ *)

type demon
val demon : unit -> demon
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
