(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

open Cil_types

(** This modules stores the alarms and properties for which a red status has
    been emitted. *)

(* Remembers that a red status has been emitted for an alarm or a property at
   the given kinstr. *)
val add_red_alarm:    kinstr -> Alarms.t -> unit
val add_red_property: kinstr -> Property.t -> unit

type alarm_or_property = Alarm of Alarms.t | Prop of Property.t

module AlarmOrProp : Datatype.S with type t := alarm_or_property

(* Whether a red status has been emitted for an alarm or a property at the given
   kinstr in the given callstack. *)
val is_red_in_callstack:
  kinstr -> alarm_or_property -> Value_types.callstack -> bool

(* Returns the unsorted list of all alarms and properties for which a red status
   has been emitted during the analysis. Also returns the kinstr of the alarm or
   property, and the number of callstacks in which is was invalid.*)
val get_all: unit -> (kinstr * alarm_or_property * int) list
