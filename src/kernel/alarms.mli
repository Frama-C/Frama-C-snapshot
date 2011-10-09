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

(** Alarms Database for the value analysis.
    @plugin development guide *)

(** Warning: the interface of this module will probably radically change soon.
    Do not use it on stable code *)

open Cil_types

type alarm = Cil_types.alarm * code_annotation

module Alarm_datatype: Datatype.S with type t = alarm
(* [compare] and [equal] in this datatype are very inefficient. Don't use it
   often. *)

val pretty : Format.formatter -> Cil_types.alarm -> unit

(** Register the given alarm on the given statement. By default,
    the alarm is emitted with status [Dont_know], and by the given emitter.
    Return true if the given alarm has never been emitted before on the
    same kinstr (without taking into consideration the status or
    the emitter) *)
val register:
  deps:State.t list ->
  kinstr ->
  alarm ->
  ?status:Property_status.emitted_status ->
  Emitter.t ->
  bool
val clear: unit -> unit

val iter: (kinstr -> alarm -> unit) -> unit

val fold: (kinstr -> alarm -> 'a -> 'a) -> 'a -> 'a

val fold_kinstr: kinstr -> (alarm -> 'a -> 'a) -> 'a -> 'a

val self: State.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
