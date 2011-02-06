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

(** Alarm Database.
    @plugin development guide *)

type t =
  | Division_alarm
  | Memory_alarm
  | Index_alarm
  | Shift_alarm
  | Pointer_compare_alarm
  | Signed_overflow_alarm
  | Using_nan_or_infinite_alarm
  | Result_is_nan_or_infinite_alarm
  | Separation_alarm
  | Other_alarm

type alarm = t * Cil_types.code_annotation * Cil_types.annot_status

module Alarm_datatype: Datatype.S with type t = alarm

val pretty : Format.formatter -> t -> unit
val register: Cil_types.kinstr -> alarm -> bool
val clear: unit -> unit

val iter: (Cil_types.kinstr -> alarm -> unit) -> unit

val fold:
  (Cil_types.kinstr -> alarm -> 'a -> 'a) -> 'a -> 'a

val fold_kinstr:
  Cil_types.kinstr -> (alarm -> 'a -> 'a) -> 'a -> 'a

val self: State.t

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
