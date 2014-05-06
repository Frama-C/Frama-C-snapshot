(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

open Cil_types;;

(* Warnings can either emit ACSL (Alarm), or do not emit ACSL
   (others). *)
type warning =
| Alarm of Alarms.t * Property_status.emitted_status
| Bad_function_pointer

type value_message =
| Warning of warning
| Property_evaluated of Property.t * Property_status.emitted_status
| Precision_Loss of precision_loss_message
| Feedback of unit

and precision_loss_message =
| Exhausted_slevel
| Garbled_mix_creation of Cil_types.exp (* Expression that creates the garbled mix. *)
| Garbled_mix_propagation

(* Temporary: avoid a circular dependency while CilE is used.  *)
type callstack = unit (* Value_types.callstack;; *)
type state = unit (* Cvalue.Model.t;; *)

module Value_Message_Callback =
    Hook.Build
      (struct type t = value_message * kinstr * callstack * state end)

let new_alarm ki alarm property =
  let msg = Warning( Alarm( alarm, property)) in
  Value_Message_Callback.apply (msg, ki, (), ())
;;

let new_status ppt status _state =
  let ki = Property.get_kinstr ppt in
  let msg = Property_evaluated(ppt, status) in
  Value_Message_Callback.apply (msg, ki, (), ())
;;

(* TODO:
   - Store the messages in a data structure.
   - Emit the message only on its first occurrence.
   - Complete Value_messages with the other warning and precision loss message.
   - Actually give callstack and state
   - remove CilE

 *)


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
