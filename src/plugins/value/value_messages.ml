(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** UNDOCUMENTED. *)

open Cil_types;;

(** Warnings can either emit ACSL (Alarm), or do not emit ACSL
   (others). *)
type warning =
| Alarm of Alarms.t * code_annotation * Property_status.emitted_status
| Bad_function_pointer
| Uncategorized of string

type value_message =
| Warning of warning
| Property_evaluated of Property.t * Property_status.emitted_status
| Precision_Loss of precision_loss_message
| Lattice_message of Lattice_messages.emitter * Lattice_messages.t
| Feedback of unit

and precision_loss_message =
| Exhausted_slevel
| Garbled_mix_creation of Cil_types.exp (* Expression that creates the garbled mix. *)
| Garbled_mix_propagation
;;

type callstack = Value_types.callstack;;
type state = Cvalue.Model.t;;

module Value_Message_Callback = struct
  include Hook.Build
    (struct type t = value_message * kinstr * callstack * (state * Trace.t) end)
  (* Do not emit the messages when the callstack is empty, meaning Value
     is not running *)
  let apply (_m, _ki, callstack, _state as args) =
    if callstack <> [] then apply args
end

(* The before-state when the message is emitted. *)
let curstate = ref (Cvalue.Model.bottom,Trace.top);;

let set_current_state st = curstate := st;;

let ki_of_callstack = function
  | (_,ki)::_ -> ki
  | _ -> Kglobal
;;

(****************************************************************)
(* Alarms *)

(* Default behaviour: print one alarm per kinstr. *)
module Alarm_key = Datatype.Pair_with_collections
  (Cil_datatype.Kinstr)(Alarms)(struct
      let module_name = "Alarm_key"
  end);;
module Alarm_cache = State_builder.Hashtbl(Alarm_key.Hashtbl)(Datatype.Unit)(struct
  let name = "Value_messages.Alarm_cache"
  let dependencies = [Db.Value.self]
  let size = 35
end)

let default_alarm_report ki alarm str =
  Alarm_cache.memo (fun (_ki,_alarm) ->
    Kernel.warning ~current:true "%s" str
  ) (ki,alarm)
;;

let new_alarm ki alarm property annot str =
  let msg = Warning( Alarm( alarm, annot, property)) in
  default_alarm_report ki alarm str;
  Value_Message_Callback.apply (msg, ki, Value_util.call_stack(), !curstate)
;;

(****************************************************************)
(* Lattice messages. *)

Lattice_messages.message_destination := (fun emitter msg ->
  let callstack = Value_util.call_stack() in
  let vmsg = Lattice_message (emitter, msg) in
  Value_Message_Callback.apply
    (vmsg, ki_of_callstack callstack, callstack, !curstate);
  match msg with
  | Lattice_messages.Imprecision _ -> () (* Only for debug purposes *)
  | Lattice_messages.Approximation str
  | Lattice_messages.Costly str
  | Lattice_messages.Unsoundness str ->
     Kernel.result ~current:true ~once:true "%s" str;
);;

(****************************************************************)
(* Property statuses *)

let new_status ppt status (state,trace) =
  let ki = Property.get_kinstr ppt in
  let msg = Property_evaluated(ppt, status) in
  Value_Message_Callback.apply (msg, ki, Value_util.call_stack(), (state,trace))
;;

(****************************************************************)
(* General warnings *)

let warning x =
  Format.kfprintf (fun _fmt ->
  let str = Format.flush_str_formatter() in
  let msg = Warning(Uncategorized(str)) in
  Kernel.warning ~once:true ~current:true "%s" str;
  let callstack = Value_util.call_stack() in
  Value_Message_Callback.apply (msg, ki_of_callstack callstack,
                                callstack, !curstate))
    Format.str_formatter x
;;


(* TODO:
   - Store the messages in a data structure.
   - Emit the message only on its first occurrence.
   - Complete Value_messages with the other warning and precision loss message.
   - Use new_alarm and new_status to replace the alarm emission mechanism.
   - Actually give callstack and state
   - remove CilE

 *)


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
