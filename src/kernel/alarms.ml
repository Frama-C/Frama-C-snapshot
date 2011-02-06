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

open Cil_types
open Cil

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

let pretty fmt al =
  Format.fprintf fmt "alarm caused by %s"
  (match al with
  | Division_alarm -> "a division"
  | Memory_alarm -> "a memory access"
  | Index_alarm -> "a memory access" (* TODO: separate a day when
    		     	    	    the oracles are working *)
  | Shift_alarm -> "a shift"
  | Signed_overflow_alarm -> "an overflow in signed integer arithmetic"
  | Pointer_compare_alarm -> "a pointer comparison"
  | Using_nan_or_infinite_alarm -> "an unknown float value"
  | Result_is_nan_or_infinite_alarm -> "an overflow or nan float computation"
  | Separation_alarm ->
      "incompatible accesses to the same zone in unspecified order"
  | Other_alarm -> "a safety concern")

type alarm = t * code_annotation * annot_status

module Alarm_datatype =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = alarm
      let name = "Alarms.Alarm_datatype"
      let reprs =
	List.map
	  (fun c -> Other_alarm, c, { status = Unknown })
	  Cil_datatype.Code_annotation.reprs
      let compare (a,l,_ : alarm) (a',l',_) =
        let ca = Extlib.compare_basic a a' in
        if ca <> 0 then ca
        (* Do not use Cil_datatype.Code_annotation.compare because we want
           to compare the content of the annotation themselves, not the ids
           (to avoid duplicating annotations) *)
        else Pervasives.compare l.annot_content l'.annot_content
      let equal = Datatype.from_compare
      let copy = Datatype.identity
      let mem_project = Datatype.never_any_project
     end)

module Alarm_set =
  Datatype.Set
    (Set.Make(Alarm_datatype))
    (Alarm_datatype)
    (struct let module_name = "Alarms.Alarm_set" end)

module Alarms =
  Cil_state_builder.Kinstr_hashtbl
    (Alarm_set)
    (struct
      let name = "alarms"
      let dependencies = [] (* delayed in Ast *)
      let size = 7
      let kind = `Internal
     end)

let self = Alarms.self

let register ki to_add =
  try
    let old = Alarms.find ki in
    if Alarm_set.mem to_add old then false
    else (Alarms.add ki (Alarm_set.add to_add old);
          (*(match ki with
           |Cil_types.Kstmt k ->
              Format.eprintf "Got Id:%d@." k.Cil_types.sid
           | _ ->        Format.eprintf "Got GLOB@."
          );*)
          true)

  with Not_found ->
    (*(match ki with
           |Cil_types.Kstmt k ->
              Format.eprintf "Got Id:%d@." k.Cil_types.sid
           | _ -> Format.eprintf "Got GLOB@."
          );*)
    Alarms.add ki (Alarm_set.singleton to_add);
    true

let clear () = Alarms.clear ()

let iter f = Alarms.iter (fun ki set -> Alarm_set.iter (fun v -> f ki v) set)

let fold f acc =
  Alarms.fold
    (fun ki set acc -> Alarm_set.fold (fun v acc -> f ki v acc) set acc)
    acc

let fold_kinstr ki f acc =
  let s = Alarms.find ki in
  Alarm_set.fold f s acc

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
