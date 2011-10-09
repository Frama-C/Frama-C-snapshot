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

type alarm = Cil_types.alarm * code_annotation

module Alarm_datatype =
  Datatype.Make
    (struct
      open Cil_datatype
      include Datatype.Serializable_undefined
      type t = alarm
      let name = "Alarms.Alarm_datatype"
      let reprs = List.map (fun c -> Other_alarm, c) Code_annotation.reprs

      (* this [compare] is very inefficient. Don't use it often. *)
      let compare (a,l : alarm) (a',l') =
        let ca = Alarm.compare a a' in
        if ca <> 0 then ca
        else
          (* Do not use Cil_datatype.Code_annotation.compare because we want
             to compare the content of the annotation themselves, not the ids
             (to avoid duplicating annotations)
             [JS 2011/06/15] Ok do not use it, but neither Pervasives.compare *)
          Datatype.String.compare
            (Marshal.to_string l.annot_content [])
            (Marshal.to_string l'.annot_content [])
      let equal = Datatype.from_compare
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
      let dependencies = [ Ast.self ]
      let size = 7
      let kind = `Internal
     end)

let self = Alarms.self

let register ~deps ki (atyp, annot as to_add) ?(status=Property_status.Dont_know) emitter =
  let add old = 
    Alarms.replace ki (Alarm_set.add to_add old);
    match ki with
    | Kglobal -> 
      (* [JS 2011/07/05] where should the annotation be added? *)
      Kernel.warning ~once:true ~current:true
	"global alarm occured. Check the log below."
    | Kstmt s -> 
      let kf = Kernel_function.find_englobing_kf s in
      Annotations.add kf s deps (AI (atyp, annot));
      let p = Property.ip_of_code_annot kf s annot in
      List.iter
        (fun p -> Property_status.emit emitter ~hyps:[] p ~distinct:true status)
        p
  in
  try
    let old = Alarms.find ki in
    if Alarm_set.mem to_add old then false
    else begin
      add old;
      true
    end
  with Not_found ->
    add Alarm_set.empty;
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
