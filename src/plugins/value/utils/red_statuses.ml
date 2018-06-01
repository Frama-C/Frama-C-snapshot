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

type alarm_or_property = Alarm of Alarms.t | Prop of Property.t

(* Datatype for [alarm_or_property]. *)
module AlarmOrProp = Datatype.Make_with_collections(struct
    include Datatype.Serializable_undefined

    type t = alarm_or_property

    let reprs = [Alarm (List.hd Alarms.reprs); Prop (List.hd Property.reprs)]

    let name = "Value.Red_statuses.AlarmOrProp"

    let pretty fmt = function
      | Alarm a -> Alarms.pretty fmt a
      | Prop p -> Property.pretty fmt p

    let compare v1 v2 = match v1, v2 with
      | Alarm a1, Alarm a2 -> Alarms.compare a1 a2
      | Prop p1, Prop p2 -> Property.compare p1 p2
      | Alarm _, Prop _ -> -1
      | Prop _, Alarm _ -> 1

    let equal = Datatype.from_compare

    let hash = function
      | Alarm a -> 3 + Alarms.hash a
      | Prop p -> 175 + Property.hash p
  end)

module Callstacks = Value_types.Callstack.Set

(* For each alarm or predicate, stores the set of callstacks for which it was
   evaluated to False. *)
module RedStatuses = AlarmOrProp.Map.Make (Callstacks)

(* Stores the set of red statuses at each program point. *)
module RedStatusesTable =
  Cil_state_builder.Kinstr_hashtbl
    (RedStatuses)
    (struct
      let name = "Value.Red_statuses.RedStatusesTable"
      let size = 16
      let dependencies = [ Db.Value.self ]
    end)

let add_red_ap kinstr ap =
  let current_map =
    try RedStatusesTable.find kinstr
    with Not_found -> AlarmOrProp.Map.empty
  in
  let callstacks =
    try AlarmOrProp.Map.find ap current_map
    with Not_found -> Callstacks.empty
  in
  let new_callstacks = Callstacks.add (Value_util.call_stack ()) callstacks in
  let new_map = AlarmOrProp.Map.add ap new_callstacks current_map in
  RedStatusesTable.replace kinstr new_map

let add_red_alarm ki a = add_red_ap ki (Alarm a)

let add_red_property ki ip =
  if false then
    add_red_ap ki (Prop ip)
  else
    (* Collapses preconditions-at-callsites into the precondition itself,
       by modifying the callstack. Results in a better display *)
    let open Property in
    match ip with
    | IPPropertyInstance (_, _, _,
                          (IPPredicate (PKRequires _, _, _, _) as ip')) ->
      add_red_ap Kglobal (Prop ip')
    | _ -> add_red_ap ki (Prop ip)

let is_red_in_callstack kinstr ap callstack =
  try
    let map = RedStatusesTable.find kinstr in
    let callstacks = AlarmOrProp.Map.find ap map in
    Callstacks.mem callstack callstacks
  with Not_found -> false

let get_all () =
  let gather kinstr map acc =
    AlarmOrProp.Map.fold
      (fun ap callstacks acc ->
         let number = Callstacks.cardinal callstacks in
         (kinstr, ap, number) :: acc)
      map acc
  in
  RedStatusesTable.fold gather []
