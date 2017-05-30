(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

type wto = stmt Wto.partition


(* ********************************************************************** *)
(** {Interface to the Wto module} *)
(* ********************************************************************** *)

module Scheduler = Wto.Make
  (struct
    include Cil_datatype.Stmt
    let pretty fmt s = Format.pp_print_int fmt s.sid
  end)

let build_wto kf =
  let init = Kernel_function.find_first_stmt kf
  and succs = fun stmt -> List.rev stmt.succs
  in
  Scheduler.partition ~init ~succs


(* ********************************************************************** *)
(** {Datatype and State} *)
(* ********************************************************************** *)

(** {WTO as datatype} *)
module WTO =
  Datatype.Make
    (struct
       include Datatype.Serializable_undefined
       type t = wto
       let name = "Wto_statement.WTO"
       let pretty = Scheduler.pretty_partition
       let copy w = w
       let reprs = [List.map (fun s -> Wto.Node s) Cil_datatype.Stmt.reprs]
     end)

module WTOState =
  Kernel_function.Make_Table
    (WTO)
    (struct
      let size = 97
      let name = "Wto_statement.WTOState"
      let dependencies = [Ast.self]
     end)

(** Returns the wto of a kernel function *)
let wto_of_kf = WTOState.memo build_wto;;


(* ********************************************************************** *)
(** {WTO Indexes} *)
(* ********************************************************************** *)

type wto_index = stmt list

module WTOIndex =
  Datatype.Make
    (struct
      include Datatype.Serializable_undefined
      type t = wto_index
      let reprs = [Cil_datatype.Stmt.reprs]
      let name = "Wto_statement.WTOIndex"
      let pretty =
        Pretty_utils.pp_list ~sep:","
          (fun fmt stmt -> Format.pp_print_int fmt stmt.sid)
      let copy w = w
     end)

module StmtTable = Cil_datatype.Stmt.Hashtbl

module WTOIndexState =
  Kernel_function.Make_Table
    (StmtTable.Make (WTOIndex))
    (struct
      let size = 97
      let name = "Wto_statement.WTOIndexState"
      let dependencies = [Ast.self]
     end)

let build_wto_index_table kf =
  let table = StmtTable.create 17 in
  let rec iter_wto index w =
    List.iter (iter_element index) w
  and iter_element index = function
    | Wto.Node s ->
      StmtTable.add table s index
    | Wto.Component (h, w) ->
      let new_index = h :: index in
      iter_wto new_index (Wto.Node h :: w)
  in
  iter_wto [] (wto_of_kf kf);
  table

let get_wto_index_table = 
  WTOIndexState.memo build_wto_index_table

let wto_index_of_stmt stmt =
  let kf = Kernel_function.find_englobing_kf stmt in
  let table = get_wto_index_table kf in
  try
    StmtTable.find table stmt
  with Not_found -> []

let wto_index_diff index1 index2 =
  let rec remove_common_prefix l1 l2 =
    match l1, l2 with
    | x :: l1, y :: l2 when Cil_datatype.Stmt.equal x y ->
      remove_common_prefix l1 l2
    | l1, l2 -> l1, l2
  in
  let l1 = List.rev index1
  and l2 = List.rev index2
  in
  let left, entered = remove_common_prefix l1 l2 in
  List.rev left, entered

let wto_index_diff_of_stmt stmt1 stmt2 =
  wto_index_diff (wto_index_of_stmt stmt1) (wto_index_of_stmt stmt2)

