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

open Cil_types

(* ********************************************************************** *)
(** {type wto : Wto.partition with stmt}*)
(* ********************************************************************** *)

type wto =
| Nil
| Node of stmt * wto
| Component of wto * wto

(* ********************************************************************** *)
(** {Auxiliar functions} *)
(* ********************************************************************** *)

(** Function succ to build the partition *)
let succ stmt_to_ordered ordered_to_stmt f i =
  let stmt = Ordered_stmt.to_stmt ordered_to_stmt i in
  List.iter
    (fun s ->
      let ordered = Ordered_stmt.to_ordered stmt_to_ordered s in
      f ordered)
    stmt.succs

(** Converts a partition to a wto *)
let rec partition_to_wto ots = function
  | Wto.Nil ->
    Nil
  | Wto.Node (i, p) ->
    Node (Ordered_stmt.to_stmt ots i, partition_to_wto ots p)
  | Wto.Component (p1, p2) ->
    Component (partition_to_wto ots p1, partition_to_wto ots p2)


(** Builds a wto from a kernel function *)
let build_wto kf =
  let first_stmt = Kernel_function.find_first_stmt kf in
  let (stmt_to_ordered, ordered_to_stmt, _) = Ordered_stmt.get_conversion_tables kf in
  let succ = succ stmt_to_ordered ordered_to_stmt in
  let size = Array.length ordered_to_stmt in
  let root = Ordered_stmt.to_ordered stmt_to_ordered first_stmt in
  let partition = Wto.partition ~size ~succ ~root in
  partition_to_wto ordered_to_stmt partition
  

(** Returns the depth of the statement
    @raise Not_found if it is not in the given wto *)
let get_depth stmt wto =
  let rec aux i = function
    | Nil -> raise Not_found
    | Node (s, w) ->
      if Cil_datatype.Stmt.equal stmt s then i
      else aux i w
    | Component (w1, w2) ->
      try
	aux (i + 1) w1
      with Not_found ->
	aux i w2
  in aux 0 wto


(* ********************************************************************** *)
(** {Kernel functions state} *)
(* ********************************************************************** *)


(** {WTO as datatype input} *)
module WTO_input : Datatype.Make_input with type t = wto =
struct

  include Datatype.Undefined

  type t = wto

  let structural_descr = Structural_descr.t_abstract

  let name = "Wto_statement.WTO_input"

  let rehash w = w

  let rec pretty fmt = function
    | Nil -> ()
    | Node (s, Nil) ->
      Format.fprintf fmt "%a" Cil_printer.pp_stmt s
    | Node (s, w) ->
      Format.fprintf fmt "%a " Cil_printer.pp_stmt s;
      pretty fmt w
    | Component (w1, w2) ->
      Format.printf "(";
      pretty fmt w1;
      Format.printf ") ";
      pretty fmt w2

  let rec copy = function
    | Nil -> Nil
    | Node (s, w) -> Node (s, copy w)
    | Component (w1, w2) -> Component (copy w1, copy w2)

  let rec equal w1 w2 = match (w1 ,w2) with
    | Nil, Nil ->
      true
    | Node (s1, w1), Node (s2, w2) ->
      Cil_datatype.Stmt.equal s1 s2 && equal w1 w2
    | Component (w1,w2), Component (w1', w2') ->
      equal w1 w1' && equal w2 w2'
    | _ -> false

  let rec compare w1 w2 = match (w1 ,w2) with
    | Nil, Nil ->
      0
    | Node (s1, w1), Node (s2, w2) ->
      let cmp = Cil_datatype.Stmt.compare s1 s2 in
      if cmp = 0 then compare w1 w2
      else cmp
    | Component (w1, w2), Component (w1', w2') ->
      let cmp = compare w1 w1' in
      if cmp = 0 then compare w2 w2'
      else cmp
    | Nil, _ -> -1
    | Node _, Nil -> 1
    | Node _, Component _ -> -1
    | Component _, _ -> 1

  let reprs = [Nil]
 
end

module WTO = Datatype.Make(WTO_input)

module KF_State =
  Kernel_function.Make_Table
    (WTO)
    (struct
      let size = 97
      let name = "Wto_statement.KF_State"
      let dependencies = [Ast.self]
     end)

(** Returns the wto of a kernel function *)
let wto_of_kf = KF_State.memo build_wto;;

module Stmt_Depth =
  Cil_state_builder.Stmt_hashtbl
    (Datatype.Int)
    (struct
      let size = 97
      let name = "__stmt_state__"
      let dependencies = [Ast.self]
     end)

let depth_of_stmt = Stmt_Depth.memo (fun stmt ->
  let kf = Kernel_function.find_englobing_kf stmt in
  let wto = wto_of_kf kf in
  get_depth stmt wto);;
