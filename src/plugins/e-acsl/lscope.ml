(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2018                                               *)
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

type lscope_var =
  | Lvs_let of logic_var * term
  | Lvs_quantif of term * relation * logic_var * relation * term
  | Lvs_formal of logic_var * logic_info
  | Lvs_global of logic_var * term

type t = lscope_var list
(* The logic scope is usually small, so a list is fine instead of a Map *)

let empty = []

let is_empty = function [] -> true | _ :: _ -> false

let add lscope_var t = lscope_var :: t

let get_all t = List.rev t

let exists lv t =
  let is_lv = function
  | Lvs_let(lv', _) | Lvs_quantif(_, _, lv', _, _) | Lvs_formal(lv', _)
  | Lvs_global(lv', _) ->
    Cil_datatype.Logic_var.equal lv lv'
  in
  List.exists is_lv t

exception Lscope_used
let is_used lscope pot =
  let o = object inherit Visitor.frama_c_inplace
    method !vlogic_var_use lv = match lv.lv_origin with
    | Some _ -> Cil.SkipChildren
    | None -> if exists lv lscope then raise Lscope_used else Cil.SkipChildren
  end
  in
  try
    (match pot with
    | Misc.PoT_pred p -> ignore (Visitor.visitFramacPredicate o p)
    | Misc.PoT_term t -> ignore (Visitor.visitFramacTerm o t));
    false
  with Lscope_used ->
    true