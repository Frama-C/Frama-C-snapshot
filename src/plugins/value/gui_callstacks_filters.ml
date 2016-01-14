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

type rcallstack = Value_types.callstack

let empty = []

let from_callstack = List.rev

let callstack_matches_callstack (rcs1:rcallstack) (rcs2:rcallstack) =
  let rec aux q1 q2 = match q1, q2 with
    | [], _ | _, [] -> true
    | call1 :: q1, call2 :: q2 ->
      Value_types.Callsite.equal call1 call2 && aux q1 q2
  in
  aux rcs1 rcs2

type filter = rcallstack list option

let callsite_matches_callstack stmt (rcs: rcallstack) =
  let ki = Kstmt stmt in
  List.exists (fun (_, ki') -> Cil_datatype.Kinstr.equal ki ki') rcs

let callstack_matches csf rcs = match csf with
  | None -> true
  | Some lrcs -> List.exists (callstack_matches_callstack rcs) lrcs

let callsite_matches csf stmt =
  match csf with
  | None -> true
  | Some lrcs -> List.exists (callsite_matches_callstack stmt) lrcs

let is_reachable_stmt csf stmt =
  match csf with
  | None -> Db.Value.is_reachable_stmt stmt
  | Some _ as csf ->
    try
      let h = Db.Value.Table_By_Callstack.find stmt in
      Value_types.Callstack.Hashtbl.iter
        (fun cs' state ->
           let rcs' = from_callstack cs' in
           if callstack_matches csf rcs' && Db.Value.is_reachable state
           then raise Exit
        ) h;
      false
    with
    | Not_found -> false
    | Exit -> true

(* Called only when the statement is reachable *)
let is_non_terminating_instr csf stmt =
  match csf with
  | None -> Value_results.is_non_terminating_instr stmt
  | Some _ as csf ->
    try
      let h = Db.Value.AfterTable_By_Callstack.find stmt in
      Value_types.Callstack.Hashtbl.iter
        (fun cs' _state ->
           let rcs' = from_callstack cs' in
           (* Bottom states are never stored in this table *)
           if callstack_matches csf rcs'
           then raise Exit) h;
      true
    with
    | Not_found -> true
    | Exit -> false

(* This function evaluates [v] using [ev] at [stmt] (in the pre-state), but
   only for the callstacks matching [csf]. *)
let eval_filter csf stmt ev v =
  try
    let h = Db.Value.Table_By_Callstack.find stmt in
    Value_types.Callstack.Hashtbl.fold
      (fun cs state acc ->
         let rcs' = from_callstack cs in
         if callstack_matches csf rcs' then
           let env = ev.Gui_eval.env state cs in
           let r, _ = ev.Gui_eval.eval_and_warn env v in
           ev.Gui_eval.join acc r
         else acc
      ) h ev.Gui_eval.bottom
  with
  | Not_found -> ev.Gui_eval.bottom

let lval_to_zone_callstacks csf stmt lv =
  eval_filter csf stmt Gui_eval.lval_zone_ev lv

let tlval_to_zone_callstacks csf stmt tlv =
  let kf = Kernel_function.find_englobing_kf stmt in
  let ev = Gui_eval.tlval_zone_ev (Gui_types.GL_Stmt (kf, stmt)) in
  eval_filter csf stmt ev tlv

let set_callstacks_filter  =
  let lcs = ref None in
  let open Cil_datatype in
  let lval_to_zone_gui stmt lv = lval_to_zone_callstacks !lcs stmt lv in
  let tlval_to_zone_gui stmt tlv = tlval_to_zone_callstacks !lcs stmt tlv in
  let _eval_lv =
    Dynamic.register
      ~comment:"Evaluation of a l-value on the callstacks focused in the GUI"
      ~plugin:"Value" "lval_to_zone_gui"
      (Datatype.func2 Stmt.ty Lval.ty Locations.Zone.ty)
      ~journalize:false lval_to_zone_gui
  in
  let _eval_tlv =
    Dynamic.register
      ~comment:"Evaluation of a term, supposed to be a location, on the callstacks focused in the GUI"
      ~plugin:"Value" "tlval_to_zone_gui"
      (Datatype.func2 Stmt.ty Term.ty Locations.Zone.ty)
      ~journalize:false tlval_to_zone_gui
  in
  (fun l -> lcs := l)
