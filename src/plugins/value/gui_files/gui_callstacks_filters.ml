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

let focus = ref None
let focused_callstacks () = !focus
let focus_on_callstacks cs = focus := cs

let has_matching_callstack ~after csf stmt =
  let module Results = (val Analysis.current_analyzer ()) in
  match Results.get_stmt_state_by_callstack ~after stmt with
  | `Top -> true
  | `Bottom -> false
  | `Value h ->
    try
      Value_types.Callstack.Hashtbl.iter
        (fun cs' _state ->
           let rcs' = from_callstack cs' in
           if callstack_matches csf rcs' then raise Exit
        ) h;
      false
    with
    | Exit -> true

let is_reachable_stmt csf stmt =
  has_matching_callstack ~after:false csf stmt

(* Called only when the statement is reachable *)
let is_non_terminating_instr csf stmt =
  match stmt.skind with
  | Instr _ -> not (has_matching_callstack ~after:true csf stmt)
  | _ -> false


(* The two functions below depends on the abstractions used in the Eva analysis,
   but must be registered only once through the Dynamic module. We thus use
   references to the function, that are changed by the Make functor. *)
let lval_to_zone_callstacks_ref = ref (fun _ _ _ -> Locations.Zone.top)
let tlval_to_zone_callstacks_ref = ref (fun _ _ _ -> Locations.Zone.top)

exception Top

let register_to_zone_functions (module Eval: Gui_eval.S) =
  (* This function evaluates [v] using [ev] at [stmt] (in the pre-state), but
     only for the callstacks matching [csf]. *)
  let eval_filter csf stmt ev v =
    match Eval.Analysis.get_stmt_state_by_callstack ~after:false stmt with
    | `Value h ->
      Value_types.Callstack.Hashtbl.fold
        (fun cs state acc ->
           let rcs' = from_callstack cs in
           if callstack_matches csf rcs' then
             let env = ev.Eval.env state cs in
             let r, _ = ev.Eval.eval_and_warn env v in
             ev.Eval.join acc r
           else acc
        ) h ev.Eval.bottom
    | `Bottom -> ev.Eval.bottom
    | `Top -> raise Top
  in
  let lval_to_zone_callstacks csf stmt lv =
    try eval_filter csf stmt Eval.lval_zone_ev lv
    with Top -> Locations.Zone.top
  and tlval_to_zone_callstacks csf stmt tlv =
    try
      let kf = Kernel_function.find_englobing_kf stmt in
      let ev = Eval.tlval_zone_ev (Gui_types.GL_Stmt (kf, stmt)) in
      eval_filter csf stmt ev tlv
    with Top -> Locations.Zone.top
  in
  lval_to_zone_callstacks_ref := lval_to_zone_callstacks;
  tlval_to_zone_callstacks_ref := tlval_to_zone_callstacks

(* Register evaluation functions that depend on the currently focused
   callstacks. *)
let () =
  let open Cil_datatype in
  let lval_to_zone_gui stmt lv =
    let filter = focused_callstacks () in
    !lval_to_zone_callstacks_ref filter stmt lv
  in
  let tlval_to_zone_gui stmt tlv =
    let filter = focused_callstacks () in
    !tlval_to_zone_callstacks_ref filter stmt tlv
  in
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
  ()
