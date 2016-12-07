(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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

open VCS

(* -------------------------------------------------------------------------- *)
(* --- Prover Implementation against Task API                             --- *)
(* -------------------------------------------------------------------------- *)

open Task
open Wpo

let dispatch wpo mode prover =
  begin
    match prover with
    | AltErgo -> ProverErgo.prove mode wpo
    | Coq -> ProverCoq.prove mode wpo
    | Why3 prover -> ProverWhy3.prove ~prover wpo
    | Qed -> Task.return VCS.unknown
    | _ -> Task.failed "Prover '%a' not available" VCS.pp_prover prover
  end

let qed_time wpo =
  match wpo.po_formula with
  | GoalCheck _ | GoalLemma _ -> 0.0
  | GoalAnnot vcq -> GOAL.qed_time vcq.VC_Annot.goal

let started ?start wpo =
  match start with
  | None -> ()
  | Some f -> f wpo

let signal ?callin wpo prover =
  match callin with
  | None -> ()
  | Some f -> f wpo prover

let update ?callback wpo prover result =
  Wpo.set_result wpo prover result ;
  match callback with
  | None -> ()
  | Some f -> f wpo prover result

let run_prover wpo ?(mode=BatchMode) ?callin ?callback prover =
  signal ?callin wpo prover ;
  dispatch wpo mode prover >>>
  fun status ->
  let result = match status with
    | Task.Result r -> r
    | Task.Canceled -> VCS.no_result
    | Task.Timeout t -> VCS.timeout t
    | Task.Failed exn -> VCS.failed (error exn)
  in
  let result = { result with solver_time = qed_time wpo } in
  update ?callback wpo prover result ;
  Task.return (Wpo.is_valid result)

let resolve wpo =
  match wpo.po_formula with
  | GoalAnnot vcq -> VC_Annot.resolve vcq
  | GoalLemma vca -> VC_Lemma.is_trivial vca
  | GoalCheck _ -> false

let simplify ?start ?callback wpo =
  Task.call
    (fun wpo ->
       let r = Wpo.get_result wpo VCS.Qed in
       VCS.( r.verdict == Valid ) ||
       begin
         started ?start wpo ;
         if resolve wpo then
           let time = qed_time wpo in
           let result = VCS.result ~time VCS.Valid in
           (update ?callback wpo VCS.Qed result ; true)
         else false
       end)
    wpo

let prove wpo ?mode ?start ?callin ?callback prover =
  simplify ?start ?callback wpo >>= fun succeed ->
  if succeed
  then Task.return true
  else (run_prover wpo ?mode ?callin ?callback prover)

let spawn wpo ?start ?callin ?callback ?success provers =
  let do_monitor f wpo = function
    | None -> f wpo None
    | Some p ->
        let r = Wpo.get_result wpo VCS.Qed in
        let p = if VCS.( r.verdict == Valid ) then VCS.Qed else p in
        f wpo (Some p) in
  let monitor = match success with
    | None -> None
    | Some f -> Some (do_monitor f wpo) in
  ProverTask.spawn ?monitor
    begin
      List.map
        (fun (mode,prover) ->
           prover , prove wpo ~mode ?start ?callin ?callback prover)
        provers
    end
