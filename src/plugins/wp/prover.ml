(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

let dispatch ?(config=VCS.default) mode prover wpo =
  begin
    match prover with
    | AltErgo -> ProverErgo.prove ~config ~mode wpo
    | Coq -> ProverCoq.prove mode wpo
    | Why3 prover -> ProverWhy3.prove ?timeout:config.timeout ~prover wpo
    | Qed | Tactical -> Task.return VCS.no_result
    | _ -> Task.failed "Prover '%a' not available" VCS.pp_prover prover
  end

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

let run_prover wpo ?config ?(mode=BatchMode) ?callin ?callback prover =
  signal ?callin wpo prover ;
  dispatch ?config mode prover wpo >>>
  fun status ->
  let result = match status with
    | Task.Result r -> r
    | Task.Canceled -> VCS.no_result
    | Task.Timeout t -> VCS.timeout t
    | Task.Failed exn -> VCS.failed (error exn)
  in
  let result = { result with solver_time = Wpo.qed_time wpo } in
  update ?callback wpo prover result ;
  Task.return (VCS.is_valid result)

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

let prove wpo ?config ?mode ?start ?callin ?callback prover =
  simplify ?start ?callback wpo >>= fun succeed ->
  if succeed
  then Task.return true
  else (run_prover wpo ?config ?mode ?callin ?callback prover)

let spawn wpo ?config ?start ?callin ?callback ?success ?pool provers =
  let do_monitor on_success wpo = function
    | None -> on_success wpo None
    | Some prover ->
        let r = Wpo.get_result wpo VCS.Qed in
        let prover = if VCS.( r.verdict == Valid ) then VCS.Qed else prover in
        on_success wpo (Some prover)
  in
  let monitor = match success with
    | None -> None
    | Some on_success -> Some (do_monitor on_success wpo)
  in
  ProverTask.spawn ?monitor ?pool
    begin
      List.map
        (fun (mode,prover) ->
           prover , prove wpo ?config ~mode ?start ?callin ?callback prover)
        provers
    end
