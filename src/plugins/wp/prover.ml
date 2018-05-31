(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2018                                               *)
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

let signal ?progress wpo msg =
  match progress with
  | None -> ()
  | Some f -> f wpo msg

let update ?result wpo prover res =
  Wpo.set_result wpo prover res ;
  match result with
  | None -> ()
  | Some f -> f wpo prover res

let run_prover wpo ?config ?(mode=BatchMode) ?progress ?result prover =
  signal ?progress wpo (VCS.name_of_prover prover) ;
  dispatch ?config mode prover wpo >>>
  fun status ->
  let res = match status with
    | Task.Result r -> r
    | Task.Canceled -> VCS.no_result
    | Task.Timeout t -> VCS.timeout t
    | Task.Failed exn -> VCS.failed (error exn)
  in
  let res = { res with solver_time = Wpo.qed_time wpo } in
  update ?result wpo prover res ;
  Task.return (VCS.is_valid res)

let simplify ?start ?result wpo =
  Task.call
    (fun wpo ->
       let r = Wpo.get_result wpo VCS.Qed in
       VCS.( r.verdict == Valid ) ||
       begin
         started ?start wpo ;
         if resolve wpo then
           let time = qed_time wpo in
           let res = VCS.result ~time VCS.Valid in
           (update ?result wpo VCS.Qed res ; true)
         else false
       end)
    wpo

let prove wpo ?config ?mode ?start ?progress ?result prover =
  simplify ?start ?result wpo >>= fun succeed ->
  if succeed
  then Task.return true
  else (run_prover wpo ?config ?mode ?progress ?result prover)

let spawn wpo ~delayed
    ?config ?start ?progress ?result ?success ?pool provers =
  if provers<>[] then
    let do_monitor on_success wpo = function
      | None -> on_success wpo None
      | Some prover ->
          let r = Wpo.get_result wpo VCS.Qed in
          let prover =
            if VCS.( r.verdict == Valid ) then VCS.Qed else prover in
          on_success wpo (Some prover)
    in
    let monitor = match success with
      | None -> None
      | Some on_success -> Some (do_monitor on_success wpo)
    in
    let process (mode,prover) =
      prove wpo ?config ~mode ?start ?progress ?result prover in
    let canceled =
      match success with None -> None | Some f -> Some (fun _ -> f wpo None) in
    ProverTask.spawn ?monitor ?pool
      (List.map (fun mp -> snd mp ,
                           if delayed then Task.later ?canceled process mp
                           else process mp)
         provers)
  else
    let process = simplify ?start ?result wpo in
    let thread = Task.thread process in
    let server = ProverTask.server () in
    Task.spawn server ?pool thread
