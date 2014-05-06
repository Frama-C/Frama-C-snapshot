(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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

let signal ?callin wpo prover =
  match callin with
    | None -> ()
    | Some f -> f wpo prover

let update ?callback wpo prover result =
  Wpo.set_result wpo prover result ;
  match callback with
    | None -> ()
    | Some f -> f wpo prover result

let run_prover wpo ?(mode=BatchMode) ?callback prover =
  dispatch wpo mode prover >>>
    fun status ->
      let result = match status with
	| Task.Result r -> r
	| Task.Canceled -> VCS.no_result
	| Task.Timeout -> VCS.timeout
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

let simplify ?callin ?callback wpo prover =
  Task.call
    (fun wpo ->
       signal ?callin wpo prover ;
       if resolve wpo then
	 let time = qed_time wpo in
	 let result = VCS.result ~time VCS.Valid in
	 (update ?callback wpo VCS.Qed result ; true)
       else false)
    wpo

let prove wpo ?mode ?callin ?callback prover =
  simplify ?callin ?callback wpo prover >>= fun succeed -> 
    if succeed 
    then Task.return true
    else (run_prover wpo ?mode ?callback prover)
      
let spawn wpo ?callin ?callback provers =
  ProverTask.spawn 
    begin
      List.map
	(fun (mode,prover) ->
	   prove wpo ~mode ?callin ?callback prover)
	provers
    end

(* ------------------------------------------------------------------------ *)
(* ---  Why3ide                                                         --- *)
(* ------------------------------------------------------------------------ *)

module String = Datatype.String

(** Different instance of why3ide can't be run simultanely *)
let why3ide_running = ref false

(** Update Wpo from Sessions *)
let update_wpo_from_session ?callback ~goals ~session:filename_session () =
  let open ProverWhy3 in
  let open Why3_session in
  let module HStr = String.Hashtbl in
  let session = read_session filename_session in
  Wpo.S.Hashtbl.iter (fun wpo g ->
    match g with
    | None -> (* proved by QED *)
      let time = qed_time wpo in
      let result = VCS.result ~time VCS.Valid in
      update ?callback wpo VCS.Qed result;
      update ?callback wpo VCS.Why3ide (VCS.result VCS.NoResult)
    | Some g ->
    try
      let filename = Sysutil.relativize_filename filename_session g.gfile in
      let file   = HStr.find session.session_files filename in
      let theory = HStr.find file.file_theories g.gtheory in
      let goal   = HStr.find theory.theory_goals g.ggoal in
      let result = VCS.result
        (if goal.goal_verified then VCS.Valid else VCS.NoResult) in
      update ?callback wpo VCS.Why3ide result
    with Not_found ->
      if Wp_parameters.has_dkey "prover" then
        Wp_parameters.feedback
          "[WP.Why3ide] a goal normally present in generated file \
            is not present in the session: %s %s %s@."
          g.gfile g.gtheory g.ggoal;
      update ?callback wpo VCS.Why3ide (VCS.result VCS.NoResult)
  ) goals;
  why3ide_running := false

let wp_why3ide ?callback iter =
  let includes = String.Hashtbl.create 2 in
  let files    = String.Hashtbl.create 5 in
  let goals    = Wpo.S.Hashtbl.create 24 in
  let on_goal wpo  =
    match  ProverWhy3.assemble_wpo wpo with
    | None ->
      Wpo.S.Hashtbl.add goals wpo None;
    | Some (incs,goal) ->
    Wpo.S.Hashtbl.add goals wpo (Some goal);
    List.iter (fun f -> String.Hashtbl.replace includes f ()) incs;
    String.Hashtbl.replace files goal.ProverWhy3.gfile ()
  in
  iter on_goal;
  let dir = Wp_parameters.get_output () in
  let session = Format.sprintf "%s/project.session" dir in
  let get_value h = String.Hashtbl.fold_sorted (fun s () acc -> s::acc) h [] in
  let includes = get_value includes in
  let files = get_value files in
  if files = [] then (why3ide_running := false; Task.nop)
  else
    begin
      ProverWhy3.call_ide ~includes ~files ~session >>=
	fun ok -> begin
	  if ok then begin
            try update_wpo_from_session ?callback ~goals ~session ()
            with Why3_session.LoadError ->
              Wp_parameters.error
                "[WP] why3session: can't import back why3 results because of \
                   previous error"
          end;
          Task.return ()
        end
    end

let wp_why3ide ?callback iter =
  if !why3ide_running
  then begin
    Wp_parameters.feedback "Why3ide is already running. Close it before \
                            starting other tasks for it.";
    Task.nop
  end
  else begin
    why3ide_running := true;
    wp_why3ide ?callback iter
  end
