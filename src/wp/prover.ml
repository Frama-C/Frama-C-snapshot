(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
(*    CEA (Commissariat a l'énergie atomique et aux énergies              *)
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

(* -------------------------------------------------------------------------- *)
(* --- Prover Implementation against Task API                             --- *)
(* -------------------------------------------------------------------------- *)

open Task
open VCS
open Wpo

let dispatch wpo ~interactive prover =
  match wpo.po_formula with
    | Legacy vcd -> ProverVCD.prove wpo vcd ~interactive prover
    | GoalLemma _ | GoalAnnot _ ->
	begin
	  match prover with
	    | AltErgo -> ProverErgo.prove wpo ~interactive
	    | Coq -> ProverCoq.prove wpo ~interactive
	    | _ -> Task.failed "Prover '%a' not available"
		VCS.pp_prover prover
	end

let run_prover wpo ~interactive ?callback prover =
  dispatch wpo ~interactive prover >>>
    fun status ->
      let result = match status with
	| Task.Result r -> r
	| Task.Canceled -> VCS.no_result
	| Task.Timeout -> VCS.timeout
	| Task.Failed (Failure msg) -> VCS.failed msg
	| Task.Failed (Sys_error msg) -> VCS.failed msg
	| Task.Failed (Unix.Unix_error(e,_,"")) -> VCS.failed (Unix.error_message e)
	| Task.Failed (Unix.Unix_error(e,_,p)) -> 
	    VCS.failed (Printf.sprintf "%s (%s)" (Unix.error_message e) p)
	| Task.Failed exn -> VCS.failed (Printexc.to_string exn)
      in
      Wpo.set_result wpo prover result ;
      ( match callback with None -> () | Some f -> f wpo prover result ) ;
      Task.return (Wpo.is_valid result)

let simplified wpo =
  match wpo.po_formula with
    | Legacy _ -> false
    | GoalAnnot vcq -> VC_Annot.is_simplified vcq 
    | GoalLemma vca -> VC_Lemma.is_trivial vca

let simplify wpo =
  Task.call
    (fun wpo ->
       if simplified wpo then
	 (Wpo.set_result wpo VCS.WP VCS.valid ; true)
       else false) 
    wpo

let prove wpo ?(interactive=false) ?callback prover =
  simplify wpo >>= fun succeed -> 
    if succeed 
    then Task.return true
    else (run_prover wpo ~interactive ?callback prover)
      
let spawn wpo ?callback provers =
  ProverTask.spawn 
    begin
      List.map
	(fun (interactive,prover) ->
	   prove wpo ~interactive ?callback prover)
	provers
    end

(* -------------------------------------------------------------------------- *)
