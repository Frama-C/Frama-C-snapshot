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

open Tactical
open ProofScript

(* -------------------------------------------------------------------------- *)
(* --- Alternatives Ordering                                              --- *)
(* -------------------------------------------------------------------------- *)

module Priority =
struct
  open VCS

  let stage = function
    | Prover( Qed , { verdict = Valid } ) -> 0
    | Prover( (AltErgo | Why3 _) , { verdict = Valid } ) -> 1
    | Prover( Coq , { verdict = Valid } ) -> 2
    | Tactic _ -> 3
    | Prover _ -> 4
    | Error _ -> 5

  let time = function
    | Tactic _ | Error _ -> 0.0
    | Prover( _ , r ) -> r.prover_time +. r.solver_time

  let compare a b =
    let sa = stage a in
    let sb = stage b in
    if sa = sb
    then Pervasives.compare (time a) (time b)
    else sa - sb

  let sort script = List.stable_sort compare script

end

(* -------------------------------------------------------------------------- *)
(* --- Running Tactical                                                   --- *)
(* -------------------------------------------------------------------------- *)

let configure (console : #Tactical.feedback) jtactic goal =
  let _ , sequent = Wpo.compute goal in
  match ProofScript.configure jtactic sequent with
  | None -> None
  | Some(tactical,selection) ->
      console#set_title "%s" tactical#title ;
      let verdict =
        try tactical#select console selection
        with Not_found | Exit -> Not_applicable
      in
      begin
        match verdict with
        | Applicable process when not console#has_error ->
            let title = tactical#title in
            let script = ProofScript.jtactic ~title tactical selection in
            Some (script , process)
        | _ -> None
      end

let fork tree ?node tactic =
  let console = new ProofScript.console ~title:tactic.header in
  try
    let anchor = ProofEngine.anchor tree ?node () in
    let goal = ProofEngine.goal anchor in
    let model = ProofEngine.model anchor in
    match Model.with_model model (configure console tactic) goal with
    | None -> None
    | Some (script,process) ->
        Some (ProofEngine.fork tree ~anchor script process)
  with
  | Not_found ->
      console#set_error "Can not configure tactic" ; None
  | e ->
      console#set_error "Exception <%s>" (Printexc.to_string e) ;
      raise e

(* -------------------------------------------------------------------------- *)
(* --- Running Alternatives                                               --- *)
(* -------------------------------------------------------------------------- *)

open Task

module Env =
struct
  type t = {
    tree : ProofEngine.tree ;
    valid : bool ; (* play valid provers *)
    failed : bool ; (* play failed provers *)
    provers : VCS.prover list ;
    stuck : unit -> unit ;
    proved : unit -> unit ;
    callin : Wpo.t -> VCS.prover -> unit ;
    callback : Wpo.t -> VCS.prover -> VCS.result -> unit ;
    success : Wpo.t -> VCS.prover option -> unit ;
    mutable signaled : bool ;
  }

  let tree env = env.tree

  let play env res =
    if VCS.is_valid res then env.valid else env.failed

  let stuck env =
    if not env.signaled then
      (env.stuck () ; env.signaled <- true)

  let signal env =
    ProofEngine.validate env.tree ;
    if not env.signaled && Wpo.is_proved (ProofEngine.main env.tree) then
      (env.proved () ; env.signaled <- true)

  let goal env = function
    | Some n -> ProofEngine.goal n
    | None -> ProofEngine.main env.tree

  let prove env wpo ?config prover =
    Prover.prove wpo ?config ~mode:VCS.BatchMode
      ~callin:env.callin ~callback:env.callback prover

  let make tree ~valid ~failed ~provers ~callin ~callback ~success =
    let wpo = ProofEngine.main tree in
    let stuck () = success wpo None in
    let proved () =
      ProofEngine.set_saved tree true ;
      success wpo (Some VCS.Tactical)
    in
    { tree ; valid ; failed ; provers ;
      stuck ; callin ; callback ; success ; proved ;
      signaled = false }

end

let rec zip nodes scripts  =
  match nodes , scripts with
  | _ , [] -> ()
  | [] , _ -> (*TODO: saveback forgiven scripts *) ()
  | n::wn , s::ws -> ProofEngine.bind n s ; zip wn ws

let rec fallback env child node = function
  | [] -> Env.stuck env ; Task.return ()
  | prv :: prvs ->
      let task =
        let wpo = Env.goal env node in
        if not (VCS.is_verdict (Wpo.get_result wpo prv)) then
          Env.prove env wpo prv
        else Task.return false in
      let continue ok =
        if ok
        then (Env.signal env ; Task.return ())
        else fallback env child node prvs
      in task >>= continue

let rec crawl env child node = function
  | [] -> fallback env child node env.Env.provers
  | Error(msg,json) :: script ->
      Wp_parameters.error "@[<hov 2>SCRIPT ERROR %S: %a@]@." msg Json.pp json ;
      crawl env child node script
  | Prover( prv , res ) :: script ->
      begin
        let task =
          if Env.play env res then
            let wpo = Env.goal env node in
            let config = VCS.configure res in
            Env.prove env wpo ~config prv
          else Task.return false in
        let continue ok =
          if ok
          then (Env.signal env ; Task.return ())
          else crawl env child node script
        in
        task >>= continue
      end
  | Tactic( _ , tactic , subscripts ) :: script ->
      begin
        match fork (Env.tree env) ?node tactic with
        | None ->
            Wp_parameters.error
              "SCRIPT ERROR: can not apply '%s'@\n\
               @[<hov 2>Params: %a@]@\n\
               @[<hov 2>Select: %a@]@."
              tactic.tactic
              Json.pp tactic.params
              Json.pp tactic.select ;
            crawl env child node script
        | Some fork ->
            (*TODO: saveback forgiven script *)
            let _,children = ProofEngine.commit fork in
            zip children subscripts ;
            if children = [] then
              Env.signal env
            else
              List.iter child children ;
            Task.return ()
      end

let schedule job =
  Task.spawn (ProverTask.server ()) (Task.thread (Task.todo job))

let rec process env node =
  schedule
    begin fun () ->
      let script = Priority.sort (ProofEngine.bound node) in
      crawl env (process env) (Some node) script
    end

let task ~valid ~failed ~provers ~start ~callin ~callback ~success wpo =
  begin fun () ->
    start wpo ;
    let json = ProofSession.load wpo in
    let script = Priority.sort (ProofScript.decode json) in
    let tree = ProofEngine.proof ~main:wpo in
    let env = Env.make tree ~valid ~failed ~provers ~callin ~callback ~success in
    crawl env (process env) None script >>? (fun _ -> ProofEngine.forward tree) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Entry Points                                                  --- *)
(* -------------------------------------------------------------------------- *)

type 'a process =
  ?valid:bool ->
  ?failed:bool ->
  ?provers:VCS.prover list ->
  ?start:(Wpo.t -> unit) ->
  ?callin:(Wpo.t -> VCS.prover -> unit) ->
  ?callback:(Wpo.t -> VCS.prover -> VCS.result -> unit) ->
  ?success:(Wpo.t -> VCS.prover option -> unit) ->
  Wpo.t -> 'a

let skip1 _ = ()
let skip2 _ _ = ()
let skip3 _ _ _ = ()
let prove
    ?(valid = true)
    ?(failed = true)
    ?(provers = [])
    ?(start = skip1)
    ?(callin = skip2)
    ?(callback = skip3)
    ?(success = skip2)
    wpo =
  Task.todo (task ~valid ~failed ~provers ~start ~callin ~callback ~success wpo)

let spawn
    ?(valid = true)
    ?(failed = true)
    ?(provers = [])
    ?(start = skip1)
    ?(callin = skip2)
    ?(callback = skip3)
    ?(success = skip2)
    wpo =
  schedule (task ~valid ~failed ~provers ~start ~callin ~callback ~success wpo)

(* -------------------------------------------------------------------------- *)
(* --- Save Session                                                       --- *)
(* -------------------------------------------------------------------------- *)

let proofs = Hashtbl.create 32
let has_proof wpo =
  let wid = wpo.Wpo.po_gid in
  try Hashtbl.find proofs wid
  with Not_found ->
    if ProofSession.exists wpo then
      let ok = 
        try
          let script = ProofScript.decode (ProofSession.load wpo) in
          ProofScript.status script = 0
        with _ -> false in
      (Hashtbl.add proofs wid ok ; ok)
    else false

let save wpo =
  let script = ProofEngine.script (ProofEngine.proof ~main:wpo) in
  Hashtbl.remove proofs wpo.Wpo.po_gid ;
  ProofSession.save wpo (ProofScript.encode script)

let get wpo =
  match ProofEngine.get wpo with
  | `None -> `None
  | `Proof -> `Proof
  | `Saved -> `Saved
  | `Script -> if has_proof wpo then `Script else `Proof

(* -------------------------------------------------------------------------- *)
