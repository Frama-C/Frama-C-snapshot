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
(* --- Running Json-Tactical                                              --- *)
(* -------------------------------------------------------------------------- *)

let jconfigure (console : #Tactical.feedback) jtactic goal =
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

let jfork tree ?node jtactic =
  let console = new ProofScript.console ~title:jtactic.header in
  try
    let anchor = ProofEngine.anchor tree ?node () in
    let goal = ProofEngine.goal anchor in
    let model = ProofEngine.node_model anchor in
    match Model.with_model model (jconfigure console jtactic) goal with
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
    progress : Wpo.t -> string -> unit ;
    result : Wpo.t -> VCS.prover -> VCS.result -> unit ;
    success : Wpo.t -> VCS.prover option -> unit ;
    depth : int ;
    width : int ;
    auto : Strategy.heuristic list ;
    mutable signaled : bool ;
    mutable backtrack : int ;
    mutable backtracking : backtracking option ;
  }
  
  and backtracking = {
    bk_node : ProofEngine.node ;
    bk_depth : int ; (* depth of search *)
    mutable bk_best : int ;    (* best index, (-1) for none *)
    mutable bk_pending : int ; (* best pending, max_int when none *)
  }

  let tree env = env.tree

  let play env res =
    if VCS.is_valid res then env.valid else env.failed

  let progress env msg = env.progress (ProofEngine.main env.tree) msg

  let stuck env =
    if not env.signaled then
      begin
        ProofEngine.validate ~unknown:true env.tree ;
        env.success (ProofEngine.main env.tree) None ;
        env.signaled <- true ;
      end

  let validate ?(finalize=false) env =
    ProofEngine.validate ~unknown:true env.tree ;
    if not env.signaled then
      let wpo = ProofEngine.main env.tree in
      let proved = Wpo.is_proved wpo in
      if proved || finalize then
        begin
          env.signaled <- true ;
          List.iter
            (fun (prv,res) -> env.result wpo prv res)
            (Wpo.get_results wpo) ;
          env.success wpo (if proved then Some VCS.Tactical else None)
        end

  let goal env = function
    | Some n -> ProofEngine.goal n
    | None -> ProofEngine.main env.tree

  let prove env wpo ?config prover =
    Prover.prove wpo ?config ~mode:VCS.BatchMode
      ~progress:env.progress prover

  let pending env =
    match ProofEngine.status env.tree with
    | `Main | `Proved -> 0 | `Pending n -> n

  let setup_backtrack env node depth =
    if env.backtrack > 0 then
      let is_nearer = match env.backtracking with
        | None -> true
        | Some { bk_depth } -> depth < bk_depth in
      if is_nearer then
        let _,hs = ProofEngine.get_strategies node in
        if Array.length hs > 1 then
          env.backtracking <- Some {
              bk_node = node ;
              bk_best = (-1) ;
              bk_depth = depth ;
              bk_pending = pending env ;
            }
  
  let search env node ~depth =
    if env.auto <> [] && depth < env.depth && pending env < env.width
    then
      match ProverSearch.search env.tree ~anchor:node env.auto with
      | None -> None
      | Some _ as fork -> setup_backtrack env node depth ; fork
    else None

  let backtrack env =
    if env.backtrack <= 0 then None else
      match env.backtracking with
      | None -> None
      | Some point ->
          let n = pending env in
          let anchor = point.bk_node in
          if n < point.bk_pending then
            begin
              point.bk_best <- fst (ProofEngine.get_strategies anchor) ;
              point.bk_pending <- n ;
            end ;
          match ProverSearch.backtrack env.tree ~anchor ~loop:false () with
          | Some fork ->
              env.backtracking <- None ; Some (point.bk_depth,fork)
          | None -> (* end of backtrack *)
              env.backtracking <- None ;
              match ProverSearch.index env.tree ~anchor ~index:point.bk_best
              with None -> None | Some fork -> Some (point.bk_depth,fork)

  let provers env = env.provers
  
  let make tree
      ~valid ~failed ~provers
      ~depth ~width ~backtrack ~auto
      ~progress ~result ~success =
    { tree ; valid ; failed ; provers ;
      depth ; width ; backtrack ; auto ;
      progress ; result ; success ;
      backtracking = None ;
      signaled = false }

end

(* -------------------------------------------------------------------------- *)
(* --- Choosing Alternatives                                              --- *)
(* -------------------------------------------------------------------------- *)

let fst_order _ _ = 0
let key_order (a,_) (b,_) = String.compare a b

let rec zip order nodes scripts =
  match nodes , scripts with
  | _ , [] | [] , _ -> (*TODO: saveback forgiven scripts *) ()
  | node :: o_nodes , script :: o_scripts ->
      let cmp = order node script in
      if cmp < 0 then zip order o_nodes scripts else
      if cmp > 0 then zip order nodes o_scripts else
        (ProofEngine.bind (snd node) (snd script) ;
         zip order o_nodes o_scripts)

let reconcile nodes scripts =
  if List.for_all (fun (k,_) -> k = "") scripts 
  then zip fst_order nodes scripts
  else zip key_order
      (List.stable_sort key_order nodes)
      (List.stable_sort key_order scripts)

let rec forall phi = function
  | x::xs ->
      phi x >>= fun ok ->
      if ok then forall phi xs else Task.return false
  | [] -> Task.return true

let rec exists phi = function
  | x::xs ->
      phi x >>= fun ok ->
      if ok then Task.return true else exists phi xs
  | [] -> Task.return false

let prove_node env node prv =
  let wpo = Env.goal env (Some node) in
  if not (VCS.is_verdict (Wpo.get_result wpo prv)) then
    Env.prove env wpo prv
  else Task.return false

(* -------------------------------------------------------------------------- *)
(* --- Auto & Seach Mode                                                  --- *)
(* -------------------------------------------------------------------------- *)

let rec auto env ?(depth=0) node : bool Task.task =
  exists (prove_node env node) (Env.provers env) >>= fun ok ->
  if ok then Task.return true else
    if depth > 0 then
      autosearch env ~depth node
    else
      begin
        autosearch env ~depth node >>= fun ok ->
        if ok then Task.return true else
          match Env.backtrack env with
          | Some (depth,fork) ->
              Env.progress env "Backtracking" ;
              autofork env ~depth fork
  | None ->
      Task.return false
      end

and autosearch env ~depth node : bool Task.task =
  match Env.search env node ~depth with
      | None -> Task.return false
  | Some fork -> autofork env ~depth fork

and autofork env ~depth fork =
          let _,children = ProofEngine.commit ~resolve:true fork in
          let pending = Env.pending env in
          if pending > 0 then
            begin
              Env.progress env (Printf.sprintf "Auto %d" pending) ;
              let depth = succ depth in
              forall (auto env ~depth) (List.map snd children)
            end
          else
    ( Env.validate env ; Task.return true )

(* -------------------------------------------------------------------------- *)
(* --- Script Crawling                                                    --- *)
(* -------------------------------------------------------------------------- *)

let rec crawl env on_child node = function
  
  | [] ->
      let node = ProofEngine.anchor (Env.tree env) ?node () in
      auto env node >>= fun ok ->
      if ok then Env.validate env else Env.stuck env ;
      Task.return ()
        
  | Error(msg,json) :: alternative ->
      Wp_parameters.error "@[<hov 2>Script Error %S: %a@]@." msg Json.pp json ;
      crawl env on_child node alternative

  | Prover( prv , res ) :: alternative ->
      begin
        let task =
          if Env.play env res then
            let wpo = Env.goal env node in
            let config = VCS.configure res in
            Env.prove env wpo ~config prv
          else Task.return false in
        let continue ok =
          if ok
          then (Env.validate env ; Task.return ())
          else crawl env on_child node alternative
        in
        task >>= continue
      end

  | Tactic( _ , jtactic , subscripts ) :: alternative ->
      begin
        match jfork (Env.tree env) ?node jtactic with
        | None ->
            Wp_parameters.error
              "Script Error: can not apply '%s'@\n\
               @[<hov 2>Params: %a@]@\n\
               @[<hov 2>Select: %a@]@."
              jtactic.tactic
              Json.pp jtactic.params
              Json.pp jtactic.select ;
            crawl env on_child node alternative
        | Some fork ->
            (*TODO: saveback forgiven script *)
            let _,children = ProofEngine.commit ~resolve:true fork in
            reconcile children subscripts ;
            if children = [] then
              Env.validate env
            else
              List.iter (fun (_,n) -> on_child n) children ;
            Task.return ()
      end

(* -------------------------------------------------------------------------- *)
(* --- Main Process                                                       --- *)
(* -------------------------------------------------------------------------- *)

let schedule job =
  Task.spawn (ProverTask.server ()) (Task.thread (Task.todo job))

let rec process env node =
  schedule
    begin fun () ->
      let script = Priority.sort (ProofEngine.bound node) in
      crawl env (process env) (Some node) script
    end

let task
    ~valid ~failed ~provers
    ~depth ~width ~backtrack ~auto
    ~start ~progress ~result ~success wpo =
  begin fun () ->
    start wpo ;
    let json = ProofSession.load wpo in
    let script = Priority.sort (ProofScript.decode json) in
    let tree = ProofEngine.proof ~main:wpo in
    let env = Env.make tree
        ~valid ~failed ~provers
        ~depth ~width ~backtrack ~auto
        ~progress ~result ~success in
    crawl env (process env) None script >>?
    (fun _ -> ProofEngine.forward tree) ;
  end

(* -------------------------------------------------------------------------- *)
(* --- Main Entry Points                                                  --- *)
(* -------------------------------------------------------------------------- *)

type 'a process =
  ?valid:bool -> ?failed:bool -> ?provers:VCS.prover list ->
  ?depth:int -> ?width:int -> ?backtrack:int ->
  ?auto:Strategy.heuristic list ->
  ?start:(Wpo.t -> unit) ->
  ?progress:(Wpo.t -> string -> unit) ->
  ?result:(Wpo.t -> VCS.prover -> VCS.result -> unit) ->
  ?success:(Wpo.t -> VCS.prover option -> unit) ->
  Wpo.t -> 'a

let skip1 _ = ()
let skip2 _ _ = ()
let skip3 _ _ _ = ()
let prove
    ?(valid = true) ?(failed = true) ?(provers = [])
    ?(depth = 0) ?(width = 0) ?(backtrack = 0) ?(auto = [])
    ?(start = skip1) ?(progress = skip2) ?(result = skip3) ?(success = skip2)
    wpo =
  Task.todo (task
               ~valid ~failed ~provers
               ~depth ~width ~backtrack ~auto
               ~start ~progress ~result ~success wpo)

let spawn
    ?(valid = true) ?(failed = true) ?(provers = [])
    ?(depth = 0) ?(width = 0) ?(backtrack = 0) ?(auto = [])
    ?(start = skip1) ?(progress = skip2) ?(result = skip3) ?(success = skip2)
    wpo =
  schedule (task
              ~valid ~failed ~provers
              ~depth ~width ~backtrack ~auto
              ~start ~progress ~result ~success wpo)


let search
    ?(depth = 0) ?(width = 0) ?(backtrack = 0) ?(auto = []) ?(provers = [])
    ?(progress = skip2) ?(result = skip3) ?(success = skip2)
    tree node =
  begin
    let env = Env.make tree
        ~valid:false ~failed:false ~provers
        ~depth ~width ~backtrack ~auto
        ~progress ~result ~success in
    schedule
      begin fun () ->
        autosearch env ~depth:0 node >>=
        fun ok ->
        if ok then Env.validate ~finalize:true env else Env.stuck env ;
        Task.return ()
      end
  end

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
