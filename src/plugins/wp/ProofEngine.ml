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

(* -------------------------------------------------------------------------- *)
(* --- Interactive Proof Engine                                           --- *)
(* -------------------------------------------------------------------------- *)

type node = {
  tree : Wpo.t ; (* root, to check consistency *)
  goal : Wpo.t ; (* only GoalAnnot of a sequent *)
  parent : node option ;
  mutable script : script ;
  mutable search_index : int ;
  mutable search_space : Strategy.t array ; (* sorted by priority *)
}

and script =
  | Opened
  | Script of ProofScript.jscript (* to replay *)
  | Tactic of ProofScript.jtactic * node list (* played *)

type tree = {
  main : Wpo.t ; (* Main goal to be proved. *)
  mutable saved : bool ; (* Saved on Disk. *)
  mutable gid : int ; (* WPO goal numbering *)
  mutable head : node option ; (* the current node *)
  mutable root : node option ; (* the root node *)
}

module PROOFS = Model.StaticGenerator(Wpo.S)
    (struct
      type key = Wpo.S.t
      type data = tree
      let name = "Wp.ProofEngine.Proofs"
      let compile main = {
        main ; gid = 0 ;
        head = None ;
        root = None ;
        saved = false ;
      }
    end)

let () = Wpo.on_remove PROOFS.remove

let get wpo =
  try
    let proof = PROOFS.get wpo in
    match proof.root with
    | None | Some { script = Opened | Script _ } -> raise Not_found
    | Some { script = Tactic _ } -> if proof.saved then `Saved else `Proof
  with Not_found ->
    if ProofSession.exists wpo then `Script else `None

(* -------------------------------------------------------------------------- *)
(* --- Constructors                                                       --- *)
(* -------------------------------------------------------------------------- *)

let proof ~main =
  assert (not (Wpo.is_tactic main)) ;
  PROOFS.get main

let rec reset_node n =
  if Wpo.is_tactic n.goal then Wpo.remove n.goal ;
  match n.script with
  | Opened | Script _ -> ()
  | Tactic(_,children) -> List.iter reset_node children

let reset_root = function None -> () | Some n -> reset_node n

let reset t =
  begin
    Wpo.clear_results t.main ;
    reset_root t.root ;
    t.gid <- 0 ;
    t.head <- None ;
    t.root <- None ;
    t.saved <- false ;
  end

let remove w = if PROOFS.mem w then reset (PROOFS.get w)

let saved t = t.saved
let set_saved t s = t.saved <- s

(* -------------------------------------------------------------------------- *)
(* --- Indexing                                                           --- *)
(* -------------------------------------------------------------------------- *)

let rec walk f node =
  if not (Wpo.is_proved node.goal) then
    match node.script with
    | Tactic (_,children) -> List.iter (walk f) children
    | Opened | Script _ -> f node

let pending n =
  let k = ref 0 in
  walk (fun _ -> incr k) n ; !k

let iteri f tree =
  match tree.root with
  | None -> ()
  | Some r ->
      let k = ref 0 in
      walk (fun node -> f !k node ; incr k) r

let validate tree =
  match tree.root with
  | None -> ()
  | Some r ->
      if not (Wpo.is_proved tree.main) then
        let pending = ref false in
        walk (fun _ -> pending := true) r ;
        if not !pending then
          Wpo.set_result tree.main VCS.Tactical VCS.valid

(* -------------------------------------------------------------------------- *)
(* --- Accessors                                                          --- *)
(* -------------------------------------------------------------------------- *)

let main t = t.main
let head t = match t.head with
  | None -> t.main
  | Some n -> n.goal
let goal n = n.goal
let model n = n.goal.Wpo.po_model
let parent n = n.parent
let title n = n.goal.Wpo.po_name
let tactical n =
  match n.script with
  | Tactic(tactic,_) -> Some tactic
  | Opened | Script _ -> None
let get_strategies n = n.search_index , n.search_space
let set_strategies n ?(index=0) hs =
  n.search_index <- index ; n.search_space <- hs
let children n =
  match n.script with
  | Tactic(_,children) -> children
  | Opened | Script _ -> []

(* -------------------------------------------------------------------------- *)
(* --- State & Status                                                     --- *)
(* -------------------------------------------------------------------------- *)

type status = [ `Main | `Proved | `Pending of int ]
type state = [ `Opened | `Proved | `Pending of int | `Script of int ]

let status t : status =
  match t.root with
  | None ->
      if Wpo.is_proved t.main then `Proved else `Main
  | Some root ->
      `Pending (pending root)

let proved n = Wpo.is_proved n.goal
let opened n = not (Wpo.is_proved n.goal)

let state n =
  if Wpo.is_proved n.goal then `Proved else
    match n.script with
    | Opened -> `Opened
    | Script s ->
        begin
          match List.partition ProofScript.is_prover s with
          | [] , s -> `Script (ProofScript.status s)
          | p , [] -> `Pending (ProofScript.status p)
          | provers , scripts ->
              let np = ProofScript.status provers in
              let ns = ProofScript.status scripts in
              `Script( min ns np )
        end
    | Tactic _ -> `Pending (pending n)

(* -------------------------------------------------------------------------- *)
(* --- Navigation                                                         --- *)
(* -------------------------------------------------------------------------- *)

type current = [ `Main | `Internal of node | `Leaf of int * node ]

let current t : current =
  match t.head with
  | Some h ->
      let p = ref (`Internal h) in
      iteri (fun i n -> if n == h then p := `Leaf(i,n)) t ; !p
  | None -> `Main

type position = [ `Main | `Node of node | `Leaf of int ]
let goto t = function
  | `Main ->
      t.head <- t.root
  | `Node n ->
      if n.tree == t.main then t.head <- Some n
  | `Leaf k ->
      t.head <- t.root ;
      iteri (fun i n -> if i = k then t.head <- Some n) t

let fetch t node =
  try walk (fun n -> t.head <- Some n ; raise Exit) node ; false
  with Exit -> true

let rec forward t =
  match t.head with
  | None -> t.head <- t.root
  | Some hd ->
      if not (fetch t hd) then
        begin
          t.head <- hd.parent ;
          forward t ;
        end

let cancel t =
  match t.head with
  | None -> ()
  | Some node ->
      begin
        match node.script with
        | Opened ->
            t.head <- node.parent ;
            if t.head = None then t.root <- None ;
        | Tactic _ | Script _ ->
            (*TODO: save the current script *)
            Wpo.clear_results node.goal ;
            node.script <- Opened ;
      end

(* -------------------------------------------------------------------------- *)
(* --- Sub-Goal                                                           --- *)
(* -------------------------------------------------------------------------- *)

let mk_annot axioms goal vc =
  let open Wpo.VC_Annot in
  match vc with
  | Wpo.GoalAnnot annot -> { annot with goal ; axioms }
  | _ -> {
      axioms ; goal ;
      tags = [] ; warn = [] ;
      deps = Property.Set.empty ;
      path = Cil_datatype.Stmt.Set.empty ;
      effect = None ;
    }

let mk_formula ~main axioms sequent =
  Wpo.(GoalAnnot (mk_annot axioms (GOAL.make sequent) main))

let mk_goal t ~title ~part ~axioms sequent =
  let id = t.gid in t.gid <- succ id ;
  let gid = Printf.sprintf "%s-%d" t.main.Wpo.po_gid id in
  let sid = Printf.sprintf "%s-%d" t.main.Wpo.po_sid id in
  Wpo.({
      po_gid = gid ;
      po_sid = sid ;
      po_name = Printf.sprintf "%s (%s)" title part ;
      po_idx = t.main.po_idx ;
      po_pid = WpPropId.tactical ~gid ;
      po_model = t.main.po_model ;
      po_formula = mk_formula ~main:t.main.po_formula axioms sequent ;
    })

let mk_tree_node ~tree ~anchor goal = {
  tree = tree.main ; goal ;
  parent = Some anchor ;
  script = Opened ;
  search_index = 0 ;
  search_space = [| |] ;
}

let mk_root_node goal = {
  tree = goal ; goal ;
  parent = None ;
  script = Opened ;
  search_index = 0 ;
  search_space = [| |] ;
}

let mk_root ~tree =
  let goal = tree.main in
  let node = mk_root_node goal in
  let root = Some node in
  tree.root <- root ;
  tree.head <- root ;
  node

(* -------------------------------------------------------------------------- *)
(* --- Forking                                                            --- *)
(* -------------------------------------------------------------------------- *)

module Fork =
struct
  type t = {
    tree : tree ;
    anchor : node ;
    tactic : ProofScript.jtactic ;
    goals : Wpo.t list ;
  }

  let create tree ~anchor tactic process =
    let axioms , sequent = Wpo.compute anchor.goal in
    let dseqs = process sequent in
    let title = tactic.ProofScript.header in
    let goals = List.map
        (fun (part,s) -> mk_goal tree ~title ~part ~axioms s) dseqs
    in { tree ; tactic ; anchor ; goals }

  let iter f w = List.iter f w.goals
end

type fork = Fork.t

let fork = Fork.create
let iter = Fork.iter

let anchor tree ?node () =
  match node with
  | Some n -> n
  | None ->
      match tree.head with
      | Some n -> n
      | None ->
          match tree.root with
          | Some n -> n
          | None -> mk_root tree

let non_trivial wpo = not (Wpo.resolve wpo)

let commit fork =
  let residual = List.filter non_trivial fork.Fork.goals in
  let tree = fork.Fork.tree in
  let anchor = fork.Fork.anchor in
  let children = List.map (mk_tree_node ~tree ~anchor) residual in
  tree.saved <- false ;
  anchor.script <- Tactic( fork.Fork.tactic , children ) ;
  anchor , children

(* -------------------------------------------------------------------------- *)
(* --- Scripting                                                          --- *)
(* -------------------------------------------------------------------------- *)

let results wpo =
  List.map (fun (p,r) -> ProofScript.a_prover p r) (Wpo.get_results wpo)

let rec script_node (node : node) =
  let provers = results node.goal in
  let scripts =
    match node.script with
    | Script s -> List.filter ProofScript.is_tactic s
    | Tactic( tactic , children ) ->
        [ ProofScript.a_tactic tactic (List.map script_node children) ]
    | Opened -> []
  in
  provers @ scripts

let script tree =
  match tree.root with
  | None -> results tree.main
  | Some node -> script_node node

let bind node script =
  match node.script with
  | Tactic _ ->
      (*TODO: saveback the thrown script *)
      ()
  | Opened | Script _ ->
      (*TODO: saveback the previous script *)
      node.script <- Script script

let bound node =
  match node.script with
  | Tactic _ | Opened -> []
  | Script s -> s

(* -------------------------------------------------------------------------- *)
