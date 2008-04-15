(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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
open Cil
open Db_types
open Db
open Cilutil

module DomSet = struct
  type t = Value of Cilutil.StmtSet.t | Top

  let hash = function Top -> 222222 | Value s -> Cilutil.StmtSet.cardinal s

  let pretty fmt d =
    match d with
    | Top ->
	Format.fprintf fmt "Top"
    | Value d ->
	Format.fprintf fmt "{%a}"
          (Cil.fprintfList ~sep:","
	     (fun fmt s -> Format.fprintf fmt "%d" s.sid))
          (StmtSet.elements d)

  let inter a b = match a,b with
    | Top,Top -> Top
    | Value v, Top | Top, Value v -> Value v
    | Value v, Value v' -> Value (StmtSet.inter v v')

  let equal a b = match a,b with
    | Top,Top -> true
    | Value _v, Top | Top, Value _v -> false
    | Value v, Value v' -> StmtSet.equal v v'

  let add v d = match d with
    | Top -> Top
    | Value d -> Value (StmtSet.add v d)

  let mem v = function
    | Top -> true
    | Value d -> StmtSet.mem v d

  let map f = function
    | Top -> Top
    | Value set -> Value (f set)
end

module Datatype = struct
  include Project.Datatype.Register
    (struct
       type t = DomSet.t
       let map = DomSet.map
       let copy = map Cil_datatype.StmtSet.copy
       let rehash = map Cil_datatype.StmtSet.rehash
       let name = "postdominator"
     end)
  let () = register_comparable ~hash ~equal ()
end

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module Dom =
  Cil_computation.IntHashtbl
    (Datatype)
    (struct
       let name = "dominator"
       let dependencies = [ Cil_state.self ]
       let size = 503
     end)

module DomComputer = struct

  let name = "dominators"
  let debug = ref false
  type t = DomSet.t
  module StmtStartData = Dom
  let pretty = DomSet.pretty
  let copy (s:t) = s

  let computeFirstPredecessor _stmt _set = assert false

  let combinePredecessors stmt ~old new_ =
    let new_ = DomSet.add stmt new_ in
    let set = DomSet.inter old new_ in
    if DomSet.equal set old then None else Some set

  let doStmt _stmt _set =  Dataflow.SDefault

  let doInstr stmt _instr set =
    Dataflow.Done (DomSet.add stmt set)

  let stmt_can_reach _ _ = true
  let filterStmt _stmt = true
  let doGuard _ _ _ = Dataflow.GDefault
end
module DomCompute = Dataflow.ForwardsDataFlow(DomComputer)

let compute_dom kf =
  let start = Kernel_function.find_first_stmt kf in
  try 
    let _ = Dom.find start.sid in
      Cil.log "[dominators] computed for function %a"
        Kernel_function.pretty_name kf;
  with Not_found ->
    Cil.log "[dominators] computing for function %a"
      Kernel_function.pretty_name kf;
    let f = kf.fundec in
    let stmts = match f with
      | Definition (f,_) -> f.sallstmts
      | Declaration _ -> invalid_arg
                           "[dominators] cannot compute for a leaf function"
    in
      List.iter (fun s -> Dom.add s.sid DomSet.Top) stmts;
      Dom.replace start.sid (DomSet.Value (StmtSet.singleton start));
      DomCompute.compute [start];
      Cil.log "[dominators] done for function %a"
        Kernel_function.pretty_name kf

let get_stmt_dominators f stmt =
  let do_it () = Dom.find stmt.sid in
  try do_it ()
  with Not_found -> compute_dom f; do_it ()

let stmt_dominators f stmt =
    match get_stmt_dominators f stmt with
    | DomSet.Value s -> s
    | DomSet.Top -> raise Db.Dominators.Top

let is_dominator f ~opening ~closing =
  let dominators = get_stmt_dominators f closing in
  DomSet.mem opening dominators

let display_dom () =
  Dom.iter
    (fun k v -> Format.printf "Stmt:%d\n%a\n======" k DomSet.pretty v)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module PostDom =
  Cil_computation.IntHashtbl
    (Datatype)
    (struct
       let name = "postdominator"
       let dependencies = [ Cil_state.self ]
       let size = 503
     end)

module PostComputer = struct

  let name = "postdominator"
  let debug = ref false

  type t = DomSet.t
  module StmtStartData = PostDom

  let pretty = DomSet.pretty

  let combineStmtStartData _stmt ~old new_ =
    let result = (* inter old *) new_ in
    if DomSet.equal result old then None else
      Some result

  let combineSuccessors = DomSet.inter

  let doStmt stmt =
    !Db.progress ();
    if !debug then Cil.log "doStmt : %d\n" stmt.sid;
        match stmt.skind with
          | Return _ -> Dataflow.Done (DomSet.Value (StmtSet.singleton stmt))
          | _ -> Dataflow.Post (fun data -> DomSet.add stmt data)

  let doInstr _ _ _ = Dataflow.Default

  let filterStmt _stmt _next = true

  let funcExitData = DomSet.Value StmtSet.empty

end
module PostCompute = Dataflow.BackwardsDataFlow(PostComputer)

let compute_postdom kf =
  let return = Kernel_function.find_return kf in
  try 
    let _ = PostDom.find return.sid in 
    Cil.log "[postdominators] computed for function %a"
      Kernel_function.pretty_name kf
  with Not_found ->
    Cil.log "[postdominators] computing for function %a"
      Kernel_function.pretty_name kf;
    let f = kf.fundec in
    let stmts = match f with
      | Definition (f,_) -> f.sallstmts
      | Declaration _ -> 
          invalid_arg "[postdominators] cannot compute for a leaf function"
    in
      List.iter (fun s -> PostDom.add s.sid DomSet.Top) stmts;
      PostCompute.compute [return];
      Cil.log "[postdominators] done for function %a"
        Kernel_function.pretty_name kf


let get_stmt_postdominators f stmt =
  let do_it () = PostDom.find stmt.sid in
  try do_it ()
  with Not_found -> compute_postdom f; do_it ()

(** @raise Db.Top_postdominators when the statement postdominators
* have not been computed ie neither the return statement is reachable,
* nor the statement is in a natural loop. *)
let stmt_postdominators f stmt =
    match get_stmt_postdominators f stmt with
    | DomSet.Value s -> s
    | DomSet.Top -> raise Db.Postdominators.Top

let is_postdominator f ~opening ~closing =
  let open_postdominators = get_stmt_postdominators f opening in
  DomSet.mem closing open_postdominators

let display_postdom () =
  PostDom.iter
    (fun k v -> Format.printf "Stmt:%d\n%a\n======" k PostComputer.pretty v)

let print_dot_postdom basename kf =
  let filename = basename ^ "." ^ Kernel_function.get_name kf ^ ".dot" in
    Print.build_dot filename kf;
    Format.printf "[postdominators] dot file generated in %s@\n" filename

let main _fmt =
  if Cmdline.Pdg.DotPostdomBasename.get () <> "" then begin
    let base = Cmdline.Pdg.DotPostdomBasename.get () in
    let print kf = !Db.Postdominators.print_dot base kf in
    !Db.Semantic_Callgraph.topologically_iter_on_functions print
  end

let () = Db.Main.extend main

let () = Db.Dominators.compute := compute_dom
let () = Db.Dominators.is_dominator := is_dominator
let () = Db.Dominators.stmt_dominators := stmt_dominators
let () = Db.Dominators.display := display_dom

let () = Db.Postdominators.compute := compute_postdom
let () = Db.Postdominators.is_postdominator := is_postdominator
let () = Db.Postdominators.stmt_postdominators := stmt_postdominators
let () = Db.Postdominators.display := display_postdom
let () = Db.Postdominators.print_dot := print_dot_postdom

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
