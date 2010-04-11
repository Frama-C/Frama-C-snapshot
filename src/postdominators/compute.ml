(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
open Cil
open Db_types
open Db
open Cilutil

module Parameters =
  Plugin.Register
    (struct
      let name = "dominators"
      let shortname = "dominators"
      let descr = "Compute dominators and postdominators of statements"
    end)

module DomSet = struct
  type t = Value of Cilutil.StmtSet.t | Top

  let hash _ = assert false
    (* better no hash function than a hash function as stupid as
       function Top -> 222222 | Value s -> Cilutil.StmtSet.cardinal s
       (iterate on all elements AND have so many collisions as to
       be useless. A WTF-worthy jewel) *)

  let pretty fmt d =
    match d with
    | Top ->
	Format.fprintf fmt "Top"
    | Value d ->
	Pretty_utils.pp_list ~pre:"@[{" ~sep:",@," ~suf:"}@]"
	  (fun fmt s -> Format.fprintf fmt "%d" s.sid)
	  fmt (StmtSet.elements d)

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

module Datatype =
  Project.Datatype.Register
    (struct
       type t = DomSet.t
       let map = DomSet.map
       let copy = map Cil_datatype.StmtSet.copy
       let descr = Project.no_descr
       let name = "postdominator"
     end)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module Dom =
  Cil_computation.IntHashtbl
    (Datatype)
    (struct
       let name = "dominator"
       let dependencies = [ Ast.self ]
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
  let doEdge _ _ d = d
end
module DomCompute = Dataflow.ForwardsDataFlow(DomComputer)

let compute_dom kf =
  let start = Kernel_function.find_first_stmt kf in
  try
    let _ = Dom.find start.sid in
    Parameters.feedback "computed for function %a"
      Kernel_function.pretty_name kf;
  with Not_found ->
    Parameters.feedback "computing for function %a"
      Kernel_function.pretty_name kf;
    let f = kf.fundec in
    let stmts = match f with
    | Definition (f,_) -> f.sallstmts
    | Declaration _ -> Parameters.fatal "cannot compute for a leaf function"
    in
    List.iter (fun s -> Dom.add s.sid DomSet.Top) stmts;
    Dom.replace start.sid (DomSet.Value (StmtSet.singleton start));
    DomCompute.compute [start];
    Parameters.feedback "done for function %a"
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
    (fun k v -> Parameters.result "Stmt:%d@\n%a@\n======" k DomSet.pretty v)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module PostDom =
  Cil_computation.IntHashtbl
    (Datatype)
    (struct
       let name = "postdominator"
       let dependencies = [ Ast.self ]
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
    Parameters.debug "doStmt : %d" stmt.sid;
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
    Parameters.result "(post) computed for function %a"
      Kernel_function.pretty_name kf
  with Not_found ->
    Parameters.feedback "computing (post) for function %a"
      Kernel_function.pretty_name kf;
    let f = kf.fundec in
    let stmts = match f with
      | Definition (f,_) -> f.sallstmts
      | Declaration _ ->
          Parameters.fatal "cannot compute postdominators for a leaf function"
    in
      List.iter (fun s -> PostDom.add s.sid DomSet.Top) stmts;
      PostCompute.compute [return];
      Parameters.feedback "done for function %a"
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
  let disp_all fmt =
    PostDom.iter
      (fun k v -> Format.fprintf fmt "Stmt:%d\n%a\n======" k PostComputer.pretty v)
  in Parameters.result "%t" disp_all

let print_dot_postdom basename kf =
  let filename = basename ^ "." ^ Kernel_function.get_name kf ^ ".dot" in
  Print.build_dot filename kf;
  Parameters.result "(post) dot file generated in %s" filename

let main _fmt = ()

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
