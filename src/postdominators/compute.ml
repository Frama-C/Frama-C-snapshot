(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
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
open Db
open Cil_datatype

module DomKernel =
  Plugin.Register
    (struct
      let name = "dominators"
      let shortname = "dominators"
      let help = "Compute dominators and postdominators of statements"
    end)

module DomSet = struct

  type domset = Value of Stmt.Hptset.t | Top

  let inter a b = match a,b with
    | Top,Top -> Top
    | Value v, Top | Top, Value v -> Value v
    | Value v, Value v' -> Value (Stmt.Hptset.inter v v')

  let add v d = match d with
    | Top -> Top
    | Value d -> Value (Stmt.Hptset.add v d)

  let mem v = function
    | Top -> true
    | Value d -> Stmt.Hptset.mem v d

  let map f = function
    | Top -> Top
    | Value set -> Value (f set)

  include Datatype.Make
      (struct
        include Datatype.Serializable_undefined
        type t = domset
        let name = "dominator_set"
        let reprs = Top :: List.map (fun s -> Value s) Stmt.Hptset.reprs
        let structural_descr =
          Structural_descr.Structure
            (Structural_descr.Sum [| [| Stmt.Hptset.packed_descr |] |])
        let pretty fmt = function
          | Top -> Format.fprintf fmt "Top"
          | Value d ->
            Pretty_utils.pp_iter ~pre:"@[{" ~sep:",@," ~suf:"}@]"
              Stmt.Hptset.iter
              (fun fmt s -> Format.fprintf fmt "%d" s.sid)
              fmt d
        let equal a b = match a,b with
          | Top,Top -> true
          | Value _v, Top | Top, Value _v -> false
          | Value v, Value v' -> Stmt.Hptset.equal v v'
        let copy = map Cil_datatype.Stmt.Hptset.copy
        let mem_project = Datatype.never_any_project
       end)

end

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module Dom =
  Cil_state_builder.Inthash
    (DomSet)
    (struct
       let name = "dominator"
       let dependencies = [ Ast.self ]
       let size = 503
       let kind = `Correctness
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
  let doGuard _ _ _ = Dataflow.GDefault, Dataflow.GDefault
  let doEdge _ _ d = d
end
module DomCompute = Dataflow.ForwardsDataFlow(DomComputer)

let compute_dom kf =
  let start = Kernel_function.find_first_stmt kf in
  try
    let _ = Dom.find start.sid in
    DomKernel.feedback ~level:2 "computed for function %a"
      Kernel_function.pretty kf;
  with Not_found ->
    DomKernel.feedback ~level:2 "computing for function %a"
      Kernel_function.pretty kf;
    let f = kf.fundec in
    let stmts = match f with
    | Definition (f,_) -> f.sallstmts
    | Declaration _ ->
      DomKernel.fatal "cannot compute for a leaf function %a"
        Kernel_function.pretty kf
    in
    List.iter (fun s -> Dom.add s.sid DomSet.Top) stmts;
    Dom.replace start.sid (DomSet.Value (Stmt.Hptset.singleton start));
    DomCompute.compute [start];
    DomKernel.feedback ~level:2 "done for function %a"
      Kernel_function.pretty kf

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
    (fun k v -> DomKernel.result "Stmt:%d@\n%a@\n======" k DomSet.pretty v)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

module type MakePostDomArg = sig
  val is_accessible: stmt -> bool
  (* Evaluation of an expression which is supposed to be the condition of an
     'if'. The first boolean (resp. second) represents the possibility that
     the expression can be non-zero (resp. zero), ie. true (resp. false). *)
  val eval_cond: stmt -> exp -> bool * bool

  val dependencies: State.t list
  val name: string
end

module MakePostDom(X: MakePostDomArg) =
struct

  module PostDom =
    Cil_state_builder.Stmt_hashtbl
      (DomSet)
      (struct
         let name = "postdominator." ^ X.name
         let dependencies = Ast.self :: X.dependencies
         let size = 503
         let kind = `Internal
       end)

  module PostComputer = struct

    let name = "postdominator"
    let debug = ref false

    type t = DomSet.t
    module StmtStartData = PostDom

    let pretty = DomSet.pretty

    let combineStmtStartData _stmt ~old new_ =
      (* No need to compute the intersection: the results can only decrease
         (except on Top, but Top \inter Set = Set *)
      let result = (* DomSet.inter old *) new_ in
      if DomSet.equal result old then None else Some result

    let combineSuccessors = DomSet.inter

    let doStmt stmt =
      !Db.progress ();
      Postdominators_parameters.debug ~level:2 "doStmt: %d" stmt.sid;
      match stmt.skind with
        | Return _ -> Dataflow.Done (DomSet.Value (Stmt.Hptset.singleton stmt))
        | _ -> Dataflow.Post (fun data -> DomSet.add stmt data)


    let doInstr _ _ _ = Dataflow.Default

    (* We make special tests for 'if' statements without a 'then' or
       'else' branch.  It can lead to better precision if we can evaluate
       the condition of the 'if' with always the same truth value *)
    let filterIf ifstmt next = match ifstmt.skind with
      | If (e, { bstmts = sthen :: _ }, { bstmts = [] }, _)
          when not (Stmt.equal sthen next) ->
          (* [next] is the syntactic successor of the 'if', ie the
             'else' branch. If the condition is never false, then
             [sthen] postdominates [next]. We must not follow the edge
             from [ifstmt] to [next] *)
          snd (X.eval_cond ifstmt e)

      | If (e, { bstmts = [] }, { bstmts = selse :: _ }, _)
          when not (Stmt.equal selse next) ->
          (* dual case *)
          fst (X.eval_cond ifstmt e)

      | _ -> true

    let filterStmt pred next =
      X.is_accessible pred && filterIf pred next


    let funcExitData = DomSet.Value Stmt.Hptset.empty

  end
  module PostCompute = Dataflow.Backwards(PostComputer)

  let compute_postdom kf =
    let return =
      try Kernel_function.find_return kf
      with Kernel_function.No_Statement ->
        Postdominators_parameters.abort
          "No return statement for a function with body %a"
          Kernel_function.pretty kf
    in
    try
      let _ = PostDom.find return in
      Postdominators_parameters.feedback ~level:2 "computed for function %a"
        Kernel_function.pretty kf
    with Not_found ->
      Postdominators_parameters.feedback ~level:2 "computing for function %a"
        Kernel_function.pretty kf;
      let f = kf.fundec in
      let stmts = match f with
        | Definition (f,_) -> f.sallstmts
        | Declaration _ ->
            Postdominators_parameters.fatal
              "cannot compute postdominators for leaf function %a"
              Kernel_function.pretty kf
      in
        List.iter (fun s -> PostDom.add s DomSet.Top) stmts;
        PostCompute.compute [return];
        Postdominators_parameters.feedback ~level:2 "done for function %a"
          Kernel_function.pretty kf

  let get_stmt_postdominators f stmt =
    let do_it () = PostDom.find stmt in
    try do_it ()
    with Not_found -> compute_postdom f; do_it ()

  (** @raise Db.PostdominatorsTypes.Top when the statement postdominators
  * have not been computed ie neither the return statement is reachable,
  * nor the statement is in a natural loop. *)
  let stmt_postdominators f stmt =
      match get_stmt_postdominators f stmt with
      | DomSet.Value s ->
          Postdominators_parameters.debug ~level:1 "Postdom for %d are %a"
            stmt.sid Stmt.Hptset.pretty s;
          s
      | DomSet.Top -> raise Db.PostdominatorsTypes.Top

  let is_postdominator f ~opening ~closing =
    let open_postdominators = get_stmt_postdominators f opening in
    DomSet.mem closing open_postdominators

  let display_postdom () =
    let disp_all fmt =
      PostDom.iter
        (fun k v -> Format.fprintf fmt "Stmt:%d -> @[%a@]\n"
           k.sid PostComputer.pretty v)
    in Postdominators_parameters.result "%t" disp_all

  let print_dot_postdom basename kf =
    let filename = basename ^ "." ^ Kernel_function.get_name kf ^ ".dot" in
    Print.build_dot filename kf;
    Postdominators_parameters.result "dot file generated in %s" filename

end

module PostDomDb(X: MakePostDomArg)(DbPostDom: Db.PostdominatorsTypes.Sig) =
struct
  include MakePostDom(X)

  let () = DbPostDom.compute := compute_postdom
  let () = DbPostDom.is_postdominator := is_postdominator
  let () = DbPostDom.stmt_postdominators := stmt_postdominators
  let () = DbPostDom.display := display_postdom
  let () = DbPostDom.print_dot := print_dot_postdom

end

module PostDomBasic =
  PostDomDb(
    struct
      let is_accessible _ = true
      let dependencies = []
      let name = "basic"
      let eval_cond _ _ = true, true
    end)
    (Db.Postdominators)


let output () =
  let dot_postdom = Postdominators_parameters.DotPostdomBasename.get () in
  if dot_postdom <> "" then (
    Ast.compute ();
    Globals.Functions.iter (!Db.Postdominators.print_dot dot_postdom)
  )

let output, _ = State_builder.apply_once "Postdominators.Compute.output"
  [PostDomBasic.PostDom.self] output

let () = Db.Main.extend output


module PostDomVal =
  PostDomDb(
    struct
      let is_accessible = Db.Value.is_reachable_stmt
      let dependencies = [ Db.Value.self ]
      let name = "value"
      let eval_cond stmt _e =
        Db.Value.condition_truth_value stmt

    end)
    (Db.PostdominatorsValue)


let () = Db.Dominators.compute := compute_dom
let () = Db.Dominators.is_dominator := is_dominator
let () = Db.Dominators.stmt_dominators := stmt_dominators
let () = Db.Dominators.display := display_dom

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
