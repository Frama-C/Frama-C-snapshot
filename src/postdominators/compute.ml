(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
open Cil_datatype

module DomKernel =
  Plugin.Register
    (struct
      let name = "dominators"
      let shortname = "dominators"
      let help = "Compute postdominators of statements"
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
          Structural_descr.t_sum [| [| Stmt.Hptset.packed_descr |] |]
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
       end)

  module PostComputer = struct

    let name = "postdominator"
    let debug = false

    type t = DomSet.t
    module StmtStartData = PostDom

    let pretty = DomSet.pretty

    let combineStmtStartData _stmt ~old new_ =
      if DomSet.equal old new_ then None else Some new_

    let combineSuccessors = DomSet.inter

    let doStmt stmt =
      !Db.progress ();
      Postdominators_parameters.debug ~level:2 "doStmt: %d" stmt.sid;
      match stmt.skind with
        | Return _ -> Dataflow2.Done (DomSet.Value (Stmt.Hptset.singleton stmt))
        | _ -> Dataflow2.Post (fun data -> DomSet.add stmt data)


    let doInstr _ _ _ = Dataflow2.Default

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
  module PostCompute = Dataflow2.Backwards(PostComputer)

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
      match f with
        | Definition (f,_) ->
          let stmts = f.sallstmts in
          List.iter (fun s -> PostDom.add s DomSet.Top) stmts;
          PostCompute.compute [return];
          Postdominators_parameters.feedback ~level:2 "done for function %a"
            Kernel_function.pretty kf
        | Declaration _ -> ()

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
    Globals.Functions.iter (fun kf ->
      if Kernel_function.is_definition kf then
        !Db.Postdominators.print_dot dot_postdom kf)
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

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
