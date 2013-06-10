(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2013                                               *)
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

(* Currently, non strict mode is not available as it needs multiple return
 *)


open Cil_types
open Local_slevel_types

(* Forward reference to {Local_slevel.compute_sub_function} *)
let (compute_sub_function_ref
    :  (Kernel_function.t
    -> stmt
    -> local_slevel_info
    -> State_set.t
    -> Cvalue.Model.t
     * Base.SetLattice.t)
    ref) = ref (fun _ -> assert false)
let compute_sub_function kf = !compute_sub_function_ref kf

(* Checks for a function call of a given name *)
let is_call_with_name stmt name =
  match stmt.skind with
    | Instr
      (Call
        (None
        , {enode = Lval (Var {vname = vname; vtype = TFun _}, _)}
        , _
        , _
        )
      ) when vname = name -> true
    | _ -> false

let is_split_builtin_call stmt = is_call_with_name stmt "Frama_C_split"
let is_merge_builtin_call stmt = is_call_with_name stmt "Frama_C_merge"
let optional_slevel stmt =
 let fail () = Value_parameters.abort "Slevel must be a positive\
                                       integral number, fitting into type int"
 in
 match stmt.skind with
   | Instr (Call (None, {enode = Lval (Var {vtype = TFun _}, _)}, _ ::
       {enode = Const (CInt64 (n, _, _))} :: _, _)) ->
           let n' = try Integer.to_int n with Assert_failure _ ->
               fail () in if n' < 0 then fail () else Some n'
   | _ -> None

let get_id stmt =
 let fail () = Value_parameters.abort "Each \"Frama_C_split\" and \
             \"Frama_C_merge\" builtin must have an integral as first argument."
  in
  match stmt.skind with
    | Instr
      (Call
        (None
        , {enode = Lval (Var {vtype = TFun _}, _)}
        , {enode = Const (CInt64 (n, _, _))} :: _
        , _
        )
      ) -> begin try Integer.to_int n with Assert_failure _ -> fail () end
    | _ -> fail ()


(* This represents a split instruction:
 * map from split stmt to pair of
   * optional slevel
   * set of merge stmts
 *)
module Split_merge = Cil_datatype.Stmt.Map.Make
  (Datatype.Pair
    (Datatype.Option (Datatype.Int))
    (Cil_datatype.Stmt.Hptset))

(* Contains a mapping betwen kf and associated Split_merge_set.
 * this is the datastructure expected from other modules
 * kf -> (splitstmt -> mergestmt set)
 *)
module Kf_split_merge_map = struct
  include Kernel_function.Map.Make(Split_merge)
end

let get_split_merge kf map =
  try Kernel_function.Map.find kf map with Not_found ->
    Cil_datatype.Stmt.Map.empty

module Api_kf_split_merge_map = State_builder.Ref
  (Kf_split_merge_map)
  (struct
    let name = "empty" (* FIXME [SCM] check! *)
    let dependencies = [ Ast.self ] (* FIXME [SCM] check! *)
    let default () = Kernel_function.Map.empty
  end)

module Strict_mode = State_builder.Ref
  (Datatype.Bool)
  (struct
    let name = "true" (* FIXME [SCM] check! *)
    let dependencies = [ Ast.self ] (* FIXME [SCM] check! *)
    let default () = true
  end)

(* Checks a set of split/merge tuples for validity
 *
 * A valid set of split/merge tuples fulfills the follwing:
     1 Each split must be associated to a function of the current project
     FIXME [SCM] currently not checked
     2  The collection of all split nodes forms a set (uniqueness)
     checked by build_kf_split_merge_map
     3  There does not exist a path of length >= 1 from split to split without
        going through an associated mege
     4  A split node dominates all associated merge nodes
     5  All merge nodes associated with a split are inside the same function

    (6) Strict mode:
        1 The merge part of the tuple must be a singleton
        2 A split must dominate its merge and a merge must postdominate its
          split
        3 If on a cfg, there exists a split node s1 and a split node s2, such
        that s2 is reachable from s1 without going through merge(s1),
        then merge(s2) must postdominate merge(s1)
        4 A node must not be a split and a merge node
        FIXME [SCM] untested
 *)
let check_split_merges_for_kf kf strict split_merges =
  Value_parameters.debug "Checking split/merge configuration for function %s"
                                                  (Kernel_function.get_name kf);
  let check3 (split, merges) = begin
    if Stmts_graph.stmt_is_in_cycle_filtered
       (fun x -> not (Cil_datatype.Stmt.Hptset.mem x merges))
       split
    then Value_parameters.abort "Split node in cycle without any associated \
                                                                         merges"
  end
  in
  let check_4_and_5 (split, merges) =
    let check4 merge = begin if not
      (!Db.Dominators.is_dominator kf ~opening:split ~closing:merge)
    then Value_parameters.abort "Split node must dominate all associated merges"
    end
    in
    let check5 merge = begin if not
      (try
        Kernel_function.equal kf (Kernel_function.find_englobing_kf merge)
      with Not_found -> false)
    then Value_parameters.abort "Split/merge combination not in same function"
    end
    in
    Cil_datatype.Stmt.Hptset.iter
      (fun merge -> check4 merge; check5 merge)
      merges
  in
  let check6 (split, merges) =
    let from_singleton ms = match Cil_datatype.Stmt.Hptset.elements ms with
      | [m] -> m
      | _ -> assert false (* call check61 first *)
    in
    let check61 merges =
      if Cil_datatype.Stmt.Hptset.cardinal merges <> 1 then
         Value_parameters.abort "In strict mode, only one merge allowed per \
                                                                          split"
    in
    let check62 merge =
      if not (!Db.Dominators.is_dominator kf ~opening:split ~closing:merge &&
           !Db.Postdominators.is_postdominator kf ~opening:split ~closing:merge)
      then Value_parameters.abort "In srict mode, a split must dominate its \
                              merge, and the merge must postdominate its split."
    in
    let check6_3_and_4 merge =
      let check63 split2 merge2 =
          if Stmts_graph.stmt_can_reach_filtered
              (fun s -> s.sid <> merge.sid) split split2
          && not (!Db.Postdominators.is_postdominator kf
                    ~opening:merge2
                    ~closing:merge) then
            Value_parameters.abort "In strict mode, split instructions must be \
                                                                  well matched."
      in
      let check64 merge2 =
        if split.sid = merge2.sid then
          Value_parameters.abort "In strict mode, a stmt can either be a split \
                                                            a merge, or neither"
      in
      Cil_datatype.Stmt.Map.iter
        (fun split2 (_, merges2) ->
          check61 merges;
          let merge2 = from_singleton merges2 in
          check63 split2 merge2;
          check64 merge2)
        split_merges
    in
    check61 merges;
    let merge = from_singleton merges in
    check62 merge;
    check6_3_and_4 merge
  in
  Cil_datatype.Stmt.Map.iter
    (fun split (_, merges) ->
      let split_merge = split, merges in
          check3 split_merge
        ; check_4_and_5 split_merge
        ; if strict then check6 split_merge)
    split_merges

(* Finds the explicit split/merge buildin in function kf
 * Aborts on error
 *)
let retrieve_inner_split_merges kf : Split_merge.t =
  (* FIXME [SCM] exception unhandled - get_definition *)
  let fun_stmts = (Kernel_function.get_definition kf).sallstmts in
  let (stmt_id, id_mergeset) = List.fold_left
    (fun (stmt_id, id_mergeset) stmt ->
        match is_split_builtin_call stmt, is_merge_builtin_call stmt with
          | false, false -> stmt_id, id_mergeset
          | true, true -> assert false
          | true, false ->
              let id = get_id stmt in
              let slevel = optional_slevel stmt in
              Cil_datatype.Stmt.Map.add stmt (slevel, id) stmt_id, id_mergeset
          | false, true ->
              let id = get_id stmt in
              let newval =
                try
                  let oldval = Datatype.Int.Map.find id id_mergeset in
                  Cil_datatype.Stmt.Hptset.add stmt oldval
               with Not_found -> Cil_datatype.Stmt.Hptset.singleton stmt
              in
              stmt_id, Datatype.Int.Map.add id newval id_mergeset
      )
      (Cil_datatype.Stmt.Map.empty, Datatype.Int.Map.empty)
      fun_stmts
  in
  Cil_datatype.Stmt.Map.fold (fun split (slevel, id) map ->
      let stmtset =
       try
         Datatype.Int.Map.find id id_mergeset
       with Not_found -> Cil_datatype.Stmt.Hptset.empty
      in
      Cil_datatype.Stmt.Map.add split (slevel, stmtset) map)
    stmt_id
    Cil_datatype.Stmt.Map.empty

(* This function takes two split_merges and merges them to one
 * It fails if it finds a split instruction with the same split stmt
 *)
let merge_split_merges split_merge1 split_merge2 =
  Cil_datatype.Stmt.Map.fold (fun split split_merge map ->
    if Cil_datatype.Stmt.Map.mem split split_merge1 then
      Value_parameters.abort "Same split stmt defined twice.";
    Cil_datatype.Stmt.Map.add split split_merge map
  )
  split_merge2
  split_merge1

(* Worker for get_check_tuples *)
let get_check_tuples_raw kf =
  let strict = Strict_mode.get () in
  let split_merges_api_all = Api_kf_split_merge_map.get () in
  let split_merges_api = get_split_merge kf split_merges_api_all in
  let split_merges_kf  = retrieve_inner_split_merges kf in
  let split_merges = merge_split_merges split_merges_api split_merges_kf in
  check_split_merges_for_kf kf strict split_merges;
  split_merges

(* Cache for get_check_tuples *)
module Get_check_tuples_cache = Kernel_function.Make_Table
  (Split_merge)
  (struct
    let size = 10 (* FIXME [SCM] Check! *)
    let name = "Get_check_tuples_cache"
    let dependencies = [ Ast.self
                       ; Api_kf_split_merge_map.self
                       ; Strict_mode.self ] (* FIXME [SCM] Check! *)
  end)

(* This is the key function that retrieves the split instructions for kf by
   * looking up programmatically defined instructions
   * retrieving instructions from the statements in kf
   * merging both sets together
   * checking the merged set (according to strictness setting)
 * The result of this function is memoized
 *)
let get_check_tuples =
  Get_check_tuples_cache.memo get_check_tuples_raw


(* This function determines what should be done in eval_slevel.ml computers
 * doStmt
 * FIXME [SCM] currently only implemented for strict mode because of single
 * return restriction
 *
 * 1 Look at prevmode:
   * Merge -> set prevmode to normal and return merge
   * Split -> set prevmode to normal and return normal
   * Normal -> look at current stmt type
 * 2 Current stmt type:
   * issplit -> return Split with prevmode field set to split and merges set
     to corresponding merge set
   * ismerge -> set info.prevmode to merge and return normal
   * neither -> return normal
 *)
let determine_mode kf stmt info =
  assert (Strict_mode.get ()); (* FIXME [SCM] currently only strict mode *)
  let split_merges = get_check_tuples kf in
  match info.prevmode with
    | MergeSplit _ -> assert false
    (*| Merge -> info.prevmode <- Normal; Merge*)
    | Split _ -> info.prevmode <- Normal; Normal
    (*| Normal ->*)
    | _ ->
  let issplit = try
    Some (Cil_datatype.Stmt.Map.find stmt split_merges)
    with Not_found -> None
  in
  let ismerge = Cil_datatype.Stmt.Hptset.mem stmt info.merges in
  match issplit, ismerge with
    | Some _, true -> assert false
    | Some (slevel, mergeset), false -> Split
      { prevmode = Split (empty_info())
      ; merges = mergeset
      ; slevel = slevel }
    | None, true -> (*info.prevmode <- Merge; Normal*) Merge
    | None, false -> Normal

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
