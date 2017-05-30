(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(* Note: this does not represent exactly the weird slevel consumption
   strategy. *)

module Needs_Merge_After_Loop =
  Kernel_function.Make_Table
    (Datatype.Bool)
    (struct
      let size = 97
      let name = "Needs_Merge_After_Loop"
      let dependencies = [Ast.self]
    end)
module Suggested_Slevel =
  Kernel_function.Make_Table
    (Datatype.Integer)
    (struct
      let size = 97
      let name = "Suggested_Slevel"
      let dependencies = [Ast.self]
    end)
module Functions_With_Unknown_Loop =
  Kernel_function.Make_Table
    (Datatype.Bool)
    (struct
      let size = 97
      let name = "Functions_With_Unknown_Loop"
      let dependencies = [Ast.self]
    end)

let max_slevel_encountered = ref Integer.zero;;
let update_max_slevel_encountered x = match x, !max_slevel_encountered with
  | None, _ -> ()
  | Some a, b -> max_slevel_encountered := Integer.max a b
;;

type path_bound = Integer.t option  (* None = infinite *)

module Specific(KF:sig val kf: Kernel_function.t end) = struct

  let join2_stmts stmt1 stmt2 =
    (* Cil.dummyStmt is bottom for statements. *)
    if Cil_datatype.Stmt.equal stmt1 stmt2
    then stmt1
    else if Cil_datatype.Stmt.equal stmt1 Cil.dummyStmt
    then stmt2
    else if Cil_datatype.Stmt.equal stmt2 Cil.dummyStmt
    then stmt1
    else assert false
  ;;

  let add_path_bounds a b = match (a,b) with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (Integer.add a b)

  type abstract_value = path_bound * Cil_types.stmt

  let join2 (a1,s1) (a2,s2) = (add_path_bounds a1 a2,join2_stmts s1 s2);;

  let join = function
    | [] -> (Some Integer.zero,Cil.dummyStmt)
    | [x] -> x
    | a::b -> List.fold_left join2 a b
  ;;

  let mu f (entry,loop) =
    let max_iteration =
      try Some(Loop_analysis.Loop_Max_Iteration.find loop)
      with Not_found -> Functions_With_Unknown_Loop.replace KF.kf true; None
    in
    let (in_loop,_) = f (Some Integer.one,loop) in
    let result =
      match (max_iteration,in_loop, entry) with
      (* If merge_after_loop, set to 1 after the loop. *)
      | None, _,_ | _, None,_ | _,_,None ->
        Needs_Merge_After_Loop.replace KF.kf true; Some Integer.one
      | Some max_iteration, Some in_loop, Some entry ->
        (* Kernel.feedback "max_iteration %d in_loop %a entry %a" *)
        (*   max_iteration (Integer.pretty ~hexa:false) in_loop *)
        (*   (Integer.pretty ~hexa:false) entry; *)
        try
          let in_loop_i = Integer.to_int in_loop in
          match in_loop_i with
          | 1 -> Some(Integer.mul entry (Integer.of_int max_iteration))
          | _ ->
            (* Ignoring entry, we have 1 state at the loop entry, then q,
               then q^2, etc.
               Sum i=0 to n q^n = (q^{n+1} - 1)/(q - 1)). *)
            let s = if in_loop_i > 1 && (max_iteration + 1) > 100 then
                raise (Invalid_argument "exponent too large for slevel")
              else
                Integer.power_int_positive_int in_loop_i (max_iteration + 1)
            in
            let slevel_inside_loop =
              Integer.div (Integer.pred s) (Integer.pred in_loop)
            in
            let result = Integer.mul entry slevel_inside_loop in
            (* Kernel.feedback "s %a slevel_inside_loop %a result %a" *)
            (*   (Integer.pretty ~hexa:false) s *)
            (*   (Integer.pretty ~hexa:false) slevel_inside_loop *)
            (*   (Integer.pretty ~hexa:false) result; *)
            if Integer.le result (Integer.of_int (Options.MaxIterations.get()))
            then Some result
            else raise Exit
        with
        | Invalid_argument _ (* Possible exponent too big *)
        | Failure _          (* Integer too big *)
        | Exit  ->          (* Above MaxIterations. *)
          update_max_slevel_encountered
            (Some (Integer.mul entry (Integer.mul in_loop
                                        (Integer.of_int max_iteration))));
          Needs_Merge_After_Loop.replace KF.kf true; Some Integer.one
    in
    (* (match result with *)
    (*  | None -> () *)
    (*  | Some res -> *)
    (*    Kernel.feedback "final result %a" (Integer.pretty ~hexa:false) res); *)
    (result,loop)

  let kf = KF.kf

  let compile_node stmt (num,stmt2) =
    let stmt = join2_stmts stmt stmt2 in
    let open Cil_types in
    let map_on_all_succs (value) =
      List.map (fun x -> (Region_analysis.Edge(stmt,x),(value,x))) stmt.succs in
    map_on_all_succs num

end

(* does not compute branches, and sets -merge-after-loop for all functions *)
module SpecificNoBranches(KF:sig val kf: Kernel_function.t end) = struct

  type abstract_value = path_bound * Cil_types.stmt

  let join2_stmts stmt1 stmt2 =
    (* Cil.dummyStmt is bottom for statements. *)
    if Cil_datatype.Stmt.equal stmt1 stmt2 then stmt1
    else if Cil_datatype.Stmt.equal stmt1 Cil.dummyStmt then stmt2
    else if Cil_datatype.Stmt.equal stmt2 Cil.dummyStmt then stmt1
    else assert false

  let join2 (a1,s1) (a2,s2) =
    let path_bounds =
      match a1, a2 with
      | None, None -> None
      | Some a, None | None, Some a -> Some a
      | Some a1, Some a2 -> Some (Integer.max a1 a2)
    in
    path_bounds, join2_stmts s1 s2;;

  let join = function
    | [] -> (Some Integer.zero, Cil.dummyStmt)
    | [x] -> x
    | a::b -> List.fold_left join2 a b
  ;;

  let mu f (entry,loop) =
    let max_iteration =
      try Some (Loop_analysis.Loop_Max_Iteration.find loop)
      with Not_found -> Functions_With_Unknown_Loop.replace KF.kf true; None
    in
    let (in_loop,_) = f (Some Integer.one, loop) in
    let result =
      match (max_iteration, in_loop, entry) with
      (* If merge_after_loop, set to 1 after the loop. *)
      | None, _, _ | _, None, _ | _, _, None -> Some Integer.one
      | Some max_iteration, Some in_loop, Some entry ->
        try
          let in_loop_i = Integer.to_int in_loop in
          match in_loop_i with
          | 1 -> Some Integer.(max entry (of_int max_iteration))
          | _ ->
            (* We only want the loop iteration count, so just multiply bounds;
               add 1 to avoid issues with slevel counting of first/last
               iterations in nested loops *)
            Some Integer.(pred (mul (succ (of_int in_loop_i))
                                  (of_int max_iteration)))
        with
        | Invalid_argument _ (* Possible exponent too big *)
        | Failure _          (* Integer too big *)
          -> update_max_slevel_encountered
               (Some (Integer.mul entry (Integer.mul in_loop
                                           (Integer.of_int max_iteration))));
          Some Integer.one
    in
    Needs_Merge_After_Loop.replace KF.kf true;
    (result, loop)

  let kf = KF.kf

  let compile_node stmt (num,stmt2) =
    let stmt = join2_stmts stmt stmt2 in
    let map_on_all_succs (value) =
      List.map (fun x -> Region_analysis.Edge(stmt, x), (value, x))
        stmt.Cil_types.succs
    in
    map_on_all_succs num

end

module type M' = Region_analysis_stmt.M with
  type abstract_value = path_bound * Cil_types.stmt

(* [nobranches] defines whether this function will compute a full slevel
   analysis (by default), or estimate loop bounds without branching
   analysis (if [nobranches = true]). *)
let analyze ?(nobranches=false) kf =
  max_slevel_encountered := Integer.zero;
  Options.debug "slevel analysis of function %a" Kernel_function.pretty kf;
  let m =
    if nobranches then
      (module SpecificNoBranches(struct let kf = kf end) : M')
    else
      (module Specific(struct let kf = kf end) : M')
  in
  let module M = (val m : M') in
  let module Node = Region_analysis_stmt.MakeNode(M) in
  let module Result = Region_analysis.Make(Node) in
  let after = Result.after in
  let dict = after (Some Integer.one, (Kernel_function.find_first_stmt kf)) in
  Node.Edge_Dict.iter dict (fun _ (x,_) -> update_max_slevel_encountered x);
  Suggested_Slevel.replace kf !max_slevel_encountered
;;

let cmp_kf_by_name kf1 kf2 =
  String.compare (Kernel_function.get_name kf1) (Kernel_function.get_name kf2)

let display_results() =
  let display_functions_without_bounds fmt =
    Functions_With_Unknown_Loop.iter_sorted ~cmp:cmp_kf_by_name (fun kf _ ->
        Format.fprintf fmt "%a@\n" Kernel_function.pretty kf) in
  if Functions_With_Unknown_Loop.length () > 0 then
    Options.result "Functions with loops whose bounds we could not find:@\n%t"
      display_functions_without_bounds;
  let display_merge_after_loop fmt =
    Needs_Merge_After_Loop.iter_sorted ~cmp:cmp_kf_by_name (fun kf _ ->
        Format.fprintf fmt "-val-slevel-merge-after-loop %a \\@\n"
          Kernel_function.pretty kf)
  in
  let max_slevel_opt = Integer.of_int (Options.MaxSlevel.get ()) in
  let bounds_over_max_slevel =
    List.rev (
      Suggested_Slevel.fold_sorted ~cmp:cmp_kf_by_name
        (fun kf i acc ->
           if Integer.gt i max_slevel_opt then (kf, i) :: acc else acc)
        [])
  in
  let display_slevel_function fmt (kf, i) =
    Format.fprintf fmt "-slevel-function %a:%a"
      Kernel_function.pretty kf (Integer.pretty ~hexa:false) i
  in
  if bounds_over_max_slevel <> [] then
    Options.result "Functions with loops whose estimated bounds \
                    were larger than %s@ (we recommend setting \
                    their slevel to 0 to avoid wasting time):@\n%a"
      Options.MaxSlevel.name
      (Pretty_utils.pp_list ~sep:"@\n"
         (Pretty_utils.pp_pair ~sep:" " Kernel_function.pretty
            (fun fmt i -> Format.fprintf fmt "(estimated bounds: %a)"
                (Integer.pretty ~hexa:false) i)))
      bounds_over_max_slevel;
  let functions_with_bounds =
    List.rev (
      Suggested_Slevel.fold_sorted ~cmp:cmp_kf_by_name
        (fun kf i acc ->
           (* Do not report -slevel-function for functions whose bounds
              were not found or were larger than -max-slevel-loop *)
           let slevel =
             if Integer.le i max_slevel_opt &&
                not (Functions_With_Unknown_Loop.mem kf) then i
             else Integer.zero
           in
           (kf, slevel) :: acc
        ) [])
  in
  (* for a more usable output, in case the user does not want functions
     with bounds equal to 0, sort them before the others *)
  let functions_with_bounds_0, functions_with_bounds_pos =
    List.partition (fun (_kf, i) -> Integer.equal i Integer.zero)
      functions_with_bounds
  in
  let display_slevel fmt =
    Format.fprintf fmt "%a"
      (Pretty_utils.pp_list ~sep:" \\@\n" display_slevel_function)
      (functions_with_bounds_0 @ functions_with_bounds_pos)
  in
  Options.result "Add this to your command line:@\n%t%t @\n"
    display_merge_after_loop display_slevel;

;;
