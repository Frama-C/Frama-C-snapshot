(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
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

  type path_bound = Integer.t option  (* None = infinite *)
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
            if Integer.le result (Integer.of_int (Options.MaxIterations.get()))
            then Some result
            else (Needs_Merge_After_Loop.replace KF.kf true;
                  Some (Integer.mul entry (Integer.mul in_loop
                                             (Integer.of_int max_iteration))))
        with
        | Invalid_argument _ (* Possible exponent too big *)
        | Failure _ ->       (* Integer too big *)
          Needs_Merge_After_Loop.replace KF.kf true; Some Integer.one
    in (result,loop)

  let kf = KF.kf

  let compile_node stmt (num,stmt2) =
    let stmt = join2_stmts stmt stmt2 in
    let open Cil_types in
    let map_on_all_succs (value) =
      List.map (fun x -> (Region_analysis.Edge(stmt,x),(value,x))) stmt.succs in
    map_on_all_succs num

end

let analyze kf =
  Options.debug "slevel analysis of function %a" Kernel_function.pretty kf;
  let module Specific = Specific(struct let kf = kf end) in
  let module Node = Region_analysis_stmt.MakeNode(Specific) in
  let module Result = Region_analysis.Make(Node) in
  let after = Result.after in
  let dict = after (Some Integer.one, (Kernel_function.find_first_stmt kf)) in
  let max_slevel = ref (Some Integer.zero) in
  Node.Edge_Dict.iter dict (fun _ (x,_) ->
      max_slevel := match x, !max_slevel with
        | None, _ | _, None -> None
        | Some a, Some b -> Some(Integer.max a b));
  (match !max_slevel with
   | None -> Suggested_Slevel.replace kf Integer.zero
   | Some x -> Suggested_Slevel.replace kf x);
;;


let display_results() =
  let display_functions_without_bounds fmt =
    Functions_With_Unknown_Loop.iter_sorted (fun kf _ ->
        Format.fprintf fmt "%a@\n" Kernel_function.pretty kf) in
  if Functions_With_Unknown_Loop.length () > 0 then
    Options.result "Functions with loops whose bounds we could not find:@\n%t"
      display_functions_without_bounds;
  let display_merge_after_loop fmt =
    Needs_Merge_After_Loop.iter_sorted (fun kf _ ->
        Format.fprintf fmt "-val-slevel-merge-after-loop %a \\@\n"
          Kernel_function.pretty kf)
  in
  let max_slevel_opt = Integer.of_int (Options.MaxSlevel.get ()) in
  let bounds_over_max_slevel =
    Suggested_Slevel.fold_sorted (fun kf i acc ->
        if Integer.gt i max_slevel_opt then (kf, i) :: acc else acc)
      []
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
      (List.rev bounds_over_max_slevel);
  let functions_with_bounds =
    Suggested_Slevel.fold_sorted (fun kf i acc ->
        (* Do not report -slevel-function for functions whose bounds
           were not found or were larger than -max-slevel-loop *)
        let slevel =
          if Integer.le i max_slevel_opt &&
             not (Functions_With_Unknown_Loop.mem kf) then i
          else Integer.zero
        in
        (kf, slevel) :: acc
      ) []
  in
  let display_slevel fmt =
    Format.fprintf fmt "%a"
      (Pretty_utils.pp_list ~sep:" \\@\n" display_slevel_function)
      (List.rev functions_with_bounds)
  in
  Options.result "Add this to your command line:@\n%t%t @\n"
    display_merge_after_loop display_slevel;

;;
