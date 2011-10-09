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
open Cil_datatype
open Cil

let name = "natural_loops"

module Natural_Loops =
  Kernel_function.Make_Table
    (Datatype.List
       (Datatype.Pair(Stmt)(Datatype.List(Stmt))))
    (struct
       let name = name
       let size = 97
       let dependencies = [ Ast.self ]
       let kind = `Internal
     end)

let pretty_natural_loops fmt loops =
  List.iter
    (fun (start,members) ->
       Format.fprintf fmt "Loop start: %d ( " start.sid;
       List.iter (fun d -> Format.fprintf fmt "%d " d.sid) members;
       Format.fprintf fmt ")@\n";)
    loops

let get_naturals kf =
  let loops =
    Natural_Loops.memo
      (fun kf ->
         match kf.fundec with
         | Declaration _ ->
             []
         | Definition (cilfundec,_) ->
             Kernel.debug "Compute natural loops for '%a'"
               Kernel_function.pretty kf;
             let dominators = Dominators.computeIDom cilfundec in
             (*if dbg then
               Format.printf "DONE COMPUTE NATURAL LOOPS IDOM FOR %S@."
                 (Kernel_function.get_name kf);*)
             let naturals = Dominators.findNaturalLoops cilfundec dominators in
             Kernel.debug
               "Done computing natural loops for '%a':@.%a"
               Kernel_function.pretty kf
               pretty_natural_loops naturals;
             naturals
      )
      kf
  in
  loops

let is_natural kf =
  let natural_loops =
    List.fold_left
      (fun acc (n, _) -> Stmt.Set.add n acc)
      Stmt.Set.empty
      (get_naturals kf)
  in
(* non natural loop over-approximation try:
  let can_reach = !stmt_can_reach kf in *)
  fun stmt ->
    let nat_loop = Stmt.Set.mem stmt natural_loops in
    nat_loop
(*  if nat_loop then nat_loop
  else
    if can_reach stmt stmt
    then true (* this is non natural loop or an imbricated loop... *)
    else false
*)
let back_edges kf stmt =
  if is_natural kf stmt then
    let rec lookup = function
      | [] -> assert false
      | (s, pred_s) :: sl -> if s.sid = stmt.sid then pred_s else lookup sl
    in
    lookup (get_naturals kf)
  else
    []

let while_for_natural_loop kf stmt =
  match stmt.skind with
  | Loop _ -> stmt
  | _ -> (* the while stmt is probably the non looping predecessor *)
      let be = back_edges kf stmt in
      Format.printf "Stmt:%d " stmt.sid;
      List.iter (fun x -> Format.printf "B_edge:%d " x.sid) be;
      List.iter (fun x -> Format.printf "Preds:%d " x.sid) stmt.preds;
      let non_looping_pred =
        List.filter (fun pred -> not (List.mem pred be)) stmt.preds
      in
      match non_looping_pred with
      | [x] -> x
      | _ ->
          Format.eprintf "@.Lexical non natural loop detected !@.";
          assert false

let compute_allstmt_block block =
  let visitor = object
    val mutable allstmts = Stmt.Set.empty
    method allstmts = allstmts
    inherit nopCilVisitor as super
    method vstmt s =
      allstmts <- Stmt.Set.add s allstmts;
      DoChildren
  end
  in
  ignore (visitCilBlock (visitor:>cilVisitor) block);
  visitor#allstmts

module Result = Kinstr.Hashtbl

let compute_loops_stmts kf =
  let tbl = Result.create 17 in
  let visitor = object
    inherit nopCilVisitor as super
    method vstmt s =
      (match s.skind with
       | Loop (_,block,_,_,_) ->
           Result.add tbl (Kstmt s) (compute_allstmt_block block)
       |  _ -> ());
      DoChildren
  end
  in
  (try
     ignore
       (visitCilFunction
          (visitor :> cilVisitor) (Kernel_function.get_definition kf));
   with Kernel_function.No_Definition ->
     ());
  tbl

exception No_such_while

(** @raise No_such_while if [stmt.skind] is not a [While]. *)
let get_loop_stmts =
  let module S =
    Kernel_function.Make_Table
      (Result.Make(Stmt.Set))
      (struct
         let name = "LoopStmts"
         let size = 97
         let dependencies = [ Ast.self ]
         let kind = `Internal
       end)
  in
  fun kf loop_stmt ->
    (match loop_stmt.skind with
     | Loop _ -> ()
     | _ -> raise No_such_while);
    let tbl = S.memo compute_loops_stmts kf in
    try Result.find tbl (Kstmt loop_stmt) with Not_found -> assert false

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
