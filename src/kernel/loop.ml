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

(* $Id: loop.ml,v 1.12 2008/10/03 13:09:16 uid568 Exp $ *)

open Cil_types
open Db_types
open Cilutil
open Cil

let name = "natural_loops"

module Natural_Loops =
  Kernel_function.Make_Table
    (Datatype.List
       (Datatype.Couple(Cil_datatype.Stmt)(Datatype.List(Cil_datatype.Stmt))))
    (struct
       let name = name
       let size = 97
       let dependencies = [ Cil_state.self ]
     end)

let pretty_natural_loops fmt kf loops =
  Format.fprintf fmt "Natural_loops for %s:@." (Kernel_function.get_name kf);
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
	     let dbg = 
	       Cmdline.Debug.get () > 0 || Cmdline.Pdg.Verbosity.get() > 0 
	     in
             if dbg then
               Format.printf "COMPUTE NATURAL LOOPS FOR %S@." (Kernel_function.get_name kf);
	     let dominators = Dominators.computeIDom cilfundec in
             (*if dbg then 
               Format.printf "DONE COMPUTE NATURAL LOOPS IDOM FOR %S@." 
                 (Kernel_function.get_name kf);*)
	     let naturals = Dominators.findNaturalLoops cilfundec dominators in
	     if dbg then begin
               Format.printf "DONE COMPUTE NATURAL LOOPS FOR %S@."
                 (Kernel_function.get_name kf);
               pretty_natural_loops Format.std_formatter kf naturals;
             end;
	     naturals)
      kf
  in
  loops

let is_natural kf =
  let natural_loops =
    List.fold_left
      (fun acc (n,_) -> StmtSet.add n acc)
      StmtSet.empty
      (get_naturals kf)
  in
(* non natural loop over-approximation try:
  let can_reach = !stmt_can_reach kf in *)
  fun stmt -> let nat_loop = StmtSet.mem stmt natural_loops in
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
    val mutable allstmts = StmtSet.empty
    method allstmts = allstmts
    inherit nopCilVisitor as super
    method vstmt s = 
      allstmts <- StmtSet.add s allstmts;
      DoChildren
  end
  in
  ignore (visitCilBlock (visitor:>cilVisitor) block);
  visitor#allstmts

let compute_loops_stmts kf =
  let tbl = InstrHashtbl.create 17 in
  let visitor = object
    inherit nopCilVisitor as super
    method vstmt s =
      (match s.skind with
       | Loop (_,block,_,_,_) ->
           InstrHashtbl.add tbl (Kstmt s) (compute_allstmt_block block)
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
      (Cil_datatype.InstrHashtbl(Cil_datatype.StmtSet))
      (struct
	 let name = "LoopStmts"
	 let size = 97
	 let dependencies = [ Cil_state.self ]
       end)
  in
  fun kf loop_stmt ->
    (match loop_stmt.skind with
     | Loop _ -> ()
     | _ -> raise No_such_while);
    let tbl = S.memo compute_loops_stmts kf in
    try InstrHashtbl.find tbl (Kstmt loop_stmt) with Not_found -> assert false

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
