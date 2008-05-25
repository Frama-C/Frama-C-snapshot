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

(* Syntactic loop unrolling *)

open Cil_types
open Cil
open Cilutil
open Db_types

let fresh =
  let counter = ref (-1) in
  fun () ->
    decr counter;
    Label (Format.sprintf "unrolling_%d_loop" (- !counter),
            !currentLoc,
            false)

(* Deep copy of a statement taking care of local gotos and labels. *)
let rec copy_stmt break_continue_must_change label_table stmt =
  let result = {labels=[];sid=0;succs=[];preds=[];skind=stmt.skind;ghost=stmt.ghost} in
  let new_labels,label_tbl,sid =
    let new_label = fresh () in
    let sid = new_sid() in
    let new_acc =
      List.fold_left
        (fun acc _ ->
           StmtMap.add stmt result acc)
        label_table
        stmt.labels
    in [new_label],new_acc,sid
  in
  let new_stmkind,new_label_tbl=
    copy_stmtkind break_continue_must_change label_tbl stmt.skind
  in
  if stmt.labels <> [] then result.labels <- new_labels;
  result.sid <-sid;
  result.skind <- new_stmkind;
  List.iter
    (fun annot ->
       Annotations.add result
         (let content = match Ast_info.before_after_content annot with
              User a -> User (Logic_const.refresh_code_annotation a)
            | AI(c,a) -> AI(c,Logic_const.refresh_code_annotation a)
            | WP(l,_) -> WP(l,Logic_const.fresh_annot_id())
          in match annot with
              Before _ -> Before content
            | After _ -> After content))
    (Annotations.get stmt);
  result,new_label_tbl
and copy_stmtkind break_continue_must_change label_tbl stkind =
  match stkind with
    |(Instr _ | Return _ | Goto _) as keep -> keep,label_tbl
    | If (exp,bl1,bl2,loc) ->
        currentLoc:=loc;
        let new_block1,label_tbl = copy_block break_continue_must_change label_tbl bl1 in
        let new_block2,label_tbl = copy_block break_continue_must_change label_tbl bl2 in
        If(exp,new_block1,new_block2,loc),label_tbl
    | Loop (a,bl,loc,_,_) ->
        currentLoc:=loc;
        let new_block,label_tbl =
          copy_block
            None (* from now on break and continue can be kept *)
            label_tbl
            bl
        in
        Loop (a,new_block,loc,None,None),label_tbl
    | Block bl ->
        let new_block,label_tbl = copy_block break_continue_must_change label_tbl bl in
        Block (new_block),label_tbl
    | Break loc ->
        (match break_continue_must_change with
          | None -> stkind
          | Some (brk_lbl_stmt,_) -> Goto ((ref brk_lbl_stmt),loc)),label_tbl
    | Continue loc ->
        (match break_continue_must_change with
          | None -> stkind
          | Some (_,continue_lbl_stmt) ->  Goto ((ref continue_lbl_stmt),loc)),label_tbl
    | Switch (e,block,stmts,loc) ->
        (* from now on break and continue can be kept *)
        let new_block,new_label_tbl = copy_block None label_tbl block in
        Switch(e,new_block,stmts,loc),new_label_tbl
    | UnspecifiedSequence (s1,s2) ->
        let s1,label_tbl = copy_stmt break_continue_must_change label_tbl s1 in
        let s2,label_tbl = copy_stmt break_continue_must_change label_tbl s2 in
        UnspecifiedSequence (s1,s2),label_tbl
    | TryFinally _ | TryExcept _ -> assert false



and copy_block break_continue_must_change label_tbl bl =
  let new_stmts,label_tbl =
    List.fold_left
      (fun (block_l,label_tbl) v ->
         let new_block,label_tbl = copy_stmt break_continue_must_change label_tbl v in
         new_block::block_l, label_tbl)
      ([],label_tbl) bl.bstmts
  in
  { battrs= bl.battrs;
    bstmts= List.rev new_stmts},label_tbl
(* Update to take into account annotations*)
class do_it (times:int) = object
  inherit Visitor.generic_frama_c_visitor
     (Project.current()) (Cil.inplace_visit())
  val mutable current_function = None

  method vfunc fundec =
    current_function <- Some (Globals.Functions.get fundec.svar);
    DoChildren

  method vstmt s = match s.skind with
    | Loop _ ->
        let annot = Annotations.get s in
        let pragmas =
          Ast_info.lift_annot_list_func Logic_const.extract_loop_pragma annot
        in
        let filter (b,_ as elt) p =
          match (b,p) with
            | false, Unroll_level {term_node=TConst (CInt64(v,_,_))} ->
                (try (true, Int64.to_int v)
                 with _ ->
                   ignore (CilE.warn_once
                             "skipping non integer unrolling directive");
                   elt)
            | true, Unroll_level _ ->
                ignore (CilE.warn_once "ignoring unrolling directive (directive already defined)");
                      elt
                  | _, _ ->
                      elt
        in
        let (_, number) = List.fold_left filter (false, times) pragmas in
        let f s = match s.skind with
          | Loop(_,block,loc,_,_) ->
              currentLoc:=loc;
              let break_label = fresh () in
              let break_lbl_stmt = mkEmptyStmt () in
              break_lbl_stmt.labels <- [break_label];
              break_lbl_stmt.sid <- Cil.new_sid ();
              let mk_continue () =
                let continue_label = fresh () in
                let continue_lbl_stmt = mkEmptyStmt () in
                continue_lbl_stmt.labels <- [continue_label] ;
                continue_lbl_stmt.sid <- Cil.new_sid();
                continue_lbl_stmt
              in
              let current_continue = ref (mk_continue ()) in
              let new_stmts = ref [s] in
              for i=0 to number-1 do
                new_stmts:=!current_continue::!new_stmts;
                let new_block, label_tbl =
                  copy_block
                    (Some (break_lbl_stmt,!current_continue))
                    StmtMap.empty
                    block
                in
                let updated_block = CilE.update_gotos label_tbl new_block in
                current_continue := mk_continue ();
                new_stmts:=
                  updated_block.bstmts @ !new_stmts;
              done;
              let new_stmts = !new_stmts in
              let new_stmt = match new_stmts with
                  [ s ] -> s
                | _ ->
                    let new_stmts = new_stmts @ [break_lbl_stmt] in
                    let new_block = Block (mkBlock new_stmts) in
                    mkStmt new_block
              in new_stmt
          | _ -> assert false
        in
        ChangeDoChildrenPost (s, f)
    | _ -> DoChildren
end

let compute nb file =
  let visitor = new do_it(nb)
  in visitCilFileSameGlobals (visitor:>Cil.cilVisitor) file
