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

(** Syntactic loop unrolling. *)

open Cil_types
open Cil
open Cil_datatype
open Visitor

let fresh =
  let counter = ref (-1) in
  fun ?loc ?prefix () ->
    decr counter;
    let prefix = match prefix with None -> "" | Some s -> s ^ "_"
    and loc, orig = match loc with
      | None -> CurrentLoc.get (), false
      | Some loc -> loc, true
    in
    Label (Format.sprintf "%sunrolling_%d_loop" prefix (- !counter),
           loc,
           orig)

let copy_var =
  let counter = ref (-1) in
  fun () ->
    decr counter;
    fun vi ->
      let vi' = Cil_const.copy_with_new_vid vi in
      vi'.vname <- vi.vname ^ "_unroll_" ^ (string_of_int (- !counter));
      (match vi'.vlogic_var_assoc with
           None -> ();
         | Some lv -> lv.lv_name <- vi'.vname);
      vi'

let refresh_vars new_var old_var =
  let assoc = List.combine old_var new_var in
  let visit = object
    inherit Visitor.frama_c_inplace
    method vvrbl vi =
      try ChangeTo (snd (List.find (fun (x,_) -> x.vid = vi.vid) assoc))
      with Not_found -> SkipChildren
    method vlogic_var_use vi =
      match vi.lv_origin with
          None -> SkipChildren
        | Some vi ->
            begin
              try
                let vi'=
                  snd (List.find (fun (x,_) -> x.vid = vi.vid) assoc)
                in
                ChangeTo (Extlib.the vi'.vlogic_var_assoc)
              with Not_found -> SkipChildren
                | Invalid_argument _ ->
                    Kernel.abort
                      "Loop unrolling: cannot find new representative for \
                       local var %s"
                      vi.vname
            end
  end
  in Visitor.visitFramacStmt visit

(* Deep copy of a statement taking care of local gotos and labels. *)
let rec copy_stmt kf break_continue_must_change label_tbl calls_tbl stmt =
  let result =
    { labels = []; 
      sid = Sid.next (); 
      succs = []; 
      preds = []; 
      skind = stmt.skind; 
      ghost = stmt.ghost}
  in
  let new_labels,label_tbl =
    if stmt.labels = [] then
      [], label_tbl
    else
      let new_tbl = Stmt.Map.add stmt result label_tbl
      and new_labels =
        List.fold_left
          (fun lbls -> function
           | Label (s, loc, gen) ->
               (if gen
                then fresh ~prefix:s ()
                else fresh ~prefix:s ~loc ()
               ) :: lbls

           | Case _ | Default _ as lbl -> lbl :: lbls
          )
        []
        stmt.labels
    in
    new_labels, new_tbl
  in
  let new_calls_tbl = match stmt.skind with
    | Instr(Call _) -> Stmt.Map.add stmt result calls_tbl
    | _ -> calls_tbl
  in
  let new_stmkind,new_label_tbl, new_calls_tbl =
    copy_stmtkind
      kf break_continue_must_change label_tbl new_calls_tbl stmt.skind
  in
  if stmt.labels <> [] then result.labels <- new_labels;
  result.skind <- new_stmkind;
  let new_annots =
    Annotations.fold_stmt
      (fun s (annot,_) acc ->
        (*Format.printf "Adding annots to %d@." result.sid;*)
        let new_annot =
          match annot with
          | User a -> User(Logic_const.refresh_code_annotation a)
          | AI(c, a) -> AI(c, Logic_const.refresh_code_annotation a)
        in
        (new_annot, match s with None -> [] | Some s -> [ s ]) :: acc)
      stmt
      []
  in
  List.iter (fun (a, dep) -> Annotations.add kf result dep a) new_annots;
  result, new_label_tbl, new_calls_tbl

  and copy_stmtkind kf break_continue_must_change label_tbl calls_tbl stkind =
    match stkind with
    |(Instr _ | Return _ | Goto _) as keep -> keep,label_tbl,calls_tbl
    | If (exp,bl1,bl2,loc) ->
      CurrentLoc.set loc;
      let new_block1,label_tbl,calls_tbl =
        copy_block kf break_continue_must_change label_tbl calls_tbl bl1
      in
      let new_block2,label_tbl,calls_tbl =
        copy_block kf break_continue_must_change label_tbl calls_tbl bl2
      in
      If(exp,new_block1,new_block2,loc),label_tbl,calls_tbl
    | Loop (a,bl,loc,_,_) ->
      CurrentLoc.set loc;
      let new_block,label_tbl,calls_tbl =
        copy_block
          kf
          None (* from now on break and continue can be kept *)
          label_tbl
          calls_tbl
          bl
      in
      Loop (a,new_block,loc,None,None),label_tbl,calls_tbl
    | Block bl ->
      let new_block,label_tbl,calls_tbl =
        copy_block kf break_continue_must_change label_tbl calls_tbl bl
      in
      Block (new_block),label_tbl,calls_tbl
    | UnspecifiedSequence seq ->
        let change_calls lst calls_tbl =
          List.map
            (fun x -> ref (Stmt.Map.find !x calls_tbl)) lst
        in
        let new_seq,label_tbl,calls_tbl =
          List.fold_left
            (fun (seq,label_tbl,calls_tbl) (stmt,modified,writes,reads,calls) ->
               let stmt,label_tbl,calls_tbl =
                 copy_stmt
                   kf break_continue_must_change label_tbl calls_tbl stmt
               in
               (stmt,modified,writes,reads,change_calls calls calls_tbl)::seq,
               label_tbl,calls_tbl)
            ([],label_tbl,calls_tbl)
            seq
        in
        UnspecifiedSequence (List.rev new_seq),label_tbl,calls_tbl
    | Break loc ->
        (match break_continue_must_change with
           | None -> stkind
           | Some (brk_lbl_stmt,_) -> Goto ((ref brk_lbl_stmt),loc)),
        label_tbl,
        calls_tbl
    | Continue loc ->
        (match break_continue_must_change with
           | None -> stkind
           | Some (_,continue_lbl_stmt) ->
               Goto ((ref continue_lbl_stmt),loc)),
        label_tbl,
        calls_tbl
    | Switch (e,block,stmts,loc) ->
        (* from now on break and continue can be kept *)
      let new_block,new_label_tbl,calls_tbl =
        copy_block kf None label_tbl calls_tbl block
      in
      let stmts' = List.map (fun s -> Stmt.Map.find s new_label_tbl) stmts in
      Switch(e,new_block,stmts',loc),new_label_tbl,calls_tbl
    | TryFinally _ | TryExcept _ -> assert false

  and copy_block kf break_continue_must_change label_tbl calls_tbl bl =
    let new_stmts,label_tbl,calls_tbl =
      List.fold_left
        (fun (block_l,label_tbl,calls_tbl) v ->
          let new_block,label_tbl,calls_tbl =
            copy_stmt kf break_continue_must_change label_tbl calls_tbl v
          in
          new_block::block_l, label_tbl,calls_tbl)
	([],label_tbl,calls_tbl) 
	bl.bstmts
    in
    let new_locals =
      List.map (copy_var ()) bl.blocals
    in
    let fundec = 
      try Kernel_function.get_definition kf
      with Kernel_function.No_Definition -> assert false
    in
    fundec.slocals <- fundec.slocals @ new_locals;
    let new_block = 
      mkBlock (List.rev_map (refresh_vars new_locals bl.blocals) new_stmts)
    in
    new_block.blocals <- new_locals;
    new_block,label_tbl,calls_tbl


let update_gotos sid_tbl block =
  let goto_changer =
  object
    inherit nopCilVisitor
    method vstmt s = match s.skind with
      | Goto(sref,loc) ->
          (try
             let new_stmt = Cil_datatype.Stmt.Map.find !sref sid_tbl in
             ChangeTo (mkStmt (Goto (ref new_stmt,loc)))
           with Not_found -> DoChildren)
      | _ -> DoChildren
  end
  in
  visitCilBlock goto_changer block

(* Update to take into account annotations*)
class do_it (times:int) = object(self)
  inherit Visitor.generic_frama_c_visitor
    (Project.current()) (Cil.inplace_visit())
  val mutable current_function = None

  method vfunc fundec =
    current_function <- Some (Globals.Functions.get fundec.svar);
    DoChildren

  method vstmt_aux s = match s.skind with
  | Loop _ ->
    let annot = Annotations.get_all_annotations s in
    let pragmas =
      Ast_info.lift_annot_list_func Logic_utils.extract_loop_pragma annot
    in
    let filter (b,_ as elt) p =
      match (b,p) with
      | false, Unroll_level {term_node=TConst (CInt64(v,_,_))} ->
        true, My_bigint.to_int v
      | true, Unroll_level _ ->
        Kernel.warning ~once:true ~current:true
          "ignoring unrolling directive (directive already defined)";
        elt
      | _, _ ->
        elt
    in
    let (_, number) = List.fold_left filter (false, times) pragmas in
    let f s = match s.skind with
    | Loop(_,block,loc,_,_) ->
      CurrentLoc.set loc;
      let break_label = fresh () in
      let break_lbl_stmt = mkEmptyStmt () in
      break_lbl_stmt.labels <- [break_label];
      break_lbl_stmt.sid <- Cil.Sid.next ();
      let mk_continue () =
        let continue_label = fresh () in
        let continue_lbl_stmt = mkEmptyStmt () in
        continue_lbl_stmt.labels <- [continue_label] ;
        continue_lbl_stmt.sid <- Cil.Sid.next ();
        continue_lbl_stmt
      in
      let current_continue = ref (mk_continue ()) in
      (*Assuming unrolling was enough a test generator could do:
        Annotations.add_assert s [] ~before:true
        {name=["KILLBRACNH"];
         loc=Cil_datatype.Location.unknown;
         content= Pfalse
        };
      *)
      let new_stmts = ref [s] in
      for i=0 to number-1 do
        new_stmts:=!current_continue::!new_stmts;
        let new_block, label_tbl,_calls_tbl =
                  (* calls tbl is internal. No need to fix references
                     afterwards here. *)
          copy_block
	    (Extlib.the self#current_kf)
            (Some (break_lbl_stmt,!current_continue))
            Stmt.Map.empty
            Stmt.Map.empty
            block
        in
        let updated_block = update_gotos label_tbl new_block in
        current_continue := mk_continue ();
        (match updated_block.blocals with
          [] -> new_stmts:= updated_block.bstmts @ !new_stmts;
        | _ -> (* keep the block in order to preserve locals decl *)
          new_stmts:= mkStmt (Block updated_block) :: !new_stmts);
      done;
      let new_stmt = match !new_stmts with
        [ s ] -> s
      | l ->
        let new_stmts = l @ [break_lbl_stmt] in
        let new_block = mkBlock new_stmts in
        mkStmt (Block new_block)
      in new_stmt
    | _ -> assert false
    in
    ChangeDoChildrenPost (s, f)
  | _ -> DoChildren
end

let compute nb file =
  let visitor = new do_it(nb)
  in visitFramacFileSameGlobals visitor file

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
