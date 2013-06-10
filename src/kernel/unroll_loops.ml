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

(** Syntactic loop unrolling. *)

open Cil_types
open Cil
open Cil_datatype
open Visitor

let dkey = Kernel.register_category "ulevel"

let extract_from_pragmas s times =
  let annot = Annotations.code_annot s in
  let pragmas = Logic_utils.extract_loop_pragma annot in
    
  let get_nb_times (found_times,_ as elt) p =
    let get_nb_times _ spec = match spec with
      | {term_type=typ}  when Logic_typing.is_integral_type typ ->
	  if found_times then 
	    (Kernel.warning ~once:true ~current:true
	       "ignoring unrolling directive (directive already defined)";
	     raise Not_found)
	  else
	    (try 
	       begin
		 match isInteger (Cil.constFold true (!Db.Properties.Interp.term_to_exp None spec)) with
		   | Some i -> 
		       Some (Integer.to_int i)
		   | None -> Kernel.warning ~once:true ~current:true
		       "ignoring unrolling directive (not a constant expression)";
		       raise Not_found
	       end
	     with Invalid_argument s -> 
	       Kernel.warning ~once:true ~current:true
		 "ignoring unrolling directive (%s)" s;
	       raise Not_found)
      | _ -> None
    in match p with
      | Unroll_specs specs ->
	  (try 
	     begin
	       match List.fold_left get_nb_times None specs with
		 | Some i -> true, i
		 | None -> elt
	     end
	   with Not_found -> elt)
      | _ ->
	  elt 
  in 
  let times = snd (List.fold_left get_nb_times (false, times) pragmas) 
  in
  let is_total_unrolling spec = match spec with
    | {term_node=TConst (LStr "completely") } -> true
    | _ -> false
  in
  let is_total_unrolling = function
    | Unroll_specs specs -> List.exists is_total_unrolling specs
    | _ -> false 
  in times, (List.exists is_total_unrolling pragmas) 

let fresh_label =
  let counter = ref (-1) in
  fun ?loc ?label_name () ->
    decr counter;
    let loc, orig = match loc with
      | None -> CurrentLoc.get (), false
      | Some loc -> loc, true
    and new_label_name = 
      let prefix = match label_name with None -> "" | Some s -> s ^ "_"
      in Format.sprintf "%sunrolling_%d_loop" prefix (- !counter)
    in Label (new_label_name,
           loc,
           orig)

let copy_var =
  let counter = ref (-1) in
  fun () ->
    decr counter;
    fun vi ->
      let vi' = Cil_const.copy_with_new_vid vi in
      let name = vi.vname ^ "_unroll_" ^ (string_of_int (- !counter)) in
      Cil_const.change_varinfo_name vi' name;
      vi'

let refresh_vars new_var old_var =
  let assoc = List.combine old_var new_var in
  let visit = object
    inherit Visitor.frama_c_inplace
    method vvrbl vi =
      try ChangeTo (snd (List.find (fun (x,_) -> x.vid = vi.vid) assoc))
      with Not_found -> SkipChildren
  end
  in Visitor.visitFramacStmt visit

(* Takes care of local gotos and labels into C. *)
let update_gotos sid_tbl block =
  let goto_updater =
  object
    inherit nopCilVisitor
    method vstmt s = match s.skind with
      | Goto(sref,_loc) ->
          (try (* A deep copy has already be done. Just modifies the reference in place. *)
             let new_stmt = Cil_datatype.Stmt.Map.find !sref sid_tbl in
              sref := new_stmt
           with Not_found -> ()) ;
	  DoChildren
      | _ -> DoChildren
   (* speed up: skip non interesting subtrees *)
   method vvdec _ = SkipChildren (* via visitCilFunction *)
   method vspec _ = SkipChildren (* via visitCilFunction *)
   method vcode_annot _ = SkipChildren (* via Code_annot stmt *)
   method vloop_annot _ = SkipChildren (* via Loop stmt *)
   method vexpr _ = SkipChildren (* via stmt such as Return, IF, ... *)
   method vlval _ = SkipChildren (* via stmt such as Set, Call, Asm, ... *)
   method vattr _ = SkipChildren (* via Asm stmt *)
  end
  in visitCilBlock (goto_updater:>cilVisitor) block

let is_referenced stmt l =
  let module Found = struct exception Found end in
  let vis = object
    inherit Visitor.frama_c_inplace
    method vlogic_label l =
      match l with
	| StmtLabel s when !s == stmt -> raise Found.Found
	| _ -> DoChildren
  end
  in
  try
    List.iter (fun x -> ignore (Visitor.visitFramacStmt vis x)) l;
    false
  with Found.Found -> true

(* Deep copy of annotations taking care of labels into annotations. *)
let copy_annotations kf assoc labelled_stmt_tbl (stmt_src,stmt_dst) =
  let fresh_annotation a =
    let visitor = object
      inherit Visitor.frama_c_copy (Project.current())
      method vlogic_var_use vi =
	match vi.lv_origin with
            None -> SkipChildren
          | Some vi ->
            begin
              try
                let vi'= snd (List.find (fun (x,_) -> x.vid = vi.vid) assoc) in
                ChangeTo (Extlib.the vi'.vlogic_var_assoc)
              with Not_found -> SkipChildren
                | Invalid_argument _ ->
                    Kernel.abort
                      "Loop unrolling: cannot find new representative for \
                       local var %s"
                      vi.vname
            end
      method vlogic_label (label:logic_label) =
	match label with
	| StmtLabel (stmt) -> 
	    (try (* A deep copy has already be done. 
		    Just modifies the reference in place. *)
               let new_stmt = Cil_datatype.Stmt.Map.find !stmt labelled_stmt_tbl
	       in ChangeTo (StmtLabel (ref new_stmt))
	     with Not_found -> SkipChildren) ;
	| LogicLabel (None, _str) -> SkipChildren
	| LogicLabel (Some _stmt, str) -> ChangeTo (LogicLabel (None, str))
    end
    in visitCilCodeAnnotation (visitor:>cilVisitor) (Logic_const.refresh_code_annotation a) 
  in
  let new_annots =
    Annotations.fold_code_annot
      (fun emitter annot acc ->
         Kernel.debug ~dkey
           "Copying an annotation to stmt %d from stmt %d@."
           stmt_dst.sid stmt_src.sid;
         let new_annot = fresh_annotation annot in
         (emitter, new_annot) :: acc)
      stmt_src
      []
  in
  List.iter 
    (fun (e, a) -> Annotations.add_code_annot e ~kf stmt_dst a) 
    new_annots

let update_loop_current kf loop_current block =
  let vis = object(self)
    inherit Visitor.frama_c_inplace
    initializer self#set_current_kf kf
    method vlogic_label =
      function
	| LogicLabel(_,"LoopCurrent") -> ChangeTo (StmtLabel (ref loop_current))
	| _ -> DoChildren
    method vstmt_aux s =
      match s.skind with
	| Loop _ -> SkipChildren (* loop init and current are not the same here. *)
	| _ -> DoChildren
  end in
  ignore (Visitor.visitFramacBlock vis block)

let update_loop_entry kf loop_entry stmt =
  let vis = object(self)
    inherit Visitor.frama_c_inplace
    initializer self#set_current_kf kf
    method vlogic_label =
      function
	| LogicLabel(_,"LoopEntry") -> ChangeTo (StmtLabel (ref loop_entry))
	| _ -> DoChildren
    method vstmt_aux s =
      match s.skind with
	| Loop _ -> SkipChildren (* loop init and current are not the same here. *)
	| _ -> DoChildren
  end in
  ignore (Visitor.visitFramacStmt vis stmt)

(* Deep copy of a block taking care of local gotos and labels into C code and 
   annotations. *)
let copy_block kf break_continue_must_change bl =
  let assoc = ref [] in
  let fundec = 
    try Kernel_function.get_definition kf
    with Kernel_function.No_Definition -> assert false
  and annotated_stmts = ref []
  and labelled_stmt_tbl = Stmt.Map.empty
  and calls_tbl = Stmt.Map.empty
  in
  let rec copy_stmt break_continue_must_change labelled_stmt_tbl calls_tbl stmt =
    let result =
      { labels = []; 
        sid = Sid.next (); 
        succs = []; 
        preds = []; 
        skind = stmt.skind; 
        ghost = stmt.ghost}
    in
    let new_labels,labelled_stmt_tbl =
      if stmt.labels = [] then
        [], labelled_stmt_tbl
      else
        let new_tbl = Stmt.Map.add stmt result labelled_stmt_tbl
        and new_labels =
          List.fold_left
            (fun lbls -> function
             | Label (s, loc, gen) ->
               (if gen
                then fresh_label ~label_name:s ()
                else fresh_label ~label_name:s ~loc ()
               ) :: lbls

             | Case _ | Default _ as lbl -> lbl :: lbls
            )
          []
          stmt.labels
      in new_labels, new_tbl
    in
    let new_calls_tbl = match stmt.skind with
      | Instr(Call _) -> Stmt.Map.add stmt result calls_tbl
      | _ -> calls_tbl
    in
    let new_stmkind,new_labelled_stmt_tbl, new_calls_tbl =
      copy_stmtkind
        break_continue_must_change labelled_stmt_tbl new_calls_tbl stmt.skind
    in
    if stmt.labels <> [] then result.labels <- new_labels;
    result.skind <- new_stmkind;
    if Annotations.has_code_annot stmt then 
      begin
	Kernel.debug ~dkey 
          "Found an annotation to copy for stmt %d from stmt %d@."
          result.sid stmt.sid;
	annotated_stmts := (stmt,result) :: !annotated_stmts;
      end;
    result, new_labelled_stmt_tbl, new_calls_tbl

    and copy_stmtkind
      break_continue_must_change labelled_stmt_tbl calls_tbl stkind =
      match stkind with
      |(Instr _ | Return _) as keep -> keep,labelled_stmt_tbl,calls_tbl
      | Goto (stmt_ref, loc) -> Goto (ref !stmt_ref, loc),labelled_stmt_tbl,calls_tbl
      | If (exp,bl1,bl2,loc) ->
        CurrentLoc.set loc;
        let new_block1,labelled_stmt_tbl,calls_tbl =
          copy_block break_continue_must_change labelled_stmt_tbl calls_tbl bl1
        in
        let new_block2,labelled_stmt_tbl,calls_tbl =
          copy_block break_continue_must_change labelled_stmt_tbl calls_tbl bl2
        in
        If(exp,new_block1,new_block2,loc),labelled_stmt_tbl,calls_tbl
      | Loop (a,bl,loc,_,_) ->
        CurrentLoc.set loc;
        let new_block,labelled_stmt_tbl,calls_tbl =
          copy_block
            None (* from now on break and continue can be kept *)
            labelled_stmt_tbl
            calls_tbl
            bl
        in
        Loop (a,new_block,loc,None,None),labelled_stmt_tbl,calls_tbl
      | Block bl ->
        let new_block,labelled_stmt_tbl,calls_tbl =
          copy_block break_continue_must_change labelled_stmt_tbl calls_tbl bl
        in
        Block (new_block),labelled_stmt_tbl,calls_tbl
      | UnspecifiedSequence seq ->
          let change_calls lst calls_tbl =
            List.map (fun x -> ref (Stmt.Map.find !x calls_tbl)) lst
          in
          let new_seq,labelled_stmt_tbl,calls_tbl =
            List.fold_left
              (fun (seq,labelled_stmt_tbl,calls_tbl) (stmt,modified,writes,reads,calls) ->
                 let stmt,labelled_stmt_tbl,calls_tbl =
                   copy_stmt
                     break_continue_must_change labelled_stmt_tbl calls_tbl stmt
                 in
                 (stmt,modified,writes,reads,change_calls calls calls_tbl)::seq,
                 labelled_stmt_tbl,calls_tbl)
              ([],labelled_stmt_tbl,calls_tbl)
              seq
          in
          UnspecifiedSequence (List.rev new_seq),labelled_stmt_tbl,calls_tbl
      | Break loc ->
          (match break_continue_must_change with
           | None -> stkind
           | Some (brk_lbl_stmt,_) -> Goto ((ref brk_lbl_stmt),loc)),
          labelled_stmt_tbl,
        calls_tbl
      | Continue loc ->
          (match break_continue_must_change with
           | None -> stkind
           | Some (_,continue_lbl_stmt) ->
               Goto ((ref continue_lbl_stmt),loc)),
          labelled_stmt_tbl,
          calls_tbl
      | Switch (e,block,stmts,loc) ->
          (* from now on break and continue can be kept *)
        let new_block,new_labelled_stmt_tbl,calls_tbl =
          copy_block None labelled_stmt_tbl calls_tbl block
        in
        let stmts' = List.map (fun s -> Stmt.Map.find s new_labelled_stmt_tbl) stmts in
        Switch(e,new_block,stmts',loc),new_labelled_stmt_tbl,calls_tbl
      | TryFinally _ | TryExcept _ -> assert false

    and copy_block break_continue_must_change labelled_stmt_tbl calls_tbl bl =
      let new_stmts,labelled_stmt_tbl,calls_tbl =
        List.fold_left
          (fun (block_l,labelled_stmt_tbl,calls_tbl) v ->
            let new_block,labelled_stmt_tbl,calls_tbl =
              copy_stmt break_continue_must_change labelled_stmt_tbl calls_tbl v
            in
            new_block::block_l, labelled_stmt_tbl,calls_tbl)
  	  ([],labelled_stmt_tbl,calls_tbl) 
	  bl.bstmts
      in
      let new_locals =
        List.map (copy_var ()) bl.blocals
      in
      fundec.slocals <- fundec.slocals @ new_locals;
      assoc:=(List.combine bl.blocals new_locals) @ !assoc;
      let new_block = 
        mkBlock (List.rev_map (refresh_vars new_locals bl.blocals) new_stmts)
      in
      new_block.blocals <- new_locals;
      new_block,labelled_stmt_tbl,calls_tbl
  in
  let new_block, labelled_stmt_tbl, _calls_tbl = 
    (* [calls_tbl] is internal. No need to fix references afterwards here. *)
    copy_block break_continue_must_change labelled_stmt_tbl calls_tbl bl
  in
  List.iter (copy_annotations kf !assoc labelled_stmt_tbl) !annotated_stmts ;
  update_gotos labelled_stmt_tbl new_block

(* Update to take into account annotations*)
class do_it (emitter,(times:int)) = object(self)
  inherit Visitor.frama_c_inplace

  (* We sometimes need to move labels between statements. This table
     maps the old statement to the new one *)
  val moved_labels = Cil_datatype.Stmt.Hashtbl.create 17
  val mutable gotos = [] ;
  val mutable has_unrolled_loop = false ;

  val mutable file_has_unrolled_loop = false ;

  method get_file_has_unrolled_loop () = file_has_unrolled_loop ;

  method vfunc fundec =
    assert (gotos = []) ;
    assert (not has_unrolled_loop) ;
    let post_goto_updater =
      (fun id -> 
	 if has_unrolled_loop then
	   List.iter (fun s -> match s.skind with Goto(sref,_loc) ->
			(try 
			   let new_stmt = Cil_datatype.Stmt.Hashtbl.find moved_labels !sref in
			     sref := new_stmt
			 with Not_found -> ())
			| _ -> assert false) gotos ;
	 has_unrolled_loop <- false ;
	 gotos <- [] ;
	 Cil_datatype.Stmt.Hashtbl.clear moved_labels ;
	 id) in
      ChangeDoChildrenPost (fundec, post_goto_updater)

  method vstmt_aux s = match s.skind with
  | Goto _ -> 
      gotos <- s::gotos; (* gotos that may need to be updated *)
      DoChildren
  | Switch _ -> (* Update the labels pointed to by the switch if needed *)
      let update s =
	if has_unrolled_loop then
          (match s.skind with
             | Switch (e', b', lbls', loc') ->
		 let labels_moved = ref false in 
		 let update_label s =
		   try 
		     let s = Cil_datatype.Stmt.Hashtbl.find moved_labels s
		     in labels_moved := true ; s 
		   with Not_found -> s
		 in let moved_lbls = List.map update_label lbls' in
		   if !labels_moved then
		     s.skind <- Switch (e', b', moved_lbls, loc');
             | _ -> ());
        s
      in
      ChangeDoChildrenPost (s, update)
  | Loop _ ->
    let number, is_total_unrolling = extract_from_pragmas s times 
    in
    let f sloop = 
      Kernel.debug ~dkey
        "Unrolling loop stmt %d (%d times) inside function %a@." 
	sloop.sid number Kernel_function.pretty (Extlib.the self#current_kf);
      file_has_unrolled_loop <- true ;
      has_unrolled_loop <- true ;
      match sloop.skind with
    | Loop(_,block,loc,_,_) ->
	(* Note: loop annotations are kept into the remaining loops,
	   but are not transformed into statement contracts inside the 
	   unrolled parts. *)
	(* TODO: transforms loop annotations into statement contracts 
	   inside the unrolled parts. *)
      CurrentLoc.set loc;
      let break_lbl_stmt = 
	let break_label = fresh_label () in
 	let break_lbl_stmt = mkEmptyStmt () in
	  break_lbl_stmt.labels <- [break_label];
	  break_lbl_stmt.sid <- Cil.Sid.next ();
	  break_lbl_stmt
      in
      let mk_continue () =
        let continue_label = fresh_label () in
        let continue_lbl_stmt = mkEmptyStmt () in
        continue_lbl_stmt.labels <- [continue_label] ;
        continue_lbl_stmt.sid <- Cil.Sid.next ();
        continue_lbl_stmt
      in
      let current_continue = ref (mk_continue ()) in
      let new_stmts = ref [sloop] in
      for _i=0 to number-1 do
        new_stmts:=!current_continue::!new_stmts;
        let new_block =
           copy_block
	    (Extlib.the self#current_kf)
            (Some (break_lbl_stmt,!current_continue))
            block
        in
        current_continue := mk_continue ();
	update_loop_current (Extlib.the self#current_kf) !current_continue new_block;
        (match new_block.blocals with
          [] -> new_stmts:= new_block.bstmts @ !new_stmts;
        | _ -> (* keep the block in order to preserve locals decl *)
          new_stmts:= mkStmt (Block new_block) :: !new_stmts);
      done;
      let new_stmt = match !new_stmts with
      | [ s ] -> s
      | l ->
	List.iter (update_loop_entry (Extlib.the self#current_kf) !current_continue) l;
        let l = if is_referenced !current_continue l then !current_continue :: l else l in
        let new_stmts = l @ [break_lbl_stmt] in
        let new_block = mkBlock new_stmts in
        let snew = mkStmt (Block new_block) in
        (* Move the labels in front of the original loop at the top of the
           new code *)
        Cil_datatype.Stmt.Hashtbl.add moved_labels sloop snew;
        snew.labels <- sloop.labels;
        sloop.labels <- [];
        snew;
      in new_stmt
    | _ -> assert false
    in 
    let g sloop new_stmts =
      let annot = Logic_const.new_code_annotation (AInvariant ([],true,Logic_const.pfalse))
      in Annotations.add_code_annot
	   emitter ~kf:(Extlib.the self#current_kf) sloop annot;
	new_stmts
    in
    let f = if number > 0 then f else (fun s -> s) in
    let g sloop = if is_total_unrolling then g sloop else (fun s -> s) in
    let fg sloop = g sloop (f sloop) in
    ChangeDoChildrenPost (s, fg)

  | _ -> DoChildren
end

(* Performs unrolling transformation without using -ulevel option.
   Do not forget to apply  [transformations_closure] afterwards. *)
let apply_transformation nb emitter (file,recompute_cfg) =
  (* [nb] default number of unrolling used when there is no UNROLL loop pragma.
     When [nb] is negative, no unrolling is done; all UNROLL loop pragmas 
     are ignored. *)
  if nb >= 0 then
    let visitor = new do_it (emitter, nb) in
      Kernel.debug ~dkey "Using -ulevel %d option and UNROLL loop pragmas@." nb;
      visitFramacFileSameGlobals (visitor:>Visitor.frama_c_visitor) file ;
      file,(recompute_cfg || (visitor#get_file_has_unrolled_loop ()))
  else begin
    Kernel.debug ~dkey
      "No unrolling is done; all UNROLL loop pragmas are ignored@.";
    file, recompute_cfg
  end

let transformations_closure (file,recompute_cfg) =
  if recompute_cfg then
    begin (* The CFG has be to recomputed *)
      Kernel.debug ~dkey "Closure: recomputing CFG@.";
      Cfg.clearFileCFG ~clear_id:false file;
      Cfg.computeFileCFG file;
      Ast.mark_as_changed ()
    end ;
  (file, false)
      
module Syntactic_transformations = Hook.Fold(struct type t = (Cil_types.file * bool) end)
let add_syntactic_transformation = Syntactic_transformations.extend

(* Performs and closes all syntactic transformations *)
let compute file =
  let acc = Syntactic_transformations.apply (file,false) in
  let nb = Kernel.UnrollingLevel.get () in
    ignore (transformations_closure (apply_transformation nb Emitter.end_user acc))
    
(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
