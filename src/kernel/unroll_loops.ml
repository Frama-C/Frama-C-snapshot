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

(** Syntactic loop unrolling. *)

open Cil_types
open Cil
open Visitor

let dkey = Kernel.register_category "ulevel"

type loop_pragmas_info =
  { unroll_number: int option;
    total_unroll: Emitter.t option;
    ignore_unroll: bool }

let empty_info =
  { unroll_number = None; total_unroll = None; ignore_unroll = false }

let update_info emitter info spec =
  match spec with
    | {term_type=typ}  when Logic_typing.is_integral_type typ ->
      if Extlib.has_some info.unroll_number && not info.ignore_unroll then begin
	Kernel.warning ~once:true ~current:true
	  "ignoring unrolling directive (directive already defined)";
        info
      end else begin
	try
	  begin
            let i =
              Cil.constFold true(!Db.Properties.Interp.term_to_exp None spec)
            in
	    match isInteger i with
	      | Some i -> { info with unroll_number = Some (Integer.to_int i) }
	      | None ->
                Kernel.warning ~once:true ~current:true
		  "ignoring unrolling directive (not a constant expression)";
                info
	  end
	with Invalid_argument s -> 
	  Kernel.warning ~once:true ~current:true
	    "ignoring unrolling directive (%s)" s;
	  info
      end
    | {term_node=TConst (LStr "done") } -> { info with ignore_unroll = true }
    | {term_node=TConst (LStr "completely") } ->
      if Extlib.has_some info.total_unroll then begin
        Kernel.warning ~once:true ~current:true
          "found two total unroll pragmas";
        info
      end else { info with total_unroll = Some emitter }
    | _ -> info

let extract_from_pragmas s =
  let filter _ a = Logic_utils.is_loop_pragma a in
  let pragmas = Annotations.code_annot_emitter ~filter s in
  let get_infos info (a,e) =
    match a.annot_content with
      | APragma (Loop_pragma (Unroll_specs specs)) ->
        List.fold_left (update_info e) info specs
      | APragma (Loop_pragma _) -> info
      | _ -> assert false (* should have been filtered above. *)
  in 
  List.fold_left get_infos empty_info pragmas

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
    method! vvrbl vi =
      try ChangeTo (snd (List.find (fun (x,_) -> x.vid = vi.vid) assoc))
      with Not_found -> SkipChildren
  end
  in Visitor.visitFramacStmt visit

(* Takes care of local gotos and labels into C. *)
let update_gotos sid_tbl block =
  let goto_updater =
  object
    inherit nopCilVisitor
    method! vstmt s = match s.skind with
      | Goto(sref,_loc) ->
          (try (* A deep copy has already be done. Just modifies the reference in place. *)
             let new_stmt = Cil_datatype.Stmt.Map.find !sref sid_tbl in
              sref := new_stmt
           with Not_found -> ()) ;
	  DoChildren
      | _ -> DoChildren
   (* speed up: skip non interesting subtrees *)
   method! vvdec _ = SkipChildren (* via visitCilFunction *)
   method! vspec _ = SkipChildren (* via visitCilFunction *)
   method! vcode_annot _ = SkipChildren (* via Code_annot stmt *)
   method! vexpr _ = SkipChildren (* via stmt such as Return, IF, ... *)
   method! vlval _ = SkipChildren (* via stmt such as Set, Call, Asm, ... *)
   method! vattr _ = SkipChildren (* via Asm stmt *)
  end
  in visitCilBlock (goto_updater:>cilVisitor) block

let is_referenced stmt l =
  let module Found = struct exception Found end in
  let vis = object
    inherit Visitor.frama_c_inplace
    method! vlogic_label l =
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
let copy_annotations kf assoc labelled_stmt_tbl (break_continue_must_change, stmt_src,stmt_dst) =
  let fresh_annotation a =
    let visitor = object
      inherit Visitor.frama_c_copy (Project.current())
      method! vlogic_var_use vi =
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
      method! vlogic_label (label:logic_label) =
	match label with
	| StmtLabel (stmt) -> 
	    (try (* A deep copy has already been done. 
		    Just modifies the reference in place. *)
               let new_stmt = Cil_datatype.Stmt.Map.find !stmt labelled_stmt_tbl
	       in ChangeTo (StmtLabel (ref new_stmt))
	     with Not_found -> SkipChildren) ;
	| LogicLabel (None, _str) -> SkipChildren
	| LogicLabel (Some _stmt, str) -> ChangeTo (LogicLabel (None, str))
    end
    in visitCilCodeAnnotation (visitor:>cilVisitor) (Logic_const.refresh_code_annotation a) 
  in
  let filter_annotation a = (* Special cases for some "breaks" and "continues" clauses. *)
    (* Note: it would be preferable to do that job in the visitor of 'fresh_annotation'... *) 
    Kernel.debug ~dkey
      "Copying an annotation to stmt %d from stmt %d@."
      stmt_dst.sid stmt_src.sid;
    (* TODO: transforms 'breaks' and 'continues' clauses into unimplemented
       'gotos' clause (still undefined clause into ACSL!). *)
    (* WORKS AROUND: since 'breaks' and 'continues' clauses have not be preserved
       into the unrolled stmts, and are not yet transformed into 'gotos' (see. TODO), 
       they are not copied. *)
    match break_continue_must_change, a  with
    | (None, None), _ -> Some a (* 'breaks' and 'continues' can be kept *)
    | _, { annot_content=AStmtSpec (s,spec) } -> 
      let filter_post_cond = function
	| Breaks, _  when (fst break_continue_must_change) != None  -> 
	    Kernel.debug ~dkey "Uncopied 'breaks' clause to stmt %d@." stmt_dst.sid;
	    false
	| Continues, _ when (snd break_continue_must_change) != None -> 
	    Kernel.debug ~dkey "Uncopied 'continues' clause to stmt %d@." stmt_dst.sid;
	    false
	| _ -> true in
      let filter_behavior acc bhv = 
	let bhv = { bhv with b_post_cond = List.filter filter_post_cond bhv.b_post_cond }  in 
	  (* The default behavior cannot be removed if another behavior remains... *)
	if (Cil.is_empty_behavior bhv) &&  not (Cil.is_default_behavior bhv) then acc
	else bhv::acc
      in
      let filter_behaviors bhvs =  
	(*... so the default behavior is removed there if it is alone. *)
	match List.fold_left filter_behavior [] bhvs with
	  | [bhv] when Cil.is_empty_behavior bhv -> []
	  | bhvs -> List.rev bhvs
      in
      let spec = { spec with spec_behavior = filter_behaviors spec.spec_behavior } in
      if Cil.is_empty_funspec spec then None (* No statement contract will be added *)
      else Some { a with annot_content=AStmtSpec (s,spec) }
    | _, _ -> Some a
  in
  let new_annots =
    Annotations.fold_code_annot
      (fun emitter annot acc ->
         match filter_annotation annot with
	   | None -> acc
	   | Some filtred_annot -> (emitter, fresh_annotation filtred_annot) :: acc)
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
    method! vlogic_label =
      function
	| LogicLabel(_,"LoopCurrent") -> ChangeTo (StmtLabel (ref loop_current))
	| _ -> DoChildren
    method! vstmt_aux s =
      match s.skind with
	| Loop _ -> SkipChildren (* loop init and current are not the same here. *)
	| _ -> DoChildren
  end in
  ignore (Visitor.visitFramacBlock vis block)

let update_loop_entry kf loop_entry stmt =
  let vis = object(self)
    inherit Visitor.frama_c_inplace
    initializer self#set_current_kf kf
    method! vlogic_label =
      function
	| LogicLabel(_,"LoopEntry") -> ChangeTo (StmtLabel (ref loop_entry))
	| _ -> DoChildren
    method! vstmt_aux s =
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
  and annotated_stmts = ref [] (* for copying the annotations later. *) 
  and labelled_stmt_tbl = Cil_datatype.Stmt.Map.empty
  and calls_tbl = Cil_datatype.Stmt.Map.empty
  in
  let rec copy_stmt
      break_continue_must_change labelled_stmt_tbl calls_tbl stmt =
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
        let new_tbl = Cil_datatype.Stmt.Map.add stmt result labelled_stmt_tbl
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
      | Instr(Call _) -> Cil_datatype.Stmt.Map.add stmt result calls_tbl
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
	annotated_stmts := (break_continue_must_change, stmt,result) :: !annotated_stmts;
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
            (None, None) (* from now on break and continue can be kept *)
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
            List.map
              (fun x -> ref (Cil_datatype.Stmt.Map.find !x calls_tbl)) lst
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
           | None, _ -> stkind (* kept *)
           | (Some (brk_lbl_stmt)), _ -> Goto ((ref brk_lbl_stmt),loc)),
          labelled_stmt_tbl,
        calls_tbl
      | Continue loc ->
          (match break_continue_must_change with
           | _,None -> stkind (* kept *)
           | _,(Some (continue_lbl_stmt)) ->
               Goto ((ref continue_lbl_stmt),loc)),
          labelled_stmt_tbl,
          calls_tbl
      | Switch (e,block,stmts,loc) ->
          (* from now on break only can be kept *)
        let new_block,new_labelled_stmt_tbl,calls_tbl =
          copy_block (None, (snd break_continue_must_change)) labelled_stmt_tbl calls_tbl block
        in
        let stmts' =
          List.map
            (fun s -> Cil_datatype.Stmt.Map.find s new_labelled_stmt_tbl) stmts
        in
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

let ast_has_changed = ref false

(* Update to take into account annotations*)
class do_it ((force:bool),(times:int)) = object(self)
  inherit Visitor.frama_c_inplace
    initializer ast_has_changed := false;
  (* We sometimes need to move labels between statements. This table
     maps the old statement to the new one *)
  val moved_labels = Cil_datatype.Stmt.Hashtbl.create 17
  val mutable gotos = [] ;
  val mutable has_unrolled_loop = false ;

  val mutable file_has_unrolled_loop = false ;

  method get_file_has_unrolled_loop () = file_has_unrolled_loop ;

  method! vfunc fundec =
    assert (gotos = []) ;
    assert (not has_unrolled_loop) ;
    let post_goto_updater =
      (fun id -> 
	 if has_unrolled_loop then begin
	   List.iter
             (fun s -> match s.skind with Goto(sref,_loc) ->
	       (try 
		  let new_stmt =
                    Cil_datatype.Stmt.Hashtbl.find moved_labels !sref
                  in
		  sref := new_stmt
		with Not_found -> ())
	       | _ -> assert false)
             gotos;
           File.must_recompute_cfg id;
           ast_has_changed:=true
         end;
	 has_unrolled_loop <- false ;
	 gotos <- [] ;
	 Cil_datatype.Stmt.Hashtbl.clear moved_labels ;
	 id) in
      ChangeDoChildrenPost (fundec, post_goto_updater)

  method! vstmt_aux s = match s.skind with
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
    let infos = extract_from_pragmas s in
    let number = Extlib.opt_conv times infos.unroll_number in
    let total_unrolling = infos.total_unroll in
    let is_ignored_unrolling = not force && infos.ignore_unroll in
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
	(* Note: a goto from outside a loop to inside that loop will still 
           goes into the remaining loop. *)
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
            ((Some break_lbl_stmt),(Some !current_continue))
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
    let g sloop new_stmts = (* Adds "loop invariant \false;" to the remaining 
                               loop when "completely" unrolled. *)
      (* Note: since a goto from outside the loop to inside the loop 
               still goes into the remaining loop...*)
      match total_unrolling with
        | None -> new_stmts
        | Some emitter ->
          let annot =
            Logic_const.new_code_annotation
              (AInvariant ([],true,Logic_const.pfalse))
          in
          Annotations.add_code_annot
	    emitter ~kf:(Extlib.the self#current_kf) sloop annot;
          new_stmts
    in
    let h sloop new_stmts = (* To indicate that the unrolling has been done *)
      let specs = Unroll_specs [(Logic_const.term (TConst (LStr "done"))
				   (Ctype Cil.charPtrType)) ;
				Logic_const.tinteger number
			       ] in
      let annot =
        Logic_const.new_code_annotation (APragma (Loop_pragma specs))
      in
      Annotations.add_code_annot
        Emitter.end_user ~kf:(Extlib.the self#current_kf) sloop annot;
      new_stmts
    in
    let fgh sloop = h sloop (g sloop (f sloop)) in
    let fgh =
      if (number > 0) && not is_ignored_unrolling then fgh else (fun s -> s)
    in
    ChangeDoChildrenPost (s, fgh)

  | _ -> DoChildren
end

(* Performs unrolling transformation without using -ulevel option.
   Do not forget to apply  [transformations_closure] afterwards. *)
let apply_transformation ?(force=true) nb file =
  (* [nb] default number of unrolling used when there is no UNROLL loop pragma.
     When [nb] is negative, no unrolling is done; all UNROLL loop pragmas 
     are ignored. *)
  if nb >= 0 then
    let visitor = new do_it (force, nb) in
      Kernel.debug ~dkey "Using -ulevel %d option and UNROLL loop pragmas@." nb;
      visitFramacFileSameGlobals (visitor:>Visitor.frama_c_visitor) file;
      if !ast_has_changed then Ast.mark_as_changed ()
  else begin
    Kernel.debug ~dkey
      "No unrolling is done; all UNROLL loop pragmas are ignored@."
  end

(* Performs and closes all syntactic transformations *)
let compute file =
  let nb = Kernel.UnrollingLevel.get () in
  let force = Kernel.UnrollingForce.get () in
  apply_transformation ~force nb file

let unroll_transform =
  File.register_code_transformation_category "loop unrolling"

let () = 
  File.add_code_transformation_after_cleanup 
    ~deps:[(module Kernel.UnrollingLevel:Parameter_sig.S);
           (module Kernel.UnrollingForce:Parameter_sig.S)]
    unroll_transform compute

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
