(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2012                                               *)
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
open Cil

let compare_annotations la1 la2 =
  let la1 = Annotations.code_annotation_of_rooted la1 in
  let la2 = Annotations.code_annotation_of_rooted la2 in
  let total_order = Datatype.Int.compare la1.annot_id la2.annot_id in
  match la1.annot_content,la2.annot_content with
      AAssert _, AAssert _ -> total_order
    | AAssert _,_ -> -1
    | AStmtSpec _, AStmtSpec _ -> total_order
    | AStmtSpec _, AAssert _ -> 1
    | AStmtSpec _,_ -> -1
    | AInvariant _, AAssert _ -> 1
    | AInvariant _, AStmtSpec _ -> 1
    | AInvariant ([],_,_), AInvariant ([],_,_) -> total_order
    | AInvariant ([],_,_), AAssigns ([],_) -> total_order
    | AInvariant ([],_,_), AAllocation ([],_) -> total_order
    | AInvariant ([],_,_),_ -> -1
    | AInvariant _, AInvariant([],_,_) -> 1
    | AInvariant _, AAssigns([],_) -> 1
    | AInvariant _, AAllocation([],_) -> 1
    | AInvariant _, AInvariant _ -> total_order
    | AInvariant _, AAssigns _ -> total_order
    | AInvariant _, AAllocation _ -> total_order
    | AInvariant _, _ -> -1

    | AAssigns _, AAssert _ -> 1
    | AAssigns _, AStmtSpec _ -> 1
    | AAssigns([],_),  AInvariant ([],_,_) -> total_order
    | AAssigns([],_), AAssigns ([],_) -> total_order
    | AAssigns([],_), AAllocation ([],_) -> total_order
    | AAssigns ([],_), _ -> -1
    | AAssigns _, AInvariant([],_,_) -> 1
    | AAssigns _, AAssigns([],_) -> 1
    | AAssigns _, AAllocation([],_) -> 1
    | AAssigns _, AInvariant _ -> total_order
    | AAssigns _, AAssigns _ -> total_order
    | AAssigns _, AAllocation _ -> total_order
    | AAssigns _, _ -> -1

    | AAllocation _, AAssert _ -> 1
    | AAllocation _, AStmtSpec _ -> 1
    | AAllocation([],_),  AInvariant ([],_,_) -> total_order
    | AAllocation([],_), AAssigns ([],_) -> total_order
    | AAllocation([],_), AAllocation ([],_) -> total_order
    | AAllocation ([],_), _ -> -1
    | AAllocation _, AInvariant([],_,_) -> 1
    | AAllocation _, AAssigns([],_) -> 1
    | AAllocation _, AAllocation([],_) -> 1
    | AAllocation _, AInvariant _ -> total_order
    | AAllocation _, AAssigns _ -> total_order
    | AAllocation _, AAllocation _ -> total_order
    | AAllocation _, _ -> -1

    | AVariant _, APragma _ -> -1
    | AVariant _, AVariant _ -> total_order
    | AVariant _, _ -> 1
    | APragma _, APragma _ -> total_order
    | APragma _, _ -> 1

(* All annotation are extracted from Db.
   Generated global annotations are inserted before
   the very first function definition. User-defined global annotations are
   pretty-printed at their own place in the code.
*)
class print () = object(self)
  inherit defaultCilPrinterClass as super

  val mutable declared_globs = Datatype.Int.Set.empty

  val mutable print_spec = false

  (* Are we printing ghost code? If yes, specifications are introduced by
     /@ and closed by @/. Not really tested currently, as this is not
     parseable yet.*)
  val mutable is_ghost = false

  method pVar fmt v =
    super#pVar fmt v;
    if Kernel.debug_atleast 4 then begin
      Format.fprintf fmt "/*vid:%d*/" v.vid;
      (match v.vlogic_var_assoc with
         None -> ()
       | Some v -> Format.fprintf fmt "/*lvid:%d*/" v.lv_id);
    end

  method pLogic_var fmt v =
    super#pLogic_var fmt v;
    if Kernel.debug_atleast 4 then begin
      (match v.lv_origin with
         None -> ()
       | Some v -> Format.fprintf fmt "/*vid:%d*/" v.vid);
      Format.fprintf fmt "/*lv_id:%d*/" v.lv_id
    end

  method private current_kf = match self#current_function with
  | None -> assert false
  | Some vi -> Globals.Functions.get vi

  method private current_kinstr =
    match self#current_stmt with
    | None -> Kglobal
    | Some st -> Kstmt st

  method private current_sid =
    match super#current_stmt with
    | None -> assert false
    | Some st -> st.sid

  method private pretty_funspec fmt kf =
    let spec = Annotations.funspec ~populate:false kf in
    if not (Cil.is_empty_funspec spec) then begin
      Pretty_utils.pp_open_block fmt "/*@@ ";
      Format.fprintf fmt "%a@ " self#pSpec spec;
      Pretty_utils.pp_close_block fmt "*/@\n";
    end

  (**  Do not compact statements with annotations *)
  method may_be_skipped stmt =
    not (Annotations.has_code_annot stmt) && stmt.labels = []

  method pVDecl fmt vi =
    (try
       let kf = Globals.Functions.get vi in
       if not (Datatype.Int.Set.mem vi.vid declared_globs) && print_spec
       then begin
         declared_globs <- Datatype.Int.Set.add vi.vid declared_globs;
         (* pretty prints the spec, but not for built-ins*)
         if not (Cil.Builtin_functions.mem vi.vname) then
           self#pretty_funspec fmt kf
       end
     with Not_found -> ());
    print_spec <- false;
    super#pVDecl fmt vi

  method pGlobal fmt glob =
    if Kernel.PrintComments.get () then begin
      let comments = Globals.get_comments_global glob in
      Pretty_utils.pp_list 
        ~sep:"@\n" ~suf:"@\n" 
        (fun fmt s -> Format.fprintf fmt "/* %s */" s) fmt comments
    end;
    (* Out of tree global annotations are pretty printed before the first
       variable declaration of the first function definition. *)
    (match glob with
     | GVarDecl _ | GFun _ -> print_spec <- Ast.is_last_decl glob;
     | _ -> ());
    super#pGlobal fmt glob

  (* TODO: make it a public method, with a new class type specific to Frama-C*)

  method private pInsertedAnnotation fmt ca =
    match ca with
    | User ca ->
        Format.fprintf fmt "%a" self#pCode_annot ca
    | AI(_,ca) ->
        Format.fprintf fmt "%a@\n    // synthesized@\n" self#pCode_annot ca

  method private pBeginAnnotation fmt =
    if is_ghost then Format.fprintf fmt "/@@" else Format.fprintf fmt "/*@@"

  method private pEndAnnotation fmt =
    if is_ghost then Format.fprintf fmt "@/" else Format.fprintf fmt "*/"


  method private pLoopAnnotations fmt annots =
    if annots <> [] then
      begin
        let annots = List.sort compare_annotations annots in
        Pretty_utils.pp_open_block fmt "%t " self#pBeginAnnotation;
        Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep
          self#pInsertedAnnotation
          fmt
          annots ;
        Pretty_utils.pp_close_block fmt "%t@\n" self#pEndAnnotation;
      end

  method private pAnnotations fmt annots =
    let annots = List.sort compare_annotations annots in
    Pretty_utils.pp_list
      ~pre:Pretty_utils.no_sep ~sep:Pretty_utils.no_sep ~suf:Pretty_utils.no_sep
      (fun fmt annot ->
         Pretty_utils.pp_open_block fmt "%t " self#pBeginAnnotation;
         self#pInsertedAnnotation fmt annot;
         Pretty_utils.pp_close_block fmt "%t@\n" self#pEndAnnotation)
      fmt
      annots

  method pAnnotatedStmt next fmt s =
    (* To debug location setting:
       (let loc = fst (Cil_datatype.Stmt.loc s.skind) in
       Format.fprintf fmt "/*Loc=%s:%d*/" loc.Lexing.pos_fname loc.Lexing.pos_lnum); *)

    (* print the labels *)
    self#pStmtLabels fmt s ;

    (* print the Cabscond, if any *)
    Cabscond.pp_comment fmt s ;
    Cabsbranches.pp_comment fmt s ;
    if Kernel.PrintComments.get () then begin
      let comments = Globals.get_comments_stmt s in
      Pretty_utils.pp_list 
        ~sep:"@\n" ~suf:"@\n" (fun fmt s -> Format.fprintf fmt "/* %s */" s)
        fmt comments
    end;
    if verbose then Format.fprintf fmt "/*sid:%d*/@ " s.sid ;
    (* print the annotations *)
    let all_annot =
      List.sort
        Cil_datatype.Rooted_code_annotation.compare
        (Annotations.code_annot s)
    in
    let pGhost fmt s =
      let was_ghost = is_ghost in
      if not was_ghost && s.ghost then (
        Pretty_utils.pp_open_block fmt "/*@@ ghost " ;
        is_ghost <- true
      );
      self#pStmtKind next fmt s.skind;
      if not was_ghost && s.ghost then (
        Pretty_utils.pp_close_block fmt "@,*/";
        is_ghost <- false;
      )
    in

    match all_annot with
    | [] -> pGhost fmt s
    | [ a ] when is_skip s.skind && not s.ghost ->
        Format.fprintf fmt "@[/*@@@ %a */@] %a"
          (self#pInsertedAnnotation) a
          (self#pStmtKind next) s.skind ;
    | _ ->
        let loop_annot, stmt_annot =
          List.partition
            (Ast_info.lift_annot_func Logic_utils.is_loop_annot)
            all_annot
        in
        begin
          self#pAnnotations fmt stmt_annot ;
          self#pLoopAnnotations fmt loop_annot ;
          pGhost fmt s
        end

  method requireBraces blk =
    match blk.blocals with
    | [] ->
      (match blk.bstmts with
      | [ _ ] | [] when blk.battrs = [] && blk.blocals = [] -> false
      | _ ->
	match self#current_stmt with
        | None -> false
        | Some stmt -> Annotations.has_code_annot stmt)
    | _ -> 
      true

  initializer
    logic_printer_enabled <- false;
    verbose <- Kernel.debug_atleast 1
end;;

Ast_printer.d_ident:= fun fmt x -> (new print())#pVarName fmt x;;
Ast_printer.d_exp:= fun fmt x -> Cil.printExp (new print()) fmt x;;
Ast_printer.d_var:= fun fmt x -> Cil.printVar (new print()) fmt x;;
Ast_printer.d_lval:= fun fmt x -> Cil.printLval (new print()) fmt x;;
Ast_printer.d_offset:= fun fmt x -> (new print())#pOffset fmt x;;
Ast_printer.d_init:= fun fmt x -> Cil.printInit (new print()) fmt x;;
Ast_printer.d_type:= fun fmt x -> Cil.printType (new print()) fmt x;;
Ast_printer.d_global:= fun fmt x -> Cil.printGlobal (new print()) fmt x;;
Ast_printer.d_attrlist:= fun fmt x -> Cil.printAttrs (new print()) fmt x;;
Ast_printer.d_attr:= fun fmt x -> Cil.printAttr (new print()) fmt x;;
Ast_printer.d_attrparam:= fun fmt x -> (new print())#pAttrParam fmt x;;
Ast_printer.d_label:= fun fmt x -> (new print())#pLabel fmt x;;
Ast_printer.d_stmt:= fun fmt x -> Cil.printStmt (new print()) fmt x;;
Ast_printer.d_block:= fun fmt x -> Cil.printBlock (new print()) fmt x;;
Ast_printer.d_instr:= fun fmt x -> Cil.printInstr (new print()) fmt x;;

Ast_printer.d_term_lval:= fun fmt x -> Cil.printTerm_lval (new print()) fmt x;;
Ast_printer.d_logic_var:= fun fmt x -> Cil.printLogic_var (new print()) fmt x;;
Ast_printer.d_term:= fun fmt x -> Cil.printTerm (new print()) fmt x;;
Ast_printer.d_term_offset:= fun fmt x -> Cil.printTerm_offset (new print()) fmt x;;
Ast_printer.d_predicate_named:= fun fmt x -> Cil.printPredicate_named (new print()) fmt x;;
Ast_printer.d_code_annotation:= fun fmt x -> Cil.printCode_annotation (new print()) fmt x;;
Ast_printer.d_funspec:= fun fmt x -> Cil.printFunspec (new print()) fmt x;;
Ast_printer.d_annotation:= fun fmt x -> Cil.printAnnotation (new print()) fmt x;;
Ast_printer.d_file:= fun fmt x -> Cil.printFile (new print()) fmt x;;

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
