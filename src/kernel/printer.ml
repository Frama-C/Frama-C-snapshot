(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2009                                               *)
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

open Cil_types
open Cil
open Db_types
open Extlib
open Pretty_utils

module IntSet = Set.Make(struct type t = int let compare = compare end)

(* All annotation are extracted from Db.
   Generated global annotations are inserted before
   the very first function definition. User-defined global annotations are
   pretty-printed at their own place in the code.
*)
class print () = object(self)
  inherit defaultCilPrinterClass as super

  val mutable first_function_definition = true

  val mutable declared_globs = IntSet.empty

  val mutable is_fun_def = false

  method pVar fmt v =
    super#pVar fmt v;
    if  Parameters.Debug.get () > 3 then Format.fprintf fmt "/*vid:%d*/" v.vid

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
    if not (Cil.is_empty_funspec kf.spec) then begin
      Pretty_utils.pp_open_block fmt "/*@@ ";
      Format.fprintf fmt "%a@ " self#pSpec kf.spec;
      Pretty_utils.pp_close_block fmt "*/@\n";
    end

  method private pretty_global_annot fmt =
    Globals.Annotations.iter
      (fun annot is_generated ->
         if is_generated then begin
         Format.fprintf fmt "@[/*@@ %a@ @]*/@\n" self#pAnnotation annot
         end)

  (**  Do not compact statements with annotations *)
  method may_be_skipped stmt =
    Annotations.get stmt = [] && stmt.labels = []

  method pVDecl fmt vi =
    (try
       let kf = Globals.Functions.get vi in
       if not (IntSet.mem vi.vid declared_globs) &&
         (is_fun_def || not (Ast_info.Function.is_definition kf.fundec))
       then begin
         declared_globs <- IntSet.add vi.vid declared_globs;
         (* pretty prints the spec, but not for built-ins*)
         if not (Cil.BuiltinFunctions.mem vi.vname) then
           self#pretty_funspec fmt kf
       end
     with Not_found -> ());
    is_fun_def <- false;
    super#pVDecl fmt vi

  method pGlobal fmt glob =
    (match glob with
     | GVarDecl _ when first_function_definition ->
         first_function_definition <- false;
         self#pretty_global_annot fmt
     | GFun _ -> is_fun_def <- true
     | _ -> ());
    super#pGlobal fmt glob

  (* TODO: make it a public method, with a new class type specific to Frama-C*)

  method private pInsertedAnnotation fmt ca =
    match Ast_info.before_after_content ca with
      | User ca -> 
	  Format.fprintf fmt "%a" self#pCode_annot ca
      | AI(_,ca) -> 
	  Format.fprintf fmt "%a@\n    // synthesized@\n" self#pCode_annot ca

  method private pAnnotations fmt annots =
    if annots <> [] then
      begin
	Pretty_utils.pp_open_block fmt "/*@@ " ;
        Pretty_utils.pp_list ~sep:Pretty_utils.nl_sep 
	  self#pInsertedAnnotation 
          fmt
          annots ;
	Pretty_utils.pp_close_block fmt "*/@\n" ;
      end

  method pAnnotatedStmt next fmt s =
    (* To debug location setting:
      (let loc = fst (Cilutil.get_stmtLoc s.skind) in
       Format.fprintf fmt "/*Loc=%s:%d*/" loc.Lexing.pos_fname loc.Lexing.pos_lnum); *)

    (* print the labels *)
    self#pStmtLabels fmt s ;
    if verbose then Format.fprintf fmt "/*sid:%d*/@ " s.sid ;
    (* print the annotations *)
    let all_annot = Annotations.get s in
    match all_annot with 
      | [] -> self#pStmtKind next fmt s.skind
      | [a] when is_skip s.skind ->
	  Format.fprintf fmt "@[/*@@@ %a */@] %a" 
	    (self#pInsertedAnnotation) a 
	    (self#pStmtKind next) s.skind ;
      | _ ->
	  let loop_annot, stmt_annot =
	    List.partition 
	      (Ast_info.lift_annot_func Logic_utils.is_loop_annot ) 
	      all_annot
	  in
	  let annot_before,annot_after =
	    List.partition 
	      (function Before _ -> true | After _ -> false) 
	      stmt_annot
	  in
	  let loop_annot_before, loop_annot_after =
	    List.partition 
	      (function Before _ -> true | After _ -> false) 
	      loop_annot
	  in
	  begin
	    let s_block = annot_after <> [] || loop_annot_after <> [] in
	    if s_block then Pretty_utils.pp_open_block fmt "{" ;
	    self#pAnnotations fmt loop_annot_before ;
	    self#pAnnotations fmt annot_before ;
	    if s.ghost then Pretty_utils.pp_open_block fmt "/*@@ ghost" ;
	    self#pStmtKind next fmt s.skind;
	    if s.ghost then Pretty_utils.pp_close_block fmt "@ */@\n" ;
	    self#pAnnotations fmt loop_annot_after ;
	    self#pAnnotations fmt annot_after ;
	    if s_block then Pretty_utils.pp_close_block fmt "}" ;
	  end

  method requireBraces blk =
    match blk.bstmts with
      | [_] | [] when blk.battrs = [] ->
          (match self#current_stmt with
             | None -> false
             | Some stmt -> (Annotations.get stmt) <> [])
      | _ -> true

  (** Get the comment out of a location if there is one *)
  method pLineDirective ?(forcefile=false) fmt l =
    super#pLineDirective ~forcefile fmt l;
    if Parameters.PrintComments.get () then
      List.iter
        (fun c -> Format.fprintf fmt "/* %s@ */@\n" c)
        (Zrapp.get_comments l)

  initializer
    logic_printer_enabled <- false;
    verbose <- Parameters.Debug.get () >= 1
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
Ast_printer.d_file:= fun fmt x -> Cil.d_file (new print()) fmt x;;

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
