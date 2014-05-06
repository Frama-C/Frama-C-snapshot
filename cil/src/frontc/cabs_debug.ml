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


open Cabs
open Format

let pp_cabsloc fmt (pos1 , _pos2) =
  fprintf fmt "%d,%s" pos1.Lexing.pos_lnum (Filename.basename (pos1.Lexing.pos_fname))

let pp_storage  fmt = function
  |	NO_STORAGE -> fprintf fmt "NO_STORAGE"
  |	AUTO -> fprintf fmt "AUTO"
  |	STATIC -> fprintf fmt "STATIC"
  |	EXTERN -> fprintf fmt "EXTERN"
  |	REGISTER -> fprintf fmt "REGISTER"

let pp_fun_spec  fmt = function
  |	INLINE -> fprintf fmt "INLINE"
  |	VIRTUAL -> fprintf fmt "VIRTUAL"
  |	EXPLICIT -> fprintf fmt "EXPLICIT"

let pp_cvspec  fmt = function
  |	CV_CONST -> fprintf fmt "CV_CONST"
  |	CV_VOLATILE -> fprintf fmt "CV_VOLATILE"
  |	CV_RESTRICT -> fprintf fmt "CV_RESTRICT"
  |	CV_ATTRIBUTE_ANNOT s -> fprintf fmt "CV_ATTRIBUTE_ANNOT %s" s

let pp_const fmt = function
  |	CONST_INT s -> fprintf fmt "CONST_INT %s" s
  |	CONST_FLOAT s -> fprintf fmt "CONST_FLOAT %s" s
  |	CONST_CHAR l ->
    fprintf fmt "CONST_CHAR{";
    List.iter (fun i -> fprintf fmt ",@ %s" (Int64.to_string i)) l;
    fprintf fmt "}"
  |	CONST_WCHAR l -> 
    fprintf fmt "CONST_WCHAR{";
    List.iter (fun i -> fprintf fmt ",@ %s" (Int64.to_string i)) l;
    fprintf fmt "}"
  |	CONST_STRING s -> fprintf fmt "CONST_STRING %s" s
  |	CONST_WSTRING l -> 
    fprintf fmt "CONST_WSTRING{";
    List.iter (fun i -> fprintf fmt ",@ %s" (Int64.to_string i)) l;
    fprintf fmt "}"

let pp_labels fmt lbls =
  fprintf fmt "%s" (String.concat " " lbls)
  
    
let rec pp_typeSpecifier fmt = function
  |     Tvoid -> fprintf fmt "Tvoid"
  |	Tchar -> fprintf fmt "Tchar"
  |	Tbool -> fprintf fmt "Tbool"
  |	Tshort -> fprintf fmt "Tshort"
  |	Tint -> fprintf fmt "Tint"
  |	Tlong -> fprintf fmt "Tlong"
  |	Tint64 -> fprintf fmt "Tint64"
  |	Tfloat -> fprintf fmt "Tfloat"
  |	Tdouble -> fprintf fmt "Tdouble"
  |	Tsigned -> fprintf fmt "Tsigned"
  |	Tunsigned -> fprintf fmt "Tunsigned"
  |	Tnamed s -> fprintf fmt "%s" s
  |	Tstruct (sname, None, alist) -> fprintf fmt "struct@ %s {} %a" sname pp_attrs alist
  | 	Tstruct (sname, Some fd_gp_list, alist) ->
    fprintf fmt "struct@ %s {%a}@ attrs=(%a)" sname pp_field_groups fd_gp_list pp_attrs alist
  |	Tunion (uname, None, alist) -> fprintf fmt "union@ %s {} %a" uname pp_attrs alist
  | Tunion (uname, Some fd_gp_list, alist) ->
    fprintf fmt "union@ %s {%a}@ attrs=(%a)" uname pp_field_groups fd_gp_list pp_attrs alist
  |	Tenum (ename, None, alist) -> fprintf fmt "enum@ %s {} %a" ename pp_attrs alist
  |     Tenum (ename, Some e_item_list, alist) -> 
    fprintf fmt "enum@ %s {" ename;
    List.iter (fun e -> fprintf fmt ",@ %a" pp_enum_item e) e_item_list;
    fprintf fmt "}@ %a" pp_attrs alist;
  |	TtypeofE exp -> fprintf fmt "typeOfE %a" pp_exp exp
  |	TtypeofT (spec, d_type) -> fprintf fmt "typeOfT(%a,%a)" pp_spec spec pp_decl_type d_type


and pp_spec_elem  fmt = function
  |	SpecTypedef -> fprintf fmt "SpecTypedef"
  |	SpecCV cvspec -> fprintf fmt "SpecCV %a" pp_cvspec cvspec
  |	SpecAttr attr -> fprintf fmt "SpecAttr %a" pp_attr attr
  |	SpecStorage storage -> fprintf fmt "SpecStorage %a" pp_storage storage
  |	SpecInline -> fprintf fmt "SpecInline"
  |	SpecType typeSpec -> fprintf fmt "SpecType %a" pp_typeSpecifier typeSpec
  |	SpecPattern s -> fprintf fmt "SpecPattern %s" s

and pp_spec fmt spec_elems =
  fprintf fmt "@[<hv 2>{" ;
  List.iter
    (fun s -> fprintf fmt "@ %a" pp_spec_elem s)
    spec_elems ;
  fprintf fmt "} @]"

and pp_decl_type fmt = function
  |	JUSTBASE -> fprintf fmt "@[<hov 2>JUSTBASE@]"
  |	PARENTYPE (attrs1, decl_type, attrs2)
     -> fprintf fmt "@[<hov 2>PARENTYPE(%a, %a, %a)@]" pp_attrs attrs1 pp_decl_type decl_type pp_attrs attrs2
  |	ARRAY (decl_type, attrs, exp)
     -> fprintf fmt "@[<hov 2>ARRAY[%a, %a, %a]@]" pp_decl_type decl_type pp_attrs attrs pp_exp exp
  |	PTR (attrs, decl_type) -> fprintf fmt "@[<hov 2>PTR(%a, %a)@]" pp_attrs attrs pp_decl_type decl_type
  |	PROTO (decl_type, single_names, b)
    -> fprintf fmt "@[<hov 2>PROTO decl_type(%a), single_names(" pp_decl_type decl_type;
      List.iter (fun sn -> fprintf fmt ",@ %a" pp_single_name sn) single_names;
      fprintf fmt "),@ %b@]" b
      
and pp_name_group fmt (spec, names) =
  fprintf fmt "@[<hov 2>name_group@ spec(%a), names{" pp_spec spec;
  List.iter
    (fun n -> fprintf fmt "@ %a" pp_name n)
    names;
  fprintf fmt "}@]"

(* Warning : printing for TYPE_ANNOT is not complete *)
and pp_field_group fmt = function
  | FIELD (spec, l) ->
    fprintf fmt "@[<hov 2>FIELD spec(%a), {" pp_spec spec;
    List.iter
      (fun (n,e_opt) -> fprintf fmt "@ %a" pp_name n;
	match e_opt with Some exp -> fprintf fmt "@ %a" pp_exp exp | _ -> ())
      l;
    fprintf fmt "}@]"
  | TYPE_ANNOT _ -> fprintf fmt "TYPE_ANNOT"

and pp_field_groups fmt l =
  fprintf fmt "{";
  List.iter (fun a -> fprintf fmt ",@ %a" pp_field_group a) l;
  fprintf fmt "}"

and pp_init_name_group fmt (spec,init_names) =
  fprintf fmt "@[<hov 2>init_name_group spec(%a), {" pp_spec spec;
  List.iter
    ( fun i -> fprintf fmt "@ %a" pp_init_name i)
    init_names;
  fprintf fmt "}@]"
  
and pp_name fmt (s,decl_type,attrs,loc) =
  fprintf fmt "name %s, decl_type(%a), attrs(%a), loc(%a)"
    s pp_decl_type decl_type pp_attrs attrs pp_cabsloc loc
    
and pp_init_name fmt (name,init_exp) =
  fprintf fmt "init_name name(%a), init_exp(%a)" pp_name name pp_init_exp init_exp
    
and pp_single_name fmt (spec,name) =
  fprintf fmt "@[<hov 2>single_name{spec(%a), name(%a)}@]" pp_spec spec pp_name name
    
and pp_enum_item fmt (s,exp,loc) =
  fprintf  fmt "@[<hov 2>enum_item %s, exp(%a, loc(%a))@]" s pp_exp exp pp_cabsloc loc


(* Warning : printing for GLOBANNOT and CUSTOM is not complete *)
and pp_def fmt = function
  |	FUNDEF (_, single_name, bl, loc1, loc2) ->
    fprintf fmt "@[<hov 2>FUNDEF (%a), loc1(%a), loc2(%a) {%a} @]"
      pp_single_name single_name pp_cabsloc loc1 pp_cabsloc loc2 pp_block bl
  |	DECDEF (_, init_name_group, loc) ->
    fprintf fmt "@[<hov 2>DECDEF (%a, loc(%a))@]" pp_init_name_group init_name_group pp_cabsloc loc
  |	TYPEDEF (name_group, loc) -> (* typedef normal *)
    fprintf fmt "@[<hov 2>TYPEDEF (%a), loc(%a)@]" pp_name_group name_group pp_cabsloc loc 
  |	ONLYTYPEDEF (spec, loc) -> (* ex : struct s{...}; *)
    fprintf fmt "@[<hov 2>ONLYTYPEDEF (%a), loc(%a)@]" pp_spec spec  pp_cabsloc loc 
  |	GLOBASM (s, loc) ->
    fprintf fmt "@[<hov 2>GLOBASM %s, loc(%a)@]" s pp_cabsloc loc 
  |	PRAGMA (exp, loc) ->
    fprintf  fmt "@[<hov 2>PRAGMA exp(%a, loc(%a))@]" pp_exp exp pp_cabsloc loc
  |	LINKAGE (s, loc, defs) ->
    fprintf  fmt "@[<hov 2>LINKAGE %s, loc(%a), defs(" s pp_cabsloc loc;
    List.iter (fun def -> fprintf fmt ",@ def(%a)" pp_def def) defs;
    fprintf fmt ")@]"
  |	GLOBANNOT _ -> fprintf fmt "GLOBANNOT"
  |	CUSTOM _ -> fprintf fmt "CUSTOM"
    
and pp_file fmt (s,l) =
  fprintf fmt "@[FILE %s, {" s;
  List.iter
    (fun (b,def) -> fprintf fmt "@ %b, def(%a)" b pp_def def)
    l;
  fprintf fmt "@] }"

and pp_block fmt bl =
  fprintf fmt "@[<hv 2>labels(%a), attrs(%a), {" pp_labels bl.blabels pp_attrs bl.battrs;    
  List.iter
    (fun s -> fprintf fmt "@ %a" pp_stmt s)
    bl.bstmts ;
  fprintf fmt "}@]"

(* Warning : printing for ASM, CODE_ANNOT and CODE_SPEC is not complete *)
and pp_raw_stmt fmt = function
  |	NOP loc -> fprintf fmt "@[<hov 2>NOP loc(%a)@]" pp_cabsloc loc 
  |	COMPUTATION (exp, loc) ->
    fprintf fmt "@[<hov 2>COMPUTATION exp(%a, loc(%a))@]" pp_exp exp pp_cabsloc loc 
  |	BLOCK  (bl, loc1, loc2) ->
   fprintf fmt "@[<hov 2>BLOCK loc1(%a), loc2(%a) {%a} @]"
    pp_cabsloc loc1 pp_cabsloc loc2 pp_block bl
  |	SEQUENCE (stmt1, stmt2, loc) ->
    fprintf fmt "@[<hov 2>SEQUENCE stmt(%a), stmt(%a), loc(%a)@]" pp_stmt stmt1 pp_stmt stmt2 pp_cabsloc loc
  |	IF (exp, stmt1, stmt2, loc) ->
    fprintf fmt "@[<hov 2>IF cond(%a), stmt(%a), stmt(%a), loc(%a)@]"
      pp_exp exp pp_stmt stmt1 pp_stmt stmt2 pp_cabsloc loc    
  |	WHILE (_loop_inv, exp, stmt, loc) -> (* Warning : no printing for loop_invariant *)
     fprintf fmt "@[<hov 2>WHILE cond(%a), stmt(%a), loc(%a)@]"
         pp_exp exp pp_stmt stmt pp_cabsloc loc    
  |	DOWHILE (_loop_inv, exp, stmt, loc) -> (* Warning : no printing for loop_invariant *)
    fprintf fmt "@[<hov 2>DOWHILE cond(%a), stmt(%a), loc(%a)@]"
      pp_exp exp pp_stmt stmt pp_cabsloc loc    
  |	FOR (_loop_inv, for_clause, exp1, exp2, stmt, loc) -> (* Warning : no printing for loop_invariant *)
    fprintf fmt "@[<hov 2>FOR for_clause(%a), exp1(%a), exp2(%a), stmt(%a), loc(%a)@]"
      pp_for_clause for_clause pp_exp exp1 pp_exp exp2 pp_stmt stmt pp_cabsloc loc    
  |	BREAK loc -> fprintf fmt "@[<hov 2>BREAK loc(%a)@]" pp_cabsloc loc
  |	CONTINUE loc -> fprintf fmt "@[<hov 2>CONTINUE loc(%a)@]" pp_cabsloc loc
  |	RETURN (exp, loc) ->  fprintf fmt "@[<hov 2>RETURN exp(%a, loc(%a))@]" pp_exp exp pp_cabsloc loc
  |	SWITCH (exp, stmt, loc) ->
    fprintf fmt "@[<hov 2>SWITH exp(%a), stmt(%a), loc(%a)@]" pp_exp exp pp_stmt stmt pp_cabsloc loc
  |	CASE (exp, stmt, loc) ->
    fprintf fmt "@[<hov 2>CASE exp(%a), stmt(%a), loc(%a)@]" pp_exp exp pp_stmt stmt pp_cabsloc loc
  |	CASERANGE (exp1, exp2, stmt, loc) ->
    fprintf fmt "@[<hov 2>CASE exp(%a), exp(%a), stmt(%a), loc(%a)@]" pp_exp exp1 pp_exp exp2 pp_stmt stmt pp_cabsloc loc
  |	DEFAULT (stmt, loc) ->
    fprintf fmt "@[<hov 2>DEFAULT stmt(%a), loc(%a)@]" pp_stmt stmt pp_cabsloc loc
  |	LABEL (s, stmt, loc) ->
    fprintf fmt "@[<hov 2>LABEL %s stmt(%a), loc(%a)@]" s pp_stmt stmt pp_cabsloc loc
  |	GOTO (s, loc) ->
    fprintf fmt "@[<hov 2>GOTO %s, loc(%a)@]" s pp_cabsloc loc
  |	COMPGOTO (exp, loc) ->  fprintf fmt "@[<hov 2>COMPGOTO exp(%a, loc(%a))@]" pp_exp exp pp_cabsloc loc
  |	DEFINITION def -> fprintf fmt "@[<hov 2>DEFINITION %a@]" pp_def def
  |	ASM (_,_,_,_) -> fprintf fmt "ASM"
  |	TRY_EXCEPT (bl1, exp, bl2, loc) ->
    fprintf fmt "@[<hov 2>TRY_EXCEPT block(%a) exp(%a) block(%a) loc(%a)@]"
      pp_block bl1 pp_exp exp pp_block bl2 pp_cabsloc loc
  |	TRY_FINALLY (bl1, bl2, loc) ->
    fprintf fmt "@[<hov 2>TRY_EXCEPT block(%a) block(%a) loc(%a)@]"
      pp_block bl1 pp_block bl2 pp_cabsloc loc
  |	CODE_ANNOT (_,_) -> fprintf fmt "CODE_ANNOT"
  |     CODE_SPEC _ -> fprintf fmt "CODE_SPEC"

and pp_stmt fmt stmt =
  fprintf fmt "@[<hov 2>ghost(%b), stmt(%a)@]" stmt.stmt_ghost pp_raw_stmt stmt.stmt_node

(*and loop_invariant = Logic_ptree.code_annot list *)

and pp_for_clause fmt = function
    |	FC_EXP exp -> fprintf fmt "@[<hov 2>FC_EXP %a@]" pp_exp exp
    |	FC_DECL def -> fprintf fmt "@[<hov 2>FC_DECL %a@]" pp_def def
	
and pp_bin_op fmt = function
  |	ADD -> fprintf fmt "ADD"
  |	SUB -> fprintf fmt "SUB"
  |	MUL -> fprintf fmt "MUL"
  |	DIV -> fprintf fmt "DIV"
  |	MOD -> fprintf fmt "MOD"
  |	AND -> fprintf fmt "AND"
  |	OR -> fprintf fmt "OR"
  |	BAND -> fprintf fmt "BAND"
  |	BOR -> fprintf fmt "BOR"
  |	XOR -> fprintf fmt "XOR"
  |	SHL -> fprintf fmt "SHL"
  |	SHR -> fprintf fmt "SHR"
  |	EQ -> fprintf fmt "EQ"
  |	NE -> fprintf fmt "NE"
  |	LT -> fprintf fmt "LT"
  |	GT -> fprintf fmt "GT"
  |	LE -> fprintf fmt "LE"
  |	GE -> fprintf fmt "GE"
  |	ASSIGN -> fprintf fmt "ASSIGN"
  |	ADD_ASSIGN -> fprintf fmt "ADD_ASSIGN"
  |	SUB_ASSIGN -> fprintf fmt "SUB_ASSIGN"
  |	MUL_ASSIGN -> fprintf fmt "MUL_ASSIGN"
  |	DIV_ASSIGN -> fprintf fmt "DIV_ASSIGN"
  |	MOD_ASSIGN -> fprintf fmt "MOD_ASSIGN"
  |	BAND_ASSIGN -> fprintf fmt "BAND_ASSIGN"
  |	BOR_ASSIGN -> fprintf fmt "BOR_ASSIGN"
  |	XOR_ASSIGN -> fprintf fmt "XOR_ASSIGN"
  |	SHL_ASSIGN -> fprintf fmt "SHL_ASSIGN"
  |	SHR_ASSIGN -> fprintf fmt "SHR_ASSIGN"

and pp_un_op fmt = function
  |	MINUS -> fprintf fmt "MINUS"
  |	PLUS -> fprintf fmt "PLUS"
  |	NOT -> fprintf fmt "NOT"
  |	BNOT -> fprintf fmt "BNOT"
  |	MEMOF -> fprintf fmt "MEMOF"
  |	ADDROF -> fprintf fmt "ADDROF"
  |	PREINCR -> fprintf fmt "PREINCR"
  |	PREDECR -> fprintf fmt "PREDECR"
  |	POSINCR -> fprintf fmt "POSINCR"
  |	POSDECR -> fprintf fmt "POSDECR"

and pp_exp fmt exp =
  fprintf fmt "exp(%a)" pp_exp_node exp.expr_node
    
and pp_exp_node fmt = function
    |	NOTHING -> fprintf fmt "NOTHING"
    |	UNARY (un_op, exp) -> fprintf fmt "@[<hov 2>%a(%a)@]" pp_un_op un_op pp_exp exp
    |	LABELADDR s -> fprintf fmt "@[<hov 2>LABELADDR %s@]" s
    |	BINARY (bin_op, exp1, exp2) ->
      fprintf fmt "@[<hov 2>%a %a %a@]" pp_exp exp1 pp_bin_op bin_op pp_exp exp2
    |	QUESTION (exp1, exp2, exp3) ->
      fprintf fmt "@[<hov 2>QUESTION(%a, %a, %a)@]" pp_exp exp1 pp_exp exp2 pp_exp exp3
    |	CAST ((spec, decl_type), init_exp) ->
      fprintf fmt "@[<hov 2>CAST (%a, %a) %a@]" pp_spec spec pp_decl_type decl_type pp_init_exp init_exp
    |	CALL (exp1, exps) ->
      fprintf fmt "@[<hov 2>CALL %a {" pp_exp exp1;
      List.iter
	(fun e -> fprintf fmt ",@ %a" pp_exp e)
	exps;
      fprintf fmt "}@]"
    |	COMMA exps ->
      fprintf fmt "@[<hov 2>COMMA {";
      List.iter
	(fun e -> fprintf fmt ",@ %a" pp_exp e)
	exps;
      fprintf fmt "}@]"
    |	CONSTANT c -> fprintf fmt "%a" pp_const c
    |	PAREN exp -> fprintf fmt "PAREN(%a)" pp_exp exp
    |	VARIABLE s -> fprintf fmt "VAR %s" s
    |	EXPR_SIZEOF exp -> fprintf fmt "EXPR_SIZEOF(%a)" pp_exp exp
    |	TYPE_SIZEOF (spec, decl_type) ->
      fprintf fmt "TYP_SIZEOF(%a,%a)" pp_spec spec pp_decl_type decl_type
    |	EXPR_ALIGNOF exp ->
      fprintf fmt "EXPR_ALIGNOF(%a)" pp_exp exp
    |	TYPE_ALIGNOF (spec, decl_type) ->
      fprintf fmt "TYP_ALIGNEOF(%a,%a)" pp_spec spec pp_decl_type decl_type
    |	INDEX (exp1, exp2) ->
      fprintf fmt "INDEX(%a, %a)" pp_exp exp1 pp_exp exp2
    |	MEMBEROF (exp, s) ->
      fprintf fmt "MEMBEROF(%a,%s)" pp_exp exp s
    |	MEMBEROFPTR (exp, s) ->
      fprintf fmt "MEMBEROFPTR(%a,%s)" pp_exp exp s
    |	GNU_BODY bl -> fprintf fmt "GNU_BODY %a" pp_block bl
    |	EXPR_PATTERN s -> fprintf fmt "EXPR_PATTERN %s" s
      
and pp_init_exp fmt = function
  |	NO_INIT -> fprintf fmt "NO_INIT"
  |	SINGLE_INIT exp ->
    fprintf fmt "SINGLE_INIT %a" pp_exp exp
  |	COMPOUND_INIT l ->
    fprintf fmt "@[<hov 2>COMPOUND_INIT {";
    match l with
      | [] -> fprintf fmt "}@]"
      | (iw, ie)::rest ->
	fprintf fmt ",@ (%a, %a)" pp_initwhat iw pp_init_exp ie;
	List.iter (fun (iw, ie) -> fprintf fmt ",@ (%a, %a)" pp_initwhat iw pp_init_exp ie) rest;
	fprintf fmt "}@]"
      
and pp_initwhat fmt = function
  |	NEXT_INIT -> fprintf fmt "NEXT_INIT"
  |	INFIELD_INIT (s,iw) -> fprintf fmt "@[<hov 2>INFIELD_INIT (%s, %a)@]" s pp_initwhat iw
  |	ATINDEX_INIT (exp,iw) -> fprintf fmt "@[<hov 2>ATINDEX_INIT (%a, %a)@]" pp_exp exp pp_initwhat iw
  |	ATINDEXRANGE_INIT (exp1, exp2) -> fprintf fmt "@[<hov 2>ATINDEXRANGE_INIT (%a, %a)@]" pp_exp exp1 pp_exp exp2
    
and pp_attr fmt (s,el) =
  fprintf fmt "ATTR (%s, {" s;
  match el with
    | [] -> fprintf fmt "})"
    | e :: es ->
      fprintf fmt ",@ %a" pp_exp e;
      List.iter (fun e -> fprintf fmt ",@ %a" pp_exp e) es;
      fprintf fmt "})"

and pp_attrs fmt l =
  fprintf fmt "{";
  match l with
    | [] -> fprintf fmt "}"
    | a :: attrs ->
      fprintf fmt ",@ %a" pp_attr a;
      List.iter (fun a -> fprintf fmt ",@ %a" pp_attr a) attrs;
      fprintf fmt "}"
