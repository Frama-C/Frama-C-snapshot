(****************************************************************************)
(*                                                                          *)
(*  Copyright (C) 2001-2003                                                 *)
(*   George C. Necula    <necula@cs.berkeley.edu>                           *)
(*   Scott McPeak        <smcpeak@cs.berkeley.edu>                          *)
(*   Wes Weimer          <weimer@cs.berkeley.edu>                           *)
(*   Ben Liblit          <liblit@cs.berkeley.edu>                           *)
(*  All rights reserved.                                                    *)
(*                                                                          *)
(*  Redistribution and use in source and binary forms, with or without      *)
(*  modification, are permitted provided that the following conditions      *)
(*  are met:                                                                *)
(*                                                                          *)
(*  1. Redistributions of source code must retain the above copyright       *)
(*  notice, this list of conditions and the following disclaimer.           *)
(*                                                                          *)
(*  2. Redistributions in binary form must reproduce the above copyright    *)
(*  notice, this list of conditions and the following disclaimer in the     *)
(*  documentation and/or other materials provided with the distribution.    *)
(*                                                                          *)
(*  3. The names of the contributors may not be used to endorse or          *)
(*  promote products derived from this software without specific prior      *)
(*  written permission.                                                     *)
(*                                                                          *)
(*  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     *)
(*  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       *)
(*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       *)
(*  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE          *)
(*  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,     *)
(*  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,    *)
(*  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;        *)
(*  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER        *)
(*  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT      *)
(*  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN       *)
(*  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE         *)
(*  POSSIBILITY OF SUCH DAMAGE.                                             *)
(*                                                                          *)
(*  File modified by CEA (Commissariat Ã  l'Ã©nergie atomique et aux          *)
(*                        Ã©nergies alternatives)                            *)
(*               and INRIA (Institut National de Recherche en Informatique  *)
(*                          et Automatique).                                *)
(****************************************************************************)

(* cprint -- pretty printer of C program from abstract syntax
**
** Project:	FrontC
** File:	cprint.ml
** Version:	2.1e
** Date:	9.1.99
** Author:	Hugues Cassé
**
**	1.0		2.22.99	Hugues Cassé	First version.
**	2.0		3.18.99	Hugues Cassé	Compatible with Frontc 2.1, use of CAML
**									pretty printer.
**	2.1		3.22.99	Hugues Cassé	More efficient custom pretty printer used.
**	2.1a	4.12.99	Hugues Cassé	Correctly handle:
**									char *m, *m, *p; m + (n - p)
**	2.1b	4.15.99	Hugues Cassé	x + (y + z) stays x + (y + z) for
**									keeping computation order.
**	2.1c	7.23.99	Hugues Cassé	Improvement of case and default display.
**	2.1d	8.25.99	Hugues Cassé	Rebuild escape sequences in string and
**									characters.
**	2.1e	9.1.99	Hugues Cassé	Fix, recognize and correctly display '\0'.
*)

(* George Necula: I changed this pretty dramatically since CABS changed *)
open Format
open Pretty_utils
open Logic_print
open Cabs
open Escape

let version = "Cprint 2.1e 9.1.99 Hugues Cassé"

let msvcMode = ref false

let printLn = ref true
let printLnComment = ref false

let printCounters = ref false
let printComments = ref false

(*
** Expression printing
**		Priorities
**		16	variables
**		15	. -> [] call()
**		14  ++, -- (post)
**		13	++ -- (pre) ~ ! - + & *(cast)
**		12	* / %
**		11	+ -
**		10	<< >>
**		9	< <= > >=
**		8	== !=
**		7	&
**		6	^
**		5	|
**		4	&&
**		3	||
**		2	? :
**		1	= ?=
**		0	,
*)

let cast_level = 13

let get_operator exp =
  match exp.expr_node with
    NOTHING -> ("", 16)
  | PAREN _ -> ("", 16)
  | UNARY (op, _) ->
      (match op with
	MINUS -> ("-", 13)
      | PLUS -> ("+", 13)
      | NOT -> ("!", 13)
      | BNOT -> ("~", 13)
      | MEMOF -> ("*", 13)
      | ADDROF -> ("&", 13)
      | PREINCR -> ("++", 13)
      | PREDECR -> ("--", 13)
      | POSINCR -> ("++", 14)
      | POSDECR -> ("--", 14))
  | LABELADDR _ -> ("", 16)  (* Like a constant *)
  | BINARY (op, _, _) ->
      (match op with
	MUL -> ("*", 12)
      | DIV -> ("/", 12)
      | MOD -> ("%", 12)
      | ADD -> ("+", 11)
      | SUB -> ("-", 11)
      | SHL -> ("<<", 10)
      | SHR -> (">>", 10)
      | LT -> ("<", 9)
      | LE -> ("<=", 9)
      | GT -> (">", 9)
      | GE -> (">=", 9)
      | EQ -> ("==", 8)
      | NE -> ("!=", 8)
      | BAND -> ("&", 7)
      | XOR -> ("^", 6)
      | BOR -> ("|", 5)
      | AND -> ("&&", 4)
      | OR -> ("||", 3)
      | ASSIGN -> ("=", 1)
      | ADD_ASSIGN -> ("+=", 1)
      | SUB_ASSIGN -> ("-=", 1)
      | MUL_ASSIGN -> ("*=", 1)
      | DIV_ASSIGN -> ("/=", 1)
      | MOD_ASSIGN -> ("%=", 1)
      | BAND_ASSIGN -> ("&=", 1)
      | BOR_ASSIGN -> ("|=", 1)
      | XOR_ASSIGN -> ("^=", 1)
      | SHL_ASSIGN -> ("<<=", 1)
      | SHR_ASSIGN -> (">>=", 1))
  | QUESTION _ -> ("", 2)
  | CAST _ -> ("", cast_level)
  | CALL _ -> ("", 15)
  | COMMA _ -> ("", 0)
  | CONSTANT _ -> ("", 16)
  | VARIABLE _ -> ("", 16)
  | EXPR_SIZEOF _ -> ("", 16)
  | TYPE_SIZEOF _ -> ("", 16)
  | EXPR_ALIGNOF _ -> ("", 16)
  | TYPE_ALIGNOF _ -> ("", 16)
  | INDEX (_, _) -> ("", 15)
  | MEMBEROF (_, _) -> ("", 15)
  | MEMBEROFPTR (_, _) -> ("", 15)
  | GNU_BODY _ -> ("", 17)
  | EXPR_PATTERN _ -> ("", 16)     (* sm: not sure about this *)

(*
** FrontC Pretty printer
*)

let print_string fmt s = fprintf fmt "\"%s\"" (escape_string s)

let print_wstring fmt s = fprintf fmt "\"%s\"" (escape_wstring s)

(*
** Base Type Printing
*)

let rec print_specifiers fmt (specs: spec_elem list) =
  let print_spec_elem fmt = function
      SpecTypedef -> fprintf fmt "typedef"
    | SpecInline -> fprintf fmt "inline"
    | SpecStorage NO_STORAGE -> fprintf fmt "/* no storage */"
    | SpecStorage AUTO -> fprintf fmt "auto"
    | SpecStorage STATIC -> fprintf fmt "static"
    | SpecStorage EXTERN -> fprintf fmt "extern"
    | SpecStorage REGISTER -> fprintf fmt "register"
    | SpecCV CV_CONST -> fprintf fmt "const"
    | SpecCV CV_VOLATILE -> fprintf fmt "volatile"
    | SpecCV CV_RESTRICT -> fprintf fmt "restrict"
    | SpecCV (CV_ATTRIBUTE_ANNOT a) -> fprintf fmt "/*@@ %s */" a
    | SpecAttr al -> print_attribute fmt al
    | SpecType bt -> print_type_spec fmt bt
    | SpecPattern name -> fprintf fmt "@@specifier(%s)" name
  in
  Pretty_utils.pp_list ~sep:"@ " print_spec_elem fmt specs

and print_type_spec fmt = function
    Tvoid -> fprintf fmt "void"
  | Tchar -> fprintf fmt "char"
  | Tbool -> fprintf fmt "_Bool"
  | Tshort -> fprintf fmt "short"
  | Tint -> fprintf fmt "int"
  | Tlong -> fprintf fmt "long"
  | Tint64 -> fprintf fmt  "__int64"
  | Tfloat -> fprintf fmt  "float"
  | Tdouble -> fprintf fmt "double "
  | Tsigned -> fprintf fmt "signed"
  | Tunsigned -> fprintf fmt "unsigned"
  | Tnamed s -> fprintf fmt "%s" s
  | Tstruct (n, None, _) -> fprintf fmt "struct %s" n
  | Tstruct (n, Some flds, extraAttrs) ->
      fprintf fmt "@[<hov 2>%a@ {@ %a@;}@]"
        (print_struct_name_attr "struct") (n, extraAttrs) print_fields flds
  | Tunion (n, None, _) -> fprintf fmt "union %s" n
  | Tunion (n, Some flds, extraAttrs) ->
      fprintf fmt "@[<hov 2>%a@ {@ %a@;}@]"
        (print_struct_name_attr "union") (n, extraAttrs) print_fields flds
  | Tenum (n, None, _) -> fprintf fmt "enum %s" n
  | Tenum (n, Some enum_items, extraAttrs) ->
      fprintf fmt "@[<hov 2>%a@ {@ %a@;}@]"
        (print_struct_name_attr "enum") (n, extraAttrs)
        print_enum_items enum_items
  | TtypeofE e -> fprintf fmt "__typeof__(@[%a@])" print_expression e
  | TtypeofT (s,d) ->
      fprintf fmt "__typeof__(@[%a@])"print_onlytype (s, d)

(* print "struct foo", but with specified keyword and a list of
 * attributes to put between keyword and name *)
and print_struct_name_attr keyword fmt (name, extraAttrs) =
    fprintf fmt "%s%a%a@ %s"
      keyword
      (pp_cond (extraAttrs <> [])) "@ "
      print_attributes extraAttrs name

(* This is the main printer for declarations. It is easy bacause the
 * declarations are laid out as they need to be printed. *)
and print_decl (n: string) fmt = function
    JUSTBASE ->
      let cond =  n = "___missing_field_name" in
      fprintf fmt "%a%s%a" (pp_cond cond) "/*@ " n (pp_cond cond) "@ */"
  | PARENTYPE (al1, d, al2) ->
      fprintf fmt "(@[%a%a%a@])"
        print_attributes al1 (print_decl n) d print_attributes al2
  | PTR (al, d) ->
      fprintf fmt "*%a%a" print_attributes al (print_decl n) d
  | ARRAY (d, al, e) ->
      fprintf fmt "%a[@[%a%a@]]"
        (print_decl n) d print_attributes al print_expression e
  | PROTO(d, args, isva) ->
      fprintf fmt "@[%a@;(%a)@]"
        (print_decl n) d print_params (args,isva)

and print_fields fmt (flds : field_group list) =
  pp_list ~sep:"@ " print_field_group fmt flds

and print_enum_items fmt items =
  let print_item fmt (id,exp,_) =
    fprintf fmt "%s%a%a"
      id (pp_cond (exp.expr_node=NOTHING)) "@ =@ " print_expression exp
  in
  pp_list ~sep:",@ " print_item fmt items

and print_onlytype fmt (specs, dt) =
  fprintf fmt "%a%a" print_specifiers specs (print_decl "") dt

and print_name fmt ((n, decl, attrs, _) : name) =
  fprintf fmt "%a%a" (print_decl n) decl print_attributes attrs

and print_init_name fmt ((n, i) : init_name) =
  match i with
      NO_INIT -> print_name fmt n
    | _ -> fprintf fmt "%a@ =@ %a" print_name n print_init_expression i

and print_name_group fmt (specs, names) =
  fprintf fmt "%a@ %a"
    print_specifiers specs (pp_list ~sep:",@ " print_name) names

and print_field_group fmt fld = match fld with
  | FIELD (specs, fields) ->
      fprintf fmt "%a@ %a;"
        print_specifiers specs
        (pp_list ~sep:",@ " print_field) fields
  | TYPE_ANNOT annot ->
      fprintf fmt "@\n/*@@@[@ %a@]@ */@\n"
        Logic_print.print_type_annot annot

and print_field fmt (name, widtho) =
  match widtho with
      None -> print_name fmt name
    | Some w -> fprintf fmt "%a:@ %a" print_name name print_expression w

and print_init_name_group fmt (specs, names) =
  fprintf fmt "%a@ @[%a@]"
    print_specifiers specs (pp_list ~sep:",@ " print_init_name) names

and print_single_name fmt (specs, name) =
  fprintf fmt "%a@ %a" print_specifiers specs print_name name

and print_params fmt (pars,ell) =
  pp_list ~sep:",@ " print_single_name fmt pars;
  if ell then begin
    match pars with
        [] -> pp_print_string fmt "..."
      | _ -> fprintf fmt ",@ ..."
  end

and print_comma_exps fmt exps =
  pp_list ~sep:",@ " print_expression fmt exps

and print_init_expression fmt (iexp: init_expression) =
  match iexp with
    NO_INIT -> ()
  | SINGLE_INIT e -> print_expression fmt e
  | COMPOUND_INIT  initexps ->
      let doinitexp fmt = function
          NEXT_INIT, e -> print_init_expression fmt e
        | i, e ->
            let rec doinit fmt = function
                NEXT_INIT -> ()
              | INFIELD_INIT (fn, i) -> fprintf fmt ".%s%a" fn doinit i
              | ATINDEX_INIT (e, i) ->
                  fprintf fmt "[@[%a@]]%a" print_expression e doinit i
              | ATINDEXRANGE_INIT (s, e) ->
                  fprintf fmt "@[%a@;...@;%a@]"
                    print_expression s print_expression e
            in
            fprintf fmt "%a@ =@ %a"
              doinit i print_init_expression e
      in
      fprintf fmt "{@[<hov 2>%a@]}"
        (pp_list ~sep:",@ " doinitexp) initexps

and print_cast_expression fmt = function
    NO_INIT -> Kernel.fatal "no init in cast"
  | COMPOUND_INIT _ as ie ->
      fprintf fmt "(@[%a@])" print_init_expression ie
  | SINGLE_INIT e -> print_expression_level cast_level fmt e

and print_expression fmt (exp: expression) = print_expression_level 0 fmt exp

and print_expression_level (lvl: int) fmt (exp : expression) =
  let (txt, lvl') = get_operator exp in
  let print_expression fmt exp = print_expression_level lvl' fmt exp in
  let print_exp fmt e =
    Cil_const.CurrentLoc.set e.expr_loc;
    match e.expr_node with
      NOTHING -> ()
    | PAREN exp -> print_expression fmt exp
        (* parentheses are added by the level matching. *)
    | UNARY ((POSINCR|POSDECR), exp') ->
	fprintf fmt "%a%s" print_expression exp' txt
    | UNARY (_,exp') -> fprintf fmt "%s%a" txt print_expression exp'
    | LABELADDR l -> fprintf fmt "&&%s" l
    | BINARY (_op, exp1, exp2) ->
        fprintf fmt "%a@ %s@ %a"
          print_expression exp1 txt print_expression exp2
    | QUESTION (exp1, exp2, exp3) ->
        fprintf fmt "%a@ ?@ %a@ :@ %a"
          print_expression exp1 print_expression exp2 print_expression exp3
    | CAST (typ, iexp) ->
        fprintf fmt "(@[%a@])@;%a"
          print_onlytype typ print_cast_expression iexp
    | CALL ({ expr_node = VARIABLE "__builtin_va_arg"},
            [arg; { expr_node = TYPE_SIZEOF (bt, dt) } ]) ->
        fprintf fmt "__builtin_va_arg(@[%a,@ %a@])"
          (print_expression_level 0) arg print_onlytype (bt, dt)
    | CALL (exp, args) ->
        fprintf fmt "%a(@[@;%a@])"
          print_expression exp print_comma_exps args
    | CONSTANT (CONST_INT i) -> pp_print_string fmt i
    | CONSTANT (CONST_FLOAT f) -> pp_print_string fmt f
    | CONSTANT (CONST_CHAR c) -> fprintf fmt "'%s'" (escape_wstring c)
    | CONSTANT (CONST_WCHAR c) -> fprintf fmt  "L'%s'" (escape_wstring c)
    | CONSTANT (CONST_STRING s) -> print_string fmt s
    | CONSTANT (CONST_WSTRING s) -> print_wstring fmt s
    | VARIABLE name -> pp_print_string fmt name
    | EXPR_SIZEOF exp ->
        fprintf fmt "sizeof%a" print_expression exp
    | TYPE_SIZEOF (bt,dt) ->
        fprintf fmt "sizeof(@[%a@])" print_onlytype (bt,dt)
    | EXPR_ALIGNOF exp ->
        fprintf fmt "__alignof__%a" print_expression exp
    | TYPE_ALIGNOF (bt,dt) ->
        fprintf fmt "__alignof__(@[%a@])" print_onlytype (bt, dt)
    | INDEX (exp, idx) ->
        fprintf fmt "%a[@[%a@]]"
          print_expression exp (print_expression_level 0) idx
    | MEMBEROF (exp, fld) ->
        fprintf fmt "%a.%s" print_expression exp fld
    | MEMBEROFPTR (exp, fld) ->
        fprintf fmt "%a->%s"
          print_expression exp fld
    | GNU_BODY blk -> fprintf fmt "(@[%a@])" print_block blk
    | EXPR_PATTERN (name) -> fprintf fmt "@@expr(%s)" name
    | COMMA l -> pp_list ~sep:",@ " print_expression fmt l
  in
  if lvl >= lvl' then
    fprintf fmt "(@[%a@])" print_exp exp
  else print_exp fmt exp

(*
** Statement printing
*)
and print_for_init fmt fc =
  match fc with
      FC_EXP exp -> print_expression fmt exp
    | FC_DECL dec -> print_def fmt dec

and print_statement fmt stat =
  let loc = Cabshelper.get_statementloc stat in
  Cil_const.CurrentLoc.set loc;
  if Kernel.debug_atleast 2 then 
    fprintf fmt "@\n/* %a */@\n" Cil_printer.pp_location loc;
  match stat.stmt_node with
      NOP _ -> pp_print_string fmt ";"
    | COMPUTATION (exp,_) -> fprintf fmt "%a;" print_expression exp
    | BLOCK (blk, _,_) -> print_block fmt blk
    | SEQUENCE (s1, s2,_) ->
        fprintf fmt "%a;@ %a" print_statement s1 print_statement s2
    | IF (exp, s1, s2, _) ->
        fprintf fmt "@[<hov 2>if@ (@[%a@])@ %a@."
          print_expression exp print_substatement s1;
        (match s2.stmt_node with
           | NOP(_) -> fprintf fmt "@]"
           | _ -> fprintf fmt "@ else@ %a@]" print_substatement s2)
    | WHILE (annot,exp, stat,_) ->
        fprintf fmt "%a@[<hov 2>while@ (@[%a@])@ %a@]"
          (pp_list ~pre:"/*@@ @[" ~sep:"@\n" ~suf:"@]*/" print_code_annot)
          annot
          print_expression exp print_substatement stat
    | DOWHILE (annot,exp, stat, _) ->
        fprintf fmt "%a@[<hov 2>do@ %a@ while@ (@[%a@])@]"
          (pp_list ~pre:"/*@@ @[" ~sep:"@\n" ~suf:"@]*/" print_code_annot)
          annot
          print_substatement stat print_expression exp
    | FOR (annot,fc1, exp2, exp3, stat, _) ->
        fprintf fmt "%a@[<hov 2>for(@[%a;@ %a;@ %a@])@ %a@]"
          (pp_list ~pre:"/*@@ @[" ~sep:"@\n" ~suf:"@]*/" print_code_annot)
          annot
          print_for_init fc1
          print_expression exp2
          print_expression exp3
          print_substatement stat
    | BREAK _ -> pp_print_string fmt "break;"
    | CONTINUE _ -> pp_print_string fmt "continue;"
    | RETURN (exp, _) ->
      let has_paren exp =
        match exp.expr_node with
          | PAREN _ -> true
          | _  -> false in
      fprintf fmt "return%a%a;"
        (pp_cond (not (exp.expr_node = NOTHING || has_paren exp))) "@ "
	            print_expression exp
    | SWITCH (exp, stat,_) ->
        fprintf fmt "@[<hov 2>switch@ (@[%a@])@ %a@]"
          print_expression exp print_substatement stat
    | CASE (exp, stat, _) ->
        fprintf fmt "@[<2>case@ %a:@ %a@]"
          print_expression exp print_substatement stat
    | CASERANGE (expl, exph, stat, _) ->
        fprintf fmt "@[<2>case@ %a@;...@;%a:@ %a@]"
          print_expression expl
          print_expression exph
          print_substatement stat
    | DEFAULT (stat,_) ->
        fprintf fmt "@[<2>default:@ %a@]" print_substatement stat
    | LABEL (name, stat, _) ->
        fprintf fmt "@.@[<2>%s:@ %a@]" name print_substatement stat
    | GOTO (name, _) -> fprintf fmt "goto %s;" name
    | COMPGOTO (exp, _) ->
        fprintf fmt "goto@ @[*%a@];" print_expression exp
    | DEFINITION d -> print_def fmt d
    | ASM (attrs, tlist, details, _) ->
        let print_asm_operand fmt (_identop,cnstr, e) =
          fprintf fmt "@[%s@ (@[%a@])@]" cnstr print_expression e
        in
        if !msvcMode then begin
          fprintf fmt "__asm@ {@[%a@]}"
            (pp_list ~sep:"@\n" pp_print_string) tlist
        end else begin
          let print_details
              fmt { aoutputs = outs; ainputs = ins; aclobbers = clobs } =
            pp_list ~sep:",@ " print_asm_operand fmt outs;
            pp_cond (ins<>[]||clobs<>[]) fmt ":@ ";
            pp_list ~sep:",@ " print_asm_operand fmt ins;
            pp_cond (clobs<>[]) fmt ":@ ";
            pp_list ~sep:",@ " pp_print_string fmt clobs
          in
          fprintf fmt "@[__asm__%a@;(@[%a%a])@]"
            print_attributes attrs
            (pp_list ~sep:"@ " pp_print_string) tlist
            (pp_opt ~pre:":@ " print_details) details
	end
    | TRY_FINALLY (b, h, _) ->
        fprintf fmt "__try@ @[%a@]@ __finally@ @[%a@]"
          print_block b print_block h
    | TRY_EXCEPT (b, e, h, _) ->
        fprintf fmt "__try@ @[%a@]@ __except(@[%a@])@ @[%a@]"
          print_block b print_expression e print_block h
    | CODE_ANNOT (a, _) ->
        fprintf fmt "/*@@@ @[%a@]@ */"
          Logic_print.print_code_annot a
    | CODE_SPEC (a, _) ->
        fprintf fmt "/*@@@ @[%a@]@ */" Logic_print.print_spec a

and print_block fmt blk =
  fprintf fmt "@ {@ @[<hov>%a%a%a@]@ }"
    (pp_list
       ~pre:"__label__@ " ~sep:",@ " ~suf:";@\n" pp_print_string)
    blk.blabels
    (pp_list ~suf:"@ " print_attribute) blk.battrs
    (pp_list ~sep:"@ " print_statement) blk.bstmts

and print_substatement fmt stat =
  match stat.stmt_node with
    IF _
  | SEQUENCE _
  | DOWHILE _ ->
      fprintf fmt "@ {@ @[%a@]@ }" print_statement stat
  | _ ->
      print_statement fmt stat

(*
** GCC Attributes
*)
and print_attribute fmt (name,args) =
  match args with
      [] -> pp_print_string fmt name
    | _ ->
        let cond = name = "__attribute__" in
        let print_args fmt = function
            [{expr_node = VARIABLE "aconst"}] ->
              pp_print_string fmt "const"
          | [{expr_node = VARIABLE "restrict"}] ->
              pp_print_string fmt "restrict"
          | args -> pp_list ~sep:",@ " print_expression fmt args
        in
        fprintf fmt "%s(%a@[%a@]%a)"
          name (pp_cond cond) "(" print_args args (pp_cond cond) ")"

(* Print attributes. *)
and print_attributes fmt attrs =
  pp_list ~pre:"@ " ~sep:"@ " ~suf:"@ " print_attribute fmt attrs

(*
** Declaration printing
*)
and print_defs fmt defs =
  let prev = ref false in
  List.iter
    (fun (ghost,def) ->
      (match def with
	DECDEF _ -> prev := false
      | _ ->
	  if not !prev then pp_print_newline fmt ();
	  prev := true);
       if ghost then fprintf fmt "/*@@@ @[ghost@ %a@]@ */" print_def def
       else print_def fmt def
    )
    defs

and print_def fmt def =
  Cil_const.CurrentLoc.set (Cabshelper.get_definitionloc def);
  match def with
    FUNDEF (spec, proto, body, loc, _) ->
      if !printCounters then begin
        try
          let fname =
            match proto with
              (_, (n, _, _, _)) -> n
          in
          print_def fmt (DECDEF (None,([SpecType Tint],
                              [(fname ^ "__counter", JUSTBASE, [], loc),
                                NO_INIT]), loc));
        with Not_found ->
          pp_print_string fmt "/* can't print the counter */"
      end;
      fprintf fmt "@[%a%a@\n%a@]@\n"
        (Pretty_utils.pp_opt ~pre:"/*@@ @[" ~suf:"@]@\n */@\n"
           (fun fmt (spec,_) -> Logic_print.print_spec fmt spec))
        spec
        print_single_name proto print_block body

  | DECDEF (spec,names, _) ->
      fprintf fmt "@[%a%a;@\n@]"
        (Pretty_utils.pp_opt ~pre:"/*@@ @[" ~suf:"@]@\n */@\n"
           (fun fmt (spec,_) -> Logic_print.print_spec fmt spec))
        spec
        print_init_name_group names

  | TYPEDEF (names, _) ->
      fprintf fmt "@[%a;@\n@]" print_name_group names

  | ONLYTYPEDEF (specs, _) ->
      fprintf fmt "@[%a;@\n@]" print_specifiers specs

  | GLOBASM (asm, _) ->
      fprintf fmt "@[__asm__(%s);@\n@]" asm

  | GLOBANNOT (annot) ->
      fprintf fmt "@[/*@@@ @[%a@]@ */@]@\n"
        (pp_list ~sep:"@\n" Logic_print.print_decl) annot

  | CUSTOM _ -> fprintf fmt "<custom annot>"

  | PRAGMA (a,_) ->
      fprintf fmt "@[#pragma %a@]@\n" print_expression a

  | LINKAGE (n, _, dl) ->
      fprintf fmt "@[<2>extern@ %s@ {%a@;}@]" n (pp_list print_def) dl

(*  print abstrac_syntax -> ()
**		Pretty printing the given abstract syntax program.
*)
let printFile fmt ((_fname, defs) : file) = print_defs fmt defs

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
