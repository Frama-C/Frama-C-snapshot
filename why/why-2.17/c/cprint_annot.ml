(**************************************************************************)
(*                                                                        *)
(*  The Why platform for program certification                            *)
(*  Copyright (C) 2002-2008                                               *)
(*    Romain BARDOU                                                       *)
(*    Jean-François COUCHOT                                               *)
(*    Mehdi DOGGUY                                                        *)
(*    Jean-Christophe FILLIÂTRE                                           *)
(*    Thierry HUBERT                                                      *)
(*    Claude MARCHÉ                                                       *)
(*    Yannick MOY                                                         *)
(*    Christine PAULIN                                                    *)
(*    Yann RÉGIS-GIANAS                                                   *)
(*    Nicolas ROUSSET                                                     *)
(*    Xavier URBAIN                                                       *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: cprint_annot.ml,v 1.12 2008/11/05 14:03:13 filliatr Exp $ i*)

(* Pretty-printer for typed AST *)

open Format
open Ctypes
open Clogic
open Cast
open Info
open Pp

let rec ctype fmt ty =
  ctype_node fmt ty.Ctypes.ctype_node

and ctype_node fmt = function
  | Tvoid -> fprintf fmt "void"
  | Tint _ -> fprintf fmt "int"
  | Tfloat _ -> fprintf fmt "float"
  | Ctypes.Tvar s -> fprintf fmt "%s" s
  | Tarray (ty, None) -> fprintf fmt "%a[]" ctype ty
  | Tarray (ty, Some n) -> fprintf fmt "%a[%Ld]" ctype ty n
  | Tpointer ty -> fprintf fmt "%a*" ctype ty
  | Tstruct s -> fprintf fmt "struct %s" s
  | Tunion s -> fprintf fmt "union %s" s
  | Tenum s -> fprintf fmt "enum %s" s
  | Tfun _ -> fprintf fmt "<fun>"

let declare_struct fmt s (_,fields) =
  fprintf fmt "@[<hov 2>struct %s {@\n" s;
  begin match Cenv.tag_type_definition s with
    | Cenv.TTStructUnion(_,fields) ->
	List.iter (fun f ->
		     fprintf fmt "%a %s;@\n" ctype f.var_type f.var_unique_name) fields 
    | _ -> assert false
  end;
  fprintf fmt "};@]@\n@\n"

let term_unop = function
  | Clogic.Uminus -> "-"
  | Clogic.Utilde -> "~"
  | Clogic.Ustar -> "*"
  | Clogic.Uamp -> "&"
  | Clogic.Ufloat_of_int -> "float_of_int"
  | Clogic.Uint_of_float -> "int_of_float"

let term_binop = function
  | Clogic.Badd -> "+"
  | Clogic.Bsub -> "-"
  | Clogic.Bmul -> "*"
  | Clogic.Bdiv -> "/"
  | Clogic.Bmod -> "%"
 
let rec term fmt t = match t.term_node with
  | Tconstant (IntConstant s | FloatConstant s) ->
      fprintf fmt "%s" s
  | Tvar x ->
      fprintf fmt "%s" x.var_name
  | Tapp (li, tl) ->
      fprintf fmt "%s(%a)" li.logic_name (print_list comma term) tl
  | Tunop (op, t) ->
      fprintf fmt "%s%a" (term_unop op) term_p t
(*  | Tstar t ->
      fprintf fmt "*%a" term_p t*)
  | Tdot (t, vi) ->
       fprintf fmt "%a.%s" term_p t vi.var_name
  | Tarrget (t, n) ->
      fprintf fmt "%a[%a]" term_p t term_p n
  | Tbinop (t1, op, t2) ->
      fprintf fmt "%a %s %a" term_p t1 (term_binop op) term_p t2
  | Tarrow (t, vi) ->
      fprintf fmt "%a->%s" term_p t vi.var_name
  | Tif (t1, t2, t3) ->
      fprintf fmt "%a ? %a : %a" term_p t1 term_p t2 term_p t3
  | Told t ->
      fprintf fmt "\\old(%a)" term t
  | Tat (t, l) ->
      fprintf fmt "\\at(%a, %s)" term t l
  | Tbase_addr t ->
      fprintf fmt "\\base_addr(%a)" term t
  | Tblock_length t ->
      fprintf fmt "\\block_length(%a)" term t
  | Tarrlen t ->
      fprintf fmt "\\arrlen(%a)" term t
  | Tstrlen t ->
      fprintf fmt "\\strlen(%a)" term t
  | Tnull ->
      fprintf fmt "null"
  | Tcast (ty, t) ->
      fprintf fmt "(%a)%a" ctype ty term t
  | Trange (t1, t2, t3) ->
      fprintf fmt "%a[%a..%a]" term t1 term_option t2 term_option t3

and term_p fmt t = match t.term_node with
  | Tconstant _ | Tvar _ | Tapp _ | Tnull | Told _ | Tat _ ->
      term fmt t
  | _ ->
      fprintf fmt "(%a)" term t

and term_option fmt = function
  | None -> ()
  | Some t -> term fmt t

let quantifier fmt (ty, x) = fprintf fmt "%a %s" ctype ty x.var_name

let quantifiers = print_list comma quantifier

let relation = function
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Eq -> "=="
  | Neq -> "!="
 
let rec predicate fmt = function
  | Pfalse ->
      fprintf fmt "false"
  | Ptrue ->
      fprintf fmt "true"
  | Papp (li, tl) ->
      fprintf fmt "%s(%a)" li.logic_name (print_list comma term) tl
  | Prel (t1, rel, t2) ->
      fprintf fmt "%a %s %a" term t1 (relation rel) term t2
  | Pand (p1, p2) ->
      fprintf fmt "%a &&@ %a" predicate p1 predicate p2
  | Por (p1, p2) ->
      fprintf fmt "%a ||@ %a" predicate p1 predicate p2
  | Pimplies (p1, p2) ->
      fprintf fmt "%a ->@ %a" predicate p1 predicate p2
  | Piff (p1, p2) ->
      fprintf fmt "%a <->@ %a" predicate p1 predicate p2
  | Pnot p ->
      fprintf fmt "! %a" predicate p
  | Pif (t, p1, p2) ->
      fprintf fmt "%a ? %a : %a" term t predicate p1 predicate p2
  | Pforall (q, p) ->
      fprintf fmt "\\forall %a;@ %a" quantifiers q predicate p
  | Pexists (q, p) ->
      fprintf fmt "\\exists %a;@ %a" quantifiers q predicate p
  | Pold p ->
      fprintf fmt "\\old(%a)" predicate p
  | Pat (p, l) ->
      fprintf fmt "\\at(%a, %s)" predicate p l
  | Pseparated (t1,t2) ->
      fprintf fmt "\\separated(%a, %a)" term t1 term t2  
  | Pfullseparated (t1,t2) ->
      fprintf fmt "\\fullseparated(%a, %a)" term t1 term t2
  | Pvalid t ->
      fprintf fmt "\\valid(%a)" term t
  | Pvalid_index (t1, t2) ->
      fprintf fmt "\\valid_index(%a, %a)" term t1 term t2
  | Pvalid_range (t1, t2, t3) ->
      fprintf fmt "\\valid_range(%a, %a, %a)" term t1 term t2 term t3
  | Pfresh t ->
      fprintf fmt "\\fresh(%a)" term t
  | Pnamed (id, p) ->
      fprintf fmt "%s:: %a" id predicate p

let parameter fmt  x = fprintf fmt "%a %s" ctype x.var_type x.var_name

let parameters = print_list comma parameter

let logic_parameter fmt (x, ty) = fprintf fmt "%a %s" ctype ty x.var_unique_name

let logic_parameters = print_list comma logic_parameter

let location = term
(****
let location fmt = function
  | Lterm t -> nterm fmt t
  | Lstar t -> fprintf fmt "%a[*]" nterm t
  | Lrange (t1, t2, t3) -> fprintf fmt "%a[%a..%a]" nterm t1 nterm t2 nterm t3
****)

let locations = print_list comma location

let logic_symbol fmt li = function
  | Predicate_reads (pl, locs) ->
      fprintf fmt "/*@@ @[predicate %s(%a) reads %a@] */@\n" li.logic_name
	logic_parameters pl locations locs
  | Predicate_def (pl, p) ->
      fprintf fmt "/*@@ @[predicate %s(%a) { %a }@] */@\n" li.logic_name
	logic_parameters pl predicate p
  | Function (pl, ty, locs) ->
      fprintf fmt "/*@@ @[logic %a %s(%a) reads %a@] */@\n" ctype ty 
	li.logic_name logic_parameters pl locations locs
	
let spec fmt = function
  | { requires = None; assigns = None; ensures = None; decreases = None } ->
      ()
  | s ->
      let requires fmt p = fprintf fmt "@[requires %a@]@\n" predicate p in
      let assigns fmt a = fprintf fmt "@[assigns %a@]@\n" locations a in
      let ensures fmt p = fprintf fmt "@[ensures %a@]@\n" predicate p in
      let decreases fmt = function
	| (t, None) -> fprintf fmt "@[decreases %a@]@\n" term t
	| (t, Some r) -> fprintf fmt "@[decreases %a for %s@]@\n" term t r
      in
      fprintf fmt "/*@@ @[%a%a%a%a@] */@\n"
	(print_option requires) s.requires
	(print_option assigns) s.assigns
	(print_option ensures) s.ensures
	(print_option decreases) s.decreases

let loop_annot fmt = function
  | { invariant = None; loop_assigns = None; variant = None } ->
      ()
  | a ->
      let invariant fmt p = fprintf fmt "@[invariant %a@]@\n" predicate p in
      let loop_assigns fmt a = fprintf fmt "@[assigns %a@]@\n" locations a in
      let variant fmt = function
	| (t, None) -> fprintf fmt "@[variant %a@]@\n" term t
	| (t, Some r) -> fprintf fmt "@[variant %a for %s@]@\n" term t r
      in
      fprintf fmt "/*@@ @[%a%a%a@] */@\n"
	(print_option invariant) a.invariant
	(print_option loop_assigns) a.loop_assigns
	(print_option variant) a.variant

let binop fmt = function
  | Badd | Badd_int | Badd_float | Badd_pointer_int -> fprintf fmt "+" 
  | Bsub | Bsub_int | Bsub_float | Bsub_pointer -> fprintf fmt "-"
  | Bmul | Bmul_int | Bmul_float -> fprintf fmt "*"
  | Bdiv | Bdiv_int | Bdiv_float -> fprintf fmt "/"
  | Bmod | Bmod_int -> fprintf fmt "%%" 
  | Blt | Blt_int | Blt_float | Blt_pointer -> fprintf fmt "<"
  | Bgt | Bgt_int | Bgt_float | Bgt_pointer -> fprintf fmt ">"
  | Ble | Ble_int | Ble_float | Ble_pointer -> fprintf fmt "<="
  | Bge | Bge_int | Bge_float | Bge_pointer -> fprintf fmt ">="
  | Beq | Beq_int | Beq_float | Beq_pointer -> fprintf fmt "=="
  | Bneq | Bneq_int | Bneq_float | Bneq_pointer -> fprintf fmt "!=" 
  | Bbw_and -> fprintf fmt "&"
  | Bbw_xor -> fprintf fmt "^"
  | Bbw_or -> fprintf fmt "|"
  | Band -> fprintf fmt "&&"
  | Bor -> fprintf fmt "||"
  | Bshift_left -> fprintf fmt "<<"
  | Bshift_right -> fprintf fmt ">>"


let unop fmt = function
  | Uplus -> fprintf fmt "+"
  | Uminus -> fprintf fmt "-"
  | Unot -> fprintf fmt "!"
  | Ustar -> fprintf fmt "*"
  | Uamp -> fprintf fmt "&"
  | Utilde -> fprintf fmt "~"
  (* these are introduced during typing *)
  | Ufloat_of_int -> fprintf fmt "float_of_int"
  | Uint_of_float -> fprintf fmt "int_of_float"

let rec expr fmt e = match e.texpr_node with
  | TEnop ->
      ()
  | TEconstant (IntConstant s | FloatConstant s) ->
      fprintf fmt "%s" s
  | TEstring_literal s ->
      fprintf fmt "\"%S\"" s
  | TEvar (Var_info x) ->
      fprintf fmt "%s" x.var_name
  | TEvar (Fun_info x) ->
      fprintf fmt "%s" x.fun_name
  | TEarrow (e, x) ->
      fprintf fmt "%a->%s" expr_p e x.var_name
  | TEarrget (e1, e2) ->
      fprintf fmt "%a[%a]" expr_p e1 expr_p e2
  | TEdot (e, vi) ->
      fprintf fmt "%a.%s" expr_p e vi.var_name
  | TEsizeof (ty, s) ->
      fprintf fmt "sizeof(%a)" ctype ty
(*  | TEstar e ->
      fprintf fmt "*%a" expr_p e*)
  | TEseq (e1, e2) ->
      fprintf fmt "%a, %a" expr e1 expr e2
  | TEassign (e1, e2) ->
      fprintf fmt "%a = %a" expr e1 expr e2
  | TEassign_op (e1, op, e2) ->
      fprintf fmt "%a %a= %a" expr e1 binop op expr e2
  | TEunary (op, e) ->
      fprintf fmt "%a%a" unop op expr_p e
  | TEincr (Uprefix_inc, e) -> fprintf fmt "++%a" expr_p e
  | TEincr (Uprefix_dec, e) -> fprintf fmt "--%a" expr_p e
  | TEincr (Upostfix_inc, e) -> fprintf fmt "%a++" expr_p e
  | TEincr (Upostfix_dec, e) -> fprintf fmt "%a--" expr_p e
  | TEbinary (e1, op, e2) ->
      fprintf fmt "%a %a %a" expr_p e1 binop op expr_p e2
  | TEcall (e, l) ->
      fprintf fmt "%a(%a)" expr e (print_list comma expr) l
  | TEcond (e1, e2, e3) ->
      fprintf fmt "%a ? %a : %a" expr e1 expr e2 expr e3
  | TEcast (ty, e) ->
      fprintf fmt "(%a)%a" ctype ty expr_p e

and expr_p fmt e = match e.texpr_node with
  | TEnop | TEconstant _ | TEstring_literal _ | TEvar _ -> expr fmt e
  | _ -> fprintf fmt "(@[%a@])" expr e

let rec c_initializer fmt = function
  | Iexpr e -> expr fmt e
  | Ilist l -> fprintf fmt "@[{ %a }@]" (print_list comma c_initializer) l

let rec c_initializer_term fmt = function
  | Iexpr e -> term fmt e
  | Ilist l -> fprintf fmt "@[{ %a }@]" (print_list comma c_initializer_term) l

let rec statement fmt s = match s.st_node with
  | TSnop -> 
      fprintf fmt ";"
  | TSexpr e ->
      fprintf fmt "@[%a;@]" expr e
  | TSif (e, s1, s2) ->
      fprintf fmt "@[if (%a) {@\n    @[%a@]@\n} else {@\n    @[%a@]@\n}@]"
	expr e statement_nb s1 statement_nb s2
  | TSwhile (a, e, s) ->
      fprintf fmt "@[%awhile (%a) {@\n    @[%a@]@\n}@]" 
	loop_annot a expr e statement_nb s
  | TSdowhile (a, s, e) ->
      fprintf fmt "@[%ado {@\n    @[%a@]@\n} while (%a);@]" 
	loop_annot a statement_nb s expr e 
  | TSfor (a, e1, e2, e3, s) ->
      fprintf fmt "@[%afor (%a; %a; %a) {@\n    @[%a@]@\n}@]"
	loop_annot a expr e1 expr e2 expr e2 statement_nb s
  | TSreturn None ->
      fprintf fmt "return;"
  | TSreturn (Some e) ->
      fprintf fmt "@[return %a;@]" expr e
  | TSbreak ->
      fprintf fmt "break;"
  | TScontinue ->
      fprintf fmt "continue;"
  | TSlabel (l, s) ->
      fprintf fmt "%s: %a" l statement s
  | TSswitch (e, m) ->
      (*** nexpr * (nexpr Cconst.IntMap.t) * 
      ((nexpr Cconst.IntMap.t * nstatement list) list) ***)
      fprintf fmt "<switch>"
  | TSassert p ->
      fprintf fmt "/*@@ assert %a */" predicate p
  | TSlogic_label l ->
      fprintf fmt "/*@@ label %s */" l
  | TSspec (sp, s) ->
      fprintf fmt "%a%a" spec sp statement s
  | TSblock b ->
      fprintf fmt "@[{@\n  @[%a@]@\n}@]" block b
  | TSset (vi, t) -> 
      fprintf fmt "/*@@ Set %s = %a */" vi.var_name term t
  | TSgoto s -> 
      fprintf fmt "goto %s" s
  | TSdefault ts ->
      fprintf fmt "default %a" statement ts 
  | TScase (e, ts) ->
      fprintf fmt "case %a:@\n %a" expr e statement ts 
(*  | TSdecl (ty, vi, None,rem) ->
      fprintf fmt "%a %s;@\n" ctype ty vi.var_unique_name;
      statement fmt rem
  | TSdecl (ty, vi, Some i, rem) ->
      fprintf fmt "@[<hov 2>{@\n%a %s = %a;@\n" ctype ty vi.var_unique_name c_initializer i;
      statement fmt rem;
      fprintf fmt "}@\n@]"
*)
and statement_nb fmt s = match s.st_node with
  | TSblock b -> block fmt b
  | _ -> statement fmt s

and block fmt (el,sl) =
 fprintf fmt "@[";
  List.iter (fun d -> decl fmt d; fprintf fmt "@\n") el;
  List.iter (fun d -> statement fmt d; fprintf fmt "@\n") sl;
  fprintf fmt "@]@."

and decl fmt d = match d.node with
  | Tlogic (li, ls) -> 
      logic_symbol fmt li ls
  | Taxiom (x, p) -> 
      fprintf fmt "/*@@ @[axiom %s:@ %a@] */@\n" x predicate p
  | Tinvariant (x, p) -> 
      fprintf fmt "/*@@ @[<hov 2>invariant %s:@ %a@] */@\n" x predicate p 
  | Ttypedef (ty, x) ->
      fprintf fmt "typedef %a %s;@\n" ctype ty x
  | Ttypedecl ty ->
      fprintf fmt "%a;@\n" ctype ty
  | Tdecl (ty, vi, None) ->
      fprintf fmt "%a %s;@\n" ctype ty vi.var_unique_name
  | Tdecl (ty, vi, Some i) ->
      fprintf fmt "%a %s = %a;@\n" ctype ty vi.var_unique_name c_initializer i
  | Tfunspec (s, ty, fi) ->
      fprintf fmt "%a%a %s(@[%a@]);@\n" spec s ctype ty fi.fun_name
	parameters fi.args
  | Tfundef (s, ty, fi, st) ->
      fprintf fmt "%a%a %s(@[%a@])@\n%a@\n" spec s ctype ty fi.fun_name
	 parameters fi.args statement st
  | Tghost (vi, None) ->
      fprintf fmt "/*@@  @[ghost %s@]*/@\n" vi.var_name	
  | Tghost (vi, Some i) ->
      fprintf fmt "/*@@  @[ghost %s = @ %a@]*/@\n" vi.var_name 
	c_initializer_term i  

let file fmt p = 
  fprintf fmt "@[";
  Cenv.iter_all_struct (declare_struct fmt);
  List.iter (fun d -> decl fmt d; fprintf fmt "@\n") p;
  fprintf fmt "@]@."


