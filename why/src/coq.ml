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
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(*                                                                        *)
(**************************************************************************)

(*i $Id: coq.ml,v 1.169 2008/04/15 08:06:40 regisgia Exp $ i*)

open Options
open Logic
open Logic_decl
open Types
open Vcg
open Cc
open Ident
open Util
open Format
open Misc
open Cc
open Pp

(* common to V7 and V8 *)

let is_coq_keyword =
  let ht = Hashtbl.create 50  in
  List.iter (fun kw -> Hashtbl.add ht kw ()) 
    ["at"; "cofix"; "exists2"; "fix"; "IF"; "mod"; "Prop";
     "return"; "Set"; "Type"; "using"; "where"];
  Hashtbl.mem ht

let rename s = if is_coq_keyword s then "why__" ^ s else s
let idents fmt s = fprintf fmt "%s" (rename s)
let ident fmt id = fprintf fmt "%s" (rename (Ident.string id))

let rec print_pure_type fmt = function
  | PTint -> fprintf fmt "Z"
  | PTbool -> fprintf fmt "bool"
  | PTunit -> fprintf fmt "unit"
  | PTreal -> fprintf fmt "R"
  | PTexternal ([v], id) when id == farray -> 
      fprintf fmt "(array %a)" print_pure_type v
  | PTexternal([],id) -> ident fmt id
  | PTexternal(l,id) -> 
      fprintf fmt "(%a %a)"
      ident id (print_list space print_pure_type) l
  | PTvar { type_val = Some t} -> 
      fprintf fmt "%a" print_pure_type t      
  | PTvar v ->
      fprintf fmt "A%d" v.tag

let instance = print_list space print_pure_type

let prefix_id id =
  (* int cmp *)
  if id == t_lt_int then "Z_lt_ge_bool" 
  else if id == t_le_int then "Z_le_gt_bool"
  else if id == t_gt_int then "Z_gt_le_bool"
  else if id == t_ge_int then "Z_ge_lt_bool"
  else if id == t_eq_int then "Z_eq_bool"
  else if id == t_neq_int then "Z_noteq_bool"
  (* real cmp *)
  else if id == t_lt_real then "R_lt_ge_bool" 
  else if id == t_le_real then "R_le_gt_bool"
  else if id == t_gt_real then "R_gt_le_bool"
  else if id == t_ge_real then "R_ge_lt_bool"
  else if id == t_eq_real then "R_eq_bool"
  else if id == t_neq_real then "R_noteq_bool"
  (* bool cmp *)
  else if id == t_eq_bool then "B_eq_bool"
  else if id == t_neq_bool then "B_noteq_bool"
  (* unit cmp *)
  else if id == t_eq_unit then "U_eq_bool"
  else if id == t_neq_unit then "U_noteq_bool"
  (* int ops *)
  else if id == t_add_int then "Zplus"
  else if id == t_sub_int then "Zminus"
  else if id == t_mul_int then "Zmult"
  else if id == t_div_int then "Zdiv"
  else if id == t_mod_int then "Zmod"
  else if id == t_neg_int then "Zopp"
  (* real ops *)
  else if id == t_add_real then "Rplus"
  else if id == t_sub_real then "Rminus"
  else if id == t_mul_real then "Rmult"
  else if id == t_div_real then "Rdiv"
  else if id == t_neg_real then "Ropp"
  else if id == t_sqrt_real then "sqrt"
  else if id == t_real_of_int then "IZR"
  else if id == t_int_of_real then "int_of_real"
  else assert false

let infix_relation id =
       if id == t_lt_int then "<" 
  else if id == t_le_int then "<="
  else if id == t_gt_int then ">"
  else if id == t_ge_int then ">="
  else if id == t_eq_int then "="
  else if id == t_neq_int then "<>"
  else assert false

let pprefix_id id =
  if id == t_lt_real then "Rlt"
  else if id == t_le_real then "Rle"
  else if id == t_gt_real then "Rgt" 
  else if id == t_ge_real then "Rge"
  else assert false

let rec collect_app l = function
  | CC_app (e1, e2) -> collect_app (e2 :: l) e1
  | p -> p :: l

let print_binder_id fmt (id,_) = ident fmt id

let collect_lambdas x = 
  let rec collect bl = function
    | CC_lam (b,c) -> collect (b :: bl) c
    | c -> List.rev bl, c
  in
  collect [] x

(* printers for Coq V7 *)

let inz = ref 0
let openz fmt = if !inz == 0 then fprintf fmt "`@["; incr inz 
let closez fmt = decr inz; if !inz == 0 then fprintf fmt "@]`"

let print_term_v7 fmt t = 
  let rec print0 fmt = function
    | Tapp (id, [a;b], _) when is_relation id ->
	fprintf fmt "(@[<hov 2>%s@ %a@ %a@])" (prefix_id id)
	print1 a print1 b
    | t -> 
	print1 fmt t
  and print1 fmt = function
    | Tapp (id, [a;b], _) when id == t_add_int ->
	openz fmt; fprintf fmt "%a +@ %a" print1 a print2 b; closez fmt
    | Tapp (id, [a;b], _) when id == t_sub_int ->
	openz fmt; fprintf fmt "%a -@ %a" print1 a print2 b; closez fmt
    | t ->
	print2 fmt t
  and print2 fmt = function
    | Tapp (id, [a;b], _) when id == t_mul_int ->
	openz fmt; fprintf fmt "%a *@ %a" print2 a print3 b; closez fmt
    | Tapp (id, [a;b], _) when id == t_div_int ->
	fprintf fmt "(@[Zdiv %a@ %a@])" print3 a print3 b
    | Tapp (id, [a;b], _) when id == t_mod_int ->
	fprintf fmt "(@[Zmod %a@ %a@])" print3 a print3 b
    | t ->
	print3 fmt t
  and print3 fmt = function
    | Tconst (ConstInt n) -> 
	openz fmt; fprintf fmt "%s" n; closez fmt
    | Tconst (ConstBool b) -> 
	fprintf fmt "%b" b
    | Tconst ConstUnit -> 
	fprintf fmt "tt" 
    | Tconst (ConstFloat (i,f,e)) -> 
	assert (!inz == 0); (* TODO: reals inside integer expressions *)
	failwith "real constants not supported with Coq V7"
    | Tvar id when id == implicit ->
	fprintf fmt "?"
    | Tvar id when id == t_zwf_zero ->
	fprintf fmt "(Zwf ZERO)"
    | Tvar id | Tapp (id, [], []) -> 
	ident fmt id
    | Tapp (id, [], i) ->
	fprintf fmt "(@[@@%a %a@])" ident id instance i
    | Tderef _ ->
	assert false
    | Tapp (id, [t], _) when id == t_neg_int ->
	openz fmt; fprintf fmt "(-%a)" print3 t; closez fmt
    | Tapp (id, [_;_], _) as t when is_relation id || is_int_arith_binop id ->
	fprintf fmt "@[(%a)@]" print0 t
    | Tapp (id, [a; b; c], _) when id == if_then_else -> 
	fprintf fmt "(@[if_then_else %a@ %a@ %a@])" print0 a print0 b print0 c
    | Tapp (id, tl, _) when id == t_zwf_zero -> 
	fprintf fmt "(@[Zwf 0 %a@])" print_terms tl
    | Tapp (id, tl, _) when is_relation id || is_arith id -> 
	fprintf fmt "(@[%s@ %a@])" (prefix_id id) print_terms tl
    | Tapp (id, tl, _) -> 
	fprintf fmt "(@[%a@ %a@])" ident id print_terms tl
    | Tnamed (User n, t) ->
	fprintf fmt "@[((* %s *)@ %a)@]" n print3 t
    | Tnamed (_, t) -> print3 fmt t
  and print_terms fmt tl =
    print_list space print0 fmt tl
  in
  print0 fmt t

let print_predicate_v7 fmt p =
  let rec print0 fmt = function
    | Pif (a, b, c) -> 
	fprintf fmt "(@[if %a@ then %a@ else %a@])"
	  print_term_v7 a print0 b print0 c
    | Pimplies (_, a, b) -> 
	fprintf fmt "(@[%a ->@ %a@])" print1 a print0 b
    | Piff (a, b) -> 
	fprintf fmt "(@[%a <->@ %a@])" print1 a print0 b
    | p -> print1 fmt p
  and print1 fmt = function
    | Por (a, b) -> fprintf fmt "%a \\/@ %a" print2 a print1 b
    | p -> print2 fmt p
  and print2 fmt = function
    | Pand (_, _, a, b) | Forallb (_, a, b) -> 
        fprintf fmt "%a /\\@ %a" print3 a print2 b
    | p -> print3 fmt p
  and print3 fmt = function
    | Ptrue -> 
	fprintf fmt "True"
    | Pvar id when id == Ident.default_post ->
	fprintf fmt "True"
    | Pfalse -> 
	fprintf fmt "False"
    | Pvar id -> 
	ident fmt id
    | Papp (id, tl, _) when id == t_distinct ->
	fprintf fmt "@[(%a)@]" print0 (Util.distinct tl)
    | Papp (id, [t], _) when id == well_founded ->
	fprintf fmt "@[(well_founded %a)@]" print_term_v7 t
    | Papp (id, [a;b], _) when id == t_zwf_zero ->
	fprintf fmt "(Zwf `0` %a %a)" print_term_v7 a print_term_v7 b
    | Papp (id, [a;b], _) when is_int_comparison id ->
	openz fmt; 
	fprintf fmt "%a %s@ %a" 
	  print_term_v7 a (infix_relation id) print_term_v7 b; 
	closez fmt
    | Papp (id, [a;b], _) when id == t_eq_real ->
	fprintf fmt "(@[eqT R %a %a@])" print_term_v7 a print_term_v7 b
    | Papp (id, [a;b], _) when id == t_neq_real ->
	fprintf fmt "~(@[eqT R %a %a@])" print_term_v7 a print_term_v7 b
    | Papp (id, [a;b], _) when is_eq id ->
	fprintf fmt "@[%a =@ %a@]" print_term_v7 a print_term_v7 b
    | Papp (id, [a;b], _) when is_neq id -> 
	fprintf fmt "@[~(%a =@ %a)@]" print_term_v7 a print_term_v7 b
    | Papp (id, [a;b], _) when is_real_comparison id ->
	fprintf fmt "(@[%s %a %a@])" 
	(pprefix_id id) print_term_v7 a print_term_v7 b
    | Papp (id, l, _) ->
	fprintf fmt "(@[%a %a@])" ident id
	  (print_list space print_term_v7) l
    | Pnot p -> 
	fprintf fmt "~%a" print3 p
    | Forall (_,id,n,t,_,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[(%a:%a)@ %a@])" ident id'
	  print_pure_type t print0 p'
    | Exists (id,n,t,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[EX %a:%a |@ %a@])" ident id'
	  print_pure_type t print0 p'
    | Pfpi _ ->
	failwith "fpi not supported with Coq V7"
    | Pnamed (User n, p) ->
	fprintf fmt "@[((* %s *)@ %a)@]" n print3 p
    | Pnamed (_, p) -> print3 fmt p
    | (Por _ | Piff _ | Pand _ | Pif _ | Pimplies _ | Forallb _) as p -> 
	fprintf fmt "(%a)" print0 p
  in
  print0 fmt p

let rec print_cc_type_v7 fmt = function
  | TTpure pt -> 
      print_pure_type fmt pt
  | TTarray v -> 
      fprintf fmt "(@[array@ %a@])" print_cc_type_v7 v
  | TTlambda (b, t) ->
      fprintf fmt "[%a]@,%a" print_binder_v7 b print_cc_type_v7 t
(*i***
  | TTarrow ((id, CC_var_binder t1), t2) when not (occur_cc_type id t2) -> 
      fprintf fmt "%a -> %a" print_cc_type t1 print_cc_type t2
***i*)
  | TTarrow (b, t) -> 
      fprintf fmt "(%a)@,%a" print_binder_v7 b print_cc_type_v7 t
  | TTtuple ([_,CC_var_binder t], None) -> 
      print_cc_type_v7 fmt t
  | TTtuple (bl, None) ->
      fprintf fmt "(@[tuple_%d@ %a@])" (List.length bl) 
	(print_list space print_binder_type_v7) bl
  | TTtuple (bl, Some q) -> 
      fprintf fmt "(@[sig_%d@ %a@ %a(%a)@])" (List.length bl)
	(print_list space print_binder_type_v7) bl 
	(print_list nothing 
	   (fun fmt b -> fprintf fmt "[%a]@," print_binder_v7 b)) bl
	print_cc_type_v7 q
  | TTpred p ->
      print_predicate_v7 fmt p
  | TTapp (tt, l) ->
      fprintf fmt "(@[%a@ %a@])" print_cc_type_v7 tt
	(print_list space print_cc_type_v7) l
  | TTterm t ->
      print_term_v7 fmt t
  | TTSet ->
      fprintf fmt "Set"

and print_binder_v7 fmt (id,b) = 
  ident fmt id;
  match b with
    | CC_pred_binder p -> fprintf fmt ": %a" print_predicate_v7 p
    | CC_var_binder t -> fprintf fmt ": %a" print_cc_type_v7 t
    | CC_untyped_binder -> ()

and print_binder_type_v7 fmt = function
  | _, CC_var_binder t -> print_cc_type_v7 fmt t
  | _ -> assert false


let print_sequent_v7 fmt (hyps,concl) =
  let rec print_seq fmt = function
    | [] ->
	print_predicate_v7 fmt concl
    | Svar (id, v) :: hyps -> 
	fprintf fmt "(%a: @[%a@])@\n" ident id print_pure_type v;
	print_seq fmt hyps
    | Spred (id, p) :: hyps -> 
	fprintf fmt "(%a: @[%a@])@\n" ident id print_predicate_v7 p;
	print_seq fmt hyps
  in
  fprintf fmt "@[%a@]@?" print_seq hyps

let print_lambdas_v7 = print_list semi print_binder_v7

let rec print_gen_cc_term_v7 print_hole fmt t = 
  let print_cc_term_v7 = print_gen_cc_term_v7 print_hole in
    match t with
      | CC_var id ->  
 	  ident fmt id 
      | CC_lam _ as t ->
	  let bl,c = collect_lambdas t in
	    fprintf fmt "@[<hov 2>[@[%a@]]@,%a@]"
	      print_lambdas_v7 bl print_cc_term_v7 c
      | CC_app (f,a) ->
	  let tl = collect_app [a] f in
	    fprintf fmt "@[<hov 2>(%a)@]" (print_list space print_cc_term_v7) tl
      | CC_tuple (cl, None) -> 
	  fprintf fmt "(Build_tuple_%d %a)" (List.length cl)
	    (print_list space print_cc_term_v7) cl
      | CC_tuple (cl, Some q) ->
	  fprintf fmt "(exist_%d %a %a)" (List.length cl - 1)
	    print_cc_type_v7 q (print_list space print_cc_term_v7) cl
	    (* special treatment for the if-then-else *)
      | CC_letin (_, bl, e1,
		  CC_if (CC_var idb,
			 CC_lam ((idt, CC_pred_binder _), brt),
			 CC_lam ((idf, CC_pred_binder _), brf)))
	  when annotated_if idb bl ->
	  let qb, q = annotation_if bl in
	    fprintf fmt "@[@[<hov 2>let (%a) =@ %a in@]@\n@[<hov 2>Cases@ (@[btest@ @[[%a:bool]@,%a@] %a@ %a@]) of@]@\n| @[<hov 2>(left %a) =>@ %a@]@\n| @[<hov 2>(right %a) =>@ %a@] end@]"
	      (print_list comma print_binder_id) bl print_cc_term_v7 e1
	      ident idb print_predicate_v7 q ident idb ident qb
	      ident idt print_cc_term_v7 brt
	      ident idf print_cc_term_v7 brf
	      (* non-dependent boolean if-then-else (probably not of use) *)
      | CC_if (b,e1,e2) ->
	  fprintf fmt "@[if "; print_cc_term_v7 fmt b; fprintf fmt " then@\n  ";
	  hov 0 fmt (print_cc_term_v7 fmt) e1;
	  fprintf fmt "@\nelse@\n  ";
	  hov 0 fmt (print_cc_term_v7 fmt) e2;
	  fprintf fmt "@]"
      | CC_case (e, pl) ->
	  let print_case_v7 fmt (p,e) =
	    fprintf fmt "@[<hov 2>| %a =>@ %a@]"
	      print_cc_pattern p print_cc_term_v7 e
	  in
	    fprintf fmt "@[Cases %a of@\n%a@\nend@]" print_cc_term_v7 e
	      (print_list newline print_case_v7) pl
      | CC_letin (_,[id,_],c,c1) ->
	  fprintf fmt "@[@[<hov 2>let %a =@ %a in@]@\n%a@]"
	    ident id print_cc_term_v7 c print_cc_term_v7 c1
      | CC_letin (_,bl,c,c1) ->
	  fprintf fmt "@[@[<hov 2>let (%a) =@ %a in@]@\n%a@]"
	    (print_list comma print_binder_id) bl
	    print_cc_term_v7 c print_cc_term_v7 c1
      | CC_term c ->
	  fprintf fmt "@[%a@]" print_term_v7 c
       | CC_hole pr -> 
 	  print_hole fmt pr 
      | CC_type t ->
	  print_cc_type_v7 fmt t
      | CC_any _ ->
	  Report.raise_unlocated
	    (Error.AnyMessage "can't produce a validation for an incomplete program")
	    
let rec print_proof_v7 fmt = function
  | Lemma (s, []) ->
      fprintf fmt "%s" s
  | Lemma (s, vl) ->
      fprintf fmt "@[(%s %a)@]" s (print_list space ident) vl
  | True ->
      fprintf fmt "I"
  | Reflexivity t ->
      fprintf fmt "@[(refl_equal ? %a)@]" print_term_v7 t
  | Assumption id -> 
      ident fmt id
  | Proj1 id ->
      fprintf fmt "@[(proj1 ? ? %a)@]" ident id
  | Proj2 id ->
      fprintf fmt "@[(proj2 ? ? %a)@]" ident id
  | Conjunction (id1, id2) ->
      fprintf fmt "@[(conj ? ? %a %a)@]" ident id1 ident id2
  | WfZwf t ->
      fprintf fmt "(Zwf_well_founded %a)" print_term_v7 t
  | Loop_variant_1 (h, h') ->
      fprintf fmt "(loop_variant_1 %a %a)" ident h ident h'
  | Absurd h ->
      fprintf fmt "(False_ind ? %a)" ident h
  | ProofTerm t ->
      fprintf fmt "@[%a@]" print_cc_term_v7 t
  | ShouldBeAWp ->
      Report.raise_unlocated 
	(Error.AnyMessage "can't produce a validation for an incomplete program")

and print_cc_term_v7 x = print_gen_cc_term_v7 print_proof_v7 x

let print_cc_functional_program_v7 = 
  print_gen_cc_term_v7 (fun fmt (loc, p) -> print_predicate_v7 fmt p)

(* printers for Coq V8 *)

let print_term_v8 fmt t = 
  let rec print0 fmt = function
    | Tapp (id, [a;b], _) when is_relation id ->
	fprintf fmt "(@[<hov 2>%s@ %a@ %a@])" (prefix_id id)
	print3 a print3 b
    | t -> 
	print1 fmt t
  and print1 fmt = function
    | Tapp (id, [a;b], _) when id == t_add_int ->
	fprintf fmt "%a +@ %a" print1 a print2 b
    | Tapp (id, [a;b], _) when id == t_sub_int ->
	fprintf fmt "%a -@ %a" print1 a print2 b
    | t ->
	print2 fmt t
  and print2 fmt = function
    | Tapp (id, [a;b], _) when id == t_mul_int ->
	fprintf fmt "%a *@ %a" print2 a print3 b
    | Tapp (id, [a;b], _) when id == t_div_int ->
	fprintf fmt "(@[Zdiv %a@ %a@])" print3 a print3 b
    | Tapp (id, [a;b], _) when id == t_mod_int ->
	fprintf fmt "(@[Zmod %a@ %a@])" print3 a print3 b
    | t ->
	print3 fmt t
  and print3 fmt = function
    | Tconst (ConstInt n) -> 
	fprintf fmt "%s" n
    | Tconst (ConstBool b) -> 
	fprintf fmt "%b" b
    | Tconst ConstUnit -> 
	fprintf fmt "tt" 
    | Tconst (ConstFloat (i,f,e)) -> 
	let f = if f = "0" then "" else f in
	let e = (if e = "" then 0 else int_of_string e) - String.length f in
	if e = 0 then
	  fprintf fmt "(%s%s)%%R" i f
	else if e > 0 then
	  fprintf fmt "(%s%s * 1%s)%%R" i f (String.make e '0')
	else
	  fprintf fmt "(%s%s / 1%s)%%R" i f (String.make (-e) '0')
    | Tvar id when id == implicit ->
	fprintf fmt "?"
    | Tvar id when id == t_zwf_zero ->
	fprintf fmt "(Zwf Z0)"
    | Tvar id | Tapp (id, [], []) -> 
	ident fmt id
    | Tapp (id, [], i) ->
	fprintf fmt "(@[@@%a %a@])" ident id instance i
    | Tderef _ ->
	assert false
    | Tapp (id, [a; Tapp (id', [b], _)], _) 
      when id == t_pow_real && id' == t_real_of_int ->
	fprintf fmt "(@[powerRZ@ %a@ %a@])" print3 a print3 b
    | Tapp (id, [a;b], _) when id == t_pow_real ->
	fprintf fmt "(@[Rpower@ %a@ %a@])" print3 a print3 b
    | Tapp (id, [a], _) when id == t_abs_real ->
	fprintf fmt "(@[Rabs@ %a@])" print3 a
    | Tapp (id, [t], _) when id == t_neg_int ->
	fprintf fmt "(Zopp@ %a)" print3 t
    | Tapp (id, [_;_], _) as t when is_relation id || is_int_arith_binop id ->
	fprintf fmt "@[(%a)@]" print0 t
    | Tapp (id, [a; b; c], _) when id == if_then_else -> 
	fprintf fmt "(@[if_then_else %a@ %a@ %a@])" print0 a print0 b print0 c
    | Tapp (id, tl, _) when id == t_zwf_zero -> 
	fprintf fmt "(@[Zwf 0 %a@])" print_terms tl
    | Tapp (id, tl, _) when is_relation id || is_arith id -> 
	fprintf fmt "(@[%s@ %a@])" (prefix_id id) print_terms tl
    | Tapp (id, tl, _) -> 
	fprintf fmt "(@[%a@ %a@])" ident id print_terms tl
    | Tnamed (User n, t) ->
	fprintf fmt "@[((* %s *)@ %a)@]" n print3 t
    | Tnamed (_, t) -> print3 fmt t
  and print_terms fmt tl =
    print_list space print3 fmt tl
  in
  print3 fmt t

let print_predicate_v8 fmt p =
  let rec print0 fmt = function
    | Pif (a, b, c) -> 
	fprintf fmt "(@[if %a@ then %a@ else %a@])"
	  print_term_v8 a print0 b print0 c
    | Pimplies (_, a, b) -> 
	fprintf fmt "(@[%a ->@ %a@])" print1 a print0 b
    | Piff (a, b) -> 
	fprintf fmt "(@[%a <->@ %a@])" print1 a print0 b
    | p -> print1 fmt p
  and print1 fmt = function
    | Por (a, b) -> fprintf fmt "%a \\/@ %a" print2 a print1 b
    | p -> print2 fmt p
  and print2 fmt = function
    | Pand (_, _, a, b) | Forallb (_, a, b) -> 
        fprintf fmt "%a /\\@ %a" print3 a print2 b
    | p -> print3 fmt p
  and print3 fmt = function
    | Ptrue -> 
	fprintf fmt "True"
    | Pvar id when id == Ident.default_post ->
	fprintf fmt "True"
    | Pfalse -> 
	fprintf fmt "False"
    | Pvar id -> 
	ident fmt id
    | Papp (id, tl, _) when id == t_distinct ->
	fprintf fmt "@[(%a)@]" print0 (Util.distinct tl)
    | Papp (id, [t], _) when id == well_founded ->
	fprintf fmt "@[(well_founded %a)@]" print_term_v8 t
    | Papp (id, [a;b], _) when id == t_zwf_zero ->
	fprintf fmt "(Zwf 0 %a %a)" print_term_v8 a print_term_v8 b
    | Papp (id, [a;b], _) when is_int_comparison id ->
	fprintf fmt "%a %s@ %a" 
	  print_term_v8 a (infix_relation id) print_term_v8 b
    | Papp (id, [a;b], _) when id == t_eq_real ->
	fprintf fmt "(@[eq %a %a@])" print_term_v8 a print_term_v8 b
    | Papp (id, [a;b], _) when id == t_neq_real ->
	fprintf fmt "~(@[eq %a %a@])" print_term_v8 a print_term_v8 b
    | Papp (id, [a;b], _) when is_eq id ->
	fprintf fmt "@[%a =@ %a@]" print_term_v8 a print_term_v8 b
    | Papp (id, [a;b], _) when is_neq id -> 
	fprintf fmt "@[~(%a =@ %a)@]" print_term_v8 a print_term_v8 b
    | Papp (id, [a;b], _) when is_real_comparison id ->
	fprintf fmt "(@[%s@ %a@ %a@])" 
	(pprefix_id id) print_term_v8 a print_term_v8 b
    | Papp (id, l, _) ->
	fprintf fmt "(@[%a@ %a@])" ident id
	  (print_list space print_term_v8) l
    | Pnot p -> 
	fprintf fmt "~%a" print3 p
    | Forall (_,id,n,t,_,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[forall (%a:%a),@ %a@])" ident id'
	  print_pure_type t print0 p'
    | Exists (id,n,t,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[exists %a:%a,@ %a@])" ident id'
	  print_pure_type t print0 p'
    | Pfpi _ ->
	failwith "fpi not supported with Coq V8"
    | Pnamed (User n, p) ->
	fprintf fmt "@[(* %s *)@ %a@]" n print3 p
    | Pnamed (_, p) -> print3 fmt p
    | (Por _ | Piff _ | Pand _ | Pif _ | Pimplies _ | Forallb _) as p -> 
	fprintf fmt "(%a)" print0 p
  in
  print0 fmt p

let rec print_cc_type_v8 fmt = function
  | TTpure pt -> 
      print_pure_type fmt pt
  | TTarray v -> 
      fprintf fmt "(@[array@ %a@])" print_cc_type_v8 v
  | TTlambda (b, t) ->
      fprintf fmt "(@[fun %a =>@ %a@])" print_binder_v8 b print_cc_type_v8 t
(*i***
  | TTarrow ((id, CC_var_binder t1), t2) when not (occur_cc_type id t2) -> 
      fprintf fmt "%a -> %a" print_cc_type t1 print_cc_type t2
***i*)
  | TTarrow (b, t) -> 
      fprintf fmt "forall %a,@ %a" print_binder_v8 b print_cc_type_v8 t
  | TTtuple ([_,CC_var_binder t], None) -> 
      print_cc_type_v8 fmt t
  | TTtuple (bl, None) ->
      fprintf fmt "(@[tuple_%d@ %a@])" (List.length bl) 
	(print_list space print_binder_type_v8) bl
  | TTtuple (bl, Some q) -> 
      fprintf fmt "(@[sig_%d@ %a@ (@[fun %a =>@ (%a)@])@])" (List.length bl)
	(print_list space print_binder_type_v8) bl 
	(print_list nothing 
	   (fun fmt b -> fprintf fmt "%a@ " print_binder_v8 b)) bl
	print_cc_type_v8 q
  | TTpred p ->
      print_predicate_v8 fmt p
  | TTapp (tt, l) ->
      fprintf fmt "(@[%a@ %a@])" print_cc_type_v8 tt
	(print_list space print_cc_type_v8) l
  | TTterm t ->
      print_term_v8 fmt t
  | TTSet ->
      fprintf fmt "Set"

and print_binder_v8 fmt (id,b) = match b with
  | CC_pred_binder p -> 
      fprintf fmt "(%a: %a)" ident id print_predicate_v8 p
  | CC_var_binder t -> 
      fprintf fmt "(%a: %a)" ident id print_cc_type_v8 t
  | CC_untyped_binder -> 
      ident fmt id

and print_binder_type_v8 fmt = function
  | _, CC_var_binder t -> print_cc_type_v8 fmt t
  | _ -> assert false


let print_sequent_v8 fmt (hyps,concl) =
  let rec print_seq fmt = function
    | [] ->
	print_predicate_v8 fmt concl
    | Svar (id, v) :: hyps -> 
	fprintf fmt "forall (%a: @[%a@]),@\n" 
	ident id print_pure_type v;
	print_seq fmt hyps
    | Spred (id, p) :: hyps -> 
	fprintf fmt "forall (%a: @[%a@]),@\n" 
	ident id print_predicate_v8 p;
	print_seq fmt hyps
  in
  fprintf fmt "@[%a@]@?" print_seq hyps

let print_lambdas_v8 = print_list space print_binder_v8

let rec print_gen_cc_term_v8 print_hole fmt t = 
  let print_cc_term_v8 = print_gen_cc_term_v8 print_hole in
    match t with
      | CC_var id -> 
	  ident fmt id
      | CC_lam _ as t ->
	  let bl,c = collect_lambdas t in
	    fprintf fmt "(@[<hov 2>fun @[%a@] =>@ %a@])" 
	      print_lambdas_v8 bl print_cc_term_v8 c
      | CC_app (f,a) ->
	  let tl = collect_app [a] f in
	    (match tl with
	       | [CC_term (Tapp (f, [t], _)); CC_lam (x, u)] when f = nt_bind ->
		   fprintf fmt "@[<hov 2>@[do %s <- @[(%a);@] @ @] @\n %a @]"
		     (Ident.string (fst x)) print_cc_term_v8 
		     (CC_term t) print_cc_term_v8 u
	       | [CC_term (Tvar f); t; CC_lam (x, u) ]
	       | [CC_var f; t; CC_lam (x, u) ] when f = nt_bind ->
		   fprintf fmt "@[<hov 2>@[do %s <- @[(%a);@] @ @] @\n %a @]"
		     (Ident.string (fst x)) print_cc_term_v8 
		     t print_cc_term_v8 u
	       | tl -> 
		   fprintf fmt "@[<hov 2>(%a)@]" 
		     (print_list_par space print_cc_term_v8) tl)
      | CC_tuple (cl, None) ->
	  fprintf fmt "(Build_tuple_%d %a)" (List.length cl)
	    (print_list_par space print_cc_term_v8) cl
      | CC_tuple (cl, Some q) ->
	  fprintf fmt "(exist_%d %a %a)" (List.length cl - 1)
	    print_cc_type_v8 q (print_list_par space print_cc_term_v8) cl
	    (* special treatment for the if-then-else *)
      | CC_letin (_, bl, e1, 
		  CC_if (CC_var idb,
			 CC_lam ((idt, CC_pred_binder _), brt),
			 CC_lam ((idf, CC_pred_binder _), brf)))
	  when annotated_if idb bl ->
	  let qb, q = annotation_if bl in
	    fprintf fmt "@[@[<hov 2>let (%a) :=@ %a in@]@\n@[<hov 2>match@ (@[btest@ (@[fun (%a:bool) =>@ %a@]) %a@ %a@]) with@]@\n| @[<hov 2>(left %a) =>@ %a@]@\n| @[<hov 2>(right %a) =>@ %a@] end@]"
	      (print_list comma print_binder_id) bl print_cc_term_v8 e1 
	      ident idb print_predicate_v8 q ident idb ident qb
	      ident idt print_cc_term_v8 brt
	      ident idf print_cc_term_v8 brf
	      (* non-dependent boolean if-then-else (probably not of use) *)
      | CC_if (b,e1,e2) ->
	  fprintf fmt "@[if "; print_cc_term_v8 fmt b; fprintf fmt " then@\n  ";
	  hov 0 fmt (print_cc_term_v8 fmt) e1;
	  fprintf fmt "@\nelse@\n  ";
	  hov 0 fmt (print_cc_term_v8 fmt) e2;
	  fprintf fmt "@]"
      | CC_case (e, pl) ->
	  let print_case_v8 fmt (p,e) =
	    fprintf fmt "@[<hov 2>| %a =>@ %a@]" 
	      print_cc_pattern p print_cc_term_v8 e
	  in
	    fprintf fmt "@[match %a with@\n%a@\nend@]" print_cc_term_v8 e
	      (print_list newline print_case_v8) pl
      | CC_letin (_,[id,_],c,c1) ->
	  fprintf fmt "@[@[<hov 2>let %a :=@ %a in@]@\n%a@]"
	    ident id print_cc_term_v8 c print_cc_term_v8 c1
      | CC_letin (_,bl,c,c1) ->
	  fprintf fmt "@[@[<hov 2>let (%a) :=@ %a in@]@\n%a@]"
	    (print_list comma print_binder_id) bl
	    print_cc_term_v8 c print_cc_term_v8 c1
      | CC_term c ->
	  fprintf fmt "@[%a@]" print_term_v8 c
      | CC_hole pr ->
	  print_hole fmt pr
      | CC_type t ->
	  print_cc_type_v8 fmt t
      | CC_any _ ->
	  Report.raise_unlocated 
	    (Error.AnyMessage "can't produce a validation for an incomplete program")

let rec print_proof_v8 fmt = function
  | Lemma (s, []) ->
      fprintf fmt "%s" s
  | Lemma (s, vl) ->
      fprintf fmt "@[(%s %a)@]" s (print_list space ident) vl
  | True ->
      fprintf fmt "I"
  | Reflexivity t ->
      fprintf fmt "@[(refl_equal %a)@]" print_term_v8 t
  | Assumption id -> 
      ident fmt id
  | Proj1 id ->
      fprintf fmt "@[(proj1 %a)@]" ident id
  | Proj2 id ->
      fprintf fmt "@[(proj2 %a)@]" ident id
  | Conjunction (id1, id2) ->
      fprintf fmt "@[(conj %a %a)@]" ident id1 ident id2
  | WfZwf t ->
      fprintf fmt "(Zwf_well_founded %a)" print_term_v8 t
  | Loop_variant_1 (h, h') ->
      fprintf fmt "(loop_variant_1 %a %a)" ident h ident h'
  | Absurd h ->
      fprintf fmt "(False_ind _ %a)" ident h
  | ProofTerm t ->
      fprintf fmt "@[%a@]" print_cc_term_v8 t
  | ShouldBeAWp ->
      if debug then Report.raise_unlocated (Error.AnyMessage "should be a WP");
      Report.raise_unlocated 
	(Error.AnyMessage "can't produce a validation for an incomplete program")

and print_cc_term_v8 x = print_gen_cc_term_v8 print_proof_v8 x

let print_cc_functional_program_v8 = 
  print_gen_cc_term_v8 (fun fmt (loc, p) -> print_predicate_v8 fmt p)

(* printers selection *)

let v8 = match prover () with Coq V8 -> true | _ -> false

let print_term = if v8 then print_term_v8 else print_term_v7
let print_predicate = if v8 then print_predicate_v8 else print_predicate_v7
let print_sequent = if v8 then print_sequent_v8 else print_sequent_v7
let print_cc_type = if v8 then print_cc_type_v8 else print_cc_type_v7
let print_cc_term = if v8 then print_cc_term_v8 else print_cc_term_v7
let print_cc_functional_program = 
  if v8 then print_cc_functional_program_v8 else print_cc_functional_program_v7

(* while printing scheme, we rename type variables (for more stability 
   of the Coq source); this is safe only because we throw away the types
   as soon as they are printed *)
let print_scheme fmt l =
  let r = ref 0 in
  Env.Vmap.iter
    (fun _ x -> 
       incr r;
       x.type_val <- Some (PTvar { tag = !r; user = false; type_val = None });
       if v8 then fprintf fmt "forall (A%d:Set),@ " !r
       else fprintf fmt "(A%d:Set)@ " !r)
    l

let print_sequent fmt s =
  let (l,s) = Env.specialize_sequent s in
  print_scheme fmt l;
  print_sequent fmt s

(*let _ = Vcg.log_print_function := print_sequent*)
      
let reprint_obligation fmt loc expl id s =
  fprintf fmt "@[(* %a *)@]@\n" Loc.report_obligation_position loc;
  fprintf fmt "@[<hov 2>(*Why goal*) Lemma %s : @\n%a.@]@\n" id print_sequent s
  (*;
  fprintf fmt "@[<hov 2>(* %a *)@]@\n" Util.print_explanation expl
  *)

let print_obligation fmt loc expl id s = 
  reprint_obligation fmt loc expl id s;
  fprintf fmt "Proof.@\n";
  option_iter (fun t -> fprintf fmt "%s.@\n" t) coq_tactic;
  fprintf fmt "(* FILL PROOF HERE *)@\nSave.@\n"

let reprint_parameter fmt id c =
  let (l,c) = Env.specialize_cc_type c in
  fprintf fmt 
    "@[<hov 2>(*Why*) Parameter %a :@ @[%a%a@].@]@\n" idents id 
    print_scheme l print_cc_type c

let print_parameter = reprint_parameter

let print_logic_type fmt s = 
  let (l,t) = Env.specialize_logic_type s in
  print_scheme fmt l;
  match t with
  | Function ([], t) ->
      print_pure_type fmt t
  | Function (pl, t) ->
      fprintf fmt "%a -> %a" 
	(print_list arrow print_pure_type) pl print_pure_type t
  | Predicate [] ->
      fprintf fmt "Prop"
  | Predicate pl ->
      fprintf fmt "%a -> Prop" (print_list arrow print_pure_type) pl

let reprint_logic fmt id t = 
  fprintf fmt 
    "@[<hov 2>(*Why logic*) Definition %a :@ @[%a@].@]@\n" 
    idents id print_logic_type t

let polymorphic t = not (Env.Vset.is_empty t.Env.scheme_vars)

let implicits_for_logic_type t = match t.Env.scheme_type with
  | Predicate [] -> false
  | Function _ | Predicate _ -> polymorphic t
let implicits_for_predicate t = 
  let (bl,_) = t.Env.scheme_type in bl <> [] && polymorphic t
let is_logic_constant t = match t.Env.scheme_type with
  | Function ([],_) -> true
  | Function _ | Predicate _ -> false

let print_logic fmt id t =
  reprint_logic fmt id t;
  fprintf fmt "Admitted.@\n";
  if implicits_for_logic_type t then 
    begin
      let b = is_logic_constant t in
      if b then fprintf fmt "Set Contextual Implicit.@\n";
      fprintf fmt "Implicit Arguments %a.@\n" idents id;
      if b then fprintf fmt "Unset Contextual Implicit.@\n";
    end

let print_predicate_scheme fmt p =
  let (l,p) = Env.specialize_predicate p in
  print_scheme fmt l;
  print_predicate fmt p

let reprint_axiom fmt id p =
  fprintf fmt
    "@[<hov 2>(*Why axiom*) Lemma %a :@ @[%a@].@]@\n" 
    idents id print_predicate_scheme p

let print_axiom fmt id p = 
  reprint_axiom fmt id p;
  fprintf fmt "Admitted.@\n"
  (* if is_polymorphic p then fprintf fmt "Implicit Arguments %s.@\n" id *)


let reprint_predicate fmt id p =
  let (l,(bl,p)) = Env.specialize_predicate_def p in
  let l = Env.Vmap.fold (fun _ t acc -> t :: acc) l [] in
  let print_poly fmt x = 
    if v8 then fprintf fmt "(A%d:Set)" x.tag else fprintf fmt "[A%d:Set]" x.tag
  in
  let print_binder fmt (x,pt) = 
    if v8 then 
      fprintf fmt "(%a:%a)" ident x print_pure_type pt
    else
      fprintf fmt "[%a:%a]" ident x print_pure_type pt
  in
  fprintf fmt
     "@[<hov 2>(*Why predicate*) Definition %a %a %a@ := @[%a@].@]@\n" 
    idents id 
    (print_list space print_poly) l
    (print_list space print_binder) bl
    print_predicate p 

let print_predicate fmt id p = 
  reprint_predicate fmt id p;
  if implicits_for_predicate p then 
    fprintf fmt "Implicit Arguments %a.@\n" idents id

let reprint_type fmt id vl =
  fprintf fmt "@[<hov 2>(*Why type*) Definition %a: @[%aSet@].@]@\n"
    idents id (print_list space (fun fmt _ -> fprintf fmt "Set ->")) vl

let print_type fmt id vl = 
  reprint_type fmt id vl;
  fprintf fmt "Admitted.@\n"

let reprint_function fmt id p =
  let (l,(bl,t,e)) = Env.specialize_function_def p in
  let l = Env.Vmap.fold (fun _ t acc -> t :: acc) l [] in
  let print_poly fmt x = 
    if v8 then fprintf fmt "(A%d:Set)" x.tag else fprintf fmt "[A%d:Set]" x.tag
  in
  let print_binder fmt (x,pt) = 
    if v8 then 
      fprintf fmt "(%a:%a)" ident x print_pure_type pt
    else
      fprintf fmt "[%a:%a]" ident x print_pure_type pt
  in
  fprintf fmt
     "@[<hov 2>(*Why function*) Definition %a %a %a@ := @[%a@].@]@\n" 
    idents id
    (print_list space print_poly) l
    (print_list space print_binder) bl
    print_term e 

let print_function fmt id p = 
  reprint_function fmt id p;
  if polymorphic p then 
    begin
      let b = let (bl,_,_) = p.Env.scheme_type in bl = [] in
      if b then fprintf fmt "Set Contextual Implicit.@\n";
      fprintf fmt "Implicit Arguments %a.@\n" idents id;
      if b then fprintf fmt "Unset Contextual Implicit.@\n";
    end

let print_lam_scheme fmt l =
  List.iter
    (fun v -> match v with
       | {type_val=Some (PTvar {type_val=None; tag=x})} -> 
	   if v8 then fprintf fmt "fun (A%d:Set) =>@ " x
	   else fprintf fmt "[A%d:Set]@ " x
       | _ ->
	   assert false)
    l

let print_validation fmt (id, tt, v) =
  let (l,tt,v) = Env.specialize_validation tt v in
  let print_cc_type fmt t = print_scheme fmt l; print_cc_type fmt t in
  let l = Env.Vmap.fold (fun _ t acc -> t :: acc) l [] in
  let print_cc_term fmt c = print_lam_scheme fmt l; print_cc_term fmt c in
  fprintf fmt 
    "@[Definition %a (* validation *)@\n  : @[%a@]@\n  := @[%a@].@]@\n@\n" 
    idents id print_cc_type tt print_cc_term v

let print_cc_functional_program fmt (id, tt, v) =
  let (l,tt,v) = Env.specialize_cc_functional_program tt v in
  let print_cc_type fmt t = print_scheme fmt l; print_cc_type fmt t in
  let l = Env.Vmap.fold (fun _ t acc -> t :: acc) l [] in
  let print_cc_functional_program fmt c = 
    print_lam_scheme fmt l; print_cc_functional_program fmt c 
  in
  fprintf fmt 
    "@[Definition %a (* validation *)@\n  : @[%a@]@\n  := @[%a@].@]@\n@\n" 
    idents id print_cc_type tt print_cc_functional_program v 


open Regen

module Gen = Regen.Make(
struct

  let print_element fmt e = 
    begin match e with
      | Parameter (id, c) -> print_parameter fmt id c
      | Program (id, tt, v) -> print_cc_functional_program fmt (id, tt, v)
      | Obligation (loc, expl, id, s) -> print_obligation fmt loc expl id s
      | Logic (id, t) -> print_logic fmt id t
      | Axiom (id, p) -> print_axiom fmt id p
      | Predicate (id, p) -> print_predicate fmt id p
      | Function (id, f) -> print_function fmt id f
      | AbstractType (id, vl) -> print_type fmt id vl
    end;
    fprintf fmt "@\n"
      
  let reprint_element fmt = function
    | Parameter (id, c) -> reprint_parameter fmt id c
    | Program (id, tt, v) -> print_cc_functional_program fmt (id, tt, v)
    | Obligation (loc, expl, id, s) -> reprint_obligation fmt loc expl id s
    | Logic (id, t) -> reprint_logic fmt id t
    | Axiom (id, p) -> reprint_axiom fmt id p
    | Predicate (id, p) -> reprint_predicate fmt id p
    | Function (id, f) -> reprint_function fmt id f
    | AbstractType (id, vl) -> reprint_type fmt id vl

  let re_oblig_loc = Str.regexp "(\\* Why obligation from .*\\*)"

  let first_time fmt =
    fprintf fmt "\
(* This file was originally generated by why.
   It can be modified; only the generated parts will be overwritten. *)@\n";
    if floats then fprintf fmt "Require Export WhyFloats.@\n";
    fprintf fmt "%s@\n" coq_preamble;
    fprintf fmt "@\n"

  let first_time_trailer fmt = ()

  let not_end_of_element _ s =
    let n = String.length s in n = 0 || s.[n-1] <> '.'

end)

let reset = Gen.reset

let push_decl = function
  | Dgoal (loc,expl,l,s) -> Gen.add_elem (Oblig, l) (Obligation (loc,expl,l,s))
  | Dlogic (_, id, t) -> Gen.add_elem (Lg, rename id) (Logic (id, t))
  | Daxiom (_, id, p) -> Gen.add_elem (Ax, rename id) (Axiom (id, p))
  | Dpredicate_def (_,id,p) -> Gen.add_elem (Pr, rename id) (Predicate (id, p))
  | Dfunction_def (_,id,f) -> Gen.add_elem (Fun, rename id) (Function (id, f))
  | Dtype (_, vl, id) -> Gen.add_elem (Ty, rename id) (AbstractType (id, vl))


let push_parameter id v =
  Gen.add_elem (Param, id) (Parameter (id,v))

let push_program id tt v = 
  Gen.add_elem (Prog, id) (Program (id, tt, v))

let _ = 
  Gen.add_regexp 
    "Lemma[ ]+\\(.*_po_[0-9]+\\)[ ]*:[ ]*" Oblig;
  Gen.add_regexp 
    "(\\*Why goal\\*) Lemma[ ]+\\([^ ]*\\)[ ]*:[ ]*" Oblig;
  Gen.add_regexp 
    "Definition[ ]+\\([^ ]*\\)[ ]*:=[ ]*(\\* validation \\*)[ ]*" Valid;
  Gen.add_regexp 
    "Definition[ ]+\\([^ ]*\\)[ ]*(\\* validation \\*)[ ]*" Valid;
  Gen.add_regexp 
    "(\\*Why\\*) Parameter[ ]+\\([^ ]*\\)[ ]*:[ ]*" Param;
  Gen.add_regexp 
    "(\\*Why axiom\\*) Lemma[ ]+\\([^ ]*\\)[ ]*:[ ]*" Ax;
  Gen.add_regexp 
    "(\\*Why logic\\*) Definition[ ]+\\([^ ]*\\)[ ]*:[ ]*" Lg;
  Gen.add_regexp 
    "(\\*Why predicate\\*) Definition[ ]+\\([^ ]*\\) " Pr;
  Gen.add_regexp 
    "(\\*Why function\\*) Definition[ ]+\\([^ ]*\\) " Fun;
  Gen.add_regexp 
    "(\\*Why program\\*) Definition[ ]+\\([^ ]*\\) " Fun;
  Gen.add_regexp 
    "(\\*Why type\\*) Parameter[ ]+\\([^ ]*\\):" Ty;
  Gen.add_regexp 
    "(\\*Why type\\*) Definition[ ]+\\([^ ]*\\):" Ty

(* validations *)

let valid_q = Queue.create ()

let deps_q = Queue.create ()
let add_dep f = Queue.add f deps_q

let push_validation id tt v = Queue.add (id,tt,v) valid_q 

let print_validations fwe fmt =
  fprintf fmt "(* This file is generated by Why; do not edit *)@\n@\n";
  fprintf fmt "Require Import Why.@\n";
  Queue.iter (fun m -> fprintf fmt "Require Export %s_valid.@\n" m) deps_q;
  fprintf fmt "Require Export %s_why.@\n@\n" fwe;
  Queue.iter (print_validation fmt) valid_q;
  Queue.clear valid_q

let output_validations fwe =
  let f = fwe ^ "_valid.v" in
  let m = Filename.basename fwe in
  print_in_file (print_validations m) f;
  add_dep m

(* output file. *)

let output_file fwe =
  let f = fwe ^ "_why.v" in
  Gen.output_file f;
  if valid then begin
    output_validations fwe
  end

   
