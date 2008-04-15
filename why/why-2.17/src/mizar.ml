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

(*i $Id: mizar.ml,v 1.50 2008/11/05 14:03:17 filliatr Exp $ i*)

(*s Mizar output *)

open Options
open Ident
open Misc
open Error
open Logic
open Logic_decl
open Vcg
open Format
open Cc
open Pp

type elem = 
  | Obligation of obligation
  | Logic of string * logic_type Env.scheme
  | Axiom of string * predicate Env.scheme
  | Predicate of string * predicate_def Env.scheme

let elem_q = Queue.create ()

let reset () = Queue.clear elem_q

let push_decl = function
  | Dgoal (loc, expl, id, s) -> 
      Queue.add (Obligation (loc, expl, id, s.Env.scheme_type)) elem_q
  | Dlogic (_, id, t) -> Queue.add (Logic (id, t)) elem_q
  | Daxiom (_, id, p) -> Queue.add (Axiom (id, p)) elem_q
  | Dpredicate_def (_, id, p) -> 
      Queue.add (Predicate (Ident.string id, p)) elem_q
  | Dinductive_def(loc, ident, inddef) ->
      failwith "Mizar output: inductive def not yet supported"
  | Dfunction_def _ -> () (*TODO*)
  | Dtype _ -> () (*TODO*)

(*s Pretty print *)

let rec print_pure_type fmt = function
  | PTint -> fprintf fmt "Integer"
  | PTbool -> fprintf fmt "Element of BOOLEAN"
  | PTunit -> fprintf fmt "Element of {0}"
  | PTreal -> fprintf fmt "Real"
  | PTexternal([],id) -> Ident.print fmt id
  | PTexternal([v],id) when id == farray ->
      begin match v with
	| PTunit -> fprintf fmt "XFinSequence of {0}"
	| PTbool -> fprintf fmt "XFinSequence of BOOLEAN"
	| PTint -> fprintf fmt "XFinSequence of INT"
	| PTreal -> fprintf fmt "XFinSequence of REAL"
	| PTexternal _ as ty ->
	    fprintf fmt "XFinSequence of %a" print_pure_type ty
	| PTvar _ -> failwith "no polymorphism with Mizar yet"
      end
  | PTexternal _ | PTvar _ ->  failwith "no polymorphism with Mizar yet"

let prefix_id id =
  (* int cmp *)
  if id == t_lt_int then "int_lt" 
  else if id == t_le_int then "int_le"
  else if id == t_gt_int then "int_gt"
  else if id == t_ge_int then "int_ge"
  else if id == t_eq_int then assert false (* TODO *)
  else if id == t_neq_int then assert false (* TODO *)
  (* real cmp *)
  else if id == t_lt_real then "real_lt" 
  else if id == t_le_real then "real_le"
  else if id == t_gt_real then "real_gt"
  else if id == t_ge_real then "real_ge"
  else if id == t_eq_real then assert false (* TODO *)
  else if id == t_neq_real then assert false (* TODO *)
  (* bool cmp *)
  else if id == t_eq_bool then assert false (* TODO *)
  else if id == t_neq_bool then assert false (* TODO *)
  (* unit cmp *)
  else if id == t_eq_unit then assert false (* TODO *)
  else if id == t_neq_unit then assert false (* TODO *)
  (* int ops *)
  else if id == t_add_int then "int_add"
  else if id == t_sub_int then "int_sub"
  else if id == t_mul_int then "int_mul"
  else if id == t_div_int then assert false (* TODO *)
  else if id == t_mod_int then assert false (* TODO *)
  else if id == t_neg_int then "int_neg"
  (* real ops *)
  else if id == t_add_real then "real_add"
  else if id == t_sub_real then "real_sub"
  else if id == t_mul_real then "real_mul"
  else if id == t_div_real then "real_div"
  else if id == t_neg_real then "real_neg"
  else if id == t_sqrt_real then assert false (* TODO *)
  else if id == t_real_of_int then assert false (* TODO *)
  else assert false

let rec print_term fmt t = 
  let rec print0 fmt = function
    | Tapp (id, [a; b], _) when is_relation id -> 
	fprintf fmt "@[%s(@[%a,@ %a@])@]" 
	  (prefix_id id) print0 a print0 b
    | t ->
	print1 fmt t
  and print1 fmt = function
    | Tapp (id, [a; b], _) when id == t_add_int || id == t_add_real ->
	fprintf fmt "%a +@ %a" print1 a print2 b
    | Tapp (id, [a; b], _) when id == t_sub_int || id == t_sub_real ->
	fprintf fmt "%a -@ %a" print1 a print2 b
    | t ->
	print2 fmt t
  and print2 fmt = function
    | Tapp (id, [a; b], _) when id == t_mul_int || id == t_mul_real ->
	fprintf fmt "%a *@ %a" print2 a print3 b
    | Tapp (id, [a; b], _) when id == t_div_int || id == t_div_real ->
	fprintf fmt "%a /@ %a" print2 a print3 b
    | Tapp (id, [a; b], _) when id == t_mod_int ->
	fprintf fmt "%a mod %a" print2 a print3 b
    | t -> 
	print3 fmt t
  and print3 fmt = function
    | Tvar id -> 
	Ident.print fmt id
    | Tconst (ConstInt n) -> 
	fprintf fmt "%s" n
    | Tconst (ConstBool true) -> 
	fprintf fmt "TRUE" 
    | Tconst (ConstBool false) -> 
	fprintf fmt "FALSE" 
    | Tconst ConstUnit -> 
	fprintf fmt "(Extract 0)" 
    | Tconst (ConstFloat (i,f,"")) ->
	fprintf fmt "%s.%s" i f
    | Tconst (ConstFloat (i,f,e)) ->
	fprintf fmt "%s.%se%s" i f e
    | Tderef _ -> 
	assert false
    (* arithmetic *)
    | Tapp (id, [a], _) when id == t_neg_int || id == t_neg_real ->
	fprintf fmt "(@[-%a@])" print3 a
    | Tapp (id, [_;_], _) as t when is_relation id || is_int_arith_binop id ->
	fprintf fmt "(@[%a@])" print0 t
    (* arrays *)
    | Tapp (id, [a; b], _) when id == access ->
	fprintf fmt "(@[%a.%a@])" print0 a print0 b
    | Tapp (id, [a; b; c], _) when id == store ->
	fprintf fmt "(@[%a+*(%a,@ %a)@])" 
	print3 b print0 a print0 c
    | Tapp (id, [a], _) when id == Ident.array_length ->
	fprintf fmt "(@[len %a@])" print0 a
    (* any other application *)
    | Tapp (id, tl, _) when is_relation id || is_arith id -> 
	fprintf fmt "%s(@[%a@])" (prefix_id id) print_terms tl
    | Tapp (id, tl, _) ->
	fprintf fmt "%a(@[%a@])" Ident.print id print_terms tl
    | Tnamed (_, t) -> (* TODO: print name *)
	print3 fmt t
  in
  print0 fmt t

and print_terms fmt tl = 
  print_list comma print_term fmt tl

let infix_relation id =
       if id == t_lt_int || id == t_lt_real then "<" 
  else if id == t_le_int || id == t_le_real then "<="
  else if id == t_gt_int || id == t_gt_real then ">"
  else if id == t_ge_int || id == t_ge_real then ">="
  else if is_eq id then "="
  else if is_neq id then "<>"
  else assert false

let print_predicate fmt p = 
  let rec print0 fmt = function
    | Pimplies (_, a, b) ->
	fprintf fmt "(@[%a implies@ %a@])" print1 a print0 b
    | Piff ( a, b) ->
	fprintf fmt "(@[%a iff@ %a@])" print1 a print0 b
    | Pif (a, b, c) ->
	fprintf fmt "(@[(%a = TRUE implies %a) &@ (%a = FALSE implies %a)@])" 
	print_term a print0 b print_term a print0 c
    | p -> 
	print1 fmt p
  and print1 fmt = function
    | Por (a, b) ->
	fprintf fmt "@[%a or@ %a@]" print2 a print1 b
    | p ->
	print2 fmt p
  and print2 fmt = function
    | Pand (_, _, a, b) | Forallb (_, a, b) ->
	fprintf fmt "@[%a &@ %a@]" print3 a print2 b
    | p ->
	print3 fmt p
  and print3 fmt = function
    | Ptrue ->
	fprintf fmt "not contradiction"
    | Pvar id when id == Ident.default_post ->
	fprintf fmt "not contradiction"
    | Pfalse ->
	fprintf fmt "contradiction"
    | Pvar id -> 
	fprintf fmt "%a" Ident.print id
    | Papp (id, tl, _) when id == t_distinct ->
	fprintf fmt "@[(%a)@]" print0 (Util.distinct tl)
    | Papp (id, [a; b], _) when is_relation id ->
	fprintf fmt "@[%a %s@ %a@]" 
	print_term a (infix_relation id) print_term b
    | Papp (id, [a; b], _) when id == t_zwf_zero ->
	fprintf fmt "@[(0 <= %a &@ %a < %a)@]" 
	print_term b print_term a print_term b
    | Papp (id, tl, _) when is_relation id || is_arith id ->
	fprintf fmt "@[%s(%a)@]" (prefix_id id) print_terms tl
    | Papp (id, tl, _) -> 
	fprintf fmt "@[%a(%a)@]" Ident.print id print_terms tl
    | Pnot a ->
	fprintf fmt "@[not %a@]" print3 a
    | Forall (_,id,n,t,_,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[for %s@ being %a holds@ %a@])" (Ident.string id')
	  print_pure_type t print0 p'
    | Exists (id,n,t,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[ex %s being %a st@ %a@])" (Ident.string id')
	  print_pure_type t print0 p'
    | Pfpi _ ->
	failwith "fpi not supported with Mizar"
    | Pnamed (_, p) -> (* TODO: print name *)
	print3 fmt p
    | (Por _ | Piff _ | Pand _ | Pif _ | Pimplies _ | Forallb _) as p -> 
	fprintf fmt "(%a)" print0 p
  in
  print0 fmt p

let print_sequent fmt (hyps,concl) =
  let rec print_seq fmt = function
    | [] ->
	print_predicate fmt concl
    | Svar (id, v) :: hyps -> 
	fprintf fmt "@[for %a being @[%a@] holds@]@\n" 
	  Ident.print id print_pure_type v;
	print_seq fmt hyps
    | Spred (_, p) :: hyps -> 
	fprintf fmt "@[(@[%a@]) %s@]@\n" print_predicate p
	  (match hyps with Spred _ :: _ -> "&" | _ -> "implies");
	print_seq fmt hyps
  in
  let print_intro fmt = function
    | Svar (id, v) -> 
	fprintf fmt "let %a be @[%a@];@\n" Ident.print id print_pure_type v
    | Spred (id, p) -> 
	fprintf fmt "assume %a: @[%a@];@\n" Ident.print id print_predicate p
  in
  let print_intros fmt = List.iter (print_intro fmt) in
  fprintf fmt "@[ @[%a@]@\nproof@\n @[%a@]:: EDIT BELOW THIS LINE@]" 
    print_seq hyps print_intros hyps

let rec print_thesis fmt = function
  | Pand (_, _, t1, t2) -> fprintf fmt "%a@ %a" print_thesis t1 print_thesis t2
  | t -> fprintf fmt "@[thus %a@];" print_predicate t

let reprint_obligation fmt loc expl id s =
  let s = s.Env.scheme_type in
  fprintf fmt "@[ :: %a @]@\n" Loc.report_obligation_position loc;
  fprintf fmt "@[ (*Why goal*) theorem %s:@\n @[%a@]@]@\n" id print_sequent s
  (*;
  fprintf fmt "@[ :: %a @]@\n@\n" Util.print_explanation expl
  *)

let print_obligation fmt loc expl id s =
  reprint_obligation fmt loc expl id s;
  let t = snd s.Env.scheme_type in
  fprintf fmt "@[  :: FILL PROOF HERE@\n  @[%a@]@]@\n end;@\n" print_thesis t

let print_logic fmt id t = 
  let _ = Env.specialize_logic_type t in
  assert false (*TODO*)

let reprint_logic fmt id t = print_logic fmt id t

let reprint_axiom fmt id p =
  let _ = Env.specialize_predicate p in
  assert false (*TODO*)
  (*
  fprintf fmt "@[ :: Why Axiom @]@\n";
  fprintf fmt "@[ theorem %s:@\n @[%a@];@]@\n" id print_predicate p *)

let print_axiom = reprint_axiom

open Regen

module Gen = Regen.Make(
struct

  let print_element fmt e = 
    begin match e with
      | Parameter _ -> assert false
      | Program _ -> assert false
      | Obligation (loc, expl, id, s) -> print_obligation fmt loc expl id s
      | Logic (id, t) -> print_logic fmt id t
      | Axiom (id, p) -> print_axiom fmt id p
      | Predicate _ -> assert false (*TODO*)
      | Inductive _ -> assert false (*TODO*)
      | Function _ -> assert false (*TODO*)
      | AbstractType _ -> assert false (*TODO*)
    end;
    fprintf fmt "@\n"
      
  let reprint_element fmt = function
    | Parameter _ -> assert false
    | Program _ -> assert false
    | Obligation (loc, expl, id, s) -> reprint_obligation fmt loc expl id s
    | Logic (id, t) -> reprint_logic fmt id t
    | Axiom (id, p) -> reprint_axiom fmt id p
    | Predicate _ -> assert false (*TODO*)
    | Inductive _ -> assert false (*TODO*)
    | Function _ -> assert false (*TODO*)
    | AbstractType _ -> assert false (*TODO*)

  let re_oblig_loc = Str.regexp " :: Why obligation from .*"

  let environ = " vocabularies INT_1, ARYTM_1, MARGREL1, ALGSTR_1, FUNCT_1, FUNCT_4, FINSEQ_1,
  AFINSQ_1, WHY;
 notations TARSKI, SUBSET_1, ARYTM_0, XCMPLX_0, XREAL_0, REAL_1, INT_1,
  MARGREL1, ALGSTR_1, AFINSQ_1, WHY;
 constructors NAT_1, ALGSTR_1, AFINSQ_1, WHY;
 clusters XREAL_0, INT_1, WHY;
 requirements BOOLE, SUBSET, ARITHM, REAL, NUMERALS;
 theorems AXIOMS, XCMPLX_1, SQUARE_1, REAL_1, INT_1, WHY;"

  let first_time fmt =
    fprintf fmt "\
:: This file was originally generated by why.
:: It can be modified; only the generated parts will be overwritten. 

environ
%s

begin :: proof obligations start here
" (match mizar_environ with None -> environ | Some s -> s)

  let first_time_trailer fmt = ()

  let edit_below = Str.regexp "[ ]*:: EDIT BELOW THIS LINE[ ]*"
  let not_end_of_element _ s = not (Str.string_match edit_below s 0)

end)

let reset = Gen.reset

let push_obligations = 
  List.iter (fun (loc,expl,l,s) -> Gen.add_elem (Oblig, l) (Obligation (loc,expl,l,s)))

let push_parameter id v =
  Gen.add_elem (Param, id) (Parameter (id,v))

let _ = 
  Gen.add_regexp 
    " theorem[ ]+\\(.*_po_[0-9]+\\)[ ]*:[ ]*" Oblig;
  Gen.add_regexp 
    "(\\*Why goal\\*) theorem[ ]+\\([^ ]*\\)[ ]*:[ ]*" Oblig;
  Gen.add_regexp 
    "(\\*Why\\*) Parameter[ ]+\\([^ ]*\\)[ ]*:[ ]*" Param

let output_file fwe =
  let f = fwe ^ "_why.miz" in
  Gen.output_file ~margin:75 f
