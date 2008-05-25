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

(*i $Id: cvcl.ml,v 1.54 2008/02/05 12:10:49 marche Exp $ i*)

(*s CVC Lite's output *)

open Ident
open Options
open Misc
open Error
open Logic
open Logic_decl
open Vcg
open Format
open Cc
open Pp
open Ltyping
open Env
open Report

(*s Pretty print *)

let infix id =
  if id == t_lt then "<"
  else if id == t_le then "<="
  else if id == t_gt then ">"
  else if id == t_ge then ">="
  (* int cmp *)
  else if id == t_lt_int then "<"
  else if id == t_le_int then "<="
  else if id == t_gt_int then ">"
  else if id == t_ge_int then ">="
  (* int ops *)
  else if id == t_add_int then "+"
  else if id == t_sub_int then "-"
  else if id == t_mul_int then "*"
  else if id == t_div_int then "/"
  (* real ops *)
  else if id == t_add_real then "+"
  else if id == t_sub_real then "-"
  else if id == t_mul_real then "*"
  else if id == t_div_real then "/"
  else if id == t_lt_real then "<"
  else if id == t_le_real then "<="
  else if id == t_gt_real then ">"
  else if id == t_ge_real then ">="
  else assert false

let external_type = function
  | PTexternal _ -> true
  | _ -> false

let rec print_pure_type fmt = function
  | PTint -> fprintf fmt "INT"
  | PTbool -> fprintf fmt "BOOL"
  | PTreal -> fprintf fmt "REAL"
  | PTunit -> fprintf fmt "UNIT"
  | PTexternal ([pt], id) when id == farray -> 
      fprintf fmt "(ARRAY INT OF %a)" print_pure_type pt
  | PTvar {type_val=Some pt} -> print_pure_type fmt pt
  | PTvar _ -> assert false
  | PTexternal (i ,id) -> fprintf fmt "%s" (Encoding.symbol (id, i))

let rec print_term fmt = function
  | Tvar id -> 
      fprintf fmt "%a" Ident.print id
  | Tconst (ConstInt n) -> 
      fprintf fmt "%s" n
  | Tconst (ConstBool true) -> 
      fprintf fmt "true"
  | Tconst (ConstBool false) -> 
      fprintf fmt "false"
  | Tconst ConstUnit -> 
      fprintf fmt "tt" (* TODO: CORRECT? *)
  | Tconst (ConstFloat (i,f,e)) ->
      let f = if f = "0" then "" else f in
      let e = (if e = "" then 0 else int_of_string e) - String.length f in
      if e = 0 then
	fprintf fmt "%s%s" i f
      else if e > 0 then
	fprintf fmt "(%s%s * 1%s)" i f (String.make e '0')
      else
	fprintf fmt "(%s%s / 1%s)" i f (String.make (-e) '0')
  | Tderef _ -> 
      assert false
  | Tapp (id, ([_;_] as tl), _) when id == t_mod_int ->
      fprintf fmt "@[%a(%a)@]" Ident.print id print_terms tl
  | Tapp (id, [a], _) when id == t_sqrt_real || id == t_int_of_real ->
      fprintf fmt "@[%a(%a)@]" Ident.print id print_term a
  | Tapp (id, [a], _) when id == t_real_of_int ->
      fprintf fmt "@[%a@]" print_term a
(**
  | Tapp (id, [a; b], _) when id == access ->
      fprintf fmt "@[%a[%a]@]" print_term a print_term b
  | Tapp (id, [a; b; c], _) when id == store ->
      fprintf fmt "@[(%a WITH@ [%a] := %a)@]" 
	print_term a print_term b print_term c
**)
  | Tapp (id, [t], _) when id == t_neg_int || id == t_neg_real ->
      fprintf fmt "@[(-%a)@]" print_term t
  | Tapp (id, [a;b], _) when is_relation id || is_arith id ->
      fprintf fmt "@[(%a %s %a)@]" print_term a (infix id) print_term b
  | Tapp (id, [], i) ->
      fprintf fmt "%s" (Encoding.symbol (id, i))
  | Tapp (id, tl, i) ->
      fprintf fmt "@[%s(%a)@]" (Encoding.symbol (id, i)) print_terms tl
  | Tnamed (_, t) -> (* TODO: print name *)
      print_term fmt t

and print_terms fmt tl = 
  print_list comma print_term fmt tl

let rec print_predicate fmt = function
  | Ptrue ->
      fprintf fmt "TRUE"
  | Pvar id when id == Ident.default_post ->
      fprintf fmt "TRUE"
  | Pfalse ->
      fprintf fmt "FALSE"
  | Pvar id -> 
      fprintf fmt "%a" Ident.print id
  | Papp (id, tl, _) when id == t_distinct ->
      fprintf fmt "@[(%a)@]" print_predicate (Util.distinct tl)
  | Papp (id, [t], _) when id == well_founded ->
      fprintf fmt "TRUE %% was well_founded@\n"
  | Papp (id, [a; b], _) when is_eq id ->
      fprintf fmt "@[(%a =@ %a)@]" print_term a print_term b
  | Papp (id, [a; b], _) when is_neq id ->
      fprintf fmt "@[(%a /=@ %a)@]" print_term a print_term b
  | Papp (id, [a;b], _) when is_int_comparison id || is_real_comparison id ->
      fprintf fmt "@[(%a %s %a)@]" print_term a (infix id) print_term b
  | Papp (id, [a;b], _) when id == t_zwf_zero ->
      fprintf fmt "@[((0 <= %a) AND@ (%a < %a))@]" 
	print_term b print_term a print_term b
  | Papp (id, tl, i) -> 
      fprintf fmt "@[%s(%a)@]" (Encoding.symbol (id, i)) print_terms tl
  | Pimplies (_, a, b) ->
      fprintf fmt "@[(%a =>@ %a)@]" print_predicate a print_predicate b
  | Piff (a, b) ->
      fprintf fmt "@[(%a <=>@ %a)@]" print_predicate a print_predicate b
  | Pif (a, b, c) ->
      fprintf fmt 
     "@[((%a=TRUE => %a) AND@ (%a=FALSE => %a))@]"
      print_term a print_predicate b print_term a print_predicate c
  | Pand (_, _, a, b) | Forallb (_, a, b) ->
      fprintf fmt "@[(%a AND@ %a)@]" print_predicate a print_predicate b
  | Por (a, b) ->
      fprintf fmt "@[(%a OR@ %a)@]" print_predicate a print_predicate b
  | Pnot a ->
      fprintf fmt "@[(NOT@ %a)@]" print_predicate a
  | Forall (_,id,n,t,_,p) -> 
      let id' = next_away id (predicate_vars p) in
      let p' = subst_in_predicate (subst_onev n id') p in
      fprintf fmt "@[(FORALL (%a:%a):@ %a)@]" 
	Ident.print id' print_pure_type t print_predicate p'
  | Exists (id,n,t,p) -> 
      let id' = next_away id (predicate_vars p) in
      let p' = subst_in_predicate (subst_onev n id') p in
      fprintf fmt "@[(EXISTS (%a:%a):@ %a)@]" 
	Ident.print id' print_pure_type t print_predicate p'
  | Pfpi _ ->
      failwith "fpi not supported with Simplify"
  | Pnamed (_, p) -> (* TODO: print name *)
      print_predicate fmt p

let print_sequent fmt (hyps,concl) =
  let rec print_seq fmt = function
    | [] ->
	print_predicate fmt concl
    | Svar (id, v) :: hyps -> 
	fprintf fmt "@[(FORALL (%a:%a):@ %a)@]" 
	  Ident.print id print_pure_type v print_seq hyps
    | Spred (_,p) :: hyps -> 
	fprintf fmt "@[(%a =>@ %a)@]" print_predicate p print_seq hyps
  in
  print_seq fmt hyps

let rec print_logic_type fmt = function
  | Predicate [] ->
      fprintf fmt "BOOLEAN"
  | Predicate [pt] ->
      fprintf fmt "(%a -> BOOLEAN)" print_pure_type pt
  | Predicate pl ->
      fprintf fmt "((%a) -> BOOLEAN)" (print_list comma print_pure_type) pl
  | Function ([], pt) ->
      print_pure_type fmt pt
  | Function ([pt1], pt2) ->
      fprintf fmt "(%a -> %a)" print_pure_type pt1 print_pure_type pt2
  | Function (pl, pt) ->
      fprintf fmt "((%a) -> %a)" 
	(print_list comma print_pure_type) pl print_pure_type pt

(* we need to recognize array types after monomorphization *)

let is_array s = String.length s >= 6 && String.sub s 0 6 = "array_"

let declare_type fmt id = 
  if not (is_array id) then fprintf fmt "@[%s: TYPE;@]@\n@\n" id

let print_logic fmt id t =
  fprintf fmt "%%%% Why logic %s@\n" id;
  fprintf fmt "@[%s: %a;@]@\n@\n" id print_logic_type t

let print_predicate_def fmt id (bl,p) =
  fprintf fmt "@[%%%% Why predicate %s@]@\n" id;
  fprintf fmt "@[<hov 2>%s: %a =@ LAMBDA (%a):@ @[%a@];@]@\n@\n"
    id 
    print_logic_type (Predicate (List.map snd bl))
    (print_list comma 
       (fun fmt (x,pt) -> 
	  fprintf fmt "%a: %a" Ident.print x print_pure_type pt )) bl 
    print_predicate p
    
let print_function_def fmt id (bl,t,e) =
  fprintf fmt "@[%%%% Why function %s@]@\n" id;
  fprintf fmt "@[<hov 2>%s: %a =@ LAMBDA (%a):@ @[%a@];@]@\n@\n"
    id
    print_logic_type (Function (List.map snd bl, t))
    (print_list comma 
       (fun fmt (x,pt) -> 
	  fprintf fmt "%a: %a" Ident.print x print_pure_type pt )) bl 
    print_term e

let print_axiom fmt id p =
  fprintf fmt "@[%%%% Why axiom %s@]@\n" id;
  fprintf fmt "@[<hov 2>ASSERT %a;@]@\n@\n" print_predicate p

let print_obligation fmt loc expl o s = 
  fprintf fmt "@[%%%% %s, %a@]@\n" o Loc.gen_report_line loc;
  fprintf fmt "PUSH;@\n@[<hov 2>QUERY %a;@]@\nPOP;@\n@\n" print_sequent s
(*;
  fprintf fmt "@[%%%% %a@]@\n" Util.print_explanation expl
*)

let push_decl d = Encoding.push d

let iter = Encoding.iter

let reset () = Encoding.reset ()

let output_elem fmt = function
  | Dtype (loc, [], id) -> declare_type fmt id
  | Dtype _ -> assert false
  | Dlogic (loc, id, t) -> print_logic fmt id t.scheme_type
  | Dpredicate_def (loc, id, d) -> print_predicate_def fmt id d.scheme_type
  | Dfunction_def (loc, id, d) -> print_function_def fmt id d.scheme_type
  | Daxiom (loc, id, p) -> print_axiom fmt id p.scheme_type
  | Dgoal (loc, expl, id, s) -> print_obligation fmt loc expl id s.Env.scheme_type

let prelude_done = ref false
let prelude fmt = 
  if not !prelude_done && not no_cvcl_prelude then begin
    prelude_done := true;
    fprintf fmt "
UNIT: TYPE;
tt: UNIT;
BOOL: TYPE;
true: BOOL;
false: BOOL;
ASSERT (FORALL (b:BOOL): (b=true OR b=false));
ASSERT (true /= false);
"
  end

let output_file fwe =
  let sep = "%%%% DO NOT EDIT BELOW THIS LINE" in
  let file = out_file (fwe ^ "_why.cvc") in
  do_not_edit_below ~file
    ~before:prelude
    ~sep
    ~after:(fun fmt -> 
	      (*if not no_cvcl_prelude then predefined_symbols fmt;*)
	      iter (output_elem fmt))

