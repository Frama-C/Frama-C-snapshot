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

(*i $Id: zenon.ml,v 1.28 2008/02/18 11:06:36 moy Exp $ i*)

(*s Zenon output *)

open Pp
open Ident
open Options
open Misc
open Error
open Logic
open Logic_decl
open Vcg
open Format
open Cc
open Ltyping
open Env
open Report

(*s Pretty print *)

let is_zenon_keyword =
  let ht = Hashtbl.create 17 in
  List.iter (fun kw -> Hashtbl.add ht kw ()) 
    ["True"; "False"];
  Hashtbl.mem ht

let is_zenon_ident s =
  let is_zenon_char = function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true 
    | _ -> false
  in
  try 
    String.iter (fun c -> if not (is_zenon_char c) then raise Exit) s; true
  with Exit ->
    false

let renamings = Hashtbl.create 17
let fresh_name = 
  let r = ref 0 in fun () -> incr r; "zenon__" ^ string_of_int !r

let rename s =
  if is_zenon_keyword s then
    "zenon__" ^ s
  else if not (is_zenon_ident s) then begin
    try
      Hashtbl.find renamings s
    with Not_found ->
      let s' = fresh_name () in
      Hashtbl.add renamings s s';
      s'
  end else
    s

let ident fmt id = fprintf fmt "%s" (rename (string id))
let idents fmt s = fprintf fmt "%s" (rename s)

let symbol fmt (id,i) = ident fmt id
  (* Monomorph.symbol fmt (id, i) *)

let infix id =
  if id == t_lt then "why__lt"
  else if id == t_le then "why__le"
  else if id == t_gt then "why__gt"
  else if id == t_ge then "why__ge"
  (* int cmp *)
  else if id == t_lt_int then "why__lt_int"
  else if id == t_le_int then "why__le_int"
  else if id == t_gt_int then "why__gt_int"
  else if id == t_ge_int then "why__ge_int"
  (* int ops *)
  else if id == t_add_int then "why__add_int"
  else if id == t_sub_int then "why__sub_int"
  else if id == t_mul_int then "why__mul_int"
  else if id == t_div_int then "why__div_int"
  (* real ops *)
  else if id == t_add_real then "why__add_real"
  else if id == t_sub_real then "why__sub_real"
  else if id == t_mul_real then "why__mul_real"
  else if id == t_div_real then "why__div_real"
  else if id == t_lt_real then "why__lt_real"
  else if id == t_le_real then "why__le_real"
  else if id == t_gt_real then "why__gt_real"
  else if id == t_ge_real then "why__ge_real"
  else assert false

let external_type = function
  | PTexternal _ -> true
  | _ -> false

let rec print_pure_type fmt = function
  | PTint -> fprintf fmt "INT"
  | PTbool -> fprintf fmt "BOOLEAN"
  | PTreal -> fprintf fmt "REAL"
  | PTunit -> fprintf fmt "UNIT"
  | PTvar {type_val=Some pt} -> print_pure_type fmt pt
  | PTvar {type_val=None; tag=t} -> fprintf fmt "'a%d" t
  | PTexternal (i ,id) -> symbol fmt (id,i)

let rec print_term fmt = function
  | Tvar id -> 
      fprintf fmt "%a" ident id
  | Tconst (ConstInt n) -> 
      fprintf fmt "why__int_const_%s" n
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
	fprintf fmt "why__real_const_%s%s" i f
      else if e > 0 then
	fprintf fmt "(why__mul_real why__real_const_%s%s why__real_const_1%s)" i f (String.make e '0')
      else
	fprintf fmt "(why_div_real why__real_const_%s%s why__real_const_1%s)" i f (String.make (-e) '0')
  | Tderef _ -> 
      assert false
  | Tapp (id, ([_;_] as tl), _) when id == t_mod_int ->
      fprintf fmt "@[(%a %a)@]" ident id print_terms tl
  | Tapp (id, [a], _) when id == t_sqrt_real || id == t_int_of_real ->
      fprintf fmt "@[(%a %a)@]" ident id print_term a
  | Tapp (id, [a], _) when id == t_real_of_int ->
      fprintf fmt "@[(%a %a)@]" ident id print_term a
(**
  | Tapp (id, [a; b], _) when id == access ->
      fprintf fmt "@[(access@ %a@ %a)@]" print_term a print_term b
  | Tapp (id, [a; b; c], _) when id == store ->
      fprintf fmt "@[(update@ %a@ %a@ %a)@]" 
	print_term a print_term b print_term c
**)
  | Tapp (id, [t], _) when id == t_neg_int ->
      fprintf fmt "@[(why__sub_int why__int_const_0 %a)@]" print_term t
  | Tapp (id, [t], _) when id == t_neg_real ->
      fprintf fmt "@[(why__sub_real why__real_const_0 %a)@]" print_term t
  | Tapp (id, [a;b], _) when is_relation id || is_arith id ->
      fprintf fmt "@[(%s %a %a)@]" (infix id) print_term a print_term b
  | Tapp (id, [], i) ->
      symbol fmt (id,i)
  | Tapp (id, tl, i) ->
      fprintf fmt "@[(%a %a)@]" symbol (id,i) print_terms tl
  | Tnamed (_, t) -> (* TODO: print name *)
      print_term fmt t

and print_terms fmt tl = 
  print_list space print_term fmt tl

let rec print_predicate fmt = function
  | Ptrue ->
      fprintf fmt "True"
  | Pvar id when id == Ident.default_post ->
      fprintf fmt "True"
  | Pfalse ->
      fprintf fmt "False"
  | Pvar id -> 
      fprintf fmt "%a" ident id
  | Papp (id, tl, _) when id == t_distinct ->
      fprintf fmt "@[(%a)@]" print_predicate (Util.distinct tl)
  | Papp (id, [t], _) when id == well_founded ->
      fprintf fmt "True ;; was well_founded@\n"
  | Papp (id, [a; b], _) when id == t_eq_bool ->
      fprintf fmt "@[(= %a@ %a)@]" print_term a print_term b
  | Papp (id, [a; b], _) when id == t_neq_bool ->
      fprintf fmt "@[(-. (= %a@ %a))@]" print_term a print_term b
  | Papp (id, [a; b], _) when is_eq id ->
      fprintf fmt "@[(= %a@ %a)@]" print_term a print_term b
  | Papp (id, [a; b], _) when is_neq id ->
      fprintf fmt "@[(-. (= %a@ %a))@]" print_term a print_term b
  | Papp (id, [a;b], _) when is_int_comparison id || is_real_comparison id ->
      fprintf fmt "@[(%s %a %a)@]" (infix id) print_term a print_term b
  | Papp (id, [a;b], _) when id == t_zwf_zero ->
      fprintf fmt 
	"@[(/\\ (why__le_int why__int_const_0 %a)@ (why__lt_int %a %a))@]" 
	print_term b print_term a print_term b
  | Papp (id, tl, i) -> 
      fprintf fmt "@[(%a@ %a)@]" symbol (id, i) print_terms tl
  | Pimplies (_, a, b) ->
      fprintf fmt "@[(=> %a@ %a)@]" print_predicate a print_predicate b
  | Piff (a, b) ->
      fprintf fmt "@[(<=> %a@ %a)@]" print_predicate a print_predicate b
  | Pif (a, b, c) ->
      fprintf fmt 
     "@[(/\\ (=> (= %a true) %a)@ (=> (= %a false) %a))@]"
      print_term a print_predicate b print_term a print_predicate c
  | Pand (_, _, a, b) | Forallb (_, a, b) ->
      fprintf fmt "@[(/\\ %a@ %a)@]" print_predicate a print_predicate b
  | Por (a, b) ->
      fprintf fmt "@[(\\/ %a@ %a)@]" print_predicate a print_predicate b
  | Pnot a ->
      fprintf fmt "@[(-.@ %a)@]" print_predicate a
  | Forall (_,id,n,t,_,p) -> 
      let id' = next_away id (predicate_vars p) in
      let p' = subst_in_predicate (subst_onev n id') p in
      fprintf fmt "@[(A. ((%a \"%a\")@ %a))@]" 
	ident id' print_pure_type t print_predicate p'
  | Exists (id,n,t,p) -> 
      let id' = next_away id (predicate_vars p) in
      let p' = subst_in_predicate (subst_onev n id') p in
      fprintf fmt "@[(E. ((%a \"%a\")@ %a))@]" 
	ident id' print_pure_type t print_predicate p'
  | Pfpi _ ->
      failwith "fpi not supported with Zenon"
  | Pnamed (_, p) -> (* TODO: print name *)
      print_predicate fmt p

let cc_external_type = function
  | Cc.TTpure ty -> external_type ty
  | Cc.TTarray (Cc.TTpure (PTexternal _)) -> true
  | _ -> false

let rec print_cc_type fmt = function
  | TTpure pt -> 
      print_pure_type fmt pt
  | TTarray v -> 
      fprintf fmt "(@[ARRAY_%a@])" print_cc_type v
  | _ -> 
      assert false

let print_sequent fmt (hyps,concl) =
  let rec print_seq fmt = function
    | [] ->
	print_predicate fmt concl
    | Svar (id, v) :: hyps -> 
	fprintf fmt "@[(A. ((%a \"%a\")@ %a))@]" 
	  ident id print_pure_type v print_seq hyps
    | Spred (_,p) :: hyps -> 
	fprintf fmt "@[(=> %a@ %a)@]" print_predicate p print_seq hyps
  in
  print_seq fmt hyps

let rec print_logic_type fmt = function
  | Predicate [] ->
      fprintf fmt "prop"
  | Predicate pl ->
      fprintf fmt "%a -> prop" (print_list comma print_pure_type) pl
  | Function ([], pt) ->
      print_pure_type fmt pt
  | Function (pl, pt) ->
      fprintf fmt "%a -> %a" 
	(print_list comma print_pure_type) pl print_pure_type pt

let declare_type fmt id = 
  fprintf fmt "@[;; type %s@]@\n@\n" id
    
let print_parameter fmt id c =
  fprintf fmt 
    "@[;; Why parameter %a@]@\n" idents id;
  fprintf fmt 
    "@[<hov 2>;;  %a: %a@]@\n@\n" idents id print_cc_type c
    
let print_logic fmt id t =
  fprintf fmt ";; Why logic %a@\n@\n" idents id
  (*
  fprintf fmt "@[;; %a%a: %a@]@\n@\n" idents id instance i print_logic_type t
  *)

let print_predicate_def fmt id (bl,p) =
  fprintf fmt "@[;; Why predicate %a@]@\n" idents id;
  fprintf fmt "@[<hov 2>\"%a\" " idents id;
  List.iter (fun (x,_) -> fprintf fmt "(A. ((%a)@ " ident x) bl;
  fprintf fmt "(<=> (%a %a)@ %a)" 
    idents id
    (print_list space (fun fmt (x,_) -> fprintf fmt "%a" ident x)) bl
    print_predicate p;
  List.iter (fun _ -> fprintf fmt "))") bl;
  fprintf fmt "@]@\n@\n"

let print_function_def fmt id (bl,t,e) =
  fprintf fmt "@[;; Why function %a@]@\n" idents id;
  fprintf fmt "@[<hov 2>\"%a\" " idents id;
  List.iter (fun (x,_) -> fprintf fmt "(A. ((%a)@ " ident x) bl;
  fprintf fmt "(= (%a %a)@ %a)" 
    idents id
    (print_list space (fun fmt (x,_) -> fprintf fmt "%a" ident x)) bl
    print_term e;
  List.iter (fun _ -> fprintf fmt "))@]@\n") bl;
  fprintf fmt "@]@\n"

let print_axiom fmt id p =
  fprintf fmt "@[;; Why axiom %s@]@\n" id;
  fprintf fmt "@[<hov 2>\"%s\" %a@]@\n@\n" id print_predicate p

let print_obligation fmt loc expl o s = 
  fprintf fmt "@[;; %s, %a@]@\n" o Loc.gen_report_line loc;
  fprintf fmt "@[<hov 2>$goal %a@]@\n\n" print_sequent s

let push_decl d = Encoding.push (*Monomorph.push_decl*) d

let iter = Encoding.iter (*Monomorph.iter*)

let reset () = Encoding.reset (*Monomorph.reset*) ()

let output_elem fmt = function
  | Dtype (loc, _, id) -> declare_type fmt id
  | Dlogic (loc, id, t) -> print_logic fmt id t.scheme_type
  | Dpredicate_def (loc, id, d) -> print_predicate_def fmt id d.scheme_type
  | Dfunction_def (loc, id, d) -> print_function_def fmt id d.scheme_type
  | Daxiom (loc, id, p) -> print_axiom fmt id p.scheme_type
  | Dgoal (loc, expl, id, s) -> 
      print_obligation fmt loc expl id s.Env.scheme_type

let prelude_done = ref false
let prelude fmt = 
  if not !prelude_done && not no_zenon_prelude then begin
    prelude_done := true;
    fprintf fmt "
;; Zenon prelude

\"why__prelude_1\" ; x+0=x
   (A. ((x \"INT\") (= (why__add_int x why__int_const_0) x)))
\"why__prelude_2\" ; 0+x=x
   (A. ((x \"INT\") (= (why__add_int why__int_const_0 x) x)))
\"why__prelude_3\" ; x+y=y+x
   (A. ((x \"INT\") (A. ((y \"INT\") 
     (= (why__add_int x y) (why__add_int y x))))))

"
  end

let output_file fwe =
  let sep = ";; DO NOT EDIT BELOW THIS LINE" in
  let file = out_file (fwe ^ "_why.znn") in
  do_not_edit_below ~file
    ~before:prelude
    ~sep
    ~after:(fun fmt -> iter (output_elem fmt))

