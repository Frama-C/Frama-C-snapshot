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

open Logic
open Cc
open Ident
open Format
open Misc
open Pp
open Tags
open Astprinter

let print_term fmt t = 
  let rec print0 fmt t = 
    print1 fmt t
  and print1 fmt = function
    | Tapp (id, [a;b], _) when id == t_add_int || id == t_add_real ->
	fprintf fmt "@[%a +@ %a@]" print1 a print2 b
    | Tapp (id, [a;b], _) when id == t_sub_int || id == t_sub_real ->
	fprintf fmt "@[%a -@ %a@]" print1 a print2 b
    | t ->
	print2 fmt t
  and print2 fmt = function
    | Tapp (id, [a;b], _) when id == t_mul_int || id == t_mul_real ->
	fprintf fmt "@[%a *@ %a@]" print2 a print3 b
    | Tapp (id, [a;b], _) when id == t_div_int || id == t_div_real ->
	fprintf fmt "@[%a /@ %a@]" print2 a print3 b
    | Tapp (id, [a;b], _) when id == t_mod_int ->
	fprintf fmt "@[%a %%@ %a@]" print2 a print3 b
    | t ->
	print3 fmt t
  and print3 fmt = function
    | Tconst (ConstInt n) -> 
	fprintf fmt "%s" n
    | Tconst (ConstBool b) -> 
	fprintf fmt "%b" b
    | Tconst ConstUnit -> 
	fprintf fmt "void" 
    | Tconst (ConstFloat (i,f,e)) -> 
	let f = if f = "0" then "" else f in
	let e = (if e = "" then 0 else int_of_string e) - String.length f in
	if e = 0 then
	  fprintf fmt "%s%s" i f
	else if e > 0 then
	  fprintf fmt "(%s%s * 1%s)" i f (String.make e '0')
	else
	  fprintf fmt "(%s%s / 1%s)" i f (String.make (-e) '0')
    | Tvar id when id == implicit ->
	fprintf fmt "<?>" (* should not happen *)
    | Tvar id when id == t_zwf_zero ->
	fprintf fmt "<Zwf(0)>" (* should not happen *)
    | Tvar id | Tapp (id, [], _) -> 
	Ident.print fmt id
    | Tderef _ ->
	assert false
    | Tapp (id, [t], _) when id == t_neg_int ->
	fprintf fmt "-%a" print3 t
    | Tapp (id, [_;_], _) as t when is_arith_binop id ->
	fprintf fmt "@[(%a)@]" print0 t
    | Tapp (id, [a; b; c], _) when id == if_then_else -> 
	fprintf fmt "(@[if %a then@ %a else@ %a@])" print0 a print0 b print0 c
    | Tapp (id, tl, _) -> 
	fprintf fmt "@[%s(%a)@]" (Ident.string id) print_terms tl
    | Tnamed (User n, t) ->
	(match (Tools.grab_infos n) with
	   | None -> 
	       fprintf fmt "@[%a@]" print3 t
	   | Some l ->
	       let n = new_tag l in
	       pp_open_tag fmt n;
	       fprintf fmt "@[%a@]" print3 t;
	       pp_close_tag fmt ()
	)
    | Tnamed (Internal n, t) ->
	begin
	  try
	    let _xpl = Hashtbl.find Util.explanation_table n in
	    (* TODO *)
	    (* let n = new_tag xpl in
	    pp_open_tag fmt n;
	    *)
	    fprintf fmt "@[%a@]" print3 t;
	    (* pp_close_tag fmt () *)
	    
	  with
	      Not_found -> fprintf fmt "@[%a@]" print3 t
	end
  and print_terms fmt tl =
    print_list comma print3 fmt tl
  in
  print1 fmt t

let pprefix_id id =
  if id == t_lt || id == t_lt_int || id == t_lt_real then "<" 
  else if id == t_le || id == t_le_int || id == t_le_real then "<="
  else if id == t_gt || id == t_gt_int || id == t_gt_real then ">"
  else if id == t_ge || id == t_ge_int || id == t_ge_real then ">="
  else if is_eq id then "="
  else if is_neq id then "<>"
  else assert false

let print_predicate fmt p =
  let rec print0 fmt = function
    | Pif (a, b, c) -> 
	fprintf fmt "(@[if %a@ then %a@ else %a@])"
	  print_term a print0 b print0 c
    | Pimplies (_, a, b) -> 
	fprintf fmt "@[%a ->@ %a@]" print1 a print0 b
    | Piff (a, b) -> 
	fprintf fmt "@[%a <->@ %a@]" print1 a print0 b
    | p -> print1 fmt p
  and print1 fmt = function
    | Por (a, b) -> fprintf fmt "@[%a or@ %a@]" print2 a print1 b
    | p -> print2 fmt p
  and print2 fmt = function
    | Pand (_, _, a, b) | Forallb (_, a, b) -> 
        fprintf fmt "@[%a and@ %a@]" print3 a print2 b
    | p -> print3 fmt p
  and print3 fmt = function
    | Ptrue -> 
	fprintf fmt "true"
    | Pvar id when id == Ident.default_post ->
	fprintf fmt "true"
    | Pfalse -> 
	fprintf fmt "false"
    | Pvar id -> 
	Ident.print fmt id
    | Papp (id, [t], _) when id == well_founded ->
	fprintf fmt "@[well_founded(%a)@]" print_term t
    | Papp (id, [a;b], _) when id == t_zwf_zero ->
	fprintf fmt "zwf(0, %a, %a)" print_term a print_term b
    | Papp (id, [a;b], _) when is_relation id ->
	fprintf fmt "@[%a %s %a@]" 
	print_term a (pprefix_id id) print_term b
    | Papp (id, l, _) ->
	fprintf fmt "@[@[%a(%a@])@]" Ident.print id
	  (print_list comma print_term) l
    | Pnot p -> 
	fprintf fmt "not %a" print3 p
    | Forall (_,id,n,t,_,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[forall %s:%a.@ %a@])" (Ident.string id')
	  print_pure_type t print0 p'
    | Exists (id,n,t,p) -> 
	let id' = next_away id (predicate_vars p) in
	let p' = subst_in_predicate (subst_onev n id') p in
	fprintf fmt "(@[exists %s:%a,@ %a@])" (Ident.string id')
	  print_pure_type t print0 p'
    | Pfpi _ ->
	failwith "fpi not supported"
    | Pnamed (User n, p) ->
	(match (Tools.grab_infos n) with
	   | None -> 
	       fprintf fmt "@[%a@]" print3 p
	   | Some l ->
	       let n = new_tag l in
	       pp_open_tag fmt n;
	       fprintf fmt "@[%a@]" print3 p;
	       pp_close_tag fmt ()
	)
    | Pnamed (Internal n, p) ->
	begin
	  try
	    let _xpl = Hashtbl.find Util.explanation_table n in
	    (* TODO *)
	    (* let n = new_tag xpl in
	    pp_open_tag fmt n;
	    *)
	    fprintf fmt "@[%a@]" print3 p;
	    (* pp_close_tag fmt () *)
	    
	  with
	      Not_found -> fprintf fmt "@[%a@]" print3 p
	end
    | (Por _ | Piff _ | Pand _ | Pif _ | Pimplies _ | Forallb _) as p -> 
	fprintf fmt "@[(%a)@]" print0 p
  in
  print0 fmt p
