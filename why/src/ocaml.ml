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

(*i $Id: ocaml.ml,v 1.25 2008/02/05 12:10:50 marche Exp $ i*)

(*s Ocaml code output *)

open Format
open Options
open Ident
open Logic
open Misc
open Types
open Env
open Ast
open Pp

(*s pre- and postconditions *)

let pre print_assertion = 
  let print fmt p = fprintf fmt "(* @[%a@] *)" print_assertion p in
  print_list newline print

let post print_assertion fmt q = 
  let exn fmt (x,a) = fprintf fmt "%a => %a" Ident.print x print_assertion a in
  match q with
    | (a, []) -> 
	fprintf fmt "(* @[%a@] *)" print_assertion a 
    | (a, al) -> 
	fprintf fmt "(* @[%a@ | %a@] *)" 
	  print_assertion a (print_list alt exn) al

(*s types and constants *)

let identifiers = print_list comma Ident.print

let print_predicate = Util.print_predicate
let print_assertion = Util.print_assertion

let rec typev fmt = function
  | PureType PTreal -> 
      fprintf fmt "float"
  | PureType pt -> 
      Util.print_pure_type fmt pt
  | Ref (PTexternal ([t], id)) when id == farray  -> 
      fprintf fmt "(%a array)" Util.print_pure_type t
  | Ref v -> 
      fprintf fmt "(%a ref)" Util.print_pure_type v
  | Arrow (bl, c) -> 
      fprintf fmt "%a ->@ %a" (print_list arrow binder_type) bl typec c

and typec fmt c = match c.c_pre, c.c_post with
  | [], None ->
      fprintf fmt "%a" typev c.c_result_type
  | [], Some q ->
      fprintf fmt "%a@ %a" typev c.c_result_type (post print_predicate) q
  | p, None ->
      fprintf fmt "%a@ %a" (pre print_predicate) p typev c.c_result_type
  | p, Some q ->
      fprintf fmt "%a@ %a@ %a" 
	(pre print_predicate) p typev c.c_result_type (post print_predicate) q

and binder_type fmt = function
  | id, v when id == Ident.anonymous -> typev fmt v
  | id, v -> fprintf fmt "(*%a:*)%a" Ident.print id typev v

let binder_id fmt (id, v) =
  fprintf fmt "%a (*:%a*)" Ident.print id typev v

let binder_ids = print_list space binder_id

let constant fmt = function
  | ConstInt n -> fprintf fmt "%s" n
  | ConstBool b -> fprintf fmt "%b" b
  | ConstUnit -> fprintf fmt "()"
  | ConstFloat (i,f,"") -> fprintf fmt "%s.%s" i f
  | ConstFloat (i,f,e) -> fprintf fmt "%s.%se%s" i f e

(*s logical expressions *)

let caml_infix id = is_arith_binop id || is_relation_ id

let infix id = 
  if id == t_add_int then "+" 
  else if id == t_sub_int then "-" 
  else if id == t_mul_int then "*"
  else if id == t_div_int then "/"
  else if id == t_mod_int then "mod"
  else if id == t_add_real then "+." 
  else if id == t_sub_real then "-." 
  else if id == t_mul_real then "*."
  else if id == t_div_real then "/."
  else if is_eq id || is_eq_ id then "="
  else if is_neq id || is_neq_ id then "<>"
  else if id == t_lt_int_ || id == t_lt_real_ then "<"
  else if id == t_le_int_ || id == t_le_real_ then "<="
  else if id == t_gt_int_ || id == t_gt_real_ then ">"
  else if id == t_ge_int_ || id == t_ge_real_ then ">="
  else begin eprintf "infix id = %a@." Ident.print id; assert false end

let prefix fmt id = fprintf fmt "( %s )" (infix id)

let rec expression fmt = function
  | Tvar id -> 
      Ident.print fmt id
  | Tconst c -> 
      constant fmt c
  | Tderef id ->
      fprintf fmt "!%a" Ident.print id
  | Tapp (id, [Tderef t], _) when id == Ident.array_length ->
      fprintf fmt "(Array.length %a)" Ident.print t
  | Tapp (id, [t], _) when id == t_neg_int ->
      fprintf fmt "(-%a)" expression t
  | Tapp (id, [t], _) when id == t_neg_real ->
      fprintf fmt "(-. %a)" expression t
  | Tapp (id, [t], _) when id == t_sqrt_real ->
      fprintf fmt "(sqrt %a)" expression t
  | Tapp (id, [t], _) when id == t_real_of_int ->
      fprintf fmt "(real %a)" expression t
  | Tapp (id, [a; b], _) when id == access ->
      fprintf fmt "%a.(%a)" expression a expression b
  | Tapp (id, [a; b], _) when caml_infix id ->
      fprintf fmt "(%a %s %a)" expression a (infix id) expression b
  | Tapp (id, tl, _) -> 
      fprintf fmt "(%a %a)" Ident.print id (print_list space expression) tl
  | Tnamed (User n, t) ->
      fprintf fmt "@[(* %s: *)@ %a@]" (String.escaped n) expression t
  | Tnamed (_, t) -> expression fmt t

(*s program expressions *)

let rec expr fmt e = 
  let k = e.info in
  let q = k.t_post in
  if not ocaml_annot || q = None then
    fprintf fmt "@[%a@]" exprd e.desc
  else match q with
    | Some q -> 
	fprintf fmt "@[<hv>%a@ %a@]" exprd e.desc (post print_assertion) q
    | None ->
	fprintf fmt "@[<hv>%a@]" exprd e.desc

and exprd fmt = function
  | Var id when caml_infix id ->
      fprintf fmt "%a" prefix id
  | Var id ->
      Ident.print fmt id
  | Seq (e1, e2) ->
      fprintf fmt "@[<hv>begin@;<1 2>%a;@ %a end@]" expr e1 expr e2
  | Loop (inv, var, e2) ->
      fprintf fmt "@[<hv>while true do@;<1 2>%a@ done@]" expr e2
  | If (e1, e2, e3) ->
      fprintf fmt "(@[<hv>if %a then@;<1 2>%a@ else@;<1 2>%a@])" 
	expr e1 expr e2 expr e3
  | Lam (bl, p, e) ->
      fprintf fmt "@[<hov 2>(fun %a ->@ %a)@]" binder_ids bl expr_pre (p,e)
  | AppTerm ({desc=AppTerm ({desc=Var id}, t1, _)}, t2, _) 
    when is_poly id || id == t_mod_int ->
      fprintf fmt "@[<hov 2>(%a %s@ %a)@]" 
      expression t1 (infix id) expression t2
  | AppTerm (e, t, _) ->
      fprintf fmt "@[<hov 2>(%a@ %a)@]" expr e expression t
  | AppRef (e, a, _) ->
      fprintf fmt "@[<hov 2>(%a@ %a)@]" expr e Ident.print a
  | LetRef (id, e1, e2) ->
      fprintf fmt "@[(@[<hov 2>let %a =@ ref %a in@]@\n%a)@]" 
	Ident.print id expr e1 expr e2
  | LetIn (id, e1, e2) ->
      fprintf fmt "@[(@[<hov 2>let %a =@ %a in@]@\n%a)@]" 
	Ident.print id expr e1 expr e2
  | Rec (id, bl, v, var, p, e) ->
      fprintf fmt "@[<hov 2>(let rec %a %a =@ %a in@ %a)@]" 
	Ident.print id binder_ids bl expr_pre (p,e) Ident.print id
  | Raise (id, None) ->
      fprintf fmt "@[<hov 2>(raise %a)@]" Ident.print id
  | Raise (id, Some e) ->
      fprintf fmt "@[<hov 2>(raise@ (%a %a))@]" Ident.print id expr e
  | Try (e, hl) ->
      fprintf fmt "(@[<hv>try@;<1 2>%a@ with@ @[<v>%a@]@])" 
	expr e (print_list newline handler) hl
  | Expression t -> 
      expression fmt t
  | Absurd ->
      fprintf fmt "@[assert false@]"
  | Any _ ->
      fprintf fmt "@[assert false (* code not given *)@]"
  | Assertion (k, p,e) ->
      expr_pre fmt (p,e)
  | Post (e, q, _) ->
      let q = post_app a_value q in
      fprintf fmt "@[(%a@ %a)@]" expr e (post print_predicate) q
  | Label (l, e) -> 
      fprintf fmt "@[(* label %s *)@ %a@]" l expr e

and expr_pre fmt (p,e) =
  fprintf fmt "@[%a@ %a@]" (pre print_assertion) p expr e

and handler fmt = function
  | (id, None), e -> 
      fprintf fmt "| %a -> %a" Ident.print id expr e
  | (id, Some id'), e -> 
      fprintf fmt "| %a %a -> %a" Ident.print id Ident.print id' expr e

let decl fmt (id, e) =
  fprintf fmt "@[<hov 2>let %a =@ %a@]@\n@\n" Ident.print id expr e

(*s Parameters (collected to make a functor) *)

let params = Queue.create ()

let push_parameters ids v = List.iter (fun id -> Queue.add (id, v) params) ids

(*s We make a functor if some parameters are present *)

let parameter fmt (id, v) =
  fprintf fmt "@\n@[<hov 2>val %a : %a@]@\n" Ident.print id typev v 

let progs = Queue.create ()

let push_program id p = Queue.add (id, p) progs

let output fmt = 
  fprintf fmt "(* code generated by why --ocaml *)@\n@\n";
  let print_decls fmt () = Queue.iter (decl fmt) progs in
  if Queue.is_empty params then
    fprintf fmt "@[%a@]" print_decls ()
  else begin
    fprintf fmt "@[module type Parameters = sig@\n  @[";
    Queue.iter (parameter fmt) params;
    fprintf fmt "@]@\nend@\n@\n@]";
    fprintf fmt "@[module Make(P : Parameters) = struct@\n";
    fprintf fmt "  open P@\n@\n";
    fprintf fmt "  @[%a@]@\nend@\n@]" print_decls ()
  end



