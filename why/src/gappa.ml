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

(*i $Id: gappa.ml,v 1.19 2008/03/17 08:56:17 filliatr Exp $ i*)

(*s Gappa's output *)

open Ident
open Options
open Misc
open Error
open Logic
open Cc
open Format
open Pp

(* Gappa terms and formulas *)

type exact = Exact | Double

type mode = Mnearest_even

type gappa_constant =
  | GCdecimal of real_constant
  | GCbinary of string * string (* <nnn>b<mmm> *)

type gterm =
  | Gvar of exact * string
  | Grnd of exact * mode * gterm
  | Gcst of gappa_constant
  | Gneg of gterm
  | Gadd of gterm * gterm
  | Gsub of gterm * gterm
  | Gmul of gterm * gterm
  | Gdiv of gterm * gterm
  | Gabs of gterm

type gpred =
  | Gle of gterm * gappa_constant
  | Gge of gterm * gappa_constant
  | Gin of gterm * gappa_constant * gappa_constant
  | Gimplies of gpred * gpred
  | Gand of gpred * gpred
  | Gor of gpred * gpred
  | Gnot of gpred

type gobligation = (string * gterm) list * gpred

exception NotGappa

(* compilation to Gappa formulas *)

let real_of_int = Ident.create "real_of_int"

let nearest_even = Ident.create "nearest_even"

let t_double = Ident.create "double"
let neg_double = Ident.create "neg_double"
let add_double = Ident.create "add_double"
let sub_double = Ident.create "sub_double"
let mul_double = Ident.create "mul_double"
let div_double = Ident.create "div_double"
let pow_real = Ident.create "pow_real"
let d_to_r = Ident.create "d_to_r"
let d_to_exact = Ident.create "d_to_exact"
let r_to_d = Ident.create "r_to_d"

let double_round_error = Ident.create "double_round_error"

let mode = function
  | Tapp (id, [], _) | Tvar id when id == nearest_even ->
      Mnearest_even
  | _ ->
      raise NotGappa

let is_int_constant = function
  | Tconst (ConstInt _) -> true
  | Tapp (id, [Tconst (ConstInt _)], _) when id == t_neg_int -> true
  | _ -> false

let eval_int_constant = function
  | Tconst (ConstInt n) -> n
  | Tapp (id, [Tconst (ConstInt n)], _) when id == t_neg_int -> "-"^n
  | _ -> assert false

let is_constant = function
  | Tconst (ConstFloat _) -> true
  | Tapp (id, [Tconst (ConstInt _)], _) when id == real_of_int -> true
  | Tapp (pr, [Tapp (ri1, [Tconst (ConstInt "2")], _);
	       Tapp (ri2, [n], _)], _) 
    when pr == pow_real && ri1 == real_of_int && ri2 = real_of_int 
      && is_int_constant n -> true
  | _ -> false

let eval_constant = function
  | Tconst (ConstFloat f) -> 
      GCdecimal f
  | Tapp (id, [Tconst (ConstInt n)], _) when id == real_of_int -> 
      GCdecimal (n, "0", "")
  | Tapp (pr, [Tapp (ri1, [Tconst (ConstInt "2")], _);
	       Tapp (ri2, [n], _)], _) 
    when pr == pow_real && ri1 == real_of_int && ri2 = real_of_int -> 
      GCbinary ("1", eval_int_constant n)
  | _ -> 
      assert false

let rec term e = function
  | t when is_constant t ->
      Gcst (eval_constant t)
  | Tconst _ ->
      raise NotGappa
  | Tvar id -> 
      Gvar (e, Ident.string id)
  | Tderef id -> 
      Gvar (e, Ident.string id)
  (* real ops *)
  | Tapp (id, [t], _) when id == t_neg_real -> 
      Gneg (term e t)
  | Tapp (id, [t1; t2], _) when id == t_add_real -> 
      Gadd (term e t1, term e t2)
  | Tapp (id, [t1; t2], _) when id == t_sub_real -> 
      Gsub (term e t1, term e t2)
  | Tapp (id, [t1; t2], _) when id == t_mul_real -> 
      Gmul (term e t1, term e t2)
  | Tapp (id, [t1; t2], _) when id == t_div_real -> 
      Gdiv (term e t1, term e t2)
  (* float ops *)
  | Tapp (id, [m; t], _) when id == neg_double -> 
      Grnd (e, mode m, Gneg (term e t))
  | Tapp (id, [m; t1; t2], _) when id == add_double -> 
      Grnd (e, mode m, Gadd (term e t1, term e t2))
  | Tapp (id, [m; t1; t2], _) when id == sub_double -> 
      Grnd (e, mode m, Gsub (term e t1, term e t2))
  | Tapp (id, [m; t1; t2], _) when id == mul_double -> 
      Grnd (e, mode m, Gmul (term e t1, term e t2))
  | Tapp (id, [m; t1; t2], _) when id == div_double -> 
      Grnd (e, mode m, Gdiv (term e t1, term e t2))
  (* conversions *)
  | Tapp (id, [t], _) when id == real_of_int ->
      term Exact t
  | Tapp (id, [t], _) when id == d_to_r ->
      term Double t
  | Tapp (id, [t], _) when id == d_to_exact ->
      term Exact t
  | Tapp (id, [m; t], _) when id == r_to_d && e = Exact ->
      term Exact t
  | Tapp (id, [m; t], _) when id == r_to_d ->
      Grnd (Double, mode m, term Exact t)
  (* errors *)
  | Tapp (id, [t], _) when id == double_round_error ->
      Gabs (Gsub (term Double t, term Exact t))
  (* anything else is discarded *)
  | Tapp _ -> 
      raise NotGappa
  | Tnamed(_,t) -> term e t

let termo e t = try Some (term e t) with NotGappa -> None

let gando = function
  | Some p1, Some p2 -> Some (Gand (p1, p2))
  | (Some p as v), None | None, (Some p as v) -> v
  | None, None -> None

(* recognition of a Gappa predicate *)

let rec gpred = function
  | Papp (id, [t1; t2], _) when id == t_le_real ->
      begin match termo Exact t1, termo Exact t2 with
	| Some (Gcst c1), Some t2 -> Some (Gge (t2, c1))
	| Some t1, Some (Gcst c2) -> Some (Gle (t1, c2))
	| _ -> None
      end
  | Papp (id, [t1; t2], _) when id == t_ge_real ->
      begin match termo Exact t1, termo Exact t2 with
	| Some (Gcst c1), Some t2 -> Some (Gle (t2, c1))
	| Some t1, Some (Gcst c2) -> Some (Gge (t1, c2))
	| _ -> None
      end
  | Pand (_, _, Papp (id1, [f1; t1], _), Papp (id2, [t2; f2], _))
    when id1 == t_le_real && id2 == t_le_real && t1 = t2 
    && is_constant f1 && is_constant f2 ->
      begin 
	try Some (Gin (term Double t1, eval_constant f1, eval_constant f2))
	with NotGappa -> None 
      end
  | Pand (_, _, p1, p2) ->
      gando (gpred p1, gpred p2)
  | Por (p1, p2) ->
      begin match gpred p1, gpred p2 with
	| Some p1, Some p2 -> Some (Gor (p1, p2))
	| _ -> None
      end
  | Pimplies (_, p1, p2) ->
      begin match gpred p1, gpred p2 with
	| Some p1, Some p2 -> Some (Gimplies (p1, p2))
	| _ -> None
      end
  | Pnamed (_, p) ->
      gpred p
  | Forall _ 
  | Papp _
  | Pif _
  | Piff _
  | Pnot _
  | Forallb _
  | Exists _
  | Pfpi _
  | Ptrue | Pfalse | Pvar _ -> (* discarded *)
      None

let gpred p =
  (*Format.printf "gpred %a@." Util.print_predicate p;*)
  gpred p

(* extraction of a list of equalities and possibly a Gappa predicate *)

let rec ghyp = function
  | Papp (id, [Tvar x; t], _) when is_eq id ->
      let x = Ident.string x in
      begin match termo Double t, termo Exact t with
	| Some t1, Some t2 -> [x, t1; "exact_"^x, t2], None
	| _ -> [], None
      end
  | Papp (id, [Tapp (id', [Tvar x], _); t], _) 
    when is_eq id && id' == d_to_exact ->
      let x = Ident.string x in
      begin match termo Exact t with
	| Some t -> ["exact_"^x, t], None
	| None -> [], None
      end
  | Pand (_, _, p1, p2) as p ->
      begin match ghyp p1, ghyp p2 with
	| ([], _), ([], _) -> [], gpred p
	| (e1,p1), (e2, p2) -> e1 @ e2, gando (p1, p2)
      end
  | Pnamed (_, p) ->
      ghyp p
  | p ->
      [], gpred p

(* Processing obligations.
   One Why obligation can be split into several Gappa obligations *)

let queue = Queue.create ()

let reset () = Queue.clear queue

let add_ctx_vars =
  List.fold_left 
    (fun acc -> function Svar (id,_) -> Idset.add id acc | _ -> acc)

let rec intros ctx = function
  | Forall (_, id, n, t, _, p) ->
      let id' = next_away id (add_ctx_vars (predicate_vars p) ctx) in
      let p' = subst_in_predicate (subst_onev n id') p in
      intros (Svar (id', t) :: ctx) p'
  | Pimplies (_, a, b) -> 
      let h = fresh_hyp () in 
      intros (Spred (h, a) :: ctx) b
  | Pnamed (_, p) ->
      intros ctx p
  | c -> 
      ctx, c

let gands = function
  | [] -> assert false
  | p0 :: l -> List.fold_right (fun p acc -> Gand (p, acc)) l p0

let gimplies l p = match l with
  | [] -> p
  | _ -> Gimplies (gands l, p)

let equations vl el0 =
  List.fold_left
    (fun el x ->
       if List.mem_assoc x el0 then el 
       else (x, Grnd (Double, Mnearest_even, Gvar (Exact, "init_"^x))) :: el)
    (List.rev el0) vl
  
let process_obligation (ctx, concl) =
  let ctx,concl = intros ctx concl in
  match gpred concl with
    | None -> (* goal is not a gappa prop *)
	if debug then Format.eprintf "not a gappa prop; skipped@."
    | Some p ->
	let vl,el,pl = 
	  List.fold_left 
	    (fun ((vl,el,pl) as acc) h -> match h with
	       | Svar (x, PTexternal ([], id)) when id == t_double ->
		   Ident.string x :: vl, el, pl
	       | Svar _ -> 
		   acc
	       | Spred (_, p) -> 
		   let ep,pp = ghyp p in
		   let pl = match pp with None -> pl | Some pp -> pp::pl in
		   vl, ep :: el, pl)
	    ([], [],[]) ctx
	in
	let gconcl = gimplies pl p in
	let el = equations vl (List.flatten el) in
	Queue.add (el, gconcl) queue

let push_decl = function
  | Logic_decl.Dgoal (_,_,_,s) -> process_obligation s.Env.scheme_type
  | _ -> ()

let print_real fmt = function
  | (i,f,"") -> fprintf fmt "%s.%s" i f
  | (i,f,e) -> fprintf fmt "%s.%se%s" i f e

let print_constant fmt = function
  | GCdecimal r -> print_real fmt r
  | GCbinary (n,m) -> fprintf fmt "%sb%s" n m

let rec print_term fmt = function
  | Gvar (Double, x) -> fprintf fmt "%s" x
  | Gvar (Exact, x) -> fprintf fmt "exact_%s" x
  | Grnd (Exact, _, t) -> print_term fmt t
  | Grnd (Double, Mnearest_even, t) -> fprintf fmt "dne(%a)" print_term t
  | Gcst c -> print_constant fmt c
  | Gneg t -> fprintf fmt "(-%a)" print_term t
  | Gadd (t1, t2) -> fprintf fmt "(%a + %a)" print_term t1 print_term t2
  | Gsub (t1, t2) -> fprintf fmt "(%a - %a)" print_term t1 print_term t2
  | Gmul (t1, t2) -> fprintf fmt "(%a * %a)" print_term t1 print_term t2
  | Gdiv (t1, t2) -> fprintf fmt "(%a / %a)" print_term t1 print_term t2
  | Gabs t -> fprintf fmt "|%a|" print_term t

let rec print_pred fmt = function
  | Gle (t, r1) ->
      fprintf fmt "%a <= %a" print_term t print_constant r1
  | Gge (t, r1) ->
      fprintf fmt "%a >= %a" print_term t print_constant r1
  | Gin (t, r1, r2) ->
      fprintf fmt "%a in [%a, %a]" 
	print_term t print_constant r1 print_constant r2
  | Gimplies (p1, p2) ->
      fprintf fmt "(%a ->@ %a)" print_pred p1 print_pred p2
  | Gand (p1, p2) ->
      fprintf fmt "(%a /\\ %a)" print_pred p1 print_pred p2
  | Gor (p1, p2) ->
      fprintf fmt "(%a \\/ %a)" print_pred p1 print_pred p2
  | Gnot p ->
      fprintf fmt "(not %a)" print_pred p

let print_equation fmt (x, t) = 
  fprintf fmt "@[%s = %a;@]@\n" x print_term t

let print_obligation fmt (eq,p) =
  fprintf fmt "@@dne = float<ieee_64, ne>;@\n@\n";
  fprintf fmt "%a@\n" (print_list newline print_equation) eq;
  fprintf fmt "@[{ %a }@]@." print_pred p

let output_file fwe =
  let sep = "### DO NOT EDIT ABOVE THIS LINE" in
  let i = ref 0 in
  Queue.iter
    (fun o ->
       incr i;
       if debug then eprintf "gappa obligation %d@." !i;
       let file = fwe ^ "_why_po_" ^ string_of_int !i ^ ".gappa" in
       do_not_edit_above ~file
	 ~before:(fun fmt -> print_obligation fmt o)
	 ~sep
	 ~after:(fun fmt -> ()))
    queue


