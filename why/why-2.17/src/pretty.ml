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

(*i $Id: pretty.ml,v 1.38 2008/11/10 17:47:20 moy Exp $ i*)

open Format
open Pp
open Ident
open Options
open Misc
open Logic
open Logic_decl

let queue = Queue.create ()

let reset () = Queue.clear queue

let push_decl ?(ergo=false) d = 
  if ergo
  then
    match d with
      | Dinductive_def (loc, id, d) ->
	  List.iter
	    (fun d -> Queue.add d queue)
            (PredDefExpansor.inductive_def loc id d)
      | _ -> Queue.add d queue
  else Queue.add d queue

let iter f = Queue.iter f queue

(*
let reset = Encoding.reset
let push_decl = Encoding.push
let iter = Encoding.iter
*)

let ident = Ident.print

let type_vars = Hashtbl.create 17

let type_var fmt n =
  try
    fprintf fmt "'a%d" (Hashtbl.find type_vars n)
  with Not_found -> 
    assert false

let specialize s =
  Hashtbl.clear type_vars;
  let n = ref 0 in
  Env.Vset.iter 
    (fun tv -> incr n; Hashtbl.add type_vars tv.tag !n) s.Env.scheme_vars;
  s.Env.scheme_type

let rec pure_type fmt = function
  | PTint -> fprintf fmt "int"
  | PTbool -> fprintf fmt "bool"
  | PTunit -> fprintf fmt "unit"
  | PTreal -> fprintf fmt "real"
  | PTexternal ([],id) -> fprintf fmt "%a" ident id
  | PTvar {tag=t; type_val=None} -> type_var fmt t
  | PTvar {tag=t; type_val=Some pt} -> pure_type fmt pt
  | PTexternal ([t],id) -> 
      fprintf fmt "%a %a" pure_type t ident id
  | PTexternal (l,id) -> fprintf fmt "(%a) %a" 
      (print_list comma pure_type) l ident id

let rec term fmt = function
  | Tconst (ConstInt n) -> 
      fprintf fmt "%s" n
  | Tconst (ConstBool b) -> 
      fprintf fmt "%b" b
  | Tconst ConstUnit -> 
      fprintf fmt "void" 
  | Tconst (ConstFloat (i,f,"")) -> 
      fprintf fmt "%s.%s" i f
  | Tconst (ConstFloat (i,f,e)) -> 
      fprintf fmt "%s.%se%s" i f e
  | Tvar id | Tderef id | Tapp (id, [], _) -> 
      ident fmt id
  | Tapp (id, [t1; t2], _) when id == t_add_int || id == t_add_real ->
      fprintf fmt "(%a + %a)" term t1 term t2
  | Tapp (id, [t1; t2], _) when id == t_sub_int || id == t_sub_real ->
      fprintf fmt "(%a - %a)" term t1 term t2
  | Tapp (id, [t1; t2], _) when id == t_mul_int || id == t_mul_real ->
      fprintf fmt "(%a * %a)" term t1 term t2
  | Tapp (id, [t1; t2], _) when id == t_div_int || id == t_div_real ->
      fprintf fmt "(%a / %a)" term t1 term t2
  | Tapp (id, [t1; t2], _) when id == t_mod_int ->
      fprintf fmt "(%a %% %a)" term t1 term t2
  | Tapp (id, [t1], _) when id == t_neg_int ->
      fprintf fmt "(-%a)" term t1
  | Tapp (id, tl, _) -> 
      fprintf fmt "%a(%a)" ident id (print_list comma term) tl
  | Tnamed (User n, t) ->
      fprintf fmt "@[(%S:@ %a)@]" n term t
  | Tnamed (_, t) -> term fmt t

let int_relation_string id =
  if id == t_lt_int then "<" 
  else if id == t_le_int then "<="
  else if id == t_gt_int then ">"
  else if id == t_ge_int then ">="
  else assert false

let real_relation_string id =
  if id == t_lt_real then "<" 
  else if id == t_le_real then "<="
  else if id == t_gt_real then ">"
  else if id == t_ge_real then ">="
  else assert false

let rec predicate fmt = function
  | Pvar id | Papp (id, [], _) -> 
      ident fmt id
  | Papp (id, [t1; t2], _) when is_eq id ->
      fprintf fmt "(%a = %a)" term t1 term t2
  | Papp (id, [t1; t2], _) when is_neq id ->
      fprintf fmt "(%a <> %a)" term t1 term t2
  | Papp (id, [t1; t2], _) when is_int_comparison id ->
      fprintf fmt "(%a %s %a)" term t1 (int_relation_string id) term t2
  | Papp (id, [t1; t2], _) when is_real_comparison id ->
      fprintf fmt "(%a %s %a)" term t1 (real_relation_string id) term t2
  | Papp (id, [a;b], _) when id == t_zwf_zero ->
      fprintf fmt "@[((0 <= %a) and@ (%a < %a))@]" term b term a term b
  | Papp (id, [t], _) when id == well_founded ->
      fprintf fmt "@[true (* was well founded *)@]" 
  | Papp (id, l, _) ->
      fprintf fmt "%s(%a)" (Ident.string id) (print_list comma term) l
  | Ptrue ->
      fprintf fmt "true"
  | Pfalse ->
      fprintf fmt "false"
  | Pimplies (_, a, b) -> 
      fprintf fmt "(@[%a ->@ %a@])" predicate a predicate b
  | Piff (a, b) -> 
      fprintf fmt "(@[%a <->@ %a@])" predicate a predicate b
  | Pif (a, b, c) -> 
      fprintf fmt "(@[if %a then@ %a else@ %a@])" 
	term a predicate b predicate c
  | Pand (_, _, a, b) ->
      fprintf fmt "(@[%a and@ %a@])" predicate a predicate b
  | Forallb (_, ptrue, pfalse) ->
      fprintf fmt "(@[forallb(%a,@ %a)@])" 
	predicate ptrue predicate pfalse
  | Por (a, b) ->
      fprintf fmt "(@[%a or@ %a@])" predicate a predicate b
  | Pnot a ->
      fprintf fmt "(not %a)" predicate a
  | Forall (_,id,n,v,tl,p) ->
      let id = next_away id (predicate_vars p) in
      let s = subst_onev n id in
      let p = subst_in_predicate s p in
      let tl = List.map (List.map (subst_in_pattern s)) tl in
      fprintf fmt "@[<hov 2>(forall %a:%a%a.@ %a)@]"
	ident id pure_type v print_triggers tl predicate p
  | Exists (id,n,v,p) ->
      let id = next_away id (predicate_vars p) in
      let p = subst_in_predicate (subst_onev n id) p in
      fprintf fmt "@[<hov 2>(exists %a:%a.@ %a)@]" 
	ident id pure_type v predicate p
  | Pfpi (t, (i1,f1,e1), (i2,f2,e2)) ->
      fprintf fmt "@[<hov 2>fpi(%a,@ %s.%se%s,@ %s.%se%s)@]" 
	term t i1 f1 e1 i2 f2 e2
  | Pnamed (User n, p) ->
      fprintf fmt "@[(%S:@ %a)@]" n predicate p
  | Pnamed (_, p) -> predicate fmt p

and print_pattern fmt = function
  | TPat t -> term fmt t
  | PPat p -> predicate fmt p

and print_triggers fmt = function
  | [] -> 
      ()
  | tl -> 
      fprintf fmt " [%a]" (print_list alt (print_list comma print_pattern)) tl

let sequent fmt (ctx, p) =
  let context fmt = function
    | Cc.Svar (id, pt) ->
	fprintf fmt "forall %a:%a." ident id pure_type pt
    | Cc.Spred (_, p) ->
	fprintf fmt "@[%a ->@]" predicate p
  in
  print_list newline context fmt ctx;
  if ctx <> [] then fprintf fmt "@\n";
  predicate fmt p

let logic_binder fmt (id, pt) =
  fprintf fmt "%a: %a" ident id pure_type pt

let logic_type fmt = function
  | Predicate ptl -> 
      fprintf fmt "%a -> prop" (print_list comma pure_type) ptl
  | Function (ptl, pt) -> 
      fprintf fmt "%a -> %a" (print_list comma pure_type) ptl pure_type pt

let type_parameters fmt l = 
  let type_var fmt id = fprintf fmt "'%s" id in
  match l with
  | [] -> ()
  | [id] -> fprintf fmt "%a " type_var id
  | l -> fprintf fmt "(%a) " (print_list comma type_var) l

let decl fmt d = 
  match d with
  | Dtype (_, pl, id) ->
      fprintf fmt "@[type %a%s@]" type_parameters pl id
  | Dlogic (_, id, lt) ->
      let lt = specialize lt in
      fprintf fmt "@[logic %s : %a@]" id logic_type lt
  | Dpredicate_def (_, id, def) ->
      let bl,p = specialize def in
      fprintf fmt "@[<hov 2>predicate %a(%a) =@ %a@]" ident id 
	(print_list comma logic_binder) bl predicate p
  | Dinductive_def (_, id, indcases) ->
      let bl,l = specialize indcases in
      fprintf fmt 
	"@[<hov 0>predicate %a: @[%a -> prop@] {@\n  @[<v 0>%a@]@\n}@\n@]" 
	ident id 
	(print_list comma pure_type) bl 
	(print_list newline 
	   (fun fmt (id,p) -> fprintf fmt "@[%a: %a;@]" ident id predicate p)) l
  | Dfunction_def (_, id, def) ->
      let bl,pt,t = specialize def in
      fprintf fmt "@[<hov 2>function %a(%a) : %a =@ %a@]" ident id
	(print_list comma logic_binder) bl pure_type pt term t
  | Daxiom (_, id, p) ->
      let p = specialize p in
      fprintf fmt "@[<hov 2>axiom %s:@ %a@]" id predicate p
  | Dgoal (_, expl, id, sq) ->
      let sq = specialize sq in
      fprintf fmt "@[<hov 2>goal %s:@\n%a@]" id sequent sq

let decl fmt d = fprintf fmt "@[%a@]@\n@\n" decl d

let print_file fmt = iter (decl fmt) 

let print_trace fmt id expl =
  fprintf fmt "[%s]@\n" id;
  Explain.print fmt expl;
  fprintf fmt "@\n"

let print_traces fmt =
  Queue.iter
    (function
       | Dgoal (loc, expl, id, _) -> print_trace fmt id ((*loc,*)expl)
       | _ -> ())
    queue

let output_file f =
  print_in_file print_file (f ^ "_why.why");
  if explain_vc then print_in_file print_traces (f ^ "_why.xpl")

let output_files f =
  let po = ref 0 in
  print_in_file
    (fun ctxfmt ->
       Queue.iter 
	 (function 
	    | Dgoal (loc,expl,id,_) as d -> 
		incr po;		
		let fpo = f ^ "_po" ^ string_of_int !po ^ ".why" in
		print_in_file (fun fmt -> decl fmt d) fpo;
		if explain_vc then
		  let ftr = f ^ "_po" ^ string_of_int !po ^ ".xpl" in
		  print_in_file (fun fmt -> print_trace fmt id ((*loc,*)expl)) ftr
	    | d -> 
		decl ctxfmt d)
	 queue)
    (f ^ "_ctx.why")

let push_or_output_decl = 
  let po = ref 0 in 
  function d ->
    match d with
      | Dgoal (loc,expl,id,_) as d -> 
	  incr po;
	  let fpo = Options.out_file (id ^ ".why") in
	  print_in_file (fun fmt -> decl fmt d) fpo;
	  if explain_vc then
	    let ftr = Options.out_file (id ^ ".xpl") in
	    print_in_file (fun fmt -> print_trace fmt id ((*loc,*)expl)) ftr
      | d -> push_decl d

module SMap = Map.Make(String)

let output_project f =
  let po = ref 0 in
  let lemmas = ref [] in
  let functions = ref SMap.empty in
  print_in_file
    (fun ctxfmt ->
       Queue.iter 
	 (function 
	    | Dgoal (_,e,id,_) as d -> 
		incr po;
		let fpo = f ^ "_po" ^ string_of_int !po ^ ".why" in
		print_in_file (fun fmt -> decl fmt d) fpo;
		begin
		  match e.vc_kind with
		    | EKLemma -> 
			lemmas := (e,fpo) :: !lemmas;
		    | _ ->
			let fn = e.lemma_or_fun_name in
			let behs =
			  try SMap.find fn !functions
			  with Not_found -> SMap.empty
			in
			let vcs =
			  try SMap.find e.behavior behs 
			  with Not_found -> []
			in
			let behs =
			  SMap.add e.behavior ((e,fpo)::vcs) behs
			in
			functions := SMap.add fn behs !functions;
		end
	    | d -> 
		decl ctxfmt d)
	 queue)
    (f ^ "_ctx.why");
  Hashtbl.iter 
    (fun key (fn,beh,loc) -> 
       try
	 let _ = SMap.find fn !functions in ()
       with Not_found ->
	 functions := SMap.add fn SMap.empty !functions)  
    Util.program_locs ;
  let p = Project.create (Filename.basename f) in
  Project.set_project_context_file p (f ^ "_ctx.why");      
  List.iter
    (fun (expl,fpo) ->      
       let n = expl.lemma_or_fun_name in
       let _ = Project.add_lemma p n expl fpo in ())
    !lemmas;
  SMap.iter
    (fun fname behs ->
       let floc =
	 try 
	   let (_,_,floc) = Hashtbl.find Util.program_locs fname in
	   floc
	 with Not_found -> Loc.dummy_floc
       in
       let f = Project.add_function p fname floc in
       SMap.iter
	 (fun beh vcs ->
	    let be = Project.add_behavior f beh floc in
	    List.iter
	      (fun (expl,fpo) ->
		 let _ = Project.add_goal be expl fpo in ())
	      vcs)
	 behs)
    !functions;
  Project.save p f;
  p
