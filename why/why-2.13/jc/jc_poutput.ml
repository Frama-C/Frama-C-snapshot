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

(* $Id: jc_poutput.ml,v 1.14 2008/07/11 06:35:50 moy Exp $ *)

open Format
open Jc_env
open Jc_fenv
open Jc_pervasives
open Jc_ast
open Jc_output_misc
open Pp

let bin_op = function
  | `Blt -> "<"
  | `Bgt -> ">"
  | `Ble -> "<="
  | `Bge -> ">="
  | `Beq -> "=="
  | `Bneq -> "!="
  | `Badd -> "+"
  | `Bsub -> "-"
  | `Bmul -> "*"
  | `Bdiv -> "/"
  | `Bmod -> "%"
  | `Bland -> "&&"
  | `Blor -> "||"
  | `Bimplies -> "==>"
  | `Biff -> "<==>"
  | `Bbw_and -> "&"
  | `Bbw_or -> "|"
  | `Bbw_xor -> "^"
  | `Blogical_shift_right -> ">>"
  | `Barith_shift_right -> ">>>"
  | `Bshift_left -> "<<"
  | `Bconcat -> "@"

let unary_op = function
  | `Uplus -> "+"
  | `Uminus -> "-"
  | `Unot -> "!"
  | `Upostfix_inc | `Uprefix_inc -> "++"
  | `Upostfix_dec | `Uprefix_dec -> "--"
  | `Ubw_not -> "~"

let real_conversion fmt rc =
  match rc with
    | Integer_to_real -> fprintf fmt "real"
    | Real_to_integer -> fprintf fmt "integer"

let rec ppattern fmt p =
  match p#node with
    | JCPPstruct(st, lbls) ->
	fprintf fmt "@[<hv 2>%s{@ " st#name;
	List.iter
	  (fun (lbl, pat) ->
	     fprintf fmt "%s = %a;@ " lbl#name ppattern pat)
          lbls;
	fprintf fmt "}@]"
    | JCPPvar vi ->
	fprintf fmt "%s" vi#name
    | JCPPor(p1, p2) ->
	fprintf fmt "(%a)@ | (%a)" ppattern p1 ppattern p2
    | JCPPas(pat, vi) ->
	fprintf fmt "(%a as %s)" ppattern pat vi#name
    | JCPPany ->
	fprintf fmt "_"
    | JCPPconst c ->
	fprintf fmt "%a" const c

let quantifier fmt = function
  | Forall -> fprintf fmt "forall"
  | Exists -> fprintf fmt "exists"

let rec pexpr fmt e =
  match e#node with
    | JCPElabel(lab,e) ->
	assert (lab <> "");
	fprintf fmt "@[(%s : %a)@]" lab pexpr e
    | JCPEconst c -> const fmt c
    | JCPEvar vi -> 
	fprintf fmt "%s" vi
    | JCPEbinary (e1, op, e2) ->
	fprintf fmt "@[<hv 2>(%a %s@ %a)@]" pexpr e1 (bin_op op) pexpr e2
    | JCPEunary((`Upostfix_dec | `Upostfix_inc) as op,e1) ->
	fprintf fmt "@[(%a %s)@]" pexpr e1 (unary_op op) 
    | JCPEunary(op,e1) ->
	fprintf fmt "@[(%s %a)@]" (unary_op op) pexpr e1 
    | JCPEif (e1,e2,e3) -> 
	fprintf fmt "@[(if %a then %a else %a)@]" pexpr e1 pexpr e2 pexpr e3
    | JCPElet(Some ty,vi,Some e1,e2) -> 
	fprintf fmt "@[(let %a %s =@ %a@ in %a)@]" 
          ptype ty vi pexpr e1 pexpr e2 
    | JCPElet(None,vi,Some e1,e2) -> 
	fprintf fmt "@[(let %s =@ %a@ in %a)@]" 
          vi pexpr e1 pexpr e2 
    | JCPElet(ty,vi,None,e2) -> assert false
    | JCPEassign (v, e) -> 
	fprintf fmt "(%a = %a)" pexpr v pexpr e
    | JCPEassign_op (v, op, e) -> 
	fprintf fmt "%a %s= %a" pexpr v (bin_op op) pexpr e
    | JCPEcast (e, si) ->
	fprintf fmt "(%a :> %s)" pexpr e si
    | JCPEalloc (e, si) ->
	fprintf fmt "(new %s[%a])" si pexpr e 
    | JCPEfree (e) ->
	fprintf fmt "(free(%a))" pexpr e 
    | JCPEoffset(k,e) ->
	fprintf fmt "\\offset_m%a(%a)" offset_kind k pexpr e 
    | JCPEinstanceof (e, si) ->
	fprintf fmt "(%a <: %s)" pexpr e si
    | JCPEderef (e, fi) -> 
	fprintf fmt "%a.%s" pexpr e fi
    | JCPEmatch (e, pel) ->
	fprintf fmt "@[<v 2>match %a with@ " pexpr e;
	List.iter
	  (fun (p, e) -> fprintf fmt "  @[<v 2>%a ->@ %a;@]@ "
	     ppattern p pexpr e) pel;
	fprintf fmt "end@]"
    | JCPEold t -> fprintf fmt "@[\\old(%a)@]" pexpr t
    | JCPEat(t,lab) -> fprintf fmt "@[\\at(%a,%a)@]" pexpr t label lab
    | JCPEapp(f,labs,args) ->
	fprintf fmt "%s%a(@[%a@])" f
	  (fun fmt labs -> if List.length labs = 0 then () else
	    fprintf fmt "%a" (print_list_delim lbrace rbrace comma label) labs) labs
	  (print_list comma pexpr) args 
    | JCPErange(t1,t2) -> 
	fprintf fmt "@[[%a..%a]@]" (print_option pexpr) t1 (print_option pexpr) t2
    | JCPEquantifier (q,ty,vil, a)-> 
	fprintf fmt "@[<hv 2>(\\%a %a %a;@\n%a)@]"
	  quantifier q
	  ptype ty
	  (print_list comma string) vil
	  pexpr a
    | JCPEmutable _ -> assert false (* TODO *)
    | JCPEtagequality _ -> assert false (* TODO *)
    | JCPEreturn (e) ->
	fprintf fmt "@\n(return %a)" pexpr e
    | JCPEunpack _ -> assert false (* TODO *) 
    | JCPEpack _ -> assert false (* TODO *) 
    | JCPEthrow (ei, eo) ->
	fprintf fmt "@\n(throw %s %a)" 
	  ei#name 
	  pexpr eo
    | JCPEtry (s, hl, fs) ->
	fprintf fmt 
	  "@\n@[<v 2>try %a@]%a@\n@[<v 2>finally%a@ end@]"
	  pexpr s 
	  (print_list nothing handler) hl
	  pexpr fs
    | JCPEgoto lab -> 
	fprintf fmt "@\n(goto %s)" lab
    | JCPEcontinue lab -> 
	fprintf fmt "@\n(continue %s)" lab
    | JCPEbreak lab -> 
	fprintf fmt "@\n(break %s)" lab
    | JCPEwhile (e, invariant,variant, s)-> 
	fprintf fmt "@\n@[%a%a@\nwhile (%a)%a@]"
	  (print_list nothing 
	     (fun fmt (behav,inv) -> fprintf fmt "@\ninvariant %a%a;"
		(print_list_delim 
		   (constant_string "for ") (constant_string ": ") 
		   comma string)
		behav
		pexpr inv))
	  invariant
	  (print_option (fun fmt t -> fprintf fmt "@\nvariant %a;" pexpr t))
	  variant
	  pexpr e block [s]
    | JCPEfor (inits, cond, updates, invariant,variant, body)-> 
	fprintf fmt "@\n@[invariant %a;%a@\nfor (%a ; %a ; %a)%a@]"
	  pexpr invariant 
	  (print_option (fun fmt t -> fprintf fmt "@\nvariant %a;" pexpr t))
	  variant
	  (print_list comma pexpr) inits 
	  pexpr cond (print_list comma pexpr) updates
	  block [body]
    | JCPEdecl (ty,vi, None)-> 
	fprintf fmt "@\n(var %a %s)" ptype ty vi
    | JCPEdecl (ty,vi, Some e)-> 
	fprintf fmt "@\n(var %a %s = %a)" 
	  ptype ty vi pexpr e
    | JCPEassert(behav,a)-> 
	fprintf fmt "@\n(assert %a%a)" 
	  (print_list_delim 
	     (constant_string "for ") (constant_string ": ") 
	     comma string)
	  behav
	  pexpr a
    | JCPEblock l -> block fmt l
    | JCPEswitch (e, csl) ->
	fprintf fmt "@\n@[<v 2>switch (%a) {%a@]@\n}"
	  pexpr e (print_list nothing case) csl

and handler fmt (ei,vio,s) =
  fprintf fmt "@\n@[<v 2>catch %s %s %a@]"
    ei#name vio
    pexpr s

and pexprs fmt l = print_list semi pexpr fmt l

and block fmt b =
  match b with
    | [] -> fprintf fmt "{()}"
    | _::_ ->
        fprintf fmt "@\n@[<v 0>{@[<v 2>  ";
        pexprs fmt b;
        fprintf fmt "@]@\n}@]"

and case fmt (c,sl) =
  let onecase fmt = function
    | Some c ->
	fprintf fmt "@\ncase %a:" pexpr c
    | None ->
	fprintf fmt "@\ndefault:"
  in
  fprintf fmt "%a%a" (print_list nothing onecase) c pexpr sl

let pclause fmt = function
  | JCCrequires e -> 
      fprintf fmt "@\n@[<v 2>  requires @[%a@];@]" pexpr e
  | JCCbehavior(_loc,id,throws,assumes,requires,assigns,ensures) ->
      fprintf fmt "@\n@[<v 2>behavior %s:" id;
      Option_misc.iter (fprintf fmt "@\nassumes %a;" pexpr) assumes;
      Option_misc.iter (fprintf fmt "@\nrequires %a;" pexpr) requires;
      Option_misc.iter 
	(fun id -> fprintf fmt "@\nthrows %s;" id#name) throws;
      Option_misc.iter 
	(fun (_,locs) -> fprintf fmt "@\nassigns %a;" 
	  (print_list_or_default "\\nothing" comma pexpr) locs)
	assigns;
      fprintf fmt "@\nensures %a;@]" pexpr ensures

let param fmt (ty,vi) =
  fprintf fmt "%a %s" ptype ty vi

let field fmt (rep,ty,fi) =
  fprintf fmt "@\n";
  if rep then
    fprintf fmt "rep ";
  fprintf fmt "%a %s;" 
    ptype ty fi

let invariant fmt (id, vi, a) =
  fprintf fmt "@\n@[<hv 2>invariant %s(%s) =@ %a;@]"
    id#name vi pexpr a

let reads_or_expr fmt = function
  | JCreads [] -> 
      fprintf fmt "reads \\nothing;"
  | JCreads el -> 
      fprintf fmt "reads %a;" (print_list comma pexpr) el
  | JCexpr e -> 
      fprintf fmt "=@\n%a" pexpr e

let type_params_decl fmt = function
  | [] -> ()
  | l -> fprintf fmt "<%a>" (print_list comma Pp.string) l

let type_params fmt = function
  | [] -> ()
  | l -> fprintf fmt "<%a>" (print_list comma ptype) l

let super_option fmt = function
  | None -> ()
  | Some(s, p) -> fprintf fmt "%s%a with " s type_params p

let rec pdecl fmt d =
  match d#node with
    | JCDfun(ty,id,params,clauses,body) ->
	fprintf fmt "@\n@[%a %s(@[%a@])%a%a@]@." ptype ty id#name
	  (print_list comma param) params 
	  (print_list nothing pclause) clauses
	  (print_option_or_default "\n;" pexpr) body
    | JCDenum_type(id,min,max) ->
	fprintf fmt "@\n@[type %s = %s..%s@]@."
	  id (Num.string_of_num min) (Num.string_of_num max)
    | JCDvariant_type(id, tags) ->
	fprintf fmt "@\n@[type %s = [" id;
	print_list
	  (fun fmt () -> fprintf fmt " | ")
	  (fun fmt tag -> fprintf fmt "%s" tag#name)
	  fmt tags;
	fprintf fmt "]@]@."
    | JCDunion_type(id, tags) ->
	fprintf fmt "@\n@[type %s = [" id;
	print_list
	  (fun fmt () -> fprintf fmt " & ")
	  (fun fmt tag -> fprintf fmt "%s" tag#name)
	  fmt tags;
	fprintf fmt "]@]@."
    | JCDtag(id, params, super, fields, invs) ->
	fprintf fmt "@\n@[<v 2>tag %s%a = %a{%a%a@]@\n}@."
          id
          type_params_decl params
          super_option super
          (print_list space field) fields 
	  (print_list space invariant) invs
    | JCDvar(ty,id,init) ->
	fprintf fmt "@\n@[%a %s%a;@]@." ptype ty id
	  (print_option (fun fmt e -> fprintf fmt " = %a" pexpr e)) init
    | JCDlemma(id,is_axiom,lab,a) ->
	fprintf fmt "@\n@[%s %s" (if is_axiom then "axiom" else "lemma") id;
	if lab <> [] then 
	  fprintf fmt "%a" 
	    (print_list_delim lbrace rbrace comma label) lab;
	fprintf fmt " :@\n%a@]@." pexpr a
    | JCDglobal_inv(id,a) ->
	fprintf fmt "@\n@[invariant %s :@\n%a@]@." id pexpr a
    | JCDexception(id,tyopt) ->
	fprintf fmt "@\n@[exception %s of %a@]@." id
	  (print_option ptype) tyopt
    | JCDlogic_var (ty, id, body) ->
	fprintf fmt "@\n@[logic %a %s %a@]@." 
	  ptype ty id
	  (print_option (fun fmt e -> fprintf fmt "=@\n%a" pexpr e)) body 
    | JCDlogic (None, id, labels, params, body) ->
	fprintf fmt "@\n@[logic %s%a(@[%a@]) %a@]@." 
	  id (print_list_delim lbrace rbrace comma label) labels
	  (print_list comma param) params
	  reads_or_expr body 
    | JCDlogic (Some ty, id, labels, params, body) ->
	fprintf fmt "@\n@[logic %a %s%a(@[%a@]) %a@]@." 
	  ptype ty 
	  id (print_list_delim lbrace rbrace comma label) labels
	  (print_list comma param) params
	  reads_or_expr body 
    | JCDlogic_type id ->
	fprintf fmt "@\n@[logic type %s@]@." id
    | JCDinvariant_policy p ->
        fprintf fmt "# InvariantPolicy = %s@\n" (string_of_invariant_policy p)  
    | JCDseparation_policy p ->
        fprintf fmt "# SeparationPolicy = %s@\n" (string_of_separation_policy p)
    | JCDannotation_policy p ->
        fprintf fmt "# AnnotationPolicy = %s@\n" (string_of_annotation_policy p)
    | JCDabstract_domain p ->
        fprintf fmt "# AbstractDomain = %s@\n" (string_of_abstract_domain p)
    | JCDint_model p ->
        fprintf fmt "# IntModel = %s@\n" (string_of_int_model p)
   
let rec pdecls fmt d =
  match d with
    | [] -> ()
    | d::r -> pdecl fmt d; pdecls fmt r

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte bin/krakatoa.byte"
End: 
*)
