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

(* $Id: jc_output.ml,v 1.105 2008/04/15 13:09:53 moy Exp $ *)

open Format
open Jc_env
open Jc_fenv
open Jc_pervasives
open Jc_constructors
open Jc_output_misc
open Jc_ast
open Pp

type jc_decl =
  | JCfun_def of jc_type * string * var_info list *
      fun_spec * expr option
  | JCenum_type_def of string * Num.num * Num.num
  | JCvariant_type_def of string * string list
  | JCunion_type_def of string * string list
  | JCstruct_def of string * string option * field_info list *
      (string * var_info * assertion) list
  | JCrec_struct_defs of jc_decl list
      (* deprecated, all tag definitions of a file are mutually recursive *)
  | JCrec_fun_defs of jc_decl list
  | JCvar_def of jc_type * string * expr option
  | JClemma_def of string * bool * logic_label list * assertion
  | JClogic_fun_def of jc_type option * string * logic_label list 
      * var_info list * term_or_assertion      
  | JCexception_def of string * exception_info
  | JCglobinv_def of string * assertion
  | JClogic_const_def of jc_type * string * term option
  | JClogic_type_def of string
  | JCinvariant_policy of Jc_env.inv_sem
  | JCseparation_policy of Jc_env.separation_sem
  | JCannotation_policy of Jc_env.annotation_sem
  | JCabstract_domain of Jc_env.abstract_domain 
  | JCint_model of Jc_env.int_model      

(*
let lbin_op op =
  if op == Jc_pervasives.ge_int then ">=" else
  if op == Jc_pervasives.le_int then "<=" else
  if op == Jc_pervasives.gt_int then ">" else
  if op == Jc_pervasives.lt_int then "<" else
  if op == Jc_pervasives.eq then "==" else
  if op == Jc_pervasives.neq then "!=" else
  if op == Jc_pervasives.add_int then "+" else
  if op == Jc_pervasives.sub_int then "-" else
  if op == Jc_pervasives.mul_int then "*" else
  if op == Jc_pervasives.div_int then "/" else
  if op == Jc_pervasives.mod_int then "%" else
  if op == Jc_pervasives.shift then "+" else
  raise Not_found
*)

let bin_op (op, _) = match op with
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

let unary_op (op, _) = match op with
  | `Uminus -> "-"
  | `Unot -> "!"
  | `Ubw_not -> "~"

let real_conversion = Jc_poutput.real_conversion

let rec pattern fmt p =
  match p#node with
    | JCPstruct(st, lbls) ->
	fprintf fmt "@[<hv 2>%s{@ " st.jc_struct_info_name;
	List.iter
	  (fun (lbl, pat) ->
	     fprintf fmt "%s = %a;@ " lbl.jc_field_info_name pattern pat)
	  lbls;
	fprintf fmt "}@]"
    | JCPvar vi ->
	fprintf fmt "%s" vi.jc_var_info_name
    | JCPor(p1, p2) ->
	fprintf fmt "(%a)@ | (%a)" pattern p1 pattern p2
    | JCPas(pat, vi) ->
	fprintf fmt "(%a as %s)" pattern pat vi.jc_var_info_name
    | JCPany ->
	fprintf fmt "_"
    | JCPconst c ->
	fprintf fmt "%a" const c

let rec term fmt t =
  if t#name_label <> "" then
    fprintf fmt "@[(%s : %a)@]" 
      t#name_label term (new term_with ~name_label:"" t)
  else
  match t#node with
    | JCTvar vi -> fprintf fmt "%s" vi.jc_var_info_name
    | JCTbinary(t1,op,t2) ->
	fprintf fmt "@[(%a %s %a)@]" term t1 (bin_op op) term t2
    | JCTunary(op,t1) ->
	fprintf fmt "@[(%s %a)@]" (unary_op op) term t1
    | JCTif (t1,t2,t3) -> 
	fprintf fmt "@[(%a ? %a : %a)@]" term t1 term t2 term t3
    | JCTcast (t, _, si) ->
	fprintf fmt "(%a :> %s)" term t si.jc_struct_info_name
    | JCTrange_cast (t, ei) ->
	fprintf fmt "(%a :> %s)" term t ei.jc_enum_info_name
    | JCTreal_cast (t, rc) ->
	fprintf fmt "(%a :> %a)" term t real_conversion rc
    | JCTinstanceof (t, _, si) ->
	fprintf fmt "(%a <: %s)" term t si.jc_struct_info_name
    | JCToffset (k,t,_)->
	fprintf fmt "@[\\offset_m%a(%a)@]" offset_kind k term t
    | JCTold t -> fprintf fmt "@[\\old(%a)@]" term t
    | JCTat(t,lab) -> fprintf fmt "@[\\at(%a,%a)@]" term t label lab
(*
    | JCTapp app when List.length app.jc_app_args = 1 ->
	let op = app.jc_app_fun in
	let t1 = List.hd app.jc_app_args in
	begin try 
	  ignore 
	    (Hashtbl.find Jc_typing.enum_conversion_logic_functions_table op);
	  (* conversion due to enumeration. Ignore it. *)
	  term fmt t1
	with Not_found ->
	  fprintf fmt "%s(@[%a@])" op.jc_logic_info_name term t1
	end
    | JCTapp app when List.length app.jc_app_args = 2 ->
	let op = app.jc_app_fun in
	let l = app.jc_app_args in
	  begin try
	  let s = lbin_op op in
	  fprintf fmt "@[(%a %s %a)@]" term t1 s term t2
	with Not_found ->
	  fprintf fmt "@[%s(%a)@]" op.jc_logic_info_name
	    (print_list comma term) l 
	end
*)
    | JCTapp app ->
	let op = app.jc_app_fun and l = app.jc_app_args in
	fprintf fmt "%s(@[%a@])" op.jc_logic_info_name
	  (print_list comma term) l 
    | JCTderef (t, _label, fi)-> 
	fprintf fmt "@[%a.%s@]" term t fi.jc_field_info_name	
    | JCTshift (t1, t2) -> 
	fprintf fmt "@[(%a + %a)@]" term t1 term t2
    | JCTconst c -> const fmt c
    | JCTrange (t1,t2) -> 
	fprintf fmt "@[[%a..%a]@]" (print_option term) t1 (print_option term) t2
    | JCTmatch (t, ptl) ->
	fprintf fmt "@[<v 2>match %a with@ " term t;
	List.iter
	  (fun (p, t) -> fprintf fmt "  @[<v 2>%a ->@ %a;@]@ "
	     pattern p term t) ptl;
	fprintf fmt "end@]"

let quantifier = Jc_poutput.quantifier

let rec assertion fmt a =
  if a#name_label <> "" then
    fprintf fmt "@[(%s : %a)@]" 
      (a#name_label) assertion
      (new assertion_with ~name_label:"" a)
  else
  match a#node with
    | JCAtrue -> fprintf fmt "true"
    | JCAif (t, a1, a2) ->
	fprintf fmt "@[(%a ? %a : %a)@]" term t assertion a1 assertion a2
    | JCAbool_term t -> term fmt t
    | JCAinstanceof (t, _lab, st) ->
	fprintf fmt "(%a <: %s)" term t st.jc_struct_info_name
    | JCAold a -> fprintf fmt "\\old(%a)" assertion a
    | JCAat(a,lab) -> fprintf fmt "\\at(%a,%a)" assertion a label lab
    | JCAquantifier (q,vi, a)-> 
	fprintf fmt "@[<v 3>(\\%a %a %s;@\n%a)@]"
	  quantifier q
	  print_type vi.jc_var_info_type
	  vi.jc_var_info_name
	  assertion a
    | JCArelation (t1, op, t2) ->
	fprintf fmt "@[(%a %s %a)@]" term t1 (bin_op op) term t2
(*
    | JCAapp (op, ([t1;t2] as l)) ->
	begin
	  try
	    let s = lbin_op op in
	    fprintf fmt "@[(%a %s %a)@]" term t1 s term t2
	  with Not_found ->
	    fprintf fmt "@[%s(%a)@]" op.jc_logic_info_name
	      (print_list comma term) l 
	end
*)
    | JCAapp app ->
	 fprintf fmt "@[%s(%a)@]" app.jc_app_fun.jc_logic_info_name
	      (print_list comma term) app.jc_app_args 
    | JCAnot a1 ->
	fprintf fmt "@[(!@ %a)@]" assertion a1
    | JCAiff (a1, a2)-> 
	fprintf fmt "@[(%a <==>@ %a)@]" assertion a1 assertion a2
    | JCAimplies (a1, a2)-> 
	fprintf fmt "@[(%a ==>@ %a)@]" assertion a1 assertion a2
    | JCAor [] -> assert false
    | JCAor (a::l) -> 
	fprintf fmt "@[(%a" assertion a;
	List.iter
	  (fun a -> fprintf fmt " ||@ %a" assertion a)
	  l;
	fprintf fmt ")@]"
    | JCAand [] -> assert false
    | JCAand (a::l) -> 
	fprintf fmt "@[(%a" assertion a;
	List.iter
	  (fun a -> fprintf fmt " &&@ %a" assertion a)
	  l;
	fprintf fmt ")@]"
    | JCAfalse -> fprintf fmt "false"
    | JCAmutable _ -> assert false (* TODO *)
    | JCAtagequality _ -> assert false (* TODO *)
    | JCAmatch (t, pal) ->
	fprintf fmt "@[<v 2>match %a with@ " term t;
	List.iter
	  (fun (p, a) -> fprintf fmt "  @[<v 2>%a ->@ %a;@]@ "
	     pattern p assertion a) pal;
	fprintf fmt "end@]"

let rec location_set fmt = function
  | JCLSvar vi-> 
      fprintf fmt "%s" vi.jc_var_info_name
  | JCLSderef (locset, _, fi, _) ->
      fprintf fmt "%a.%s" location_set locset fi.jc_field_info_name
  | JCLSrange (locset, t1, t2) ->
      fprintf fmt "(%a + [%a..%a])" location_set locset 
	(print_option term) t1 (print_option term) t2

let rec location fmt = function
  | JCLvar vi -> 
      fprintf fmt "%s" vi.jc_var_info_name
  | JCLderef (locset, _, fi,_) ->
      fprintf fmt "%a.%s" location_set locset fi.jc_field_info_name
  | JCLat (loc, lab) ->
      fprintf fmt "\\at(%a,%a)" location loc label lab

let behavior fmt (_loc,id,b) =
  fprintf fmt "@\n@[<v 2>behavior %s:" id;
  Option_misc.iter
    (fun a -> fprintf fmt "@\nassumes %a;" assertion a) 
    b.jc_behavior_assumes;
  Option_misc.iter
  (fun a -> fprintf fmt "@\nthrows %s;" a.jc_exception_info_name) 
    b.jc_behavior_throws;    
  Option_misc.iter 
    (fun (_,locs) -> fprintf fmt "@\nassigns %a;" 
       (print_list_or_default "\\nothing" comma location) locs)
    b.jc_behavior_assigns;
  fprintf fmt "@\nensures %a;@]" assertion b.jc_behavior_ensures
  
let print_spec fmt s =
  fprintf fmt "@\n@[<v 2>  requires @[%a@];" assertion s.jc_fun_requires;
  List.iter (behavior fmt) s.jc_fun_behavior;
  fprintf fmt "@]"

let call_bin_op _op =
(*
  if op == Jc_pervasives.shift_ then "+" else
*)
  raise Not_found

let rec expr fmt e =
  if e#name_label <> "" then
    fprintf fmt "@[(%s : %a)@]" 
      e#name_label expr (new expr_with ~name_label:"" e)
  else
    match e#node with
      | JCEconst c -> const fmt c
      | JCEvar vi -> 
	  fprintf fmt "%s" vi.jc_var_info_name
      | JCEderef(e, fi) -> 
	  fprintf fmt "%a.%s" expr e fi.jc_field_info_name 
      | JCEbinary(e1, op, e2) ->
	  fprintf fmt "@[(%a %s %a)@]" expr e1 (bin_op op) expr e2
      | JCEunary(op,e1) ->
	  fprintf fmt "@[(%s %a)@]" (unary_op op) expr e1
      | JCEassign_var(v, e) -> 
	  fprintf fmt "(%s = %a)" v.jc_var_info_name expr e
      | JCEassign_heap(e1, fi, e2) -> 
	  fprintf fmt "%a.%s = %a" expr e1 fi.jc_field_info_name expr e2
      | JCEinstanceof(e, si) ->
	  fprintf fmt "(%a <: %s)" expr e si.jc_struct_info_name
      | JCEcast(e, si) ->
	  fprintf fmt "(%a :> %s)" expr e si.jc_struct_info_name
      | JCErange_cast(e, ri) ->
	  fprintf fmt "(%a :> %s)" expr e ri.jc_enum_info_name
      | JCEreal_cast(e, rc) ->
	  fprintf fmt "(%a :> %a)" expr e real_conversion rc
      | JCEif(e1,e2,e3) -> 
	  fprintf fmt "@[(%a ? %a : %a)@]" expr e1 expr e2 expr e3
      | JCEoffset(k,e, _) ->
	  fprintf fmt "\\offset_m%a(%a)" offset_kind k expr e
      | JCEalloc(e, si) ->
	  fprintf fmt "(new %s[%a])" si.jc_struct_info_name expr e
      | JCEfree e ->
	  fprintf fmt "(free(%a))" expr e
      | JCElet(vi,Some e1,e2) -> 
	  fprintf fmt "@[(let %s =@ %a@ in %a)@]" 
	    vi.jc_var_info_name expr e1 expr e2
      | JCElet(vi,None,e2) -> 
	  fprintf fmt "@[%a %s; %a@]"  
	    print_type vi.jc_var_info_type vi.jc_var_info_name expr e2
      | JCEassert a -> 
	  fprintf fmt "@\nassert %a;" assertion a
      | JCEblock l ->
          block fmt l
      | JCEreturn_void ->
	  fprintf fmt "@\nreturn;"
      | JCEreturn(_, e) ->
	  fprintf fmt "@\nreturn %a;" expr e
      | JCEtry(s, hl, fs) ->
	  fprintf fmt
	    "@\n@[<v 2>try %a@]%a@\n@[<v 2>finally%a@]"
	    expr s
	    (print_list nothing handler) hl
	    expr fs
      | JCEthrow(ei, eo) ->
	  fprintf fmt "@\nthrow %s %a;"
	    ei.jc_exception_info_name
	    (print_option_or_default "()" expr) eo
      | JCEpack _ | JCEunpack _ ->
          assert false (* TODO *)
      | JCEmatch(e, pel) ->
	  fprintf fmt "@[<v 2>match %a with@ " expr e;
	  List.iter
	    (fun (p, e) -> fprintf fmt "  @[<v 2>%a ->@ %a;@]@ "
	       pattern p expr e) pel;
	  fprintf fmt "end@]"
      | JCEshift(e1, e2) -> 
	  fprintf fmt "@[(%a + %a)@]" expr e1 expr e2
      | JCEloop(la, e) ->
          fprintf fmt "@\n@[invariant %a; %a@\nwhile (true)%a@]"
            assertion la.jc_loop_invariant
            (print_option (fun fmt t -> fprintf fmt "@\nvariant %a;" term t))
            la.jc_loop_variant
            expr e
      | JCEapp{ jc_call_fun = (JClogic_fun{ jc_logic_info_final_name = name }
                | JCfun{ jc_fun_info_final_name = name });
                jc_call_args = args } ->
          fprintf fmt "@[%s(%a)@]" name
            (print_list comma expr) args
	
and case fmt (c,sl) =
  let onecase fmt = function
    | Some c ->
	fprintf fmt "@\ncase %a:" expr c
    | None ->
	fprintf fmt "@\ndefault:"
  in
  fprintf fmt "%a%a" (print_list nothing onecase) c block sl

and handler fmt (ei,vio,s) =
  fprintf fmt "@\n@[<v 2>catch %s %a %a@]"
    ei.jc_exception_info_name 
    (print_option_or_default "__dummy"
      (fun fmt vi -> fprintf fmt "%s" vi.jc_var_info_name)) vio
    expr s

and statements fmt l = List.iter (expr fmt) l

and block fmt b =
  fprintf fmt "@\n@[<v 0>{@[<v 2>  ";
  statements fmt b;
  fprintf fmt "@]@\n}@]"


let param fmt vi =
  fprintf fmt "%a %s" print_type vi.jc_var_info_type vi.jc_var_info_name

let field fmt fi =
  fprintf fmt "@\n";
  if fi.jc_field_info_rep then
    fprintf fmt "rep ";
  fprintf fmt "%a %s;" 
    print_type fi.jc_field_info_type fi.jc_field_info_name

let invariant fmt (id, vi, a) =
  fprintf fmt "@\ninvariant %s(%s) = %a;"
    id vi.jc_var_info_name assertion a

let term_or_assertion fmt = function
  | JCAssertion a -> 
      fprintf fmt "=@\n%a" assertion a
  | JCTerm t ->
      fprintf fmt "=@\n%a" term t
  | JCReads [] -> 
      fprintf fmt "reads \\nothing;"
  | JCReads locs -> 
      fprintf fmt "reads %a;" (print_list comma location) locs

let print_super fmt = function
  | None -> ()
  | Some id -> fprintf fmt "%s with " id

let string_of_invariant_policy p =
  match p with
    | Jc_env.InvNone -> "None"
    | Jc_env.InvArguments -> "Arguments"
    | Jc_env.InvOwnership -> "Ownership"

let string_of_separation_policy p =
  match p with
    | Jc_env.SepNone -> "None"
    | Jc_env.SepRegions -> "Regions"

let string_of_annotation_policy p =
  match p with
    | Jc_env.AnnotNone -> "None"
    | Jc_env.AnnotInvariants -> "Invariants"
    | Jc_env.AnnotWeakPre -> "WeakPre"
    | Jc_env.AnnotStrongPre -> "StrongPre"

let string_of_abstract_domain p =
  match p with
    | Jc_env.AbsNone -> "None"
    | Jc_env.AbsBox -> "Box"
    | Jc_env.AbsOct -> "Oct"
    | Jc_env.AbsPol -> "Pol"

let string_of_int_model p =
  match p with
    | Jc_env.IMbounded -> "bounded"
    | Jc_env.IMmodulo -> "modulo"

let rec print_decl fmt d =
  match d with
    | JCinvariant_policy p ->
        fprintf fmt "# InvariantPolicy = %s@\n" (string_of_invariant_policy p)  
    | JCseparation_policy p ->
        fprintf fmt "# SeparationPolicy = %s@\n" (string_of_separation_policy p)
    | JCannotation_policy p ->
        fprintf fmt "# AnnotationPolicy = %s@\n" (string_of_annotation_policy p)
    | JCabstract_domain p ->
        fprintf fmt "# AbstractDomain = %s@\n" (string_of_abstract_domain p)
    | JCint_model p ->
        fprintf fmt "# IntModel = %s@\n" (string_of_int_model p)
    | JCfun_def(ty,id,params,spec,body) ->
	fprintf fmt "@\n@[%a %s(@[%a@])%a%a@]@." print_type ty id
	  (print_list comma param) params 
	  print_spec spec 
	  (print_option_or_default "\n;" expr) body
    | JCenum_type_def(id,min,max) ->
	fprintf fmt "@\n@[type %s = %s..%s@]@."
	  id (Num.string_of_num min) (Num.string_of_num max)
    | JCvariant_type_def(id, tags) ->
	fprintf fmt "@\n@[type %s = [" id;
	print_list
	  (fun fmt () -> fprintf fmt " | ")
	  (fun fmt tag -> fprintf fmt "%s" tag)
	  fmt tags;
	fprintf fmt "]@]@."
    | JCunion_type_def(id, tags) ->
	fprintf fmt "@\n@[type %s = [" id;
	print_list
	  (fun fmt () -> fprintf fmt " & ")
	  (fun fmt tag -> fprintf fmt "%s" tag)
	  fmt tags;
	fprintf fmt "]@]@."
    | JCstruct_def (id, extends, fields, invs) ->
	fprintf fmt "@\n@[<v 2>tag %s = %a{%a%a@]@\n}@."
	  id print_super extends (print_list space field) fields 
	  (print_list space invariant) invs
    | JCrec_struct_defs dlist | JCrec_fun_defs dlist ->
	print_list (fun _fmt () -> ()(*fprintf fmt "@\nand"*))
	  print_decl fmt dlist
    | JCvar_def(ty,id,init) ->
	fprintf fmt "@\n@[%a %s%a;@]@." print_type ty id
	  (print_option (fun fmt e -> fprintf fmt " = %a" expr e)) init
    | JClemma_def(id,is_axiom,lab,a) ->
	fprintf fmt "@\n@[%s %s" (if is_axiom then "axiom" else "lemma") id;
	if lab <> [] then
	  fprintf fmt "%a" (print_list_delim lbrace rbrace comma label) lab;
	fprintf fmt " :@\n%a@]@." assertion a
    | JCglobinv_def(id,a) ->
	fprintf fmt "@\n@[invariant %s :@\n%a@]@." id assertion a
    | JCexception_def(id,ei) ->
	fprintf fmt "@\n@[exception %s of %a@]@." id
	  (print_option_or_default "unit" print_type)
	  ei.jc_exception_info_type
    | JClogic_const_def(ty,id,None) ->
	fprintf fmt "@\n@[logic %a %s@]@." print_type ty id
    | JClogic_const_def(ty,id,Some t) ->
	fprintf fmt "@\n@[logic %a %s = %a@]@." print_type ty id
	  term t
    | JClogic_fun_def(ty,id,labels,[],JCReads l) ->
	assert (l=[]);
	assert (labels=[]);
	fprintf fmt "@\n@[logic %a %s@]@." 
	  (print_option print_type) ty id
(* Yannick: no need for different rule for const logic *)
(*     | JClogic_fun_def(ty,id,labels,[],JCTerm t) -> *)
(* 	assert (labels=[]); *)
(* 	fprintf fmt "@\n@[logic %a %s = %a@]@."  *)
(* 	  (print_option print_type) ty id term t *)
    | JClogic_fun_def(ty,id,[],params,body) ->
	fprintf fmt "@\n@[logic %a %s(@[%a@]) %a@]@." 
	  (print_option print_type) ty 
	  id (print_list comma param) params
	  term_or_assertion body 
    | JClogic_fun_def (None, id, labels, params, body) ->
	fprintf fmt "@\n@[logic %s%a(@[%a@]) %a@]@." 
	  id (print_list_delim lbrace rbrace comma label) labels
	  (print_list comma param) params
	  term_or_assertion body 
    | JClogic_fun_def (Some ty, id, labels, params, body) ->
	fprintf fmt "@\n@[logic %a %s%a(@[%a@]) %a@]@." 
	  print_type ty 
	  id (print_list_delim lbrace rbrace comma label) labels
	  (print_list comma param) params
	  term_or_assertion body 
    | JClogic_type_def id ->
	fprintf fmt "@\n@[logic type %s@]@." id
   
let rec print_decls fmt d =
  match d with
    | [] -> ()
    | d::r -> print_decl fmt d; print_decls fmt r


(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte bin/krakatoa.byte"
End: 
*)
