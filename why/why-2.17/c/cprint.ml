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

(*i $Id: cprint.ml,v 1.53 2008/11/05 14:03:13 filliatr Exp $ i*)

(* Pretty-printer for normalized AST *)

open Format
open Ctypes
open Clogic
open Cast
open Info
open Pp
open Cutil

let declare_struct fmt s (_,_fields) =
  fprintf fmt "@[<hov 2>struct %s {@\n" s;
  begin match Cenv.tag_type_definition s with
    | Cenv.TTStructUnion(_,fields) ->
	List.iter (fun f ->
		     fprintf fmt "%a %s;@\n" ctype f.var_type f.var_unique_name) fields 
    | _ -> assert false
  end;
  fprintf fmt "};@]@\n@\n"

let term_unop = function
  | Clogic.Uminus -> "-"
  | Clogic.Utilde -> "~"
  | Clogic.Ustar -> "*"
  | Clogic.Uamp -> "&"
  | Clogic.Ufloat_of_int -> "float_of_int"
  | Clogic.Uint_of_float -> "int_of_float"
  | Clogic.Ufloat_conversion -> "float_conversion"
  | Clogic.Uint_conversion -> "int_conversion"
  | Clogic.Uabs_real -> "abs_real"
  | Clogic.Usqrt_real -> "sqrt_real"
  | Clogic.Uround_error -> "round_error"
  | Clogic.Utotal_error -> "total_error"
  | Clogic.Uexact -> "exact"
  | Clogic.Umodel -> "model"
  | Clogic.Unot -> "!"
  | Clogic.Uplus -> "+"


let term_binop = function
  | Clogic.Badd -> "+"
  | Clogic.Bsub -> "-"
  | Clogic.Bmul -> "*"
  | Clogic.Bdiv -> "/"
  | Clogic.Bmod -> "%"
  | Clogic.Bpow_real -> "^^"
  | Clogic.Bbw_and -> "&"
  | Clogic.Bbw_or -> "|"
  | Clogic.Bbw_xor -> "^"
  | Clogic.Bshift_right -> ">>"
  | Clogic.Bshift_left -> "<<"
 
let rec nterm fmt t = match t.nterm_node with
  | NTconstant (IntConstant s | RealConstant s) ->
      fprintf fmt "%s" s
  | NTstring_literal s -> fprintf fmt "\"%s\"" s
  | NTvar x ->
      fprintf fmt "%s" x.var_unique_name
  | NTapp {napp_pred = li; napp_args = tl;} ->
      fprintf fmt "%s(%a)" li.logic_name (print_list comma nterm) tl
  | NTunop (op, t) ->
      fprintf fmt "%s%a" (term_unop op) nterm_p t
(*  | NTstar t ->
      fprintf fmt "*%a" nterm_p t*)
  | NTbinop (t1, op, t2) ->
      fprintf fmt "%a %s %a" nterm_p t1 (term_binop op) nterm_p t2
  | NTarrow (t,_, vi) ->
      fprintf fmt "%a->%s" nterm_p t vi.var_unique_name
  | NTif (t1, t2, t3) ->
      fprintf fmt "%a ? %a : %a" nterm_p t1 nterm_p t2 nterm_p t3
  | NTold t ->
      fprintf fmt "\\old(%a)" nterm t
  | NTat (t, l) ->
      fprintf fmt "\\at(%a, %s)" nterm t l
  | NTbase_addr t ->
      fprintf fmt "\\base_addr(%a)" nterm t
  | NToffset t ->
      fprintf fmt "\\offset(%a)" nterm t
  | NTblock_length t ->
      fprintf fmt "\\block_length(%a)" nterm t
  | NTarrlen t ->
      fprintf fmt "\\arrlen(%a)" nterm t
  | NTstrlen (t,_, _) ->
      fprintf fmt "\\strlen(%a)" nterm t
  | NTmin (t1,t2) ->
      fprintf fmt "\\min(%a,%a)" nterm t1 nterm t2
  | NTmax (t1,t2) ->
      fprintf fmt "\\max(%a,%a)" nterm t1 nterm t2
  | NTminint ty ->
      fprintf fmt "\\minint(%a)" ctype ty
  | NTmaxint ty ->
      fprintf fmt "\\maxint(%a)" ctype ty
  | NTcast (ty, t) ->
      fprintf fmt "(%a)%a" ctype ty nterm t
  | NTrange (t1, t2, t3, _,f) ->
      fprintf fmt "(%a+%a..%a+%a)->%s" nterm t1 nterm_option t2 nterm t1 nterm_option t3 f.var_unique_name

and nterm_p fmt t = match t.nterm_node with
  | NTconstant _ | NTvar _ | NTapp _ | NTold _ | NTat _ 
  | NTarrlen _ | NTstrlen _ | NTmin _ | NTmax _ ->
      nterm fmt t
  | _ ->
      fprintf fmt "(%a)" nterm t

and nterm_option fmt = function
  | None -> ()
  | Some t -> nterm fmt t

let quantifier fmt (ty, x) = fprintf fmt "%a %s" ctype ty x.var_unique_name

let quantifiers = print_list comma quantifier

let relation = function
  | Lt -> "<"
  | Gt -> ">"
  | Le -> "<="
  | Ge -> ">="
  | Eq -> "=="
  | Neq -> "!="
 
let rec npredicate fmt p = match p.npred_node with
  | NPfalse ->
      fprintf fmt "false"
  | NPtrue ->
      fprintf fmt "true"
  | NPapp {napp_pred = li; napp_args = tl;} ->
      fprintf fmt "%s(%a)" li.logic_name (print_list comma nterm) tl
  | NPrel (t1, rel, t2) ->
      (* no need for parentheses around a relation. It can only be used
	 inside a boolean formula, with the relational operator morally
	 binding tighter than any boolean operator. *)
      fprintf fmt "@[%a %s %a@]" nterm t1 (relation rel) nterm t2
  | NPand (_p1, _p2) ->
      (* improved printing for range inequalities, e.g. 0 <= i < 100 *)
      nconjunct fmt p
  | NPor (p1, p2) ->
      fprintf fmt "@[(%a ||@ %a)@]" npredicate p1 npredicate p2
  | NPimplies (p1, p2) ->
      fprintf fmt "@[(%a =>@ %a)@]" npredicate p1 npredicate p2
  | NPiff (p1, p2) ->
      fprintf fmt "@[(%a <=>@ %a)@]" npredicate p1 npredicate p2
  | NPnot p ->
      fprintf fmt "! %a" npredicate p
  | NPif (t, p1, p2) ->
      fprintf fmt "@[(%a ? %a : %a)@]" nterm t npredicate p1 npredicate p2
  | NPforall (q, p) ->
      fprintf fmt "@[(\\forall %a;@ %a)@]" quantifiers q npredicate p
  | NPexists (q, p) ->
      fprintf fmt "@[(\\exists %a;@ %a)@]" quantifiers q npredicate p
  | NPold p ->
      fprintf fmt "\\old(%a)" npredicate p
  | NPat (p, l) ->
      fprintf fmt "\\at(%a, %s)" npredicate p l
  | NPvalid t ->
      fprintf fmt "\\valid(%a)" nterm t
  | NPvalid_index (t1, t2) ->
      fprintf fmt "\\valid_index(%a, %a)" nterm t1 nterm t2
  | NPvalid_range (t1, t2, t3) ->
      fprintf fmt "\\valid_range(%a, %a, %a)" nterm t1 nterm t2 nterm t3
  | NPfresh t ->
      fprintf fmt "\\fresh(%a)" nterm t
  | NPnamed (id, p) ->
      fprintf fmt "@[(%s::@ %a)@]" id npredicate p
  | NPseparated (t1,t2) ->
      fprintf fmt "\\separated(%a, %a)" nterm t1 nterm t2
  | NPfull_separated (t1,t2) ->
      fprintf fmt "\\full_separated(%a, %a)" nterm t1 nterm t2
  | NPbound_separated (t1,t2,t3,t4) ->
      fprintf fmt "\\bound_separated(%a, %a, %a, %a)" 
	nterm t1 nterm t2 nterm t3 nterm t4

(* given a conjunct [p], try to match relations inside p's conjuncts, in order
   to print together range inequalities that refer to the same variable, e.g.
   print
       0 <= i < 100
   instead of
       (i < 100) && (i >= 0)
*)
and nconjunct fmt p =
  (* extract the conjuncts *)
  let rec cnf p = match p.npred_node with
    | NPand (p1,p2) -> cnf p1 @ (cnf p2)
    | _ -> [p]
  in
  (* change sides in a relation *)
  let change_side_rel rel = match rel with
    | Lt -> Gt | Le -> Ge | Gt -> Lt | Ge -> Le | Eq -> Eq | Neq -> Neq
  in
  (* if a valid combination exists, return it.
     Each parameter is a pair of a boolean stating whether the matching term
     was found on the left-hand side or right-hand side of the corresponding
     relation, and the predicate for this relation.
   *)
  let combine (left1,p1) (left2,p2) =
    if left1 = left2 then
      match p1.npred_node,p2.npred_node with
        | NPrel (tl1,op1,tr1),NPrel (tl2,op2,tr2) ->
	    begin match op1,op2 with
	      | (Lt | Le),(Gt | Ge) | (Gt | Ge),(Lt | Le) ->
		  (* combination is possible *)
		  if left1 then
		    begin match op1 with
		      | Lt | Le ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tr2 (relation (change_side_rel op2))
			    nterm tl1 (relation op1) nterm tr1)
		      | Gt | Ge ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tr1 (relation (change_side_rel op1))
			    nterm tl1 (relation op2) nterm tr2)
		      | Eq | Neq -> 
			  (* not a valid combination *)
			  assert false
		    end
		  else
		    begin match op1 with
		      | Lt | Le ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tl1 (relation op1) nterm tr1
			    (relation (change_side_rel op2)) nterm tl2)
		      | Gt | Ge ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tl2 (relation op2) nterm tr1
			    (relation (change_side_rel op1)) nterm tl1)
		      | Eq | Neq -> 
			  (* not a valid combination *)
			  assert false
		    end
	      | _ -> None
	    end
	| _ -> 
	    (* both [p1] and [p2] should be relations *)
	    assert false
    else
      match p1.npred_node,p2.npred_node with
        | NPrel (tl1,op1,tr1),NPrel (tl2,op2,tr2) ->
	    begin match op1,op2 with
	      | (Lt | Le),(Lt | Le) | (Gt | Ge),(Gt | Ge) ->
		  (* combination is possible *)
		  if left1 then
		    begin match op1 with
		      | Lt | Le ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tl2 (relation op2)
			    nterm tl1 (relation op1) nterm tr1)
		      | Gt | Ge ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tr1 (relation (change_side_rel op1))
			    nterm tl1 (relation (change_side_rel op2)) 
			    nterm tl2)
		      | Eq | Neq -> 
			  (* not a valid combination *)
			  assert false
		    end
		  else
		    begin match op1 with
		      | Lt | Le ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tl1 (relation op1) nterm tr1
			    (relation op2) nterm tr2)
		      | Gt | Ge ->
			  Some (fun fmt ->
			  fprintf fmt "%a %s %a %s %a"
			    nterm tr2 (relation (change_side_rel op2)) 
			    nterm tr1 (relation op1) nterm tl1)
		      | Eq | Neq -> 
			  (* not a valid combination *)
			  assert false
		    end
	      | _ -> None
	    end
	| _ -> 
	    (* both [p1] and [p2] should be relations *)
	    assert false
  in
  (* search if sub-term [t] of relational predicate [p] can be recognized 
     in another relation. We base this search on names used for printing.
     If another relation [mp] is found in map [m] with the same term [t],
     consider whether they can be combined by calling [combine].
     Otherwise, add the correspondance [t] -> [p] to the map.
   *)
  let search_and_combine p t left m =
    match t.nterm_node with
      | NTvar _ | NTstrlen _ | NTarrlen _ ->
	  let name = match t.nterm_node with
	     | NTvar v -> Some v.var_unique_name
	     | NTstrlen (t1,_,_) ->
		 begin match t1.nterm_node with
		   | NTvar v -> Some ("\\strlen(" ^ v.var_unique_name ^ ")")
		   | _ -> None
		 end
	     | NTarrlen t1 ->
		 begin match t1.nterm_node with
		   | NTvar v -> Some ("\\arrlen(" ^ v.var_unique_name ^ ")")
		   | _ -> None
		 end
	     | _ -> assert false
	  in
	  begin match name with 
	    | Some name ->
		begin try 
		  let mp = StringMap.find name m in
		  match combine (left,p) mp with
		    | Some cp -> (Some cp),StringMap.remove name m
		    | None -> None,StringMap.add name (left,p) m
		with Not_found ->
		  None,StringMap.add name (left,p) m
		end
	    | _ -> None,m
	  end
      | _ -> None,m
  in
  (* [list_conjuncts] is the list of relational predicates combined.
     [name_map] is the pending list of correspondance from term to predicates.
   *)
  let list_conjuncts,name_map =
    List.fold_left 
      (fun (cl,m) p -> match p.npred_node with
         | NPrel (t1,_rel,t2) ->
	     begin match search_and_combine p t1 (* left = *)true m with
	       | (Some cp),new_m ->
		   (* [p] combined, do not search with [t2] *)
		   cp :: cl,new_m
	       | None,new_m ->
		   (* continue search for a combination with [t2] *)
		   begin match search_and_combine p t2 (* left = *)false new_m
		       with
		     | (Some cp),new_m ->
			 cp :: cl,new_m
		     | None,new_m ->
			 (* if [p] added to [m], do not return it now *)
			 if m == new_m then
			   (fun fmt -> npredicate fmt p) :: cl,m
			 else cl,new_m
		   end
	     end
	 | _ -> (fun fmt -> npredicate fmt p) :: cl,m
      ) ([],StringMap.empty) (cnf p)
  in
  (* if [name_map] not empty, add these predicates to the list of conjuncts *)
  let list_conjuncts =
    StringMap.fold (fun _ (_,p) cl -> (fun fmt -> npredicate fmt p) :: cl) 
      name_map list_conjuncts
  in
  fprintf fmt "@[(%a)@]" 
    (print_list (fun fmt () -> fprintf fmt "@\n && ") (fun fmt f -> f fmt))
    list_conjuncts

let parameter fmt  x = fprintf fmt "%a %s" ctype x.var_type x.var_unique_name

let parameters = print_list comma parameter

let logic_parameter fmt (x, ty) = fprintf fmt "%a %s" ctype ty x.var_unique_name

let logic_parameters = print_list comma logic_parameter

let location = nterm

let locations = print_list comma location

let nlogic_symbol fmt li = function
  | NPredicate_reads (pl, locs) ->
      fprintf fmt "/*@@ @[predicate %s(%a) reads %a@] */@\n" li.logic_name
	logic_parameters pl locations locs
  | NPredicate_def (pl, p) ->
      fprintf fmt "/*@@ @[predicate %s(%a) { %a }@] */@\n" li.logic_name
	logic_parameters pl npredicate p
  | NFunction (pl, ty, locs) ->
      fprintf fmt "/*@@ @[logic %a %s(%a) reads %a@] */@\n" ctype ty 
	li.logic_name logic_parameters pl locations locs
  | NFunction_def (pl, ty, t) ->
      fprintf fmt "/*@@ @[logic %a %s(%a) { %a }@] */@\n" ctype ty 
	li.logic_name logic_parameters pl nterm t
	
let spec fmt = function
  | { requires = None; assigns = None; ensures = None; decreases = None } ->
      ()
  | s ->
      let requires fmt p = fprintf fmt "@[requires %a@]@\n" npredicate p in
      let assigns fmt (_,a) = fprintf fmt "@[assigns %a@]@\n" locations a in
      let ensures fmt p = fprintf fmt "@[ensures %a@]@\n" npredicate p in
      let decreases fmt = function
	| (t, None) -> fprintf fmt "@[decreases %a@]@\n" nterm t
	| (t, Some r) -> fprintf fmt "@[decreases %a for %s@]@\n" nterm t r
      in
      fprintf fmt "/*@@ @[%a%a%a%a@] */@\n"
	(print_option requires) s.requires
	(print_option assigns) s.assigns
	(print_option ensures) s.ensures
	(print_option decreases) s.decreases

let loop_annot fmt = function
  | { invariant = None; assume_invariant = None; 
      loop_assigns = None; variant = None } ->
      ()
  | a ->
      let invariant fmt p = fprintf fmt "@[invariant %a@]@\n" npredicate p in
      let assume_invariant fmt p = 
	fprintf fmt "@[assume invariant %a@]@\n" npredicate p in
      let loop_assigns fmt (_,a) = 
	fprintf fmt "@[assigns %a@]@\n" locations a in
      let variant fmt = function
	| (t, None) -> fprintf fmt "@[variant %a@]@\n" nterm t
	| (t, Some r) -> fprintf fmt "@[variant %a for %s@]@\n" nterm t r
      in
      fprintf fmt "/*@@ @[%a%a%a%a@] */@\n"
	(print_option assume_invariant) a.assume_invariant
	(print_option invariant) a.invariant
	(print_option loop_assigns) a.loop_assigns
	(print_option variant) a.variant

let binop fmt = function
  | Badd | Badd_int _ | Badd_float _ | Badd_pointer_int -> fprintf fmt "+" 
  | Bsub | Bsub_int _ | Bsub_float _ | Bsub_pointer -> fprintf fmt "-"
  | Bmul | Bmul_int _ | Bmul_float _ -> fprintf fmt "*"
  | Bdiv | Bdiv_int _ | Bdiv_float _ -> fprintf fmt "/"
  | Bmod | Bmod_int _ -> fprintf fmt "%%" 
  | Blt | Blt_int | Blt_float _ | Blt_pointer -> fprintf fmt "<"
  | Bgt | Bgt_int | Bgt_float _ | Bgt_pointer -> fprintf fmt ">"
  | Ble | Ble_int | Ble_float _ | Ble_pointer -> fprintf fmt "<="
  | Bge | Bge_int | Bge_float _ | Bge_pointer -> fprintf fmt ">="
  | Beq | Beq_int | Beq_float _ | Beq_pointer -> fprintf fmt "=="
  | Bneq | Bneq_int | Bneq_float _ | Bneq_pointer -> fprintf fmt "!=" 
  | Bbw_and -> fprintf fmt "&"
  | Bbw_xor -> fprintf fmt "^"
  | Bbw_or -> fprintf fmt "|"
  | Band -> fprintf fmt "&&"
  | Bor -> fprintf fmt "||"
  | Bshift_left -> fprintf fmt "<<"
  | Bshift_right -> fprintf fmt ">>"


let unop fmt = function
  | Uplus -> fprintf fmt "+"
  | Uminus -> fprintf fmt "-"
  | Unot -> fprintf fmt "!"
  | Ustar -> fprintf fmt "*"
  | Uamp -> fprintf fmt "&"
  | Utilde -> fprintf fmt "~"
  (* these are introduced during typing *)
  | Ufloat_of_int -> fprintf fmt "float_of_int"
  | Uint_of_float -> fprintf fmt "int_of_float"
  | Ufloat_conversion -> fprintf fmt "float_conversion"
  | Uint_conversion -> fprintf fmt "int_conversion"

let rec nexpr fmt e = match e.nexpr_node with
  | NEnop ->
      ()
  | NEconstant (IntConstant s | RealConstant s) ->
      fprintf fmt "%s" s
  | NEstring_literal s ->
      fprintf fmt "\"%S\"" s
  | NEvar (Var_info x) ->
      fprintf fmt "%s" x.var_unique_name
  | NEvar (Fun_info x) ->
      fprintf fmt "%s" x.fun_name
  | NEarrow (e,_,x) ->
      let typ = e.nexpr_type in
      begin match typ.Ctypes.ctype_node with
      | Ctypes.Tpointer (Ctypes.Valid (i,j),_) 
      | Ctypes.Tarray (Ctypes.Valid(i,j),_,_) ->
	  fprintf fmt "%a-%d-ok-%d->%s" nexpr_p e (Int64.to_int i) 
	    (Int64.to_int j) x.var_unique_name
      | _ ->
	  fprintf fmt "%a->%s" nexpr_p e x.var_unique_name
      end
(*  | NEstar e ->
      fprintf fmt "*%a" nexpr_p e*)
  | NEseq (e1, e2) ->
      fprintf fmt "%a, %a" nexpr e1 nexpr e2
  | NEassign (e1, e2) ->
      fprintf fmt "%a = %a" nexpr e1 nexpr e2
  | NEassign_op (e1, op, e2) ->
      fprintf fmt "%a %a= %a" nexpr e1 binop op nexpr e2
  | NEunary (op, e) ->
      fprintf fmt "%a(%a)" unop op nexpr_p e
  | NEincr (Uprefix_inc, e) -> fprintf fmt "++%a" nexpr_p e
  | NEincr (Uprefix_dec, e) -> fprintf fmt "--%a" nexpr_p e
  | NEincr (Upostfix_inc, e) -> fprintf fmt "%a++" nexpr_p e
  | NEincr (Upostfix_dec, e) -> fprintf fmt "%a--" nexpr_p e
  | NEbinary (e1, op, e2) ->
      fprintf fmt "%a %a %a" nexpr_p e1 binop op nexpr_p e2
  | NEcall { ncall_fun = e ; ncall_args = l } ->
      fprintf fmt "%a(%a)" nexpr e (print_list comma nexpr) l
  | NEcond (e1, e2, e3) ->
      fprintf fmt "%a ? %a : %a" nexpr e1 nexpr e2 nexpr e3
  | NEcast (ty, e) ->
      fprintf fmt "(%a)%a" ctype ty nexpr_p e
  | NEmalloc (ty, e) ->
      fprintf fmt "(%a*)malloc(%a * sizeof(%a))" ctype ty nexpr_p e ctype ty

and nexpr_p fmt e = match e.nexpr_node with
  | NEnop | NEconstant _ | NEstring_literal _ | NEvar _ -> nexpr fmt e
  | _ -> fprintf fmt "(@[%a@])" nexpr e

let rec c_initializer fmt = function
  | Iexpr e -> nexpr fmt e
  | Ilist l -> fprintf fmt "@[{ %a }@]" (print_list comma c_initializer) l

let ngoto_status = function
  | GotoBackward -> "backward"
  | GotoForwardOuter -> "forward,outer"
  | GotoForwardInner -> "forward,inner"

let rec nstatement fmt s = match s.nst_node with
  | NSnop -> 
      fprintf fmt ";"
  | NSexpr e ->
      fprintf fmt "@[%a;@]" nexpr e
  | NSif (e, s1, s2) ->
      fprintf fmt "@[if (%a) {@\n    @[%a@]@\n} else {@\n    @[%a@]@\n}@]"
	nexpr e nstatement_nb s1 nstatement_nb s2
  | NSwhile (a, e, s) ->
      fprintf fmt "@[%awhile (%a) {@\n    @[%a@]@\n}@]" 
	loop_annot a nexpr e nstatement_nb s
  | NSdowhile (a, s, e) ->
      fprintf fmt "@[%ado {@\n    @[%a@]@\n} while (%a);@]" 
	loop_annot a nstatement_nb s nexpr e 
  | NSfor (a, e1, e2, e3, s) ->
      fprintf fmt "@[%afor (%a; %a; %a) {@\n    @[%a@]@\n}@]"
	loop_annot a nexpr e1 nexpr e2 nexpr e3 nstatement_nb s
  | NSreturn None ->
      fprintf fmt "return;"
  | NSreturn (Some e) ->
      fprintf fmt "@[return %a;@]" nexpr e
  | NSbreak ->
      fprintf fmt "break;"
  | NScontinue ->
      fprintf fmt "continue;"
  | NSlabel (l, s) ->
      fprintf fmt "%s: %a" l.label_info_name nstatement s
  | NSgoto (status, l) ->
      fprintf fmt "goto(%s) %s" (ngoto_status status) l.label_info_name 
  | NSswitch (e, _m, l) ->
      fprintf fmt "@[switch (%a) {@\n    @[%a@]@\n}@]"
	nexpr e (print_list newline ncase) l
  | NSassert p ->
      fprintf fmt "/*@@ assert %a */" npredicate p
  | NSassume p ->
      fprintf fmt "/*@@ assume %a */" npredicate p
  | NSlogic_label l ->
      fprintf fmt "/*@@ label %s */" l
  | NSspec (sp, s) ->
      fprintf fmt "%a%a" spec sp nstatement s
  | NSblock b ->
      fprintf fmt "@[{@\n  @[%a@]@\n}@]" nblock b
  | NSdecl _ ->
      (* successive declarations share the same block statement *)
      fprintf fmt "@[<hov 2>{@\n";
      nsdecl fmt s;
      fprintf fmt "@\n}@\n@]"

and nsdecl fmt s = match s.nst_node with
  | NSdecl (ty, vi, None,rem) ->
      fprintf fmt "%a %s;@\n" ctype ty vi.var_unique_name;
      nsdecl fmt rem
  | NSdecl (ty, vi, Some i, rem) ->
      fprintf fmt "%a %s = %a;@\n" ctype ty vi.var_unique_name c_initializer i;
      nsdecl fmt rem
  | _ -> nstatement fmt s
    
and nstatement_nb fmt s = match s.nst_node with
  | NSblock b -> nblock fmt b
  | _ -> nstatement fmt s

and nblock fmt sl =
  fprintf fmt "@[%a@]" 
    (print_list newline nstatement) sl

and ncase fmt (cmap,sl) =
  fprintf fmt "@[%a    %a@]"
    (fun fmt -> 
       if Cconst.IntMap.is_empty cmap then
	 (fun _ -> fprintf fmt "default:@\n")
       else
	 Cconst.IntMap.iter 
	   (fun _ e -> fprintf fmt "case %a:@\n" nexpr e)) cmap
    nblock sl

and ndecl fmt d = match d.node with
  | Nlogic (li, ls) -> 
      nlogic_symbol fmt li ls
  | Naxiom (x, p) -> 
      fprintf fmt "/*@@ @[axiom %s:@ %a@] */@\n" x npredicate p
  | Ninvariant (x, p) -> 
      fprintf fmt "/*@@ @[<hov 2>invariant %s:@ %a@] */@\n" x npredicate p 
  | Ninvariant_strong (x, p) -> 
      fprintf fmt "/*@@ @[<hov 2>strong invariant %s:@ %a@] */@\n" x 
	npredicate p
  | Ntypedef (ty, x) ->
      fprintf fmt "typedef %a %s;@\n" ctype ty x
  | Ntypedecl ty ->
      fprintf fmt "%a;@\n" ctype ty
  | Ndecl (ty, vi, None) ->
      fprintf fmt "%a %s;@\n" ctype ty vi.var_unique_name
  | Ndecl (ty, vi, Some i) ->
      fprintf fmt "%a %s = %a;@\n" ctype ty vi.var_unique_name c_initializer i
(*  | Nfunspec (s, ty, fi) ->
      fprintf fmt "%a%a %s(@[%a@]);@\n" spec s ctype ty fi.fun_name
	parameters fi.args
  | Nfundef (s, ty, fi, st) ->
      fprintf fmt "%a%a %s(@[%a@])@\n%a@\n" spec s ctype ty fi.fun_name
	 parameters fi.args nstatement st
*)
  | Ntype s ->
      fprintf fmt "/*@@ type %s */@\n" s

let nfile fmt p = 
  fprintf fmt "@[";
  Cenv.iter_all_struct (declare_struct fmt);
  List.iter (fun d -> ndecl fmt d; fprintf fmt "@\n") p;
  fprintf fmt "@]@."


let nfunctions fmt =
  Hashtbl.iter
    (fun _f (s, ty, fi, st,_) -> 
       match st with
	 | Some st ->
	     fprintf fmt "%a%a %s(@[%a@])@\n%a@\n" spec s ctype ty fi.fun_name
	       parameters fi.args nstatement st
	 | None ->
	     fprintf fmt "%a%a %s(@[%a@]);@\n" spec s ctype ty fi.fun_name
	       parameters fi.args)
    Cenv.c_functions


