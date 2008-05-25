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

(*i $Id: invariant.ml,v 1.49 2008/02/05 12:10:48 marche Exp $ i*)

open Coptions
open Creport
open Info
open Ctypes
open Clogic
open Cenv
open Cnorm
open Cprint
open Cseparation

let var_to_term loc v =
  {
    nterm_node = NTvar v; 
    nterm_loc = loc;
    nterm_type = v.var_type}

let indirection loc ty t =
  let info = make_field ty in
  let info = declare_arrow_var info  in
  let zone = find_zone_for_term t in
  let () = type_why_new_zone zone info in
  { nterm_node = NTarrow (t, zone, info); 
    nterm_loc = loc; 	   
    nterm_type = ty;}

let noattr_type ty =
  {ctype_node = ty;
   ctype_storage = No_storage;
   ctype_const = false;
   ctype_volatile = false;
   ctype_ghost = false;
  }

let noattr_term ty t= 
  { nterm_node = t; 
    nterm_loc = Loc.dummy_position;
    nterm_type = ty;}

let find_pred x = snd (find_pred x)
  
let rec predicate_for name t =
  match t.nterm_type.ctype_node with
    | Tstruct (n) ->
	(*	NPand *)
	npvalid t
	  (*,
 	    NPapp (find_pred ("valid_" ^ n), [t]))*)
    | Tarray (_,ty, None) ->
	error Loc.dummy_position "array size missing in `%s'" name
    | Tarray (_,ty, Some s) ->
	  let i = default_var_info "counter" in
	  set_var_type (Var_info i) c_int false; 
	  let vari = noattr_term c_int (NTvar i) in
	  let ineq = npand
		       (nprel (nzero, Le, vari),
			nprel (vari, Lt, 
			       int_nconstant (Int64.to_string s))) in
	  let pre = predicate_for name vari in
	  if pre = nptrue 
	  then
	    npand ((npvalid t),(npvalid_range (t,(int_nconstant "0"), 
			  (int_nconstant (Int64.to_string (Int64.pred s))))))
	  else
	    npand (npvalid t, 
		   (npand 
		      ((npvalid_range (t,(int_nconstant "0"), 
			  (int_nconstant (Int64.to_string (Int64.pred s))))),
		      (make_forall 
			 [noattr_type (Tint (Signed, Ctypes.Int)), i]
			 (make_implies ineq pre)))))
     | _ -> nptrue


let fresh_index = 
  let r = ref (-1) in fun () -> incr r; "index_" ^ string_of_int !r

let make_forall_range loc t b f =
  if b = Int64.one
  then f t nzero
  else
    let i = default_var_info (fresh_index ()) in
    let vari = { nterm_node = NTvar i; 
		 nterm_loc = loc;
		 nterm_type = c_int;} in
    let ti = 
      { nterm_node = NTbinop (t, Badd, vari); 
	nterm_loc = loc;
	nterm_type = t.nterm_type;}
    in
    let pred = (f ti vari) in
    if pred = nptrue
    then 
      nptrue
    else
      let ineq = npand (nprel (nzero, Le, vari),
			nprel (vari, Lt, int_nconstant (Int64.to_string b))) in
      make_forall [c_int, i] (make_implies ineq pred)
     
let rec tab_struct loc v1 v2 s ty n n1 n2=
  (make_forall_range loc v2 s 
     (fun t i -> 
	local_separation loc n1 v1 (n2^"[i]") (indirection loc ty t)))

and local_separation loc n1 v1 n2 v2 =
  match (v1.nterm_type.Ctypes.ctype_node,v2.nterm_type.Ctypes.ctype_node) 
  with
    | Tarray (_,ty, None), _ ->
	error loc "array size missing in `%s'" n1
    | _, Tarray (_,ty, None) ->
	error loc "array size missing in `%s'" n2
    | Tstruct n , Tarray (_,ty,Some s) -> tab_struct loc v1 v2 s ty n n1 n2
    | Tarray (_,ty,Some s) , Tstruct n -> tab_struct loc v2 v1 s ty n n1 n2
    | Tarray (_,ty1,Some s1), Tarray(_,ty2,Some s2) ->
	make_and
	  (if compatible_type v1.nterm_type v2.nterm_type
	   then
	     (not_alias loc v1 v2)
	   else
	     nptrue)
	  (make_and 
	     (make_forall_range loc v1 s1 
		(fun t i -> local_separation loc (n1^"[i]") 
		     (indirection loc ty1 t) n2 v2))
	     (make_forall_range loc v2 s2  
		(fun t i -> local_separation loc n1 v1 (n2^"[j]") 
		     (indirection loc ty2 t))))
    | _, _ -> nptrue

    


let rec separation_intern2  n1 v1 =
  match v1.nterm_type.Ctypes.ctype_node with
    | Tarray (_,_,None) -> 
	error Loc.dummy_position "array size missing in `%s'" n1
    | Tarray(_,ty,Some s) ->
	  begin
	    match ty.Ctypes.ctype_node with
	      | Tarray (_,_,None) -> 
		error Loc.dummy_position "array size missing in `%s[i]'" n1 
	      | Tstruct _ ->
		  (make_forall_range Loc.dummy_position v1 s 
		     (fun t1 i1 ->
			make_forall_range Loc.dummy_position v1 s
			  (fun t2 i2 -> 
			     if i1 = nzero && i2 = nzero then nptrue 
			     else
			       make_implies (nprel (i1, Neq, i2)) 
				 (not_alias Loc.dummy_position t1 t2))))
	      | Tarray _ ->  
		  make_and
		    (make_forall_range Loc.dummy_position v1 s 
		       (fun t1 i1 ->
			  make_forall_range Loc.dummy_position v1 s
			    (fun t2 i2 -> 
			       if i1 = nzero && i2 = nzero then nptrue 
			       else
				 make_implies (nprel (i1, Neq, i2)) 
				   (not_alias Loc.dummy_position t1 t2))))
		    (make_forall_range Loc.dummy_position v1 s 
		       (fun t i -> 
			  separation_intern2 n1 
			    (noattr_term ty t.nterm_node)))    
	      | _ -> nptrue
	  end
    | _ -> 
	nptrue

(*let rec separation loc n1 v1 n2 v2 =
  match (v1.nterm_type.Ctypes.ctype_node,v2.nterm_type.Ctypes.ctype_node) 
  with
    | Tarray (ty, None), _ ->
	error loc ("array size missing in `" ^ n1 ^ "'")
    | _, Tarray (ty, None) ->
	error loc ("array size missing in `" ^ n2 ^ "'")
    | Tstruct n , Tarray (ty,Some s) -> 
	if compatible_type ty v1.nterm_type 
	then
	  (make_forall_range loc v2 s 
	     (fun t i -> not_alias loc 
		(indirection loc ty 
		   (noattr_term (noattr_type (Tpointer ty)) 
		      (NTbinop (t,Badd,i)))) v1))
	else
	  NPtrue
    | Tarray (ty,Some s) , Tstruct n ->
	if compatible_type ty v2.nterm_type 
	then
	  (make_forall_range loc v1 s 
	     (fun t i -> not_alias loc 
		(indirection loc ty 
		   (noattr_term (noattr_type (Tpointer ty)) 
		      (NTbinop (t,Badd,i)))) v2))
	else
	  NPtrue
    | Tarray (ty1,Some s1), Tarray(ty2,Some s2) ->
	make_and
	  (if compatible_type v1.nterm_type v2.nterm_type
	   then
	     (not_alias loc v1 v2)
	   else
	     NPtrue)
	  (make_and 
	     (make_forall_range loc v1 s1 
		(fun t i -> separation loc (n1^"[i]") 
		     (indirection loc ty1  
			(noattr_term (noattr_type (Tpointer ty1)) 
			   (NTbinop (t,Badd,i)))) n2 v2))
	     (make_forall_range loc v2 s2  
		(fun t i -> separation loc n1 v1 (n2^"[j]") 
		     (indirection loc ty2  
			(noattr_term (noattr_type (Tpointer ty2)) 
			   (NTbinop (t,Badd,i)))))))
    | _, _ -> NPtrue
*)
  
let rec fold_2 f l = 
  match l with 
    | v::l ->
	List.fold_left (fun acc x ->(f v x)@acc) (fold_2 f l) l
    | _ -> [] 

let noattr_located n =  
  { Cast.node = n; Cast.loc = Loc.dummy_position }

(*
let heap_var_add v label m =
  try
    let l = HeapVarMap.find v m in
    HeapVarMap.add v (LabelSet.add label l) m 
  with Not_found ->
    HeapVarMap.add v (LabelSet.singleton label) m 
*)
    
let single v =  HeapVarSet.singleton v
(*
  heap_var_add v Label_current HeapVarMap.empty
*)

let pair v1 v2 = HeapVarSet.add v1 (single v2)
(*
  heap_var_add v1 Label_current (single v2)
*)  

let separation_first mark diag v1 v2 =
  let sep = if mark then "%separation1" else "%separation2" in
  let n1 =  v1.var_unique_name in
  let n2 =  v2.var_unique_name in
  match v1.var_type.Ctypes.ctype_node,v2.var_type.Ctypes.ctype_node with
    | Tarray (_,_,None), _  ->
	error Loc.dummy_position "array size missing in `[i]'" n1
    | _ , Tarray (_,_,None) ->
	error Loc.dummy_position "array size missing in `[i]'" n2
    | Tstruct _ , Tstruct _ ->
	let pre = sep ^ n1 ^ "_" ^ n2 in
	let info = Info.default_logic_info (pre) in
	info.logic_heap_args <- pair v1 v2;
	Cenv.add_pred (pre)  ([], info);
	[noattr_located (
	   Cast.Ninvariant_strong (
	     "separation" ^ n1 ^ "_" ^ n2 , 
	     npapp(find_pred (pre),[])))]
    | Tarray (_,ty,Some s) , Tstruct _ -> 
	if compatible_type ty v2.var_type then
	  let pre = sep ^ "_range1_" ^ n1 ^ "_" ^ n2 in 
	  let info = Info.default_logic_info (pre) in
	  info.logic_heap_args <- pair v1 v2; 
	  Cenv.add_pred (pre)  ([], info);
	  [noattr_located 
	     (Cast.Ninvariant_strong (
		"separation_" ^ n1 ^ "_" ^ n2,
		npapp
		  (find_pred (pre ), 
		   [noattr_term 
		      (noattr_type (Tint (Signed,Int)))
		      (NTconstant (IntConstant 
				     (Int64.to_string s)))])))]
	else []
    | Tstruct _ ,Tarray (_,ty,Some s)  -> 
	if compatible_type ty v1.var_type then
	  let pre = sep ^ "_range1_" ^ n1 ^ "_" ^ n2 in 
	  let info = Info.default_logic_info (pre) in
	  info.logic_heap_args <- pair v1 v2; 
	  Cenv.add_pred (pre)  ([], info);
	  [noattr_located
	     (Cast.Ninvariant_strong (
		"separation_" ^ n1 ^ "_" ^ n2,
		npapp(
		  find_pred (pre), 
		  (*(create_term n2)::(create_term n1)::*)
		    [noattr_term 
		       (noattr_type (Tint (Signed,Int)))
		       (NTconstant (IntConstant 
				      ((Int64.to_string s))))])))]
	else []
    | Tarray (_,ty1, Some s1),  Tarray(_,ty2, Some s2) ->
	let make_sub_term p ty v = 
	  let zone = find_zone_for_term p in
	  let () = type_why_new_zone zone v in
	  noattr_term ty (NTarrow (p, zone,v)) in
	if mark then
	  let pre = sep ^ n1 ^ "_" ^ n2 in
	  let info = Info.default_logic_info (pre) in
	  info.logic_heap_args <- pair v1 v2; 
	  Cenv.add_pred (pre)  ([], info);
	  let ty = noattr_type (Tpointer (Not_valid,noattr_type (Tvoid))) in
	  let var = default_var_info (fresh_index()) in
	  set_var_type (Var_info var) ty false;
	  let term = noattr_term ty (NTvar var) in
	  noattr_located (
	    Cast.Ninvariant_strong (
	      "internal_separation" ^ n1 ^ "_" ^ n2 , 
	      npapp(find_pred (pre),[])))::
	    [noattr_located
	       (Cast.Ninvariant_strong (
		  "separation_" ^ n1 ^ "_" ^ n2,
		  make_forall 
		    [ty,var] 
		    (local_separation Loc.dummy_position n1 
		       (make_sub_term term ty1 v1) n2
		       (make_sub_term term ty2 v2))))]
	else
 	  let pre = sep ^ n1 ^ "_" ^ n2 in
	  let info = Info.default_logic_info (pre) in
	  info.logic_heap_args <- pair v1 v2; 
	  Cenv.add_pred (pre)  ([], info);
	  let ty = noattr_type (Tpointer (Not_valid,noattr_type (Tvoid))) in
	  let var1 = default_var_info (fresh_index()) in
	  let var2 = default_var_info (fresh_index()) in
	  set_var_type (Var_info var1) ty false;
	  set_var_type (Var_info var2) ty false;
	  let term1 = noattr_term ty (NTvar var1) in
	  let term2 = noattr_term ty (NTvar var2) in
	  let pred = (local_separation Loc.dummy_position n1 
			(make_sub_term term1 ty1 v1)
			n2 
			(make_sub_term term2 ty2 v2)) in
	  if pred = nptrue then []
	  else
	    noattr_located (
	      Cast.Ninvariant_strong (
		"separation" ^ n1 ^ "_" ^ n2 , 
		npapp(find_pred (pre),[])))::
	      [noattr_located
		 (Cast.Ninvariant_strong 
		    ("separation_" ^ n1 ^ "_" ^ n2,
		     make_forall 
		       [ty,var1]
		       (make_forall 
			  [ty,var2]
			  (if diag 
			   then 
			     make_implies (nprel (term1,Neq,term2)) pred
			 else
			   pred))))]
    | _ , _ -> []
	  

let rec separation_intern n =
  let l =
    begin
      match  tag_type_definition n with
	| TTStructUnion ((Tstruct _),fl) ->
	    fl
	| TTStructUnion (t,fl) -> 
	    begin match t with
	      | Tstruct _ -> assert false
	      | Tfun (_, _) -> assert false
	      | Tenum _ -> assert false
	      | Tunion _ -> assert false
	      | Tpointer (_, _) -> assert false
	      | Tarray (_, _, _) -> assert false
	      | Ctypes.Tvar _ -> assert false
	      | Tfloat _ -> assert false
	      | Tint _ -> assert false
	      | Tvoid -> assert false
	    end
	| TTEnum (_, _) -> assert false
	| TTIncomplete -> assert false
    end  
  in
  let array_intern_separation v1  =
    let n1 = v1.var_unique_name in
    match v1.var_type.Ctypes.ctype_node with
      | Tarray (_,_,None) -> 
	  error Loc.dummy_position "array size missing in `%s[i]'" n1
      | Tarray (_,ty,Some s) ->
	  begin
	    match ty.Ctypes.ctype_node with
	      | Tstruct _ -> 
		  let pre = "%separation1_range_" ^ n1  in 
		  let info = Info.default_logic_info (pre) in
		  info.logic_heap_args <- single v1; 
		  Cenv.add_pred (pre)  ([], info);
		  [noattr_located (Cast.Ninvariant_strong (
				    "internal_separation_" ^ n1 ^ "_array1" , 
				   npapp(
				     find_pred (pre ),
				     (*(create_term n1)::*)
				     [noattr_term (noattr_type 
						     (Tint (Signed,Int))) 
					(NTconstant 
					    (IntConstant 
					       (Int64.to_string s)))])))]
	      | Tarray _ ->	
		 let pre = "%separation1_range_" ^ n1  in 
		 let info = Info.default_logic_info (pre) in
		 info.logic_heap_args <- single v1 ; 
		 Cenv.add_pred (pre)  ([], info);
		 noattr_located (Cast.Ninvariant_strong (
				   "internal_separation_" ^ n1 ^ "_array1" , 
				   npapp(
				     find_pred (pre ),
				     (*(create_term n1)::*)
				      [noattr_term (noattr_type 
						      (Tint (Signed,Int))) 
					 (NTconstant 
					    (IntConstant 
					       (Int64.to_string s)))])))::
		   noattr_located (Cast.Ninvariant_strong (
				     "internal_separation_" ^ n1 ^ "_array2",
				     (make_forall_range Loc.dummy_position 
					(var_to_term Loc.dummy_position v1) s 
					(fun t i -> 
					    separation_intern2 n1 
					      (noattr_term 
						 (noattr_type (Tpointer (Not_valid,ty))) 
						 (NTbinop (t,Badd,i)))))))::[]
	      | _ -> []
	  end
      | _ -> []
  in
  (List.fold_left (fun acc t ->
     array_intern_separation t@acc) [] l) @ 
    (fold_2 (separation_first true false ) l)  


let separation_2_struct s1 l1 s2 l2 acc=
  let l1 = snd l1 in
  let l2 = snd l2 in
  List.fold_left (fun acc1 t1 ->
		    (List.fold_left
		       (fun acc2 t2 ->
			  separation_first false (t1=t2) t1 t2@acc2) acc1 l2)) 
    acc l1

let add_predicates l =
  let f s (ty,fl) l2 = 
    let l2 = List.fold_right 
      (fun f acc -> 
	 begin
	   match f.var_type.Ctypes.ctype_node with
	     | Tstruct n ->
		 let pre = "%valid_acc_" ^ n  in 
		 let info = Info.default_logic_info (pre) in
		 info.logic_heap_args <- single f; 
		 Cenv.add_pred (pre)  ([], info);
		 [noattr_located (
		    Cast.Ninvariant_strong (
		      "valid" ^ n,npapp(find_pred (pre),[])))]
	     | Tarray(_,ty, Some s)->
		 let n1 = f.var_unique_name in
		 let pre = "%valid_acc_" ^ n1 in 
		 let info = Info.default_logic_info (pre) in
		 info.logic_heap_args <- single f; 
		 Cenv.add_pred (pre)  ([], info);
		 let pre2 = "%valid_acc_range_" ^ n1  in 
		 let info2 = Info.default_logic_info (pre2) in
		 info2.logic_heap_args <- single f; 
		 Cenv.add_pred (pre2)  ([], info2);
		 noattr_located (
		 Cast.Ninvariant_strong ("valid_array"^ n1,
					 npapp(find_pred (pre),[])))::
		   noattr_located (
		     Cast.Ninvariant_strong 
		       ("valid_range" ^ n1,
			npapp(find_pred (pre2),
			      [noattr_term (noattr_type (Tint (Signed,Int))) 
				(NTconstant 
				   (IntConstant (Int64.to_string s)))])))::
		   begin
		     match ty.ctype_node with
		       | Tarray (_,ty, None)->
			   error Loc.dummy_position 
			     "array size missing in `%s'" f.var_name
		       | Tarray (_,ty, Some s1) ->
			 [noattr_located (
			    Cast.Ninvariant_strong 
			      ("valid_matrice" ^ n1,
			       make_forall_range 
				 Loc.dummy_position 
				 (var_to_term Loc.dummy_position f) s 
				 (fun t i -> predicate_for f.var_name 
				     (noattr_term (noattr_type (Tpointer (Not_valid,ty))) 
					(NTbinop (t,Badd,i))))))]
		       | _ -> []
		   end
		   
	     | _ -> []
	 end @ acc
      )		 
      fl l2
    in
    (separation_intern s) @ l2
  in
  let l = (fold_all_struct_pairs separation_2_struct l) in
  Cenv.fold_all_struct f l

(* integer sizes (should be moved elsewhere) *)

let string_two_power_n = function
  | 64 -> "18446744073709551616"
  | 63 -> "9223372036854775808"
  | n -> 
      assert (0 <= n && n <= 62); 
      Int64.to_string (Int64.shift_left Int64.one n)

let string_two_power_n_minus_one = function
  | 64 -> "18446744073709551615"
  | 63 -> "9223372036854775807"
  | n -> 
      assert (0 <= n && n <= 62); 
      Int64.to_string (Int64.pred (Int64.shift_left Int64.one n))

let min_int = function
  | Signed, n -> "-" ^ string_two_power_n (n-1)
  | Unsigned, _ -> "0"

let max_int = function
  | Signed, n -> string_two_power_n_minus_one (n-1)
  | Unsigned, n -> string_two_power_n_minus_one n

let min_cinteger (s,ty) = min_int (s, Cenv.int_size ty)
let max_cinteger (s,ty) = max_int (s, Cenv.int_size ty)

(************* DEAD CODE (--typing-predicates) **********************

let rec pred_for_type ty t = 
  match ty.Ctypes.ctype_node with
    | Ctypes.Tstruct n when Hashtbl.mem dummy_is_struct n ->
	npvalid t
    | Ctypes.Tstruct n ->
	let _,info = Cenv.find_pred ("is_struct_" ^ n) in 
	npand (npapp (info, [t]), npvalid t)
    | Ctypes.Tint (_, Ctypes.Bitfield _) ->
	nptrue (* TODO *)
    | Ctypes.Tint si ->
	let _,info = Cenv.find_pred (predicate_for_int_type si) in 
	npapp (info, [t])
    | Ctypes.Tarray (_, ty', Some s) ->
	let var_i = var_i () in
	let tvar_i = nterm (NTvar var_i) c_int in
	let n = ntconstant (Int64.to_string (Int64.pred s)) in
	let t_i = nterm (NTbinop (t, Clogic.Badd, tvar_i)) ty in
	begin
	  match ty'.ctype_node with
	    | Tstruct _ | Tunion _ ->	
		npand (npvalid_range (t, ntzero, n),
		       npforall [c_int,var_i] 
			 (npimp (npand (nprel (ntzero, Le, tvar_i),
					nprel (tvar_i, Le, n)))
			    (pred_for_type ty' t_i)))
	    | _ -> 
		let info = make_field ty' in
		let info = declare_arrow_var info in
		let zone = find_zone_for_term t in
		let () = type_why_new_zone zone info in
		let arrow_t_i = nterm (NTarrow (t_i, zone,info)) ty' in
		npand (npvalid_range (t, ntzero, n),
		       npforall [c_int,var_i] 
			 (npimp (npand (nprel (ntzero, Le, tvar_i),
					nprel (tvar_i, Le, n)))
			    (pred_for_type ty' arrow_t_i)))
	end
    | Ctypes.Tpointer (_, ty') | Ctypes.Tarray (_, ty', None) ->
	let var_i = var_i () in
	let t_i = 
	  nterm (NTbinop (t, Clogic.Badd, nterm (NTvar var_i) c_int)) ty 
	in
	begin 
	  match ty'.ctype_node with
	    | Tstruct _ | Tunion _ ->	
		npforall [c_int,var_i] (npimp (npvalid t_i) 
					  (pred_for_type ty' t_i))
	    | _ -> 
		let info = make_field ty' in
		let info = declare_arrow_var info in
		let zone = find_zone_for_term t in
		let () = type_why_new_zone zone info in
		let arrow_t_i = nterm (NTarrow (t_i, zone,info)) ty' in
		npforall [c_int,var_i] (npimp (npvalid t_i) 
					  (pred_for_type ty' arrow_t_i))
	end
    | Ctypes.Tunion n ->
	nptrue (*TODO*)
    | Ctypes.Tenum n ->
	let _,info = Cenv.find_pred ("is_enum_" ^ n) in 
	npapp (info, [t])
    | Ctypes.Tvoid | Ctypes.Tfun _ | Ctypes.Tfloat _ | Ctypes.Tvar _ -> 
	nptrue

let add_typing_predicates dl =
  let loc = Loc.dummy_position in
  let tdecl d = { node = d; loc = loc } in
  (* 1. define all is_signed_char, ... predicates *)
  let declare_int_type si acc =
    let ty = noattr (Tint si) in
    let n = predicate_for_int_type si in
    let n' = function_for_int_type si in
    let is_int_n = Info.default_logic_info n in
    let any_int_n' = Info.default_fun_info n' in
    let result = {nterm_node = NTvar(Info.default_var_info "result");
		  nterm_loc = Loc.dummy_position ;
		  nterm_type = ty}
    in
    let spec_n' = { requires = None;
		    assigns = None;
		    ensures = Some (npapp (is_int_n, [result]));
		    decreases = None} 
    in
    Cenv.add_c_fun n' (spec_n',ty,any_int_n',None,Loc.dummy_position);
    Cenv.add_pred n ([ty], is_int_n);
    let x = Info.default_var_info "x" in
    set_formal_param x;
    set_var_type (Var_info x) ty true;
    is_int_n.logic_args <- [x];
    let var_x = nterm (NTvar x) ty in
    let min_si = ntconstant (min_cinteger si) in
    let max_si = ntconstant (max_cinteger si) in
    let p = npand (nprel (min_si, Le, var_x),
		   nprel (var_x, Le, max_si)) in
    let d = tdecl (Nlogic (is_int_n, NPredicate_def ([x,ty], p))) in
    d :: acc
  in
  (* 2. define all is_enum_E predicates *)
  let declare_enum_type s (tyn, vl) acc =
    let ty = noattr tyn in
    let n = "is_enum_" ^ s in
    let n' = "any_enum_" ^ s in
    let is_enum_s = Info.default_logic_info n in   
    let any_enum_n' = Info.default_fun_info n' in    
    let result = {nterm_node = NTvar(Info.default_var_info "result");
		  nterm_loc = Loc.dummy_position ;
		  nterm_type = ty}
    in
    let spec_n' = { requires = None;
		    assigns = None;
		    ensures = Some (npapp (is_enum_s, [result]));
		    decreases = None} 
    in    
    Cenv.add_c_fun n' (spec_n',ty,any_enum_n',None,Loc.dummy_position);
    Cenv.add_pred n ([ty], is_enum_s);
    let x = Info.default_var_info (get_fresh_name "x") in
    set_formal_param x;
    set_var_type (Var_info x) ty true;
    is_enum_s.logic_args <- [x];
    let var_x = nterm (NTvar x) ty in
    let p = 
      List.fold_left 
	(fun p (v,_) -> 
	   let p1 = nprel (var_x, Eq, nterm (NTvar v) ty) in
	   npor (p, p1)) 
	npfalse vl
    in
    let d = tdecl (Nlogic (is_enum_s, NPredicate_def ([x,ty], p))) in
    d :: acc
  in
  (* 3. declare all is_struct_S predicates *)
  let declare_is_struct s (tyn, fl) acc = 
    let ty = noattr tyn in
    let n = "is_struct_" ^ s in
    let is_struct_s = Info.default_logic_info n in
    Cenv.add_pred n ([ty], is_struct_s);
    let x = Info.default_var_info "x" in
    set_formal_param x;
    set_var_type (Var_info x) ty true;
    is_struct_s.logic_args <- [x];
    let varx = nterm (NTvar x) ty in
    let reads = (* reads = x.f1, ..., x.fn *)
      List.map (fun f ->       
		  let zone = find_zone_for_term varx in
		  let () = type_why_new_zone zone f in
		  nterm (NTarrow (varx,zone, f)) f.var_type)
	fl
    in
    let d = tdecl (Nlogic (is_struct_s, NPredicate_reads ([x,ty], reads))) in
    d :: acc
  in
  (* 4. axiomatize all is_struct_S predicates *)
  let define_is_struct s (tyn,fl) acc =
    reset_var_i ();
    let _,is_struct_s = Cenv.find_pred ("is_struct_" ^ s) in
    let x = match is_struct_s.logic_args with [x] -> x | _ -> assert false in
    let ty = noattr tyn in
    let varx = nterm (NTvar x) ty in
    let ax = 
      let def = 
	List.fold_left
	  (fun acc f -> 
	     let zone = find_zone_for_term varx in
	     let () = type_why_new_zone zone f in
	     let t = nterm (NTarrow (varx, zone, f)) f.var_type in
	     npand (acc, pred_for_type f.var_type t))
	  nptrue fl
      in
      if def.npred_node = NPtrue then begin
	Hashtbl.add dummy_is_struct s ();
	[]
      end else 
	let p = npforall [ty,x] (npiff (npapp (is_struct_s, [varx]), def)) in
	[tdecl (Naxiom ("is_struct_" ^ s ^ "_def", p))]
    in
    acc @ ax  
  in
  (* 3. add typing predicates for input variables *)
  let adding_typing_invariant_requires fun_name (sp, ty, f, st, loc) =
    let requires = 
      begin 
	match sp.requires with
	  | None -> nptrue
	  | Some p -> p
      end 
    in
    let requires =
      List.fold_left (fun acc arg -> 
			let arg_term = nterm (NTvar arg) arg.var_type in
			npand (acc ,pred_for_type arg.var_type arg_term))
	requires f.args
    in
    let requires = 
      if requires.Clogic.npred_node = NPtrue then
	None
      else
	Some requires
    in
    sp.requires <- requires
  in
  let dl = 
    List.fold_right declare_int_type
      [Signed, Char; Unsigned, Char;
       Signed, Short; Unsigned, Short;
       Signed, Int; Unsigned, Int;
       Signed, Long; Unsigned, Long;
       Signed, LongLong; Unsigned, LongLong;
      ] 
      dl
  in
  let dl = Cenv.fold_all_enum declare_enum_type dl in
  let dl = Cenv.fold_all_struct declare_is_struct dl in
  let dl = Cenv.fold_all_struct define_is_struct dl in
  Hashtbl.iter  adding_typing_invariant_requires Cenv.c_functions;
  
  (*let dl = List.fold_right add_invariant_for_global dl [] in*)
  dl





**********************************************************************)
