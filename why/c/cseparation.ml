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

open Creport
open Cast
open Info
open Clogic
open Cnorm
open Cenv



(* Automatic invariants expressing validity of local/global variables *)

open Clogic
open Ctypes

let tpred t = match t.nterm_node with
  | NTconstant (IntConstant c) -> 
      let c = string_of_int (int_of_string c - 1) in
      { t with nterm_node = NTconstant (IntConstant c) }
  | _ ->
      { t with nterm_node = NTbinop (t, Bsub, int_nconstant "1") }

let make_valid_range_from_0 t ts=
  if ts = Int64.one
  then
    npvalid t
  else
    npvalid_range (t, nzero, int_nconstant (Int64.to_string (Int64.pred ts)))


let fresh_index = 
  let r = ref (-1) in fun () -> incr r; "index_" ^ string_of_int !r


let indirection loc ty t =
  let info = make_field ty in
  let info = declare_arrow_var info in
  let zone = find_zone_for_term t in
  let () = type_why_new_zone zone info in
  { nterm_node =   NTarrow (t, zone, info);
    nterm_loc = loc; 	   
    nterm_type = ty;}
    
(*
  [make_forall_range loc t b f] builds the formula

   forall i, 0 <= i < b -> (f t i)

  unless b is 1, in which case it produces (f t 0)

*)
let make_forall_range loc t b f =
  if b = Int64.one
  then f t nzero
  else
    let i = default_var_info (fresh_index ()) in
    set_var_type (Var_info i) c_int false;
    let vari = { nterm_node = NTvar i; 
		 nterm_loc = loc;
		 nterm_type = c_int;} in
    let ti = 
      { nterm_node = NTbinop (t, Badd, vari); 
	nterm_loc = loc;
	nterm_type = t.nterm_type}
    in
    let ineq = npand (nprel (nzero, Le, vari),
		      nprel (vari, Lt, int_nconstant (Int64.to_string b))) in
    make_forall [c_int, i] (make_implies ineq (f ti vari))

let valid_for_type ?(fresh=false) loc name (t : Cast.nterm) =
  let rec valid_fields valid_for_current n (t : Cast.nterm) = 
    begin match tag_type_definition n with
      | TTStructUnion (Tstruct (_), fl) ->
	  List.fold_right 
	    (fun f acc -> 
	       let zone = find_zone_for_term t in
	       let () = type_why_new_zone zone f in
	       let tf = 
		 { nterm_node = NTarrow (t, zone, f); 
		   nterm_loc = loc;
		   nterm_type = f.var_type} 
	       in
	       make_and acc (valid_for tf))
	    fl 
	    (if valid_for_current then 
	       if fresh then npand(npvalid t, npfresh t) else npvalid t 
	     else nptrue)
      | TTIncomplete ->
	  error loc "`%s' has incomplete type" name
      | _ ->
	  assert false
    end
  and valid_for (t : Cast.nterm) = match t.nterm_type.Ctypes.ctype_node with
    | Tstruct n ->
 	valid_fields true n t
    | Tarray (_, ty, None) ->
	error loc "array size missing in `%s'" name    
    | Tarray (Not_valid,_,_) -> assert false
    | Tarray (Valid(i,j), ty, Some s) ->
	assert (i <= Int64.zero && j > Int64.zero);
	let vrange = make_valid_range_from_0 t s in
	let valid_form =
	  make_and
	    vrange
	    (if fresh then npfresh t else nptrue)
	in		   
	begin match ty.Ctypes.ctype_node with
	  | Tstruct n ->	      
	      let vti t i = valid_fields false n t in
	      make_and valid_form (make_forall_range loc t s vti)
	  | _ ->
	      make_and valid_form
		(make_forall_range loc t s 
		   (fun t i -> valid_for 
			(indirection loc ty t)))
	end
    | _ -> 
	nptrue
  in
  valid_for t

let not_alias loc x y = 
  if Info.same_why_type (type_why_for_term x) (type_why_for_term y)
  then
    let ba t = { nterm_node = NTbase_addr t; 
		 nterm_loc = loc;
		 nterm_type = c_addr} in 
    nprel (ba x, Neq, ba y)
  else
    {npred_node = NPtrue;  npred_loc = loc}

let var_to_term loc v =
  {
    nterm_node = NTvar v; 
    nterm_loc = loc;
    nterm_type = v.var_type}

let in_struct v1 v = 
  (*  match v1.nterm_node with
    | NTarrow(x,ty,_,_) ->
    | _ -> *)
  let zone = find_zone_for_term v1 in
  let () = type_why_new_zone zone v in
  { nterm_node = NTarrow (v1, zone, v); 
    nterm_loc = v1.nterm_loc;
    nterm_type = v.var_type}

	
let compatible_type ty1 ty2 = 
  match ty1.Ctypes.ctype_node,ty2.Ctypes.ctype_node with
    | Tfun _ , _  | Tenum _, _ | Tpointer _ , _ 
    | Ctypes.Tvar _ , _ | Tvoid, _ | Tint _, _ | Tfloat _, _ -> false
    | _, Tfun _ | _, Tenum _| _, Tpointer _  
    | _, Ctypes.Tvar _ | _, Tvoid | _, Tint _ | _, Tfloat _ -> false
    | _, _ -> true 

let full_compatible_type ty1 ty2 = 
  match ty1.Ctypes.ctype_node,ty2.Ctypes.ctype_node with
    | Tfun _ , _  | Tenum _, _  
    | Ctypes.Tvar _ , _ | Tvoid, _ | Tint _, _ | Tfloat _, _ -> false
    | _, Tfun _ | _, Tenum _  
    | _, Ctypes.Tvar _ | _, Tvoid | _, Tint _ | _, Tfloat _ -> false
    | _, _ -> true

(* assumes v2 is an array of objects of type ty *)
let rec tab_struct mark loc v1 v2 s ty n n1 n2=
  let l = begin
    match  tag_type_definition n with
      | TTStructUnion ((Tstruct _),fl) ->
	  fl
      | _ -> assert false
  end in
  if mark then
    List.fold_left 
      (fun p t -> 
	 if  compatible_type t.var_type v2.nterm_type 
	 then make_and p (not_alias loc v2 (in_struct v1 t))
	 else p)
      nptrue l
  else
  make_and (List.fold_left 
	      (fun p t -> 
		 if  compatible_type t.var_type v2.nterm_type 
		 then make_and p (not_alias loc v2 (in_struct v1 t))
		 else p)
	      nptrue l)
    (make_forall_range loc v2 s 
       (fun t i -> 
	  local_separation mark loc n1 v1 (n2^"[i]") (indirection loc ty t)))

and local_separation  mark loc n1 v1 n2 v2 =
  match (v1.nterm_type.Ctypes.ctype_node,v2.nterm_type.Ctypes.ctype_node) 
  with
    | Tarray (_,ty, None), _ ->
	error loc "array size missing in `%s'" n1
    | _, Tarray (_,ty, None) ->
	error loc "array size missing in `%s'" n2
    | Tstruct n , Tarray (_,ty,Some s) -> 
	tab_struct  mark loc v1 v2 s ty n n1 n2
    | Tarray (_,ty,Some s) , Tstruct n -> 
	tab_struct mark loc v2 v1 s ty n n1 n2
    | Tarray (_,ty1,Some s1), Tarray(_,ty2,Some s2) ->
	make_and
	  (if compatible_type v1.nterm_type v2.nterm_type
	   then
	     (not_alias loc v1 v2)
	   else
	     nptrue)
	  (make_and 
	     (make_forall_range loc v1 s1 
		(fun t i -> local_separation mark loc (n1^"[i]") 
		     (indirection loc ty1 t) n2 v2))
	     (make_forall_range loc v2 s2  
		(fun t i -> local_separation true loc n1 v1 (n2^"[j]")
		     (indirection loc ty2 t))))
     | _, _ -> nptrue

    
let separation loc v1 v2 =
  local_separation false loc v1.var_name (var_to_term loc v1) 
    v2.var_name (var_to_term loc v2)

let rec full_tab_struct mark loc v1 v2 s ty n n1 n2=
  let l = begin
    match  tag_type_definition n with
      | TTStructUnion ((Tstruct _),fl) ->
	  fl
      | _ -> assert false
  end in
  if mark then
    List.fold_left 
      (fun p t -> 
	 if  full_compatible_type t.var_type v2.nterm_type 
	 then make_and p (not_alias loc v2 (in_struct v1 t))
	 else p)
      nptrue l
  else
  make_and (List.fold_left 
	      (fun p t -> 
		 if  full_compatible_type t.var_type v2.nterm_type 
		 then make_and p (not_alias loc v2 (in_struct v1 t))
		 else p)
	      nptrue l)
    (make_forall_range loc v2 s 
       (fun t i -> 
	  full_local_separation mark loc n1 v1 (n2^"[i]") (indirection loc ty t)))

and full_local_separation  mark loc n1 v1 n2 v2 =
  match (v1.nterm_type.Ctypes.ctype_node,v2.nterm_type.Ctypes.ctype_node) 
  with
    | Tarray (_,ty, None), _ ->
	error loc "array size missing in `%s'" n1
    | _, Tarray (_,ty, None) ->
	error loc "array size missing in `%s'" n2
    | Tstruct n , Tarray (_,ty,Some s) -> 
	full_tab_struct  mark loc v1 v2 s ty n n1 n2
    | Tarray (_,ty,Some s) , Tstruct n -> 
	full_tab_struct mark loc v2 v1 s ty n n1 n2
    | Tarray (_,ty1,Some s1), Tarray(_,ty2,Some s2) ->
	make_and
	  (if full_compatible_type v1.nterm_type v2.nterm_type
	   then
	     (not_alias loc v1 v2)
	   else
	     nptrue)
	  (make_and 
	     (make_forall_range loc v1 s1 
		(fun t i -> full_local_separation mark loc (n1^"[i]") 
		     (indirection loc ty1 t) n2 v2))
	     (make_forall_range loc v2 s2  
		(fun t i -> full_local_separation true loc n1 v1 (n2^"[j]")
		     (indirection loc ty2 t))))
    | Tpointer (_,ty1) , Tpointer (_,ty2) ->
	if full_compatible_type v1.nterm_type v2.nterm_type
	then
	  (not_alias loc v1 v2)
	else
	  nptrue
    | Tarray (_,ty2,Some s2) ,  Tpointer (_,ty1)
    | Tpointer (_,ty1), Tarray (_,ty2,Some s2) ->
	make_and
	  (if full_compatible_type v1.nterm_type v2.nterm_type
	   then
	     (not_alias loc v1 v2)
	   else
	     nptrue)
	  (make_forall_range loc v2 s2  
	     (fun t i -> full_local_separation true loc n1 v1 (n2^"[j]")
		(indirection loc ty2 t)))
    | Tstruct n, Tpointer (_,ty)  ->
	 let l = begin
	   match  tag_type_definition n with
	     | TTStructUnion ((Tstruct _),fl) ->
		 fl
	     | _ -> assert false
	 end in 
	 (List.fold_left 
	    (fun p t -> 
	       make_and p (full_local_separation mark loc n2 v2 n1 
			     (in_struct v1 t)))
	    nptrue l)
    |  Tpointer (_,ty), Tstruct n ->
	 let l = begin
	   match  tag_type_definition n with
	     | TTStructUnion ((Tstruct _),fl) ->
		 fl
	     | _ -> assert false
	 end in 
	 (List.fold_left 
	    (fun p t -> 
	       make_and p (full_local_separation mark loc n1 v1 n2 
			     (in_struct v2 t)))
	    nptrue l)
    | Tstruct n1, Tstruct n2 ->
	let l2 = begin
	   match  tag_type_definition n2 with
	     | TTStructUnion ((Tstruct _),fl) ->
		 fl
	     | _ -> assert false
	 end in	
	let l1 = begin
	   match  tag_type_definition n1 with
	     | TTStructUnion ((Tstruct _),fl) ->
		 fl
	     | _ -> assert false
	end in
	(List.fold_left 
	    (fun p1 t1 ->
	       (List.fold_left 
		  (fun p2 t2 ->
		     make_and p2 (full_local_separation mark loc n1 
				    (in_struct v1 t1) 
				    n2  (in_struct v2 t2)))
		  p1 l2))
		 nptrue l1)
    | _, _ -> nptrue

let fullseparation loc v1 v2 =
  full_local_separation false loc v1.var_name (var_to_term loc v1) 
    v2.var_name (var_to_term loc v2)


let rec rehash z1 z2 =
  let t =
  (try 
    let t2 = Hashtbl.find type_why_table z2 in
    Hashtbl.remove type_why_table z2;
    (try 
       let t1 = Hashtbl.find type_why_table z1 in
       Hashtbl.remove type_why_table z1;
       Hashtbl.iter 
	 (fun a1 tw2 ->
	    try 
	      begin 
		let tw1 = Hashtbl.find t1 a1 in
		unifier_type_why tw1 tw2
	      end
	    with Not_found -> Hashtbl.add t1 a1 tw2
	 )
	 t2;
       t1
     with Not_found -> t2)
   with Not_found -> 
     try 
       Hashtbl.find type_why_table z1
     with Not_found -> Hashtbl.create 5) 
  in
  Hashtbl.add type_why_table z1 t;
  Hashtbl.add type_why_table z2 t
    
and unifier_type_why tw1 tw2 =
  match tw1,tw2 with
    | Pointer z1 , Pointer z2 ->
	unifier_zone z1 z2     
    | Addr z1 , Addr z2 ->
	unifier_zone z1 z2
    | Why_Logic s1, Why_Logic s2 when s1 = s2 -> 
	()
    (* int types *)
    | Info.Int, Info.Int -> ()
    | Why_Logic s1, Info.Int when is_int_type s1 -> ()
    | Info.Int, Why_Logic s2 when is_int_type s2 -> ()
    | Why_Logic s1, Why_Logic s2 when is_int_type s1 && is_int_type s2 -> ()
    (* float types *)
    | Why_Logic "double", Why_Logic "real"
    | Why_Logic "single", Why_Logic "real" 
    | Why_Logic "real", Why_Logic "single"
    | Why_Logic "real", Why_Logic "double" -> ()
    (* errors *)
    | Memory _, _ | _, Memory _ -> assert false
    | _ ->
	let t1 = output_why_type tw1 
	and t2 = output_why_type tw2
	in
	Format.eprintf "anomaly: unify why types `%a' and `%a'@."
	  Output.fprintf_logic_type t1 Output.fprintf_logic_type t2;
	raise Not_found
	  

and unifier_zone z1 z2 =
  let z1' = Info.repr z1
  and z2' = Info.repr z2
  in
  if z1' == z2' then ()
  else
    begin
      rehash z1' z2'; 
      match z1'.repr, z2'.repr with
	| None, None -> 
	    if z1'.zone_is_var then z1'.repr <- Some z2' else 
	      if z2'.zone_is_var then z2'.repr <- Some z1' else
	      if z1'.number < z2'.number then z2'.repr <- Some z1'
	      else z1'.repr <- Some z2' 
	| _ -> assert false
    end

let loc_name loc =
  let (f,l,fc,lc) = Loc.extract loc in
  Format.sprintf "line %d, characters %d-%d" l fc lc

let unifier_type_why ?(var_name="?") tw1 tw2 =
  try
    unifier_type_why tw1 tw2
  with
      e ->
	Format.eprintf "Anomaly in unifier_type_why for var '%s'@." var_name;
	raise e

let assoctype  ty assoc = 
  match ty with 
    |  Pointer z ->
	 let z = repr z in
	 let z  = try Cnorm.assoc_zone z assoc with Not_found -> z in
	 Pointer z 
    | _ -> ty

let copyhash z za assoc =
  try let t = Hashtbl.copy (Hashtbl.find type_why_table z) in
  Hashtbl.iter (fun x y -> Hashtbl.replace t x (assoctype y assoc) ) t;
(*  Hashtbl.iter
    (fun f tw -> 
       let l,n = output_why_type tw in
       Format.eprintf "adding in type_why_table :(%s,%s) -> %s@." 
	 za.name f.var_name n)
    t;*)
  Hashtbl.add type_why_table za t
  with Not_found -> () 

let rec term tyf t =
  match t.nterm_node with
    | NTconstant _ -> () 
    | NTvar v -> 
	if v.var_name = "result" then 
	    unifier_type_why ~var_name:v.var_name v.var_why_type tyf
    | NTapp ({napp_pred = f;napp_args = l} as call) ->
      List.iter (term tyf) l;
      let assoc = List.map (fun z -> (z,make_zone true)) f.logic_args_zones in
      call.napp_zones_assoc <- assoc;
      List.iter (fun (x,y) ->
		   let x = repr x  in
		   copyhash x y assoc) assoc ;
      let li =  
	List.map 
	  (fun v ->
	     let t =
	     match v.var_why_type with
	       | Pointer z as ty -> 
		   begin
		     try
		       let z = repr z in
		       Pointer (assoc_zone z assoc)
		     with
			 Not_found -> ty
		   end
	       | ty -> ty in v,t)
	    f.logic_args in

      assert (List.length li = List.length l || 
	  (Format.eprintf " wrong arguments for %s : expected %d, got %d\n" 
	     f.logic_name (List.length li) (List.length l); false));
      begin
	try      
	  List.iter2 
	    (fun (v,ty) e -> 
	       unifier_type_why ~var_name:v.var_name ty (type_why_for_term e)) 
	    li l
	with Invalid_argument _ -> assert false
      end
  | NTunop (_,t) -> term tyf t 
  | NTbinop (t1,_,t2)
  | NTmin (t1,t2)
  | NTmax (t1,t2) -> term tyf t1; term tyf t2 
  | NTarrow (t,_,_) -> term tyf t
  | NTif (t1,t2,t3) -> term tyf t1; term tyf t2; term tyf t3
  | NTold t 
  | NTat (t,_) 
  | NTbase_addr t
  | NToffset t
  | NTblock_length t 
  | NTarrlen t 
  | NTstrlen (t,_,_)
  | NTcast (_,t) 
  | NTrange (t,None,None,_,_) -> term tyf t
  | NTrange (t1,Some t2,None,_,_) | NTrange (t1,None,Some t2,_,_) -> 
      term tyf t1; term tyf t2
  | NTminint _ | NTmaxint _ ->
      ()
  | NTrange (t1,Some t2,Some t3,_,_) -> term tyf t1; term tyf t2; term tyf t3

let rec predicate tyf p =
  match p.npred_node with
  | NPfalse
  | NPtrue -> ()
  | NPapp ({napp_pred = f;napp_args = l} as call) -> 
      List.iter (term tyf) l;
      let assoc = List.map (fun z -> (z,make_zone true)) f.logic_args_zones in
      call.napp_zones_assoc <- assoc;
      List.iter (fun (x,y) ->
		   let x = repr x  in
		   copyhash x y assoc) assoc ;
      let li =  
	List.map 
	  (fun v ->
	     let t =
	       match v.var_why_type with
		 | Pointer z as ty -> 
		     begin
		       try
			 let z = repr z in
			 Pointer (assoc_zone z assoc)
		       with
			   Not_found -> ty
		     end
		 | ty -> ty in v,t )
	    f.logic_args in

      assert (List.length li = List.length l || 
	  (Format.eprintf " wrong arguments for %s : expected %d, got %d\n" 
	     f.logic_name (List.length li) (List.length l); false));
      begin 
	try      List.iter2 
	(fun (v,ty) e ->
	   unifier_type_why ~var_name:v.var_name ty (type_why_for_term e)) li l
      with Invalid_argument _ -> assert false
      end
  | NPrel (t1,op,t2) ->      
      term tyf t1; 
      term tyf t2;
      unifier_type_why ~var_name:(loc_name t1.nterm_loc) (type_why_for_term t1)
	(type_why_for_term t2)      
  | NPand (p1,p2) 
  | NPor (p1,p2) 
  | NPimplies (p1,p2) 
  | NPiff (p1,p2) -> predicate tyf p1; predicate tyf p2
  | NPnot p -> predicate tyf p
  | NPif (t,p1,p2) -> term tyf t; predicate tyf p1; predicate tyf p2
  | NPforall (_,p) 
  | NPexists (_,p) 
  | NPold p 
  | NPat (p,_) -> predicate tyf p
  | NPvalid t -> term tyf t
  | NPvalid_index (t1,t2) -> term tyf t1; term tyf t2
  | NPvalid_range (t1,t2,t3) -> term tyf t1; term tyf t2; term tyf t3
  | NPfresh  t -> term tyf t
  | NPnamed (_,p) -> predicate tyf p
  | NPseparated (t1,t2) 
  | NPfull_separated (t1,t2) -> term tyf t1; term tyf t2
  | NPbound_separated (t1,t2,t3,t4) ->
      term tyf t1; term tyf t2; term tyf t3; term tyf t4

let rec calcul_zones expr =
  match expr.nexpr_node with 
    | NEnop -> ()
    | NEconstant _ 
    | NEstring_literal _ 
    | NEvar _ -> ()
    | NEarrow (e,_,_) -> calcul_zones e
    | NEseq (e1,e2) -> calcul_zones e1; calcul_zones e2
    | NEassign_op (lv,_,e) -> () (* no 2 pointers here *)
    | NEassign (lv,e) -> calcul_zones lv; calcul_zones e;
(*
	Format.eprintf "lv = %a, e = %a@."
	  Cprint.nexpr lv Cprint.nexpr e;
*)
	let tw1 = type_why lv in
	let tw2 = type_why e in
	unifier_type_why ~var_name:(loc_name lv.nexpr_loc) tw1 tw2
    | NEunary (_,e) -> calcul_zones e
    | NEincr (_,e) -> calcul_zones e
    | NEbinary (e1,Bsub_pointer,e2) | NEbinary (e1,Blt_pointer,e2) 
    | NEbinary (e1,Bgt_pointer,e2) | NEbinary (e1,Ble_pointer,e2) 
    | NEbinary (e1,Bge_pointer,e2) | NEbinary (e1,Beq_pointer,e2) 
    | NEbinary (e1,Bneq_pointer,e2)   -> calcul_zones e1; calcul_zones e2;
	let tw1 = type_why e1 in
	let tw2 = type_why e2 in
	unifier_type_why ~var_name:(loc_name e1.nexpr_loc) tw1 tw2
    | NEbinary (e1,_,e2) -> calcul_zones e1; calcul_zones e2
    | NEcall ({ncall_fun = e;ncall_args = l} as call) -> 
	List.iter calcul_zones l;
	let f = match e.nexpr_node with 
	  | NEvar (Fun_info f) -> f
	  | _  -> assert false 
	in
	let assoc = List.map (fun z ->(z,make_zone true)) f.args_zones in
	call.ncall_zones_assoc <- assoc;
	List.iter (fun (x,y) ->
		     let x = repr x  in
		     copyhash x y assoc) assoc ;
	let arg_types =
	  List.map 
	    (fun v ->
	       let t =
	       match v.var_why_type with
		 | Pointer z as ty -> 
		     begin
		       try
			 let z = repr z in
			 Pointer (assoc_zone z assoc)
		       with
			   Not_found -> ty
		     end
		 | ty -> ty in v,t)
	    f.args 
	in
	begin
	  try List.iter2 (fun (v,ty) e -> unifier_type_why ~var_name:v.var_name ty 
			(type_why e))
	  arg_types l
	with Invalid_argument _ -> 
	   warning expr.nexpr_loc "Call to variable args function"
	end
    | NEcond (e1,e2,e3)->  calcul_zones e1; calcul_zones e2; calcul_zones e3
    | NEcast (_,e) -> calcul_zones e
    | NEmalloc (_,e) -> calcul_zones e
 
let rec c_initializer ty tw init =
  match init with 
    | Iexpr e -> 
	calcul_zones e; 
	let twe = type_why e in
	unifier_type_why ~var_name:(loc_name e.nexpr_loc) tw twe
    | Ilist l -> 
	match ty.ctype_node with  
	  | Tstruct tag  ->
	      begin
		match tag_type_definition tag with
		  | TTStructUnion(ty,fields) ->
		      begin
			try List.iter2
			(fun f v ->
			    c_initializer f.var_type f.var_why_type v)
			fields l
		      with Invalid_argument _ -> assert false
		      end
		  | _ -> assert false
	      end
	  | Tarray (_,ty,_) ->	      
	      let tw = type_type_why ty false in
	      List.iter (fun init -> c_initializer ty tw init) l 
	  | _ -> assert false


let option_iter f x =
  match x with
    | None -> ()
    | Some x -> f x

let loop_annot tyf la =
  option_iter (predicate tyf) la.invariant;
  option_iter (fun (_,l) -> List.iter (term tyf) l) la.loop_assigns;
  option_iter (fun (t,_) -> term tyf t) la.variant

let spec tyf sp =
  begin
  match sp.requires with
    | None -> ()
    | Some p -> 
	predicate tyf p
  end;
  begin
  match sp.assigns with
    | None -> ()
    | Some (_,l) -> List.iter (term tyf) l
  end;
  begin
    match sp.ensures with
      | None -> ()
      | Some p -> predicate tyf p
  end;
  begin
    match sp.decreases with
      | None -> ()
      | Some (t,_) -> term tyf t
  end

let rec statement twf st =
  match st.nst_node with 
    | NSnop | NSreturn None | NSbreak | NScontinue | NSlogic_label _ 
    | NSgoto _ -> ()
    | NSassert p | NSassume p ->  predicate twf p
    | NSexpr e -> calcul_zones e
    | NSif (e,st1,st2) -> calcul_zones e; statement twf st1; statement twf st2 
    | NSwhile (lannot,e,st) 
    | NSdowhile (lannot,st,e)-> 
	loop_annot twf lannot; calcul_zones e;statement twf st
    | NSfor (lannot, e1, e2, e3, st)-> 
	loop_annot twf lannot;
	calcul_zones e1; calcul_zones e2; 
	calcul_zones e3; statement twf st
    | NSblock ls -> List.iter (statement twf) ls
    | NSreturn (Some e) -> calcul_zones e ;
	unifier_type_why ~var_name:(loc_name e.nexpr_loc) twf (type_why e)
    | NSlabel (_,st) -> statement twf st
    | NSswitch (e1, e2, l) -> calcul_zones e1;
	List.iter (fun (x, y) -> List.iter (statement twf) y) l
    | NSspec (sp,st) -> spec twf sp; statement twf st
    | NSdecl (_, v, None, st) -> statement twf st
    | NSdecl (_, v, Some i, st) -> 
	c_initializer v.var_type v.var_why_type i;
	statement twf st 
	

module Hzone = Hashtbl.Make(struct
			      type t = zone
			      let equal z1 z2 = z1.number == z2.number
			      let hash z = z.number 
			    end)

let rec add_zone ty l =
(*  Format.eprintf "add_zone ty=%s@." (snd (output_why_type ty));*)
  match ty with
    | Pointer z ->
	let z = repr z in
	if List.mem z l then l else
	  begin match z.repr with 
	    | None ->
		begin
		  try 
		    let t =  Hashtbl.find type_why_table z in
		    Hashtbl.fold (fun _ tw  l ->
				    add_zone tw l) t (z::l)
		  with Not_found -> z::l
	      end
	    | Some _ -> l
	  end
    | _ -> l
      
let collect_zones args ret_type =	
  let l =
    List.fold_left  
      (fun l v ->
	 add_zone v.var_why_type l) [] args
  in
  add_zone ret_type l


let global_decl e =
  match e with 
    | Naxiom (_,sp) | Ninvariant (_,sp) | Ninvariant_strong (_,sp) ->
	predicate Unit sp
    | Nlogic (f, NPredicate_def (_,p)) -> 
	predicate Unit p;
	f.logic_args_zones <- collect_zones f.logic_args f.logic_why_type
    | Nlogic (f, NFunction_def (_,_,t)) ->
	term Unit t;
	f.logic_args_zones <- collect_zones f.logic_args f.logic_why_type
    | Nlogic (f, (NPredicate_reads _ | NFunction _)) -> 
	f.logic_args_zones <- collect_zones f.logic_args f.logic_why_type
(*    | Nfunspec (sp,_,f) -> 
	spec f.type_why_fun sp;
	if f.args_zones = [] then f.args_zones <- collect_zones f.args f.type_why_fun
*)
    | Ntypedef _ | Ntypedecl _ | Ndecl (_,_,None) | Ntype _ -> ()
    | Ndecl (_, v, Some i) -> 
	c_initializer v.var_type v.var_why_type i
(*    | Nfundef (sp, _, f, st) -> 
	spec f.type_why_fun sp; 
	statement f.type_why_fun st; 
	if f.args_zones = [] then f.args_zones <- collect_zones f.args f.type_why_fun
*)

let c_fun_poly fun_name (_, _, f, _,_) =
  if f.args_zones = [] then 
    begin
      let l = collect_zones f.args f.type_why_fun in
      Coptions.lprintf "Why polymorphic zones for function %s:@." f.fun_name;
      List.iter
	(fun z -> Coptions.lprintf "%s " z.name) l;
      Coptions.lprintf "@.";
      f.args_zones <- l
    end

let c_fun_separation fun_name (sp, _, f, st,_) =
  spec f.type_why_fun sp;
  begin
    match st with
      | None -> ()
      | Some st  -> statement f.type_why_fun st
  end 



let file p =  
  List.iter (fun d -> global_decl d.node) p
    
let funct l =
  List.iter 
    (fun f -> 
	let fu = try find_c_fun f.fun_name with Not_found -> assert false in
        c_fun_separation f.fun_name fu;
	c_fun_poly f.fun_name fu)
    l
(*  Hashtbl.iter c_fun_poly Cenv.c_functions;
  Hashtbl.iter c_fun_separation Cenv.c_functions;
*)

(*
Local Variables: 
compile-command: "make -j -C .. bin/caduceus.byte"
End: 
*)
