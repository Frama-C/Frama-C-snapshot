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

(*i $Id: ceffect.ml,v 1.168 2008/05/28 14:53:34 marche Exp $ i*)

open Cast
open Cnorm
open Coptions
open Clogic
open Creport
open Info
open Format
open Pp
open Output
open Ctypes
open Cenv
open Cseparation

let memory_type t1 t2 = ([t1;t2],"memory")

let heap_vars = Hashtbl.create 97

let heap_vars2 = Hashtbl.create 97

let print_heap_vars fmt () =
(*
  let base_type fmt = function
    | [], s -> fprintf fmt "%s" s
    | [x], s -> fprintf fmt "%s %s" x s
    | l, s -> fprintf fmt "(%a) %s" (print_list comma pp_print_string) l s
  in
*)
  fprintf fmt "@[";
  Hashtbl.iter 
    (fun s t -> fprintf fmt "(%s:%a)" s Output.fprintf_logic_type
       (Info.output_why_type t.var_why_type)) 
    heap_vars;
  fprintf fmt "@]"

let print_effects fmt l =
  fprintf fmt "@[%a@]"
    (print_list space (fun fmt v -> pp_print_string fmt v.var_unique_name)) 
    (HeapVarSet.elements l)

let print_effects2 fmt l =
  fprintf fmt "@[%a@]"
    (print_list space (fun fmt (z,s,_) ->let z = repr z in
		       fprintf fmt " %s_%s_%d " s z.name z.number)) 
    (ZoneSet.elements l)

let print_effects3 fmt l =
  fprintf fmt "@[%a@]"
    (print_list space (fun fmt (z,s,_) -> let z = repr z in
		       fprintf fmt " %s_%s:%b " s z.name z.zone_is_var)) 
    (ZoneSet.elements l)

let alloc = 
  let x = "alloc" in
  let v = (*

    Cenv.add_sym 
      Loc.dummy_position x Ctypes.c_void *)
    (Var_info (default_var_info x)) 
  in
  set_var_type_why v (Why_Logic "alloc_table");
  match v with
    | Var_info v -> v
    | Fun_info _ -> assert false

let is_alloc = (==) alloc

let is_memory_var v = 
  if v == alloc then false
  else
    match v.var_why_type with 
      | Memory _ -> true 
      | _ -> false


let declare_heap_var info name =
try  let info' = Hashtbl.find heap_vars name in
    if not (same_why_type_no_zone info.var_why_type info'.var_why_type)
    then
      let ty' = Info.output_why_type info'.var_why_type in
      let ty =  Info.output_why_type info.var_why_type in
	Format.eprintf "declare_heap_var : %s ; oldtype = %a ; newtype = %a@." 
	  name Output.fprintf_logic_type ty' Output.fprintf_logic_type ty ;
      assert false
    else
      info'
with
    Not_found ->
      begin
	Hashtbl.add heap_vars name info;
	info
      end


let empty = ZoneSet.empty
let union = ZoneSet.union


(* static variables *)


let add_var v (ty : Info.why_type) s =
  let info = declare_heap_var v v.var_unique_name in
  HeapVarSet.add info s

  
let add_alloc s = HeapVarSet.add alloc s

let add_heap_var n z ty =
  let ty' = Memory(ty,z) in
  let info = default_var_info n in
  let n' = info.var_name ^ "_" ^ (found_repr z) in
  set_var_type_why (Var_info info) ty';
  let _ = declare_heap_var info n' in ()
(*  let info = declare_heap_var info n' in
  HeapVarSet.add info s*)
  

let add_field_var v ty s =
  match ty with
    | Pointer z ->
	let z = repr z in
	let ty' = 
	  try
	    let table = 
	      try
		Hashtbl.find type_why_table z 
	      with Not_found -> 
		Format.eprintf "no why type table for zone %s@\n" z.name;
		assert false
	    in
	    Hashtbl.find  table v 
	  with Not_found -> 
	    Format.eprintf "no  why type for field %s@\n" v.var_name;
	    assert false
	in
	let n = v.var_unique_name in
	if not z.zone_is_var then  add_heap_var n z ty';
	ZoneSet.add (z,n,ty') s
    | Unit -> assert false
    | Info.Int -> assert false
    | _ -> assert false
	

type effect =
    {
      reads : ZoneSet.t;
      assigns : ZoneSet.t;
      reads_var : HeapVarSet.t;
      assigns_var : HeapVarSet.t;
      (* useful for generating separation invariants *)
      reads_under_pointer : HeapVarSet.t;
      assigns_under_pointer : HeapVarSet.t;
    }

let ef_empty = { reads = empty; assigns = empty ; 
		 reads_var = HeapVarSet.empty ; 
		 assigns_var = HeapVarSet.empty;
		 reads_under_pointer = HeapVarSet.empty;
		 assigns_under_pointer = HeapVarSet.empty; }

(*
let merge_var_map m1 m2 =
  HeapVarMap.fold
    (fun v labs acc ->
       try
	 let l = HeapVarMap.find v m2 in
	 HeapVarMap.add v (LabelSet.union labs l) acc
       with Not_found ->
	   HeapVarMap.add v labs acc)
    m1 m2
*)

let ef_union e1 e2 = 
  { reads = union e1.reads e2.reads;
    assigns = union e1.assigns e2.assigns ;
    reads_var = HeapVarSet.union e1.reads_var e2.reads_var;
    assigns_var = HeapVarSet.union e1.assigns_var e2.assigns_var;
    reads_under_pointer = 
      HeapVarSet.union e1.reads_under_pointer e2.reads_under_pointer;
    assigns_under_pointer = 
      HeapVarSet.union e1.assigns_under_pointer e2.assigns_under_pointer; }

let reads_add_var v ty e =   { e with reads_var = add_var v ty e.reads_var }
let reads_add_field_var v ty e = { e with reads = add_field_var v ty e.reads }
(*let reads_add_pointer_var ty e = { e with reads = add_pointer_var ty e.reads }*)

let reads_add_alloc e = 
  (* [alloc] not used when the alloc table is dropped *)
  assert (not no_alloc_table);
  { e with reads_var = add_alloc e.reads_var }

let assigns_add_var v ty e = { e with reads_var = add_var v ty e.reads_var;
				 assigns_var = add_var v ty e.assigns_var }

let reads_add_under_pointer v ty e = 
  { e with reads_under_pointer = HeapVarSet.add v e.reads_under_pointer }

let assigns_add_under_pointer v ty e = 
  { e with assigns_under_pointer = HeapVarSet.add v e.assigns_under_pointer }

let assigns_add_field_var v ty e = 
  { e with reads = add_field_var v ty e.reads;
      assigns = add_field_var v ty e.assigns }

let assigns_add_alloc e = 
  (* [alloc] should not be used when the alloc table is dropped *)
  assert (not no_alloc_table);
  { e with reads_var = add_alloc e.reads_var;
      assigns_var = add_alloc e.assigns_var }

let assigns_alloc e = HeapVarSet.mem alloc e.assigns_var

let rec term t = match t.nterm_node with 
  | NTvar v -> 
      if v.var_is_static
      then reads_add_var v v.var_why_type ef_empty
      else ef_empty
  | NTarrow (t1,z,f) -> 
      let z = repr z in
      assert (same_why_type (Cnorm.type_why_for_term t1) (Pointer z));
      let ef = reads_add_field_var f (Pointer z) (term t1) in
      (* [alloc] not used when the alloc table is dropped *)
      (* CLAUDE: in terms e->f, DOES NOT reads alloc *)
      (* if no_alloc_table then *) ef (* else reads_add_alloc ef *)
  | NTunop (Ustar,_) -> assert false
  | NTunop (Uamp, t) -> term t
  | NTunop (Uplus, t) -> term t
  | NTunop (Uminus, t) -> term t
  | NTunop (Unot, t) -> term t
  | NTunop (Utilde, t) -> term t
  | NTunop (( Ufloat_of_int | Uint_of_float | Ufloat_conversion 
	    | Uint_conversion | Uabs_real | Usqrt_real 
	    | Uround_error | Utotal_error | Uexact | Umodel), t) -> term t
  | NTbase_addr t -> term t
  | NToffset t -> reads_add_alloc (term t)
  | NTblock_length t -> 
      (* [block_length] should not be used with the arithmetic memory model *)
      assert (not arith_memory_model);
      reads_add_alloc (term t)
  | NTarrlen t -> 
      let ef = term t in
      (* [alloc] not used when the alloc table is dropped *)
      if no_alloc_table then ef else reads_add_alloc ef
  | NTstrlen (t1,zone,var) ->
      (* effect of [strlen(p)] is to read the memory pointed-to by [p] *)
      let zone = repr zone in
      assert (same_why_type (Cnorm.type_why_for_term t1) (Pointer zone));
      reads_add_var var (Pointer zone) (term t1)
  | NTat (t, _) -> 
      term t
  | NTold t -> 
      term t
  | NTif (t1, t2, t3) -> 
      ef_union (term t1) (ef_union (term t2) (term t3))
  | NTbinop (t1, _, t2) 
  | NTmin (t1,t2)
  | NTmax (t1,t2) -> 
      ef_union (term t1) (term t2) 
  | NTapp {napp_pred = id; napp_args = tl; napp_zones_assoc = assoc} -> 
	let reads = ZoneSet.fold 
	  (fun (z,s,ty) acc ->
	     let z = repr z in
	     let z = try assoc_zone z assoc with Not_found -> z in
	     let z = repr z in
	     let ty = Cseparation.assoctype ty assoc in
	     if not z.zone_is_var then add_heap_var s z ty else ();
	     ZoneSet.add (z,s,ty) acc)
	  id.logic_heap_zone empty in
	List.fold_left 
	  (fun acc t -> ef_union acc (term t))
	  {ef_empty with reads = reads; reads_var = id.logic_heap_args; }
	  tl  
  | NTconstant _ -> ef_empty
  | NTminint _ | NTmaxint _ -> ef_empty
  | NTcast (_, t) -> term t
  | NTrange (t1, t2, t3, z, f) ->
      let z = repr z in
      assert (same_why_type (Cnorm.type_why_for_term t1) (Pointer z));
      let ef = 
	(reads_add_field_var f (Pointer z)
	   (ef_union (term t1) (ef_union (term_option t2) (term_option t3))))
      in
      (* [alloc] not used when the alloc table is dropped *)
      (* CLAUDE: e->f ne lit jamais alloc *)
      (* if no_alloc_table then *) ef (* else reads_add_alloc ef *)

and term_option = function None -> ef_empty | Some t -> term t


(* used to interpret the reads clause *)
let locations ll =
  List.fold_left
    (fun acc l -> ef_union acc (term l)) ef_empty ll

(* used to interpret the assigns clause *)
let rec assign_location t = match t.nterm_node with 
  | NTvar v ->
      if v.var_is_static
      then
	{ ef_empty with assigns_var = add_var v (Cnorm.type_why_for_term t) HeapVarSet.empty } 
      else ef_empty
  | NTarrow (t1,z,f) -> 
      let ef = 
	(assigns_add_field_var f (Cnorm.type_why_for_term t1) (term t1))
      in
      (* [alloc] not used when the alloc table is dropped *)
      (* CLAUDE: in terms e->f, DOES NOT reads alloc *)
      (* if no_alloc_table then *) ef (* else reads_add_alloc ef *)
  | NTunop (Ustar,_) -> assert false
  | NTunop (Uamp, _) -> assert false
  | NTunop (Uminus, _)  | NTunop (Uplus, _)    | NTunop (Unot, _)  
  | NTunop (Utilde, _)  
  | NTunop ((Ufloat_of_int | Uint_of_float | Ufloat_conversion 
	    | Uint_conversion | Uabs_real | Usqrt_real | Uround_error 
	    | Utotal_error |Uexact | Umodel), _)  
  | NTbase_addr _  
  | NToffset _  
  | NTblock_length _  
  | NTarrlen _  
  | NTstrlen _
  | NTmin _
  | NTmax _
  | NTminint _
  | NTmaxint _
  | NTat (_, _)  
  | NTold _  
  | NTif (_, _, _)  
  | NTbinop (_, _, _)  
  | NTapp _  
  | NTconstant _  
  | NTcast (_, _) -> 
      error t.nterm_loc "invalid location"
  | NTrange (t1, t2, t3, z, f) ->
      let ef = 
	(assigns_add_field_var f (Cnorm.type_why_for_term t1)
	   (ef_union (term t1) (ef_union (term_option t2) (term_option t3))))
      in
      (* [alloc] not used when the alloc table is dropped *)
      (* CLAUDE: in terms e->f, DOES NOT reads alloc *)
      (* if no_alloc_table then *) ef (* else reads_add_alloc ef *)

(***let assign_location loc =
  match loc with
    | Lterm t ->
	 begin 
	   match t.nterm_node with
	     | NTarrow (t1,f) -> 
		 { reads = add_alloc (term t1);
		   assigns = add_field_var f t.nterm_type empty }
	     | NTstar t1 ->
		 { reads = add_alloc (term t1);
		   assigns = add_pointer_var t1.nterm_type empty }
	     | NTunop (Ustar,_) -> assert false
	     | NTvar v ->
		 { reads = empty;
		   assigns = 
		     if v.var_is_static
		     then add_var v t.nterm_type empty
		     else empty }
	     | _ -> assert false
	 end
    | Lstar t ->
	{ reads = add_alloc (term t);
	  assigns = add_pointer_var t.nterm_type empty }
    | Lrange(t1,t2,t3) -> 
	{ reads = add_alloc (union (term t1) (union (term t2) (term t3)));
	  assigns = add_pointer_var t1.nterm_type empty }
***)	  

let rec predicate p =  
  match p.npred_node with
    | NPtrue -> ef_empty
    | NPfalse -> ef_empty
    | NPapp {napp_pred = id; napp_args = tl; napp_zones_assoc = assoc} -> 
	let reads = ZoneSet.fold 
	  (fun (z,s,ty) acc ->
	     let z = repr z in
	     let z = try assoc_zone z assoc with Not_found -> z in
	     let z = repr z in
	     let ty = Cseparation.assoctype ty assoc in
	     if not z.zone_is_var then add_heap_var s z ty else ();
	     ZoneSet.add (z,s,ty) acc)
	  id.logic_heap_zone empty in
	List.fold_left 
	  (fun acc t -> ef_union acc (term t)) 
	  {ef_empty with reads = reads; reads_var = id.logic_heap_args; }
	  tl 
    | NPrel (t1, _, t2) -> ef_union (term t1) (term t2)
    | NPand (p1, p2)
    | NPor (p1, p2) 
    | NPiff (p1, p2) 
    | NPimplies (p1, p2) -> ef_union (predicate p1) (predicate p2)
    | NPnot p -> predicate p
    | NPif (t, p1, p2) -> ef_union (term t) 
	(ef_union (predicate p1) (predicate p2))
    | NPforall (_, p) -> predicate p	
    | NPexists (_, p) -> predicate p
    | NPfresh t -> 
	(* [fresh] should not be used when the alloc table is dropped *)
	assert (not no_alloc_table);
	assigns_add_alloc (term t)
    | NPvalid t -> 
	(* [alloc] not used when the alloc table is dropped *)
	if no_alloc_table then term t else reads_add_alloc (term t)
    | NPvalid_index (t1,t2) ->
	let ef = ef_union (term t1) (term t2) in
	(* [alloc] not used when the alloc table is dropped *)
	if no_alloc_table then ef else reads_add_alloc ef
    | NPvalid_range (t1,t2, t3) -> 
	let ef = ef_union (term t1) (ef_union (term t2) (term t3)) in
	(* [alloc] not used when the alloc table is dropped *)
	if no_alloc_table then ef else reads_add_alloc ef
    | NPold p -> predicate p
    | NPat (p,_) -> predicate p
    | NPnamed (_, p) -> predicate p
    | NPseparated (t1,t2) | NPfull_separated (t1,t2) -> 
	ef_union (term t1) (term t2)
    | NPbound_separated (t1,t2,t3,t4) -> 
	let ef1 = ef_union (term t1) (term t2) in 
	let ef2 = ef_union (term t3) (term t4) in
	ef_union ef1 ef2

(* table for weak invariants *)
let weak_invariants = Hashtbl.create 97


let add_weak_invariant id p =
  Hashtbl.add weak_invariants id (p, predicate p)

(* table for strong invariants *)
let strong_invariants = Hashtbl.create 97

let add_strong_invariant id p vars =
  if p.npred_node <> NPtrue then
  let ef = predicate p in
  Hashtbl.add strong_invariants id (p,ef,vars)

let strong_invariants_2 = Hashtbl.create 97

let mem_strong_invariant_2 id =
  Hashtbl.mem strong_invariants_2 id

let add_strong_invariant_2 id p args =
  if not (mem_strong_invariant_2 id) 
  then
    if p.npred_node <> NPtrue then
      let ef = predicate p in
      Hashtbl.add strong_invariants_2 id (p,ef,args)      

let invariants_for_struct = Hashtbl.create 97

let add_invariants_for_struct id p vars =
  if p.npred_node <> NPtrue then
  let ef = predicate p in
  Hashtbl.add invariants_for_struct id (p,ef,vars)


let intersect_only_alloc e1 e2 =
  HeapVarSet.is_empty 
    (HeapVarSet.remove alloc 
       (HeapVarSet.inter e1.reads_var e2.reads_var))
(* TODO : useless, because always empty because zones are distincts *)
  &&
    ZoneSet.is_empty (ZoneSet.inter e1.reads e2.reads)



let weak_invariants_for hvs =
  Hashtbl.fold
    (fun name (_,e) acc ->
        if intersect_only_alloc e hvs then acc
       else ef_union e acc) 
    weak_invariants ef_empty

let strong_invariants_for hvs =
  Hashtbl.fold
    (fun s (_,_,e) acc -> 
       if HeapVarSet.subset e.reads_var hvs.reads_var &&
	 ZoneSet.subset e.reads hvs.reads 
       then ef_union e acc
       else acc) 
    strong_invariants ef_empty


let logic_type ls =
  match ls with
    | Clogic.NPredicate_reads(args,locs) -> locations locs
    | Clogic.NPredicate_def(args,pred) -> predicate pred
    | Clogic.NFunction(args,ret,locs) -> locations locs
    | Clogic.NFunction_def(args,ret,t) -> term t


let option f = function None -> empty | Some x -> f x
let ef_option f = function None -> ef_empty | Some x -> f x

let variant (t,_) = term t

let loop_annot a = 
  ef_union (ef_option predicate a.invariant) 
    (ef_option variant a.variant) 
  (* TODO : loop_assigns ? *) 


let spec sp = 
  ef_union
    (ef_union 
	(ef_union (ef_option predicate sp.requires) 
	   (ef_option predicate sp.ensures))
	(ef_option variant sp.decreases))
    (ef_option 
       (fun (_,l) -> 
	 List.fold_left
	   (fun acc l -> ef_union acc (assign_location l)) ef_empty l)
       sp.Clogic.assigns)

open Cast

let rec expr ?(with_local=false) e = match e.nexpr_node with
  | NEnop
  | NEconstant _
  | NEstring_literal _ -> ef_empty
  | NEvar (Var_info v) ->
      if with_local || v.var_is_static 
      then reads_add_var v (type_why e) ef_empty
      else ef_empty
  | NEvar (Fun_info v) ->
      ef_empty
  | NEarrow (e1,z, f) ->
      let z = repr z in
      assert (same_why_type (type_why e1)  (Pointer z));
      let ef = 
	reads_add_field_var f (Pointer z) (expr ~with_local e1) in
      let ef = ef_union ef (reads_under_pointer e1) in
      (* [alloc] not used when the alloc table is dropped *)
      if no_alloc_table then ef else reads_add_alloc ef 
  | NEbinary (e1, _, e2) | NEseq (e1, e2) ->
      ef_union (expr ~with_local e1) 
	(expr ~with_local e2)
  | NEassign (lv, e) | NEassign_op (lv, _, e) ->
      ef_union (assign_expr ~with_local lv)
	(expr ~with_local e)
  | NEunary (Ustar , _ ) -> assert false
  | NEunary (Uamp, e) -> assert false (* address_expr e *)
  | NEunary 
      (( Uplus | Uminus | Unot | Utilde 
       | Ufloat_of_int | Uint_of_float 
       | Ufloat_conversion | Uint_conversion), e) ->
      expr ~with_local e
  | NEincr (_, e) ->
      assign_expr ~with_local e
  | NEcall {ncall_fun = e; ncall_args = el; ncall_zones_assoc = assoc} ->
      let ef = match e.nexpr_node with
	| NEvar (Fun_info f) ->    	   
	    let reads = ZoneSet.fold 
	      (fun (z,s,ty) acc ->
		 let z = repr z in
		 let z = try assoc_zone z assoc with Not_found -> z in
		 let z = repr z in
		 let ty = Cseparation.assoctype ty assoc in
		 if not z.zone_is_var then add_heap_var s z ty else ();
		 ZoneSet.add (z,s,ty) acc)
	      f.function_reads empty in
	    let writes = ZoneSet.fold 
	      (fun (z,s,ty) acc ->	 
		 let z = repr z in
		 let z = try assoc_zone z assoc with Not_found -> z in
		 let z = repr z in
		 let ty = Cseparation.assoctype ty assoc in
		 if not z.zone_is_var then add_heap_var s z ty else ();
		 ZoneSet.add (z,s,ty) acc)
	(*	 let z = repr z in
		 ZoneSet.add 
		   ((try assoc_zone z assoc with Not_found -> z),s,ty) acc)*)
	      f.function_writes empty in
	    { reads = reads; assigns = writes; 
	      reads_var = f.function_reads_var; 
	      assigns_var = f.function_writes_var;
	      (* TODO: consider pointer arguments written by function *)
	      reads_under_pointer = HeapVarSet.empty;
	      assigns_under_pointer = HeapVarSet.empty; } 
	| _ -> expr ~with_local e
      in

      List.fold_left 
	(fun ef arg -> ef_union (expr ~with_local arg) ef) ef el
  | NEcond (e1, e2, e3) ->
      ef_union (ef_union (expr ~with_local e1) 
		  (expr ~with_local e2))
	(expr ~with_local e3)
  | NEcast (_, e) ->
      expr ~with_local e
  | NEmalloc (_, e) ->
      if no_alloc_table then
	expr ~with_local e
      else
	assigns_add_alloc (expr ~with_local e)

(* effects for [e = ...] *)
and assign_expr ?(with_local=false) e = match e.nexpr_node with
  | NEvar (Var_info v) -> 
      if with_local || v.var_is_static
      then assigns_add_var v v.var_why_type ef_empty
      else ef_empty
  | NEvar (Fun_info _) ->
      ef_empty
  | NEunary (Ustar,_) -> assert false
  | NEarrow (e1,z, f) ->
      let ef = assigns_add_field_var f (type_why e1)
	  (expr ~with_local e1) in
      let ef = ef_union ef (assign_under_pointer e1) in
      (* [alloc] not used when the alloc table is dropped *)
      if no_alloc_table then ef else reads_add_alloc ef 
  | NEcast (_, e1) ->
      assign_expr ~with_local e1
  | _ -> 
      assert false (* not a left value *)

and reads_under_pointer e = match e.nexpr_node with
  | NEvar (Var_info v) ->
      reads_add_under_pointer v v.var_why_type ef_empty
  | NEbinary (e1,Badd_pointer_int,_) ->
      reads_under_pointer e1
  | NEcast (_, e1) ->
      reads_under_pointer e1
  | _ -> ef_empty

and assign_under_pointer e = match e.nexpr_node with
  | NEvar (Var_info v) ->
      assigns_add_under_pointer v v.var_why_type ef_empty
  | NEbinary (e1,Badd_pointer_int,_) ->
      assign_under_pointer e1
  | NEcast (_, e1) ->
      assign_under_pointer e1
  | _ -> ef_empty (* TODO: encode in some way the fact some pointer was
		     assigned *)

(* effects for [&e] *)
(*
and address_expr e = match e.nexpr_node with
  | NEvar v -> 
      begin match e.nexpr_type.Ctypes.ctype_node with
	| Tstruct _ | Tunion _ -> assert false (* ef_empty *)
	| _ -> ef_empty (* unsupported "& operator" *)
      end
  | NEarrow (e1,z, f) ->
      begin match e1.nexpr_type.Ctypes.ctype_node with
	| Tenum _ | Tint _ | Tfloat _ -> expr e1
	| _ -> reads_add_field_var f (type_why e1) (expr e1)
      end
 (* | NEcast (_, e1) ->
      address_expr e1*)
  | _ -> 
      assert false (* not a left value *)
*)

let rec statement ?(with_local=false) s = match s.nst_node with
  | NSnop
  | NSbreak
  | NScontinue
  | NSlogic_label _
  | NSreturn None 
  | NSgoto _ ->
      ef_empty
  | NSexpr e -> 
      expr ~with_local e
  | NSif (e, s1, s2) -> 
      ef_union (expr ~with_local e)
	(ef_union (statement ~with_local s1)
	   (statement ~with_local s2))
  | NSwhile (annot, e, s)
  | NSdowhile (annot, s, e) ->
      ef_union (loop_annot annot) 
	(ef_union (statement ~with_local s)
	   (expr ~with_local e))
  | NSfor (annot, e1, e2, e3, s) ->
      ef_union (loop_annot annot) 
	(ef_union (ef_union (expr ~with_local e1)
		     (expr ~with_local e2))
	   (ef_union (expr ~with_local e3)
	      (statement ~with_local s)))
  | NSblock bl ->
      block ~with_local bl
  | NSreturn (Some e) ->
      expr ~with_local e
  | NSlabel (_, s) ->
      statement ~with_local s
  | NSswitch (e, used_cases, case_list) -> 
      List.fold_left
	(fun ef (cases,bl) ->
	   List.fold_left 
	     (fun ef i -> ef_union ef (statement ~with_local i))
	     ef bl)
	(expr ~with_local e)
	case_list
  | NSassert p | NSassume p ->
      predicate p
  | NSspec (sp, s) ->
      ef_union (spec sp) (statement ~with_local s)
  | NSdecl (_, _, i,rem) -> 
      ef_union (initializer_option ~with_local i)
	(statement ~with_local rem)

and block ?(with_local=false) sl =
(*  let local_decl d = match d.node with
    | Ndecl (_, _, i) -> initializer_option i
    | Ntypedecl _ -> ef_empty
    | _ -> ef_empty (* unsupported local declaration *)
  in*)
  List.fold_left
    (fun ef s -> ef_union (statement ~with_local s) ef)
(*    (List.fold_left (fun ef d -> ef_union (local_decl d) ef) ef_empty dl)*)
    ef_empty
    sl

and initializer_ ?(with_local=false) = function
  | Iexpr e -> 
      expr ~with_local e
  | Ilist il -> 
      List.fold_left (fun ef i -> ef_union
	  (initializer_ ~with_local i) ef) ef_empty il

and initializer_option ?(with_local=false) = function
  | None -> ef_empty
  | Some i -> initializer_ ~with_local i

(* first pass: declare invariants and computes effects for logics *)


let rec ctype ty =
  ctype_node ty.Ctypes.ctype_node

and ctype_node = function
  | Tvoid -> sprintf "void"
  | Tint _ -> sprintf "int"
  | Tfloat _ -> sprintf "float"
  | Ctypes.Tvar s -> sprintf "%s" s
  | Tarray (_,ty, _) -> sprintf "%s" (ctype ty)
  | Tpointer (_,ty) -> sprintf "%s_Pointer" (ctype ty)
  | Tstruct s -> sprintf "%s" s;
  | Tunion s -> sprintf "%s" s
  | Tenum s -> sprintf "%s" s
  | Tfun _ -> assert false
		   
let global_var = ref [] 

let invariant_for_global loc v =
  assert (not (List.mem v !global_var));
  let form =
    List.fold_left 
      (fun p x ->
	 ("separation_" ^ (ctype v.var_type) ^ "_" ^ (ctype x.var_type),
	   "separation_"^v.var_name^"_"^x.var_name,
	  Cseparation.separation loc v x,
	  HeapVarSet.add v (HeapVarSet.singleton x)) :: p) 
      [] !global_var 
  in 
  global_var := v::!global_var;
  form
    
let not_a_constant_value loc = error loc "is not a constant value"

let unop = function
  | Ustar -> Clogic.Ustar
  | Uamp -> Clogic.Uamp
  | Utilde -> Clogic.Utilde
  | Ufloat_of_int -> Clogic.Ufloat_of_int
  | Uint_of_float -> Clogic.Uint_of_float
  | Ufloat_conversion -> Clogic.Ufloat_conversion
  | Uminus -> Clogic.Uminus
  | Uplus | Unot | Uint_conversion -> assert false

let rec term_of_expr e =
  let make n = 
    { nterm_node = n; nterm_type = e.nexpr_type; nterm_loc = e.nexpr_loc}
  in
  match e.nexpr_node with 
  | NEconstant e -> make (NTconstant e)
  | NEvar (Var_info info) -> make (NTvar info)
  | NEarrow (nlvalue,z,var_info) -> 
      make (NTarrow (term_of_expr nlvalue,z, var_info))
  | NEunary (Uplus, nexpr) -> 
      term_of_expr nexpr
  | NEunary (Unot, nexpr) -> 
      make (NTif (term_of_expr nexpr, 
		  make (NTconstant (IntConstant "0")), 
		  make (NTconstant (IntConstant "1"))))
  | NEunary (Uint_conversion, e) ->
      term_of_expr e
  | NEunary (op, nexpr) ->  
      make (NTunop (unop op, term_of_expr nexpr))  
  | NEbinary (e1, op, e2) ->
      begin
	match e1.nexpr_node, e2.nexpr_node, op with
	  | _, _, Badd | _, _, Badd_int _ | _, _, Badd_float _
	  | _, _, Badd_pointer_int ->
	      make (NTbinop(term_of_expr e1, Clogic.Badd, term_of_expr e2))
	  | _, _, Bsub | _, _, Bsub_int _ | _, _, Bsub_float _
	  | _, _, Bsub_pointer -> 
	      make (NTbinop(term_of_expr e1, Clogic.Bsub, term_of_expr e2))
	  | _, _, Bmul | _, _, Bmul_int _ | _, _, Bmul_float _ -> 
	      make (NTbinop(term_of_expr e1, Clogic.Bmul, term_of_expr e2))
	  | _, _, Bdiv | _, _, Bdiv_int _ | _, _, Bdiv_float _ -> 
	      make (NTbinop(term_of_expr e1, Clogic.Bdiv, term_of_expr e2))
	  | _, _, Bmod | _, _, Bmod_int _ ->  
	      make (NTbinop(term_of_expr e1, Clogic.Bmod, term_of_expr e2))
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), Beq_int 
	  | NEconstant (RealConstant e1), NEconstant (RealConstant e2), 
	    Beq_float _
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), 
	    Beq_pointer  ->
	      if e1 = e2 then make (NTconstant (IntConstant "0"))
	      else make (NTconstant (IntConstant "1"))
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), Bneq_int 
	  | NEconstant (RealConstant e1), NEconstant (RealConstant e2), 
	    Bneq_float _
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), 
	    Bneq_pointer  ->
	      if e1 = e2 then make (NTconstant (IntConstant "1"))
	      else make (NTconstant (IntConstant "0"))
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), Blt_int 
	  | NEconstant (RealConstant e1), NEconstant (RealConstant e2), 
	    Blt_float _
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), 
	    Blt_pointer  ->
	      if e1 < e2 then make (NTconstant (IntConstant "0"))
	      else make (NTconstant (IntConstant "1"))
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), Bgt_int 
	  | NEconstant (RealConstant e1), NEconstant (RealConstant e2), 
	    Bgt_float _
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), 
	    Bgt_pointer  ->
	      if e1 > e2 then make (NTconstant (IntConstant "0"))
	      else make (NTconstant (IntConstant "1"))
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), Ble_int 
	  | NEconstant (RealConstant e1), NEconstant (RealConstant e2), 
	    Ble_float _
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), 
	    Ble_pointer  ->
	      if e1 <= e2 then make (NTconstant (IntConstant "0"))
	      else make (NTconstant (IntConstant "1"))
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), Bge_int 
	  | NEconstant (RealConstant e1), NEconstant (RealConstant e2), 
	    Bge_float _
	  | NEconstant (IntConstant e1), NEconstant (IntConstant e2), 
	    Bge_pointer  ->
	      if e1 >= e2 then make (NTconstant (IntConstant "0"))
	      else make (NTconstant (IntConstant "1"))
	  | _, _, Beq | _, _, Beq_int | _, _, Beq_float _ | _, _, Beq_pointer 
	  | _, _, Blt | _, _, Blt_int | _, _, Blt_float _ | _, _, Blt_pointer
	  | _, _, Bgt | _, _, Bgt_int | _, _, Bgt_float _ | _, _, Bgt_pointer
	  | _, _, Ble | _, _, Ble_int | _, _, Ble_float _ | _, _, Ble_pointer
	  | _, _, Bge | _, _, Bge_int | _, _, Bge_float _ | _, _, Bge_pointer
	  | _, _, Bneq | _, _, Bneq_int | _, _, Bneq_float _
	  | _, _, Bneq_pointer
	  | _, _, Bbw_and
	  | _, _, Bbw_xor
	  | _, _, Bbw_or
	  | _, _, Band
	  | _, _, Bor
	  | _, _, Bshift_left
	  | _, _, Bshift_right -> error e.nexpr_loc "not a constant value"
      end
  | NEcond (e1, e2, e3) ->
      make (NTif (term_of_expr e1, term_of_expr e2, term_of_expr e3))
  | NEcast (ty, e) ->
      make (NTcast (ty, term_of_expr e))
  | NEvar (Fun_info _)
  | NEcall _ 
  | NEincr (_, _)
  | NEassign_op (_, _, _)
  | NEassign (_, _) 
  | NEseq (_, _)
  | NEstring_literal _
  | NEnop -> 
      not_a_constant_value e.nexpr_loc
  | NEmalloc _ ->
      error e.nexpr_loc "not a side-effects free expression"

let noattr loc ty e =
  { nterm_node = e;
    nterm_type = ty;
    nterm_loc  = loc;
  }

let rec pop_initializer loc t i =
  let mk node ty = { nterm_node = node; nterm_type = ty; nterm_loc = loc } in
  match i with 
    | [] -> { nterm_node = 
	       (match t.Ctypes.ctype_node with
		  | Tint _ | Tenum _ -> 
		      NTconstant(IntConstant "0")
		  | Tfloat _ -> 
		      NTunop (Clogic.Ufloat_conversion,
			      mk (NTconstant(RealConstant "0.0")) c_real)
		  | Tpointer _ -> 
		      NTcast (t, mk (NTconstant (IntConstant "0")) c_int)
		  | _ -> 
		      assert false);
	      nterm_type = t;
	      nterm_loc  = loc;
	    },[]
    | (Iexpr e)::l -> term_of_expr e,l
    | (Ilist [])::l -> pop_initializer loc t l
    | (Ilist l)::l' -> 
	let e,r = pop_initializer loc t l in e,r@l'

let rec invariant_for_constant loc t lvalue initializers =
  match t.Ctypes.ctype_node with
    | Tint _ | Tfloat _ | Tpointer _ | Tenum _ -> 
	let x,l = pop_initializer loc t initializers in
	nprel (lvalue, Eq, x), l
    | Tstruct n ->
	begin match tag_type_definition n with
	  | TTStructUnion (Tstruct _, fl) ->
	      List.fold_left 
		(fun (acc,init) f -> 
		   let tyf = f.var_type in
		   let tyf =  { tyf with
		     Ctypes.ctype_const = tyf.Ctypes.ctype_const 
					  || t.Ctypes.ctype_const;
		   }  in 
		   let block, init' =
		     invariant_for_constant loc tyf 
		       (Cseparation.in_struct lvalue f) init
		   in 
		   if tyf.Ctypes.ctype_const then
		     (npand (acc,block),init')
		   else
		     (acc,init'))
		(nptrue,initializers)  fl
	  | _ ->
	      assert false
	end
    | Tunion n ->
	begin match tag_type_definition n with
	  | TTStructUnion (Tstruct (_), f::_) ->
	      let zone = Cnorm.find_zone_for_term lvalue in
	      let () = type_why_new_zone zone f in
	      let block, init' =
		 invariant_for_constant loc f.var_type 
		  (noattr loc f.var_type 
		     (NTarrow(lvalue,zone, f)))
		  initializers
	      in (block,init')
	  | _ ->
	      assert false
	end
    | Tarray (_,ty,Some t) ->
	let rec init_cells i (block,init) =
	  if i >= t then (block,init)
	  else
	    let ts = (noattr loc c_int 
			(NTconstant (IntConstant (Int64.to_string i)))) in
	    let shift = 
	      noattr loc 
		{ty with Ctypes.ctype_node = (Tpointer (Not_valid,ty)) }
		(NTbinop (lvalue,Clogic.Badd, ts))
	    in
	    let e =
	      match ty.Ctypes.ctype_node with
		| Tstruct _ | Tunion _ -> shift
		| _ ->
		    let info = make_field ty in
		    let info = declare_arrow_var info in
		    let zone = find_zone_for_term lvalue in
		    let () = type_why_new_zone zone info in
		    noattr loc ty 
		      (NTarrow
			 (shift, zone,info))
	    in
	    let (b,init') = 
	      invariant_for_constant loc ty e init 
	    in
	    init_cells (Int64.add i Int64.one) (npand (block,b),init')
	in
	init_cells Int64.zero (nptrue,initializers)
    | Tarray (_,ty,None) -> assert false
    | Tfun (_, _) -> assert false
    | Tvar _ -> assert false
    | Tvoid -> nptrue,initializers 

let rec has_constant_values ty = match ty.Ctypes.ctype_node with
  | Tvoid | Tint _ | Tfloat _ | Tenum _ | Tpointer _ ->
      ty.Ctypes.ctype_const
  | Tstruct n -> 
      ty.Ctypes.ctype_const ||
      (match tag_type_definition n with
	 | TTStructUnion (Tstruct _, fl) -> 
	     List.exists (fun f -> has_constant_values f.var_type) fl
	 | _ -> assert false)
  | Tarray (_,ty', _) -> has_constant_values ty'
  | Tunion _ | Tfun _ | Tvar _ -> false

let diff loc x y = 
  nprel ( x, Neq,  y)
	  
let rec validity x ty size =
  match ty.Ctypes.ctype_node with
    | Tarray (_,ty', Some size') ->
	let i = default_var_info "counter" in
	set_var_type (Var_info i) c_int false;
	let vari = { nterm_node = NTvar i; 
		     nterm_loc = x.nterm_loc;
		     nterm_type = c_int;
		     } in
	let j = default_var_info "counter2" in
	let varj = { nterm_node = NTvar j; 
		     nterm_loc = x.nterm_loc;
		     nterm_type = c_int;
		     } in	  
	let term_sup = { nterm_node = 
   	                   NTconstant (IntConstant 
					 (Int64.to_string (Int64.pred size))); 
			 nterm_loc = x.nterm_loc;
			 nterm_type = c_int;
			 } in
	let ineq = npand 
		     (nprel (Cnorm.nzero, Le, vari),
		      nprel (vari, Lt, 
			       term_sup)) in	
	let jneq = npand 
		     (nprel (Cnorm.nzero, Le, varj),
		      nprel (varj, Lt, 
			     term_sup)) in
	let (pre1,pre2) = validity 
			(noattr x.nterm_loc ty 
			   (NTbinop (x,Clogic.Badd,vari)))
					     ty' size' in
	(npand (
	  npvalid_range (x, Cnorm.nzero,term_sup),
	  make_forall 
	    [c_int,i]
	    (make_implies ineq pre1)),
	 make_forall 
	   [(c_int,j); (c_int,i)]
	   (make_implies
	      (npand (npand (ineq,jneq),
		      diff x.nterm_loc vari varj))
	      (npand (diff x.nterm_loc 
			(noattr x.nterm_loc ty 
			   (NTbinop (x,Clogic.Badd,vari)))
			(noattr x.nterm_loc ty 
			   (NTbinop (x,Clogic.Badd,varj))),
		      pre2))))
    | _ ->  
	let term_sup = { nterm_node = 
	                   NTconstant 
	                     (IntConstant (Int64.to_string (Int64.pred size)));
			 nterm_loc = x.nterm_loc;
			 nterm_type = c_int;
			 } in
      npvalid_range (x, Cnorm.nzero,term_sup), nptrue

let decl d =
  match d.Cast.node with
    | Nlogic(id,ltype) -> 
	let l = logic_type ltype in
	lprintf 
	  "effects of logic declaration of %s: @[%a %a@]@." id.logic_name
	  print_effects l.reads_var print_effects2 l.reads;
	id.logic_heap_args <- l.reads_var;
	id.logic_heap_zone <- l.reads
    | Ninvariant(id,p) -> 
	add_weak_invariant id p
    | Ninvariant_strong(id,p) -> 
	let pre = (predicate p) in 
	lprintf 
	  "effects of strong invariant %s: @[reads_var : %a  reads :%a@]@." id
	  print_effects pre.reads_var print_effects2 pre.reads;
	add_invariants_for_struct id p pre	  
    | Ndecl(ty,v,init) when ty.Ctypes.ctype_storage <> Extern -> 
	begin
	  match ty.Ctypes.ctype_node with
	    | Tvoid -> ()
	    | Tint _| Tfloat _ | Tpointer _ | Tenum _ -> ()
	    | Tvar s -> ()
	    | Tfun _ -> ()
	    | Tunion _ -> ()
	    | Tarray (_,_,None) -> 
		if ty.ctype_ghost then () else assert false
	    | Tstruct _ -> assert false
	    | Tarray (_,typ, Some s) ->
		lprintf "adding implicit invariant for type of %s@." 
		 v.var_name;
		let t = { nterm_node = NTvar v; 
			  nterm_loc = d.loc;
			  nterm_type = ty ;
			} in
		let name1 = "predicate_for_" ^ v.var_name in
		let pre1,_ = validity t typ s in
		add_strong_invariant name1 pre1 
		  {ef_empty with reads_var =(HeapVarSet.singleton v)};
		List.iter 
		  (fun (x1,x2,p,y) -> 
		     add_strong_invariant x2 p {ef_empty with reads_var = y};
		    add_strong_invariant_2 x1 p [])
		  (invariant_for_global d.loc v);
	end;
	
	let init = (match init with | None -> [] | Some l -> [l]) in
	if has_constant_values ty then begin
	  lprintf "adding implicit invariant for constant %s@." v.var_name;
	  let id = "constant_" ^ v.var_name in
	  let t = {nterm_node = NTvar v; 
		   nterm_loc = Loc.dummy_position;
		   nterm_type = ty ;
		     } in
	  let (pre,_) = invariant_for_constant d.loc ty t init in
	  add_strong_invariant_2 id pre [] ;
	  add_strong_invariant id pre 
	    {ef_empty with reads_var = (HeapVarSet.singleton v)}
	end;
    | Ndecl(ty,v,init) -> () (* nothing to do for extern var *)	
    | Naxiom(id,p) -> () (* TODO *)
    | Ntypedef(ctype,id) -> () 
    | Ntypedecl(ctype) -> ()
    | Ntype _ -> ()

let file l = List.iter decl l

(* second pass: compute functions effects as a fixpoint *)

let warnings = Queue.create ()

let functions fun_list  = 
  let fixpoint = ref true in
  let declare id ef =
    lprintf "previous effects for function %s: reads %a %a writes %a %a@." id.fun_name 
      print_effects id.function_reads_var print_effects2 id.function_reads print_effects id.function_writes_var print_effects2 id.function_writes;
(*
    lprintf "effects for function %s before invariants: reads %a %a writes %a %a@." 
      id.fun_name print_effects ef.reads_var print_effects2 ef.reads print_effects ef.assigns_var print_effects2 ef.assigns;
*)
    let ef  = 
      ef_union
	(ef_union
	   (weak_invariants_for ef)
	   (strong_invariants_for ef)) 
	(ef_union ef { reads = id.function_reads ;
		       reads_var = id.function_reads_var;
		       assigns = id.function_writes;
		       assigns_var = id.function_writes_var;
		       (* TODO: consider pointer params written by function *)
		       reads_under_pointer = HeapVarSet.empty;
		       assigns_under_pointer = HeapVarSet.empty; })
    in
    lprintf "effects for function %s: reads %a %a writes %a %a@." id.fun_name 
      print_effects ef.reads_var print_effects2 ef.reads print_effects ef.assigns_var print_effects2 ef.assigns;
    if not (HeapVarSet.subset ef.reads_var id.function_reads_var) then begin
      fixpoint := false;
      lprintf "effects for function %s: reads_var changed@." id.fun_name;
      id.function_reads_var <- ef.reads_var
    end;
    if not (ZoneSet.subset ef.reads id.function_reads) then begin
      fixpoint := false;
      lprintf "effects for function %s: reads changed@." id.fun_name;
      id.function_reads <- ef.reads;
    end;
    if not (HeapVarSet.subset ef.assigns_var id.function_writes_var) then begin
      fixpoint := false;
      lprintf "effects for function %s: assigns_var changed@." id.fun_name;
      id.function_writes_var <- ef.assigns_var;
    end;
    if not (ZoneSet.subset ef.assigns id.function_writes) then begin
      fixpoint := false;
      lprintf "effects for function %s: assigns changed@." id.fun_name;
      id.function_writes <- ef.assigns;
    end
  in
  let decl f  = 
    let (sp,_,id,s,loc) = find_c_fun f.fun_name in
    let ef_spec = spec sp in
    let ef = 
(*      if (verify id.fun_name) && s <> None then*)
      begin 
	match s with 
	  | None -> ef_spec
	  | Some s -> 
  	      let ef_body = statement s in
	      begin match sp.Clogic.assigns with
		| None -> 
		    (*no assigns given by user:
		      emit a warning if some side-effects have been detected *)
		    if id <> Cinit.invariants_initially_established_info &&
		      not ((ZoneSet.is_empty ef_body.assigns) && 
			     (HeapVarSet.is_empty ef_body.assigns_var)) then
			Queue.add 
			  (loc,
			   "function " ^ id.fun_name ^ 
			     " has side-effects but no 'assigns' clause given")
			  warnings
		| Some _ -> 
		    (* some assigns given by user:
		       emit a warning if side-effects of spec differs from 
		       side-effects of body *) 
		    if not ((ZoneSet.equal ef_spec.assigns ef_body.assigns) &&
			      (HeapVarSet.equal 
				 ef_spec.assigns_var ef_body.assigns_var))
		    then begin 
		      Queue.add 
			(loc,
			 "'assigns' clause for function " ^ id.fun_name ^
			   " do not match side-effects of its body ")
			warnings		    
		    end
	      end;
	      ef_union ef_spec ef_body
      end
(*      else
	ef_spec*)
    in
    declare id ef
  in
  List.iter decl fun_list (*dl*);
  !fixpoint
    
let effect  nfiles fun_list =
  while not (functions [Cinit.invariants_initially_established_info]) 
  do 
    Queue.clear warnings
  done;
  List.iter (fun f ->
	       while not (functions fun_list) 
	       do 
		 Queue.clear warnings
	       done) 
    fun_list 


(*
Local Variables: 
compile-command: "make -j -C .. bin/caduceus.byte"
End: 
*)
