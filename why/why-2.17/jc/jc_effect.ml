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


(* $Id: jc_effect.ml,v 1.154 2008/12/09 09:14:18 marche Exp $ *)

open Jc_stdlib
open Jc_env
open Jc_envset
open Jc_region
open Jc_ast
open Jc_fenv

open Jc_name
open Jc_constructors
open Jc_pervasives
open Jc_iterators
open Jc_struct_tools

open Format
open Pp

type precision_mode = MApprox | MPrecise
let current_mode = ref MApprox
  
(* Constant memories *)
let constant_memories = Hashtbl.create 17

(* Constant allocation tables *)
let constant_alloc_tables = Hashtbl.create 17

(* Constant allocation tables *)
let constant_tag_tables = Hashtbl.create 17

let add_constant_memory (mc,r) =
  Hashtbl.add constant_memories (memory_name (mc,r)) (mc,r)

let add_constant_alloc_table (ac,r) =
  Hashtbl.add constant_alloc_tables (alloc_table_name (ac,r)) (ac,r)

let add_constant_tag_table (vi,r) =
  Hashtbl.add constant_tag_tables (tag_table_name (vi,r)) (vi,r)

(* Transposition for calls *)

let transpose_labels ~label_assoc labs =
  match label_assoc with
    | None -> labs
    | Some assoc ->
	LogicLabelSet.fold
	  (fun lab acc ->
	     let lab = try List.assoc lab assoc with Not_found -> lab in
	     LogicLabelSet.add lab acc)
	  labs LogicLabelSet.empty

let transpose_region ~region_assoc r =
  if Region.polymorphic r then
    try Some (RegionList.assoc r region_assoc)
    with Not_found -> None (* Local region *)
  else Some r

(* TODO: complete for recursive *)
let make_region_mem_assoc ~params =
  let rec aux acc r ty =
    if Region.bitwise r then
      match ty with
	| JCTpointer(pc,_,_) ->
	    let all_mems = all_memories pc in
	    List.fold_left (fun acc mc -> MemorySet.add (mc,r) acc) acc all_mems
	| JCTnative _ | JCTlogic _ | JCTenum _ | JCTnull | JCTany -> acc
	| JCTtype_var _ -> assert false
    else acc
  in
  List.fold_left 
    (fun acc v -> aux acc v.jc_var_info_region v.jc_var_info_type) 
    MemorySet.empty params

let transpose_alloc_table ~region_mem_assoc (ac,r) =
  if Region.bitwise r then
    try
      (* Translate bitwise allocation table into typed ones *)
      let mems = MemorySet.find_region r region_mem_assoc in
      MemorySet.fold (fun (mc,r) acc ->
		       let ac = alloc_class_of_mem_class mc in
		       AllocSet.add (ac,r) acc
		    ) mems AllocSet.empty
    with Not_found ->
      (* No possible effect on caller types *)
      AllocSet.empty
  else AllocSet.singleton (ac,r)

let transpose_tag_table ~region_mem_assoc (vi,r) =
  if Region.bitwise r then
    try
      (* Translate bitwise tag table into typed ones *)
      let mems = MemorySet.find_region r region_mem_assoc in
      MemorySet.fold (fun (mc,r) acc ->
		       let vi = variant_of_mem_class mc in
		       TagSet.add (vi,r) acc
		     ) mems TagSet.empty
    with Not_found ->
      (* No possible effect on caller types *)
      TagSet.empty
  else TagSet.singleton (vi,r)

let transpose_memory ~region_mem_assoc (mc,r) =
  if Region.bitwise r then
    try
      (* Translate bitwise memories into typed ones *)
      MemorySet.find_region r region_mem_assoc
    with Not_found ->
      (* No possible effect on caller types *)
      MemorySet.empty
  else MemorySet.singleton (mc,r)

let has_alloc_table (ac,r) alloc_effect =
  let allocs = AllocSet.of_list (AllocMap.keys alloc_effect) in
  if Region.bitwise r then AllocSet.mem_region r allocs
  else AllocSet.mem (ac,r) allocs

let has_memory (mc,r) mem_effect =
  let mems = MemorySet.of_list (MemoryMap.keys mem_effect) in
  if Region.bitwise r then MemorySet.mem_region r mems
  else MemorySet.mem (mc,r) mems

(* Printing effects *)

let print_list_assoc_label pr fmt ls =
  fprintf fmt "%a" 
    (print_list comma 
       (fun fmt (k,labs) ->
	  fprintf fmt "%a (%a)" 
	    pr k
	    (print_list comma Jc_output_misc.label)
	    (LogicLabelSet.elements labs))
    ) ls

let print_alloc_table fmt (ac,r) =
  fprintf fmt "(%a,%a)" Jc_output_misc.alloc_class ac Region.print r

let print_tag_table fmt (vi,r) =
  fprintf fmt "(%s,%a)" vi.jc_root_info_name Region.print r

let print_memory fmt (mc,r) =
  fprintf fmt "(%a,%a)" Jc_output_misc.memory_class mc Region.print r

let print_location fmt (loc,(mc,r)) =
  fprintf fmt "(%a,%a)" Jc_output.location loc print_memory (mc,r)

let print_variable fmt v =
  fprintf fmt "%s" v.jc_var_info_name

let print_exception fmt exc =
  fprintf fmt "%s" exc.jc_exception_info_name

let print_effect fmt ef =
  fprintf fmt 
"@[@[ alloc_table: @[%a@]@]@\n\
@[ tag_table: @[%a@]@]@\n\
@[ memories: @[%a@]@]@\n\
@[ raw memories: @[%a@]@]@\n\
@[ precise memories: @[%a@]@]@\n\
@[ globals: @[%a@]@]@\n\
@[ locals: @[%a@]@]@]@." 
    (print_list_assoc_label print_alloc_table)
    (AllocMap.elements ef.jc_effect_alloc_tables)
    (print_list_assoc_label print_tag_table)
    (TagMap.elements ef.jc_effect_tag_tables)
    (print_list_assoc_label print_memory)
    (MemoryMap.elements ef.jc_effect_memories)
    (print_list_assoc_label print_memory)
    (MemoryMap.elements ef.jc_effect_raw_memories)
    (print_list_assoc_label print_location)
    (LocationMap.elements ef.jc_effect_precise_memories)
    (print_list_assoc_label print_variable)
    (VarMap.elements ef.jc_effect_globals)
    (print_list_assoc_label print_variable)
    (VarMap.elements ef.jc_effect_locals)

(* Operations on effects *)

let ef_union ef1 ef2 = 
  { 
    jc_effect_alloc_tables = 
      AllocMap.merge LogicLabelSet.union
	ef1.jc_effect_alloc_tables ef2.jc_effect_alloc_tables;
    jc_effect_tag_tables = 
      TagMap.merge LogicLabelSet.union
	ef1.jc_effect_tag_tables ef2.jc_effect_tag_tables;
    jc_effect_raw_memories = 
      MemoryMap.merge LogicLabelSet.union
	ef1.jc_effect_raw_memories ef2.jc_effect_raw_memories;
    jc_effect_precise_memories = 
      LocationMap.merge LogicLabelSet.union
	ef1.jc_effect_precise_memories ef2.jc_effect_precise_memories;
    jc_effect_memories = 
      MemoryMap.merge LogicLabelSet.union
	ef1.jc_effect_memories ef2.jc_effect_memories;
    jc_effect_globals = 
      VarMap.merge LogicLabelSet.union 
	ef1.jc_effect_globals ef2.jc_effect_globals;
    jc_effect_locals = 
      VarMap.merge LogicLabelSet.union 
	ef1.jc_effect_locals ef2.jc_effect_locals;
    jc_effect_mutable =
      StringSet.union
	ef1.jc_effect_mutable ef2.jc_effect_mutable;
    jc_effect_committed =
      StringSet.union
	ef1.jc_effect_committed ef2.jc_effect_committed;
  }

let ef_filter_labels ~label_assoc ef =
  let filter_labels labs =
    List.fold_left
      (fun acc (l1,l2) ->
	 if LogicLabelSet.mem l2 labs then LogicLabelSet.add l1 acc else acc)
      LogicLabelSet.empty label_assoc
  in
  { ef with
      jc_effect_memories =
        MemoryMap.fold 
	  (fun m labs acc ->
	     MemoryMap.add m (filter_labels labs) acc
	  ) ef.jc_effect_memories MemoryMap.empty;
  }

let ef_assoc ?label_assoc ~region_assoc ~region_mem_assoc ef =
  { ef with 
      jc_effect_alloc_tables = 
        AllocMap.fold 
	  (fun (ac,distr) labs acc ->
	     let labs = transpose_labels ~label_assoc labs in
	     match transpose_region ~region_assoc distr with
	       | None -> acc
	       | Some locr ->
		   let allocs = 
		     transpose_alloc_table ~region_mem_assoc (ac,distr)
		   in
		   AllocSet.fold 
		     (fun (ac,_r) acc ->
			if not (Region.polymorphic locr) then
			  add_constant_alloc_table (ac,locr);
			AllocMap.add (ac,locr) labs acc 
		     ) allocs acc
	  ) ef.jc_effect_alloc_tables AllocMap.empty;
      jc_effect_tag_tables =
        TagMap.fold 
	  (fun (vi,distr) labs acc -> 
	     let labs = transpose_labels ~label_assoc labs in
	     match transpose_region ~region_assoc distr with
	       | None -> acc
	       | Some locr ->
		   let tags = 
		     transpose_tag_table ~region_mem_assoc (vi,distr)
		   in
		   TagSet.fold 
		     (fun (vi,_r) acc ->
			if not (Region.polymorphic locr) then
			  add_constant_tag_table (vi,locr);
			TagMap.add (vi,locr) labs acc 
		     ) tags acc
	  ) ef.jc_effect_tag_tables TagMap.empty;
      jc_effect_raw_memories =
        MemoryMap.fold 
	  (fun (mc,distr) labs acc ->
	     let labs = transpose_labels ~label_assoc labs in
	     match transpose_region ~region_assoc distr with
	       | None -> acc
	       | Some locr ->
		   let mems = 
		     transpose_memory ~region_mem_assoc (mc,distr)
		   in
		   MemorySet.fold 
		     (fun (mc,_r) acc ->
			if not (Region.polymorphic locr) then 
			  add_constant_memory (mc,locr);
			MemoryMap.add (mc,locr) labs acc 
		     ) mems acc
	  ) ef.jc_effect_raw_memories MemoryMap.empty;
      jc_effect_memories =
        MemoryMap.fold 
	  (fun (mc,distr) labs acc ->
	     let labs = transpose_labels ~label_assoc labs in
	     match transpose_region ~region_assoc distr with
	       | None -> acc
	       | Some locr ->
		   let mems = 
		     transpose_memory ~region_mem_assoc (mc,distr)
		   in
		   MemorySet.fold 
		     (fun (mc,_r) acc ->
			if not (Region.polymorphic locr) then 
			  add_constant_memory (mc,locr);
			MemoryMap.add (mc,locr) labs acc 
		     ) mems acc
	  ) ef.jc_effect_memories MemoryMap.empty;
      jc_effect_globals =
        VarMap.fold 
	  (fun v labs acc -> 
	     VarMap.add v (transpose_labels ~label_assoc labs) acc
	  ) ef.jc_effect_globals VarMap.empty;
      jc_effect_locals = VarMap.empty;
  }

let same_effects ef1 ef2 =
  let eq = LogicLabelSet.equal in
  AllocMap.equal eq ef1.jc_effect_alloc_tables ef2.jc_effect_alloc_tables
  && TagMap.equal eq ef1.jc_effect_tag_tables ef2.jc_effect_tag_tables
  && MemoryMap.equal eq ef1.jc_effect_raw_memories ef2.jc_effect_raw_memories
  && LocationMap.equal eq
    ef1.jc_effect_precise_memories ef2.jc_effect_precise_memories
  && MemoryMap.equal eq ef1.jc_effect_memories ef2.jc_effect_memories
  && VarMap.equal eq ef1.jc_effect_globals ef2.jc_effect_globals
  && VarMap.equal eq ef1.jc_effect_locals ef2.jc_effect_locals
  && StringSet.equal ef1.jc_effect_mutable ef2.jc_effect_mutable
  && StringSet.equal ef1.jc_effect_committed ef2.jc_effect_committed
    
(* Operations on function effects *)

let fef_reads ef =
  { 
    jc_reads = ef;
    jc_writes = empty_effects;
    jc_raises = ExceptionSet.empty;
  }

let fef_union fef1 fef2 =
  { 
    jc_reads = ef_union fef1.jc_reads fef2.jc_reads;
    jc_writes = ef_union fef1.jc_writes fef2.jc_writes;
    jc_raises = ExceptionSet.union fef1.jc_raises fef2.jc_raises;
  }

let fef_assoc ~region_assoc ~region_mem_assoc fef =
  { 
    jc_reads = ef_assoc ~region_assoc ~region_mem_assoc fef.jc_reads;
    jc_writes = ef_assoc ~region_assoc ~region_mem_assoc fef.jc_writes;
    jc_raises = fef.jc_raises;
  }

let same_feffects fef1 fef2 =
  same_effects fef1.jc_reads fef2.jc_reads 
  && same_effects fef1.jc_writes fef2.jc_writes 
  && ExceptionSet.equal fef1.jc_raises fef2.jc_raises

(* Query of a single effect *)

let has_memory_effect ef (mc,r) =
  try
    ignore (MemoryMap.find (mc,r) ef.jc_effect_memories);
    true
  with Not_found ->
    try
      ignore (MemoryMap.find (mc,r) ef.jc_effect_raw_memories);
      true
    with Not_found ->
      let locs = 
	LocationMap.filter 
	  (fun (loc,mem) _ -> Memory.equal mem (mc,r))
	  ef.jc_effect_precise_memories
      in
      not (LocationMap.is_empty locs)

(* Addition of a single effect *)
    
let add_alloc_effect lab ef (ac, r) =
  if not (Region.polymorphic r) then add_constant_alloc_table (ac,r);
  let labs = LogicLabelSet.singleton lab in
  { ef with jc_effect_alloc_tables = 
      AllocMap.add_merge LogicLabelSet.union 
	(ac,r) labs ef.jc_effect_alloc_tables }

let add_tag_effect lab ef (vi,r) =
  if not (Region.polymorphic r) then add_constant_tag_table (vi,r);
  let labs = LogicLabelSet.singleton lab in
  { ef with jc_effect_tag_tables = 
      TagMap.add_merge LogicLabelSet.union 
	(vi,r) labs ef.jc_effect_tag_tables }

let add_memory_effect lab ef (mc,r) =
  if not (Region.polymorphic r) then add_constant_memory (mc,r);
  let labs = LogicLabelSet.singleton lab in
  match !current_mode with
    | MApprox ->
	{ ef with jc_effect_memories = 
	    MemoryMap.add_merge LogicLabelSet.union 
	      (mc,r) labs ef.jc_effect_memories }
    | MPrecise ->
	{ ef with jc_effect_raw_memories = 
	    MemoryMap.add_merge LogicLabelSet.union 
	      (mc,r) labs ef.jc_effect_raw_memories }

let add_precise_memory_effect lab ef (loc,(mc,r)) =
  let labs = LogicLabelSet.singleton lab in
  { ef with jc_effect_precise_memories = 
      LocationMap.add_merge LogicLabelSet.union 
	(loc,(mc,r)) labs ef.jc_effect_precise_memories }

let add_global_effect lab ef v =
  let labs = LogicLabelSet.singleton lab in
  { ef with jc_effect_globals = 
      VarMap.add_merge LogicLabelSet.union v labs ef.jc_effect_globals } 

let add_local_effect lab ef v =
  let labs = LogicLabelSet.singleton lab in
  { ef with jc_effect_locals = 
      VarMap.add_merge LogicLabelSet.union v labs ef.jc_effect_locals } 

let add_mutable_effect ef pc =
  { ef with jc_effect_mutable = StringSet.add
      (pointer_class_type_name pc) ef.jc_effect_mutable }
  
let add_committed_effect ef pc =
  { ef with jc_effect_committed = StringSet.add
      (pointer_class_type_name pc) ef.jc_effect_committed }

(* Addition of a single read *)

let add_alloc_reads lab fef (ac,r) =
  { fef with jc_reads = add_alloc_effect lab fef.jc_reads (ac,r) }

let add_tag_reads lab fef (vi,r) =
  { fef with jc_reads = add_tag_effect lab fef.jc_reads (vi,r) }

let add_memory_reads lab fef (mc,r) =
  { fef with jc_reads = add_memory_effect lab fef.jc_reads (mc,r) }

let add_precise_memory_reads lab fef (loc,(mc,r)) =
  { fef with jc_reads = 
      add_precise_memory_effect lab fef.jc_reads (loc,(mc,r)) }

let add_global_reads lab fef v =
  { fef with jc_reads = add_global_effect lab fef.jc_reads v }

let add_local_reads lab fef v =
  { fef with jc_reads = add_local_effect lab fef.jc_reads v }

let add_mutable_reads fef pc =
  { fef with jc_reads = add_mutable_effect fef.jc_reads pc }

let add_committed_reads fef pc =
  { fef with jc_reads = add_committed_effect fef.jc_reads pc }

(* Addition of a single write *)

let add_alloc_writes lab fef (ac,r) =
  { fef with jc_writes = add_alloc_effect lab fef.jc_writes (ac,r) }

let add_tag_writes lab fef (vi,r) =
  { fef with jc_writes = add_tag_effect lab fef.jc_writes (vi,r) }

let add_memory_writes lab fef (mc,r) =
  { fef with jc_writes = add_memory_effect lab fef.jc_writes (mc,r) }

let add_precise_memory_writes lab fef (loc,(mc,r)) =
  { fef with jc_writes = 
      add_precise_memory_effect lab fef.jc_writes (loc,(mc,r)) }

let add_global_writes lab fef vi =
  { fef with jc_writes = add_global_effect lab fef.jc_writes vi }

let add_local_writes lab fef vi =
  { fef with jc_writes = add_local_effect lab fef.jc_writes vi }

let add_mutable_writes fef pc =
  { fef with jc_writes = add_mutable_effect fef.jc_writes pc }

let add_committed_writes fef pc =
  { fef with jc_writes = add_committed_effect fef.jc_writes pc }

(* Addition of a single exception *)

let add_exception_effect fef exc =
  { fef with jc_raises = ExceptionSet.add exc fef.jc_raises }


(*****************************************************************************)
(*                                  Unions                                   *)
(*****************************************************************************)

type shift_offset = 
  | Int_offset of string
  | Expr_offset of Jc_fenv.expr 
  | Term_offset of Jc_fenv.term 

let offset_of_expr e =
  match e#node with
    | JCEconst (JCCinteger s) -> Int_offset s
    | _ -> Expr_offset e

let offset_of_term t =
  match t#node with
    | JCTconst (JCCinteger s) -> Int_offset s
    | _ -> Term_offset t

let offset_of_field fi = 
  match field_offset_in_bytes fi with
    | None -> assert false
    | Some off -> Int_offset (string_of_int off) 

let mult_offset i off =
  match off with
    | Int_offset j -> Int_offset (string_of_int (i * (int_of_string j)))
    | Expr_offset e -> 
	let ie = Expr.mkint ~value:i ()  in
	let mule = 
	  Expr.mkbinary 
	    ~expr1:ie ~op:(`Bmul,`Integer) ~expr2:e ~typ:integer_type ()
	in
	Expr_offset mule
    | Term_offset t ->
	let it = Term.mkint ~value:i ()  in
	let mult = 
	  Term.mkbinary 
	    ~term1:it ~op:(`Bmul,`Integer) ~term2:t ~typ:integer_type ()
	in
	Term_offset mult

let add_offset off1 off2 = 
  match off1,off2 with
    | Int_offset i, Int_offset j -> 
	let k = int_of_string i + int_of_string j in
	Int_offset (string_of_int k)
    | Expr_offset e1, Expr_offset e2 ->
	let adde = 
	  Expr.mkbinary 
	    ~expr1:e1 ~op:(`Badd,`Integer) ~expr2:e2 ~typ:integer_type ()
	in
	Expr_offset adde
    | Expr_offset e, Int_offset i
    | Int_offset i, Expr_offset e ->
	let ie = Expr.mkint ~valuestr:i ()  in
	let adde = 
	  Expr.mkbinary 
	    ~expr1:ie ~op:(`Badd,`Integer) ~expr2:e ~typ:integer_type ()
	in
	Expr_offset adde
    | Term_offset t1, Term_offset t2 ->
	let addt = 
	  Term.mkbinary 
	    ~term1:t1 ~op:(`Badd,`Integer) ~term2:t2 ~typ:integer_type ()
	in
	Term_offset addt
    | Term_offset t, Int_offset i
    | Int_offset i, Term_offset t ->
	let it = Term.mkint ~valuestr:i ()  in
	let addt = 
	  Term.mkbinary 
	    ~term1:it ~op:(`Badd,`Integer) ~term2:t ~typ:integer_type ()
	in
	Term_offset addt
    | Expr_offset _, Term_offset _
    | Term_offset _, Expr_offset _ -> assert false

let possible_union_type = function
  | JCTpointer(pc,_,_) -> 
      let rt = pointer_class_root pc in
      if root_is_union rt then Some rt else None
  | _ -> None

let union_type ty = 
  match possible_union_type ty with Some rt -> rt | None -> assert false

let type_is_union ty =
  match possible_union_type ty with Some _rt -> true | None -> false

let overlapping_union_memories fi =
  let st = fi.jc_field_info_struct in
  let rt = struct_root st in
  let stlist = 
    List.filter
      (fun st' -> not (st.jc_struct_info_name = st'.jc_struct_info_name))
      rt.jc_root_info_hroots
  in
  let mems = 
    List.flatten 
      (List.map
	 (fun st -> all_memories ~select:fully_allocated (JCtag(st,[]))) 
	 stlist)
  in
  MemClassSet.to_list (MemClassSet.of_list mems)

let possible_union_access e fi_opt = 
  let fieldoffbytes fi = 
    match field_offset_in_bytes fi with
      | None -> assert false
      | Some off -> Int_offset (string_of_int off) 
  in
  (* Count offset in bytes before last field access in union *)
  let rec access e = 
    match e#node with
      | JCEderef(e,fi) when embedded_field fi ->
	  begin match access e with
	    | Some(e,off) ->
		Some (e, add_offset off (fieldoffbytes fi))
	    | None -> 
		if type_is_union e#typ then
		  Some (e, fieldoffbytes fi)
		else None
	  end
      | JCEshift(e1,e2) ->
	  begin match access e1 with
	    | Some(e,off1) ->
		let off2 = offset_of_expr e2 in
		let siz = struct_size_in_bytes (pointer_struct e1#typ) in
		let off2 = mult_offset siz off2 in
		Some (e, add_offset off1 off2)
	    | None -> None
	  end
      | JCEalloc(_e1,st) ->
	  if struct_of_union st then
	    Some(e,Int_offset "0")
	  else None
      | _ ->	
	  if type_is_union e#typ then
	    Some (e,Int_offset "0")
	  else None
  in
  match fi_opt with
    | None -> access e
    | Some fi ->
	match access e with
	  | Some(e,off) -> Some (e, add_offset off (fieldoffbytes fi))
	  | None -> 
	      if type_is_union e#typ then Some (e, fieldoffbytes fi) else None

(* Optionally returns a triple of
   1) a prefix [ue] of type union
   2) the field of the union [ue] accessed
   3) the offset from the address of [ue] to the final field
*)
let possible_union_deref e fi = 
  let rec access e fi = 
    match e#node with
      | JCEderef(e1,fi1) when embedded_field fi1 ->
	  begin match access e1 fi1 with
	    | Some(e2,fi2,off) ->
		Some(e2,fi2,add_offset off (offset_of_field fi))
	    | None -> 
		if type_is_union e1#typ then 
		  Some(e1,fi,offset_of_field fi)
		else None
	  end
      | JCEshift(e1,e2) ->
	  begin match access e1 fi with
	    | Some(e2,fi2,off1) ->
		let off2 = offset_of_expr e2 in
		let siz = struct_size_in_bytes (pointer_struct e1#typ) in
		let off2 = mult_offset siz off2 in
		Some(e2,fi2,add_offset off1 off2)
	    | None -> None
	  end
      | _ -> None
  in
  match access e fi with
    | Some(e1,fi1,off) -> Some(e1,fi1,add_offset off (offset_of_field fi))
    | None -> 
	if type_is_union e#typ then Some(e,fi,offset_of_field fi) else None

let destruct_union_access e fi_opt = 
  match possible_union_access e fi_opt with
    | Some x -> x
    | None -> assert false

let tpossible_union_access t fi_opt =
  let fieldoffbytes fi = 
    match field_offset_in_bytes fi with
      | None -> assert false
      | Some off -> Int_offset (string_of_int off) 
  in
  (* Count offset in bytes before last field access in union *)
  let rec access t = 
    match t#node with
      | JCTderef(t,_lab,fi) when embedded_field fi ->
	  begin match access t with
	    | Some(t,off) ->
		Some (t, add_offset off (fieldoffbytes fi))
	    | None -> 
		if type_is_union t#typ then
		  Some (t, fieldoffbytes fi)
		else None
	  end
      | JCTshift(t1,t2) ->
	  begin match access t1 with
	    | Some(t3,off1) ->
		let off2 = offset_of_term t2 in
		let siz = struct_size_in_bytes (pointer_struct t1#typ) in
		let off2 = mult_offset siz off2 in
		Some (t3, add_offset off1 off2)
	    | None -> None
	  end
      | _ ->	
	  if type_is_union t#typ then
	    Some (t,Int_offset "0")
	  else None
  in

  match fi_opt with
    | None ->
	access t
    | Some fi ->
(* 	let fieldoff fi = Int_offset (string_of_int (field_offset fi)) in *)
	match access t with
	  | Some(t,off) ->
	      Some (t, add_offset off (fieldoffbytes fi))
	  | None -> 
	      if type_is_union t#typ then
		Some (t, fieldoffbytes fi)
	      else None

let tpossible_union_deref t fi = 
  let rec access t fi = 
    match t#node with
      | JCTderef(t1,_lab,fi1) when embedded_field fi1 ->
	  begin match access t1 fi1 with
	    | Some(t2,fi2,off) ->
		Some(t2,fi2,add_offset off (offset_of_field fi))
	    | None -> 
		if type_is_union t1#typ then 
		  Some(t1,fi,offset_of_field fi)
		else None
	  end
      | JCTshift(t1,t2) ->
	  begin match access t1 fi with
	    | Some(t2,fi2,off1) ->
		let off2 = offset_of_term t2 in
		let siz = struct_size_in_bytes (pointer_struct t1#typ) in
		let off2 = mult_offset siz off2 in
		Some(t2,fi2,add_offset off1 off2)
	    | None -> None
	  end
      | _ -> None
  in
  match access t fi with
    | Some(t1,fi1,off) -> Some(t1,fi1,add_offset off (offset_of_field fi))
    | None -> 
	if type_is_union t#typ then Some(t,fi,offset_of_field fi) else None

let tdestruct_union_access t fi_opt = 
  match tpossible_union_access t fi_opt with
    | Some x -> x
    | None -> assert false

let lpossible_union_access t fi = None (* TODO *)

let lpossible_union_deref t fi = None (* TODO *)

let ldestruct_union_access loc fi_opt = 
  match lpossible_union_access loc fi_opt with
    | Some x -> x
    | None -> assert false

let foreign_union e = [] (* TODO: subterms of union that are not in union *)
let tforeign_union t = []

let common_deref_alloc_class ~type_safe union_access e =
  if not type_safe && Region.bitwise e#region then
    JCalloc_bitvector
  else match union_access e None with 
    | None -> JCalloc_root (struct_root (pointer_struct e#typ))
    | Some(e,_off) -> JCalloc_root (union_type e#typ)

let deref_alloc_class ~type_safe e =
  common_deref_alloc_class ~type_safe possible_union_access e
  
let tderef_alloc_class ~type_safe t =
  common_deref_alloc_class ~type_safe tpossible_union_access t

let lderef_alloc_class ~type_safe locs =
  common_deref_alloc_class ~type_safe lpossible_union_access locs

let common_deref_mem_class ~type_safe union_deref e fi =
  if not type_safe && Region.bitwise e#region then
    JCmem_bitvector, None
  else match union_deref e fi with 
    | None -> 
	assert (not (root_is_union (struct_root fi.jc_field_info_struct)));
	JCmem_field fi, None
    | Some(e1,fi1,_off) -> 
	let rt = union_type e1#typ in
	if root_is_plain_union rt then JCmem_plain_union rt, Some fi1 
	else JCmem_field fi, Some fi1

let deref_mem_class ~type_safe e fi =
  common_deref_mem_class ~type_safe possible_union_deref e fi

let tderef_mem_class ~type_safe t fi =
  common_deref_mem_class ~type_safe tpossible_union_deref t fi

let lderef_mem_class ~type_safe locs fi =
  common_deref_mem_class ~type_safe lpossible_union_deref locs fi


(******************************************************************************)
(*                                  patterns                                  *)
(******************************************************************************)

(* TODO: check the use of "label" and "r" *)
let rec pattern ef (*label r*) p =
  let r = dummy_region in
  match p#node with
    | JCPstruct(st, fpl) ->
	let ef = add_tag_effect (*label*)LabelHere ef (struct_root st,r) in
	List.fold_left
	  (fun ef (fi, pat) ->
	     let mc = JCmem_field fi in
	     let ef = add_memory_effect (*label*)LabelHere ef (mc,r) in
	     pattern ef (*label r*) pat)
	  ef fpl
    | JCPor(p1, p2) ->
	pattern (pattern ef (*label r*) p1) (*label r*) p2
    | JCPas(p, _) ->
	pattern ef (*label r*) p
    | JCPvar _
    | JCPany
    | JCPconst _ ->
	ef


(******************************************************************************)
(*                              environment                                   *)
(******************************************************************************)

let current_function = ref None
let set_current_function f = current_function := Some f
let reset_current_function () = current_function := None
let get_current_function () = 
  match !current_function with None -> assert false | Some f -> f

(******************************************************************************)
(*                             immutable locations                            *)
(******************************************************************************)

let term_of_expr e =
  let rec term e = 
    let tnode = match e#node with
      | JCEconst c -> JCTconst c
      | JCEvar vi -> JCTvar vi
      | JCEbinary (e1, (bop,opty), e2) -> 
	  JCTbinary (term e1, ((bop :> bin_op),opty), term e2)
      | JCEunary (uop, e1) -> JCTunary (uop, term e1)
      | JCEshift (e1, e2) -> JCTshift (term e1, term e2)
      | JCEderef (e1, fi) -> JCTderef (term e1, LabelHere, fi)
      | JCEinstanceof (e1, st) -> JCTinstanceof (term e1, LabelHere, st)
      | JCEcast (e1, st) -> JCTcast (term e1, LabelHere, st)
      | JCErange_cast(e1,_) | JCEreal_cast(e1,_) -> 
	  (* range does not modify term value *)
	  (term e1)#node 
      | JCEif (e1, e2, e3) -> JCTif (term e1, term e2, term e3)
      | JCEoffset (off, e1, st) -> JCToffset (off, term e1, st)
      | JCEalloc (e, _) -> (* Note: \offset_max(t) = length(t) - 1 *)
	  JCTbinary (term e, (`Bsub,`Integer), new term ~typ:integer_type (JCTconst (JCCinteger "1")) )
      | JCEfree _ -> failwith "Not a term"
      | _ -> failwith "Not a term"
(*       | JCEmatch (e, pel) -> *)
(* 	  let ptl = List.map (fun (p, e) -> (p, term_of_expr e)) pel in *)
(* 	    JCTmatch (term_of_expr e, ptl) *)
    in
      new term ~typ:e#typ ~region:e#region tnode 
  in
    try Some (term e) with Failure _ -> None

let rec location_of_expr e = 
  try
    let loc_node = match e#node with
      | JCEvar v -> 
	  JCLvar v
      | JCEderef(e1,fi) ->
	  JCLderef(location_set_of_expr e1, LabelHere, fi, e#region)
      | _ -> failwith "No location for expr"
    in
    Some(new location_with ~node:loc_node e)
  with Failure "No location for expr" -> None

and location_set_of_expr e =
  let locs_node = match e#node with
    | JCEvar v -> 
	JCLSvar v
    | JCEderef(e1,fi) ->
	JCLSderef(location_set_of_expr e1, LabelHere, fi, e#region)
    | JCEshift(e1,e2) ->
	let t2_opt = term_of_expr e2 in
	JCLSrange(location_set_of_expr e1, t2_opt, t2_opt)
    | _ -> failwith "No location for expr"
  in
  new location_set_with ~node:locs_node e

let location_set_of_expr e =
  try Some(location_set_of_expr e) with Failure "No location for expr" -> None

let rec location_of_term t = 
  try
    let loc_node = match t#node with
      | JCTvar v -> 
	  JCLvar v
      | JCTderef(t1,lab,fi) ->
	  JCLderef(location_set_of_term t1, LabelHere, fi, t#region)
      | _ -> failwith "No location for term"
    in
    Some(new location_with ~node:loc_node t)
  with Failure "No location for term" -> None

and location_set_of_term t =
  let locs_node = match t#node with
    | JCTvar v -> 
	JCLSvar v
    | JCTderef(t1,lab,fi) ->
	JCLSderef(location_set_of_term t1, LabelHere, fi, t#region)
    | _ -> failwith "No location for term"
  in
  new location_set_with ~node:locs_node t


(* last location can be mutated *)
let rec immutable_location fef loc =
  match loc#node with
    | JCLvar v -> true
    | JCLderef(locs,lab,fi,_r) ->
	immutable_location_set fef locs 
    | _ -> false

and immutable_location_set fef locs =
  match locs#node with
    | JCLSvar v -> not v.jc_var_info_assigned
    | JCLSderef(locs,lab,fi,_r) ->
	let mc,_fi_opt = lderef_mem_class ~type_safe:true locs fi in
	immutable_location_set fef locs 
	&& not (MemoryMap.mem (mc,locs#region) fef.jc_writes.jc_effect_memories)
    | _ -> false

let expr_immutable_location e =
  match !current_mode with
    | MApprox -> None
    | MPrecise ->
	match !current_function with
	  | None -> None
	  | Some f ->
	      let fef = f.jc_fun_info_effects in
	      match location_of_expr e with
		| None -> None
		| Some loc ->
		    if immutable_location fef loc then
		      Some loc
		    else None

let term_immutable_location t =
  match !current_mode with
    | MApprox -> None
    | MPrecise ->
	match !current_function with
	  | None -> None
	  | Some f ->
	      let fef = f.jc_fun_info_effects in
	      match location_of_term t with
		| None -> None
		| Some loc ->
		    if immutable_location fef loc then
		      Some loc
		    else None

(* let immutable_heap_location fef ~full_expr e1 fi = *)
(*   match !current_mode with *)
(*     | MApprox -> None *)
(*     | MPrecise -> *)
(* 	match e1#node with *)
(* 	  | JCEvar v -> *)
(* 	      if v.jc_var_info_assigned then  *)
(* 		None *)
(* 	      else *)
(* 		Some(new location_with ~node:(JCLvar v) full_expr) *)
(* 	  | _ -> None *)

(* let timmutable_heap_location ef ~full_term t1 fi = (\* TODO: fef *\) *)
(*   match !current_mode with *)
(*     | MApprox -> None *)
(*     | MPrecise -> *)
(* 	match t1#node with *)
(* 	  | JCTvar v -> *)
(* 	      if v.jc_var_info_assigned then  *)
(* 		None *)
(* 	      else *)
(* 		Some(new location_with ~node:(JCLvar v) full_term) *)
(* 	  | _ -> None *)
	      

(******************************************************************************)
(*                             terms and assertions                           *)
(******************************************************************************)

let rec single_term ef t =
  let lab = 
    match t#label with None -> LabelHere | Some lab -> lab
  in
  match t#node with
    | JCTvar vi ->
	true,
	if vi.jc_var_info_assigned then
	  if vi.jc_var_info_static then
	    add_global_effect lab ef vi
	  else 
	    add_local_effect lab ef vi
	else ef
    | JCToffset(_k,t,st) ->
        let ac = tderef_alloc_class ~type_safe:true t in
	true,
	add_alloc_effect lab ef (ac,t#region)
    | JCTapp app -> 
	let region_mem_assoc = 
	  make_region_mem_assoc app.jc_app_fun.jc_logic_info_parameters
	in
	let ef_app = 
	  ef_assoc 
	    ~label_assoc:app.jc_app_label_assoc 
	    ~region_assoc:app.jc_app_region_assoc 
	    ~region_mem_assoc
	    app.jc_app_fun.jc_logic_info_effects 
	in
	true,
	ef_union ef_app ef
    | JCTderef(t1,lab,fi) ->
	let mc,ufi_opt = tderef_mem_class ~type_safe:true t1 fi in
	let mem = mc, t1#region in
	begin match term_immutable_location t with
	  | Some loc ->
	      let ef = add_precise_memory_effect lab ef (loc,mem) in
	      (* TODO: treat union *)
	      true, ef
	  | None ->
	      let ef = add_memory_effect lab ef mem in
	      begin match mc,ufi_opt with
		| JCmem_plain_union _vi, _ -> 
		    false, (* do not call on sub-terms of union *)
		    List.fold_left term ef (tforeign_union t1)
		| JCmem_field _, _
		| JCmem_bitvector, _ ->
		    true, ef
	      end
	end
    | JCTcast(t,lab,st)
    | JCTinstanceof(t,lab,st) ->
	true,
	add_tag_effect lab ef (struct_root st,t#region)
    | JCTmatch(t,ptl) ->
	true,
	List.fold_left pattern ef (List.map fst ptl)
    | JCTconst _ | JCTrange _ | JCTbinary _ | JCTunary _
    | JCTshift _ | JCTold _ | JCTat _ | JCTaddress _ | JCTbase_block _
    | JCTbitwise_cast _ | JCTrange_cast _ | JCTreal_cast _ | JCTif _ ->
	true, ef

and term ef t =
  fold_rec_term single_term ef t

let tag ef lab t vi_opt r =
  match vi_opt with
    | None -> ef
    | Some vi -> add_tag_effect lab ef (vi,r)

let single_assertion ef a =
  let lab = 
    match a#label with None -> LabelHere | Some lab -> lab
  in
  match a#node with
    | JCAinstanceof(t,lab,st) -> 
	true,
	add_tag_effect lab ef (struct_root st,t#region)
    | JCAapp app -> 
	let region_mem_assoc = 
	  make_region_mem_assoc app.jc_app_fun.jc_logic_info_parameters
	in
	let ef_app = 
	  ef_assoc
	    ~label_assoc:app.jc_app_label_assoc 
	    ~region_assoc:app.jc_app_region_assoc 
	    ~region_mem_assoc
	    app.jc_app_fun.jc_logic_info_effects 
	in
	true,
	ef_union ef_app ef
    | JCAmutable(t,st,ta) ->
	true,
	add_mutable_effect
	  (tag ef lab ta (* Yannick: really effect on tag here? *)
	     (Some (struct_root st)) t#region)
	  (JCtag(st, []))
    | JCAmatch(t,pal) ->
	true,
	List.fold_left pattern ef (List.map fst pal)
    | JCAtrue | JCAfalse | JCAif _ | JCAbool_term _ | JCAnot _
    | JCAold _ | JCAat _ | JCAquantifier _ | JCArelation _
    | JCAand _ | JCAor _ | JCAiff _ | JCAimplies _ 
    | JCAeqtype _ | JCAsubtype _ ->
	true, ef

let assertion ef a =
  fold_rec_term_and_assertion single_term single_assertion ef a


(******************************************************************************)
(*                                locations                                   *)
(******************************************************************************)

let single_term fef t = 
  let cont,ef = single_term fef.jc_reads t in
  cont,{ fef with jc_reads = ef }

let single_assertion fef a = 
  let cont,ef = single_assertion fef.jc_reads a in
  cont,{ fef with jc_reads = ef }

let single_location ~in_assigns fef loc =
  let lab = 
    match loc#label with None -> LabelHere | Some lab -> lab
  in
  let fef = match loc#node with
    | JCLvar v ->
	if v.jc_var_info_assigned then
	  if v.jc_var_info_static then
	    if in_assigns then
	      add_global_writes lab fef v
	    else
	      add_global_reads lab fef v
	  else fef
	else fef
    | JCLderef(locs,lab,fi,_r) ->
	let add_mem ~only_writes fef mc =
	  if in_assigns then
	    let fef = add_memory_writes lab fef (mc,locs#region) in
	    if only_writes then fef else
	      (* Add effect on allocation table for [not_assigns] predicate *)
	      let ac = alloc_class_of_mem_class mc in
	      add_alloc_reads lab fef (ac,locs#region)
	  else
	    if only_writes then fef else
	      add_memory_reads lab fef (mc,locs#region)
	in
	let mc,ufi_opt = lderef_mem_class ~type_safe:true locs fi in
	let fef = add_mem ~only_writes:false fef mc in
	begin match mc,ufi_opt with
	  | JCmem_field fi, Some ufi ->
	      let mems = overlapping_union_memories ufi in
	      List.fold_left (add_mem ~only_writes:true) fef mems
	  | JCmem_field _, None 
	  | JCmem_plain_union _, _ 
	  | JCmem_bitvector, _ -> fef
	end
    | JCLderef_term(t1,fi) ->
	let add_mem ~only_writes fef mc =
	  if in_assigns then
	    let fef = add_memory_writes lab fef (mc,t1#region) in
	    if only_writes then fef else
	      (* Add effect on allocation table for [not_assigns] predicate *)
	      let ac = alloc_class_of_mem_class mc in
	      add_alloc_reads lab fef (ac,t1#region)
	  else
	    if only_writes then fef else
	      add_memory_reads lab fef (mc,t1#region)
	in
	let mc,ufi_opt = tderef_mem_class ~type_safe:true t1 fi in
	let fef = add_mem ~only_writes:false fef mc in
	begin match mc,ufi_opt with
	  | JCmem_field fi, Some ufi ->
	      let mems = overlapping_union_memories ufi in
	      List.fold_left (add_mem ~only_writes:true) fef mems
	  | JCmem_field _, None 
	  | JCmem_plain_union _, _ 
	  | JCmem_bitvector, _ -> fef
	end
    | JCLat(loc,_lab) -> fef
  in true, fef

let single_location_set fef locs =
  let lab = 
    match locs#label with None -> LabelHere | Some lab -> lab
  in
  let fef = match locs#node with
    | JCLSvar v ->
	if v.jc_var_info_assigned then
	  if v.jc_var_info_static then
	    add_global_reads lab fef v
	  else fef
	else fef
    | JCLSderef(locs,lab,fi,_r) ->
	let mc,ufi_opt = lderef_mem_class ~type_safe:true locs fi in
	add_memory_reads lab fef (mc,locs#region)
    | JCLSrange(_,_,_)
    | JCLSrange_term(_,_,_) ->
	fef
  in true, fef

let location ~in_assigns fef loc =
  fold_rec_location single_term
    (single_location ~in_assigns) single_location_set fef loc


(******************************************************************************)
(*                                expressions                                 *)
(******************************************************************************)

let rec expr fef e =
  fold_rec_expr_and_term_and_assertion single_term single_assertion
    (single_location ~in_assigns:true) single_location_set
    (fun (fef : fun_effect) e -> match e#node with
       | JCEvar v ->
	   true,
	   if v.jc_var_info_assigned then
	     if v.jc_var_info_static then
	       add_global_reads LabelHere fef v
	     else 
	       add_local_reads LabelHere fef v
	   else fef
       | JCEassign_var(v,_e) -> 
	   true,
	   if v.jc_var_info_assigned then
	     if v.jc_var_info_static then
	       add_global_writes LabelHere fef v
	     else 
	       add_local_writes LabelHere fef v
	   else fef
       | JCEoffset(_k,e,st) ->
	   let ac = deref_alloc_class ~type_safe:true e in
	   true,
	   add_alloc_reads LabelHere fef (ac,e#region)
       | JCEapp app -> 
	   let fef_call = match app.jc_call_fun with
	     | JClogic_fun f -> 
		 let region_mem_assoc = 
		   make_region_mem_assoc f.jc_logic_info_parameters
		 in
		 fef_reads 
		   (ef_assoc
		      ~label_assoc:app.jc_call_label_assoc 
		      ~region_assoc:app.jc_call_region_assoc
		      ~region_mem_assoc
		      f.jc_logic_info_effects)
	     | JCfun f -> 
		 let region_mem_assoc = 
		   make_region_mem_assoc f.jc_fun_info_parameters
		 in
		 fef_assoc 
		   ~region_assoc:app.jc_call_region_assoc f.jc_fun_info_effects 
		   ~region_mem_assoc
	   in
	   true,
	   fef_union fef_call fef
       | JCEderef(e1,fi) -> 
	   let mc,ufi_opt = deref_mem_class ~type_safe:true e1 fi in
	   let ac = alloc_class_of_mem_class mc in
	   let mem = mc, e1#region in
	   begin match expr_immutable_location e with
	     | Some loc ->
		 let fef = 
		   add_precise_memory_reads LabelHere fef (loc,mem) 
		 in
		 let fef = add_alloc_reads LabelHere fef (ac,e1#region) in
		 (* TODO: treat union *)
		 true, fef
	     | None ->
		 let fef = add_memory_reads LabelHere fef mem in
		 let fef = add_alloc_reads LabelHere fef (ac,e1#region) in
		 begin match mc,ufi_opt with
		   | JCmem_plain_union _vi, _ -> 
		       false, (* do not call on sub-expressions of union *)
		       List.fold_left expr fef (foreign_union e1)
		   | JCmem_field _, _ 
		   | JCmem_bitvector, _ ->
		       true, fef
		 end
	   end
       | JCEassign_heap(e1,fi,_e2) ->
	   let mc,ufi_opt = deref_mem_class ~type_safe:true e1 fi in
	   let ac = alloc_class_of_mem_class mc in
	   let deref = new expr_with ~node:(JCEderef(e1,fi)) e in
	   let mem = mc, e1#region in
	   begin match expr_immutable_location deref with
	     | Some loc ->
		 let fef = 
		   add_precise_memory_writes LabelHere fef (loc,mem)
		 in
		 (* allocation table is read *)
		 let fef = add_alloc_reads LabelHere fef (ac,e1#region) in
		 (* TODO: treat union *)
		 true, fef
	     | None ->
		 let fef = add_memory_writes LabelHere fef mem in
		 (* allocation table is read *)
		 let fef = add_alloc_reads LabelHere fef (ac,e1#region) in
		 begin match mc,ufi_opt with
		   | JCmem_plain_union _vi, _ -> 
		       false, (* do not call on sub-expressions of union *)
		       List.fold_left expr fef (foreign_union e1)
		   | JCmem_field fi, Some ufi ->
		       let mems = overlapping_union_memories ufi in
		       true,
		       List.fold_left 
			 (fun fef mc -> 
			    add_memory_writes LabelHere fef (mc,e1#region))
			 fef mems
		   | JCmem_field _, None | JCmem_bitvector, _ ->
		       true, fef
		 end
	   end
       | JCEcast(e,st)
       | JCEinstanceof(e,st) -> 
	   true,
	   add_tag_reads LabelHere fef (struct_root st,e#region)
       | JCEalloc(_e1,st) ->
	   let pc = JCtag(st,[]) in
	   let ac = deref_alloc_class ~type_safe:true e in
	   let all_allocs = match ac with
	     | JCalloc_bitvector -> [ ac ]
	     | JCalloc_root rt ->
		 match rt.jc_root_info_kind with
		   | Rvariant -> all_allocs ~select:fully_allocated pc
		   | RdiscrUnion -> assert false (* TODO *)
		   | RplainUnion -> [ ac ]
	   in
	   let all_mems = match ac with
	     | JCalloc_bitvector -> []
	     | JCalloc_root rt -> 
		 match rt.jc_root_info_kind with
		   | Rvariant -> all_memories ~select:fully_allocated pc
		   | RdiscrUnion -> assert false (* TODO *)
		   | RplainUnion -> []
	   in
	   let all_tags = match ac with
	     | JCalloc_bitvector -> [ struct_root st ]
	     | JCalloc_root rt ->
		 match rt.jc_root_info_kind with
		   | Rvariant -> all_tags ~select:fully_allocated pc
		   | RdiscrUnion -> assert false (* TODO *)
		   | RplainUnion -> [ struct_root st ]
	   in
	   let fef = 
	     List.fold_left 
	       (fun fef mc -> 
		  add_memory_writes LabelHere fef (mc,e#region)
	       ) fef all_mems
	   in
	   let fef = 
	     List.fold_left
	       (fun fef ac -> add_alloc_writes LabelHere fef (ac,e#region))
	       fef all_allocs
	   in
	   true,
	   List.fold_left 
	     (fun fef vi -> add_tag_writes LabelHere fef (vi,e#region))
	     fef all_tags
       | JCEfree e ->
	   let pc = pointer_class e#typ in
	   let ac = alloc_class_of_pointer_class pc in
	   true,
	   add_alloc_writes LabelHere fef (ac,e#region)
       | JCEpack(st,e,_st) ->
	   (* Assert the invariants of the structure 
	      => need the reads of the invariants *)
	   let (_, invs) = 
	     Hashtbl.find Jc_typing.structs_table st.jc_struct_info_name 
	   in
	   let fef =
	     List.fold_left
	       (fun fef (li, _) -> 
		  { fef with jc_reads = 
		      ef_union fef.jc_reads li.jc_logic_info_effects })
	       fef
	       invs
	   in
	   (* Fields *)
	   let fef = List.fold_left
	     (fun fef fi ->
		match fi.jc_field_info_type with
		  | JCTpointer(pc, _, _) ->
	              (* Assert fields fully mutable 
			 => need mutable and tag_table (of field) as reads *)
		      let fef = add_mutable_reads fef pc in
		      let fef = 
			add_tag_reads LabelHere fef 
			  (pointer_class_root pc,e#region) 
		      in
	              (* Modify field's "committed" field 
			 => need committed (of field) as reads and writes *)
		      let fef = add_committed_reads fef pc in
		      let fef = add_committed_writes fef pc in
		      (* ...and field as reads *)
		      add_memory_reads LabelHere fef (JCmem_field fi,e#region)
		  | _ -> fef)
	     fef
	     st.jc_struct_info_fields in
	   (* Change structure mutable => need mutable as reads and writes *)
	   let fef = add_mutable_reads fef (JCtag(st, [])) in
	   let fef = add_mutable_writes fef (JCtag(st, [])) in
           (* And that's all *)
	   true, fef
       | JCEunpack(st,e,_st) ->
	   (* Change structure mutable => need mutable as reads and writes *)
	   let fef = add_mutable_reads fef (JCtag(st, [])) in
	   let fef = add_mutable_writes fef (JCtag(st, [])) in
	   (* Fields *)
	   let fef = List.fold_left
	     (fun fef fi ->
		match fi.jc_field_info_type with
		  | JCTpointer(st, _, _) ->
	              (* Modify field's "committed" field
			 => need committed (of field) as reads and writes *)
		      let fef = add_committed_reads fef st in
		      let fef = add_committed_writes fef st in
		      (* ...and field as reads *)
		      add_memory_reads LabelHere fef (JCmem_field fi,e#region)
		  | _ -> fef)
	     fef
	     st.jc_struct_info_fields in
	   (* And that's all *)
	   true, fef
       | JCEthrow(exc,_e_opt) -> 
	   true,
	   add_exception_effect fef exc
       | JCEtry(s,catches,finally) -> 
	   let fef = expr fef s in
	   let fef = 
	     List.fold_left 
	       (fun fef (exc,_vi_opt,_s) -> 
		  { fef with 
		      jc_raises = ExceptionSet.remove exc fef.jc_raises }
	       ) fef catches
	   in
	   let fef = 
	     List.fold_left 
	       (fun fef (_exc,_vi_opt,s) -> expr fef s) fef catches
	   in
	   false, (* do not recurse on try-block due to catches *)
	   expr fef finally
       | JCEmatch(_e, psl) ->
	   let pef = List.fold_left pattern empty_effects (List.map fst psl) in
	   true,
	   { fef with jc_reads = ef_union fef.jc_reads pef }
       | JCEloop _ | JCElet _ | JCEassert _ | JCEcontract _ | JCEblock _ 
       | JCEconst _  | JCEshift _ | JCEif _ | JCErange_cast _
       | JCEreal_cast _ | JCEunary _ | JCEaddress _ | JCEbinary _ 
       | JCEreturn_void  | JCEreturn _ | JCEbitwise_cast _ | JCEbase_block _ ->
	   true, fef
    ) fef e

let behavior fef (_pos,_id,b) =
  let fef = 
    fold_rec_behavior single_term single_assertion
      (single_location ~in_assigns:true) single_location_set fef b
  in
  Option_misc.fold_left add_exception_effect fef b.jc_behavior_throws

let spec fef s = 
  let fef = 
    List.fold_left behavior fef 
      (s.jc_fun_default_behavior :: s.jc_fun_behavior)
  in
  let fef = { fef with jc_reads = assertion fef.jc_reads s.jc_fun_requires } in
  { fef with jc_reads = assertion fef.jc_reads s.jc_fun_free_requires }

(* Type invariant added to precondition for pointer parameters with bounds.
   Therefore, add allocation table to reads. *)
let parameter fef v =
  match v.jc_var_info_type with
    | JCTpointer(pc,Some _i,Some _j) ->
	let ac = alloc_class_of_pointer_class pc in
	add_alloc_reads LabelOld fef (ac,v.jc_var_info_region)
    | _ -> fef


(******************************************************************************)
(*                                 fix-point                                  *)
(******************************************************************************)
   
let fixpoint_reached = ref false

let axiomatic_decl_effect ef d =
  match d with
    | Jc_typing.ABaxiom(_,_,_,a) -> assertion ef a

let effects_from_app fi ax_effects acc app =
  Jc_options.lprintf "@[Jc_effect.effects_from_app, fi = %s, app = %s@]@."
    fi.jc_logic_info_name
    app.jc_app_fun.jc_logic_info_name;
  Jc_options.lprintf "fi == app.jc_app_fun ? %b@." (fi == app.jc_app_fun);
  if fi == app.jc_app_fun then
    begin
      Jc_options.lprintf 
	"@[fi labels = @[{%a}@] ; app label_assoc = @[{%a}@]@]@."
	(print_list comma Jc_output_misc.label) 
	fi.jc_logic_info_labels
	(print_list comma 
	   (fun fmt (l1,l2) -> 
	      Format.fprintf fmt "%a -> %a" 
		Jc_output_misc.label l1 
		Jc_output_misc.label l2))
	app.jc_app_label_assoc;      
      ef_union
	(ef_filter_labels app.jc_app_label_assoc ax_effects)
	acc
    end
  else
    acc

let effects_from_term_app fi ax_effects acc t =
  match t#node with
    | JCTapp app -> effects_from_app fi ax_effects acc app
    | _ -> acc

let effects_from_pred_app fi ax_effects acc a =
  match a#node with
    | JCAapp app -> effects_from_app fi ax_effects acc app
    | _ -> acc

let effects_from_assertion fi ax_effects acc a =
  Jc_iterators.fold_term_and_assertion 
    (effects_from_term_app fi ax_effects) 
    (effects_from_pred_app fi ax_effects) acc a

let effects_from_decl fi ax_effects acc d =
  match d with
    | Jc_typing.ABaxiom(_,_,_,a) -> effects_from_assertion fi ax_effects acc a

let effects_from_axiomatic fi ax acc =
  try
    let l = Hashtbl.find Jc_typing.axiomatics_table ax in
    let ef = List.fold_left axiomatic_decl_effect empty_effects l.Jc_typing.axiomatics_decls in
    List.fold_left (effects_from_decl fi ef) acc l.Jc_typing.axiomatics_decls    
  with Not_found -> assert false

let logic_fun_effects f = 
  let f,ta = 
    Hashtbl.find Jc_typing.logic_functions_table f.jc_logic_info_tag 
  in
  let ef = f.jc_logic_info_effects in
  let ef = match ta with
    | JCTerm t -> term ef t 
    | JCAssertion a -> assertion ef a
    | JCInductive l ->
	let ax_effects =
	  List.fold_left 
	    (fun ef (id,labels,a) -> assertion ef a) empty_effects l
	in
	List.fold_left (fun acc (id,labels,a) -> 
			  effects_from_assertion f ax_effects acc a) ef l
    | JCReads [] ->
	begin match f.jc_logic_info_axiomatic with
	  | Some a ->
	      (* axiomatic def in a *)
	      effects_from_axiomatic f a ef 
	  | None -> assert false
	      (* not allowed outside axiomatics *)
	end
    | JCReads loclist ->
	assert (1==0); (* cause obsolete *)
	List.fold_left
	  (fun ef loc ->
	     let fef = location ~in_assigns:false empty_fun_effect loc in
	     ef_union ef fef.jc_reads 
	  ) ef loclist
  in
  if same_effects ef f.jc_logic_info_effects then () else
    (fixpoint_reached := false;
     f.jc_logic_info_effects <- ef)

let fun_effects f =
  let (f,_pos,s,e_opt) = 
    Hashtbl.find Jc_typing.functions_table f.jc_fun_info_tag 
  in
  let fef = f.jc_fun_info_effects in
  let fef = spec fef s in
  let fef = Option_misc.fold_left expr fef e_opt in
  let fef = List.fold_left parameter fef f.jc_fun_info_parameters in
  if same_feffects fef f.jc_fun_info_effects then () else
    (fixpoint_reached := false;
     f.jc_fun_info_effects <- fef)

let fun_effects f =
  set_current_function f;
  fun_effects f;
  reset_current_function ()

let logic_effects funs =

  List.iter (fun f -> f.jc_logic_info_effects <- empty_effects) funs;

  fixpoint_reached := false;
  while not !fixpoint_reached do
    fixpoint_reached := true;
    Jc_options.lprintf "Effects: doing one iteration...@.";
    List.iter logic_fun_effects funs
  done;
  Jc_options.lprintf "Effects: fixpoint reached@.";
  List.iter
    (fun f ->
       Jc_options.lprintf "Effects for logic function %s:@\n%a@."
	 f.jc_logic_info_name print_effect f.jc_logic_info_effects
    ) funs
    
let function_effects funs =

  List.iter (fun f -> f.jc_fun_info_effects <- empty_fun_effect) funs;

  let iterate () =
    fixpoint_reached := false;
    while not !fixpoint_reached do
      fixpoint_reached := true;
      Jc_options.lprintf "Effects: doing one iteration...@.";
      List.iter fun_effects funs
    done
  in

  (* Compute raw effects to bootstrap *)
  current_mode := MApprox;
  iterate ();
  (* Compute precise effects *)
  current_mode := MPrecise;
  iterate ();
  (* Reset mode to raw effects for individual calls *)
  current_mode := MApprox;

  Jc_options.lprintf "Effects: fixpoint reached@.";

  (* Global variables that are only read are translated into logic 
     functions in Why, and thus they should not appear in effects. *)
  Jc_options.lprintf "Effects: removing global reads w/o writes@.";
  List.iter
    (fun f ->
       let fef = f.jc_fun_info_effects in
       let efr = fef.jc_reads.jc_effect_globals in
       let efw = fef.jc_writes.jc_effect_globals in
       let ef = 
	 VarMap.filter 
	   (fun v _labs -> VarMap.mem v efw || v.jc_var_info_assigned) efr
       in
       let ef = { fef.jc_reads with jc_effect_globals = ef } in
       f.jc_fun_info_effects <- { fef with jc_reads = ef }
    ) funs;

  List.iter
    (fun f ->
       Jc_options.lprintf
	 "Effects for function %s:@\n\
@[ reads: %a@]@\n\
@[ writes: %a@]@\n\
@[ raises: %a@]@." 
	 f.jc_fun_info_name
	 print_effect f.jc_fun_info_effects.jc_reads
	 print_effect f.jc_fun_info_effects.jc_writes
	 (print_list comma print_exception)
	 (ExceptionSet.elements f.jc_fun_info_effects.jc_raises)
    ) funs


(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
