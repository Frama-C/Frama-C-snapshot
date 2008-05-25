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


(* $Id: jc_effect.ml,v 1.103 2008/04/10 16:05:55 moy Exp $ *)

open Jc_interp_misc
open Jc_name
open Jc_env
open Jc_envset
open Jc_fenv
open Jc_ast
open Jc_pervasives
open Jc_iterators
open Format
open Pp
open Jc_region
open Jc_struct_tools

(* Constant memories. Their region should be declared in Why. 
 * They should be passed to Why as global parameters. 
 *)
let constant_memories_set = ref FieldOrVariantRegionSet.empty

let alloc_region_table_set = ref StringRegionSet.empty

let mergeRegionMap m1 m2 =
  FieldOrVariantRegionMap.fold
    (fun v labs acc ->
       try
	 let l = FieldOrVariantRegionMap.find v m2 in
	 FieldOrVariantRegionMap.add v (LogicLabelSet.union labs l) acc
       with Not_found ->
	   FieldOrVariantRegionMap.add v labs acc)
    m1 m2

let ef_union ef1 ef2 =
  { jc_effect_alloc_table = 
      StringRegionSet.union
	ef1.jc_effect_alloc_table ef2.jc_effect_alloc_table;
    jc_effect_tag_table = 
      VariantSet.union
	ef1.jc_effect_tag_table ef2.jc_effect_tag_table;
    jc_effect_memories = 
(*     
      FieldRegionMap.union 
*)
      mergeRegionMap
	ef1.jc_effect_memories ef2.jc_effect_memories;
    jc_effect_globals = 
      VarSet.union 
	ef1.jc_effect_globals ef2.jc_effect_globals;
    jc_effect_mutable =
      StringSet.union
	ef1.jc_effect_mutable ef2.jc_effect_mutable;
    jc_effect_committed =
      StringSet.union
	ef1.jc_effect_committed ef2.jc_effect_committed;
  }

let ef_assoc ?label_assoc ef assoc =
  { ef with 
    jc_effect_memories =
      FieldOrVariantRegionMap.fold 
	(fun (fvi,r) labels acc ->
	   let labels =
	     match label_assoc with
	       | None -> labels
	       | Some a ->
(*
		   eprintf "label assoc:@.";
*)
		   LogicLabelSet.fold
		     (fun lab acc ->
			try
			  let l = List.assoc lab a in
(*
			  eprintf " %a -> %a@." Jc_output.label lab Jc_output.label l;
*)
			  LogicLabelSet.add l acc
			with Not_found -> LogicLabelSet.add lab acc (* assert false*))
		     labels LogicLabelSet.empty
	   in
	   if Region.polymorphic r then
	     try FieldOrVariantRegionMap.add (fvi,RegionList.assoc r assoc) labels acc 
	     with Not_found -> 
	       (* Local memory. Not counted as effect for the caller. *)
	       acc
	   else FieldOrVariantRegionMap.add (fvi,r) labels acc 
	) ef.jc_effect_memories FieldOrVariantRegionMap.empty;
    jc_effect_alloc_table =
      StringRegionSet.fold (fun (a,r) acc ->
	if Region.polymorphic r then
	  try StringRegionSet.add (a,RegionList.assoc r assoc) acc 
	  with Not_found -> 
	    (* Local alloc table. Not counted as effect for the caller. *)
	    acc
	else StringRegionSet.add (a,r) acc
      ) ef.jc_effect_alloc_table StringRegionSet.empty;
  }

let fef_reads ef =
  { jc_reads = ef;
    jc_writes = empty_effects;
    jc_raises = ExceptionSet.empty;
  }

let fef_union fef1 fef2 =
  { jc_reads = ef_union fef1.jc_reads fef2.jc_reads ;
    jc_writes = ef_union fef1.jc_writes fef2.jc_writes ;
    jc_raises = ExceptionSet.union fef1.jc_raises fef2.jc_raises;
  }

let fef_assoc fef assoc =
  { jc_reads = 
      if !Jc_options.separation_sem = SepRegions then 
	ef_assoc fef.jc_reads assoc 
      else fef.jc_reads;
    jc_writes = 
      if !Jc_options.separation_sem = SepRegions then
	ef_assoc fef.jc_writes assoc
      else fef.jc_writes;
    jc_raises = fef.jc_raises;
  }

let fieldRegionMap_add key lab m =
  let s = 
    try
      let s = FieldOrVariantRegionMap.find key m in
      LogicLabelSet.add lab s
    with Not_found -> LogicLabelSet.singleton lab
  in FieldOrVariantRegionMap.add key s m
    
let add_memory_effect label ef (fi,r) =
  (* If region is constant, add memory for [fi] to constant memories. *)
  if not(Region.polymorphic r) then
    constant_memories_set := FieldOrVariantRegionSet.add (fi,r) !constant_memories_set;
  { ef with jc_effect_memories = fieldRegionMap_add (fi,r) label ef.jc_effect_memories } 
  
let add_global_effect ef vi =
  { ef with jc_effect_globals = VarSet.add vi ef.jc_effect_globals } 

let add_alloc_effect ef (tov, r) =
  let a = tag_or_variant_type_name tov in
  if not(Region.polymorphic r) then
    alloc_region_table_set := StringRegionSet.add (a,r) !alloc_region_table_set;
  { ef with jc_effect_alloc_table = 
      StringRegionSet.add (a,r) ef.jc_effect_alloc_table } 
  
let add_tag_effect ef tov =
  let a = tag_or_variant_variant tov in
  { ef with jc_effect_tag_table = VariantSet.add a ef.jc_effect_tag_table } 

let add_mutable_effect ef tov =
  { ef with jc_effect_mutable = StringSet.add
      (tag_or_variant_type_name tov) ef.jc_effect_mutable }
  
let add_committed_effect ef tov =
  { ef with jc_effect_committed = StringSet.add
      (tag_or_variant_type_name tov) ef.jc_effect_committed }

let add_exception_effect ef a =
  { ef with jc_raises = ExceptionSet.add a ef.jc_raises }
  
let add_field_reads label fef (fi,r) =
  { fef with jc_reads = add_memory_effect label fef.jc_reads (fi,r) }

let add_field_alloc_reads label fef (fvi,r) =
  let ef = add_memory_effect label fef.jc_reads (fvi,r) in
  let root = match fvi with 
    | FVfield fi -> JCtag fi.jc_field_info_root
    | FVvariant vi -> JCvariant vi
  in
  let ef = add_alloc_effect ef (root, r) in
  { fef with jc_reads = ef }

let add_global_reads fef vi =
  { fef with jc_reads = add_global_effect fef.jc_reads vi }

let add_alloc_reads fef (a,r) =
  { fef with jc_reads = add_alloc_effect fef.jc_reads (a,r) }

let add_tag_reads fef a =
  { fef with jc_reads = add_tag_effect fef.jc_reads a }

let add_mutable_reads fef tov =
  { fef with jc_reads = add_mutable_effect fef.jc_reads tov }

let add_committed_reads fef tov =
  { fef with jc_reads = add_committed_effect fef.jc_reads tov }

let add_field_writes label fef (fi,r) =
  { fef with jc_writes = add_memory_effect label fef.jc_writes (fi,r) }

let add_field_alloc_writes label fef (fvi,r) =
  let efw = add_memory_effect label fef.jc_writes (fvi,r) in
  let root = match fvi with 
    | FVfield fi -> JCtag fi.jc_field_info_root
    | FVvariant vi -> JCvariant vi
  in
  let efr = add_alloc_effect fef.jc_reads (root, r) in
  { fef with jc_reads = efr; jc_writes = efw; }

let add_global_writes fef vi =
  { fef with jc_writes = add_global_effect fef.jc_writes vi }

let add_alloc_writes fef (a,r) =
  { fef with jc_writes = add_alloc_effect fef.jc_writes (a,r) }

let add_tag_writes fef a =
  { fef with jc_writes = add_tag_effect fef.jc_writes a }

let add_mutable_writes fef tov =
  { fef with jc_writes = add_mutable_effect fef.jc_writes tov }

let add_committed_writes fef tov =
  { fef with jc_writes = add_committed_effect fef.jc_writes tov }

let same_effects ef1 ef2 =
  StringRegionSet.equal ef1.jc_effect_alloc_table ef2.jc_effect_alloc_table
  && VariantSet.equal ef1.jc_effect_tag_table ef2.jc_effect_tag_table
  && FieldOrVariantRegionMap.equal (fun x y -> true) ef1.jc_effect_memories ef2.jc_effect_memories
  && VarSet.equal ef1.jc_effect_globals ef2.jc_effect_globals
  && StringSet.equal ef1.jc_effect_mutable ef2.jc_effect_mutable
  && StringSet.equal ef1.jc_effect_committed ef2.jc_effect_committed
    
let same_feffects fef1 fef2 =
  same_effects fef1.jc_reads fef2.jc_reads 
  && same_effects fef1.jc_writes fef2.jc_writes 
  && ExceptionSet.equal fef1.jc_raises fef2.jc_raises

(******************************************************************************)
(*                                  patterns                                  *)
(******************************************************************************)

(* TODO: check the use of "label" and "r" *)
let rec pattern ef (*label r*) p =
  let r = dummy_region in
  match p#node with
    | JCPstruct(st, fpl) ->
	let ef = add_tag_effect ef (JCtag st) in
	List.fold_left
	  (fun ef (fi, pat) ->
	     let ef = add_memory_effect (*label*)LabelHere ef (FVfield fi, r) in
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

(***********************

terms and assertions 

**************************)

let rec term ef t =
  match t#node with
    | JCTvar vi ->
	if vi.jc_var_info_static then
	  add_global_effect ef vi
	else ef
    | JCToffset(_,t,st) ->
	add_alloc_effect
	  (term ef t)
	  (JCtag st, t#region)
    | JCTapp app -> 
	let li = app.jc_app_fun and tls = app.jc_app_args in
	let efapp = 
	  ef_assoc li.jc_logic_info_effects ~label_assoc:app.jc_app_label_assoc app.jc_app_region_assoc 
	in
	ef_union efapp (List.fold_left term ef tls)
    | JCTderef (t, lab, fi) ->
	let fvi = 
	  if field_of_union fi then FVvariant (union_of_field fi) else FVfield fi
	in
	let ef = add_memory_effect lab ef (fvi,t#region) in
	term ef t
    | JCTrange (_, _) -> assert false (* TODO *)
    | JCTif (_, _, _) -> assert false (* TODO *)
    | JCTcast (t1, _, _) | JCTrange_cast(t1,_) | JCTreal_cast(t1,_) ->  term ef t1
    | JCTinstanceof (_, _, _) -> assert false (* TODO *)
    | JCTat (t1, lab) -> term ef t1
    | JCTold t1 -> term ef t1
    | JCTunary (_, t1) -> term ef t1
    | JCTshift (t1, t2) 
    | JCTbinary (t1, _, t2) ->
	term (term ef t1) t2
    | JCTconst _ -> ef
    | JCTmatch (t, ptl) ->
	let ef = List.fold_left pattern ef (List.map fst ptl) in
	term (List.fold_left term ef (List.map snd ptl)) t

let tag ef t h =
  let ef = match h with
    | None -> ef
    | Some h -> add_tag_effect ef h
  in
  match t#node with
    | JCTtag _
    | JCTbottom -> ef
    | JCTtypeof(t, _) -> term ef t

let rec assertion ef a =
  match a#node with
    | JCAtrue | JCAfalse -> ef
    | JCAif (t, a1, a2) -> 
	assertion (assertion (term ef t) a1) a2
    | JCAbool_term t -> term ef t
    | JCAinstanceof (t, lab, st) -> 
	add_tag_effect (term ef t) (JCtag st)
    | JCAnot a
    | JCAold a -> assertion ef a
    | JCAat(a,lab) -> assertion ef a
    | JCAquantifier (_, vi, a) -> assertion ef a 
    | JCArelation (t1, _, t2) -> term (term ef t1) t2
    | JCAapp app -> 
	let li = app.jc_app_fun and tls = app.jc_app_args in
	let efapp = 
	  ef_assoc li.jc_logic_info_effects ~label_assoc:app.jc_app_label_assoc app.jc_app_region_assoc 
	in
	ef_union efapp (List.fold_left term ef tls)
    | JCAiff (a1, a2)
    | JCAimplies (a1, a2) -> 
	assertion (assertion ef a1) a2
    | JCAand al | JCAor al -> 
	List.fold_left assertion ef al
    | JCAmutable (t, st, ta) ->
	term 
	  (add_mutable_effect 
	     (tag ef ta
		(Some (JCtag st)))
	     (JCtag st)) t
    | JCAtagequality (t1, t2, h) ->
	let h = match h with
	  | None -> None
	  | Some h -> Some (tov_of_name h)
	in
	tag (tag ef t1 h) t2 h
    | JCAmatch (t, pal) ->
	let ef = List.fold_left pattern ef (List.map fst pal) in
	term (List.fold_left assertion ef (List.map snd pal)) t

(********************

expressions and statements

***********************)

let rec expr ef e =
  match e#node with
    | JCEconst _ -> ef
    | JCEcast(e,st)
    | JCEinstanceof(e,st) -> 
	add_tag_reads (expr ef e) (JCtag st)
    | JCEderef (e, fi) -> 
	let fvi = 
	  if field_of_union fi then FVvariant (union_of_field fi) else FVfield fi
	in
	add_field_alloc_reads LabelHere (expr ef e) (fvi,e#region)
    | JCEshift (e1, e2) -> expr (expr ef e1) e2
    | JCEif(e1,e2,e3) -> expr (expr (expr ef e1) e2) e3
    | JCEvar vi -> 
	if vi.jc_var_info_static then
	  add_global_reads ef vi
	else ef
    | JCErange_cast(e1,_)
    | JCEreal_cast(e1,_)
    | JCEunary(_,e1) -> expr ef e1
    | JCEbinary(e1,op,e2) -> expr (expr ef e1) e2
    | JCEoffset(k,e,st) ->
	add_alloc_reads (expr ef e)
	  (JCtag st, e#region)
    | JCEalloc(_,st) ->
(*	let fields = embedded_struct_fields st in 
	let roots = embedded_struct_roots st in*)
	let fields = all_memories ~select:fully_allocated (JCtag st) in
	let roots = all_types ~select:fully_allocated (JCtag st) in
	let roots = List.map (fun x -> JCvariant x) roots in
	let ef = 
	  List.fold_left 
	    (fun ef fi -> add_field_writes LabelHere ef (FVfield fi,e#region))
	    ef fields
	in
	let ef = 
	  List.fold_left
	    (fun ef a -> add_alloc_writes ef (a,e#region))
	    ef ((JCtag st)::roots)
	in
	List.fold_left add_tag_writes ef ((JCtag st)::roots)
	
(*
	let mut = Jc_invariants.mutable_name st.jc_struct_info_root in
	add_global_writes ef mut
*)
    | JCEfree e ->
	begin match e#typ with
	  | JCTpointer(tov, _, _) ->
	      (* write tag table ? *)
	      add_alloc_writes (add_tag_writes ef tov) (tov,e#region)
	  | _ -> assert false
	end
(*    | JCEmatch(e, pel) ->
	expr (List.fold_left expr ef (List.map snd pel)) e*)
    | JCEapp app -> 
	let fi = app.jc_call_fun in
	let le = app.jc_call_args in
	let efcall = match fi with
	  | JClogic_fun f -> 
	      fef_reads 
		(ef_assoc f.jc_logic_info_effects 
		   ~label_assoc:app.jc_call_label_assoc app.jc_call_region_assoc)
	  | JCfun f -> 
	      fef_assoc f.jc_fun_info_effects app.jc_call_region_assoc 
	in
	fef_union efcall (List.fold_left expr ef le)
    | JCEassign_heap (e1, fi, e2) ->
	let fvi = 
	  if field_of_union fi then FVvariant (union_of_field fi) else FVfield fi
	in
	expr (expr (add_field_alloc_writes LabelHere ef (fvi,e1#region)) e1) e2
    | JCEassign_var (vi, e) ->
	if vi.jc_var_info_static then
	  add_global_writes (expr ef e) vi
	else 
	  expr ef e
    | JCEreturn_void -> ef
    | JCEreturn(_,e) -> expr ef e
    | JCEpack(st, e, _) ->
	let ef = expr ef e in
	(* Assert the invariants of the structure => need the reads of the invariants *)
	let (_, invs) = Hashtbl.find Jc_typing.structs_table st.jc_struct_info_name in
	let ef =
	  List.fold_left
	    (fun ef (li, _) -> { ef with jc_reads = ef_union ef.jc_reads li.jc_logic_info_effects })
	    ef
	    invs
	in
	(* Fields *)
	let ef = List.fold_left
	  (fun ef fi ->
	     match fi.jc_field_info_type with
	       | JCTpointer(tov, _, _) ->
	           (* Assert fields fully mutable => need mutable and tag_table (of field) as reads *)
		   let ef = add_mutable_reads ef tov in
		   let ef = add_tag_reads ef tov in
	           (* Modify field's "committed" field => need committed (of field) as reads and writes *)
		   let ef = add_committed_reads ef tov in
		   let ef = add_committed_writes ef tov in
		   (* ...and field as reads *)
		   add_field_reads LabelHere ef (FVfield fi,e#region)
	       | _ -> ef)
	  ef
	  st.jc_struct_info_fields in
	(* Change structure mutable => need mutable as reads and writes *)
	let ef = add_mutable_reads ef (JCtag st) in
	let ef = add_mutable_writes ef (JCtag st) in
        (* And that's all *)
	ef
    | JCEunpack(st, e, _) ->
	let ef = expr ef e in
	(* Change structure mutable => need mutable as reads and writes *)
	let ef = add_mutable_reads ef (JCtag st) in
	let ef = add_mutable_writes ef (JCtag st) in
	(* Fields *)
	let ef = List.fold_left
	  (fun ef fi ->
	     match fi.jc_field_info_type with
	       | JCTpointer(st, _, _) ->
	           (* Modify field's "committed" field => need committed (of field) as reads and writes *)
		   let ef = add_committed_reads ef st in
		   let ef = add_committed_writes ef st in
		   (* ...and field as reads *)
		   add_field_reads LabelHere ef (FVfield fi,e#region)
	       | _ -> ef)
	  ef
	  st.jc_struct_info_fields in
	(* And that's all *)
	ef
    | JCEthrow (ei, Some e) -> 
	add_exception_effect (expr ef e) ei
    | JCEthrow (ei, None) -> 
	add_exception_effect ef ei
    | JCEtry (s, catches, finally) -> 
	let ef = 
	  List.fold_left 
	    (fun ef (excep,_,s) -> 
	       let ef = 
		 { ef with 
		     jc_raises = ExceptionSet.remove excep ef.jc_raises }
	       in
	       expr ef s)
	    (expr ef s) catches
	in
	expr ef finally
    | JCEloop (la, s) -> 
	let ef = {ef with jc_reads = loop_annot ef.jc_reads la } in
	expr ef s
    | JCElet(vi,e,s) -> 
	expr (Option_misc.fold_left expr ef e) s
    | JCEassert((*_,*)a) -> 
	{ ef with jc_reads = assertion ef.jc_reads a; }
    | JCEblock l -> List.fold_left expr ef l
    | JCEmatch(e, psl) ->
	let pef = List.fold_left pattern empty_effects (List.map fst psl) in
	let ef = expr (List.fold_left expr ef (List.map snd psl)) e in
	{ ef with jc_reads = ef_union ef.jc_reads pef }

and loop_annot ef la = 
  let ef = assertion ef la.jc_loop_invariant in
  match la.jc_loop_variant with
  | None -> ef
  | Some t -> term ef t

(* Conservatively consider location is both read and written. *)
let rec location ef l =
  match l with
    | JCLderef(t,lab,fi,r) ->
	begin
	  let fvi = 
	    if field_of_union fi then FVvariant (union_of_field fi) else FVfield fi
	  in
	  let ef = add_field_writes lab ef (fvi,location_set_region t) in
	  add_field_reads lab ef (fvi,location_set_region t)
	end
    | JCLvar vi ->
	if vi.jc_var_info_static then
	  begin
	    let ef = add_global_writes ef vi in
	    add_global_reads ef vi
	  end
	else ef
    | JCLat(loc,_) ->
	location ef loc

let behavior ef (_,_, b) =
  (* assigns *)
  let ef = Option_misc.fold
    (fun (_,x) ef -> List.fold_left location ef x) 
    b.jc_behavior_assigns ef
  in
    (* requires: reads *)
    (*
      let ef = match b.jc_behavior_requires with
      None -> ef
      | Some r ->	{ ef with jc_reads = assertion ef.jc_reads r }
      in
    *)
    (* assumes: reads *)
  let ef = match b.jc_behavior_assumes with
      None -> ef
    | Some a ->	{ ef with jc_reads = assertion ef.jc_reads a }
  in
    (* ensures: reads *)
  let ef = 
    { ef with jc_reads = assertion ef.jc_reads b.jc_behavior_ensures } 
  in
    (* throws: raises *)
    Option_misc.fold_left add_exception_effect ef b.jc_behavior_throws

let spec ef s = 
  let ef = List.fold_left behavior ef s.jc_fun_behavior in
    { ef with jc_reads = assertion ef.jc_reads s.jc_fun_requires }

let parameter ef vi =
  match vi.jc_var_info_type with
    | JCTpointer (tov, _, _) ->
	add_alloc_reads (add_tag_reads ef tov) (tov,vi.jc_var_info_region)
    | _ -> ef
	
(* computing the fixpoint *)

let fixpoint_reached = ref false

let logic_fun_effects f = 
  let (f, ta) = 
    Hashtbl.find Jc_typing.logic_functions_table f.jc_logic_info_tag 
  in
  let ef = f.jc_logic_info_effects in
  let ef = match ta with
    | JCTerm t -> term ef t 
    | JCAssertion a -> assertion ef a
    | JCReads r ->
	List.fold_left
	  (fun ef l ->
	     let ef = {
	       jc_reads = ef;
	       jc_writes = ef; (* could be anything *)
	       jc_raises = ExceptionSet.empty;
	     }
	     in (location ef l).jc_reads)
	  ef r
  in
  if same_effects ef f.jc_logic_info_effects then ()
  else begin
    fixpoint_reached := false;
    f.jc_logic_info_effects <- ef
  end

let fun_effects fi =
  let (f, _, s, b) = 
    Hashtbl.find Jc_typing.functions_table fi.jc_fun_info_tag 
  in
  let ef = f.jc_fun_info_effects in
  let ef = spec ef s in
  let ef = Option_misc.fold_left ((*List.fold_left*) expr) ef b in
  let ef = List.fold_left parameter ef f.jc_fun_info_parameters in
    if same_feffects ef f.jc_fun_info_effects then ()
    else begin
    fixpoint_reached := false;
      f.jc_fun_info_effects <- ef
    end

let mapElements m =
  FieldOrVariantRegionMap.fold (fun key labels acc -> (key,labels)::acc) m []
      
let logic_effects funs =
  fixpoint_reached := false;
  while not !fixpoint_reached do
    fixpoint_reached := true;
    Jc_options.lprintf "Effects: doing one iteration...@.";
    List.iter logic_fun_effects funs
  done;
  Jc_options.lprintf "Effects: fixpoint reached@.";
  List.iter
    (fun f ->
       Jc_options.lprintf
	 "Effects for logic function %s:@\n@[ reads alloc_table: %a@]@\n@[ reads tag_table: %a@]@\n@[ reads memories: %a@]@." 
	 f.jc_logic_info_name
	 (print_list comma (fun fmt (a,r) ->
			      fprintf fmt "%s,%s" a r.jc_reg_name))
	 (StringRegionSet.elements f.jc_logic_info_effects.jc_effect_alloc_table)
	 (print_list comma (fun fmt v -> fprintf fmt "%s" v.jc_variant_info_name))
	 (VariantSet.elements f.jc_logic_info_effects.jc_effect_tag_table)
	 (print_list comma (fun fmt ((fvi,r),labels) ->
			      fprintf fmt "%s,%s (%a)" 
				(match fvi with 
				   | FVfield fi -> fi.jc_field_info_name
				   | FVvariant vi -> vi.jc_variant_info_name)
				r.jc_reg_name
				(print_list comma Jc_output_misc.label) (LogicLabelSet.elements labels)  
			   ))
	 (mapElements f.jc_logic_info_effects.jc_effect_memories))
    funs
    
let function_effects funs =
  fixpoint_reached := false;
  while not !fixpoint_reached do
    fixpoint_reached := true;
    Jc_options.lprintf "Effects: doing one iteration...@.";
    List.iter fun_effects funs
  done;
  Jc_options.lprintf "Effects: fixpoint reached@.";
  Jc_options.lprintf "Effects: removing global reads with writes@.";
  List.iter 
    (fun f ->
       f.jc_fun_info_effects <-
	 let efw = f.jc_fun_info_effects.jc_writes.jc_effect_globals in
	 let efr = f.jc_fun_info_effects.jc_reads.jc_effect_globals in
	 let efg = VarSet.diff efr (VarSet.inter efr efw) in
	 let efg = VarSet.filter (fun vi -> vi.jc_var_info_assigned) efg in
	 let ef = { f.jc_fun_info_effects.jc_reads
		    with jc_effect_globals = efg } in
	   { f.jc_fun_info_effects with jc_reads = ef }
    ) funs;
  List.iter
    (fun f ->
       Jc_options.lprintf
	 "Effects for function %s:@\n@[ reads: %a@]@\n@[ writes: %a@]@\n@[ raises: %a@]@." 
	 f.jc_fun_info_name
	 (print_list comma (fun fmt ((fvi,r),_) ->
			      fprintf fmt "%s,%s"
				(match fvi with 
				   | FVfield fi -> fi.jc_field_info_name
				   | FVvariant vi -> vi.jc_variant_info_name)
				r.jc_reg_name))
	 (mapElements f.jc_fun_info_effects.jc_reads.jc_effect_memories)
	 (print_list comma (fun fmt ((fvi,r),_) ->
			      fprintf fmt "%s,%s"
				(match fvi with 
				   | FVfield fi -> fi.jc_field_info_name
				   | FVvariant vi -> vi.jc_variant_info_name)
				r.jc_reg_name))
	 (mapElements f.jc_fun_info_effects.jc_writes.jc_effect_memories)
	 (print_list comma 
	    (fun fmt ei -> fprintf fmt "%s" ei.jc_exception_info_name))
	 (ExceptionSet.elements f.jc_fun_info_effects.jc_raises))
    funs


(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
