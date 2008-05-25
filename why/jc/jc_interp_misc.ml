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

open Output

(*
open Jc_env
open Jc_ast
open Jc_pervasives
open Jc_iterators
*)

open Jc_ast
open Jc_pervasives
open Jc_envset
open Jc_env
open Jc_fenv
open Jc_region
open Jc_name

(* Why type which modelises a variant. *)
let variant_model_type vi =
  simple_logic_type (variant_type_name vi)

(* Why type which modelises a structure "root". *)
let struct_model_type st = variant_model_type (struct_variant st)

let tag_or_variant_model_type = function
  | JCtag st -> struct_model_type st
  | JCvariant vi -> variant_model_type vi
  | JCunion vi -> variant_model_type vi

let struct_model_type2 name =
  let st, _ = Hashtbl.find Jc_typing.structs_table name in
  struct_model_type st

let pointer_type tov = 
  {
    logic_type_name = pointer_type_name;
    logic_type_args = [tag_or_variant_model_type tov];
  }

let tag_table_type tov = 
  {
    logic_type_name = tag_table_type_name;
    logic_type_args = [tag_or_variant_model_type tov];
  }

let tag_id_type tov = 
  {
    logic_type_name = tag_id_type_name;
    logic_type_args = [tag_or_variant_model_type tov];
  }

let alloc_table_type tov =
  {
    logic_type_name = alloc_table_type_name;
    logic_type_args = [tag_or_variant_model_type tov];
  }

let tr_native_type t =
  match t with
    | Tunit -> "unit"
    | Tboolean -> "bool"
    | Tinteger -> "int"
    | Treal -> "real"
    | Tstring -> "string"

let tr_base_type t =
  match t with
    | JCTnative t -> simple_logic_type (tr_native_type t)
    | JCTlogic s -> simple_logic_type s
    | JCTenum ri -> 
	simple_logic_type ri.jc_enum_info_name
    | JCTpointer (JCtag st, _, _) ->
	{ logic_type_name = pointer_type_name;
	  logic_type_args = [struct_model_type st] }
    | JCTpointer ((JCvariant vi | JCunion vi), _, _) ->
	{ logic_type_name = pointer_type_name;
	  logic_type_args = [variant_model_type vi] }
    | JCTnull | JCTany -> assert false

let why_integer_type = simple_logic_type "int"
  
let tr_type t = Base_type (tr_base_type t)

let memory_type t v =
  { logic_type_name = memory_type_name;
    logic_type_args = [t;v] }

let field_memory_type fi =
  memory_type 
    (struct_model_type fi.jc_field_info_root)
    (tr_base_type fi.jc_field_info_type)

let union_memory_type vi =
  memory_type 
    (variant_model_type vi)
    (if integral_union vi then why_integer_type 
     else simple_logic_type (union_memory_type_name vi))
	
let field_or_variant_memory_type fvi =
  match fvi with
    | FVfield fi -> field_memory_type fi
    | FVvariant vi -> union_memory_type vi

let logic_params ~label_in_name ?region_assoc ?label_assoc li =
  let l =
    FieldOrVariantRegionMap.fold
      (fun (fvi,r) labs acc ->
	 let r =
	   match region_assoc with 
	     | Some region_assoc when Region.polymorphic r ->
		 begin
		   Jc_options.lprintf "assoc:%a@." Region.print_assoc region_assoc;
		   Jc_options.lprintf "r:%a@." Region.print r;
		   try RegionList.assoc r region_assoc with Not_found -> assert false
		 end
	     | _ -> r
	 in
	 let name = field_or_variant_region_memory_name(fvi,r) in
	 LogicLabelSet.fold
	   (fun lab acc ->
	      let label =
		match label_assoc with
		  | None -> lab 
		  | Some a ->
		      try List.assoc lab a
		      with Not_found -> lab
	      in			
	      let name =
		if label_in_name then label_var label name
		else
		  match label with (* hack ?? *)
		    | LabelHere -> name
		    | LabelPost -> name
		    | LabelPre -> name ^ "@init"
		    | LabelOld -> name ^ "@"
		    | LabelName l -> name ^ "@" ^ l.label_info_final_name
	      in
	      (name, field_or_variant_memory_type fvi)::acc)
	   labs acc)
      li.jc_logic_info_effects.jc_effect_memories
      []
  in
  let l = 
    StringRegionSet.fold
      (fun (a,r) acc ->
	let r =
	  match region_assoc with
	    | Some assoc when Region.polymorphic r ->
		begin
		  Jc_options.lprintf "assoc:%a@." Region.print_assoc assoc;
		  Jc_options.lprintf "r:%a@." Region.print r;
		  try RegionList.assoc r assoc with Not_found -> assert false
		end
	    | _ -> r
	in
	let st, _ = Hashtbl.find Jc_typing.structs_table a in
	(alloc_region_table_name (JCtag st, r),
	 alloc_table_type (JCtag st))::acc)
      li.jc_logic_info_effects.jc_effect_alloc_table
      l	    
  in
  let l = 
    VariantSet.fold
      (fun v acc -> 
	 let t = { logic_type_args = [variant_model_type v];
		   logic_type_name = "tag_table" }
	 in
	 (tag_table_name_vi v, t)::acc)
      li.jc_logic_info_effects.jc_effect_tag_table
      l	    
  in
  VarSet.fold
    (fun v acc -> 
       (v.jc_var_info_final_name, tr_base_type v.jc_var_info_type) :: acc
    ) li.jc_logic_info_effects.jc_effect_globals
    l

let logic_params_call ~label_in_name li l region_assoc label_assoc =
  List.map 
    (fun (id,t) -> LVar id)
    (logic_params ~label_in_name ~region_assoc ~label_assoc li) @ l

let make_logic_fun_call ~label_in_name li l region_assoc label_assoc =
  let params = logic_params_call ~label_in_name li l region_assoc label_assoc in
  LApp(li.jc_logic_info_final_name,params)

let make_logic_pred_call ~label_in_name li l region_assoc label_assoc =
  let params = logic_params_call ~label_in_name li l region_assoc label_assoc in 
    LPred (li.jc_logic_info_final_name, params)



(* *)
let logic_info_reads acc li = 
  let acc =
    FieldOrVariantRegionMap.fold
      (fun (fvi,r) _ acc -> 
	 StringSet.add (field_or_variant_region_memory_name(fvi,r)) acc)
      li.jc_logic_info_effects.jc_effect_memories
      acc
  in
  let acc =
    StringRegionSet.fold
      (fun (a,r) acc ->
	 let st, _ = Hashtbl.find Jc_typing.structs_table a in
	 StringSet.add (alloc_region_table_name (JCtag st, r)) acc)
      li.jc_logic_info_effects.jc_effect_alloc_table
      acc
  in
  VariantSet.fold
    (fun v acc -> StringSet.add (tag_table_name_vi v) acc)
    li.jc_logic_info_effects.jc_effect_tag_table
    acc
(* *)


(*

(* same as in jc_interp.ml *)
let tag_name st = st.jc_struct_info_name ^ "_tag"

(* same as in jc_interp.ml *)
let logic_params li l =
  let l =
    FieldRegionSet.fold
      (fun (fi,r) acc -> (LVar(field_region_memory_name(fi,r)))::acc)
      li.jc_logic_info_effects.jc_effect_memories
      l	    
  in
  let l = 
    StringRegionSet.fold
      (fun (a,r) acc -> (LVar(alloc_region_table_name(a,r))::acc))
      li.jc_logic_info_effects.jc_effect_alloc_table
      l
  in
  StringSet.fold
    (fun v acc -> (LVar (v ^ "_tag_table"))::acc)
    li.jc_logic_info_effects.jc_effect_tag_table
    l	    

*)

let stringmap_elements map =
  StringMap.fold (fun _ i acc -> i::acc) map []

(* The following functions should be eliminated eventually, but before,
 * effect.ml must be redone.
 * They are here, and not in Jc_name, so that Krakatoa do not depends on
 * Jc_typing. *)

let find_struct a =
(*
  Format.printf "[find_struct] %s@." a;
*)
  fst (Hashtbl.find Jc_typing.structs_table a)

let find_variant a =
  Hashtbl.find Jc_typing.variants_table a

let find_tag_or_variant a =
  try
    JCtag (find_struct a)
  with Not_found ->
    JCvariant (find_variant a)

let tag_table_name2 a =
  tag_table_name (find_tag_or_variant a)

let alloc_table_name2 a =
  alloc_table_name (find_tag_or_variant a)

let alloc_region_table_name2 (a, r) =
  alloc_region_table_name (find_tag_or_variant a, r)

let mutable_name2 a =
  mutable_name (JCtag (find_struct a))

let committed_name2 a =
  committed_name (JCtag (find_struct a))

let alloc_table_type2 a =
  {
    logic_type_name = alloc_table_type_name;
    logic_type_args = [variant_model_type (find_variant a)];
  }

(* fold all effects into a list *)
let all_effects ef =
  let res =
    FieldOrVariantRegionMap.fold
      (fun (fvi,r) labels acc -> 
	let mem = field_or_variant_region_memory_name(fvi,r) in
	if Region.polymorphic r then
(*	  if RegionList.mem r f.jc_fun_info_param_regions then
	    if FieldRegionMap.mem (fi,r) 
	      f.jc_fun_info_effects.jc_writes.jc_effect_memories 
	    then mem::acc 
	    else acc
	  else acc*)
	  assert false (* TODO *)
	else mem::acc)
      ef.jc_effect_memories
      []
  in
  let res =
    VarSet.fold
      (fun v acc -> v.jc_var_info_final_name::acc)
      ef.jc_effect_globals
      res
  in
  let res =
    StringRegionSet.fold
      (fun (a,r) acc -> 
	let alloc = alloc_region_table_name2(a,r) in
	if Region.polymorphic r then
(*	  if RegionList.mem r f.jc_fun_info_param_regions then
	    if StringRegionSet.mem (a,r) 
	      f.jc_fun_info_effects.jc_writes.jc_effect_alloc_table 
	    then alloc::acc 
	    else acc
	  else acc*)
	  assert false (* TODO *)
	else alloc::acc)
      ef.jc_effect_alloc_table
      res
  in
  let res =
    VariantSet.fold
      (fun v acc -> (tag_table_name_vi v)::acc)
      ef.jc_effect_tag_table
      res
  in
  let res =
    StringSet.fold
      (fun v acc -> (mutable_name2 v)::acc)
      ef.jc_effect_mutable
      res
  in
  let res =
    StringSet.fold
      (fun v acc -> (committed_name2 v)::acc)
      ef.jc_effect_committed
      res
  in
  res

(* functions to make Why expressions *)

let make_if_term cond a b =
  LApp("ite", [ cond; a; b ])

let make_eq_term ty a b =
  let eq = match ty with
    | JCTpointer _ | JCTnull -> "eq_pointer_bool"
    | JCTenum _ | JCTlogic _ | JCTany -> assert false
    | JCTnative Tunit -> "eq_unit_bool"
    | JCTnative Tboolean -> "eq_bool_bool"
    | JCTnative Tinteger -> "eq_int_bool"
    | JCTnative Treal -> "eq_real_bool"
    | JCTnative Tstring -> "eq_string_bool"
  in
  LApp(eq, [a; b])

let make_and_term a b =
  make_if_term a b (LConst(Prim_bool false))

let make_or_term a b =
  make_if_term a (LConst(Prim_bool true)) b

let make_not_term a =
  make_if_term a (LConst(Prim_bool false)) (LConst(Prim_bool true))

let make_eq a b =
  LPred("eq", [ a; b ])

let make_select f this =
  LApp("select", [ f; this ])

let make_select_fi fi =
  make_select (LVar fi.jc_field_info_final_name)

let make_select_committed tov =
  make_select (LVar (committed_name tov))

let make_typeof_vi vi x =
  LApp("typeof", [ LVar (tag_table_name_vi vi); x ])

let make_typeof st x =
  make_typeof_vi (struct_variant st) x

let make_subtag t u =
  LPred("subtag", [ t; u ])

let make_subtag_bool t u =
  LApp("subtag_bool", [ t; u ])

let make_instanceof tt p st =
  LPred("instanceof", [ tt; p; LVar (tag_name st) ])

let make_offset_min tov p =
  LApp("offset_min", [LVar(alloc_table_name tov); p])

let make_offset_max tov p =
  LApp("offset_max", [LVar(alloc_table_name tov); p])

let make_int_of_tag st =
  LApp("int_of_tag", [LVar(tag_name st)])

let any_value ty = 
  match ty with
  | JCTnative t -> 
      begin match t with
	| Tunit -> Void
	| Tboolean -> App (Var "any_bool", Void)
	| Tinteger -> App (Var "any_int", Void)
	| Treal -> App (Var "any_real", Void)
	| Tstring -> App (Var "any_string", Void)
      end
  | JCTnull 
  | JCTpointer _ -> App (Var "any_pointer", Void)
  | JCTenum ri -> 
      App (Var ("any_" ^ ri.jc_enum_info_name), Void)
  | JCTlogic _ | JCTany -> assert false

let tov_of_name name = JCtag (find_struct name)

let fully_allocated fi =
  match fi.jc_field_info_type with
    | JCTpointer(_, Some _, Some _) -> true
    | JCTpointer(_, None, Some _)
    | JCTpointer(_, Some _, None)
    | JCTpointer(_, None, None)
    | JCTnull
    | JCTenum _
    | JCTlogic _
    | JCTnative _
    | JCTany -> false

(* see make_valid_pred in jc_interp.ml *)
let make_valid_pred_app tov p a b =
  let allocs = List.map
    (fun vi -> LVar(alloc_table_name (JCvariant vi)))
    (Jc_struct_tools.all_types ~select:fully_allocated tov)
  in
  let memories = List.map
    (fun fi -> LVar(field_memory_name fi))
    (Jc_struct_tools.all_memories ~select:fully_allocated tov)
  in
  LPred(valid_pred_name tov, p::a::b::allocs@memories)

let make_valid_one_pred_app tov p =
  let zero = LConst(Prim_int "0") in
  make_valid_pred_app tov p zero zero

let const_of_num n = LConst(Prim_int(Num.string_of_num n))

type forall_or_let =
  | JCforall of string * Output.logic_type
  | JClet of string * Output.term

let make_pred_binds binds body =
  List.fold_left
    (fun body bind ->
       match bind with
	 | JCforall(id, ty) -> LForall(id, ty, body)
	 | JClet(id, value) -> LLet(id, value, body))
    body (List.rev binds)

let const c =
  match c with
    | JCCvoid -> Prim_void
    | JCCnull -> assert false
    | JCCreal s -> Prim_real s
    | JCCinteger s -> Prim_int (Num.string_of_num (Numconst.integer s))
    | JCCboolean b -> Prim_bool b
    | JCCstring s -> assert false (* TODO *)

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
