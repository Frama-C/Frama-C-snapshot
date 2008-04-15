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

open Jc_stdlib
open Jc_env
open Jc_envset
open Jc_region
open Jc_ast
open Jc_fenv

open Jc_name
open Jc_constructors
open Jc_pervasives
open Jc_struct_tools
open Jc_effect

open Output
open Pp
open Format

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
  Hashtbl.find Jc_typing.roots_table a

let find_pointer_class a =
  try
    JCtag (find_struct a, []) (* TODO: fill parameters ? *)
  with Not_found ->
    JCroot (find_variant a)

let mutable_name2 a =
  mutable_name (JCtag (find_struct a, []))

let committed_name2 a =
  committed_name (JCtag (find_struct a, []))

let logic_enum_of_int ri = ri.jc_enum_info_name ^ "_of_integer"
let safe_fun_enum_of_int ri = "safe_" ^ ri.jc_enum_info_name ^ "_of_integer_"
(* Yannick: why have both logic_enum_of_int and safe_fun_enum_of_int? *)
let fun_enum_of_int ri = ri.jc_enum_info_name ^ "_of_integer_"
let logic_int_of_enum ri = "integer_of_" ^ ri.jc_enum_info_name
let mod_of_enum ri = "mod_" ^ ri.jc_enum_info_name ^ "_of_integer"
let fun_any_enum ri = "any_" ^ ri.jc_enum_info_name
let eq_of_enum ri = "eq_" ^ ri.jc_enum_info_name

let logic_bitvector_of_enum ri = "bitvector_of_" ^ ri.jc_enum_info_name
let logic_enum_of_bitvector ri = ri.jc_enum_info_name  ^ "_of_bitvector"

let native_name = function
  | Tunit -> "unit"
  | Tboolean -> "boolean"
  | Tinteger -> "integer"
  | Treal -> "real"
  | Tdouble -> "gen_float"
  | Tfloat -> "gen_float"
  | Tstring -> "string"

(* let logic_bitvector_of_native nty = "bitvector_of_" ^ (native_name nty) *)
(* let logic_native_of_bitvector nty = (native_name nty) ^ "_of_bitvector" *)

let logic_bitvector_of_variant vi = "bitvector_of_" ^ vi.jc_root_info_name
let logic_variant_of_bitvector vi = vi.jc_root_info_name ^ "_of_bitvector"

let logic_union_of_field fi = "bitvector_of_" ^ fi.jc_field_info_name
let logic_field_of_union fi = fi.jc_field_info_name ^ "_of_bitvector"



let any_value = function
  | JCTnative ty -> 
      begin match ty with
	| Tunit -> Void
	| Tboolean -> App(Var "any_bool", Void)
	| Tinteger -> App(Var "any_int", Void)
	| Treal -> App(Var "any_real", Void)
        | Tdouble -> App(Var "any_gen_float", Var "Double")
	| Tfloat -> App(Var "any_gen_float", Var "Single") 
	| Tstring -> App(Var "any_string", Void)
      end
  | JCTnull 
  | JCTpointer _ -> App(Var "any_pointer", Void)
  | JCTenum ri -> App (Var(fun_any_enum ri), Void)
  | JCTlogic _ | JCTany -> assert false
  | JCTtype_var _ -> assert false (* TODO: need environment *)

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
    | JCTnative Tdouble -> "eq_gen_float"
    | JCTnative Tfloat -> "eq_gen_float"
    | JCTnative Tstring -> "eq_string_bool"
    | JCTtype_var _ -> assert false (* TODO: need environment *)
  in
  LApp(eq, [a; b])

let make_eq_pred ty a b =
  let eq = match ty with
    | JCTpointer _ | JCTnull -> "eq"
    | JCTenum ri -> eq_of_enum ri
    | JCTlogic _ | JCTany -> assert false
    | JCTnative Tunit -> "eq_unit"
    | JCTnative Tboolean -> "eq_bool"
    | JCTnative Tinteger -> "eq_int"
    | JCTnative Treal -> "eq_real"
    | JCTnative Tdouble -> "eq_gen_float"
    | JCTnative Tfloat -> "eq_gen_float"
    | JCTnative Tstring -> "eq_string"
    | JCTtype_var _ -> assert false (* TODO: need environment *)
  in
  LPred(eq, [a; b])

let make_and_term a b =
  make_if_term a b (LConst(Prim_bool false))

let make_or_term a b =
  make_if_term a (LConst(Prim_bool true)) b

let make_not_term a =
  make_if_term a (LConst(Prim_bool false)) (LConst(Prim_bool true))

let make_eq a b =
  LPred("eq", [ a; b ])

let make_le a b =
  LPred("le", [ a; b ])

let make_ge a b =
  LPred("ge", [ a; b ])

let make_select f this =
  LApp("select", [ f; this ])

let make_select_fi fi =
  make_select (LVar (field_memory_name fi))

let make_select_committed pc =
  make_select (LVar (committed_name pc))

let make_typeof_vi (vi,r) x =
  LApp("typeof", [ LVar (tag_table_name (vi,r)); x ])

let make_typeof st r x =
  make_typeof_vi (struct_root st,r) x

let make_subtag t u =
  LPred("subtag", [ t; u ])

let make_subtag_bool t u =
  LApp("subtag_bool", [ t; u ])

let make_instanceof tt p st =
  LPred("instanceof", [ tt; p; LVar (tag_name st) ])

let make_offset_min ac p =
  LApp("offset_min", [LVar(generic_alloc_table_name ac); p])

let make_offset_max ac p =
  LApp("offset_max", [LVar(generic_alloc_table_name ac); p])

let make_int_of_tag st =
  LApp("int_of_tag", [LVar(tag_name st)])



let pc_of_name name = JCtag (find_struct name, []) (* TODO: parameters *)


let const c =
  match c with
    | JCCvoid -> Prim_void
    | JCCnull -> assert false
    | JCCreal s -> Prim_real s
    | JCCinteger s -> Prim_int (Num.string_of_num (Numconst.integer s))
    | JCCboolean b -> Prim_bool b
    | JCCstring s -> assert false (* TODO *)

type forall_or_let =
  | JCforall of string * Output.logic_type
  | JClet of string * Output.term


(******************************************************************************)
(*                              environment                                   *)
(******************************************************************************)

let current_behavior : string option ref = ref None
let set_current_behavior behav = current_behavior := Some behav
let reset_current_behavior () = current_behavior := None
let get_current_behavior () = 
  match !current_behavior with None -> assert false | Some behav -> behav
let safety_checking () = get_current_behavior () = "safety"
let default_checking () = get_current_behavior () = "default"
let assert_in_current_behavior = function
  | [] -> default_checking ()
  | ls -> List.exists (fun behav -> behav = get_current_behavior ()) ls
let assume_in_current_behavior = function
  | [] -> not (default_checking ())
  | ls -> List.exists (fun behav -> behav = get_current_behavior ()) ls

let current_spec : fun_spec option ref = ref None
let set_current_spec s = current_spec := Some s
let reset_current_spec () = current_spec := None
let get_current_spec () = 
  match !current_spec with None -> assert false | Some s -> s


(******************************************************************************)
(*                                   types                                    *)
(******************************************************************************)

(* basic model types *)

let why_integer_type = simple_logic_type "int"
let why_unit_type = simple_logic_type "unit"

let root_model_type vi =
  simple_logic_type (root_type_name vi)

let struct_model_type st = 
  root_model_type (struct_root st)

let pointer_class_model_type pc = 
  root_model_type (pointer_class_root pc)

let bitvector_type = simple_logic_type bitvector_type_name

let alloc_class_type = function
  | JCalloc_root vi -> root_model_type vi
  | JCalloc_bitvector -> why_unit_type

let memory_class_type mc = 
  alloc_class_type (alloc_class_of_mem_class mc)

(* raw types *)

let raw_pointer_type ty' =
  { logic_type_name = pointer_type_name;
    logic_type_args = [ty']; }

let raw_pset_type ty' =
  { logic_type_name = pset_type_name;
    logic_type_args = [ty']; }

let raw_alloc_table_type ty' =
  { logic_type_name = alloc_table_type_name;
    logic_type_args = [ty']; }

let raw_tag_table_type ty' = 
  { logic_type_name = tag_table_type_name;
    logic_type_args = [ty']; }

let raw_tag_id_type ty' = 
  { logic_type_name = tag_id_type_name;
    logic_type_args = [ty']; }

let raw_memory_type ty1' ty2' =
  { logic_type_name = memory_type_name;
    logic_type_args = [ty1';ty2'] }

(* pointer model types *)

let pointer_type ac pc = 
  match ac with
    | JCalloc_root _ ->
	raw_pointer_type (pointer_class_model_type pc)
    | JCalloc_bitvector ->
	raw_pointer_type why_unit_type

(* translation *)

let tr_native_type = function
  | Tunit -> "unit"
  | Tboolean -> "bool"
  | Tinteger -> "int"
  | Treal -> "real"
  | Tdouble -> "gen_float"
  | Tfloat -> "gen_float"
  | Tstring -> "string"

let tr_base_type ?region = function
  | JCTnative ty -> 
      simple_logic_type (tr_native_type ty)
  | JCTlogic s -> 
      simple_logic_type s
  | JCTenum ri -> 
      simple_logic_type ri.jc_enum_info_name
  | JCTpointer(pc, _, _) ->
      let ac = match region with 
	| None -> 
	    alloc_class_of_pointer_class pc
	| Some r ->
	    if Region.bitwise r then JCalloc_bitvector
	    else alloc_class_of_pointer_class pc
      in
      pointer_type ac pc
  | JCTnull | JCTany -> assert false
  | JCTtype_var _ -> assert false (* TODO (need environment) *)

let tr_type ~region ty = Base_type (tr_base_type ~region ty)

let tr_var_base_type v = 
  tr_base_type ~region:v.jc_var_info_region v.jc_var_info_type

let tr_var_type v = 
  tr_type ~region:v.jc_var_info_region v.jc_var_info_type

(* model types *)

let pset_type ac = raw_pset_type (alloc_class_type ac)

let alloc_table_type ac = raw_alloc_table_type (alloc_class_type ac)

let tag_table_type vi = raw_tag_table_type (root_model_type vi)

let tag_id_type vi = raw_tag_id_type (root_model_type vi)

let memory_type mc = 
  let value_type = match mc with
    | JCmem_field fi -> tr_base_type fi.jc_field_info_type
    | JCmem_plain_union _ 
    | JCmem_bitvector -> bitvector_type
  in
  raw_memory_type (memory_class_type mc) value_type

(* query model types *)

let is_alloc_table_type ty' = ty'.logic_type_name = alloc_table_type_name

let is_tag_table_type ty' = ty'.logic_type_name = tag_table_type_name

let is_memory_type ty' = ty'.logic_type_name = memory_type_name

let deconstruct_memory_type_args ty =
  match ty.logic_type_args with [t;v] -> t,v | _ -> assert false

let any_value' ty' =
  let anyfun = 
    if is_alloc_table_type ty' then "any_alloc_table"
    else if is_tag_table_type ty' then "any_tag_table"
    else if is_memory_type ty' then "any_memory"
    else assert false
  in
  App(Var anyfun,Void)


(******************************************************************************)
(*                                 variables                                  *)
(******************************************************************************)

let transpose_label ~label_assoc lab =
  match label_assoc with
    | None -> lab
    | Some l ->  try List.assoc lab l with Not_found -> lab

let lvar_name ~constant ~label_in_name ?label_assoc lab n =
  let lab = transpose_label ~label_assoc lab in
  if label_in_name && not constant then 
    match lab with
      | LabelHere -> n
      | LabelOld -> assert false
      | LabelPre -> n ^ "_at_Pre"
      | LabelPost -> n ^ "_at_Post"
      | LabelName lab -> n ^ "_at_" ^ lab.label_info_final_name
  else n

let lvar ~constant ~label_in_name lab n =
  let n = lvar_name ~constant ~label_in_name lab n in
  if label_in_name then 
    LVar n
  else match lab with 
    | LabelHere -> LVar n
    | LabelOld -> LVarAtLabel(n,"")
    | LabelPre -> LVarAtLabel(n,"init")
    | LabelPost -> LVar n
    | LabelName lab -> LVarAtLabel(n,lab.label_info_final_name)

(* simple variables *)

let plain_var n = Var n
let deref_var n = Deref n

let var_name' = function
  | Var n | Deref n -> n
  | _ -> assert false

let var v =
  if v.jc_var_info_assigned 
  then deref_var v.jc_var_info_final_name
  else plain_var v.jc_var_info_final_name

let param ~type_safe v = 
  v.jc_var_info_final_name, 
  if type_safe then
    tr_base_type v.jc_var_info_type 
  else
    tr_base_type ~region:v.jc_var_info_region v.jc_var_info_type 

let tvar_name ~label_in_name lab v = 
  lvar_name ~constant:(not v.jc_var_info_assigned) ~label_in_name
    lab v.jc_var_info_final_name

let tvar ~label_in_name lab v = 
  lvar ~constant:(not v.jc_var_info_assigned) ~label_in_name
    lab v.jc_var_info_final_name

let tparam ~label_in_name lab v = 
  tvar_name ~label_in_name lab v,
  tvar ~label_in_name lab v,
  tr_base_type v.jc_var_info_type 

let local_of_parameter (v',ty') = (var_name' v',ty')
let effect_of_parameter (v',_ty') = var_name' v'
let wparam_of_parameter (v',ty') = (v',Ref_type(Base_type ty'))
let rparam_of_parameter (v',ty') = (v',Base_type ty')

(* model variables *)

let mutable_memory infunction (mc,r) =
  if Region.polymorphic r then
    if Region.bitwise r then
      MemoryMap.exists (fun (_mc',r') _labs -> Region.equal r r')
	infunction.jc_fun_info_effects.jc_writes.jc_effect_memories
    else
      MemoryMap.mem (mc,r)
	infunction.jc_fun_info_effects.jc_writes.jc_effect_memories
  else true

let mutable_alloc_table infunction (ac,r) =
  if Region.polymorphic r then
    if Region.bitwise r then
      AllocMap.exists (fun (_ac',r') _labs -> Region.equal r r')
	infunction.jc_fun_info_effects.jc_writes.jc_effect_alloc_tables
    else
      AllocMap.mem (ac,r)
	infunction.jc_fun_info_effects.jc_writes.jc_effect_alloc_tables
  else true

let mutable_tag_table infunction (vi,r) =
  if Region.polymorphic r then
    if Region.bitwise r then
      TagMap.exists (fun (_vi',r') _labs -> Region.equal r r')
	infunction.jc_fun_info_effects.jc_writes.jc_effect_tag_tables
    else
      TagMap.mem (vi,r)
	infunction.jc_fun_info_effects.jc_writes.jc_effect_tag_tables
  else true

let plain_memory_var (mc,r) = Var (memory_name (mc,r))
let deref_memory_var (mc,r) = Deref (memory_name (mc,r))

let memory_var ?(test_current_function=false) (mc,r) =
  if test_current_function && !current_function = None then
    plain_memory_var (mc,r)
  else if mutable_memory (get_current_function ()) (mc,r) then
    deref_memory_var (mc,r)
  else plain_memory_var (mc,r)

let tmemory_var ~label_in_name lab (mc,r) =
  let mem = memory_name (mc,r) in
  let constant = match !current_function with
    | None -> false (* Variables at different labels should be different *)
    | Some infunction -> not (mutable_memory infunction (mc,r))
  in
  lvar ~constant ~label_in_name lab mem

let tmemory_param ~label_in_name lab (mc,r) =
  let mem = memory_name (mc,r) in
  let constant = match !current_function with
    | None -> false (* Variables at different labels should be different *)
    | Some infunction -> not (mutable_memory infunction (mc,r))
  in
  let n = lvar_name ~constant ~label_in_name lab mem in
  let v = lvar ~constant ~label_in_name lab mem in
  let ty' = memory_type mc in
  n, v, ty'

let plain_alloc_table_var (ac,r) = Var (alloc_table_name (ac,r))
let deref_alloc_table_var (ac,r) = Deref (alloc_table_name (ac,r))

let alloc_table_var ?(test_current_function=false) (ac,r) =
  if test_current_function && !current_function = None then
    plain_alloc_table_var (ac,r)
  else if mutable_alloc_table (get_current_function ()) (ac,r) then
    deref_alloc_table_var (ac,r)
  else plain_alloc_table_var (ac,r)

let talloc_table_var ~label_in_name lab (ac,r) =
  let alloc = alloc_table_name (ac,r) in
  let constant = match !current_function with
    | None -> false (* Variables at different labels should be different *)
    | Some infunction -> not (mutable_alloc_table infunction (ac,r))
  in
  lvar ~constant ~label_in_name lab alloc

let talloc_table_param ~label_in_name lab (ac,r) =
  let alloc = alloc_table_name (ac,r) in
  let constant = match !current_function with
    | None -> false (* Variables at different labels should be different *)
    | Some infunction -> not (mutable_alloc_table infunction (ac,r))
  in
  let n = lvar_name ~constant ~label_in_name lab alloc in
  let v = lvar ~constant ~label_in_name lab alloc in
  let ty' = alloc_table_type ac in
  n, v, ty'

let plain_tag_table_var (vi,r) = Var (tag_table_name (vi,r))
let deref_tag_table_var (vi,r) = Deref (tag_table_name (vi,r))

let tag_table_var (vi,r) =
  if mutable_tag_table (get_current_function ()) (vi,r) then
    deref_tag_table_var (vi,r)
  else plain_tag_table_var (vi,r)

let ttag_table_var ~label_in_name lab (vi,r) =
  let tag = tag_table_name (vi,r) in
  let constant = match !current_function with
    | None -> false (* Variables at different labels should be different *)
    | Some infunction -> not (mutable_tag_table infunction (vi,r))
  in
  lvar ~constant ~label_in_name lab tag

let ttag_table_param ~label_in_name lab (vi,r) =
  let tag = tag_table_name (vi,r) in
  let constant = match !current_function with
    | None -> false (* Variables at different labels should be different *)
    | Some infunction -> not (mutable_tag_table infunction (vi,r))
  in
  let n = lvar_name ~constant ~label_in_name lab tag in
  let v = lvar ~constant ~label_in_name lab tag in
  let ty' = tag_table_type vi in
  n, v, ty'


(******************************************************************************)
(*                           locations and separation                         *)
(******************************************************************************)

let ref_term : (type_safe:bool -> global_assertion:bool -> relocate:bool 
		 -> label -> label -> Jc_fenv.term -> Output.term) ref 
    = ref (fun ~type_safe ~global_assertion ~relocate _ _ _ -> assert false)

let rec location ~type_safe ~global_assertion lab loc = 
  let flocs = location_set ~type_safe ~global_assertion lab in
  let ft = !ref_term ~type_safe ~global_assertion ~relocate:false lab lab in
  match loc#node with
    | JCLvar _v ->
	LVar "pset_empty"
    | JCLderef(locs,_lab,_fi,_r) ->
	flocs locs
    | JCLderef_term(t1,_fi) ->
	LApp("pset_singleton",[ ft t1 ])
    | _ -> assert false (* TODO *)

and location_set ~type_safe ~global_assertion lab locs = 
  let flocs = location_set ~type_safe ~global_assertion lab in
  let ft = !ref_term ~type_safe ~global_assertion ~relocate:false lab lab in
  match locs#node with
    | JCLSvar v ->
	LApp("pset_singleton",[ tvar ~label_in_name:global_assertion lab v ])
    | JCLSderef(locs,lab,fi,r) ->
	let mc,_fi_opt = lderef_mem_class ~type_safe locs fi in
        let mem = 
	  tmemory_var ~label_in_name:global_assertion lab (mc,locs#region) 
	in
	LApp("pset_deref",[ mem; flocs locs ])
    | JCLSrange(locs,Some t1,Some t2) ->
	LApp("pset_range",[ flocs locs; ft t1; ft t2 ])
    | JCLSrange(locs,None,Some t2) ->
	LApp("pset_range_left",[ flocs locs; ft t2 ])
    | JCLSrange(locs,Some t1,None) ->
	LApp("pset_range_right",[ flocs locs; ft t1 ])
    | JCLSrange(locs,None,None) ->
	LApp("pset_all",[ flocs locs ])
    | JCLSrange_term(locs,Some t1,Some t2) ->
	LApp("pset_range",[ LApp("pset_singleton", [ ft locs ]); ft t1; ft t2 ])
    | JCLSrange_term(locs,None,Some t2) ->
	LApp("pset_range_left",[ LApp("pset_singleton", [ ft locs ]); ft t2 ])
    | JCLSrange_term(locs,Some t1,None) ->
	LApp("pset_range_right",[ LApp("pset_singleton", [ ft locs ]); ft t1 ])
    | JCLSrange_term(locs,None,None) ->
	LApp("pset_all",[ LApp("pset_singleton", [ ft locs ]) ])

let rec location_list' = function
  | [] -> LVar "pset_empty"
  | [e'] -> e'
  | e' :: el' -> LApp("pset_union",[ e'; location_list' el' ])

let separation_condition loclist loclist' =
  let floc = location ~type_safe:false ~global_assertion:false LabelHere in
  let pset = location_list' (List.map floc loclist) in
  let pset' = location_list' (List.map floc loclist') in
  LPred("pset_disjoint",[ pset; pset' ])

type memory_effect = RawMemory of Memory.t | PreciseMemory of Location.t

let rec transpose_location ~region_assoc ~param_assoc (loc,(mc,rdist)) =
  match transpose_region ~region_assoc rdist with
    | None -> None
    | Some rloc ->
	try
	  let node = match loc#node with
	    | JCLvar v ->
		if v.jc_var_info_static then
		  JCLvar v
		else
		  begin match List.mem_assoc_eq VarOrd.equal v param_assoc with
		    | None -> (* Local variable *)
			failwith "Cannot transpose location"
		    | Some e -> 
			match location_of_expr e with
			  | None -> failwith "Cannot transpose location"
			  | Some loc' -> loc'#node
		  end
	    | JCLderef(locs,lab,fi,r) ->
		let locs = 
		  transpose_location_set ~region_assoc ~param_assoc locs
		in
		JCLderef(locs,lab,fi,r) (* TODO: remove useless lab & r *)
	    | _ -> assert false (* TODO *)
	  in
	  let loc = new location_with ~region:rloc ~node loc in
	  Some(PreciseMemory(loc,(mc,rloc)))
	with Failure "Cannot transpose location" ->
	  Some(RawMemory(mc,rloc))

and transpose_location_set ~region_assoc ~param_assoc locs =
  match transpose_region ~region_assoc locs#region with
    | None -> failwith "Cannot transpose location"
    | Some rloc ->
	let node = match locs#node with
	  | JCLSvar v ->
	      if v.jc_var_info_static then
		JCLSvar v
	      else
		begin match List.mem_assoc_eq VarOrd.equal v param_assoc with
		  | None -> (* Local variable *)
		      failwith "Cannot transpose location"
		  | Some e -> 
		      match location_set_of_expr e with
			| None -> failwith "Cannot transpose location"
			| Some locs' -> locs'#node
	      end
	  | JCLSderef(locs',lab,fi,r) ->
	      let locs' = 
		transpose_location_set ~region_assoc ~param_assoc locs'
	      in
	      JCLSderef(locs',lab,fi,r) (* TODO: remove useless lab & r *)
	  | _ -> assert false (* TODO *)
	in
	new location_set_with ~region:rloc ~node locs

let transpose_location_set ~region_assoc ~param_assoc locs w=
  try Some(transpose_location_set ~region_assoc ~param_assoc locs)
  with Failure "Cannot transpose location" -> None

let transpose_location_list
    ~region_assoc ~param_assoc rw_raw_mems rw_precise_mems (mc,distr) =
  let loclist =
    if MemorySet.mem (mc,distr) rw_raw_mems then
      (Format.eprintf "memory %a@\nin memory set %a@." 
	 print_memory (mc,distr) 
	 (print_list comma print_memory) (MemorySet.elements rw_raw_mems);
       assert false) (* TODO: parameters *)
    else
      LocationSet.to_list
	(LocationSet.filter
	   (fun (_loc,(_mc,r)) -> Region.equal r distr)
	   rw_precise_mems)
  in
  List.fold_left
    (fun acc (loc,(mc,rdist)) ->
       match transpose_location ~region_assoc ~param_assoc (loc,(mc,rdist)) with
	 | None -> acc
	 | Some(RawMemory(mc,rloc)) -> assert false
	 | Some(PreciseMemory(loc,(mc,rloc))) ->
	     loc :: acc
    ) [] loclist

let write_read_separation_condition 
    ~callee_reads ~callee_writes ~region_assoc ~param_assoc 
    inter_names writes reads =
  List.fold_left
    (fun acc ((mc,distr),(v,_ty')) ->
       let n = var_name' v in
       if StringSet.mem n inter_names then
	 (* There is a read/write interference on this memory *)
	 List.fold_left 
	   (fun acc ((mc',distr'),(v',_ty')) ->
	      let n' = var_name' v' in
	      if n = n' then
		let rw_raw_mems =
		  MemorySet.of_list
		    (MemoryMap.keys callee_reads.jc_effect_raw_memories
		     @ MemoryMap.keys callee_writes.jc_effect_raw_memories)
		in
		let rw_precise_mems =
		  LocationSet.of_list
		    (LocationMap.keys callee_reads.jc_effect_precise_memories
		     @ 
		     LocationMap.keys callee_writes.jc_effect_precise_memories)
		in
		let loclist =
		  transpose_location_list ~region_assoc ~param_assoc
		    rw_raw_mems rw_precise_mems (mc,distr)
		in
		let loclist' =
		  transpose_location_list ~region_assoc ~param_assoc
		    rw_raw_mems rw_precise_mems (mc',distr')
		in
		assert (loclist <> []);
		assert (loclist' <> []);
		let pre = separation_condition loclist loclist' in
		make_and pre acc
	      else acc
	   ) acc writes
       else acc
    ) LTrue reads

let write_write_separation_condition 
    ~callee_reads ~callee_writes ~region_assoc ~param_assoc
    ww_inter_names writes reads =
  let writes = 
    List.filter 
      (fun ((mc,distr),(v,_ty)) -> 
	 let n = var_name' v in
	 StringSet.mem n ww_inter_names
      ) writes 
  in
  let write_pairs = List.all_pairs writes in
  List.fold_left
    (fun acc (((mc,distr),(v,_ty)), ((mc',distr'),(v',_ty'))) ->
       let n = var_name' v in
       let n' = var_name' v' in
       if n = n' then
	 (* There is a write/write interference on this memory *)
	 let rw_raw_mems =
	   MemorySet.of_list
	     (MemoryMap.keys callee_reads.jc_effect_raw_memories
	      @ MemoryMap.keys callee_writes.jc_effect_raw_memories)
	 in
	 let rw_precise_mems =
	   LocationSet.of_list
	     (LocationMap.keys callee_reads.jc_effect_precise_memories
	      @ 
	      LocationMap.keys callee_writes.jc_effect_precise_memories)
	 in
	 let loclist =
	   transpose_location_list ~region_assoc ~param_assoc
	     rw_raw_mems rw_precise_mems (mc,distr)
	 in
	 let loclist' =
	   transpose_location_list ~region_assoc ~param_assoc
	     rw_raw_mems rw_precise_mems (mc',distr')
	 in
	 assert (loclist <> []);
	 assert (loclist' <> []);
	 let pre = separation_condition loclist loclist' in
	 make_and pre acc
       else acc
    ) LTrue write_pairs


(******************************************************************************)
(*                                  effects                                   *)
(******************************************************************************)

let rec all_possible_memory_effects acc r ty =
  match ty with
    | JCTpointer(pc,_,_) ->
	begin match pc with
	  | JCroot _ -> acc (* TODO *)
	  | JCtag(st,_) ->
	      List.fold_left 
		(fun acc fi ->
		   let mc = JCmem_field fi in
		   let mem = mc,r in
		   if MemorySet.mem mem acc then 
		     acc
		   else
		     all_possible_memory_effects 
		       (MemorySet.add mem acc) r fi.jc_field_info_type
		) acc st.jc_struct_info_fields
	end
    | JCTnative _
    | JCTnull 
    | JCTenum _
    | JCTlogic _
    | JCTany -> acc
    | JCTtype_var _ -> assert false (* TODO: need environment *)


let rewrite_effects ~type_safe ~params ef =
  let all_mems = 
    List.fold_left 
      (fun acc v ->
	 all_possible_memory_effects acc v.jc_var_info_region v.jc_var_info_type
      ) MemorySet.empty params
  in
  if not type_safe then ef else
    { ef with
	jc_effect_memories = 
	MemoryMap.fold 
	  (fun (mc,r) labs acc ->
	     match mc with
	       | JCmem_field _ | JCmem_plain_union _ -> 
		   MemoryMap.add (mc,r) labs acc
	       | JCmem_bitvector ->
		   MemorySet.fold 
		     (fun (mc',r') acc ->
			if Region.equal r r' then 
			  MemoryMap.add (mc',r') labs acc
			else acc
		     ) all_mems acc
	  ) ef.jc_effect_memories MemoryMap.empty
    }


(******************************************************************************)
(*                                 Structures                                 *)
(******************************************************************************)

let const_of_num n = LConst(Prim_int(Num.string_of_num n))

let define_locals ?(reads=[]) ?(writes=[]) e' =
  let e' =
    List.fold_left 
      (fun acc (n,ty') -> Let(n,any_value' ty',acc)) 
      e' reads
  in
  let e' =
    List.fold_left 
      (fun acc (n,ty') -> Let_ref(n,any_value' ty',acc)) 
      e' writes
  in
  e'

(* Validity *)

let make_valid_pred_app ~equal (ac, r) pc p ao bo =
  let all_allocs = match ac with
    | JCalloc_bitvector -> [ ac ]
    | JCalloc_root rt -> 
	match rt.jc_root_info_kind with
	  | Rvariant
	  | RdiscrUnion -> all_allocs ~select:fully_allocated pc
	  | RplainUnion -> [ ac ]
  in
  let allocs = 
    List.map (fun ac -> LVar(alloc_table_name (ac,r))) all_allocs
  in
  let all_mems = match ac with
    | JCalloc_bitvector -> []
    | JCalloc_root rt -> 
	match rt.jc_root_info_kind with
	  | Rvariant
	  | RdiscrUnion -> all_memories ~select:fully_allocated pc
	  | RplainUnion -> []
  in
  let mems = List.map (fun mc -> LVar(memory_name (mc,r))) all_mems in
  let params = allocs @ mems in
  let f x acc = x :: acc in
  let params = Option_misc.fold f bo params in
  let params = Option_misc.fold f ao params in
  LPred (valid_pred_name ~equal ~left:(ao <> None) ~right:(bo <> None) ac pc, p :: params)

(*
If T is a structure:
   valid_T(p, a, b, allocs ...) =
     if T is root:
       offset_min(alloc_i, p) == a &&
       offset_max(alloc_i, p) == b
     else if S is the direct superclass of T:
       valid_S(p, a, b, allocs ...)
     and for all field (T'[a'..b'] f) of p,
       valid_T'(p.f, a', b', allocs ...)
If T is a variant, then we only have the condition on offset_min and max.
*)
let make_valid_pred ~equal ?(left=true) ?(right=true) ac pc =
  let p = "p" in
  let a = "a" in
  let b = "b" in
  let params =
    let all_allocs = match ac with
      | JCalloc_bitvector -> [ ac ]
      | JCalloc_root rt ->
	  match rt.jc_root_info_kind with
	    | Rvariant
	    | RdiscrUnion -> all_allocs ~select:fully_allocated pc
	    | RplainUnion -> [ ac ]
    in
    let allocs = 
      List.map (fun ac -> generic_alloc_table_name ac,alloc_table_type ac) 
	all_allocs
    in
    let all_mems = match ac with
      | JCalloc_bitvector -> []
      | JCalloc_root rt ->
	  match rt.jc_root_info_kind with
	    | Rvariant
	    | RdiscrUnion -> all_memories ~select:fully_allocated pc
	    | RplainUnion -> []
    in
    let mems = 
      List.map (fun mc -> generic_memory_name mc,memory_type mc) all_mems
    in
    let p = p, pointer_type ac pc in
    let a = a, why_integer_type in
    let b = b, why_integer_type in
    let params = allocs @ mems in
    let params = if right then b :: params else params in
    let params = if left then a :: params else params in
      p :: params
  in
  let validity =
    let omin, omax, super_valid = match pc with
      | JCtag ({ jc_struct_info_parent = Some(st, pp) }, _) ->
          LTrue,
          LTrue,
          make_valid_pred_app ~equal
	    (ac,dummy_region) (JCtag(st, pp)) (LVar p) 
	    (if left then Some (LVar a) else None)
	    (if right then Some (LVar b) else None)
      | JCtag ({ jc_struct_info_parent = None }, _)
      | JCroot _ ->
          (if equal then make_eq else make_le) (make_offset_min ac (LVar p)) (LVar a),
          (if equal then make_eq else make_ge) (make_offset_max ac (LVar p)) (LVar b),
          LTrue
    in
    let fields_valid = match ac with
      | JCalloc_bitvector -> [ LTrue ]
      | JCalloc_root rt ->
	  match rt.jc_root_info_kind with
	    | Rvariant ->
		begin match pc with
		  | JCtag(st,_) ->
		      List.map
			(function
			   | { jc_field_info_type =
				 JCTpointer(fpc, Some fa, Some fb) } as fi ->
			       make_valid_pred_app ~equal (ac,dummy_region) fpc
				 (make_select_fi fi (LVar p))
				 (if left then Some (const_of_num fa) else None)
				 (if right then Some (const_of_num fb) else None)
			   | _ ->
			       LTrue)
			st.jc_struct_info_fields
		  | JCroot _ -> [ LTrue ]
		end
	    | RdiscrUnion
	    | RplainUnion -> [ LTrue ]
    in
    let validity = super_valid :: fields_valid in
    let validity = if right then omax :: validity else validity in
    let validity = if left then omin :: validity else validity in
      make_and_list validity
  in
  Predicate(false, valid_pred_name ~equal ~left ~right ac pc, params, validity)

(* Allocation *)

let alloc_write_parameters (ac,r) pc =
  let all_allocs = match ac with
    | JCalloc_bitvector -> [ ac ]
    | JCalloc_root rt ->
	match rt.jc_root_info_kind with
	  | Rvariant
	  | RdiscrUnion -> all_allocs ~select:fully_allocated pc
	  | RplainUnion -> [ ac ]
  in
  let allocs = 
    List.map (fun ac -> plain_alloc_table_var (ac,r), alloc_table_type ac)
      all_allocs
  in
  let all_tags = match ac with
    | JCalloc_bitvector -> []
    | JCalloc_root rt ->
	match rt.jc_root_info_kind with
	  | Rvariant
	  | RdiscrUnion -> [ rt ]
	  | RplainUnion -> []
  in
  let tags = 
    List.map (fun vi -> plain_tag_table_var (vi,r), tag_table_type vi)
      all_tags
  in
  allocs @ tags

let alloc_read_parameters (ac,r) pc =
  let all_mems = match ac with
    | JCalloc_bitvector -> []
    | JCalloc_root rt ->
	match rt.jc_root_info_kind with
	  | Rvariant
	  | RdiscrUnion -> all_memories ~select:fully_allocated pc
	  | RplainUnion -> []
  in
  let mems = 
    List.map 
      (fun mc -> 
	 memory_var ~test_current_function:true (mc,r), memory_type mc)
      all_mems 
  in
  mems

let alloc_arguments (ac,r) pc = 
  let writes = alloc_write_parameters (ac,r) pc in
  let reads = alloc_read_parameters (ac,r) pc in
  List.map fst writes @ List.map fst reads
  
let make_alloc_param ~check_size ac pc =
  let n = "n" in
  let alloc = generic_alloc_table_name ac in
  (* parameters and effects *)
  let writes = alloc_write_parameters (ac,dummy_region) pc in
  let write_effects = 
    List.map (function (Var n,ty') -> n | _ -> assert false) writes
  in
  let write_params = 
    List.map (fun (n,ty') -> (n,Ref_type(Base_type ty'))) writes
  in
  let reads = alloc_read_parameters (ac,dummy_region) pc in
  let read_params = 
    List.map (fun (n,ty') -> (n,Base_type ty')) reads
  in
  let params = [ (Var n,Base_type why_integer_type) ] in
  let params = params @ write_params @ read_params in
  let params = 
    List.map (function (Var n,ty') -> (n,ty') | _ -> assert false) params
  in
  (* precondition *)
  let pre =
    if check_size then LPred("ge_int",[LVar n;LConst(Prim_int "0")]) 
    else LTrue
  in
  (* postcondition *)
  let instanceof_post = match ac with
    | JCalloc_bitvector -> []
    | JCalloc_root rt ->
	match rt.jc_root_info_kind with
	  | Rvariant ->
	      begin match pc with
		| JCtag(st,_) ->
		    let tag = generic_tag_table_name (struct_root st) in
		    (* [instanceof(tagtab,result,tag_st)] *)
		    [ LPred("instanceof",
			    [LVar tag;LVar "result";LVar(tag_name st)]) ]
		| JCroot _ -> []
	      end
	  | RdiscrUnion
	  | RplainUnion -> []
  in
  let alloc_type = 
    Annot_type(
      (* [n >= 0] *)
      pre,
      (* argument pointer type *)
      Base_type(pointer_type ac pc),
      (* reads and writes *)
      [], write_effects,
      (* normal post *)
      make_and_list (
	[
          (* [valid_st(result,0,n-1,alloc...)] *)
          make_valid_pred_app ~equal:true (ac,dummy_region) pc
            (LVar "result")
            (Some (LConst(Prim_int "0")))
            (Some (LApp("sub_int",[LVar n; LConst(Prim_int "1")])));
          (* [alloc_extends(old(alloc),alloc)] *)
          LPred("alloc_extends",[LVarAtLabel(alloc,"");LVar alloc]);
          (* [alloc_fresh(old(alloc),result,n)] *)
          LPred("alloc_fresh",[LVarAtLabel(alloc,"");LVar "result";LVar n])
	] 
	@ instanceof_post ),
      (* no exceptional post *)
      [])
  in
  let alloc_type = 
    List.fold_right (fun (n,ty') acc -> Prod_type(n, ty', acc))
      params alloc_type
  in
  let name = alloc_param_name ~check_size ac pc in
  Param(false,name,alloc_type)

(* Conversion to and from bitvector *)

let make_param ~name ~writes ~reads ~pre ~post ~return_type =
  (* parameters and effects *)
  let write_effects = List.map effect_of_parameter writes in
  let write_params = List.map wparam_of_parameter writes in
  let read_params = List.map rparam_of_parameter reads in
  let params = write_params @ read_params in
  let params = List.map local_of_parameter params in
  (* type *)
  let annot_type = 
    Annot_type(
      pre,
      Base_type return_type,
      (* reads and writes *)
      [], write_effects,
      (* normal post *)
      post,
      (* no exceptional post *)
      [])
  in
  let annot_type = 
    List.fold_right (fun (n,ty') acc -> Prod_type(n, ty', acc))
      params annot_type
  in
  Param(false,name,annot_type)

let conv_bw_alloc_parameters ~deref r pc =
  let ac = JCalloc_bitvector in
  let allocv = 
    if deref then
      alloc_table_var ~test_current_function:true (ac,r)
    else
      plain_alloc_table_var (ac,r)
  in
  let alloc = (allocv, alloc_table_type ac) in
  [ alloc ]

let conv_bw_mem_parameters ~deref r pc =
  let mc = JCmem_bitvector in
  let memv = 
    if deref then
      memory_var ~test_current_function:true (mc,r)
    else 
      plain_memory_var (mc,r)
  in
  let mem = (memv, memory_type mc) in
  [ mem ]

(* let conv_bw_mem_tparameters r pc = *)
(*   let mc = JCmem_bitvector in *)
(*   let memv = tmemory_var ~label_in_name:global_assertion lab (mc,locs#region)  *)
(* tmemory_var ~test_current_function:true (mc,r) *)
(*     else  *)
(*       plain_memory_var (mc,r) *)
(*   in *)
(*   let mem = (memv, memory_type mc) in *)
(*   [ mem ] *)

let conv_typ_alloc_parameters r pc =
  match pc with
    | JCtag _ ->
	let ac = alloc_class_of_pointer_class pc in
	let alloc = (plain_alloc_table_var (ac,r), alloc_table_type ac) in
	[ alloc ]
    | JCroot vi ->
	let ac = JCalloc_root vi in
	let alloc = (plain_alloc_table_var (ac,r), alloc_table_type ac) in
	[ alloc ]

let conv_typ_mem_parameters ~deref r pc =
  let memvar = if deref then deref_memory_var else plain_memory_var in
  match pc with
    | JCtag _ ->
	let all_mems = all_memories pc in
	let mems = 
	  List.map 
	    (fun mc -> memvar (mc,r), memory_type mc) all_mems 
	in
	mems
    | JCroot rt ->
	match rt.jc_root_info_kind with
	  | Rvariant -> []
	  | RdiscrUnion -> assert false (* TODO *)
	  | RplainUnion -> 
	      let mc = JCmem_plain_union rt in
	      let mem = (memvar (mc,r), memory_type mc) in
	      [ mem ]

let make_ofbit_alloc_param_app r pc =
  let writes = conv_typ_alloc_parameters r pc in
  let reads = conv_bw_alloc_parameters ~deref:true r pc in
  let args = List.map fst writes @ List.map fst reads in
  let app = match pc with
    | JCtag _ ->
	make_app (alloc_of_bitvector_param_name pc) args 
    | JCroot rt ->
	match rt.jc_root_info_kind with
	  | Rvariant -> Void
	  | RdiscrUnion -> assert false (* TODO *)
	  | RplainUnion -> assert false (* TODO *)
  in
  let locals = List.map local_of_parameter writes in
  locals, app

let make_ofbit_mem_param_app r pc =
  let writes = conv_typ_mem_parameters ~deref:false r pc in
  let reads = conv_bw_mem_parameters ~deref:true r pc in
  let args = List.map fst writes @ List.map fst reads in
  let app = match pc with
    | JCtag _ ->
	make_app (mem_of_bitvector_param_name pc) args 
    | JCroot rt ->
	match rt.jc_root_info_kind with
	  | Rvariant -> Void
	  | RdiscrUnion -> assert false (* TODO *)
	  | RplainUnion -> assert false (* TODO *)
  in
  let locals = List.map local_of_parameter writes in
  locals, app

let make_tobit_alloc_param_app r pc =
  let writes = conv_bw_alloc_parameters ~deref:false r pc in
  let reads = conv_typ_alloc_parameters r pc in
  let args = List.map fst writes @ List.map fst reads in
  let app = match pc with
    | JCtag _ ->
	make_app (alloc_to_bitvector_param_name pc) args 
    | JCroot rt ->
	match rt.jc_root_info_kind with
	  | Rvariant -> Void
	  | RdiscrUnion -> assert false (* TODO *)
	  | RplainUnion -> assert false (* TODO *)
  in
  app

let make_tobit_mem_param_app r pc =
  let writes = conv_bw_mem_parameters ~deref:false r pc in
  let reads = conv_typ_mem_parameters ~deref:true r pc in
  let args = List.map fst writes @ List.map fst reads in
  let app = match pc with
    | JCtag _ ->
	make_app (mem_to_bitvector_param_name pc) args 
    | JCroot rt ->
	match rt.jc_root_info_kind with
	  | Rvariant -> Void
	  | RdiscrUnion -> assert false (* TODO *)
	  | RplainUnion -> assert false (* TODO *)
  in
  app

let make_of_bitvector_app fi e' =
  (* Convert bitvector into appropriate type *)
  match fi.jc_field_info_type with
    | JCTenum ri -> LApp(logic_enum_of_bitvector ri,[e'])
(*     | JCTnative nty -> LApp(logic_native_of_bitvector nty,[e']) *)
    | JCTpointer(pc,_,_) -> 
	LApp(logic_variant_of_bitvector (pointer_class_root pc),[e'])
    | _ty -> assert false (* TODO *)

let make_conversion_params pc =
  let p = "p" in
  let bv_mem = generic_memory_name JCmem_bitvector in
  let bv_alloc = generic_alloc_table_name JCalloc_bitvector in

  (* postcondition *)
  let post_alloc = match pc with
    | JCtag(st,_) ->
	if struct_has_size st then
	  let post_alloc =
	    let ac = alloc_class_of_pointer_class pc in
	    let alloc = generic_alloc_table_name ac in
	    let s = string_of_int (struct_size_in_bytes st) in
	    let post_min = 
	      make_eq_pred integer_type
		(LApp("offset_min",[ LVar alloc; LVar p ]))
		(LApp("offset_min_bytes",[ LVar bv_alloc; 
					   LApp("pointer_address",[ LVar p ]);
					   LConst(Prim_int s)]))
	    in
	    let post_max = 
	      make_eq_pred integer_type
		(LApp("offset_max",[ LVar alloc; LVar p ]))
		(LApp("offset_max_bytes",[ LVar bv_alloc; 
					   LApp("pointer_address",[ LVar p ]);
					   LConst(Prim_int s)]))
	    in
	    let ty' = pointer_type ac pc in
	    let post = make_and post_min post_max in
	    LForall(p,ty',post)
	  in
	  post_alloc
	else LTrue
    | JCroot _ -> assert false (* TODO *)
  in
  let post_mem = match pc with
    | JCtag(st,_) ->
	if struct_has_size st then
	  let fields = all_fields pc in
	  let post_mem,_ = 
	    List.fold_left 
	      (fun (acc,i) fi ->
		 if field_type_has_bitvector_representation fi then
		   let pi = p ^ (string_of_int i) in
		   let mc = JCmem_field fi in
		   let ac = alloc_class_of_mem_class mc in
		   let mem = 
		     tmemory_var ~label_in_name:true LabelHere
		       (mc,dummy_region) 
		   in
		   let off = 
		     match field_offset_in_bytes fi with
		       | Some x -> x
		       | None ->  assert false
		   in
		   let size = 
		     match fi.jc_field_info_bitsize with
		       | Some x -> x / 8 
		       | None -> assert false
		   in
		   let off = string_of_int off and size = string_of_int size in
		   let posti = 
		     make_eq_pred fi.jc_field_info_type
		       (LApp("select",[ mem; LVar pi ]))
		       (make_of_bitvector_app fi 
			  (LApp("select_bytes",
				[ LVar bv_mem; 
				  LApp("pointer_address",[ LVar pi ]);
				  LConst(Prim_int off); LConst(Prim_int size) ])))
		   in
		   let ty' = pointer_type ac pc in (* Correct pc *)
		   let posti = LForall(pi,ty',posti) in
		   make_and acc posti, i+1
		 else acc, i
	      ) (LTrue,0) fields
	  in
	  post_mem
	else LTrue
    | JCroot _ -> assert false (* TODO *)
  in

  (* Invariant linking typed and byte views *)

(*   let mem_logic = *)
(*     Logic(false, mem_bitvector_logic_name pc, params, result_ty')  *)
(*   in *)

  (* Conversion from bitvector *)
  let writes = conv_typ_alloc_parameters dummy_region pc in
  let reads = conv_bw_alloc_parameters ~deref:true dummy_region pc in
  let name = alloc_of_bitvector_param_name pc in
  let alloc_ofbit_param = 
    make_param ~name ~writes ~reads ~pre:LTrue ~post:post_alloc 
      ~return_type:why_unit_type
  in

  let writes = conv_typ_mem_parameters ~deref:false dummy_region pc in
  let reads = conv_bw_mem_parameters ~deref:true dummy_region pc in
  let name = mem_of_bitvector_param_name pc in
  let mem_ofbit_param = 
    make_param ~name ~writes ~reads ~pre:LTrue ~post:post_mem
      ~return_type:why_unit_type
  in

  (* Conversion to bitvector *)
  let writes = conv_bw_alloc_parameters ~deref:false dummy_region pc in
  let reads = conv_typ_alloc_parameters dummy_region pc in
  let name = alloc_to_bitvector_param_name pc in
  let alloc_tobit_param = 
    make_param ~name ~writes ~reads ~pre:LTrue ~post:post_alloc
      ~return_type:why_unit_type
  in

  let writes = conv_bw_mem_parameters ~deref:false dummy_region pc in
  let reads = conv_typ_mem_parameters ~deref:true dummy_region pc in
  let name = mem_to_bitvector_param_name pc in
  let mem_tobit_param = 
    make_param ~name ~writes ~reads ~pre:LTrue ~post:post_mem
      ~return_type:why_unit_type
  in

  [ alloc_ofbit_param; mem_ofbit_param; alloc_tobit_param; mem_tobit_param ]
  

(******************************************************************************)
(*                               call arguments                               *)
(******************************************************************************)

type param_mode = [ `MAppParam | `MFunParam ]
type effect_mode = [ `MEffect | `MLocalEffect ]
type param_or_effect_mode = [ param_mode | effect_mode ]
type param_or_local_mode = [ param_mode | `MLocal ]
type param_or_effect_or_local_mode = [ param_or_effect_mode | `MLocal ]

let remove_duplicates ~already_used entries =
  fst (List.fold_left 
	 (fun (acc,already_used) entry ->
	    (* Accumulate entry only if not already present *)
	    let n = var_name' (fst (snd entry)) in
	    if StringSet.mem n already_used then
	      acc, already_used
	    else
	      entry :: acc, StringSet.add n already_used
	 ) ([],already_used) entries)

let check_no_duplicates ~already_used entries =
  ignore (List.fold_left 
	    (fun already_used entry ->
	       (* Check entry not already present *)
	       let n = var_name' (fst (snd entry)) in
	       assert (not (StringSet.mem n already_used));
	       StringSet.add n already_used
	    ) already_used entries)

let add_alloc_table_argument 
    ~mode ~type_safe ~no_deref (ac,distr as alloc) ?region_assoc acc =
  let allocvar = 
    if no_deref then plain_alloc_table_var 
    else alloc_table_var ~test_current_function:false
  in
  let ty' = alloc_table_type ac in
  if Region.polymorphic distr then
    try 
      (* Polymorphic allocation table. Both passed in argument by the caller, 
	 and counted as effect. *)
      let locr = 
	Option_misc.map_default (RegionList.assoc distr) distr region_assoc 
      in
      match mode with
	| `MAppParam ->
	    if Region.bitwise locr && not no_deref then
	      (* Anticipate generation of local ref from bitwise *)
	      ((alloc,locr), (deref_alloc_table_var (ac,locr), ty')) :: acc
	    else
	      ((alloc,locr), (allocvar (ac,locr), ty')) :: acc
	| `MFunParam | #effect_mode -> 
	    if Region.bitwise locr && not type_safe then
	      (* Bitwise allocation table in the caller. 
		 Translate the allocation class. *)
	      let ac = JCalloc_bitvector in
	      let ty' = alloc_table_type ac in
	      ((alloc,locr), (allocvar (ac,locr), ty')) :: acc
	    else
	      ((alloc,locr), (allocvar (ac,locr), ty')) :: acc
	| `MLocal -> acc
    with Not_found -> 
      (* MLocal allocation table. Neither passed in argument by the caller, 
	 nor counted as effect. *)
      match mode with
	| #param_or_effect_mode -> acc
	| `MLocal ->
	    if Region.bitwise distr && not type_safe then
	      (* Bitwise allocation table. Translate the allocation class. *)
	      let ac = JCalloc_bitvector in
	      let ty' = alloc_table_type ac in
	      ((alloc,distr), (allocvar (ac,distr), ty')) :: acc
	    else
	      ((alloc,distr), (allocvar (ac,distr), ty')) :: acc
  else 
    (* Constant allocation table. Not passed in argument by the caller, 
       but counted as effect. *)
    match mode with
      | #param_or_local_mode -> acc
      | #effect_mode -> ((alloc,distr), (allocvar (ac,distr), ty')) :: acc

let translate_alloc_table_effects ~region_mem_assoc alloc_effect =
  AllocMap.fold 
    (fun (ac,r) labs acc ->
       let allocs = transpose_alloc_table ~region_mem_assoc (ac,r) in
       AllocSet.fold 
	 (fun (ac,_r) acc -> AllocMap.add (ac,r) labs acc) allocs acc
    ) alloc_effect AllocMap.empty

(* let translate_external_alloc_tables ~no_deref ~region_mem_assoc ~already_used *)
(*     allocs = *)
(*   let already_used = StringSet.of_list already_used in *)
(*   let allocvar =  *)
(*     if no_deref then plain_alloc_table_var  *)
(*     else alloc_table_var ~test_current_function:false *)
(*   in *)
(*   let allocs = *)
(*     List.fold_left *)
(*       (fun acc ((alloc,locr),(v',ty') as entry) -> *)
(* 	 if Region.bitwise locr then *)
(* 	   (\* Translate bitwise allocation table into typed ones *\) *)
(* 	   try *)
(* 	     let mems = MemorySet.find_region locr region_mem_assoc in *)
(* 	     let allocs =  *)
(* 	       List.map *)
(* 		 (fun (mc,_r) -> *)
(* 		    let ac = alloc_class_of_mem_class mc in *)
(* 		    let ty' = alloc_table_type ac in *)
(* 		    ((alloc,locr), (allocvar (ac,locr), ty')) *)
(* 		 ) (MemorySet.elements mems) *)
(* 	     in allocs @ acc *)
(* 	   with Not_found -> *)
(* 	     (\* No possible effect on caller types *\) *)
(* 	     acc *)
(* 	 else entry :: acc *)
(*       ) [] allocs *)
(*   in *)
(*   remove_duplicates ~already_used allocs *)

let alloc_table_detailed_writes ~mode ~type_safe ~callee_writes ?region_assoc
    ~region_mem_assoc =
  let write_effects = callee_writes.jc_effect_alloc_tables in
  let write_effects = 
    if type_safe then
      match mode with
	| #param_mode | `MEffect ->
	    translate_alloc_table_effects ~region_mem_assoc write_effects
      | `MLocalEffect | `MLocal -> write_effects
    else write_effects
  in
  let writes =
    AllocMap.fold
      (fun (ac,distr) _labs acc ->
	 add_alloc_table_argument 
	   ~mode ~type_safe ~no_deref:true (ac,distr) ?region_assoc acc
      ) write_effects []
  in
  if type_safe then
    writes
  else 
    remove_duplicates ~already_used:StringSet.empty writes

let alloc_table_writes ~mode ~type_safe ~callee_writes ?region_assoc
    ~region_mem_assoc =
  List.map snd 
    (alloc_table_detailed_writes ~mode ~type_safe ~callee_writes ?region_assoc
       ~region_mem_assoc)

let alloc_table_detailed_reads ~mode ~type_safe ~callee_writes ~callee_reads 
    ?region_assoc ~region_mem_assoc ~already_used =
  let read_effects = callee_reads.jc_effect_alloc_tables in
  let read_effects = 
    if type_safe then
      match mode with
	| #param_mode | `MEffect ->
	    translate_alloc_table_effects ~region_mem_assoc read_effects
	| `MLocalEffect | `MLocal -> read_effects
    else read_effects
  in
  let reads =
    AllocMap.fold
      (fun (ac,distr) _labs acc ->
	 if has_alloc_table (ac,distr) callee_writes.jc_effect_alloc_tables then
	   (* Allocation table is written, thus it is already taken care of
	      as a parameter. *)
	   match mode with
	     | #param_or_local_mode -> acc
	     | #effect_mode ->
		 add_alloc_table_argument 
		   ~mode ~type_safe ~no_deref:false (ac,distr) ?region_assoc acc
	 else if mutable_alloc_table (get_current_function ()) (ac,distr) then
	   add_alloc_table_argument 
	     ~mode ~type_safe ~no_deref:false (ac,distr) ?region_assoc acc
	 else
	   (* Allocation table is immutable, thus it is not passed by
	      reference. As such, it cannot be counted in effects. *)
	   match mode with
	     | #param_or_local_mode ->
		 add_alloc_table_argument 
		   ~mode ~type_safe ~no_deref:false (ac,distr) ?region_assoc acc
	     | #effect_mode -> acc
      ) read_effects []
  in
  if type_safe then
    reads
  else 
    let already_used = StringSet.of_list already_used in
    remove_duplicates ~already_used reads

let alloc_table_reads ~mode ~type_safe ~callee_writes ~callee_reads 
    ?region_assoc ~region_mem_assoc ~already_used =
  List.map snd
    (alloc_table_detailed_reads ~mode ~type_safe ~callee_writes ~callee_reads 
       ?region_assoc ~region_mem_assoc ~already_used)

let add_tag_table_argument ~mode ~no_deref (vi,distr) ?region_assoc acc =
  let tagvar = if no_deref then plain_tag_table_var else tag_table_var in
  let ty' = tag_table_type vi in
  if Region.polymorphic distr then
    try 
      (* Polymorphic tag table. Both passed in argument by the caller, 
	 and counted as effect. *)
      let locr = 
	Option_misc.map_default (RegionList.assoc distr) distr region_assoc
      in
      match mode with
	| #param_or_effect_mode -> (tagvar (vi,locr), ty') :: acc
	| `MLocal -> acc
    with Not_found -> 
      (* MLocal tag table. Neither passed in argument by the caller, 
	 nor counted as effect. *)
      match mode with
	| #param_or_effect_mode -> acc
	| `MLocal -> (tagvar (vi,distr), ty') :: acc 
  else 
    (* Constant tag table. Not passed in argument by the caller, 
       but counted as effect. *)
    match mode with
      | #param_or_local_mode -> acc
      | #effect_mode -> (tagvar (vi,distr), ty') :: acc 

let tag_table_writes ~mode ~callee_writes ?region_assoc () =
  TagMap.fold
    (fun (vi,distr) _labs acc ->
       add_tag_table_argument 
	 ~mode ~no_deref:true (vi,distr) ?region_assoc acc
    ) callee_writes.jc_effect_tag_tables []

let tag_table_reads ~mode ~callee_writes ~callee_reads ?region_assoc () =
  TagMap.fold
    (fun (vi,distr) _labs acc ->
       if TagMap.mem (vi,distr) callee_writes.jc_effect_tag_tables then
	 (* Tag table is written, thus it is already taken care of
	    as a parameter. *)
	 match mode with
	   | #param_or_local_mode -> acc
	   | #effect_mode ->
	       add_tag_table_argument 
		 ~mode ~no_deref:false (vi,distr) ?region_assoc acc
       else if mutable_tag_table (get_current_function ()) (vi,distr) then
	 add_tag_table_argument 
	   ~mode ~no_deref:false (vi,distr) ?region_assoc acc
       else
	 (* Tag table is immutable, thus it is not passed by
	    reference. As such, it cannot be counted in effects. *)
	 match mode with
	   | #param_or_local_mode ->
	       add_tag_table_argument 
		 ~mode ~no_deref:false (vi,distr) ?region_assoc acc
	   | #effect_mode -> acc
    ) callee_reads.jc_effect_tag_tables []

let add_memory_argument 
    ~mode ~type_safe ~no_deref (mc,distr as mem) ?region_assoc acc =
  let memvar = 
    if no_deref then plain_memory_var 
    else memory_var ~test_current_function:false
  in
  let ty' = memory_type mc in
  if Region.polymorphic distr then
    try 
      (* Polymorphic memory. Both passed in argument by the caller, 
	 and counted as effect. *)
      let locr = 
	Option_misc.map_default (RegionList.assoc distr) distr region_assoc
      in
      match mode with
	| `MAppParam ->
	    if Region.bitwise locr && not no_deref then
	      (* Anticipate generation of local ref from bitwise *)
	      ((mem,locr), (deref_memory_var (mc,locr), ty')) :: acc
	    else
	      ((mem,locr), (memvar (mc,locr), ty')) :: acc
	| `MFunParam | #effect_mode -> 
	    if Region.bitwise locr && not type_safe then
	      (* Bitwise memory in the caller. 
		 Translate the memory class. *)
	      let mc = JCmem_bitvector in
	      let ty' = memory_type mc in
	      ((mem,locr), (memvar (mc,locr), ty')) :: acc
	    else
	      ((mem,locr), (memvar (mc,locr), ty')) :: acc
	| `MLocal -> acc
    with Not_found -> 
      (* MLocal memory. Neither passed in argument by the caller, 
	 nor counted as effect. *)
      match mode with
	| #param_or_effect_mode -> acc
	| `MLocal ->
	    if Region.bitwise distr && not type_safe then
	      (* Bitwise memory. Translate the memory class. *)
	      let mc = JCmem_bitvector in
	      let ty' = memory_type mc in
	      ((mem,distr), (memvar (mc,distr), ty')) :: acc 
	    else
	      ((mem,distr), (memvar (mc,distr), ty')) :: acc 
  else 
    (* Constant memory. Not passed in argument by the caller, 
       but counted as effect. *)
    match mode with
      | #param_or_local_mode -> acc
      | #effect_mode -> ((mem,distr), (memvar (mc,distr), ty')) :: acc 

(* let translate_external_memories ~no_deref ~region_mem_assoc ~already_used mems = *)
(*   let already_used = StringSet.of_list already_used in *)
(*   let memvar =  *)
(*     if no_deref then plain_memory_var  *)
(*     else memory_var ~test_current_function:false *)
(*   in *)
(*   let mems = *)
(*     List.fold_left *)
(*       (fun acc ((mem,locr),(v',ty') as entry) -> *)
(* 	 if Region.bitwise locr then *)
(* 	   try *)
(* 	     (\* Translate bitwise memories into typed ones *\) *)
(* 	     let mems = MemorySet.find_region locr region_mem_assoc in *)
(* 	     let mems =  *)
(* 	       List.map *)
(* 		 (fun (mc,_r) -> *)
(* 		    let ty' = memory_type mc in *)
(* 		    ((mem,locr), (memvar (mc,locr), ty')) *)
(* 		 ) (MemorySet.elements mems) *)
(* 	     in mems @ acc *)
(* 	   with Not_found -> *)
(* 	     (\* No possible effect on caller types *\) *)
(* 	     acc *)
(* 	 else entry :: acc *)
(*       ) [] mems *)
(*   in *)
(*   remove_duplicates ~already_used mems *)

let translate_memory_effects ~region_mem_assoc mem_effect =
  MemoryMap.fold 
    (fun (mc,r) labs acc ->
       let mems = transpose_memory ~region_mem_assoc (mc,r) in
       MemorySet.fold 
	 (fun (mc,_r) acc -> MemoryMap.add (mc,r) labs acc) mems acc
    ) mem_effect MemoryMap.empty

let memory_detailed_writes
    ~mode ~type_safe ~callee_writes ?region_assoc ~region_mem_assoc =
  let write_effects = callee_writes.jc_effect_memories in
  let write_effects = 
    if type_safe then
      match mode with
	| #param_mode | `MEffect ->
	    translate_memory_effects ~region_mem_assoc write_effects
	| `MLocalEffect | `MLocal -> write_effects
    else write_effects
  in
  let writes =
    MemoryMap.fold
      (fun (mc,distr) _labs acc -> 
	 add_memory_argument 
	   ~mode ~type_safe ~no_deref:true (mc,distr) ?region_assoc acc
      ) write_effects []
  in
  if type_safe then
    (* non-interference precondition added later on *)
(*     let () = check_no_duplicates ~already_used:StringSet.empty writes in *)
    writes
  else 
    remove_duplicates ~already_used:StringSet.empty writes

let memory_writes
    ~mode ~type_safe ~callee_writes ?region_assoc ~region_mem_assoc =
  List.map snd 
    (memory_detailed_writes ~mode ~type_safe ~callee_writes 
       ?region_assoc ~region_mem_assoc)

let memory_detailed_reads ~mode ~type_safe ~callee_writes ~callee_reads 
    ?region_assoc ~region_mem_assoc ~already_used =
  let read_effects = callee_reads.jc_effect_memories in
  let read_effects = 
    if type_safe then
      match mode with
	| #param_mode | `MEffect ->
	    translate_memory_effects ~region_mem_assoc read_effects
	| `MLocalEffect | `MLocal -> read_effects
    else read_effects
  in
  let write_effects = callee_writes.jc_effect_memories in
  let write_effects = 
    if type_safe then
      match mode with
	| #param_mode | `MEffect ->
	    translate_memory_effects ~region_mem_assoc write_effects
	| `MLocalEffect | `MLocal -> write_effects
    else write_effects
  in
  let reads =
    MemoryMap.fold
      (fun (mc,distr) _labs acc ->
	 if has_memory (mc,distr) write_effects then
	   (* Memory is written, thus it is already taken care of
	      as a parameter. *)
	   match mode with
	     | #param_or_local_mode -> acc
	     | #effect_mode ->
		 add_memory_argument 
		   ~mode ~type_safe ~no_deref:false (mc,distr) ?region_assoc acc
	 else if mutable_memory (get_current_function ()) (mc,distr) then
	   add_memory_argument 
	     ~mode ~type_safe ~no_deref:false (mc,distr) ?region_assoc acc
	 else
	   (* Memory is immutable, thus it is not passed by
	      reference. As such, it cannot be counted in effects. *)
	   match mode with
	     | #param_or_local_mode ->
		 add_memory_argument 
		   ~mode ~type_safe ~no_deref:false (mc,distr) ?region_assoc acc
	     | #effect_mode -> acc
      ) read_effects []
  in
  let already_used = StringSet.of_list already_used in
  if type_safe then
    (* non-interference precondition added later on *)
(*     let () = check_no_duplicates ~already_used reads in *)
    reads
  else
    remove_duplicates ~already_used reads

let memory_reads ~mode ~type_safe ~callee_writes ~callee_reads 
    ?region_assoc ~region_mem_assoc ~already_used =
  List.map snd 
    (memory_detailed_reads ~mode ~type_safe ~callee_writes ~callee_reads 
       ?region_assoc ~region_mem_assoc ~already_used)
    
let global_writes ~callee_writes =
  VarMap.fold
    (fun v _labs acc -> 
       let n,ty' = param ~type_safe:false v in 
       (plain_var n,ty') :: acc
    ) callee_writes.jc_effect_globals []

let global_reads ~callee_reads =
  VarMap.fold
    (fun v _labs acc -> 
       let n,ty' = param ~type_safe:false v in
       (plain_var n,ty') :: acc
    ) callee_reads.jc_effect_globals []

let local_reads ~callee_reads =
  VarMap.fold
    (fun v _labs acc -> 
       let n,ty' = param ~type_safe:false v in
       (plain_var n,ty') :: acc
    ) callee_reads.jc_effect_locals []

(* Yannick: change this to avoid recovering the real type from its name
   in mutable and committed effects *)

let write_mutable callee_writes =
  StringSet.fold
    (fun v acc -> (mutable_name2 v)::acc) callee_writes.jc_effect_mutable []

let read_mutable callee_reads =
  StringSet.fold
    (fun v acc -> (mutable_name2 v)::acc) callee_reads.jc_effect_mutable []

let write_committed callee_writes =
  StringSet.fold
    (fun v acc -> (committed_name2 v)::acc) callee_writes.jc_effect_committed []

let read_committed callee_reads =
  StringSet.fold
    (fun v acc -> (committed_name2 v)::acc) callee_reads.jc_effect_committed []

let make_region_assoc region_list =
  List.map (fun r -> (r,r)) region_list 

let write_model_parameters 
    ~type_safe ~mode ~callee_reads ~callee_writes ?region_list ~params () =
  let region_assoc = Option_misc.map make_region_assoc region_list in
  let region_mem_assoc = make_region_mem_assoc ~params in
  let callee_writes = rewrite_effects ~type_safe ~params callee_writes in
  let write_allocs = 
    alloc_table_writes ~mode ~type_safe ~callee_writes
      ?region_assoc ~region_mem_assoc
  in
  let write_tags = 
    tag_table_writes ~mode ~callee_writes ?region_assoc ()
  in
  let write_mems = 
    memory_writes ~mode ~type_safe ~callee_writes 
      ?region_assoc ~region_mem_assoc
  in
  let write_globs = match mode with
    | #param_or_local_mode -> []
    | #effect_mode -> global_writes ~callee_writes
  in
  (* TODO: add mutable and committed effects *)
  write_allocs @ write_tags @ write_mems @ write_globs

let write_parameters 
    ~type_safe ~region_list ~callee_reads ~callee_writes ~params =
  let vars' = 
    write_model_parameters ~type_safe ~mode:`MFunParam
      ~callee_reads ~callee_writes ~region_list ~params ()
  in
  List.map (function (Var n,ty') -> (n,ty') | _ -> assert false) vars'

let write_locals ~region_list ~callee_reads ~callee_writes ~params =
  let vars' =
    write_model_parameters ~type_safe:false ~mode:`MLocal 
      ~callee_reads ~callee_writes ~region_list ~params ()
  in
  List.map (function (Var n,ty') -> (n,ty') | _ -> assert false) vars'

let write_effects ~callee_reads ~callee_writes ~region_list ~params =
  let vars' = 
    write_model_parameters ~type_safe:true ~mode:`MEffect
      ~callee_reads ~callee_writes ~region_list ~params ()
  in
  List.map (function (Var n,_ty') -> n | _ -> assert false) vars'

let local_write_effects ~callee_reads ~callee_writes =
  let vars' = 
    write_model_parameters ~type_safe:false ~mode:`MLocalEffect
      ~callee_reads ~callee_writes ~params:[] ()
  in
  List.map (var_name' $ fst) vars'

let read_model_parameters ~type_safe ~mode ~callee_reads ~callee_writes
    ?region_list ~params ~already_used () =
  let region_assoc = Option_misc.map make_region_assoc region_list in
  let region_mem_assoc = make_region_mem_assoc ~params in
  let callee_reads = rewrite_effects ~type_safe ~params callee_reads in
  let callee_writes = rewrite_effects ~type_safe ~params callee_writes in
  let read_allocs = 
    alloc_table_reads ~mode ~type_safe ~callee_reads ~callee_writes 
      ?region_assoc ~region_mem_assoc ~already_used
  in
  let read_tags = 
    tag_table_reads ~mode ~callee_reads ~callee_writes ?region_assoc ()
  in
  let read_mems =
    memory_reads ~mode ~type_safe ~callee_reads ~callee_writes 
      ?region_assoc ~region_mem_assoc ~already_used
  in
  let read_globs = match mode with
    | #param_or_local_mode -> []
    | #effect_mode -> global_reads ~callee_reads
  in
  let read_locs = match mode with
    | #param_or_local_mode | `MEffect -> []
    | `MLocalEffect -> local_reads ~callee_reads
  in
  (* TODO: add mutable and committed effects *)
  read_allocs @ read_tags @ read_mems @ read_globs @ read_locs

let read_parameters 
    ~type_safe ~region_list ~callee_reads ~callee_writes ~params ~already_used =
  let vars' = 
    read_model_parameters ~type_safe ~mode:`MFunParam
      ~callee_reads ~callee_writes ~region_list ~params ~already_used ()
  in
  List.map (function (Var n,ty') -> (n,ty') | _ -> assert false) vars'

let read_locals ~region_list ~callee_reads ~callee_writes ~params =
  let vars' =
    read_model_parameters ~type_safe:false ~mode:`MLocal
      ~callee_reads ~callee_writes ~region_list ~params ~already_used:[] ()
  in
  List.map (function (Var n,ty') -> (n,ty') | (Deref n,ty') -> 
	      printf "Deref %s with type %a@." n Output.fprintf_logic_type ty';
	      assert false
	      | _ -> assert false
	   ) vars'

let read_effects ~callee_reads ~callee_writes ~region_list ~params =
  let vars' = 
    read_model_parameters ~type_safe:true ~mode:`MEffect
      ~callee_reads ~callee_writes ~region_list ~params ~already_used:[] ()
  in
  List.map (var_name' $ fst) vars'

let local_read_effects ~callee_reads ~callee_writes =
  let vars' = 
    read_model_parameters ~type_safe:false ~mode:`MLocalEffect
      ~callee_reads ~callee_writes ~params:[] ~already_used:[] ()
  in
  List.map (var_name' $ fst) vars'

let alloc_table_arguments ~callee_reads ~callee_writes ~region_assoc
    ~region_mem_assoc =
  let writes = 
    alloc_table_detailed_writes 
      ~mode:`MAppParam ~type_safe:true ~callee_writes 
      ~region_assoc ~region_mem_assoc
  in
  let reads = 
    alloc_table_detailed_reads 
      ~mode:`MAppParam ~type_safe:true ~callee_reads ~callee_writes 
      ~region_assoc ~region_mem_assoc ~already_used:[]
  in
  let pointer_of_parameter = function 
      (((ac,distr),locr),(v',ty')) ->
	let pc = match ac with
	  | JCalloc_root vi -> JCroot vi
	  | JCalloc_bitvector -> assert false
	in
	(pc,locr)
  in
  let wpointers = List.map pointer_of_parameter writes in
  let rpointers = List.map pointer_of_parameter reads in
  let write_arguments = List.map (fst $ snd) writes in
  let read_arguments = List.map (fst $ snd) reads in
  wpointers, rpointers, write_arguments, read_arguments

let tag_table_arguments ~callee_reads ~callee_writes ~region_assoc =
  let writes = 
    tag_table_writes ~mode:`MAppParam ~callee_writes ~region_assoc ()
  in
  let reads = 
    tag_table_reads 
      ~mode:`MAppParam ~callee_reads ~callee_writes ~region_assoc ()
  in
  (List.map fst writes), (List.map fst reads)

let specialized_functions = Hashtbl.create 0 

let memory_arguments ~callee_reads ~callee_writes ~region_assoc 
    ~region_mem_assoc ~param_assoc ~with_body fname =
  let writes = 
    memory_detailed_writes
      ~mode:`MAppParam ~type_safe:true ~callee_writes 
      ~region_assoc ~region_mem_assoc
  in
  let reads = 
    memory_detailed_reads 
      ~mode:`MAppParam ~type_safe:true ~callee_reads ~callee_writes 
      ~region_assoc ~region_mem_assoc ~already_used:[]
  in
  let pointer_of_parameter = function
      (((mc,distr),locr),(v',ty')) ->
	let pc = match mc with
	  | JCmem_field fi -> JCtag(fi.jc_field_info_struct,[])
	  | JCmem_plain_union vi -> JCroot vi
	  | JCmem_bitvector -> assert false
	in
	(pc,locr)
  in
  let wpointers = List.map pointer_of_parameter writes in
  let rpointers = List.map pointer_of_parameter reads in
  let remove_local effects =
    List.map (fun ((mem,locr),(v',ty')) -> (mem,(v',ty'))) effects
  in
  let writes' = remove_local writes and reads' = remove_local reads in
  (* Check if there are duplicates between reads and writes *)
  let write_names = List.map (var_name' $ fst $ snd) writes in
  let read_names = List.map (var_name' $ fst $ snd) reads in
  let rw_inter_names = 
    StringSet.inter 
      (StringSet.of_list write_names) (StringSet.of_list read_names) 
  in
  let rw_pre =
    if StringSet.is_empty rw_inter_names then
      LTrue (* no read/write interference *)
    else if not with_body then
      LTrue (* no body in which region assumptions must be verified *)
    else
      write_read_separation_condition 
	~callee_reads ~callee_writes ~region_assoc ~param_assoc
	rw_inter_names writes' reads'
  in
  (* TODO: rewrite postcondition to assert it after the call, when 
     there is an interference. see, e.g., example [separation.c] in Jessie 
     tests.
  *)
  (* Check if there are duplicates between writes *)
  let ww_inter_names = 
    snd (List.fold_left 
	   (fun (first_occur,next_occur) n ->
	      if StringSet.mem n first_occur then
		first_occur, StringSet.add n next_occur
	      else StringSet.add n first_occur, next_occur
	   ) (StringSet.empty,StringSet.empty) write_names)
  in
  let ww_pre =
    if StringSet.is_empty ww_inter_names then
      LTrue (* no write/write interference *)
    else if not with_body then
      LTrue (* no body in which region assumptions must be verified *)
    else
      write_write_separation_condition 
	~callee_reads ~callee_writes ~region_assoc ~param_assoc
	ww_inter_names writes' reads'
  in
  let pre = make_and rw_pre ww_pre in
  if pre = LTrue then 
    let writes = List.map (fst $ snd) writes in
    let reads = List.map (fst $ snd) reads in
    LTrue, fname, wpointers, rpointers, writes, reads
  else 
    (* Presence of interferences. Function must be specialized. *)
    let new_fname = unique_name (fname ^ "_specialized") in
    let writes, name_assoc, already_used_names = 
      List.fold_right 
	(fun ((mc,distr),(v,_ty)) (acc,name_assoc,already_used_names) ->
	   let n = var_name' v in
	   if StringMap.mem n already_used_names then
	     let ndest = StringMap.find n already_used_names in
	     let nsrc = memory_name (mc,distr) in
	     acc, StringMap.add nsrc ndest name_assoc, already_used_names
	   else
	     let ndest = memory_name (mc,distr) in
	     v :: acc, name_assoc, StringMap.add n ndest already_used_names
	) writes' ([], StringMap.empty, StringMap.empty)
    in
    let reads, name_assoc, _ = 
      List.fold_right 
	(fun ((mc,distr),(v,_ty)) (acc,name_assoc,already_used_names) ->
	   let n = var_name' v in
	   if StringMap.mem n already_used_names then
	     let ndest = StringMap.find n already_used_names in
	     let nsrc = memory_name (mc,distr) in
	     acc, StringMap.add nsrc ndest name_assoc, already_used_names
	   else
	     let ndest = memory_name (mc,distr) in
	     v :: acc, name_assoc, StringMap.add n ndest already_used_names
	) reads' ([], name_assoc, already_used_names)
    in
    Hashtbl.add specialized_functions new_fname (fname,name_assoc);
    pre, new_fname, wpointers, rpointers, writes, reads
  
let global_arguments ~callee_reads ~callee_writes ~region_assoc =
  let writes = global_writes ~callee_writes in
  let reads = global_reads ~callee_reads in
  (List.map fst writes), (List.map fst reads)

(* Identify bitwise arguments and generate appropriate typed ones *)
let make_bitwise_arguments alloc_wpointers alloc_rpointers
    mem_wpointers mem_rpointers =
  let bw_pointers pointers = 
    PointerSet.of_list (List.filter (Region.bitwise $ snd) pointers)
  in
  let bw_alloc_wpointers = bw_pointers alloc_wpointers in
  let bw_alloc_rpointers = bw_pointers alloc_rpointers in
  let bw_alloc_pointers = 
    PointerSet.union bw_alloc_wpointers bw_alloc_rpointers
  in
  let bw_mem_wpointers = bw_pointers mem_wpointers in
  let bw_mem_rpointers = bw_pointers mem_rpointers in
  let bw_mem_pointers = 
    PointerSet.union bw_mem_wpointers bw_mem_rpointers
  in
  let bw_pointers = 
    PointerSet.union bw_alloc_pointers bw_mem_pointers
  in
  
  let locals,prolog,epilog = 
    List.fold_left 
      (fun (acc,pro,epi) (pc,r as pointer) -> 
	 let alloc_locals,alloc_ofapp = 
	   if PointerSet.mem_region r bw_alloc_pointers then
	     make_ofbit_alloc_param_app r pc 
	   else [], Void
	 in
	 let mem_locals,mem_ofapp = 
	   if PointerSet.mem pointer bw_mem_pointers then
	     make_ofbit_mem_param_app r pc 
	   else [], Void
	 in
	 let alloc_toapp = 
	   if PointerSet.mem_region r bw_alloc_wpointers then
	     make_tobit_alloc_param_app r pc 
	   else Void
	 in
	 let mem_toapp = 
	   if PointerSet.mem pointer bw_mem_wpointers then
	     make_tobit_mem_param_app r pc 
	   else Void
	 in
	 let locals = alloc_locals @ mem_locals in
	 let ofapp = append alloc_ofapp mem_ofapp in
	 let toapp = append alloc_toapp mem_toapp in
	 locals @ acc, append ofapp pro, append toapp epi
      ) ([],Void,Void) (PointerSet.to_list bw_pointers) 
  in
  let locals =
    fst (List.fold_left 
	   (fun (acc,already_used) entry ->
	      (* Accumulate entry only if not already present *)
	      let n = fst entry in
	      if StringSet.mem n already_used then
		acc, already_used
	      else
		entry :: acc, StringSet.add n already_used
	   ) ([],StringSet.empty) locals)
  in
  locals,prolog,epilog  

let make_arguments 
    ~callee_reads ~callee_writes ~region_assoc ~param_assoc 
    ~with_globals ~with_body fname args =
  let params = List.map fst param_assoc in
  let region_mem_assoc = make_region_mem_assoc ~params in
  let alloc_wpointers, alloc_rpointers, write_allocs, read_allocs = 
    alloc_table_arguments ~callee_reads ~callee_writes ~region_assoc
      ~region_mem_assoc
  in
  let write_tags, read_tags = 
    tag_table_arguments ~callee_reads ~callee_writes ~region_assoc
  in
  let pre_mems, fname, mem_wpointers, mem_rpointers, write_mems, read_mems = 
    memory_arguments ~callee_reads ~callee_writes ~region_assoc
      ~region_mem_assoc ~param_assoc ~with_body fname
  in
  let write_globs, read_globs = 
    if with_globals then
      global_arguments ~callee_reads ~callee_writes ~region_assoc
    else
      [], []
  in
  let locals, prolog, epilog =
    make_bitwise_arguments alloc_wpointers alloc_rpointers
      mem_wpointers mem_rpointers
  in
  (* Return complete list of arguments *)
  (* TODO: add mutable and committed effects *)
  let args =
    args
    @ write_allocs @ write_tags @ write_mems @ write_globs
    @ read_allocs @ read_tags @ read_mems @ read_globs
  in
  pre_mems, fname, locals, prolog, epilog, args

let tmemory_detailed_params ~label_in_name ?region_assoc ?label_assoc reads =
  MemoryMap.fold
    (fun (mc,distr) labs acc ->
       let locr = match region_assoc with
	 | None -> distr
	 | Some region_assoc -> 
	     match transpose_region ~region_assoc distr with
	       | Some r -> r
	       | None -> failwith "Unexpected internal region in logic"
       in
       LogicLabelSet.fold
	 (fun lab acc ->
	    let lab = transpose_label ~label_assoc lab in
	    let param = tmemory_param ~label_in_name lab (mc,locr) in
	    ((mc,locr), param) :: acc
	 ) labs acc
    ) reads.jc_effect_memories []

let tmemory_params ~label_in_name ?region_assoc ?label_assoc reads =
  List.map snd 
    (tmemory_detailed_params ~label_in_name ?region_assoc ?label_assoc reads)

let talloc_table_detailed_params 
    ~label_in_name ?region_assoc ?label_assoc reads =
  AllocMap.fold
    (fun (ac,distr) labs acc ->
       let locr = match region_assoc with
	 | None -> distr
	 | Some region_assoc -> 
	     match transpose_region ~region_assoc distr with
	       | Some r -> r
	       | None -> failwith "Unexpected internal region in logic"
       in
       LogicLabelSet.fold
	 (fun lab acc ->
	    let lab = transpose_label ~label_assoc lab in	    
	    let param = talloc_table_param ~label_in_name lab (ac,locr) in
	    ((ac,locr), param) :: acc
	 ) labs acc
    ) reads.jc_effect_alloc_tables []

let talloc_table_params ~label_in_name ?region_assoc ?label_assoc reads =
  List.map snd
    (talloc_table_detailed_params 
       ~label_in_name ?region_assoc ?label_assoc reads)

let ttag_table_detailed_params ~label_in_name ?region_assoc ?label_assoc reads =
  TagMap.fold
    (fun (vi,distr) labs acc ->
       let locr = match region_assoc with
	 | None -> distr
	 | Some region_assoc -> 
	     match transpose_region ~region_assoc distr with
	       | Some r -> r
	       | None -> failwith "Unexpected internal region in logic"
       in
       LogicLabelSet.fold
	 (fun lab acc ->
	    let lab = transpose_label ~label_assoc lab in	    	    
	    let param = ttag_table_param ~label_in_name lab (vi,locr) in
	    ((vi,locr), param) :: acc
	 ) labs acc
    ) reads.jc_effect_tag_tables []

let ttag_table_params ~label_in_name ?region_assoc ?label_assoc reads =
  List.map snd
    (ttag_table_detailed_params 
       ~label_in_name ?region_assoc ?label_assoc reads)

let tglob_detailed_params ~label_in_name ?region_assoc ?label_assoc reads =
  VarMap.fold
    (fun v labs acc ->
       LogicLabelSet.fold
	 (fun lab acc ->
	    let lab = transpose_label ~label_assoc lab in
	    let param = tparam ~label_in_name lab v in
	    (v, param) :: acc
	 ) labs acc
    ) reads.jc_effect_globals []

let tglob_params ~label_in_name ?region_assoc ?label_assoc reads =
  List.map snd
    (tglob_detailed_params ~label_in_name ?region_assoc ?label_assoc reads)

let tmodel_parameters ~label_in_name ?region_assoc ?label_assoc reads =
  let allocs =
    talloc_table_params ~label_in_name ?region_assoc ?label_assoc reads
  in
  let tags = 
    ttag_table_params ~label_in_name ?region_assoc ?label_assoc reads
  in
  let mems = 
    tmemory_params ~label_in_name ?region_assoc ?label_assoc reads 
  in
  let globs = 
    tglob_params ~label_in_name ?region_assoc ?label_assoc reads 
  in
  allocs @ tags @ mems @ globs

let make_logic_arguments ~label_in_name ~region_assoc ~label_assoc f args =
  let model_params = 
    tmodel_parameters ~label_in_name ~region_assoc ~label_assoc 
      f.jc_logic_info_effects
  in
  let model_args = List.map (fun (_n,v,_ty') -> v) model_params in
  args @ model_args

let make_logic_fun_call ~label_in_name ~region_assoc ~label_assoc f args =
  if Jc_options.debug then printf "logic call to %s@." f.jc_logic_info_name;
  let args = 
    make_logic_arguments ~label_in_name ~region_assoc ~label_assoc f args 
  in
  LApp(f.jc_logic_info_final_name, args)

let make_logic_pred_call ~label_in_name ~region_assoc ~label_assoc f args =
  if Jc_options.debug then printf "logic pred call to %s@." f.jc_logic_info_name;
  let args = 
    make_logic_arguments ~label_in_name ~region_assoc ~label_assoc f args
  in 
  LPred(f.jc_logic_info_final_name, args)


(* *)
let logic_info_reads acc li = 
  let acc =
    MemoryMap.fold
      (fun (mc,r) _ acc -> 
	 StringSet.add (memory_name(mc,r)) acc)
      li.jc_logic_info_effects.jc_effect_memories
      acc
  in
  let acc =
    AllocMap.fold
      (fun (ac,r) labs acc ->
	 StringSet.add (alloc_table_name (ac, r)) acc)
      li.jc_logic_info_effects.jc_effect_alloc_tables
      acc
  in
  TagMap.fold
    (fun v _ acc -> StringSet.add (tag_table_name v) acc)
    li.jc_logic_info_effects.jc_effect_tag_tables
    acc


(* fold all effects into a list *)
let all_effects ef =
  let res =
    MemoryMap.fold
      (fun (mc,r) labels acc -> 
	let mem = memory_name(mc,r) in
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
    VarMap.fold
      (fun v labs acc -> v.jc_var_info_final_name::acc)
      ef.jc_effect_globals
      res
  in
  let res =
    AllocMap.fold
      (fun (a,r) labs acc -> 
	let alloc = alloc_table_name(a,r) in
	if Region.polymorphic r then
(*	  if RegionList.mem r f.jc_fun_info_param_regions then
	    if AllocSet.mem (a,r) 
	      f.jc_fun_info_effects.jc_writes.jc_effect_alloc_tables 
	    then alloc::acc 
	    else acc
	  else acc*)
	  assert false (* TODO *)
	else alloc::acc)
      ef.jc_effect_alloc_tables
      res
  in
  let res =
    TagMap.fold
      (fun v _ acc -> (tag_table_name v)::acc)
      ef.jc_effect_tag_tables
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


(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
