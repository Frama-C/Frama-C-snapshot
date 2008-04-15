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

(* $Id: jc_name.ml,v 1.36 2008/11/05 14:03:15 filliatr Exp $ *)

open Jc_env
open Jc_ast
open Jc_region
open Jc_pervasives
open Output

let alloc_table_type_name = "alloc_table"
let tag_table_type_name = "tag_table"
let pointer_type_name = "pointer"
let pset_type_name = "pset"
let memory_type_name = "memory"
let tag_id_type_name = "tag_id"
let bitvector_type_name = "bitvector"

let simple_logic_type s =
  { logic_type_name = s; logic_type_args = [] }

let root_type_name vi = vi.jc_root_info_name

let struct_type_name st = root_type_name (struct_root st)

let pointer_class_type_name = function
  | JCtag(st, _) -> struct_type_name st
  | JCroot vi -> root_type_name vi

let alloc_class_name = function
  | JCalloc_root vi -> root_type_name vi
  | JCalloc_bitvector -> bitvector_type_name

let memory_class_name = function
  | JCmem_field fi -> fi.jc_field_info_final_name
  | JCmem_plain_union vi -> root_type_name vi
  | JCmem_bitvector -> bitvector_type_name

let variant_alloc_table_name vi = vi.jc_root_info_name ^ "_alloc_table"

let variant_tag_table_name vi = vi.jc_root_info_name ^ "_tag_table"

let variant_axiom_on_tags_name vi = vi.jc_root_info_name ^ "_tags"

let axiom_int_of_tag_name st = st.jc_struct_info_name ^ "_int"

let tag_name st = st.jc_struct_info_name ^ "_tag"

let of_pointer_address_name vi = 
  vi.jc_root_info_name ^ "_of_pointer_address"

let generic_tag_table_name vi =
  (root_type_name vi) ^ "_tag_table"

let tag_table_name (vi,r) =
  if !Jc_common_options.separation_sem = SepRegions && not (is_dummy_region r) 
  then 
    (root_type_name vi) ^ "_" ^ (Region.name r) ^ "_tag_table"
  else
    (root_type_name vi) ^ "_tag_table"

let generic_alloc_table_name ac =
  (alloc_class_name ac) ^ "_alloc_table"

let alloc_table_name (ac,r) = 
  if !Jc_common_options.separation_sem = SepRegions && not (is_dummy_region r) 
  then 
    (alloc_class_name ac) ^ "_" ^ (Region.name r) ^ "_alloc_table"
  else
    (alloc_class_name ac) ^ "_alloc_table"

let field_memory_name fi = 
  let rt = struct_root fi.jc_field_info_struct in
  if root_is_plain_union rt then
    rt.jc_root_info_name ^ "_fields"
  else
    fi.jc_field_info_final_name

let field_region_memory_name (fi,r) = 
  if !Jc_common_options.separation_sem = SepRegions && not (is_dummy_region r) 
  then 
    (field_memory_name fi) ^ "_" ^ (Region.name r)
  else field_memory_name fi

let union_memory_name vi =
  vi.jc_root_info_name ^ "_fields"

let union_region_memory_name (vi,r) = 
  if !Jc_common_options.separation_sem = SepRegions && not (is_dummy_region r) 
  then 
    (union_memory_name vi) ^ "_" ^ (Region.name r)
  else union_memory_name vi

let bitvector_region_memory_name r = 
  if !Jc_common_options.separation_sem = SepRegions && not (is_dummy_region r) 
  then 
    bitvector_type_name ^ "_" ^ (Region.name r)
  else bitvector_type_name

let union_memory_type_name vi = 
  vi.jc_root_info_name ^ "_union"

let generic_memory_name mc =
  memory_class_name mc

let memory_name (mc,r) =
  match mc with
    | JCmem_field fi -> field_region_memory_name (fi,r)
    | JCmem_plain_union vi -> union_region_memory_name (vi,r)
    | JCmem_bitvector -> bitvector_region_memory_name r

let pointer_class_name = function
  | JCtag(st, _) -> 
      if struct_of_union st then
	"root_" ^ (struct_root st).jc_root_info_name
      else
	"struct_" ^ st.jc_struct_info_name
  | JCroot vi -> "root_" ^ vi.jc_root_info_name

let valid_pred_name ~equal ~left ~right ac pc = 
  let prefix = match ac with
    | JCalloc_root _ -> 
	if equal then "strict_valid" else 
	  begin match left, right with
	    | false, false -> assert false
	    | false, true -> "right_valid"
	    | true, false -> "left_valid"
	    | true, true -> "valid"
	  end
    | JCalloc_bitvector -> "valid_bitvector" (* TODO ? *)
  in
  prefix ^ "_" ^ (pointer_class_name pc)

let alloc_param_name ~check_size ac pc = 
  let prefix = match ac with
    | JCalloc_root _ -> "alloc"
    | JCalloc_bitvector -> "alloc_bitvector"
  in
  let n = prefix ^ "_" ^ (pointer_class_name pc) in
  if check_size then n ^ "_requires" else n

let alloc_bitvector_logic_name pc =
  (pointer_class_name pc) ^ "_alloc_bitvector"

let mem_bitvector_logic_name pc =
  (pointer_class_name pc) ^ "_mem_bitvector"

let alloc_of_bitvector_param_name pc = 
  (pointer_class_name pc) ^ "_alloc_of_bitvector"

let mem_of_bitvector_param_name pc = 
  (pointer_class_name pc) ^ "_mem_of_bitvector"

let alloc_to_bitvector_param_name pc = 
  (pointer_class_name pc) ^ "_alloc_to_bitvector"

let mem_to_bitvector_param_name pc = 
  (pointer_class_name pc) ^ "_mem_to_bitvector"

let jessie_return_variable = "return"
let jessie_return_exception = "Return"

let exception_name ei =
  ei.jc_exception_info_name ^ "_exc"

let mutable_name pc =
  "mutable_"^(pointer_class_type_name pc)

let committed_name pc =
  "committed_"^(pointer_class_type_name pc)

let fully_packed_name st =
  "fully_packed_"^(root_name st)

let hierarchy_invariant_name st =
  "global_invariant_"^(root_name st)

let pack_name st =
  "pack_"^(root_name st)

let unpack_name st =
  "unpack_"^(root_name st)

let fully_packed_name = "fully_packed"

let unique_name =
  let unique_names = Hashtbl.create 127 in
  function s ->
    try
      let s = if s = "" then "unnamed" else s in
      let count = Hashtbl.find unique_names s in
      incr count;
      s ^ "_" ^ (string_of_int !count)
    with Not_found ->
      Hashtbl.add unique_names s (ref 0); s


(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
