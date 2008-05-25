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

open Ml_misc
open Jc_env
open Jc_fenv

exception Not_found_str of string

module IdentMap = Map.Make(
  struct
    type t = Ml_ocaml.Ident.t
    let compare x y = String.compare (Ml_ocaml.Ident.unique_name x)
      (Ml_ocaml.Ident.unique_name y)
  end)

type t = {
  vars: Jc_env.var_info StringMap.t;
  funs: Jc_fenv.fun_info StringMap.t;
  (*fields: Jc_env.field_info StringMap.t;
  constructors: (Jc_env.struct_info * int * Jc_env.field_info list) StringMap.t;
  structs: Jc_env.struct_info StringMap.t;*)
  logic_funs: Jc_fenv.logic_info StringMap.t;
  (*tags: Jc_env.field_info StringMap.t;*)
  fun_specs: Ml_ocaml.Typedtree.function_spec IdentMap.t;
  (*type_specs: Ml_ocaml.Typedtree.type_spec IdentMap.t;*)
}

let empty = {
  vars = StringMap.empty;
  funs = StringMap.empty;
  (*fields = StringMap.empty;
  constructors = StringMap.empty;
  structs = StringMap.empty;*)
  logic_funs = StringMap.empty;
  (*tags = StringMap.empty;*)
  fun_specs = IdentMap.empty;
  (*type_specs = IdentMap.empty;*)
}

let add_var name ty env =
  let vi = make_var name ty in
  { env with vars = StringMap.add name vi env.vars }, vi

let add_fun name params return_type env =
  let jc_name = identifier_of_symbol name in
  let fi = make_fun_info jc_name return_type params () in
  { env with funs = StringMap.add name fi env.funs }

(*let add_field name ty si env =
  let fi = {
    jc_field_info_tag = fresh_int ();
    jc_field_info_name = name;
    jc_field_info_final_name = name;
    jc_field_info_type = ty;
    jc_field_info_struct = si;
    jc_field_info_root = si.jc_struct_info_root;
    jc_field_info_rep = false;
  } in
  { env with fields = StringMap.add name fi env.fields }, fi

let add_constructor name tyl si i env =
  let _, fil = list_fold_map
    (fun cnt ty -> cnt + 1,
       let name = name^"_"^string_of_int cnt in
       {
	 jc_field_info_tag = fresh_int ();
	 jc_field_info_name = name;
	 jc_field_info_final_name = name;
	 jc_field_info_type = ty;
	 jc_field_info_struct = si;
	 jc_field_info_root = si.jc_struct_info_root;
	 jc_field_info_rep = false;
       })
    0 tyl
  in
  { env with constructors = StringMap.add name (si, i, fil) env.constructors },
  fil

let add_struct si env =
  { env with structs = StringMap.add si.jc_struct_info_name si env.structs }

let add_tag si env =
  let fi = {
    jc_field_info_tag = fresh_int ();
    jc_field_info_name = "jessica_tag";
    jc_field_info_final_name = "jessica_tag";
    jc_field_info_type = JCTnative Tinteger;
    jc_field_info_struct = si;
    jc_field_info_root = si.jc_struct_info_root;
    jc_field_info_rep = false;
  } in
  { env with tags = StringMap.add si.jc_struct_info_name fi env.tags }, fi*)

let add_logic_fun name params return_type env =
  let li = {
    jc_logic_info_tag = fresh_int ();
    jc_logic_info_name = name;
    jc_logic_info_final_name = name;
    jc_logic_info_result_type = return_type;
    jc_logic_info_parameters = params;
    jc_logic_info_effects = Jc_pervasives.empty_effects; (* ! *)
    jc_logic_info_calls = [];
    jc_logic_info_result_region = default_region;
    jc_logic_info_param_regions = [];
    jc_logic_info_labels = [];
  } in
  { env with logic_funs = StringMap.add name li env.logic_funs }, li

let add_fun_spec id spec env =
  { env with fun_specs = IdentMap.add id spec env.fun_specs }

(*let add_type_spec id spec env =
  { env with type_specs = IdentMap.add id spec env.type_specs }*)

let nf s f k m =
  try
    f k m
  with Not_found ->
    raise (Not_found_str s)

let find_var name env = nf name StringMap.find name env.vars
let find_fun name env = nf name StringMap.find name env.funs
(*let find_field name env = nf name StringMap.find name env.fields
let find_constructor name env = nf name StringMap.find name env.constructors
let find_struct name env = nf name StringMap.find name env.structs*)
let find_logic_fun name env = nf name StringMap.find name env.logic_funs
(*let find_tag si env =
  let name = si.jc_struct_info_name in
  nf name StringMap.find name env.tags*)
let find_fun_spec id env =
  nf (Ml_ocaml.Ident.name id) IdentMap.find id env.fun_specs
(*let find_type_spec id env =
  nf (Ml_ocaml.Ident.name id) IdentMap.find id env.type_specs*)

(*
Local Variables: 
compile-command: "unset LANG; make -C .. -f build.makefile jessica.all"
End: 
*)
