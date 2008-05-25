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

(* Interpretation of Ocaml types to Jessie *)

open Ml_misc
open Jc_ast
open Jc_env
open Jc_fenv
open Jc_output
open Ml_ocaml.Types
open Ml_ocaml.Ident
open Ml_ocaml.Location
open Format

let rec print_type fmt t =
  fprintf fmt "(level %d, id %d: " t.level t.id;
  begin match t.desc with
    | Tvar ->
	fprintf fmt "Tvar"
    | Tarrow(_, a, b, _) ->
	print_type fmt a;
	fprintf fmt " -> ";
	print_type fmt b
    | Ttuple tl ->
	List.iter (fun t -> print_type fmt t; fprintf fmt " * ") tl
    | Tconstr(path, tl, _) ->
	fprintf fmt "Tconstr %s [ " (Ml_ocaml.Path.name path);
	List.iter (fun t -> print_type fmt t; fprintf fmt "; ") tl;
	fprintf fmt "]";
    | Tobject _ ->
	fprintf fmt "Tobject"
    | Tfield _ ->
	fprintf fmt "Tfield"
    | Tnil ->
	fprintf fmt "Tnil"
    | Tlink lt ->
	fprintf fmt "Tlink";
	print_type fmt lt
    | Tsubst _ ->
	fprintf fmt "Tsubst"
    | Tvariant _ ->
	fprintf fmt "Tvariant"
    | Tunivar ->
	fprintf fmt "Tunivar"
    | Tpoly _ ->
	fprintf fmt "Tpoly"
  end;
  fprintf fmt ")"

let print_type t =
  let fmt = formatter_of_out_channel stdout in
  print_type fmt t;
  fprintf fmt "@."

type ml_label_info = {
  ml_li_name: string;
  ml_li_structure: Jc_env.struct_info;
  ml_li_field: Jc_env.field_info;
}

type ml_constructor_info = {
  ml_ci_name: string;
  ml_ci_structure: Jc_env.struct_info;
  ml_ci_arguments: Jc_env.field_info list;
}

type ml_array_info = {
  ml_ai_struct: Jc_env.struct_info;
  ml_ai_data_field: Jc_env.field_info;
  ml_ai_make: Jc_fenv.fun_info;
}

type ml_jessie_type =
  | MLTnot_closed
  | MLTnative of native_type
  | MLTrecord of Jc_env.struct_info * (string * ml_label_info) list
  | MLTvariant of Jc_env.variant_info * (string * ml_constructor_info) list
  | MLTtuple of Jc_env.struct_info
  | MLTlogic of string
  | MLTarray of ml_array_info

module ComparableCamlTypeList = struct
  type t = type_expr list

  let rec compare_lists f l1 l2 = match l1, l2 with
    | [], [] -> 0
    | x1::rem1, x2::rem2 ->
	let r = f x1 x2 in
	if r = 0 then compare_lists f rem1 rem2 else r
    | _::_, [] -> 1
    | [], _::_ -> -1

  let rec compare_types a b = match a.desc, b.desc with
    | Tvar, Tvar -> Pervasives.compare a.id b.id
    | Tlink a', _ -> compare_types a' b
    | _, Tlink b' -> compare_types a b'
    | Tconstr(p1, al1, _), Tconstr(p2, al2, _) ->
	let r = String.compare
	  (Ml_ocaml.Path.name p1)
	  (Ml_ocaml.Path.name p2)
	in
	if r = 0 then compare_lists compare_types al1 al2 else r
    | _ -> Pervasives.compare a b

  let rec compare = compare_lists compare_types
end

module ParamMap = Map.Make(ComparableCamlTypeList)

type ml_caml_type = {
  ml_ty_name: string;
  ml_ty_decl: Ml_ocaml.Types.type_declaration;
  ml_ty_logic: bool; (* unused, actually (using "Type_abstract" instead) *)
  mutable ml_ty_instances: ml_jessie_type ParamMap.t;
  mutable ml_ty_invariants: (string * Jc_env.var_info * Jc_ast.assertion) list;
}

let ml_types = Hashtbl.create 11 (* string -> ml_caml_type *)
let ml_tuples: Jc_env.struct_info ParamMap.t ref = ref ParamMap.empty
let ml_arrays: ml_array_info ParamMap.t ref = ref ParamMap.empty

let declare_str n td logic =
  Hashtbl.add ml_types n {
    ml_ty_name = n;
    ml_ty_decl = td;
    ml_ty_logic = logic;
    ml_ty_instances = ParamMap.empty;
    ml_ty_invariants = [];
  }

let declare id = declare_str (name id)

(* declare pervasive types *)
let _ =
  declare_str "list" Ml_ocaml.Predef.decl_list false

let add_invariant id inv =
  let mlty = Hashtbl.find ml_types (name id) in
  mlty.ml_ty_invariants <- inv::mlty.ml_ty_invariants

exception Not_closed

let rec make_type mlt =
  let not_implemented x = not_implemented none x in
  match mlt.desc with
    | Tvar -> raise Not_closed
    | Tarrow _ -> not_implemented "ml_type.ml: make_type: Tarrow"
    | Ttuple tl ->
	begin try
	  MLTtuple(ParamMap.find tl !ml_tuples)
	with Not_found ->
	  let jcty = tuple tl in
	  ml_tuples := ParamMap.add tl jcty !ml_tuples;
	  MLTtuple jcty
	end
    | Tconstr(path, args, _) ->
	begin match Ml_ocaml.Path.name path with
	  | "unit" -> MLTnative Tunit
	  | "int" -> MLTnative Tinteger
	  | "float" -> MLTnative Treal
	  | "bool" -> MLTnative Tboolean
	  | "array" ->
	      begin try
		MLTarray(ParamMap.find args !ml_arrays)
	      with Not_found ->
		let vi = make_variant (fresh_ident "jessica_array") in
		let si = make_root_struct vi (fresh_ident "jessica_array") in
		let fi, argty = match args with
		  | [ty] ->
		      let argty = make ty in
		      make_field si "t" argty, argty
		  | _ -> assert false (* array with #arguments <> 1 ?? *)
		in
		let make = make_fun_info
		  ~name:(fresh_ident "jessica_array_make")
		  ~return_type:(make_pointer ~min:0 (JCtag si))
		  ~params:[
		    make_var_info ~name:"n" ~ty:(JCTnative Tinteger);
		    make_var_info ~name:"v" ~ty:argty;
		  ]
		  ()
		in
		let ai = {
		  ml_ai_struct = si;
		  ml_ai_data_field = fi;
		  ml_ai_make = make;
		} in
		ml_arrays := ParamMap.add args ai !ml_arrays;
		MLTarray ai
	      end
	  | name ->
	      let ty = try
		Hashtbl.find ml_types name
	      with Not_found ->
		not_implemented "ml_type.ml: make_type: predefined type %s" name
	      in
	      begin try
		ParamMap.find args ty.ml_ty_instances
	      with Not_found ->
		try
		  let jcty = instance args ty in
		  ty.ml_ty_instances <-
		    ParamMap.add args jcty ty.ml_ty_instances;
		  jcty
		with Not_closed ->
		  MLTnot_closed
	      end
	end
    | Tobject _ -> not_implemented "ml_type.ml: make_type: Tobject"
    | Tfield _ -> not_implemented "ml_type.ml: make_type: Tfield"
    | Tnil -> not_implemented "ml_type.ml: make_type: Tnil"
    | Tlink t -> make_type t
    | Tsubst _ -> not_implemented "ml_type.ml: make_type: Tsubst"
    | Tvariant _ -> not_implemented "ml_type.ml: make_type: Tvariant"
    | Tunivar -> not_implemented "ml_type.ml: make_type: Tunivar"
    | Tpoly _ -> not_implemented "ml_type.ml: make_type: Tpoly"

and make mlt =
  match make_type mlt with
    | MLTnot_closed ->
	JCTlogic "caml_not_closed"
    | MLTnative t ->
	JCTnative t
    | MLTrecord(si, _)
    | MLTtuple si ->
	make_valid_pointer (JCtag si)
    | MLTvariant(vi, _) ->
	make_valid_pointer (JCvariant vi)
    | MLTlogic x ->
	JCTlogic x
    | MLTarray ai ->
	make_pointer ~min:0 (JCtag ai.ml_ai_struct)

and instance args ty =
  log "Instanciate type %s with %d/%d arguments." ty.ml_ty_name
    (List.length args) ty.ml_ty_decl.type_arity;
  match ty.ml_ty_decl.type_kind with
    | Type_abstract ->
	MLTlogic(fresh_ident ty.ml_ty_name)
    | Type_record(ll, _, _) ->
	let vi = make_variant (fresh_ident ty.ml_ty_name) in
	let si = make_root_struct vi (fresh_ident ty.ml_ty_name) in
	(* temporary declaration in case of recursive type definition *)
	ty.ml_ty_instances <- ParamMap.add
	  args (MLTrecord(si, [])) ty.ml_ty_instances;
	let lbls = List.map
	  (fun (name, _, lty) ->
	     let app_ty = Ml_ocaml.Ctype.apply (Ml_ocaml.Env.empty)
	       ty.ml_ty_decl.type_params lty args in
	     let fi = make_field si name (make app_ty) in
	     let li = {
	       ml_li_name = name;
	       ml_li_structure = si;
	       ml_li_field = fi;
	     } in
	     name, li)
	  ll
	in
	MLTrecord(si, lbls)
    | Type_variant(cl, _) ->
	let vi = make_variant (fresh_ident ty.ml_ty_name) in
	(* temporary declaration in case of recursive type definition *)
	ty.ml_ty_instances <- ParamMap.add
	  args (MLTvariant(vi, [])) ty.ml_ty_instances;
	let constrs = List.map
	  (fun (name, cargs) ->
	     let si = make_root_struct vi (fresh_ident name) in
	     let app_cargs = List.map
	       (fun caty ->
		  Ml_ocaml.Ctype.apply (Ml_ocaml.Env.empty)
		    ty.ml_ty_decl.type_params caty args)
	       cargs
	     in
	     let fi_args = list_mapi
	       (fun i ty -> make_field si (name^string_of_int i) (make ty))
	       app_cargs
	     in
	     let ci = {
	       ml_ci_name = name;
	       ml_ci_structure = si;
	       ml_ci_arguments = fi_args;
	     } in
	     name, ci)
	  cl
	in
	MLTvariant(vi, constrs)

and tuple tl =
  let vi = make_variant (fresh_ident "jessica_tuple") in
  let si = make_root_struct vi (fresh_ident "jessica_tuple") in
  list_iteri
    (fun i ty -> ignore (make_field si ("f"^string_of_int i) (make ty)))
    tl;
  si.jc_struct_info_fields <- List.rev si.jc_struct_info_fields;
  si

let structure ty =
  match make_type ty with
    | MLTrecord(si, _)
    | MLTtuple si
    | MLTarray{ml_ai_struct = si} -> si
    | _ -> failwith "ml_type.ml: structure: not translated to a structure type"

let label recty ld =
  match make_type recty with
    | MLTrecord(_, lbls) -> List.assoc ld.lbl_name lbls
    | _ -> failwith "ml_type.ml: label: not a record type"

let constructor varty cd =
  match make_type varty with
    | MLTvariant(_, constrs) -> List.assoc cd.cstr_name constrs
    | _ -> failwith "ml_type.ml: constructor: not a variant type"

let proj tty index =
  match make_type tty with
    | MLTtuple si -> List.nth si.jc_struct_info_fields index
    | _ -> failwith "ml_type.ml: proj: not a tuple type"

let array aty =
  match make_type aty with
    | MLTarray ai -> ai
    | _ -> failwith "ml_type.ml: array: not an array type"

let get_variant si = match si.jc_struct_info_variant with
  | None -> raise (Invalid_argument "ml_type.ml, get_variant")
  | Some vi -> vi

let jc_decl mlty = function
  | MLTnot_closed ->
      assert false
  | MLTnative t ->
      [ JClogic_type_def mlty.ml_ty_name ]
  | MLTrecord(si, lbls) ->
      [ make_variant_def (get_variant si);
	JCstruct_def(
	  si.jc_struct_info_name,
	  (match si.jc_struct_info_parent with
	     | None -> None
	     | Some si -> Some si.jc_struct_info_name),
	  List.map (fun (_, l) -> l.ml_li_field) lbls,
	  mlty.ml_ty_invariants
	)]
  | MLTvariant(vi, constrs) ->
      let c_defs = List.map
	(fun (_, ci) ->
	   let si = ci.ml_ci_structure in
	   JCstruct_def(
	     si.jc_struct_info_name,
	     None,
	     ci.ml_ci_arguments,
	     mlty.ml_ty_invariants
	   ))
	constrs
      in
      (make_variant_def vi)::c_defs
  | MLTlogic x ->
      [ JClogic_type_def x ]
  | MLTarray _ | MLTtuple _ ->
      (* declarations are made in jc_tuple_decl or jc_array_decl *)
      []

let jc_tuple_decl _ si acc = [
  make_variant_def (get_variant si);
  make_struct_def si [];
] @ acc

let jc_array_decl _ ai acc =
  let n, v = couple_of_list ai.ml_ai_make.jc_fun_info_parameters in
  let req = make_assertion
    (JCArelation(
       make_int_term (JCTvar n),
       Bge_int,
       term_of_int 0))
  in
  let rty = ai.ml_ai_make.jc_fun_info_result.jc_var_info_type in
  let result = result_term rty in
  let i = make_var_info ~name:"i" ~ty:(JCTnative Tinteger) in
  let ens = make_and_list [
    (*make_assertion
      (JCArelation(
	 make_offset_min result ai.ml_ai_struct,
	 Beq_int,
	 term_of_int 0));*) (* no need: included in return type *)
    make_assertion
      (JCArelation(
	 make_offset_max result ai.ml_ai_struct,
	 Beq_int,
	 make_int_term
	   (JCTbinary(
	      make_var_term n,
	      Bsub_int,
	      term_of_int 1))));
    make_assertion
      (JCAquantifier(
	 Forall, i,
	 make_assertion
	   (JCArelation(
	      make_deref_term
		(make_shift_term result (make_var_term i))
		ai.ml_ai_data_field,
	      Beq_int, (* hack *)
	      make_var_term v))))
  ] in
(*  let ass =
    [make_deref_location
       (JCLSvar (result_var rty))
       ai.ml_ai_data_field]
  in*)
  [
    make_variant_def (get_variant ai.ml_ai_struct);
    make_struct_def ai.ml_ai_struct [];
    make_fun_def
      ~name:ai.ml_ai_make.jc_fun_info_final_name
      ~return_type:rty
      ~params:ai.ml_ai_make.jc_fun_info_parameters
      ~spec:(make_fun_spec ~requires:req(* ~assigns:ass*) ~ensures:ens ())
      ()
  ] @ acc

let jc_decls () =
  let decls = [] in
  let decls = ParamMap.fold jc_tuple_decl !ml_tuples decls in
  let decls = ParamMap.fold jc_array_decl !ml_arrays decls in
  let decls = Hashtbl.fold
    (fun _ ty acc ->
       ParamMap.fold
	 (fun _ ity acc -> jc_decl ty ity @ acc)
	 ty.ml_ty_instances
	 acc)
    ml_types
    decls
  in
  decls

(*
Local Variables: 
compile-command: "unset LANG; make -C .. -f build.makefile jessica.all"
End: 
*)
