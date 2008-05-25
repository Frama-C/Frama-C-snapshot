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

(* $Id: java_typing.ml,v 1.123 2008/05/23 08:16:10 marche Exp $ *)

open Java_env
open Java_ast
open Java_tast
open Format
open Java_pervasives

(************************

Pretty-printing of types

*************************)

let rec print_qualified_ident fmt id =
  match id with
    | [] -> assert false
    | [(_,id)] -> fprintf fmt "%s" id
    | (_,id)::r ->
	print_qualified_ident fmt r;
	fprintf fmt ".%s" id
	
let print_type_name fmt t =
  match t with
    | TypeClass ci -> fprintf fmt "%s" ci.class_info_name
    | TypeInterface ii -> fprintf fmt "%s" ii.interface_info_name
(*
    | TypeLogic s -> fprintf fmt "logic type %s" s
*)

let string_of_base_type t =
  match t with
    | Tunit -> "unit"
    | Tinteger -> "integer"
    | Treal -> "real"
    | Tboolean -> "boolean"
    | Tdouble -> "double"
    | Tlong -> "long"
    | Tfloat -> "float"
    | Tint -> "int"
    | Tchar -> "char"
    | Tbyte -> "byte"
    | Tshort -> "short"


let rec print_type fmt t =
  match t with
    | JTYbase t -> fprintf fmt "%s" (string_of_base_type t)
    | JTYnull -> fprintf fmt "(nulltype)"
    | JTYarray (non_null, t) -> 
	fprintf fmt "%s%a[]" (if non_null then "" else "!") print_type t
    | JTYclass (non_null, c) -> 
	fprintf fmt "%s%s" (if non_null then "" else "!") c.class_info_name
    | JTYinterface ii -> fprintf fmt "%s" ii.interface_info_name
    | JTYlogic i ->fprintf fmt "%s" i

(***********************

Typing error handling

************************)

exception Typing_error of Loc.position * string

let typing_error l = 
  Format.kfprintf 
    (fun fmt -> raise (Typing_error(l, flush_str_formatter()))) 
    str_formatter

let integer_expected l t = 
  typing_error l "integer type expected, got %a" print_type t

let array_expected l t = 
  typing_error l "array type expected, got %a" print_type t

let int_expected l t = 
  typing_error l "int expected, got %a" print_type t

let class_or_interface_expected l t =
  typing_error l "class or interface expected, got %a" print_type t



let catch_typing_errors f a =    
 try
   f a
 with
   | Typing_error(l,s) ->
       eprintf "%a: typing error: %s@." Loc.gen_report_position l s;
       exit 1
   | Java_options.Java_error(l,s) ->
       eprintf "%a: %s@." Loc.gen_report_position l s;
       exit 1
   | Assert_failure(f,l,c) as exn -> 
       eprintf "%a:@." Loc.gen_report_line (f,l,c,c);
       raise exn
   | Match_failure(f,l,c) as exn -> 
       eprintf "%a:@." Loc.gen_report_line (f,l,c,c);
       raise exn


(**********************

Name classification 
(JLS 2nd ed., pp. 93-104)

***********************)

let split_extension filename =
  try
    let dotindex = String.rindex filename '.' in
    (String.sub filename 0 dotindex,
     String.sub filename (dotindex+1) (String.length filename - dotindex - 1))
  with Not_found -> filename,""

let read_dir d =
  let l =
    try
      Sys.readdir d
    with
	Sys_error _ -> 
	  eprintf "Cannot open directory %s@." d;
	  exit 2
  in
  Array.fold_left
    (fun (dirs,files) name ->
       let fullname = Filename.concat d name in
       let s = Unix.lstat fullname in
       match s.Unix.st_kind with
	 | Unix.S_DIR when name <> "CVS" ->
	     begin
	       Java_options.lprintf "got sub-directory %s@." name;
	       ((name,fullname)::dirs,files)
	     end
	 | Unix.S_REG ->
	     begin
	       let base,ext = split_extension name in
	       match ext with
		 | "java" ->
		     Java_options.lprintf "got Java file %s@." base;
		     (dirs,(base,fullname)::files)
		 | _ -> 
		   Java_options.lprintf "file %s ignored@." name;
		     (dirs,files)
	     end
	 | _ ->
	     begin
	       Java_options.lprintf "skipping special file %s@." name;
	       (dirs,files)
	     end)
    ([],[]) l
	     
  
(* [package_table] maps each internal package id to package_info *)
let package_table = 
  (Hashtbl.create 17 : (int,package_info) Hashtbl.t)

let package_counter = ref 0

let new_package_info name fullname =
  incr package_counter;
  let pi =
    { package_info_tag = !package_counter;
      package_info_name = name;
      package_info_directory = fullname;
    }
  in
  Hashtbl.add package_table !package_counter pi;
  pi

(* [package_contents] maps each internal package id to its contents *)
type package_member =
  | Subpackage of package_info
  | File of string
  | Type of java_type_info

let package_contents = 
  (Hashtbl.create 17 : (int,(string,package_member) Hashtbl.t) Hashtbl.t)


let anonymous_package = new_package_info "" "."

let javalang_qid = [(Loc.dummy_position,"lang"); (Loc.dummy_position,"java")]

let read_dir_contents d =
  Java_options.lprintf "reading directory '%s'@." d; 
  let (d,f) = read_dir d in
  let h = Hashtbl.create 17 in
  List.iter
    (fun (name,fullname) ->
       let pi = new_package_info name fullname in
       Java_options.lprintf "adding subpackage %s@."	name; 
       Hashtbl.add h name (Subpackage pi))
    d;
  List.iter
    (fun (name,fullname) ->
       Java_options.lprintf "adding java file %s (fullname %s)@." name fullname; 
       Hashtbl.add h name (File fullname))
    f;
  h

let get_package_contents pi =
  try
    Hashtbl.find package_contents pi.package_info_tag
  with
      Not_found ->
	let l = read_dir_contents pi.package_info_directory in
	Hashtbl.add package_contents pi.package_info_tag l;
	l

let toplevel_packages = 
  Java_options.lprintf "Reading toplevel packages@.";
  let h = Hashtbl.create 17 in
  List.iter
    (fun dir ->
       let h' = read_dir_contents dir in
       Hashtbl.iter (Hashtbl.add h) h')
    Java_options.classpath;
  h

 
let type_table : (int, java_type_info) Hashtbl.t = Hashtbl.create 97

(* A GROUPER *)
let class_type_env_table : 
    (int, (string * java_type_info) list) Hashtbl.t
    = Hashtbl.create 97
and interface_type_env_table : 
    (int, (string * java_type_info) list) Hashtbl.t
    = Hashtbl.create 97



let class_decl_table : 
    (int, (package_info list * 
	     Java_ast.class_declaration)) Hashtbl.t
    = Hashtbl.create 97


let interface_decl_table : 
    (int, (package_info list * 
	     Java_ast.interface_declaration)) Hashtbl.t
    = Hashtbl.create 97

let invariants_env : 
    (int, (* class tag *) 
     Java_env.java_type_info (* class or interface *)
     * (string * Java_env.java_var_info) list (* env [this,this_info]*)
     * Java_env.java_var_info (* this_info *)
     * (Java_ast.identifier * Java_ast.pexpr) list (* (inv_name,inv_pred) list *)
    ) Hashtbl.t 
    = Hashtbl.create 97
let invariants_table :
    ( int, (* class tag *)
      Java_env.java_class_info (* class_info *)
      * Java_env.java_var_info (* this_info *)
      * (Java_ast.identifier * Java_tast.assertion) list (* (inv_name, typed pred) list *)
      ) Hashtbl.t
    = Hashtbl.create 97

let static_invariants_env = Hashtbl.create 97
let static_invariants_table = Hashtbl.create 97


(* variables *)

module StringSet = Set.Make(String)

(* used names (in order to rename identifiers when necessary) *)
let used_names = Hashtbl.create 97

let mark_as_used x = 
  Hashtbl.add used_names x ()

let () = 
  List.iter mark_as_used 
    (* Jessie reserved names *)
    [ "tag"; "type" ; "end" ; "begin"]

let is_used_name n = Hashtbl.mem used_names n

let use_name ?local_names n = 
  if is_used_name n then raise Exit; 
  begin match local_names with 
    | Some h -> if StringSet.mem n h then raise Exit 
    | None -> () 
  end;
  mark_as_used n;
  n

let rec next_name ?local_names n i = 
  let n_i = n ^ "_" ^ string_of_int i in
  try use_name ?local_names n_i 
  with Exit -> next_name ?local_names n (succ i)

let get_unique_name ?local_names n = 
  try use_name ?local_names n 
  with Exit -> next_name ?local_names n 0

let get_final_name id =
  if id = "\\result" then id else
    get_unique_name id


let var_tag_counter = ref 0

let new_var loc ty id =
  incr var_tag_counter;
  let vi = {
    java_var_info_tag = !var_tag_counter;
    java_var_info_name = id;
    java_var_info_decl_loc = loc;
    java_var_info_final_name = get_final_name id;
    java_var_info_type = ty;
    java_var_info_assigned = false;
  }
  in vi

let rec var_type_and_id non_null ty id =
  match id with
    | Simple_id id -> ty, id
    | Array_id id -> 
	let ty, id = var_type_and_id non_null ty id in
	  JTYarray (non_null, ty), id


(* fields *)
	    
let field_prototypes_table : (int, variable_initializer option) Hashtbl.t 
    = Hashtbl.create 97

let field_tag_counter = ref 0
  
let new_field ~is_static ~is_final ~is_nullable ~is_model ~is_ghost ti ty id =
  incr field_tag_counter;
  let fi = {
    java_field_info_tag = !field_tag_counter;
    java_field_info_name = id;
    java_field_info_type = ty;
    java_field_info_class_or_interface = ti;
    java_field_info_is_static = is_static;
    java_field_info_is_final = is_final;
    java_field_info_is_nullable = is_nullable;
    java_field_info_is_model = is_model;
    java_field_info_is_ghost = is_ghost;
  } in fi
	 

let new_model_field ii ty id =
  new_field ~is_static:false ~is_final:false ~is_nullable:false
    ~is_model:true (TypeInterface ii) ty id

(* methods *)

let methods_env = Hashtbl.create 97 

let method_or_constr_tag_counter = ref 0

let new_method_info ~is_static ~result_is_nullable loc id ti ty pars =
  incr method_or_constr_tag_counter;
  let result = 
      Option_misc.map (fun t -> new_var Loc.dummy_position t "\\result") ty
  in
  {
    method_info_tag = !method_or_constr_tag_counter;
    method_info_loc = loc;
    method_info_name = id;
    method_info_class_or_interface = ti;
    method_info_trans_name = id;
    method_info_has_this = None;
    method_info_parameters = pars;
    method_info_result = result;
    method_info_result_is_nullable = result_is_nullable;
    method_info_calls = [];
    method_info_is_static = is_static;
  }

let constructors_env = Hashtbl.create 97

let new_constructor_info ci loc pars =
  incr method_or_constr_tag_counter;
  {
    constr_info_tag = !method_or_constr_tag_counter;
    constr_info_loc = loc;
    constr_info_class = ci;
    constr_info_trans_name = ci.class_info_name;
    constr_info_this = None;
    constr_info_result = None;
    constr_info_parameters = pars;
    constr_info_calls = [];
  }
    
let rec list_assoc_name f id l =
  match l with
    | [] -> raise Not_found
    | fi::r -> 
	if (f fi) = id then fi
	else list_assoc_name f id r


(******************************

Typing level 0: extract types (logic, Java classes, Java interfaces)

**********************************)


let type_counter = ref 0

(* adds an interface named [id] in package [p] *)
let new_interface_info (p:package_info) (id:string) i =
  incr type_counter;
  let ii =    
    { interface_info_tag = !type_counter;
      interface_info_name = id;
      interface_info_package_env = (fst i);
      interface_info_incomplete = true;
      interface_info_extends = [];
      interface_info_fields = [];
      interface_info_methods = [];
    }
  in
  Hashtbl.add type_table !type_counter (TypeInterface ii);
  Hashtbl.add interface_decl_table !type_counter i;
  Java_options.lprintf "adding interface %s in package '%s'@." id p.package_info_name;
  let h = get_package_contents p in
  Hashtbl.replace h id (Type (TypeInterface ii));
  ii
    
let new_class_info (p:package_info) (id:string) c =
  incr type_counter;
  let ci =    
    { class_info_tag = !type_counter;
      class_info_name = id;
      class_info_package_env = (fst c);
      class_info_incomplete = true;
      class_info_extends = None;
      class_info_is_exception = false;
      class_info_implements = [];
      class_info_fields = [];
      class_info_methods = [];
      class_info_constructors = [];
    }
  in
    Hashtbl.add type_table !type_counter (TypeClass ci);
    Hashtbl.add class_decl_table !type_counter c;
    Java_options.lprintf 
    "adding class %s in package %s@." id p.package_info_name;
    let h = get_package_contents p in
      Hashtbl.replace h id (Type (TypeClass ci));
      ci


let logic_types_table = Hashtbl.create 97

let new_logic_type id = id

let get_type_decl package package_env acc d = 
    match d with
    | JPTclass c -> 
	(*
	  class_modifiers : modifiers;
	  class_name : identifier;
	  class_extends : qualified_ident option;
	  class_implements : qualified_ident list;
	  class_fields : field_declaration list
	*)
	let (_,id) = c.class_name in
	let ci = new_class_info package id (package_env,c) in 
	(id,TypeClass ci)::acc
    | JPTinterface i -> 
	let (_,id) = i.interface_name in
	let ii = new_interface_info package id (package_env,i) in 
	(id,TypeInterface ii)::acc
    | JPTannot(loc,s) -> assert false
    | JPTlemma((loc,id),is_axiom, labels,e) -> acc
    | JPTlogic_type_decl (loc,id) ->
	begin
	  try
	    let _ = Hashtbl.find logic_types_table id in
	    typing_error loc "logic type already defined"
	  with
	      Not_found ->
		let i = new_logic_type id in
		Hashtbl.add logic_types_table id i;
		acc
	end
    | JPTlogic_reads((loc,id),ret_type,labels,params,reads) -> acc 
    | JPTlogic_def((loc,id),ret_type,labels,params,body) -> acc


type classified_name =
  | TermName of term
  | TypeName of java_type_info
  | LogicTypeName of logic_type_info
  | PackageName of package_info

let rec add_in_package_list pi l =
  match l with
    | [] -> [pi]
    | qi::_ when pi.package_info_tag = qi.package_info_tag -> l
    | qi::r -> qi::add_in_package_list pi r

let is_boolean t =
  match t with
    | JTYbase Tboolean -> true
    | _ -> false

let is_reference_type t =
  match t with
    | JTYbase t -> false
    | _ -> true

(* logic funs *)

type logic_body =
  | JAssertion of assertion
  | JTerm of term
  | JReads of term list

let logics_table = Hashtbl.create 97
let logics_env = Hashtbl.create 97

let logic_tag_counter = ref 0

(* JLS 5.1.1: Identity Conversion *)
let rec is_identity_convertible tfrom tto =
  match tfrom, tto with
    | JTYbase t1, JTYbase t2 -> t1=t2
    | JTYclass(_,c1), JTYclass(_,c2) -> c1 == c2
    | JTYinterface i1, JTYinterface i2 -> i1 == i2
    | JTYarray (_, t1), JTYarray (_, t2) -> is_identity_convertible t1 t2
    | JTYnull, JTYnull -> true
    | _ -> false

(* JLS 5.1.2: Widening Primitive Conversion *)
let is_widening_primitive_convertible tfrom tto =
  match tfrom,tto with
    | Tbyte, (Tshort | Tint | Tlong | Tfloat | Tdouble) -> true
    | Tshort, (Tint | Tlong | Tfloat | Tdouble) -> true
    | Tchar, (Tint | Tlong | Tfloat | Tdouble) -> true
    | Tint, (Tlong | Tfloat | Tdouble) -> true
    | Tlong, (Tfloat | Tdouble) -> true
    | Tfloat, Tdouble -> true
    | _ -> false

let is_logic_widening_primitive_convertible tfrom tto =
  match tfrom,tto with
    | Tbyte, (Tshort | Tint | Tlong | Tinteger | Tfloat | Tdouble | Treal) 
	-> true
    | Tshort, (Tint | Tlong | Tinteger | Tfloat | Tdouble | Treal) -> true
    | Tchar, (Tint | Tlong | Tinteger | Tfloat | Tdouble | Treal) -> true
    | Tint, (Tlong | Tinteger | Tfloat | Tdouble | Treal) -> true
    | Tlong, (Tinteger | Tfloat | Tdouble | Treal) -> true
    | Tfloat, (Tdouble | Treal) -> true
    | Tinteger, Treal -> true
    | Tdouble, Treal -> true
    | _ -> false

let logic_info id ty labels pars =
  incr logic_tag_counter;
  { java_logic_info_tag = !logic_tag_counter;
    java_logic_info_name = id;
    java_logic_info_labels = labels;
    java_logic_info_parameters = pars;
    java_logic_info_result_type = ty;
    java_logic_info_calls = [];
  }
    
let logic_unary_numeric_promotion t =
  match t with
    | JTYbase t -> 
	begin
	  match t with
	    | Treal | Tdouble | Tfloat -> Treal
	    | _ -> Tinteger		
	end
    | _ -> assert false

let logic_binary_numeric_promotion t1 t2 =
  match t1,t2 with
    | JTYbase t1,JTYbase t2 -> 
	begin
	  match t1,t2 with
	    | (Tboolean | Tunit),_
	    | _, (Tboolean | Tunit) -> raise Not_found
	    | (Treal | Tdouble | Tfloat),_ 
	    | _, (Treal | Tdouble | Tfloat) -> Treal
	    | _ -> Tinteger		
	end
    | _ -> raise Not_found

let make_logic_bin_op loc op t1 e1 t2 e2 =
  match op with
    | Bgt | Blt | Bge | Ble | Beq | Bne ->
	assert false (* TODO *)
    | Basr | Blsr | Blsl ->
	begin
	  try
	    match logic_unary_numeric_promotion t1 with
	      | Tinteger as t1 ->
		  begin
		    try
		      match logic_unary_numeric_promotion t2 with
			| Tinteger ->
			    JTYbase t1, JTbin (e1, t1, op, e2)
			| _ -> raise Not_found
		    with Not_found -> int_expected loc t2
		  end
	      | _ -> raise Not_found
	  with Not_found -> int_expected loc t1
	end
    | Bimpl | Bor | Band | Biff ->
	assert false (* TODO *)
    | Bbwxor | Bbwor | Bbwand -> 	
	if is_boolean t1 && is_boolean t2 then
	  boolean_type, JTbin (e1, Tboolean, op, e2)
	else
	  begin
	    try
	      let t1 = logic_unary_numeric_promotion t1 in
	      let t2 = logic_unary_numeric_promotion t2 in
		assert (t1 = t2);
		JTYbase t1, JTbin (e1, t1, op, e2)
	    with Not_found ->
	      typing_error loc "booleans or integers expected"
	  end	  
    | Bsub | Badd | Bmod | Bdiv | Bmul ->
	try
	  let t = logic_binary_numeric_promotion t1 t2 in
	    JTYbase t, JTbin (e1, t, op, e2)
	with Not_found ->
	  typing_error loc "numeric types expected for +,-,*, / and %%"
	    
let make_logic_un_op loc op t1 e1 = 
  match op with
    | Uplus ->
	begin
	  try 
	    let t = logic_unary_numeric_promotion t1 in
	    JTYbase t, e1.java_term_node
	  with Not_found ->
	    typing_error loc "numeric type expected for unary +"
	end
    | Uminus ->
	begin
	  try
	    let t = logic_unary_numeric_promotion t1 in
	    JTYbase t, JTun (t, op, e1)
	  with Not_found ->
	    typing_error loc "numeric type expected for unary -"
	end
    | Ucompl ->
	assert false (*TODO*)
    | Unot ->
	if is_boolean t1 then t1, JTun (Tboolean, op, e1)
	else typing_error loc "boolean type expected for unary !"

let make_predicate_bin_op loc op t1 e1 t2 e2 =
  match op with
    | Bgt | Blt | Bge | Ble ->
	begin
	  try 
	    let t = logic_binary_numeric_promotion t1 t2 in
	    JAbin(e1,t,op,e2)
	  with Not_found ->
	    typing_error loc "numeric types expected for >,<,>= and <="
	end
    | Beq | Bne ->
	begin
	  try
	    let t = logic_binary_numeric_promotion t1 t2 in
	    JAbin(e1,t,op,e2)
	  with Not_found -> 
	    if is_reference_type t1 && is_reference_type t2 then
	      JAbin_obj(e1,op,e2)
	    else
	      typing_error loc "numeric or object types expected for == and !="
	end
    | Basr|Blsr|Blsl|Bbwxor|Bbwor|Bbwand|Bimpl|Bor|Band|Biff ->
	assert false (* TODO *)
    | Bsub | Badd | Bmod | Bdiv | Bmul ->
	typing_error loc "operator +,-,*, / and %% is not a predicate"

let connective a1 op a2 =
  match op with
    | Band -> JAand(a1,a2)
    | Bor -> JAor(a1,a2)
    | Bimpl -> JAimpl(a1,a2)
    | Biff -> JAiff(a1,a2)
    | _ -> assert false

let dummy_class =
  { class_info_tag = (-1);
    class_info_name = "";
    class_info_package_env = [];
    class_info_incomplete = false;
    class_info_extends = None;
    class_info_is_exception = false;
    class_info_implements = [];
    class_info_fields = [];
    class_info_methods = [];
    class_info_constructors = [];
  }

let object_class = ref dummy_class


let rec is_subclass c1 c2 =
  c1 == c2 ||
    (check_if_class_complete c1;
     match c1.class_info_extends with
      | None -> c2 == !object_class
      | Some c -> is_subclass c c2)

and is_subinterface i1 i2 =
  i1 == i2 ||
    (check_if_interface_complete i1;
     List.exists
	(fun i' -> 
(*
	   eprintf "checking if interface '%s' is subinterface of '%s'@."
		i'.interface_info_name i2.interface_info_name;
*)
       is_subinterface i' i2)
    i1.interface_info_extends)

and implements c i =
  List.exists 
    (fun i' -> is_subinterface i' i)
    c.class_info_implements ||
    match c.class_info_extends with
      | None -> false
      | Some c -> implements c i

and get_this loc env =
  try 
    List.assoc "this" env 
  with Not_found -> 
    typing_error loc "this undefined in this context"

and get_import (packages, types) imp =
  match imp with
    | Import_package qid ->
	Java_options.lprintf "importing package %a@." print_qualified_ident qid;
	begin
	  match classify_name [] [] None [] qid with
	    | PackageName pi -> (add_in_package_list pi packages,types)
	    | _ -> typing_error (fst (List.hd qid))
		"package name expected"
	end
    | Import_class_or_interface qid ->
	Java_options.lprintf "importing %a@." print_qualified_ident qid;
	begin
	  match classify_name [] [] None [] qid with
	    | TypeName ti -> (packages, (snd (List.hd qid), ti)::types)
	    | _ -> typing_error (fst (List.hd qid))
		"type name expected"
	end

and get_types package_env cus =
  let pil =
    List.map 
      (fun cu ->
	 match cu.cu_package with
	   | [] -> anonymous_package
	   | qid ->
	       match classify_name package_env [] None [] qid with
		 | PackageName pi -> pi
		 | _ -> assert false)
      cus
  in
  let package_env, type_env = 
    let imports = List.flatten (List.map (fun cu -> cu.cu_imports) cus) in
    List.fold_left get_import ([],[])
      (Import_package javalang_qid::imports) 
  in
  let package_env = 
    List.fold_left (fun acc pi -> add_in_package_list pi package_env) 
      package_env pil 
  in
  let local_type_env =
    List.flatten
    (List.map 
      (fun (pi, cu) -> 
	 List.fold_left (get_type_decl pi package_env) 
	   [] cu.cu_type_decls)
      (List.combine pil cus))

  in
  let full_type_env = local_type_env @ type_env in
  List.iter
    (fun (_,ti) ->
       match ti with
	 | TypeClass ci ->
(*
	     eprintf "setting type env for class '%s' as:@\n" ci.class_info_name;
	     List.iter
	       (fun (id,_) -> eprintf "  %s@\n" id)
	       type_env;
*)
	     Hashtbl.add class_type_env_table ci.class_info_tag 
	       full_type_env
	 | TypeInterface ii ->
	     Hashtbl.add interface_type_env_table ii.interface_info_tag 
	       full_type_env)
    local_type_env;
  package_env,local_type_env


(* corresponds to JLS 2nd ed., par. 6.5.2, pp. 96--97 *)
and classify_name 
    (package_env : package_info list)
    (type_env : (string * java_type_info) list)
    (current_type : java_type_info option)
    (local_env : (string * java_var_info) list)
    (name : qualified_ident) =
  match name with
    | [] -> assert false
    | [(loc, id)] ->
	(* case of a simple name (JLS p 96) *)
	begin
	  (* look for a local var of that name *)
	  try
	    let vi = List.assoc id local_env in
	      TermName { 
		java_term_node = JTvar vi; 
		java_term_type = vi.java_var_info_type;
		java_term_loc = loc 
	      }
	  with Not_found -> 
	    (* look for a field of that name in current class *)
	    try
	      match current_type with
		| None -> raise Not_found
		| Some ti ->
		    let fi = lookup_field ti id in
		    let facc =
		      if fi.java_field_info_is_static then
			JTstatic_field_access (ti, fi)
		      else
			let vi = 
			  try
			    List.assoc "this" local_env 
			  with Not_found -> assert false
			in
			let this =
			  { java_term_node = JTvar vi;
			    java_term_type = vi.java_var_info_type;
			    java_term_loc = loc }
			in		
			JTfield_access (this, fi)
		    in
		    TermName {
		      java_term_node = facc;
		      java_term_type = fi.java_field_info_type;
		      java_term_loc = loc
		    }
	    with Not_found ->
	      try
		(* TODO: look for a local class of member type of that name *)
		raise Not_found
	      with
		  Not_found ->
		    try 
		      (* look for a type of that name 
			 in the current compilation unit, or
			 in another compilation unit of the current package *)
(*
		      eprintf "lookup id '%s' in type_env:@\n" id;
		      List.iter
			(fun (id,_) -> eprintf "  %s@\n" id)
			type_env;
*)
		      let ti = List.assoc id type_env in
			TypeName ti 
		    with Not_found ->
		      (* look for a type of that name 
			 declared by a type-import-on-demand declaration 
			 (fail if two of them are visible) *)
		      let l =
			List.fold_left
			  (fun acc pi ->
			     let h = get_package_contents pi in
			       try 
				 (pi, h, (Hashtbl.find h id)) :: acc
			       with Not_found -> acc)
			  [] package_env
		      in
			match l with
			  | [(pi, h, x)] ->
			      begin
				match x with
				  | Subpackage pi -> PackageName pi
				  | Type ti -> TypeName ti
				  | File f -> 
				      let ast = Java_syntax.file f in
				      let (_, t) = get_types package_env [ast] in
					try
					  let ti = List.assoc id t in
					    Hashtbl.replace h id (Type ti);
					    TypeName ti
					with Not_found -> 
					  eprintf "Anomaly: `%s' not found" id; 
					  assert false
			      end
			| (pi1,_,_)::(pi2,_,_)::_ ->
			    typing_error loc 
			      "ambiguous name from import-on-demand packages '%s' and '%s'"
			      pi1.package_info_name 
			      pi2.package_info_name
			| [] ->
			    (* otherwise look for a toplevel package 
			       of that name *)
			    try
			      match Hashtbl.find toplevel_packages id with
				| Subpackage pi -> PackageName pi 
				| Type ti -> assert false 
				| File f -> 
				    eprintf "Internal error: got file %s in place of a package@." f;
				    assert false
			    with Not_found ->
			      (* otherwise look for a logic type 
				 of that name *)
			      try
				let i = Hashtbl.find logic_types_table id in
				LogicTypeName i
			      with Not_found ->
				typing_error loc "unknown identifier %s" id
			      	  
	end		
	  
    | (loc,id)::n ->
	(* case of a qualified name (JLS p 97) *)
	match classify_name package_env type_env current_type local_env n with
	  | PackageName pi -> 
	      let contents = get_package_contents pi in
	      begin
		try
		  match Hashtbl.find contents id with
		    | Subpackage pi -> PackageName pi
		    | Type ti -> TypeName ti
		    | File f -> 
			let ast = Java_syntax.file f in
			let (_,t) = get_types package_env [ast] in
			try
			  let ti = List.assoc id t in
			  Hashtbl.replace contents id (Type ti);
			  TypeName ti
			with Not_found -> 
			  typing_error loc "type `%s' not found in file `%s'@."
			    id f
		with
		    Not_found ->
		      typing_error loc "unknown identifier %s" id
	      end
	  | TypeName ci ->
	      begin
  		try
		  let fi = lookup_field ci id in
		  if fi.java_field_info_is_static then
		    TermName { 
		      java_term_loc = loc;
		      java_term_type = fi.java_field_info_type ;
		      java_term_node = JTstatic_field_access(ci,fi)
		    }
		  else
		    typing_error loc 
		      "field %s is not static" id

		with Not_found ->
		  (* TODO: look for a method of that name ? *)
		  (* TODO: look for a member type of that name *)
		  typing_error loc 
		    "no such field in %a" print_type_name ci
	      end
	  | TermName t -> 
	      type_term_field_access t loc id 
	  | LogicTypeName i -> 
	      typing_error loc "logic type unexpected"

and type_term_field_access t loc id = 
  match t.java_term_type with
    | JTYclass(_,c) ->
	begin
	  try
	    let fi = lookup_class_field c id in
	    TermName { 
	      java_term_loc = loc;
	      java_term_type = fi.java_field_info_type ;
	      java_term_node = JTfield_access(t,fi)
	    }
	  with Not_found ->
	    typing_error loc 
	      "no such field in class %s" c.class_info_name
	end
    | JTYinterface ii ->
	assert false (* TODO *)
    | JTYarray _ -> 
	if id="length" then
	  TermName {
	    java_term_loc = loc;
	    java_term_type = integer_type;
	    java_term_node = JTarray_length(t)
	  }
	else
	  typing_error loc 
	    "no such field in array type"
    | JTYnull | JTYbase _ | JTYlogic _ ->
	class_or_interface_expected t.java_term_loc 
	  t.java_term_type

and type_expr_field_access e loc id = 
  match e.java_expr_type with
    | JTYclass(_,c) ->
	begin
	  try
	    let fi = lookup_class_field c id in
	    { 
	      java_expr_loc = loc;
	      java_expr_type = fi.java_field_info_type ;
	      java_expr_node = JEfield_access(e,fi)
	    }
	  with Not_found ->
	    typing_error loc 
	      "no such field in class %s" c.class_info_name
	end
    | JTYinterface ii ->
	assert false (* TODO *)
    | JTYarray _ -> 
	if id="length" then
	  {
	    java_expr_loc = loc;
	    java_expr_type = int_type;
	    java_expr_node = JEarray_length(e)
	  }
	else
	  typing_error loc 
	    "no such field in array type"
    | JTYnull | JTYbase _ | JTYlogic _ ->
	class_or_interface_expected e.java_expr_loc 
	  e.java_expr_type

and type_type package_env type_env non_null ty =
  match ty with
    | Base_type t -> JTYbase t
    | Type_name qid -> 
	begin
	  match classify_name package_env type_env None [] qid with
	    | TypeName ti ->
		begin
		  match ti with
		    | TypeClass ci -> JTYclass (non_null, ci)
		    | TypeInterface ii -> JTYinterface(ii)
		end
	    | LogicTypeName i -> JTYlogic i
	    | _ -> assert false (* TODO *)
	end
    | Array_type_expr t -> 
	let ty = type_type package_env type_env non_null t in
	JTYarray (non_null, ty)

and get_field_prototypes package_env type_env ci acc d =
  match d with
    | JPFvariable vd -> 
	(*
	  vd.variable_modifiers : modifiers ;
	  vd.variable_type : type_expr ;
	  vd.variable_decls : variable_declarator list }
	*)
	let is_non_null = List.mem Non_null vd.variable_modifiers in
	let is_nullable = List.mem Nullable vd.variable_modifiers in
	let non_null = 
	  if !Java_options.nonnull_sem = NonNullNone then is_non_null else
	    not is_nullable
	in
	let ty = type_type package_env type_env non_null vd.variable_type in
	let ty = match ty, ci with
	  | JTYclass (true, ci'), TypeClass ci
	      when is_subclass ci ci' || is_subclass ci' ci ->
	      JTYclass (false, ci')
	  | _ -> ty
	in
	let is_static = List.mem Static vd.variable_modifiers in
	let is_final = List.mem Final vd.variable_modifiers in
	let is_ghost = List.mem Ghost vd.variable_modifiers in
	let is_model = List.mem Model vd.variable_modifiers in
	List.fold_left
	  (fun acc vd -> 
	     let ty', (loc, id) = var_type_and_id non_null ty vd.variable_id in
	       if is_non_null && !Java_options.nonnull_sem <> NonNullNone then
		 typing_error loc 
		   "'non_null' modifier is not allowed in 'non_null by default' mode";	    
	       if is_nullable && !Java_options.nonnull_sem = NonNullNone then
		 typing_error loc 
		   "'nullable' modifier is only allowed in 'non_null by default' mode";	    
	     let fi = new_field ~is_static ~is_final ~is_nullable 
	       ~is_model ~is_ghost ci ty' id in	     
	     Hashtbl.add field_prototypes_table fi.java_field_info_tag 
	       vd.variable_initializer;
	     fi::acc
	  ) acc vd.variable_decls
    | _ -> acc

and type_param package_env type_env p =
  let nullable = ref false in
  let rec get_type p =
    match p with
      | Simple_parameter (mo, ty, (loc, id)) ->
	  (match mo with
	     | None -> 
		 nullable := 
		   !Java_options.nonnull_sem <> NonNullAll && 
		     !Java_options.nonnull_sem <> NonNullAllLocal;
	     | Some Non_null ->
		 nullable := false;
		 if !Java_options.nonnull_sem = NonNullAll ||
		   !Java_options.nonnull_sem = NonNullAllLocal then
		     typing_error loc
		       "'non_null' modifier is not allowed in 'non_null by default' mode";
	     | Some Nullable -> 
		 nullable := true;
		 if !Java_options.nonnull_sem <> NonNullAll &&
		   !Java_options.nonnull_sem <> NonNullAllLocal then
		     typing_error loc 
		       "'nullable' modifier is only allowed in 'non_null by default' mode";
	     | _ -> assert false);
	  let non_null = not !nullable in
	    (non_null, type_type package_env type_env non_null ty, loc, id)
      | Array_parameter x -> 
	  let (non_null, t, loc, i) = get_type x in
	    non_null, JTYarray (non_null, t), loc, i
  in
  let (_, t,loc,i) = get_type p in (new_var loc t i, !nullable)

and method_header package_env type_env modifiers retty mdecl =
  match mdecl with
    | Simple_method_declarator (id, l) ->
	let is_nullable = List.mem Nullable modifiers in
	let non_null =
	  (!Java_options.nonnull_sem = NonNullAll ||
	      !Java_options.nonnull_sem = NonNullAllLocal) &&
	    not is_nullable
	in
	  non_null, id, (Option_misc.map (type_type package_env type_env non_null) retty), 
	List.map (type_param package_env type_env) l
    | Array_method_declarator d -> 
	let non_null, id, t, l = 
	  method_header package_env type_env modifiers retty d 
	in
	  match t with
	    | Some t -> non_null, id, Some (JTYarray (non_null, t)),l
	    | None -> typing_error (fst id) "invalid type void array"
		
and get_constructor_prototype 
    package_env type_env current_type req decreases behs head eci body =
  match current_type with
    | TypeInterface _ -> assert false
    | TypeClass cur ->
	let loc, id = head.constr_name in
	let params = 
	  List.map (type_param package_env type_env) 
	    head.constr_parameters 
	in
	let ci = new_constructor_info cur loc params in
	  Hashtbl.add constructors_env ci.constr_info_tag 
	    (ci,req,decreases,behs,eci,body);
	  ci

and get_method_prototypes package_env type_env current_type (mis,cis) env l =
  match l with
    | [] -> (mis,cis)
    | JPFmethod(head,body) :: rem -> 
	let non_null, (loc,id), ret_ty, params = 
	  method_header package_env type_env 
	    head.method_modifiers head.method_return_type head.method_declarator 
	in
	let is_static = List.mem Static head.method_modifiers in
	let result_is_nullable = not non_null in
	let mi = new_method_info 
	  ~is_static ~result_is_nullable loc id current_type ret_ty params 
	in
	Hashtbl.add methods_env mi.method_info_tag (mi,None,None,[],body);
	get_method_prototypes package_env type_env 
	  current_type (mi::mis,cis) env rem 
    | JPFmethod_spec(req,decreases,behs) :: JPFmethod(head,body) :: rem ->
	let non_null, (loc,id), ret_ty, params = 
	  method_header package_env type_env 
	    head.method_modifiers head.method_return_type head.method_declarator 
	in
	let is_static = List.mem Static head.method_modifiers in
	let result_is_nullable = not non_null in
	let mi = new_method_info 
	  ~is_static ~result_is_nullable loc id current_type ret_ty params 
	in
	Hashtbl.add methods_env mi.method_info_tag 
	  (mi,req,decreases,behs,body);
	get_method_prototypes package_env type_env 
	  current_type (mi::mis,cis) env rem 
    | JPFconstructor(head,eci,body) :: rem -> 
	let ci =
	  get_constructor_prototype package_env type_env current_type 
	    None None [] head eci body
	in
	get_method_prototypes package_env type_env 
	  current_type (mis,ci::cis) env rem 
    | JPFmethod_spec(req,decreases,behs) :: JPFconstructor(head,eci,body) :: rem ->
	let ci =
	  get_constructor_prototype package_env type_env current_type 
	    req decreases behs head eci body
	in
	get_method_prototypes package_env type_env 
	  current_type (mis,ci::cis) env rem 
    | JPFmethod_spec _ :: _ ->
	typing_error Loc.dummy_position "out of place method specification"
    | JPFinvariant (id, e) :: rem ->
	get_method_prototypes package_env type_env
	  current_type (mis, cis) env rem 
    | JPFstatic_invariant (id, e) :: rem ->
	get_method_prototypes package_env type_env
	  current_type (mis, cis) env rem 
    | JPFannot _ :: _ -> assert false (* not possible after 2nd parsing *)
    | JPFstatic_initializer _ ::rem -> 
	(* TODO ? *)
	get_method_prototypes package_env type_env 
	  current_type (mis,cis) env rem 	
    | JPFvariable _ :: rem -> 
	get_method_prototypes package_env type_env 
	  current_type (mis,cis) env rem 
    | JPFclass _ :: rem -> (* TODO *)
	get_method_prototypes package_env type_env 
	  current_type (mis,cis) env rem 
    | JPFinterface _ :: rem -> (* TODO *)
	get_method_prototypes package_env type_env 
	  current_type (mis,cis) env rem 

and get_class_prototypes package_env type_env ci d =
  (* extends *)
  ci.class_info_extends <-
    Option_misc.map 
    (fun id -> 
       match classify_name package_env type_env None [] id with
	 | TypeName (TypeClass super) -> 
	     check_if_class_complete super;
	     Java_options.lprintf "Class %s extends class %s@." 
	       ci.class_info_name
	       super.class_info_name;
	     if super.class_info_is_exception then
	       begin
		 Java_options.lprintf "Class %s is an exception class@." 
		   ci.class_info_name;
		 ci.class_info_is_exception <- true
	       end;
	     super
	 | _ ->
	     typing_error (fst (List.hd id)) "class type expected") 
    d.class_extends;
  (* implements *)
  ci.class_info_implements <-
    List.map 
    (fun id -> 
       match classify_name package_env type_env None [] id with
	 | TypeName (TypeInterface super) -> 
	     check_if_interface_complete super;
	     super
	 | _ ->
	     typing_error (fst (List.hd id)) "interface type expected") 
    d.class_implements;
  (* fields *)
  let fields = List.fold_left 
    (get_field_prototypes package_env type_env (TypeClass ci)) [] d.class_fields in
    ci.class_info_fields <- List.rev fields;
    let methods, constructors =
      get_method_prototypes package_env type_env (TypeClass ci) ([],[]) [] d.class_fields 
    in
    let constructors = 
      (* if no constructor explicitly declared, 
	 then there is always an implicit default constructor *)
      if constructors <> [] then constructors else
	let default_constructor = new_constructor_info ci Loc.dummy_position [] in
	  Hashtbl.add constructors_env default_constructor.constr_info_tag 
	    (default_constructor, None, None, [], Invoke_none, []);
	[default_constructor]
    in
      ci.class_info_methods <- methods;
      ci.class_info_constructors <- constructors;
      (* invariants *)
      let this_type = JTYclass (true, ci) (* i.e. [this] is always non-null *) in
      let vi = new_var Loc.dummy_position this_type "this" in 
      let invs, static_invs = 
	List.fold_left
	  (fun (acc1, acc2) d -> 
	     match d with
	       | JPFinvariant (id, e) -> (id, e) :: acc1, acc2
	       | JPFstatic_invariant (id, e) -> acc1, (snd id, e) :: acc2
	       | _ -> acc1, acc2) ([], []) d.class_fields 
      in
	Hashtbl.add invariants_env
	  ci.class_info_tag (TypeClass ci, ["this", vi], vi, invs);
	Hashtbl.add static_invariants_env
	  ci.class_info_tag (TypeClass ci, static_invs)
	  
and get_interface_field_prototypes package_env type_env ii acc d =
  match d with
    | JPFvariable vd -> 
	let is_model = List.mem Model vd.variable_modifiers in
	let ty = type_type package_env type_env true vd.variable_type in
	  List.fold_left
	    (fun acc vd -> 
	       let ty',(loc,id) = var_type_and_id false ty vd.variable_id in
	       (* Note: no need to check if it is static and final, because 
		  it is implicitly the case (JLS,9.3, p 203) *)
	       let fi = 
		 if is_model then
		   new_field ~is_static:false ~is_final:false ~is_nullable:true
		     ~is_model:true ~is_ghost:false
		     (TypeInterface ii) ty' id 
		 else
		   new_field ~is_static:true ~is_final:true ~is_nullable:true 
		     ~is_model:false ~is_ghost:false
		     (TypeInterface ii) ty' id 
	       in
		 Hashtbl.add field_prototypes_table fi.java_field_info_tag 
		   vd.variable_initializer;
		 fi::acc
	    ) acc vd.variable_decls
    | _ -> acc
	
and get_interface_prototypes package_env type_env ii d =
  (* extends *)
  ii.interface_info_extends <-
    List.map 
    (fun id -> 
       match classify_name package_env type_env None [] id with
	 | TypeName (TypeInterface super) -> 
	     super
	 | _ ->
	     typing_error (fst (List.hd id)) "interface type expected") 
    d.interface_extends;
  (* fields *)
  let fields = 
    List.fold_left (get_interface_field_prototypes package_env type_env ii) [] 
      d.interface_members
  in
  ii.interface_info_fields <- fields;
  let methods,constructors = 
    get_method_prototypes  package_env type_env (TypeInterface ii) ([],[]) [] d.interface_members
  in
  ii.interface_info_methods <- methods 

and check_if_interface_complete ii =
  if ii.interface_info_incomplete then
    begin
      (* get interface decls prototypes *)
      let (p,d) = Hashtbl.find interface_decl_table ii.interface_info_tag in
      let t = Hashtbl.find interface_type_env_table ii.interface_info_tag in
      get_interface_prototypes p t ii d;
      ii.interface_info_incomplete <- false;    
    end;

and lookup_interface_field ii id =
  check_if_interface_complete ii;
  try
    list_assoc_name (fun fi -> fi.java_field_info_name) 
      id ii.interface_info_fields
  with
      Not_found ->
	match ii.interface_info_extends with
	  | [] -> raise Not_found
	  | [ii] -> lookup_interface_field ii id
	  | _ -> assert false (* TODO *)

and check_if_class_complete ci =
  if ci.class_info_incomplete then
    begin
      (* get class decls prototypes *)
      let (p, d) = Hashtbl.find class_decl_table ci.class_info_tag in
      let t = Hashtbl.find class_type_env_table ci.class_info_tag in
      ci.class_info_incomplete <- false;
      get_class_prototypes p t ci d;
    end;

and lookup_class_field ci id =
  check_if_class_complete ci;
  try
    list_assoc_name (fun fi -> fi.java_field_info_name) id ci.class_info_fields
  with
      Not_found ->
	try
	  match ci.class_info_extends with
	    | None -> raise Not_found
	    | Some ci -> lookup_class_field ci id
	with
	    Not_found ->
	      match ci.class_info_implements with
		| [] -> raise Not_found
		| [ii] -> lookup_interface_field ii id
		| _ -> assert false (* TODO *)
	      
	      
and lookup_field ti id =
  match ti with
    | TypeClass ci -> lookup_class_field ci id
    | TypeInterface ii -> lookup_interface_field ii id

let () = 
  object_class :=
    catch_typing_errors
      (fun () ->
	 match classify_name [] [] None [] 
	   ((Loc.dummy_position,"Object") :: javalang_qid)
	 with
	   | TypeName (TypeClass ci) -> ci
	   | _ -> assert false)
      ()


    
let string_class =
  if !Java_options.javacard then dummy_class else
    catch_typing_errors
      (fun () ->
	 match classify_name [] [] None [] 
	   ((Loc.dummy_position,"String") :: javalang_qid)
	 with
	   | TypeName (TypeClass ci) -> ci
	   | _ -> assert false)
      ()

let string_type ~valid = JTYclass(valid,string_class)


type typing_env =
    {
      package_env : Java_env.package_info list;
      type_env : (string * Java_env.java_type_info) list;
      current_type : Java_env.java_type_info option;
      label_env : logic_label list;
      env : (string * Java_env.java_var_info) list;
    }

(* JLS 5.1.4: Widening Reference Conversion *)
let rec is_widening_reference_convertible tfrom tto =
  match tfrom,tto with
    | JTYclass(_,c1), JTYclass(_,c2) -> is_subclass c1 c2
    | JTYclass(_,c), JTYinterface i -> implements c i
    | JTYnull, (JTYclass _ | JTYinterface _ | JTYarray _ ) -> true
    | JTYinterface i1, JTYinterface i2 -> is_subinterface i1 i2
    | JTYinterface _ , JTYclass(_,c) when c == !object_class -> true
    | JTYarray _ , JTYclass(_,c) when  c== !object_class -> true
(* TODO
    | JTYarray _ , JTYinterface i when i==cloneable_interface -> true
    | JTYarray _ , JTYinterface i when i==serializable_interface -> true
*)
    | JTYarray (_, t1), JTYarray (_, t2) -> 
	is_widening_reference_convertible t1 t2
    | _ -> false

and is_logic_call_convertible tfrom tto =
  is_identity_convertible tfrom tto ||
  match tfrom,tto with
    | JTYbase t1, JTYbase t2 -> is_logic_widening_primitive_convertible t1 t2
    | JTYlogic s1, JTYlogic s2 -> s1==s2
    | _ -> is_widening_reference_convertible tfrom tto

and term env current_label e =
  let termt = term env current_label in
  let ty,tt =
    match e.java_pexpr_node with
      | JPElit l -> 
	  let ty,l = 
	    match l with
	      | Integer s -> integer_type,l
	      | Char s -> assert false (* TODO *)
	      | String s -> assert false (* TODO *)
	      | Bool b -> boolean_type,l
	      | Float s -> real_type,l
	      | Null -> null_type,l
	  in
	  ty,(JTlit l)
      | JPEname n ->
	  begin
	    match classify_name env.package_env env.type_env env.current_type env.env n with
	      | TermName t ->
		  t.java_term_type, t.java_term_node
	      | TypeName _ ->
		  typing_error e.java_pexpr_loc
		    "term expected, got a class or interface"
	      | PackageName _ ->
		  typing_error e.java_pexpr_loc
		    "term expected, got a package name"
	      | LogicTypeName _ ->
		  typing_error e.java_pexpr_loc
		    "term expected, got a logic type"
	  end

      | JPEresult -> 
	  begin
	    try
	      let vi = List.assoc "\\result" env.env in
	      vi.java_var_info_type,JTvar vi
	    with Not_found -> 
	      typing_error e.java_pexpr_loc "\\result undefined in this context"
	  end
      | JPEthis -> 
	  let vi = get_this e.java_pexpr_loc env.env in
	  vi.java_var_info_type, JTvar vi
      | JPEbin (e1, op, e2) -> 
	  let te1 = termt e1 and te2 = termt e2 in 
	  make_logic_bin_op e.java_pexpr_loc op 
	    te1.java_term_type te1 te2.java_term_type te2
      | JPEquantifier (q, idl, e)-> 
	  typing_error e.java_pexpr_loc
	    "quantified formulas not allowed in term position"
      | JPEold e1 -> 
	  (* TODO : check label Old exists *)
	  let te1 = termt e1 in 
	  te1.java_term_type,JTat(te1,LabelOld)
      | JPEat(e1,lab) -> 
	  let te1 = termt e1 in 
	  (* TODO : check label exists *)
	  te1.java_term_type,JTat(te1,LabelName (snd lab))	
      | JPEinstanceof (_, _)-> assert false (* TODO *)
      | JPEcast (t, e1)-> 
	  let te1 = termt e1 in
	  let ty = type_type env.package_env env.type_env false t in
	  (* TODO: check if cast allowed *)
	  ty,JTcast(ty,te1)
      | JPEarray_access (e1, e2)-> 
	  let te1 = termt e1 and te2 = termt e2 in 
	  begin
	    match te1.java_term_type with
	      | JTYarray (_, t) ->
		  begin
		    try 
		      match 
			logic_unary_numeric_promotion te2.java_term_type 
		      with 
			| Tinteger -> t, JTarray_access(te1,te2)
			| _ -> raise Not_found
		    with Not_found ->
		      integer_expected e2.java_pexpr_loc te2.java_term_type
		  end	
	      | _ ->
		  array_expected e1.java_pexpr_loc te1.java_term_type
	  end
      | JPEarray_range (e1, e2, e3)->
	  let te1 = termt e1 in 
	  begin
	    match te1.java_term_type with
	      | JTYarray (_, t) ->
		  let te2 = Option_misc.map (index env current_label) e2 in
		  let te3 = Option_misc.map (index env current_label) e3 in
		  t, JTarray_range(te1,te2,te3)
	      | _ ->
		  array_expected e1.java_pexpr_loc te1.java_term_type
	  end
      | JPEnew_array _-> assert false (* TODO *)
      | JPEnew (_, _)-> assert false (* TODO *)
      | JPEsuper_call (_, _)-> assert false (* TODO *)
      | JPEcall_name (qid, args)-> 
	  begin
	    match qid with
	      | [(loc,id)] -> 
		  begin 
		    try 
		      let fi = Hashtbl.find logics_env id in
		      let args = List.map termt args in		      
		      (* TODO: check arg types *)
		      match fi.java_logic_info_result_type with
			| None ->
			    typing_error loc 
			      "logic symbol `%s' is a predicate" id
			| Some t -> t,JTapp(fi,args)
		    with Not_found ->
		      typing_error loc "logic function `%s' not found" id
		  end		
	      | _ -> 
		  typing_error e.java_pexpr_loc "method calls not allowed in annotations"
	  end
      | JPEcall_expr _ -> 
	  typing_error e.java_pexpr_loc 
	    "method calls not allowed in annotations"
      | JPEfield_access fa -> 
	  begin
	    match fa with
	      | Primary_access (e1, f) -> 
		  let te1 = termt e1 in
		    begin
		      match te1.java_term_type with
			| JTYclass (_, ci) ->
			    begin
			      try
				let fi = lookup_field (TypeClass ci) (snd f) in
				  fi.java_field_info_type,JTfield_access(te1,fi)
			      with
				  Not_found ->
				    typing_error e1.java_pexpr_loc
				      "not such field"
			    end
			| JTYinterface ii ->
			    begin
			      try
				let fi = lookup_field (TypeInterface ii) (snd f) in
				  fi.java_field_info_type,JTfield_access(te1,fi)
			      with
				  Not_found ->
				    typing_error e1.java_pexpr_loc
				      "not such field"
			    end
			| JTYarray _ ->
			    if (snd f) = "length" then
			      integer_type, JTarray_length te1
			    else
			      typing_error e1.java_pexpr_loc
				"not such field"
			| _ ->
			    typing_error e1.java_pexpr_loc
			      "not a class"
		    end
		      
	      | Super_access _ -> assert false (* TODO *)
	  end
      | JPEif (_, _, _)-> assert false (* TODO *)
      | JPEassign_array (_, _, _, _)-> assert false (* TODO *)
      | JPEassign_field (_, _, _)-> assert false (* TODO *)
      | JPEassign_name (_, _, _)-> assert false (* TODO *)
      | JPEincr (_, _)-> assert false (* TODO *)
      | JPEun (op, e1)-> 	 
	  let te1 = termt e1 in 
	  let t,e = make_logic_un_op e.java_pexpr_loc op 
	    te1.java_term_type te1 in
	  t,e

  in { java_term_node = tt; 
       java_term_type = ty;
       java_term_loc = e.java_pexpr_loc }

and index env current_label e =
  let te = term env current_label e in
  match  
    logic_unary_numeric_promotion te.java_term_type 
  with
    | Tinteger -> te
    | _ -> 
	integer_expected e.java_pexpr_loc te.java_term_type

and assertion env current_label e =
  let termt = term env current_label in
  let assertiont = assertion env current_label in
  let ta =
  match e.java_pexpr_node with
    | JPElit (Bool true) -> JAtrue
    | JPElit (Bool false) -> JAfalse
    | JPElit _ -> 
	typing_error e.java_pexpr_loc 
	  "this literal is not a boolean expression"
    | JPEun(Unot, e2) ->
	let te2 = assertiont e2 in JAnot(te2)
    | JPEun (_, _)-> assert false (* TODO *)
    | JPEbin(e1, ((Band | Bor | Bimpl | Biff) as op) , e2) ->
	let te1 = assertiont e1
	and te2 = assertiont e2
	in connective te1 op te2
    | JPEbin
	({ java_pexpr_node = 
	     JPEbin (_, (Beq | Bne | Bgt | Blt | Ble | Bge), a) } as p,
	 (Beq | Bne | Bgt | Blt | Ble | Bge as op), b) ->
	let q = { java_pexpr_loc = fst a.java_pexpr_loc, snd b.java_pexpr_loc ;
		  java_pexpr_node = JPEbin (a, op, b) } in
	  JAand (assertiont p, assertiont q)
    | JPEbin
	(a, (Beq | Bne | Bgt | Blt | Ble | Bge as op),
	({ java_pexpr_node = 
	    JPEbin (b, (Beq | Bne | Bgt | Blt | Ble | Bge), _) } as p)) ->
	let q = { java_pexpr_loc = fst a.java_pexpr_loc, snd b.java_pexpr_loc ;
		  java_pexpr_node = JPEbin (a, op, b) } in
	  JAand (assertiont q, assertiont p)
    | JPEbin(e1, op, e2) -> 
	let te1 = termt e1 and te2 = termt e2 in 
	  make_predicate_bin_op e.java_pexpr_loc op 
	    te1.java_term_type te1 te2.java_term_type te2
    | JPEquantifier (q, idl, e)-> 
	let a = make_quantified_formula 
	  e.java_pexpr_loc q idl env current_label e 
	in
	a.java_assertion_node
    | JPEold a -> 
	  (* TODO : check label Old exists *)
	  let ta = assertiont a in 
	  JAat(ta,LabelOld)
    | JPEat(a,lab) -> 
	let ta = assertiont a in 
	(* TODO : check label exists *)
	JAat(ta,LabelName (snd lab))	
    | JPEinstanceof (e, ty) ->
	begin
	  match current_label with
	    | None ->
		typing_error e.java_pexpr_loc "No memory state for this instanceof (\\at missing ?)"
	    | Some l ->
		let te = termt e and tty = type_type env.package_env env.type_env false ty in
		if is_reference_type tty then JAinstanceof (te, l, tty) else
		  typing_error e.java_pexpr_loc "unexpected type"
	end
    | JPEcast (_, _)-> assert false (* TODO *)
    | JPEarray_access (_, _)-> assert false (* TODO *)
    | JPEarray_range (_, _,_)-> assert false (* TODO *)
    | JPEnew_array _-> assert false (* TODO *)
    | JPEnew (_, _)-> assert false (* TODO *)
    | JPEsuper_call (_, _)-> assert false (* TODO *)
    | JPEcall_name([(loc,id)], args)-> 
	begin
	  try
	    let fi = Hashtbl.find logics_env id in
	    let tl =
	      try
		List.map2
		  (fun vi e ->
		     let ty = vi.java_var_info_type in
		     let te = termt e in
		     if is_logic_call_convertible te.java_term_type ty then te
		     else
		       typing_error e.java_pexpr_loc 
			 "type %a expected, got %a" 
			 print_type ty print_type te.java_term_type) 
		  fi.java_logic_info_parameters args
	      with  Invalid_argument _ ->
		typing_error e.java_pexpr_loc 
		  "wrong number of arguments for %s" id
	    in
	    JAapp(fi, tl)
		 
	  with
	      Not_found ->
		typing_error e.java_pexpr_loc 
		  "unknown predicate `%s'" id
	end
    | JPEcall_name _ | JPEcall_expr _ -> 
	typing_error e.java_pexpr_loc 
	  "method calls not allowed in assertion"	
    | JPEfield_access _-> assert false (* TODO *)
    | JPEif (_, _, _)-> assert false (* TODO *)
    | JPEassign_array (_, _, _, _)-> assert false (* TODO *)
    | JPEassign_field (_, _, _)-> assert false (* TODO *)
    | JPEassign_name (_, _, _)-> assert false (* TODO *)
    | JPEname _-> assert false (* TODO *)
    | JPEincr (_, _)-> assert false (* TODO *)
    | JPEresult -> 
	begin
	  try
	    let vi = List.assoc "\\result" env.env in
	    match vi.java_var_info_type with
	      | JTYbase Tboolean ->
		  JAbool_expr {
		    java_term_node = JTvar vi;
		    java_term_type = vi.java_var_info_type;
		    java_term_loc = e.java_pexpr_loc
		  }
	      | _ ->
		  typing_error e.java_pexpr_loc "\\result is not boolean"

	  with Not_found -> 
	    typing_error e.java_pexpr_loc "\\result undefined in this context"
	end

    | JPEthis -> 
	typing_error e.java_pexpr_loc 
	  "'this' is not a boolean expression"

  in { java_assertion_node = ta;
       java_assertion_loc = e.java_pexpr_loc }

and make_quantified_formula loc q idl env current_label e : assertion =
  match idl with
    | [] -> assertion env current_label e
    | (ty,idl)::r ->	
	let tty = type_type env.package_env env.type_env true ty in
	let env_local =
	  List.map
	    (fun id ->
	       let tyv, (loc,n) = var_type_and_id false tty id in
	       let vi = new_var loc tyv n in
	       (n,vi))
	    idl
	in
	let f = 
	  make_quantified_formula loc q r { env with env = env_local@env.env } 
	    current_label e 
	in
	List.fold_right
	  (fun (_,vi) acc ->
	     { java_assertion_loc = loc ; 
	       java_assertion_node = JAquantifier(q,vi,acc) })
	  env_local f
	


(*

  read the 'Throwable' class to initiate fields
  class_info_is_exception

*)

let () =
  match classify_name [] [] None [] 
    ((Loc.dummy_position,"Throwable") :: javalang_qid)
  with
    | TypeName (TypeClass ci) -> ci.class_info_is_exception <- true
    | _ -> assert false
	
(*******************************

Typing level 2: extract bodies
  (logic functions definitions, axioms,
   field initializers, method and constructors bodies)

**********************************)



let field_initializer_table = Hashtbl.create 17

let axioms_table = Hashtbl.create 17


(* JLS 5.6.1: Unary Numeric Promotion *)
let unary_numeric_promotion t =
  match t with
    | JTYbase t -> 
	begin
	  match t with
	    | Treal | Tinteger -> assert false
	    | Tchar | Tbyte | Tshort -> Tint
	    | _ -> t
	end
    | _ -> assert false

(* JLS 5.6.2: Binary Numeric Promotion *)
let binary_numeric_promotion ~ghost t1 t2 =
  match t1,t2 with
    | JTYbase t1,JTYbase t2 -> 
	begin
	  if ghost then
	    match t1,t2 with
	      | (Tboolean | Tunit),_
	      | _, (Tboolean | Tunit) -> raise Not_found
	      | (Treal | Tdouble | Tfloat), _ 
	      | _, (Treal | Tdouble | Tfloat) -> Treal
	      | _ -> Tinteger
	  else
	    match t1,t2 with
	      | (Tboolean | Tunit),_
	      | _, (Tboolean | Tunit) -> raise Not_found
	      | (Treal | Tinteger), _ 
	      | _, (Treal | Tinteger) -> assert false
	      | Tdouble,_ | _, Tdouble -> Tdouble
	      | Tfloat,_ | _, Tfloat -> Tfloat
	      | Tlong,_ | _, Tlong -> Tlong
	    | (Tshort | Tbyte | Tint | Tchar),
		  (Tshort | Tbyte | Tint | Tchar) -> Tint		
	  end
    | _ -> raise Not_found

let lub_object_types t1 t2 = JTYnull
  

(* JLS 5.1.3: Narrowing Primitive Conversion *)
let is_narrowing_primitive_convertible tfrom tto =
  match tfrom, tto with
    | Tshort, (Tbyte | Tchar) -> true
    | Tchar, (Tbyte | Tshort) -> true
    | Tint, (Tbyte | Tshort | Tchar) -> true
    | Tlong, (Tbyte | Tshort | Tchar | Tint) -> true
    | Tfloat, (Tbyte | Tshort | Tchar | Tint | Tlong)  -> true
    | Tdouble, (Tbyte | Tshort | Tchar | Tint | Tlong | Tfloat)  -> true
    | _ -> false

let narrowing_primitive_conversion n tfrom tto =
  match tfrom, tto with
    | Tshort, (Tbyte | Tchar) -> assert false (* TODO *)
    | Tchar, (Tbyte | Tshort) -> assert false (* TODO *)
    | Tint, Tbyte ->
	let i = Num.int_of_num n in
	let i = i land 0xFF in (* discard all but 8 lower order bits *)
	  (* if i is a byte then i else take the 2 complement of i *)
	let i = if in_byte_range (Num.num_of_int i) then i else
	  let i = (lnot i) land 0xFF in
	  let i = (i + 1) land 0xFF in -i
	in Num.num_of_int i
    | Tint, Tshort ->
	let i = Num.int_of_num n in
	let i = i land 0xFFFF in (* discard all but 16 lower order bits *)
	  (* if i is a short then i else take the 2 complement of i *)
	let i = if in_short_range (Num.num_of_int i) then i else
	  let i = (lnot i) land 0xFFFF in
	  let i = (i + 1) land 0xFFFF in -i
	in Num.num_of_int i
    | Tint, Tchar -> assert false (* TODO *)
    | Tlong, (Tbyte | Tshort | Tchar | Tint) -> assert false (* TODO *)
    | Tfloat, (Tbyte | Tshort | Tchar | Tint | Tlong) -> assert false (* TODO *)
    | Tdouble, (Tbyte | Tshort | Tchar | Tint | Tlong | Tfloat) -> assert false (* TODO *)
    | _ -> assert false (* should never happen *)


(* JLS 5.2: Assignment conversion  *)

let final_field_values_table : (int, Num.num list) Hashtbl.t 
    = Hashtbl.create 97

let bwor_num n1 n2 =
  try
    let n1 = Num.int_of_num n1 in
    let n2 = Num.int_of_num n2 in
    Num.num_of_int (n1 lor n2)
  with
      _ -> assert false

    
let rec lsl_num n1 n2 =
  if n2 = 0 then n1 else lsl_num (Num.add_num n1 n1) (n2-1)

let lsr_num n1 n2 = assert false

let asr_num n1 n2 = assert false


let rec eval_const_expression const e =
  match e.java_expr_node with
    | JElit c ->
	begin
	  match c with
	    | Integer s -> Numconst.integer s
	    | Float _ -> assert false (* TODO *)
	    | Bool false -> Num.Int 0
	    | Bool true -> Num.Int 1
	    | String _ -> assert false (* TODO *)
	    | Char _ -> assert false (* TODO *) 
	    | Null -> assert false (* TODO *)
	end
    | JEcast (ty, e) ->
	let n = eval_const_expression const e in
	  begin
	    match ty with
	      | JTYbase t ->
		  let te = match e.java_expr_type with
		    | JTYbase t -> t | _ -> assert false 
		  in
		    begin
		      match t with
			| Tbyte -> if in_byte_range n then n else
			    if is_narrowing_primitive_convertible te t then
			      narrowing_primitive_conversion n te t
			    else
			      typing_error e.java_expr_loc 
				"outside the byte range: %s" (Num.string_of_num n)
			| Tshort -> if in_short_range n then n else
			    if is_narrowing_primitive_convertible te t then
			      narrowing_primitive_conversion n te t
			    else
			      typing_error e.java_expr_loc
				"outside the short range: %s" (Num.string_of_num n)
			| Tunit | Treal | Tinteger | Tdouble | Tlong 
			| Tfloat | Tint | Tchar | Tboolean -> assert false (* TODO *)
		    end
	      | JTYarray _ | JTYinterface _ | JTYclass (_, _) 
	      | JTYnull | JTYlogic _
		  -> raise Not_found
	  end
    | JEbin(e1,op,e2) -> 
	let n1 = eval_const_expression const e1 in
	let n2 = eval_const_expression const e2 in
	begin
	  match op with
	    | Badd -> Num.add_num n1 n2
	    | Bbwxor -> assert false (* TODO *)
	    | Bbwor -> bwor_num n1 n2
	    | Bbwand -> assert false (* TODO *)		
	    | Blsl|Basr|Blsr -> 
		let max =
		  match e1.java_expr_type with
		    | JTYbase Tint -> 31
		    | JTYbase Tlong -> 63
		    | _ -> assert false
		in 
		if Num.le_num Numconst.zero n1 && Num.le_num n1 (Num.num_of_int max) then
		  match op with
		    | Blsl -> lsl_num n1 (Num.int_of_num n2)
		    | Basr -> asr_num n1 (Num.int_of_num n2)
		    | Blsr -> lsr_num n1 (Num.int_of_num n2)
		    | _ -> assert false
		else
		  typing_error e2.java_expr_loc "this expression is not in the rang 0-%d" max
	    | Bge|Ble|Blt|Bgt|Bne|Beq
	    |Biff|Bimpl|Bor|Band|Bmod|Bdiv|Bmul|Bsub -> assert false (* TODO *)

	end
    | JEstatic_field_access (ty, fi) ->
	begin
	  try
	    match fi.java_field_info_type with
	      | JTYarray _ -> raise Not_found 
	      | _ ->
		  List.hd (Hashtbl.find final_field_values_table fi.java_field_info_tag)
	  with Not_found ->
	    if const then
	      typing_error e.java_expr_loc "Cannot evaluate this expression"
	    else
	      raise Not_found
	end
    | JEif (_, _, _)-> assert false  (* TODO *)
    | JEun (op, e) -> 
	let _n = eval_const_expression const e in
	begin
	  match op with
	    | Uplus -> assert false  (* TODO *)
	    | Uminus -> Num.minus_num _n
	    | Unot -> assert false  (* TODO *)
	    | Ucompl -> raise Not_found (* TODO *)
	end
    | JEinstanceof _
    | JEvar _ 
    | JEnew_object (_, _)
    | JEnew_array (_, _)
    | JEstatic_call (_, _)
    | JEcall (_, _, _)
    | JEconstr_call _
    | JEassign_array (_, _, _)
    | JEassign_array_op (_, _, _, _)
    | JEassign_field_op (_, _, _, _)
    | JEassign_field (_, _, _)
    | JEassign_static_field _
    | JEassign_static_field_op _
    | JEassign_local_var_op (_, _, _)
    | JEassign_local_var (_, _)
    | JEarray_access (_, _)
    | JEarray_length _
    | JEfield_access (_, _)
    | JEincr_field (_, _, _)
    | JEincr_local_var (_, _)
    | JEincr_array _ -> raise Not_found

let is_assignment_convertible ?(const=false) ~ghost tfrom efrom tto =
  is_identity_convertible tfrom tto ||
  match tfrom,tto with
    | JTYbase t1, JTYbase t2 -> 
	if ghost then is_logic_widening_primitive_convertible t1 t2
	else
	  is_widening_primitive_convertible t1 t2 ||
	    begin
	      match t2 with
		| Tbyte | Tshort | Tchar ->		  
		    begin
		      try
 			let n = eval_const_expression const efrom in
			match t2 with
			  | Tbyte -> in_byte_range n
			  | Tshort -> in_short_range n
			  | Tchar -> in_char_range n
			  | _ -> assert false
		      with Not_found -> 
			if const then raise Not_found else false
		    end
		| _ -> false
	    end
    | _ -> is_widening_reference_convertible tfrom tto

(* JLS 5.3: Method invocation conversion  *)
let is_method_invocation_convertible tfrom tto =
  is_identity_convertible tfrom tto ||
  match tfrom,tto with
    | JTYbase t1, JTYbase t2 -> is_widening_primitive_convertible t1 t2
    | _ -> is_widening_reference_convertible tfrom tto

(* JLS 5.5: Cast conversion *)

let cast_convertible tfrom tto =
  is_identity_convertible tfrom tto ||
    match tfrom,tto with
      | JTYbase t1, JTYbase t2 -> true (* correct ? TODO *)
      | JTYbase _,_ | _, JTYbase _ -> false
      | JTYlogic _,_ | _,JTYlogic _ -> false
      | JTYclass(_,cfrom), JTYclass(_,cto) ->
	  is_subclass cfrom cto || is_subclass cto cfrom
      | JTYclass _, JTYinterface _ -> assert false (* TODO *)
      | JTYclass(_,c), JTYarray _ -> 
	  c == !object_class
      | JTYinterface _,JTYclass _ -> assert false (* TODO *)
      | JTYinterface ifrom, JTYinterface ito -> 
	  is_subinterface ifrom ito || is_subinterface ito ifrom
	    (* TODO: check this: JLS p73 appears to be incomplete *)
      | JTYinterface _,JTYarray _ -> assert false (* TODO *)
      | JTYarray _,_ -> assert false (* TODO *)
      | JTYnull,_ | _, JTYnull -> assert false
  
(**********************)

(* expressions *)
	  
let make_bin_op ~ghost loc op t1 e1 t2 e2 =
  match op with
    | Bgt | Blt | Bge | Ble ->
	begin
	  try
	    let _t = binary_numeric_promotion ~ghost t1 t2 in
	      boolean_type, JEbin(e1, op, e2)
	  with Not_found ->
	    typing_error loc "numeric types expected"
	end
    | Beq | Bne ->
	begin
	  try
	    let _t = binary_numeric_promotion t1 t2 in
	      boolean_type, JEbin(e1, op, e2)
	  with Not_found ->
	    if (is_boolean t1 && is_boolean t2) ||
	      (is_reference_type t1 && is_reference_type t2) then
		boolean_type, JEbin (e1, op, e2)
	    else
	      typing_error loc "numeric or object types expected for == and !="
	end
    | Bsub | Bmul | Bdiv | Bmod ->
	begin
	  try
	    let t = binary_numeric_promotion ~ghost t1 t2 in
	    JTYbase t,JEbin(e1, op, e2)
	  with Not_found ->
	    typing_error loc "numeric types expected for -, *, / and %%"
	end
    | Badd ->
	begin
	  try
	    let t = binary_numeric_promotion ~ghost t1 t2 in
	    JTYbase t,JEbin(e1, op, e2)
	  with Not_found ->
	    match t1,t2 with
	      | (_,JTYclass(_,c)) when c == string_class ->
		  (string_type ~valid:true),JEbin(e1,op,e2)
	      | (JTYclass(_,c),_) when c == string_class ->
		  (string_type ~valid:true),JEbin(e1,op,e2)
	      | _ ->
		  typing_error loc 
		    "numeric types or String expected for +, got %a + %a" 
		    print_type t1 print_type t2
	end
    | Band | Bor -> 
	if is_boolean t1 && is_boolean t2 then
	  boolean_type,JEbin(e1,op,e2)
	else
	  typing_error loc "booleans expected"
    | Basr | Blsr | Blsl -> 
	(* JLS 15.19: Shift Operators *)
	begin
	  try
	    match unary_numeric_promotion t1 with
	      | (Tint | Tlong) as t1 ->
		  begin
		    try
		      match unary_numeric_promotion t2 with
			| Tint | Tlong ->
			    JTYbase t1, JEbin(e1, op, e2)
			| _ -> raise Not_found
		    with Not_found -> int_expected loc t2
		  end
	      | _ -> raise Not_found
	  with Not_found -> int_expected loc t1
	end
    | Bbwxor|Bbwor|Bbwand -> 	
	if is_boolean t1 && is_boolean t2 then
	  boolean_type,JEbin(e1,op,e2)
	else
	  begin
	    try
	      let t1 = unary_numeric_promotion t1 in
	      let _t2 = unary_numeric_promotion t2 in
		JTYbase t1, JEbin(e1, op, e2)
	    with Not_found ->
	      typing_error loc "booleans or integers expected"
	  end	  
    | Bimpl | Biff -> assert false
	
let make_unary_op loc op t1 e1 =
  match op with
    | Unot -> 
	if is_boolean t1 then
	  Tboolean,JEun(op,e1)
	else
	  typing_error loc "boolean expected"
    | Ucompl -> 
	begin
	  try
	    match unary_numeric_promotion t1 with
	      | Tint -> Tint,JEun(op, e1)
	      | _ -> raise Not_found
	  with Not_found ->
	    typing_error loc "integer type expected for ~"
	end
    | Uminus-> 
	begin
	  try
	    let t = unary_numeric_promotion t1 in
	    t,JEun(op, e1)
	  with Not_found ->
	    typing_error loc "numeric type expected for -"
	end
    | Uplus -> assert false

let expr_var loc vi =
  { java_expr_node = JEvar vi; 
    java_expr_type = vi.java_var_info_type;
    java_expr_loc = loc }

let rec expr_of_term t =
  let ty = ref t.java_term_type in
  let n =
    match t.java_term_node with
      | JTvar vi -> JEvar vi
(*
      | JTold _ -> assert false (* TODO *)
*)
      | JTat _ -> assert false (* TODO *)
      | JTfield_access(t, fi) -> 
	  JEfield_access(expr_of_term t,fi)
      | JTstatic_field_access(ci, fi) -> 
	  JEstatic_field_access(ci,fi)
      | JTarray_length(t) -> 
	  ty := int_type;
	  JEarray_length(expr_of_term t)
      | JTarray_access(t1,t2) -> 
	  JEarray_access(expr_of_term t1, expr_of_term t2)
      | JTarray_range _  -> assert false (* TODO *)
      | JTapp (_, _) -> assert false (* TODO *)
      | JTbin (_, _, _, _) -> assert false (* TODO *)
      | JTun (t, op, e1) -> assert false (* TODO *)
      | JTlit _ -> assert false (* TODO *)
      | JTcast(ty,t) -> JEcast(ty,expr_of_term t)
  in
  { java_expr_loc = t.java_term_loc;
    java_expr_type = !ty;
    java_expr_node = n ;
  }

(*
  JLS 15.12.2: Compile-Time Step 2: Determine Method signature

  ti is the class or interface to search
  
*)

let is_accessible_and_applicable_method mi id arg_types =
  mi.method_info_name = id &&
  (* check args number *) 
  List.length arg_types = List.length mi.method_info_parameters &&
  (* check args types *)
(
  (*
    eprintf "check applicability of [%a] to [%a]@."
    (Pp.print_list Pp.comma (fun fmt t -> print_type fmt t)) arg_types
    (Pp.print_list Pp.comma (fun fmt (vi,_) -> print_type fmt vi.java_var_info_type)) mi.method_info_parameters;
  *)
  List.for_all2
   (fun vi t -> 
      is_method_invocation_convertible t vi.java_var_info_type)
  (List.map fst mi.method_info_parameters) arg_types )
  
let is_accessible_and_applicable_constructor ci arg_types =
  (* check args number *) 
  List.length arg_types = List.length ci.constr_info_parameters &&
  (* check args types *)
  List.for_all2
  (fun vi t -> 
     is_method_invocation_convertible t vi.java_var_info_type)
  (List.map fst ci.constr_info_parameters) arg_types 
  
let method_signature mi =
  let t =
    match mi.method_info_class_or_interface with
      | TypeClass ci -> JTYclass (true, ci) (* i.e. [this] is always non-null *)
      | TypeInterface ii -> JTYinterface ii
  in
  t :: List.map (fun (vi, _) -> vi.java_var_info_type) mi.method_info_parameters

let rec compare_signatures acc s1 s2 =
  match s1,s2 with
    | [],[] -> acc
    | [],_ | _,[] -> assert false
    | t1::r1,t2::r2 ->
	if is_identity_convertible t1 t2 then
	  compare_signatures acc r1 r2
	else
	  if is_method_invocation_convertible t1 t2 then
	    (* t1 convertible to t2 *)
	    if acc >= 0 then 1 else raise Not_found
	  else
	    if is_method_invocation_convertible t2 t1 then
	      (* t2 convertible to t1 *)
	      if acc <= 0 then -1 else raise Not_found
	    else raise Not_found
    
let rec filter_maximally_specific_signature mi acc =
  match acc with
    | [] -> [mi]
    | mi' :: rem ->
	let s1 = method_signature mi in
	let s2 = method_signature mi' in
	try
	  let c = compare_signatures 0 s1 s2 in
	  if c = 0 then mi :: acc else
	    if c > 0 then (* mi more specific than mi' *)
	      filter_maximally_specific_signature mi rem
	    else (* mi' more specific than mi *)
	      acc 	      
	with Not_found -> (* incomparable signatures *)
	  mi' :: (filter_maximally_specific_signature mi rem)

let rec get_maximally_specific_signatures acc meths =
  match meths with
    | [] -> acc
    | mi::rem ->
	let acc' = filter_maximally_specific_signature mi acc in
	get_maximally_specific_signatures acc' rem

let lookup_method ti (loc,id) arg_types = 
  let rec collect_methods_from_interface acc ii =
    check_if_interface_complete ii;
    let acc = 
      List.fold_left
	(fun acc mi -> 
	   if is_accessible_and_applicable_method mi id arg_types then 
	     mi::acc 
	   else acc)
	acc ii.interface_info_methods
    in
    List.fold_left
      collect_methods_from_interface acc ii.interface_info_extends
  in
  let rec collect_methods_from_class acc ci =
    check_if_class_complete ci;
    let acc = 
      List.fold_left
	(fun acc mi -> 
	   if is_accessible_and_applicable_method mi id arg_types then 
	     mi::acc 
	   else acc)
	acc ci.class_info_methods
    in
    let acc =
      match ci.class_info_extends with
	| None -> acc 
	| Some ci -> collect_methods_from_class acc ci
    in
    List.fold_left
      collect_methods_from_interface acc ci.class_info_implements
  in
  let meths = 
    match ti with
      | TypeClass ci -> collect_methods_from_class [] ci 
      | TypeInterface ii -> collect_methods_from_interface [] ii 
  in
  match meths with
    | [] -> raise Not_found
    | [mi] -> mi
    | _ -> 
(*
	eprintf "possible calls:@.";
	List.iter (fun mi ->
		     eprintf "%a.%s(%a)@." 
		       print_type_name mi.method_info_class_or_interface
		       mi.method_info_name
		       (Pp.print_list Pp.comma (fun fmt (vi,_) -> print_type fmt vi.java_var_info_type)) 
		       mi.method_info_parameters)
	  meths;
*)
	let meths = get_maximally_specific_signatures [] meths in
(*
	eprintf "maximally specific calls:@.";
	List.iter (fun mi ->
		     eprintf "%a.%s(%a)@." 
		       print_type_name mi.method_info_class_or_interface
		       mi.method_info_name
		       (Pp.print_list Pp.comma (fun fmt (vi,_) -> print_type fmt vi.java_var_info_type)) 
		       mi.method_info_parameters)
	  meths;
*)
	match meths with
	  | [] -> assert false
	  | [mi] -> mi
	  | _ -> 	
	      typing_error loc "ambiguity in overloading/overriding"

let lookup_constructor ci arg_types = 
  let rec collect_constructors_from_class acc ci =
    check_if_class_complete ci;
    List.fold_left
      (fun acc ci -> 
	 if is_accessible_and_applicable_constructor ci arg_types then 
	   ci::acc 
	 else acc)
      acc ci.class_info_constructors
  in
  let constructors = collect_constructors_from_class [] ci in
    match constructors with
      | [ci] -> ci
      | _ -> assert false
	  
let rec expr ~ghost env e =
  let exprt = expr ~ghost env in
  let ty, te = 
    match e.java_pexpr_node with
      | JPElit l -> 
	  let t, l = 
	    match l with
	      | Integer s | Char s -> int_type, l
	      | String s -> (string_type ~valid:true), l
	      | Bool b -> boolean_type, l
	      | Float s -> double_type, l
	      | Null -> null_type, l
	  in t, (JElit l)
      | JPEname n -> 
	  begin
	    match classify_name env.package_env env.type_env env.current_type env.env n with
	      | TermName t ->
		  let e = expr_of_term t in
		  e.java_expr_type, e.java_expr_node
	      | TypeName _ ->
		  typing_error e.java_pexpr_loc
		    "expression expected, got a class or interface"
	      | PackageName _ ->
		  typing_error e.java_pexpr_loc
		    "expression expected, got a package name"
	      | LogicTypeName _ ->
		  typing_error e.java_pexpr_loc
		    "expression expected, got a logic type"
	  end
      | JPEthis -> 
	  let vi = get_this e.java_pexpr_loc env.env in
	  vi.java_var_info_type, JEvar vi
      | JPEinstanceof (e1, t)-> 
	  let te1 = exprt e1 in
	  let ty = type_type env.package_env env.type_env false t in
	  if is_reference_type ty && 
	    cast_convertible te1.java_expr_type ty then
	    boolean_type,JEinstanceof(te1,ty)
	  else
	    typing_error e.java_pexpr_loc "invalid instanceof"
      | JPEcast (t, e1)-> 
	  let te1 = exprt e1 in
	  let ty = type_type env.package_env env.type_env false t in
	  if cast_convertible te1.java_expr_type ty then
	    ty,JEcast(ty,te1)
	  else
	    typing_error e.java_pexpr_loc "invalid cast"
      | JPEarray_access (e1, e2)-> 
	  let te1 = exprt e1 and te2 = exprt e2 in 
	  begin
	    match te1.java_expr_type with
	      | JTYarray (_, t) ->
		  begin
		    try
		      match
			unary_numeric_promotion te2.java_expr_type 
		      with
			| Tint -> t, JEarray_access(te1,te2)
			| _ -> raise Not_found
		    with
			Not_found ->
			  int_expected e2.java_pexpr_loc te1.java_expr_type
		  end
	      | _ ->
		  array_expected e1.java_pexpr_loc te1.java_expr_type
	  end
      | JPEnew_array(t,dims) ->
	  let ty = type_type env.package_env env.type_env true t in 
	  let l =
	    List.map (fun e ->
			let te = exprt e in
			match unary_numeric_promotion te.java_expr_type with
			  | Tint ->
			      te
			  | _ ->
			      int_expected e.java_pexpr_loc te.java_expr_type)
	      dims
	  in
	  JTYarray (true, ty), JEnew_array(ty,l)
      | JPEnew (n, args) -> 
	  let args = List.map exprt args in
	  let arg_types = List.map (fun e -> e.java_expr_type) args in	    
	    begin
	    match classify_name env.package_env env.type_env env.current_type env.env n with
	      | TypeName (TypeClass ci) ->
(*
		  eprintf "looking up constructor in class %s@." ci.class_info_name;
*)
		  let constr = lookup_constructor ci arg_types in
		  JTYclass (true, ci), JEnew_object(constr,args)
	      | _ ->
		  typing_error (fst (List.hd n))
		    "class type expected"	
	  end	  
      | JPEsuper_call (id, el) -> 
	  (* JLS 15.12 *)
	  let ci =
	    match env.current_type with
	      | Some (TypeClass ci) ->
		  if ci == !object_class then 
		    typing_error e.java_pexpr_loc "cannot resolve variable super"
		  else
		    begin
		      match ci.class_info_extends with
			| None -> 
			    typing_error e.java_pexpr_loc
			      "cannot resolve variable super"
			| Some ci -> TypeClass ci
		    end
	      | _ -> typing_error e.java_pexpr_loc "not a class"
	  in
	  let args = List.map exprt el in
	  let arg_types = List.map (fun e -> e.java_expr_type) args in
	  begin
	    let mi = lookup_method ci id arg_types in
	    let ty = 
	      match mi.method_info_result with
		| None -> unit_type
		| Some vi -> vi.java_var_info_type
	    in
	      if mi.method_info_is_static then
		ty, JEstatic_call (mi, args)
	      else
		let te2 =
		  let vi = get_this e.java_pexpr_loc env.env in
		    {
		      java_expr_node = JEvar vi;
		      java_expr_type = vi.java_var_info_type;
		      java_expr_loc = e.java_pexpr_loc;
		    }
		in
		  ty,JEcall (te2, mi, args)
	  end
      | JPEcall_name (qid, args)-> 
	  let args = List.map exprt args in
	  let arg_types = List.map (fun e -> e.java_expr_type) args in
	  let ti,id,te1 =
	    match qid with
	      | [] -> assert false
	      | [id] ->
		  begin
		    match env.current_type with
		      | None -> assert false
		      | Some ti -> ti,id,None
		  end
	      | id::n ->
		  begin
		    match 
		      classify_name env.package_env env.type_env env.current_type env.env n
		    with
		      | TypeName ti -> ti, id, None
		      | TermName te -> 
			  let ti =
			    match te.java_term_type with
			      | JTYclass(_,ci) -> TypeClass ci
			      | JTYinterface ii -> TypeInterface ii 
			      | _ -> typing_error e.java_pexpr_loc 
				  "not a class or interface type"
			  in ti,id,Some (expr_of_term te)
		      | PackageName _ | LogicTypeName _ ->
			  typing_error (fst (List.hd n)) 
			    "expr or class or interface expected"
		  end
	  in
(*
	  eprintf "looking up method '%s' in class %a @." (snd id) print_type_name ti;
*)
	  let mi = 
	    try lookup_method ti id arg_types 
	    with Not_found ->
	      typing_error e.java_pexpr_loc "Cannot find method @['%a.%s(%a)'@]" print_type_name ti (snd id)
		(Pp.print_list Pp.comma print_type) arg_types

	  in
	  let ty = 
	    match mi.method_info_result with
	      | None -> unit_type
	      | Some vi -> vi.java_var_info_type
	  in
	  if mi.method_info_is_static then
	    ty,JEstatic_call(mi,args)
	  else
	    let te2 =
	      match te1 with
		| Some e -> e
		| None ->
		    let vi = get_this e.java_pexpr_loc env.env in
		    {
		      java_expr_node = JEvar vi;
		      java_expr_type = vi.java_var_info_type;
		      java_expr_loc = e.java_pexpr_loc;
		    }
	    in
	      ty,JEcall(te2,mi,args)

      | JPEcall_expr (e1, id, args)-> 
	  let args = List.map exprt args in
	  let arg_types = List.map (fun e -> e.java_expr_type) args in
	  begin
	    let ci,te1 =
	      let te = exprt e1 in
	      begin
		match te.java_expr_type with
		  | JTYclass(_,ci) -> (TypeClass ci),Some te
		  | JTYinterface(ii) -> (TypeInterface ii),Some te
		  | _ -> typing_error e.java_pexpr_loc 
		      "not a class or interface type"
	      end
	    in
	    let mi = lookup_method ci id arg_types in
	    let ty = 
	      match mi.method_info_result with
		| None -> unit_type
		| Some vi -> vi.java_var_info_type
	    in
	    if mi.method_info_is_static then
	      ty,JEstatic_call(mi,args)
	    else
	      let te2 =
		match te1 with
		  | Some e -> e
		  | None ->
		      let vi = get_this e.java_pexpr_loc env.env in
		      {
			java_expr_node = JEvar vi;
			java_expr_type = vi.java_var_info_type;
			java_expr_loc = e.java_pexpr_loc;
		      }
	      in
	      ty,JEcall(te2,mi,args)
	  end
      | JPEfield_access(Super_access f) -> assert false (* TODO *)
      | JPEfield_access(Primary_access(e1,(loc,id))) -> 
	  let te = type_expr_field_access (exprt e1) loc id in
	  te.java_expr_type,te.java_expr_node
      | JPEif (e1, e2, e3)-> 
	  let te1 = exprt e1 in
	  if is_boolean te1.java_expr_type then	    
	    let te2 = exprt e2 in
	    let te3 = exprt e3 in
	    (* TODO: check if compatible types *)
	    te2.java_expr_type,JEif(te1,te2,te3)
	  else
	    typing_error e1.java_pexpr_loc "boolean expected"

      | JPEassign_array (e1, e2, op, e3)-> 
	  let te1 = exprt e1 
	  and te2 = exprt e2 
	  and te3 = exprt e3 
	  in 
	  begin
	    match te1.java_expr_type with
	      | JTYarray (_, t) ->
		  begin
		    try 
		      match unary_numeric_promotion te2.java_expr_type with
			| Tint ->
			    if op = Beq then
			      if is_assignment_convertible ~ghost
				te3.java_expr_type te3 t 
			      then
				t, JEassign_array(te1,te2,te3)
			      else
				typing_error e3.java_pexpr_loc
				  "type `%a' expected" print_type t	
			    else
			      if cast_convertible te3.java_expr_type t then
				t, JEassign_array_op(te1,te2,op,te3)
			      else
				typing_error e.java_pexpr_loc 
				  "type %a expected, got %a" 
				  print_type t 
				  print_type te3.java_expr_type
				
			| _ -> raise Not_found
		    with
			Not_found ->
			  int_expected e2.java_pexpr_loc te2.java_expr_type
		  end
	      | _ ->
		  array_expected e1.java_pexpr_loc te1.java_expr_type
	  end
      | JPEassign_field (Super_access((loc,id)), op, e2) -> 
	  assert false (* TODO *)
      | JPEassign_field (Primary_access(e1,(loc,id)), op, e2)-> 
	  let te2 = exprt e2 in
	  begin
	    match 
	      (type_expr_field_access (exprt e1) loc id).java_expr_node 
	    with
	      | JEfield_access(t,fi) ->
		  type_assign_field ~ghost t fi op te2
	      | _ -> assert false
	  end
      | JPEassign_name (n, op, e1)-> 
	  begin
	    let te = exprt e1 in
	    match classify_name env.package_env env.type_env env.current_type env.env n with
		| TermName t ->
		    begin
		      match t.java_term_node with
			| JTvar vi ->
			    if op = Beq then
			      if is_assignment_convertible ~ghost te.java_expr_type te 
				vi.java_var_info_type
			      then 
				(vi.java_var_info_type,
				 JEassign_local_var(vi,te))
			      else
				typing_error e.java_pexpr_loc 
				  "type %a expected, got %a" 
				  print_type vi.java_var_info_type 
				  print_type te.java_expr_type
			    else 
			      if cast_convertible te.java_expr_type 
				vi.java_var_info_type
			      then 
				(vi.java_var_info_type,
				 JEassign_local_var_op(vi,op,te))
			      else
				typing_error e.java_pexpr_loc 
				  "type %a expected, got %a" 
				  print_type vi.java_var_info_type 
				  print_type te.java_expr_type
			| JTfield_access (t, fi) ->
			    type_assign_field ~ghost (expr_of_term t) fi op te
			| JTstatic_field_access (_, fi) ->
			    type_assign_static_field ~ghost fi op te
			| _ -> assert false (* TODO *)
		    end
		| TypeName _ ->
		    typing_error e.java_pexpr_loc
		      "lvalue expected, got a class or interface"
		| PackageName _ ->
		    typing_error e.java_pexpr_loc
		      "lvalue expected, got a package name"
		| LogicTypeName _ ->
		    typing_error e.java_pexpr_loc
		      "lvalue expected, got a package name"
	  end
      | JPEincr (op, e)-> 
	  let te = exprt e in 
	    begin
	      match te.java_expr_node with
		| JEvar vi ->
		    te.java_expr_type, JEincr_local_var (op, vi)
		| JEfield_access (e1, fi) -> 
		    fi.java_field_info_type, JEincr_field (op, e1, fi)
		| JEarray_access (e1, e2) ->
		    te.java_expr_type, JEincr_array (op, e1, e2)
		| _ -> assert false (* TODO ? *)
	    end	  
      | JPEun (op, e1)-> 
	  let te1 = exprt e1 in 
	  let t,e = make_unary_op e.java_pexpr_loc op 
		      te1.java_expr_type te1 in
	  JTYbase t,e

      | JPEbin (e1, op, e2) -> 
	  let te1 = exprt e1 and te2 = exprt e2 in 
	  make_bin_op ~ghost e.java_pexpr_loc op
	    te1.java_expr_type te1 te2.java_expr_type te2

	      (* only in terms *)
      | JPEarray_range _ 
      | JPEquantifier (_, _, _)
      | JPEat _
      | JPEold _
      | JPEresult -> 
	  typing_error e.java_pexpr_loc "not allowed in expressions"

  in { java_expr_loc = e.java_pexpr_loc;
	java_expr_type = ty;
	java_expr_node = te; }

and type_assign_field ~ghost t fi op te =
  if op = Beq then
    if is_assignment_convertible ~ghost te.java_expr_type te
      fi.java_field_info_type
    then 
      (fi.java_field_info_type, JEassign_field (t, fi, te))
    else
      typing_error te.java_expr_loc 
	"type %a expected, got %a" 
	print_type fi.java_field_info_type 
	print_type te.java_expr_type
  else 
    if cast_convertible te.java_expr_type 
      fi.java_field_info_type
    then 
      (fi.java_field_info_type, JEassign_field_op (t, fi, op, te))
    else
      typing_error te.java_expr_loc 
	"type %a expected, got %a" 
	print_type fi.java_field_info_type 
	print_type te.java_expr_type
	
and type_assign_static_field ~ghost fi op te =
  if op = Beq then
    if is_assignment_convertible ~ghost te.java_expr_type te
      fi.java_field_info_type
    then 
      (fi.java_field_info_type, JEassign_static_field (fi, te))
    else
      typing_error te.java_expr_loc 
	"type %a expected, got %a" 
	print_type fi.java_field_info_type 
	print_type te.java_expr_type
  else 
    if cast_convertible te.java_expr_type 
      fi.java_field_info_type
    then 
      (fi.java_field_info_type, JEassign_static_field_op (fi, op, te))
    else
      typing_error te.java_expr_loc 
	"type %a expected, got %a" 
	print_type fi.java_field_info_type 
	print_type te.java_expr_type

let rec initializer_loc i =
  match i with
    | Simple_initializer e -> e.java_pexpr_loc
    | Array_initializer (x::_) -> initializer_loc x
    | Array_initializer [] -> assert false (* TODO *)
			   
let rec type_initializer ~ghost env ty i =
  match ty, i with
    | _, Simple_initializer e ->
	let te = expr ~ghost env e in	
	  if is_assignment_convertible ~const:true ~ghost te.java_expr_type te ty 
	  then JIexpr te
	  else
	    typing_error e.java_pexpr_loc "type %a expected, got %a"
	      print_type ty print_type te.java_expr_type
    | JTYarray (_, t), Array_initializer vil ->
	let il =
	  List.map
	    (fun vi -> match vi with
	       | Simple_initializer e -> type_initializer ~ghost env t vi
	       | Array_initializer vil -> assert false (* TODO *))
	    vil
	in JIlist il
    | _, Array_initializer l -> 
	typing_error (initializer_loc i) "wrong type for initializer"


(* statements *)

let variable_declaration ~ghost env vd =
  let is_nullable = List.mem Nullable vd.variable_modifiers in
  let non_null = !Java_options.nonnull_sem = NonNullAllLocal && not is_nullable in
  let ty = type_type env.package_env env.type_env non_null vd.variable_type in
  let l =
    List.map
      (fun vd ->
	 let ty',id = var_type_and_id false ty vd.variable_id in
	 match vd.variable_initializer with
	   | None -> (id,ty',None)
	   | Some e -> 
	       let i = type_initializer ~ghost env ty' e in
	       (id,ty',Some i))
      vd.variable_decls
  in
  List.fold_right
      (fun ((loc,id),ty,i) (env,decls)->
	 let vi = new_var loc ty id in		     
	 (id,vi)::env,(vi,i)::decls)
      l (env.env,[])

let rec statement env s =
  let assertiont = assertion env in
  let exprt = expr ~ghost:false env in
  let statementt = statement env in
  let s' =
    match s.java_pstatement_node with
      | JPSskip -> JSskip
      | JPSif (e, s1, s2) ->
	  let te = exprt e in
	  let ts1 = statementt s1 in
	  let ts2 = statementt s2 in
	  if is_boolean te.java_expr_type then	    
	    JSif(te,ts1,ts2)
	  else
	    typing_error e.java_pexpr_loc "boolean expected"
      | JPSloop_annot (inv, dec) -> assert false
      | JPSannot (_, _)-> assert false (* TODO *)
      | JPSghost_local_decls d -> assert false (* TODO *)
      | JPSghost_statement e ->
	  let te = expr ~ghost:true env e in JSexpr te
      | JPSexpr e -> 
	  let te = exprt e in JSexpr te
      | JPSassert(id,a) ->
	  let ta = assertiont (Some LabelHere) a in
	  JSassert(Option_misc.map snd id,ta)
      | JPSsynchronized (_, _)-> assert false (* TODO *)
      | JPSblock l -> (block env l).java_statement_node
      | JPSswitch (e, l)-> 
	  let te = exprt e in
	  (* JSL, p289: switch expr must be char, byte, short or int *)
	  begin
	    try 
	      let t = unary_numeric_promotion te.java_expr_type in
	      JSswitch(te,List.map (switch_case env t) l)
	    with Not_found ->
	      typing_error e.java_pexpr_loc "char, byte, short or int expected"
	  end
      | JPStry (s, catches, finally)-> 
	  let ts = statements env s in
	  let tl =
	    List.map
	      (fun (p,s) ->
		 let vi, _ = type_param env.package_env env.type_env p in
		 match vi.java_var_info_type with
		   | JTYclass(_,ci) when 
		       (check_if_class_complete ci; 
			ci.class_info_is_exception) ->
		       let e = (vi.java_var_info_name,vi)::env.env in
		       (vi,statements {env with env = e } s)
		   | _ -> 
		       typing_error vi.java_var_info_decl_loc 
			 "throwable class expected")
	      catches
	  in
	  JStry(ts, tl, 
		Option_misc.map (statements env) finally)
      | JPSfor_decl _ -> assert false
      | JPSfor _ -> assert false
      | JPSdo (_, _)-> assert false (* TODO *)
      | JPSwhile _ -> assert false
      | JPSlabel (_, _)-> assert false (* TODO *)
      | JPScontinue _-> assert false (* TODO *)
      | JPSbreak l -> JSbreak (Option_misc.map snd l)
      | JPSreturn None -> 
	  begin
	    try
	      let _vi = List.assoc "\\result" env.env in
	      typing_error s.java_pstatement_loc "missing return value"
	    with
		Not_found ->
		  JSreturn_void
	  end
	  
      | JPSreturn (Some e) -> 
	  begin
	    try
	      let te = exprt e in 
	      let vi = List.assoc "\\result" env.env in
	      if is_assignment_convertible ~ghost:false te.java_expr_type te vi.java_var_info_type then
		JSreturn te
	      else
		typing_error e.java_pexpr_loc "type %a expected, got %a"
		  print_type vi.java_var_info_type print_type te.java_expr_type
	    with
		Not_found ->
		  typing_error e.java_pexpr_loc "no result expected"
	  end
      | JPSthrow e -> 
	  let te = exprt e in
	  JSthrow te
      | JPSvar_decl _-> assert false (* TODO *)
	  
  in 
  { java_statement_loc = s.java_pstatement_loc ;
    java_statement_node = s' }

and local_decl ~ghost env loc vd rem =
  let e, decls = variable_declaration ~ghost env vd in
  let r = block { env with env = e } rem in
  let s =
    List.fold_right
      (fun (vi, i) acc -> 
	 { java_statement_loc = loc ;
	   java_statement_node =
	     JSvar_decl(vi,i,acc); })
      decls r in
    [s]
      
and statements env b =
  match b with
    | [] -> []
    | s :: rem ->
	match s.java_pstatement_node with
	  | JPSskip -> statements env rem
	  | JPSghost_local_decls vd -> 
	      local_decl ~ghost:true env s.java_pstatement_loc vd rem
	  | JPSvar_decl vd -> 
	      local_decl ~ghost:false env s.java_pstatement_loc vd rem
	  | JPSloop_annot (inv, dec) ->
	      begin
		match rem with
		  | { java_pstatement_node = JPSwhile(e,s) ;
		      java_pstatement_loc = loc } :: rem -> 
		      let twhile =
			type_while env s.java_pstatement_loc inv dec e s
		      in
			twhile :: statements env rem
		  | { java_pstatement_node = JPSfor_decl(vd,e,sl,s) ;
		      java_pstatement_loc = loc } :: rem -> 
		      let tfor =
			type_for_decl env loc vd inv dec e sl s
		      in
			tfor :: statements env rem
		  | _ -> assert false
	      end      
	  | JPSfor (el1, e, el2, s) ->
	      let tfor =
		type_for env 
		  s.java_pstatement_loc el1 expr_true None e el2 s
	      in
		tfor :: statements env rem
	  | JPSfor_decl(vd,e,sl,s) ->
	      let tfor =
		type_for_decl env 
		  s.java_pstatement_loc vd expr_true None e sl s
	      in
		tfor :: statements env rem
	  | JPSwhile(e,s) ->
	      let twhile =
		type_while env 
		  s.java_pstatement_loc expr_true None e s
	      in
		twhile :: statements env rem
	  | _ ->
	      let s' = statement env s in
		s' :: statements env rem
		  
	
and add_Pre_Here e = { e with label_env = LabelPre::LabelHere::e.label_env }
	  	  
and type_for env loc el1 inv dec e el2 s =
  let el1 = List.map (expr ~ghost:false env) el1 in
  let inv = assertion (add_Pre_Here env) (Some LabelHere) inv in
  let dec = 
    Option_misc.map (term (add_Pre_Here env) (Some LabelHere)) dec 
  in
  let e = expr ~ghost:false env e in
  let el2 = List.map (expr ~ghost:false env) el2 in
  let s = statement env s in
    { java_statement_node = JSfor (el1, e, inv, dec, el2, s);
      java_statement_loc = loc }

and type_for_decl env loc vd inv dec e sl s =
  let env',decls = variable_declaration ~ghost:false env vd in
  let env = { env with env = env'} in
  let inv = assertion (add_Pre_Here env) (Some LabelHere) inv in
  let dec = 
    Option_misc.map (term (add_Pre_Here env) (Some LabelHere)) dec 
  in
  let e = expr ~ghost:false env e in
  let sl = List.map (expr ~ghost:false env) sl in
  let s = statement env s in
  { java_statement_node = JSfor_decl(decls,e,inv,dec,sl,s);
    java_statement_loc = loc }

and type_while env loc inv dec e s =
  let inv = assertion (add_Pre_Here env) (Some LabelHere) inv in
  let dec = 
    Option_misc.map (term (add_Pre_Here env) (Some LabelHere)) dec 
  in
  let e = expr ~ghost:false env e in
  let s = statement env s in
  { java_statement_node = JSwhile(e,inv,dec,s);
    java_statement_loc = loc } 

and block env b =
  match statements env b with
    | [] -> { java_statement_loc = Loc.dummy_position ; 
	      java_statement_node = JSskip }
    | [s] -> s
    | (s::_) as l -> 
	{ java_statement_loc = s.java_statement_loc ; 
	  java_statement_node = JSblock l }

and switch_case env t (labels,b) =
  (List.map (switch_label env t) labels, 
   statements env b)

and switch_label env t = function
  | Default -> Default
  | Case e ->
      let te = expr ~ghost:false env e in
      match te.java_expr_type with
	| JTYbase _ as t' when is_assignment_convertible ~ghost:false t' te (JTYbase t) -> Case te 
	| _ ->
	     typing_error e.java_pexpr_loc "type `%s' expected, got `%a'"
		(string_of_base_type t) print_type te.java_expr_type

(* methods *)


let location env a = term env a 
  

let behavior env pre_state_env post_state_env (id, b) = 
  let throws, ensures_env = 
    match b.java_pbehavior_throws with
      | None -> None,post_state_env
      | Some (c, None) -> 
	  (* if [current_type] is an imported type, [c] may not be in [type_env] 
	     so we need to add the package env of current type in [package_env]
	     - Nicolas R. *)
	  let package_env = 
	    match env.current_type with
	      | None -> assert false
	      | Some (TypeClass ci) ->
		  let p = 
		    List.filter
		      (fun p -> not (List.mem p env.package_env)) 
		      ci.class_info_package_env in
		    p @ env.package_env
	      | Some (TypeInterface ii) -> 
		  let p = 
		    List.filter
		      (fun p -> not (List.mem p env.package_env)) 
		      ii.interface_info_package_env in
		    p @ env.package_env
	  in
	  begin
	    match classify_name package_env env.type_env env.current_type pre_state_env c with
	      | TypeName (TypeClass ci) ->
		  check_if_class_complete ci;
		  assert (ci.class_info_is_exception);
		  (Some ci),post_state_env
	      | TypeName (TypeInterface ci) ->
		  typing_error (fst (List.hd c))
		    "class type expected, not an interface"
	      | TermName _ ->
		  typing_error (fst (List.hd c))
		    "class type expected"
	      | PackageName _ | LogicTypeName _ ->
		  typing_error (fst (List.hd c))
		    "class type expected"
	  end
      | Some (c, Some id) -> 
	  assert false (* TODO *)
  in
  (id,
   Option_misc.map (assertion { env with env = pre_state_env} (Some LabelHere)) b.java_pbehavior_assumes,
   throws,
   (* Note: the `assigns' clause is typed in post-state environnement *)
   Option_misc.map 
     (fun (loc,l) ->
	(loc,List.map (location { env with env = ensures_env} (Some LabelHere)) l)) b.java_pbehavior_assigns,
   assertion {env with env = ensures_env} (Some LabelHere) b.java_pbehavior_ensures)
    


(* methods *)

type method_table_info =
    { mt_method_info : Java_env.method_info;
      mt_requires : Java_tast.assertion option;
      mt_decreases : Java_tast.term option;
      mt_behaviors : (Java_ast.identifier * 
			Java_tast.assertion option * 
			Java_env.java_class_info option *
			(Loc.position * Java_tast.term list) option * 
			Java_tast.assertion) list ;
      mt_body : Java_tast.block option;
    }


  
let methods_table = Hashtbl.create 97


let type_method_spec_and_body ?(dobody=true) 
    package_env type_env ti mi =
  try
    let _ = Hashtbl.find methods_table mi.method_info_tag in ()
  with Not_found ->
    let (_,req,decreases,behs,body) = 
      try
	Hashtbl.find methods_env mi.method_info_tag 
      with Not_found -> assert false
    in
    let local_env =
      if mi.method_info_is_static then [] else
	let this_type =
	  match ti with
	    | TypeClass ci -> JTYclass (true, ci) (* i.e. [this] is always non-null *)
	    | TypeInterface ii -> JTYinterface ii
	in
	let vi = new_var Loc.dummy_position this_type "this" in
	  mi.method_info_has_this <- Some vi;
	  [("this",vi)]
    in
    let local_env = 
      List.fold_left
	(fun acc vi -> 
	   (vi.java_var_info_name,vi)::acc)
	local_env (List.map fst mi.method_info_parameters)
    in
    let env = { package_env = package_env ;
		type_env = type_env;
		current_type = (Some ti);
		label_env = [LabelHere];
		env = local_env }
    in
    let req = Option_misc.map (assertion env (Some LabelHere)) req in
    let decreases = Option_misc.map (term env (Some LabelHere)) decreases in
    let env_result =
      match mi.method_info_result with
	| None -> local_env
	| Some vi -> (vi.java_var_info_name,vi)::local_env
    in
      (*
	let assigns = 
	Option_misc.map 
	(List.map 
	(location package_env type_env (Some ti) env_result)) assigns
	in
	let ens = Option_misc.map (assertion package_env type_env (Some ti) env_result) ens in
      *)
    let behs = List.map (behavior env local_env env_result) behs in
    let body = 
      if dobody then
	Option_misc.map (statements { env with env = env_result}) body 
      else None
    in
    Hashtbl.add methods_table mi.method_info_tag 
      { mt_method_info = mi;
	mt_requires = req;
	mt_decreases = decreases;
	mt_behaviors = behs;
	mt_body = body }
	  
	  
type constructor_table_info =
    { ct_constr_info : Java_env.constructor_info;
      ct_requires : Java_tast.assertion option;
      ct_decreases : Java_tast.term option;
      ct_behaviors : (Java_ast.identifier * 
			Java_tast.assertion option * 
			Java_env.java_class_info option *
			(Loc.position * Java_tast.term list) option * 
			Java_tast.assertion) list ;
      ct_body : Java_tast.block;
    }

let constructors_table = Hashtbl.create 97
  
let type_constr_spec_and_body ?(dobody=true) 
    package_env type_env current_type ci =
  try
    let _ = Hashtbl.find constructors_table ci.constr_info_tag in ()
  with Not_found ->
  let (_,req,decreases,behs,eci,body) = 
    try
      Hashtbl.find constructors_env ci.constr_info_tag 
    with Not_found -> assert false
  in
  let local_env = 
    List.fold_left
      (fun acc (vi, _) -> 
	 (vi.java_var_info_name,vi)::acc)
      [] ci.constr_info_parameters
  in
  let this_type =
    match current_type with
      | TypeClass ci -> JTYclass (true, ci) (* i.e. this is always non-null *)
      | TypeInterface ii -> JTYinterface ii
  in
  let this_vi = new_var Loc.dummy_position this_type "this" in
  let this_env =
    ci.constr_info_this <- Some this_vi;
    ("this", this_vi)::local_env
  in
(*
  let spec_env =
    (* spec is typed in a env that contains "this" but it will be
       renamed to "\\result" NO: TODO *)
    let vi = new_var this_type "this" (* "\\result" *) in
      ci.constr_info_result <- Some vi;
      ("this",vi)::local_env
  in
*)
  let env = { package_env = package_env ;
	      type_env = type_env;
	      current_type = (Some current_type);
	      label_env = [LabelHere];
	      env = this_env }
  in
  let req = 
    Option_misc.map 
      (assertion { env with env = local_env } (Some LabelHere)) req 
  in
  let decreases = 
    Option_misc.map 
      (term { env with env = local_env } (Some LabelHere)) decreases 
  in
  let behs = List.map (behavior env local_env this_env) behs in
  if dobody then
    match eci with
      | Invoke_none -> 
	  let body = statements { env with env = this_env } body in
	  Hashtbl.add constructors_table ci.constr_info_tag 
	    { ct_constr_info = ci;
	      ct_requires = req;
	      ct_decreases = decreases;
	      ct_behaviors = behs;
	      ct_body = body } 
      | Invoke_this el -> 
	  let tel = List.map (expr ~ghost:false env) el in
	  let arg_types = List.map (fun te -> te.java_expr_type) tel in
	  let this_ci = lookup_constructor ci.constr_info_class arg_types in
	  let this_call_s =
	    make_statement_no_loc 
	      (JSexpr (
		 make_expr_no_loc unit_type
		   (JEconstr_call
		      (make_expr_no_loc
			 this_vi.java_var_info_type			
			 (JEvar this_vi), 
		       this_ci, tel))))
	  in
	  let body = statements env body in
	  Hashtbl.add constructors_table ci.constr_info_tag 
	    { ct_constr_info = ci;
	      ct_requires = req;
	      ct_decreases = decreases;
	      ct_behaviors = behs;
	      ct_body = this_call_s :: body }
      | Invoke_super el ->
	  let tel = List.map (expr ~ghost:false env) el in
	  let super_class_info = 
	    match ci.constr_info_class.class_info_extends with
	      | None -> assert false
	      | Some ci -> ci
	  in
	  let arg_types = List.map (fun te -> te.java_expr_type) tel in
	  let super_ci = lookup_constructor super_class_info arg_types in
	  let super_call_s =
	    make_statement_no_loc 
	      (JSexpr (
		 make_expr_no_loc unit_type
		   (JEconstr_call
		      (make_expr_no_loc
			 (JTYclass (true (* [this] is always non null *), super_class_info))
			 (JEvar this_vi), 
		       super_ci, tel))))
	  in
	  let body = statements env body in
	  Hashtbl.add constructors_table ci.constr_info_tag 
	    { ct_constr_info = ci;
	      ct_requires = req;
	      ct_decreases = decreases;
	      ct_behaviors = behs;
	      ct_body = super_call_s :: body }
  else
    Hashtbl.add constructors_table ci.constr_info_tag 
      { ct_constr_info = ci;
	ct_requires = req;
	ct_decreases = decreases;
	ct_behaviors = behs;
	ct_body = [] }
    
		      
let type_field_initializer package_env type_env ci fi =
  let init = 
    try
      Hashtbl.find field_prototypes_table fi.java_field_info_tag 
    with Not_found -> assert false
  in
  let env = 
    { package_env = package_env;
      type_env = type_env;
      current_type = Some ci;
      label_env = [];
      env = [];
    }
  in
  let tinit = 
    match init with
      | None -> None
      | Some i ->
	  let ti = 
	    type_initializer ~ghost:false env fi.java_field_info_type i
	  in
	    if fi.java_field_info_is_final then
	      begin
		match ti with
		  | JIexpr e ->
		      begin
			try
			  let v = eval_const_expression false e in
			    Hashtbl.add final_field_values_table 
			      fi.java_field_info_tag [v]
			with Not_found ->
			  (**)
			  Java_options.lprintf
			    "FIXME: cannot evaluate this initializer, %a@."
			    Loc.gen_report_position e.java_expr_loc
			    (**)
			    (*
			      typing_error e.java_expr_loc "cannot evaluate this initializer"
			    *)		    
		      end
		  | JIlist vil ->
		      try
			let vil = List.map
			  (fun vi -> match vi with
			     | JIexpr e -> eval_const_expression false e
			     | JIlist _ -> assert false (* TODO *))
			  vil
			in
			  Hashtbl.add final_field_values_table 
			    fi.java_field_info_tag vil
		      with Not_found -> assert false 
	      end;
	    Some ti
  in
    Hashtbl.add field_initializer_table fi.java_field_info_tag tinit
      
let type_decl package_env type_env d = 
  match d with
    | JPTclass c -> 
	(*
	  class_modifiers : modifiers;
	  class_name : identifier;
	  class_extends : qualified_ident option;
	  class_implements : qualified_ident list;
	  class_fields : field_declaration list
	*)
	begin
	  let ty = 
	    try
	      List.assoc (snd c.class_name) type_env
	    with
		Not_found -> 
		  eprintf "Java_typing anomaly: class '%s' not found in type_env@."  
		    (snd c.class_name);		 
		  List.iter
		    (fun (id,_) -> eprintf "  '%s'@\n" id)
		    type_env;
		  assert false
	  in
	  match ty with	  
	    | TypeInterface _ -> assert false
	    | TypeClass ci as ti ->
		check_if_class_complete ci;
		let full_type_env =
		  try Hashtbl.find class_type_env_table ci.class_info_tag
		  with Not_found -> assert false
		in
		  List.iter (type_field_initializer package_env full_type_env ti) 
		    ci.class_info_fields;
		  List.iter (type_method_spec_and_body package_env full_type_env ti) 
		    ci.class_info_methods;
		  List.iter (type_constr_spec_and_body package_env full_type_env ti) 
		    ci.class_info_constructors;
	end
    | JPTinterface i -> 
	begin
	  let ty = 
	    try
	      List.assoc (snd i.interface_name) type_env
	    with
		Not_found -> 
		  eprintf "Java_typing anomaly: interface '%s' not found in type_env@."  
		    (snd i.interface_name);		 
		  List.iter
		    (fun (id,_) -> eprintf "  '%s'@\n" id)
		    type_env;
		  assert false
	  in
	  match ty with	  
	    | TypeClass _ -> assert false
	    | TypeInterface ii as ti ->
		check_if_interface_complete ii;
		let full_type_env =
		  try Hashtbl.find interface_type_env_table ii.interface_info_tag
		  with Not_found -> assert false
		in
		  List.iter (type_field_initializer package_env full_type_env ti) 
		    ii.interface_info_fields;
		  List.iter (type_method_spec_and_body package_env full_type_env ti) 
		    ii.interface_info_methods;
	end
    | JPTannot(loc,s) -> assert false
    | JPTlemma((loc,id),is_axiom, labels,e) -> 
	let env =
	  { package_env = package_env;
	    type_env = type_env;
	    current_type = None;
	    label_env = labels;
	    env = [];
	  }
	in
	(* TODO: si un seul label, c'est celui par defaut *)
	let te = assertion env None e in
	Hashtbl.add axioms_table id (is_axiom,labels,te)
    | JPTlogic_type_decl _ -> ()
    | JPTlogic_reads ((loc, id), ret_type, labels, params, reads) -> 
	let pl = List.map (fun p -> fst (type_param package_env type_env p)) params in
	let env = 
	  List.fold_left
	    (fun acc vi -> 
	       (vi.java_var_info_name,vi)::acc)
	    [] pl
	in
	let env =
	  { package_env = package_env;
	    type_env = type_env;
	    current_type = None;
	    label_env = labels;
	    env = env;
	  }
	in
	  begin match ret_type with
	    | None ->(* TODO: si un seul label, c'est celui par defaut *)
		let fi = logic_info id None labels pl in
		(* TODO: si un seul label, c'est celui par defaut *)
		let r = List.map (location env None) reads in
		  Hashtbl.add logics_env id fi;
		  Hashtbl.add logics_table fi.java_logic_info_tag (fi,JReads r)
	    | Some ty -> 
		let fi = 
		  logic_info id 
		    (Some (type_type package_env type_env false ty)) 
		    labels pl 
		in
		(* TODO: si un seul label, c'est celui par defaut *)
		let r = List.map (location env None) reads in
		  Hashtbl.add logics_env id fi;
		  Hashtbl.add logics_table fi.java_logic_info_tag (fi,JReads r)
	  end
    | JPTlogic_def ((loc, id), ret_type, labels, params, body) -> 
	let pl = List.map (fun p -> fst (type_param package_env type_env p)) params in
	let env = 
	  List.fold_left
	    (fun acc vi -> 
	       (vi.java_var_info_name,vi)::acc)
	    [] pl
	in
	let env =
	  { package_env = package_env;
	    type_env = type_env;
	    current_type = None;
	    label_env = labels;
	    env = env;
	  }
	in
	  begin match ret_type with
	    | None ->
		let fi = logic_info id None labels pl in
		let a = assertion env None body in
		  Hashtbl.add logics_env id fi;
		  Hashtbl.add logics_table fi.java_logic_info_tag (fi,JAssertion a)
	    | Some t -> 
		let fi = 
		  logic_info id 
		    (Some (type_type package_env type_env false t)) labels pl 
		in
		let a = term env None body in
		  Hashtbl.add logics_env id fi;
		  Hashtbl.add logics_table fi.java_logic_info_tag (fi,JTerm a)
	  end

let get_bodies package_env type_env cu =
  List.iter (type_decl package_env type_env) cu.cu_type_decls

let type_specs package_env type_env =
  Hashtbl.iter
    (fun _ ti ->
       match ti with 
	 | TypeInterface ii ->
	     List.iter (type_field_initializer package_env type_env (TypeInterface ii)) 
	       ii.interface_info_fields
	 | _ -> ())
  type_table;
  Hashtbl.iter
    (fun tag (current_type, env, vi, invs) -> 
       match current_type with
	 | TypeClass ci ->
	     let env =
	       { package_env = package_env;
		 type_env = type_env;
		 current_type = (Some current_type);
		 label_env = [];
		 env = env;
	       }
	     in
	     Hashtbl.add invariants_table tag
	       (ci, vi, List.map 
		  (fun (id, e) ->
		     (id, assertion env None e))
		  invs)
	 | _ -> assert false)
    invariants_env;
  Hashtbl.iter 
    (fun tag (current_type, invs) ->
       let env =
	 { package_env = package_env;
	   type_env = type_env;
	   current_type = (Some current_type);
	   label_env = [];
	   env = [];
	 }
       in
       Hashtbl.add static_invariants_table tag
	 (List.map 
	    (fun (s, e) -> s, assertion env None e)
	 invs))
    static_invariants_env;
  Hashtbl.iter 
    (fun _ (mi, _, _, _,_)  ->
       type_method_spec_and_body ~dobody:false 
	 package_env type_env mi.method_info_class_or_interface mi) 
    methods_env;
  Hashtbl.iter 
    (fun _ (ci, _, _, _, _, _)  ->
       type_constr_spec_and_body ~dobody:false 
	 package_env type_env (TypeClass ci.constr_info_class) ci) 
    constructors_env


(*
Local Variables: 
compile-command: "make -C .. bin/krakatoa.byte"
End: 
*)
