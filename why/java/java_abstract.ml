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

open Format
open Pp
open Java_env
open Java_ast

let ident fmt (_,id) = fprintf fmt "%s" id

let rec qualified_ident fmt qid =
  match qid with
    | [id] -> fprintf fmt "%a" ident id
    | id::l -> fprintf fmt "%a.%a" ident id qualified_ident l
    | [] -> assert false


let base_type fmt b =
  match b with
    | Tshort -> fprintf fmt "short"
    | Tboolean -> fprintf fmt "boolean"
    | Tbyte -> fprintf fmt "byte"
    | Tchar -> fprintf fmt "char"
    | Tint -> fprintf fmt "int"
    | Tfloat -> fprintf fmt "float"
    | Tlong -> fprintf fmt "long"
    | Tdouble -> fprintf fmt "double"
	  (* native logic types *)
    | Tinteger | Treal | Tunit | Tstring ->  assert false (* TODO *)

let modifier fmt m =
  match m with
  | Static -> fprintf fmt "static "
  | Final -> fprintf fmt "final "
  | Public -> fprintf fmt "public "
  | Private -> fprintf fmt "private "
  | Protected -> fprintf fmt "protected "
  | Native -> fprintf fmt "native "
  | Synchronized -> fprintf fmt "synchronized "
  | Abstract -> assert false (* TODO *)
  | Transient (* "threadsafe" ? *) -> assert false (* TODO *)
  | Volatile-> assert false (* TODO *)
  | Strictfp -> assert false (* TODO *)
  | Ghost -> assert false (* TODO *)
  | Model-> assert false (* TODO *)
  | Non_null -> assert false (* TODO *)
  | Nullable -> assert false (* TODO *)
  | Annot_modifier(loc,id) -> assert false (* TODO *)

let modifiers fmt = List.iter (modifier fmt)

let type_expr fmt t =
  match t with
    | Base_type b -> base_type fmt b
    | Type_name id -> fprintf fmt "%a" qualified_ident id;
    | Array_type_expr typ ->  assert false (* TODO *)

let float_suffix fmt = function
  | `Single -> fprintf fmt "f"
  | `Double -> fprintf fmt "d"
  | `Real -> ()

let literal fmt l =
  match l with
    | Null -> fprintf fmt "null"
    | Integer s -> assert false (* TODO *)
    | Float(s,suf) -> fprintf fmt "%s%a" s float_suffix suf
    | Bool b -> assert false (* TODO *)
    | String s -> assert false (* TODO *)
    | Char s -> assert false (* TODO *)

let bin_op fmt op =
  match op with
    | Badd -> fprintf fmt "+"
    | Bsub | Bmul | Bdiv | Bmod -> assert false (* TODO *)
    | Band | Bor | Bimpl | Biff -> assert false (* TODO *)
    | Bbwand | Bbwor | Bbwxor -> assert false (* TODO *)
    | Blsl | Blsr | Basr -> assert false (* TODO *)
    | Beq -> assert false (* TODO *)
    | Bne -> fprintf fmt "!="
    | Bgt -> fprintf fmt ">"
    | Blt -> fprintf fmt "<"
    | Ble -> fprintf fmt "<="
    | Bge -> fprintf fmt ">="
    | Bconcat -> assert false (* TODO *) (* + for Strings *)


let rec expr fmt e =
  match e.java_pexpr_node with
    | JPElit l -> literal fmt l
    | JPEbin(e1,op,e2) ->
	fprintf fmt "(%a %a %a)" expr e1 bin_op op expr e2
    | JPEname qid -> qualified_ident fmt qid
	
	
    | _ -> assert false (* TODO *)
(*
    | JPEun of un_op * pexpr                 (*r (pure) unary operations *)
    | JPEincr of incr_decr_op * pexpr (*r pre-post incr/decr operations *)
    | JPEassign_name of qualified_ident * bin_op * pexpr  
    | JPEassign_field of java_field_access * bin_op * pexpr  
    | JPEassign_array of pexpr * pexpr * bin_op * pexpr  
	(*r [Assign_array(e1,e2,op,e3)] is [e1[e2] op e3] *)
	(*r assignment op is =, +=, etc. *)
    | JPEif of pexpr * pexpr * pexpr
    | JPEthis
    | JPEfield_access of java_field_access
    | JPEcall_name of qualified_ident * pexpr list
    | JPEcall_expr of pexpr * identifier * pexpr list
    | JPEsuper_call of identifier * pexpr list
    | JPEnew of qualified_ident * pexpr list
    | JPEnew_array of type_expr * pexpr list 
	(*r type, explicit dimensions *)
    | JPEarray_access of pexpr * pexpr
    | JPEarray_range of pexpr * pexpr option * pexpr option
    | JPEcast of type_expr * pexpr
    | JPEinstanceof of pexpr * type_expr
	(* in annotations only *)
    | JPEresult  
    | JPEold of pexpr 
    | JPEat of pexpr * identifier 
    | JPEquantifier of quantifier * (type_expr * variable_id list) list * pexpr   
*)

let rec param fmt p =
  match p with
    | Simple_parameter(modifier, typ, id) ->
	fprintf fmt "%a %a" type_expr typ ident id
    | Array_parameter p -> 
	fprintf fmt "%a[]" param p

let method_declarator fmt d =
  match d with
    | Simple_method_declarator(id,params) ->
	fprintf fmt "%a(@[%a@]);@\n@\n" ident id (print_list comma param) params
    | Array_method_declarator md ->
	assert false (* TODO *)


let method_declaration fmt md =
  (*
    method_modifiers : modifiers ;
    method_return_type : type_expr option ;
    method_declarator : method_declarator ;
    method_throws : qualified_ident list ;
  *)
  modifiers fmt md.method_modifiers;
  begin
    match md.method_return_type with
      | None -> fprintf fmt "void "
      | Some t -> fprintf fmt "%a " type_expr t;
  end;
  assert (md.method_throws = []);
  method_declarator fmt md.method_declarator
  
let constructor_declaration fmt cd =
  (* constr_modifiers : modifiers ;
     constr_name : identifier ;
     constr_parameters : parameter list ;
     constr_throws : qualified_ident list 
  *)
  modifiers fmt cd.constr_modifiers;
  assert (cd.constr_throws = []);
  fprintf fmt "%a(%a);@\n@\n" ident cd.constr_name 
    (print_list comma param) cd.constr_parameters
  
let rec variable_id fmt = function 
    | Simple_id(id) ->
	fprintf fmt "%a" ident id
    | Array_id vd -> 
	fprintf fmt "%a[]" variable_id vd

let rec initialize fmt = function
  | Simple_initializer e -> expr fmt e
  | Array_initializer l -> assert false (* TODO *)
      

let variable_declarator ~is_final fmt vd =
  fprintf fmt "%a" variable_id vd.variable_id;
  match vd.variable_initializer with
    | Some init when is_final ->
	fprintf fmt " = %a" initialize init
    | _ -> ()

let variable_declaration fmt vd =
  (*
     variable_modifiers : modifiers ;
    variable_type : type_expr ;
    variable_decls : variable_declarator list 
  *)
  modifiers fmt vd.variable_modifiers;
  fprintf fmt "%a " type_expr vd.variable_type;
  let is_final = List.mem Final vd.variable_modifiers in
  List.iter (variable_declarator ~is_final fmt) vd.variable_decls;
  fprintf fmt ";@\n@\n"

  
  

let field_declaration fmt f =
(*
  | JPFmethod of method_declaration * block option
  | JPFconstructor of constructor_declaration *
      explicit_constructor_invocation * block
  | JPFvariable of variable_declaration
  | JPFstatic_initializer of block
  | JPFannot of Lexing.position * string
  | JPFinvariant of identifier * pexpr
  | JPFstatic_invariant of identifier * pexpr
  | JPFmethod_spec of 
      pexpr option * pexpr option * (identifier * pbehavior) list
  | JPFclass of class_declaration
  | JPFinterface of interface_declaration
*)
  match f with
    | JPFmethod(md,_block) -> method_declaration fmt md      
    | JPFmethod_spec(req, dec, behs ) -> 
	fprintf fmt "@[/*@@";
	print_option (fun fmt e -> fprintf fmt " requires %a;@\n  @@" expr e) fmt req;
	(* TODO *)
	fprintf fmt "*/@\n@]"
    | JPFinterface _ -> assert false (* TODO *)
    | JPFclass _ -> assert false (* TODO *)
    | JPFstatic_invariant (_, _) -> assert false (* TODO *)
    | JPFinvariant (_, _) -> assert false (* TODO *)
    | JPFannot (_, _) -> assert false (* TODO *)
    | JPFstatic_initializer block -> ()	
    | JPFvariable vd -> variable_declaration fmt vd
    | JPFconstructor(cd, _invoke, _block) -> 
	constructor_declaration fmt cd

let class_declaration fmt cd =
  fprintf fmt "@[%a class %a " 
    modifiers cd.class_modifiers ident cd.class_name;
  begin
    match cd.class_extends with
      | None -> ()
      | Some id -> fprintf fmt "extends %a " qualified_ident id
  end;
  assert (cd.class_implements = []);
  fprintf fmt "{@\n@\n  @[<v 0>";
  List.iter (field_declaration fmt) cd.class_fields;
  fprintf fmt "@]@\n}@\n@]"

let interface_declaration fmt id =
  (*
    { interface_modifiers : modifiers;
    interface_name : identifier;
    interface_extends : qualified_ident list;
    interface_members : field_declaration list
  *)
  fprintf fmt "@[%a interface %a {@\n@\n  @[<v 0>" 
    modifiers id.interface_modifiers ident id.interface_name;
  assert (id.interface_extends = []);
  List.iter (field_declaration fmt) id.interface_members;
  fprintf fmt "@]@\n}@\n@]"

let type_declaration fmt d =
  match d with
    | JPTclass cd -> class_declaration fmt cd
    | JPTinterface id -> interface_declaration fmt id
    | JPTannot _ -> assert false
    | JPTlemma (id,is_axiom,labels,p) -> assert false
    | JPTlogic_type_decl id -> assert false
    | JPTlogic_reads(id,rettype,labels,params,reads) -> assert false
    | JPTlogic_def(id,rettype,labels,param,e) -> assert false
    | JPTinductive(id,labels,param,e) -> assert false
    | JPTaxiomatic _ -> assert false
	

let compilation_unit fmt cu =
  (*
    cu_package : qualified_ident;
    cu_imports : import_statement list;
  *)
  List.iter (type_declaration fmt) cu.cu_type_decls

