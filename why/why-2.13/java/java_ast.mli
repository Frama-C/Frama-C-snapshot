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

(***************************************************************************

Abstract syntax trees for Java source files

$Id: java_ast.mli,v 1.36 2008/05/23 13:51:39 marche Exp $

***************************************************************************)

open Java_env

(*s Qualified identifiers *)

type identifier = Loc.position * string

type qualified_ident = identifier list

(*s Modifiers *)
 
type modifier =
  | Static | Final  | Public | Private | Protected | Native 
  | Synchronized | Abstract | Transient (* "threadsafe" ? *) | Volatile
  | Strictfp 
  | Ghost | Model
  | Non_null | Nullable | Annot_modifier of Lexing.position * string

type modifiers = modifier list

(*s type expressions *)

type type_expr = 
    | Base_type of base_type
    | Type_name of qualified_ident
    | Array_type_expr of type_expr
 
(*s expressions *)

type quantifier = Forall | Exists

type variable_id =
  | Simple_id of identifier
  | Array_id of variable_id
      

type incr_decr_op = Preincr | Predecr | Postincr | Postdecr 

type bin_op = 
    | Badd | Bsub | Bmul | Bdiv | Bmod 
    | Band | Bor | Bimpl | Biff
    | Bbwand | Bbwor | Bbwxor
    | Blsl | Blsr | Basr
    | Beq | Bne | Bgt | Blt | Ble | Bge
    | Bconcat (* + for Strings *)

type un_op = Uplus | Uminus | Unot | Ucompl

type java_field_access =
  | Super_access of identifier
  | Primary_access of pexpr * identifier

and pexpr =
  { java_pexpr_loc : Loc.position ;
    java_pexpr_node : pexpr_node }

and pexpr_node =
  | JPElit of literal
  | JPEbin of pexpr * bin_op * pexpr         (*r binary operations *)
  | JPEun of un_op * pexpr                 (*r (pure) unary operations *)
  | JPEincr of incr_decr_op * pexpr (*r pre-post incr/decr operations *)
(*
  | JPEvar of identifier
*)
  | JPEname of qualified_ident 
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
(*
  | JPEfresh of expr 
  | Type of type_expr 
  | Typeof of expr 
*)
  | JPEquantifier of quantifier * (type_expr * variable_id list) list * pexpr   


(*
and set_ref = 
  | Set_array_index of expr  
      (*r e[e] *)
  | Set_array_interval of expr * expr  
      (*r e[e..e] *)
  | Set_array
      (*r e[*] *)
  | Set_field of instance_variable_entry
      (* e.f *)
  | Set_fieldraw of identifier
*)

(*s variable declarations *)
      
type variable_initializer =
  | Simple_initializer of pexpr
  | Array_initializer of variable_initializer list

type variable_declarator = 
    { variable_id : variable_id ;
      variable_initializer : variable_initializer option }
 
      
type variable_declaration =
    { variable_modifiers : modifiers ;
      variable_type : type_expr ;
      variable_decls : variable_declarator list }


(*s statements *)

type 'a switch_label =
  | Case of 'a 
  | Default

type parameter =
  | Simple_parameter of modifier option * type_expr * identifier
  | Array_parameter of parameter

type pstatement =
  { java_pstatement_loc : Loc.position ;
    java_pstatement_node : pstatement_node }

and pstatement_node =
  | JPSskip                  (*r empty statement *)
  | JPSexpr of pexpr
  | JPSvar_decl of variable_declaration
  | JPSthrow of pexpr
  | JPSreturn of pexpr option
  | JPSbreak of identifier option
  | JPScontinue of identifier option
  | JPSlabel of identifier * pstatement
  | JPSif of pexpr * pstatement * pstatement
  | JPSwhile of pexpr * pstatement
  | JPSdo of pstatement * pexpr
  | JPSfor of pexpr list * pexpr * pexpr list * pstatement  
  | JPSfor_decl of variable_declaration * pexpr * pexpr list * pstatement
  | JPStry of block * (parameter * block) list * block option
  | JPSswitch of pexpr * (pexpr switch_label list * block) list
  | JPSblock of block
  | JPSsynchronized of pexpr * block
  | JPSassert of identifier option * pexpr
  | JPSghost_local_decls of variable_declaration
  | JPSghost_statement of pexpr
  | JPSannot of Lexing.position * string
  | JPSloop_annot of pexpr * pexpr option

and block = pstatement list
;;

(*s method declarations *)

type method_declarator =
  | Simple_method_declarator of identifier * parameter list
  | Array_method_declarator of method_declarator
;;

type method_declaration =
    { method_modifiers : modifiers ;
      method_return_type : type_expr option ;
      method_declarator : method_declarator ;
      method_throws : qualified_ident list ;
    }
;;

(*s constructor declarations *)


type explicit_constructor_invocation =
  | Invoke_none
  | Invoke_this of pexpr list
  | Invoke_super of pexpr list

type constructor_declaration =
    { constr_modifiers : modifiers ;
      constr_name : identifier ;
      constr_parameters : parameter list ;
      constr_throws : qualified_ident list }
;;

      
      
      
(*s class declarations *)

type pbehavior =
    { java_pbehavior_assumes : pexpr option;
      java_pbehavior_assigns : (Loc.position * pexpr list) option;
      java_pbehavior_throws : (qualified_ident * identifier option) option;
      java_pbehavior_ensures : pexpr 
    }

type field_declaration =
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

and class_declaration =
    { 
      class_modifiers : modifiers;
      class_name : identifier;
      class_extends : qualified_ident option;
      class_implements : qualified_ident list;
      class_fields : field_declaration list
    }

(*s interface declarations *)

and interface_declaration =
    { interface_modifiers : modifiers;
      interface_name : identifier;
      interface_extends : qualified_ident list;
      interface_members : field_declaration list
    }


(*s compilation units *)

type type_declaration =
  | JPTclass of class_declaration
  | JPTinterface of interface_declaration
  | JPTannot of Lexing.position * string
  | JPTlemma of identifier * bool * logic_label list * pexpr
  | JPTlogic_type_decl of identifier 
  | JPTlogic_reads of identifier * type_expr option * logic_label list * parameter list * pexpr list 
  | JPTlogic_def of identifier * type_expr option * logic_label list * parameter list * pexpr 

type import_statement =
  | Import_package of qualified_ident
  | Import_class_or_interface of qualified_ident

type compilation_unit =
    {
      cu_package : qualified_ident;
      cu_imports : import_statement list;
      cu_type_decls : type_declaration list
    }

(*
Local Variables: 
compile-command: "make -C .. bin/krakatoa.byte"
End: 
*)
