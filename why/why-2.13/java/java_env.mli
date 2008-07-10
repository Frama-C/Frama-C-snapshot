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

(* $Id: java_env.mli,v 1.38 2008/05/23 13:51:39 marche Exp $ *)

(*s types and environments *)

type accessibility = Apublic | Aprotected | Aprivate | Anone

type base_type =
    | Tshort | Tboolean | Tbyte | Tchar | Tint | Tfloat | Tlong | Tdouble 
	  (* native logic types *)
    | Tinteger | Treal | Tunit | Tstring

type logic_label = 
  | LabelName of string
  | LabelHere
  | LabelOld
  | LabelPre

type java_type =
  | JTYbase of base_type
  | JTYnull (*r type of 'null' *)
  | JTYclass of bool * java_class_info (*r first arg true if non_null *)
  | JTYinterface of interface_info 
  | JTYarray of bool * java_type (*r first arg true if non_null *)
  | JTYlogic of logic_type_info
      
and logic_type_info = string


and java_var_info =
    {
      java_var_info_tag : int;
      java_var_info_name : string;
      java_var_info_decl_loc : Loc.position;
      java_var_info_type : java_type;
      mutable java_var_info_final_name : string;
      mutable java_var_info_assigned : bool;
    }
    
and java_field_info =
    {
      java_field_info_tag : int;
      java_field_info_name : string;
      java_field_info_class_or_interface : java_type_info;
(*
      mutable java_field_info_trans_name : string;
      java_field_info_accessibility : accessibility;
*)
      java_field_info_is_static : bool;
      java_field_info_is_final : bool;
      java_field_info_is_nullable : bool;
      java_field_info_type : java_type;
      java_field_info_is_ghost : bool;
      java_field_info_is_model : bool;
    }
    

and method_info = 
    {
      method_info_tag : int;
      method_info_loc : Loc.position;
      method_info_name : string;
      mutable method_info_trans_name : string;
      method_info_is_static : bool;
      (*
	method_info_accessibility : accessibility;
      *)
      method_info_class_or_interface : java_type_info;
      mutable method_info_has_this : java_var_info option;
      method_info_parameters : (java_var_info * (* nullable *) bool) list;
      method_info_result : java_var_info option ;
      method_info_result_is_nullable : bool ;
      mutable method_info_calls : method_or_constructor_info list;
    }
      
and constructor_info = 
    {
      constr_info_tag : int;
      constr_info_loc : Loc.position;
      constr_info_class : java_class_info;
      mutable constr_info_trans_name : string;
      mutable constr_info_this : java_var_info option;
      mutable constr_info_result : java_var_info option;
      constr_info_parameters : (java_var_info * (* nullable : *) bool) list;
      mutable constr_info_calls : method_or_constructor_info list;
    }

and method_or_constructor_info =
  | MethodInfo of method_info
  | ConstructorInfo of constructor_info

    
and logic_type_entry =
    {
      logic_type_entry_name : string
    }

and java_logic_info =
    {
      java_logic_info_name : string;
      java_logic_info_tag : int;
      java_logic_info_result_type : java_type option;
      java_logic_info_labels : logic_label list;
      java_logic_info_parameters : java_var_info list;
      mutable java_logic_info_calls : java_logic_info list;
    }


and axiom_info = 
    {
      axiom_info_name : string;
    }
    


and package_info =
    {
      package_info_tag : int;
      package_info_name : string;
      package_info_directory : string;
    }
    
and java_class_info =
    {
      class_info_tag : int;
      class_info_name : string;
      class_info_package_env : package_info list;
      mutable class_info_incomplete : bool;
      mutable class_info_extends : java_class_info option;
      mutable class_info_is_exception : bool;
      mutable class_info_implements : interface_info list;
      mutable class_info_fields : java_field_info list;
      mutable class_info_methods : method_info list;
      mutable class_info_constructors : constructor_info list;
    }

and interface_info =
    {
      interface_info_tag : int;
      interface_info_name : string;
      interface_info_package_env : package_info list;
      mutable interface_info_incomplete : bool;
      mutable interface_info_extends : interface_info list;
      mutable interface_info_fields : java_field_info list;
      mutable interface_info_methods : method_info list;
    }

and java_type_info =
  | TypeClass of java_class_info
  | TypeInterface of interface_info

(*s literals, shared between ASTs and typed ASTs *)

type literal =
    | Integer of string
    | Float of string
    | Bool of bool
    | String of string
    | Char of string
    | Null

type nonnull_policy =
  | NonNullNone     (* Java semantics *)
  | NonNullFields   (* class fields non-null by default *)
  | NonNullAll      (* class fields and methods parameters & return value non-null by default *)
  | NonNullAllLocal (* class fields, methods parameters & return value, and local variables non-null by default *)


(*
  Local Variables: 
  compile-command: "make -C .. bin/krakatoa.byte"
  End: 
*)
