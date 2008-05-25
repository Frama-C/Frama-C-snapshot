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

(* $Id: java_tast.mli,v 1.36 2008/04/11 12:38:34 marche Exp $ *)

open Java_env

type bin_op = Java_ast.bin_op
type un_op = Java_ast.un_op
type incr_decr_op = Java_ast.incr_decr_op

type term_node =
    | JTlit of literal
    | JTvar of java_var_info
(*
    | JTold of term
*)
    | JTat of term * logic_label
    | JTbin of term * base_type * bin_op * term   
    | JTun of base_type * un_op * term
    | JTapp of java_logic_info * term list
    | JTfield_access of term * java_field_info
    | JTstatic_field_access of java_type_info * java_field_info
    | JTarray_length of term 
    | JTarray_access of term * term
    | JTarray_range of term * term option * term option
    | JTcast of java_type * term

and term =
    { java_term_node : term_node;
      java_term_type : java_type;
      java_term_loc : Loc.position;
    }


type quantifier = Java_ast.quantifier

type assertion_node =
  | JAtrue
  | JAfalse
  | JAat of assertion * logic_label
  | JAnot of assertion 
  | JAand of assertion * assertion
  | JAor of assertion * assertion
  | JAimpl of assertion * assertion
  | JAiff of assertion * assertion
  | JAquantifier of quantifier * java_var_info * assertion
  | JAbool_expr of term
  | JAbin of term * base_type * bin_op * term   
  | JAbin_obj of term * bin_op * term   
  | JAapp of java_logic_info * term list
  | JAinstanceof of term * logic_label * java_type

and assertion =
    { java_assertion_node : assertion_node;
      java_assertion_loc : Loc.position;
    }

(* expressions *)

type expr =
  { java_expr_loc : Loc.position ;
    java_expr_type : java_type;
    java_expr_node : expr_node }

and expr_node =
  | JElit of literal
  | JEvar of java_var_info
  | JEbin of expr * bin_op * expr         (*r binary operations *)
  | JEun of un_op * expr                 (*r (pure) unary operations *)
  | JEif of expr * expr * expr
      (*r pre-post incr/decr operations *)
  | JEincr_local_var of incr_decr_op * java_var_info
  | JEincr_field of incr_decr_op * expr * java_field_info 
  | JEincr_array of incr_decr_op * expr * expr
  | JEstatic_field_access of java_type_info * java_field_info
  | JEfield_access of expr * java_field_info
  | JEarray_length of expr 
  | JEarray_access of expr * expr
  | JEassign_local_var of java_var_info * expr  
  | JEassign_local_var_op of java_var_info * bin_op * expr  
  | JEassign_field of expr * java_field_info * expr
  | JEassign_field_op of expr * java_field_info * bin_op * expr
  | JEassign_static_field of java_field_info * expr
  | JEassign_static_field_op of java_field_info * bin_op * expr
  | JEassign_array of expr * expr * expr
  | JEassign_array_op of expr * expr * bin_op * expr
  | JEcall of expr * method_info * expr list
  | JEconstr_call of expr * constructor_info * expr list
  | JEstatic_call of method_info * expr list
  | JEnew_array of java_type * expr list
      (*r elements type, dimensions *)
  | JEnew_object of constructor_info * expr list
      (*r constr, args *)
  | JEcast of java_type * expr
  | JEinstanceof of expr * java_type
(*
  | Static_class of class_entry
  | Static_interface of interface_entry
  | Super_method_call of identifier * pexpr list
  | Instanceof of pexpr * type_expr
      (* in annotations only *)
  | Type of type_expr 
  | Typeof of expr 
*)

(* statements *)

type initialiser =
  | JIexpr of expr
  | JIlist of initialiser list

type 'a switch_label = 'a Java_ast.switch_label
type statement =
  { java_statement_loc : Loc.position ;
    java_statement_node : statement_node }

and statement_node =
  | JSskip                  (*r empty statement *)
  | JSif of expr * statement * statement
  | JSreturn_void 
  | JSreturn of expr 
  | JSvar_decl of java_var_info * initialiser option * statement
  | JSblock of block
  | JSwhile of expr * assertion * term option * statement  
      (*r condition, invariant, variant, loop body *)
  | JSfor of expr list * expr * assertion * term option * expr list * statement  
      (*r init, condition, invariant, variant, steps, loop body *)
  | JSfor_decl of (java_var_info * initialiser option) list * 
      expr * assertion * term option * expr list * statement  
      (*r decls, condition, invariant, variant, steps, loop body *)
  | JSexpr of expr
  | JSassert of string option * assertion
  | JSswitch of expr * (expr switch_label list * block) list
  | JSbreak of string option
  | JSthrow of expr
  | JStry of block * (java_var_info * block) list * block option
(*
  | JSvar_decl of variable_declaration
  | JPScontinue of identifier option
  | JPSlabel of identifier * pstatement
  | JPSdo of pstatement * pexpr
  | JPSfor of pstatement list * pexpr * pstatement list * pstatement  
  | JPSfor_decl of variable_declaration * pexpr * pstatement list * pstatement
  | JPSsynchronized of pexpr * block
  | JPSannot of Lexing.position * string
  | JPSloop_annot of pexpr * pexpr
*)
(*
  | Kml_annot_statement of jml_annotation_statement
  | Kml_ghost_var_decl of jml_declaration list
  | Annotated of statement_specification * statement
*)

and block = statement list
;;

(*
Local Variables: 
compile-command: "make -C .. bin/krakatoa.byte"
End: 
*)
