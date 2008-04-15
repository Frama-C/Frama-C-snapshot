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

val print_qualified_ident : Format.formatter -> Java_ast.qualified_ident -> unit

val print_type_name : Format.formatter -> Java_env.java_type_info -> unit

val print_type : Format.formatter -> Java_env.java_type -> unit

val type_table : (int, Java_env.java_type_info) Hashtbl.t 

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

val methods_table : 
  (int, method_table_info) Hashtbl.t

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

val constructors_table : 
  (int, constructor_table_info) Hashtbl.t

val invariants_table :
  (int, Java_env.java_class_info * Java_env.java_var_info * 
     (Java_ast.identifier * Java_tast.assertion) list) Hashtbl.t

val static_invariants_table :
  (int, (string * Java_tast.assertion) list) Hashtbl.t

val field_initializer_table : 
  (int, Java_tast.initialiser option) Hashtbl.t

val final_field_values_table :
  (int, Num.num list) Hashtbl.t

val lemmas_table : (string,(Java_env.logic_label list * Java_tast.assertion)) Hashtbl.t


type logic_def_body =
  [ `Assertion of Java_tast.assertion
  | `Term of Java_tast.term
  | `Inductive of 
      (Java_ast.identifier * Java_env.logic_label list * Java_tast.assertion) 
	list
  | `Builtin ]

type logic_decl_body =
  [ logic_def_body 
  | `Reads of Java_tast.term list ]

val logic_defs_table : 
  (int,Java_env.java_logic_info * logic_def_body) Hashtbl.t

type axiomatic_defs =
  | Adecl of  Java_env.java_logic_info * logic_decl_body
  | Aaxiom of string * bool * Java_env.logic_label list * Java_tast.assertion
  | Atype of string

val logic_types_table : (string, Java_env.logic_type_info) Hashtbl.t
val axiomatics_table : (string, axiomatic_defs list) Hashtbl.t

val builtin_logic_symbols : Java_env.java_logic_info list ref

exception Typing_error of Loc.position * string

val catch_typing_errors : ('a -> 'b) -> 'a -> 'b

(*
val get_types : 
  Java_ast.compilation_unit -> 
  Java_env.package_info list * 
    (string * Java_env.java_type_info) list
*)

val get_types : 
  Java_env.package_info list ->
  Java_ast.compilation_unit list -> 
  Java_env.package_info list * 
    (string * Java_env.java_type_info) list

val get_bodies : 
  Java_env.package_info list ->
  (string * Java_env.java_type_info) list ->
  Java_ast.compilation_unit -> unit

val type_specs : 
  Java_env.package_info list ->
  (string * Java_env.java_type_info) list ->
  unit



(*
Local Variables: 
compile-command: "make -j -C .. bin/krakatoa.byte"
End: 
*)
