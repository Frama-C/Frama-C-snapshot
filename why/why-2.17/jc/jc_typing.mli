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
open Jc_ast
open Jc_fenv

val default_label : label list -> label option
 
val typing_error : 
    Loc.position -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val is_root_struct : struct_info -> bool

val substruct : struct_info -> pointer_class -> bool

val logic_type_table : (string,string) Hashtbl.t
  

val logic_constants_table : 
  (int, logic_info * Jc_fenv.logic_info Jc_ast.term) Hashtbl.t

val logic_functions_table : 
  (int, logic_info * term_or_assertion) Hashtbl.t

val functions_table : 
  (int, fun_info * Loc.position * fun_spec * expr option) Hashtbl.t

val variables_table : 
  (int, var_info * expr option) Hashtbl.t

val structs_table : 
  (string, (struct_info * (logic_info * assertion) list)) Hashtbl.t

val roots_table :
  (string, root_info) Hashtbl.t

val enum_types_table : 
  (string, (enum_info (* * logic_info * fun_info * fun_info *))) Hashtbl.t

(*
val enum_conversion_functions_table : (fun_info, string) Hashtbl.t
val enum_conversion_logic_functions_table : (logic_info, string) Hashtbl.t
*)

val lemmas_table : 
  (string, Loc.position * bool * label list * assertion) Hashtbl.t

type axiomatic_decl =
  | ABaxiom of Loc.position * string * Jc_env.label list * Jc_constructors.assertion

type axiomatic_data = private
    {
      mutable axiomatics_defined_ids : logic_info list;
      mutable axiomatics_decls : axiomatic_decl list;
    }

val axiomatics_table : (string, axiomatic_data) Hashtbl.t

val global_invariants_table : 
  (logic_info, assertion) Hashtbl.t

val exceptions_table : 
  (string, exception_info) Hashtbl.t

exception Typing_error of Loc.position * string

val coerce : jc_type -> native_type -> expr -> expr

val type_range_of_term : jc_type -> term -> assertion

val find_struct_root : Jc_env.struct_info -> Jc_env.root_info

val type_file : nexpr decl list -> unit

val print_file : Format.formatter -> unit -> unit

val type_labels_in_decl : nexpr decl -> unit

(*
Local Variables: 
compile-command: "make -C .. bin/jessie.byte"
End: 
*)
