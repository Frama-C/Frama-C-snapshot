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

(** Add a type declaration like [type t = ...] so that type [t] is known and
can be instanciated. *)
val declare: Ml_ocaml.Ident.t -> Ml_ocaml.Types.type_declaration -> bool -> unit

(** Add an invariant to a declared type. *)
val add_invariant: Ml_ocaml.Ident.t ->
  (string * Jc_env.var_info * Jc_ast.assertion) -> unit

(** Translate an OCaml type into a Jessie type. May instanciate the type if
needed. *)
val make: Ml_ocaml.Types.type_expr -> Jc_env.jc_type

(** If the argument is a record type or a tuple, return the structure used
to interpret it. Fail otherwise. *)
val structure: Ml_ocaml.Types.type_expr -> Jc_env.struct_info

type ml_label_info = {
  ml_li_name: string;
  ml_li_structure: Jc_env.struct_info;
  ml_li_field: Jc_env.field_info;
}

(** Given a record type and a label of this record, instantiate the type if
needed and return the label interpretation. *)
val label: Ml_ocaml.Types.type_expr -> Ml_ocaml.Types.label_description ->
  ml_label_info

type ml_constructor_info = {
  ml_ci_name: string;
  ml_ci_structure: Jc_env.struct_info;
  ml_ci_arguments: Jc_env.field_info list;
}

(** Given a variant type and a tag of this record, instantiate the type if
needed and return the tag interpretation. *)
val constructor: Ml_ocaml.Types.type_expr -> 
  Ml_ocaml.Types.constructor_description -> ml_constructor_info

(** Return the field associated to some tuple projection. *)
val proj: Ml_ocaml.Types.type_expr -> int -> Jc_env.field_info

type ml_array_info = {
  ml_ai_struct: Jc_env.struct_info;
  ml_ai_data_field: Jc_env.field_info;
  ml_ai_make: Jc_fenv.fun_info;
}

(** Given the argument type of an array, instantiate the array if needed and
return the array info. *)
val array: Ml_ocaml.Types.type_expr -> ml_array_info

(** Return the declarations for all type instantiations. *)
val jc_decls: unit -> Jc_output.jc_decl list

(*
Local Variables: 
compile-command: "unset LANG; make -C .. -f build.makefile jessica.all"
End: 
*)
