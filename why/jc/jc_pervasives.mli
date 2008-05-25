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

open Jc_ast
open Jc_env

val ( $ ): ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

val error: Loc.position -> ('a, Format.formatter, unit, 'b) format4 -> 'a

(* labels *)

val label_var : Jc_env.logic_label -> string -> string

(* types *)

val operator_of_native: native_type -> [> native_operator_type]
val operator_of_type: jc_type -> [> operator_type]

val integer_type : Jc_env.jc_type
val boolean_type : Jc_env.jc_type
val real_type : Jc_env.jc_type
val unit_type : Jc_env.jc_type
val string_type : Jc_env.jc_type
val any_type: jc_type

val string_of_native: Jc_env.native_type -> string
val print_type : Format.formatter -> Jc_env.jc_type -> unit

val struct_of_union: Jc_env.struct_info -> bool
val field_of_union: Jc_env.field_info -> bool
val union_of_field: Jc_env.field_info -> Jc_env.variant_info
val integral_union: Jc_env.variant_info -> bool

(* constants *)

val zero: Num.num
val num_of_constant : 'a -> Jc_ast.const -> Num.num
val const : Jc_ast.const -> Jc_env.jc_type * Jc_env.region * Jc_ast.const

(* environment infos *)

val var : ?unique:bool -> ?static:bool -> ?formal:bool -> 
  Jc_env.jc_type -> string -> Jc_env.var_info
val copyvar : Jc_env.var_info -> Jc_env.var_info

val tmp_var_name : unit -> string
val newvar : Jc_env.jc_type -> Jc_env.var_info
val newrefvar : Jc_env.jc_type -> Jc_env.var_info

val exception_info :  
  Jc_env.jc_type option -> string -> Jc_env.exception_info

val make_fun_info : string -> Jc_env.jc_type -> Jc_fenv.fun_info

val make_rel : string -> Jc_fenv.logic_info

val make_logic_fun : string -> Jc_env.jc_type -> Jc_fenv.logic_info

val location_set_region : Jc_ast.tlocation_set -> Jc_env.region
(*
val direct_embedded_struct_fields : Jc_env.struct_info -> Jc_env.field_info list
val embedded_struct_fields : Jc_env.struct_info -> Jc_env.field_info list
*)
val field_sinfo : Jc_env.field_info -> Jc_env.struct_info
val field_bounds : Jc_env.field_info -> Num.num * Num.num
(*val embedded_struct_roots : Jc_env.struct_info -> string list*)

val root_name : Jc_env.struct_info -> string
val field_root_name : Jc_env.field_info -> string
val struct_variant : Jc_env.struct_info -> Jc_env.variant_info
val tag_or_variant_variant : Jc_env.tag_or_variant -> Jc_env.variant_info
val tag_or_variant_name : Jc_env.tag_or_variant -> string

(* predefined functions *)

val true_assertion : Jc_ast.assertion
val real_of_integer : Jc_fenv.logic_info
val real_of_integer_ : Jc_fenv.fun_info
val full_separated : Jc_fenv.logic_info

val default_behavior : Jc_ast.behavior

val empty_fun_effect : Jc_fenv.fun_effect
val empty_effects : Jc_fenv.effect

(* terms *)

val raw_term_equal : Jc_ast.term -> Jc_ast.term -> bool
val raw_term_compare : Jc_ast.term -> Jc_ast.term -> int

(* assertions *)

val raw_assertion_equal : Jc_ast.assertion -> Jc_ast.assertion -> bool
val make_and : Jc_ast.assertion list -> Jc_ast.assertion

val skip_term_shifts :  Jc_ast.term -> Jc_ast.term
val skip_shifts : Jc_ast.expr -> Jc_ast.expr
val skip_tloc_range : Jc_ast.tlocation_set -> Jc_ast.tlocation_set
val is_true : Jc_ast.assertion -> bool

val select_option : 'a option -> 'a -> 'a
val apply_option: ('a -> 'b) -> 'a option -> 'b option
val is_constant_assertion : Jc_ast.assertion -> bool

(* fun specs *)

val contains_normal_behavior : Jc_ast.fun_spec -> bool
val contains_exceptional_behavior : Jc_ast.fun_spec -> bool
val is_purely_exceptional_fun : Jc_ast.fun_spec -> bool

(* patterns *)

(** The set of variables bound by a pattern. *)
val pattern_vars : Jc_env.var_info Jc_envset.StringMap.t -> Jc_ast.pattern ->
  Jc_env.var_info Jc_envset.StringMap.t

(* operators *)

val string_of_op: [< bin_op | unary_op] -> string
val string_of_op_type: [< operator_type] -> string

(*
Local Variables: 
compile-command: "LC_ALL=C make -j -C .. bin/jessie.byte"
End: 
*)
