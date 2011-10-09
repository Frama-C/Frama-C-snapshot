(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

(** AST manipulation utilities.
    @plugin development guide *)

open Cil_types

(* ************************************************************************** *)
(** {2 Annotations} *)
(* ************************************************************************** *)

val lift_annot_func:
  (code_annotation -> 'a) -> rooted_code_annotation -> 'a
  (** lifts a function that operates on code_annotation up to the
      annotations used in Db. *)

val lift_annot_list_func:
  (code_annotation list -> 'a) -> rooted_code_annotation list -> 'a
  (** lifts a function taking lists of code_annotation up to the annotations
      lists in Db. Ignores WP annotations. *)

(* ************************************************************************** *)
(** {2 Expressions} *)
(* ************************************************************************** *)

val is_integral_const: constant -> bool
val possible_value_of_integral_const: constant -> My_bigint.t option
val possible_value_of_integral_expr: exp -> My_bigint.t option
val value_of_integral_const: constant -> My_bigint.t
val value_of_integral_expr: exp -> My_bigint.t
val constant_expr: loc:location -> My_bigint.t -> exp
val is_null_expr: exp -> bool
val is_non_null_expr: exp -> bool

(* ************************************************************************** *)
(** {2 Logical terms} *)
(* ************************************************************************** *)

val possible_value_of_integral_term: term -> My_bigint.t option

val term_lvals_of_term: term -> term_lval list
  (** Return the list of all the term lvals of a given term.
      Purely syntactic function. *)

val is_trivial_predicate: predicate -> bool
val is_trivial_rooted_assertion: rooted_code_annotation -> bool
val is_trivial_named_predicate: predicate named -> bool

val precondition : funspec -> predicate named
  (** @since Carbon-20101201
      Builds the precondition from [b_assumes] and [b_requires] clauses. *)

val behavior_assumes : funbehavior -> predicate named
  (** @since Nitrogen-20111001
      Builds the conjonction of the [b_assumes] *)
                                        
val behavior_precondition : funbehavior -> predicate named
  (** @since Carbon-20101201
      Builds the precondition from [b_assumes] and [b_requires] clauses. *)

val behavior_postcondition : funbehavior -> termination_kind -> predicate named
  (** @modify Boron-20100401 added termination kind as filtering argument.
      Builds the postcondition from [b_assumes] and [b_post_cond] clauses. *)

val disjoint_behaviors : funspec -> string list -> predicate named
  (** @since Nitrogen-20111001
      Builds the [disjoint_behaviors] property for the behavior names *)

val complete_behaviors : funspec -> string list -> predicate named
  (** @since Nitrogen-20111001
      Builds the [disjoint_behaviors] property for the behavior names *)

val merge_assigns: funbehavior list -> identified_term assigns
  (** Returns the assigns of an unguarded behavior. *)

val variable_term: location -> logic_var -> term
val constant_term: location -> My_bigint.t -> term
val is_null_term: term -> bool

(* ************************************************************************** *)
(** {2 Predicates} *)
(* ************************************************************************** *)

val predicate: location -> predicate -> predicate named

(* ************************************************************************** *)
(** {2 Statements} *)
(* ************************************************************************** *)

val is_loop_statement: stmt -> bool
val get_sid: kinstr -> int

val mkassign: lval -> exp -> location -> instr
val mkassign_statement: lval -> exp -> location -> stmt

(** determines if a var is local to a block. *)
val is_block_local: varinfo -> block -> bool

(* ************************************************************************** *)
(** {2 Types} *)
(* ************************************************************************** *)

val array_type: ?length:exp -> ?attr:attributes -> typ -> typ

val direct_array_size: typ -> My_bigint.t
val array_size: typ -> My_bigint.t
val direct_element_type: typ -> typ
val element_type: typ -> typ
val direct_pointed_type: typ -> typ
val pointed_type: typ -> typ

(* ************************************************************************** *)
(** {2 Functions} *)
(* ************************************************************************** *)

val is_function_type : varinfo -> bool
  (** Return [true] iff the type of the given varinfo is a function type. *)

(** Operations on cil function. *)
module Function: sig
  val formal_args: varinfo -> (string * typ * attributes) list
    (** Returns the list of the named formal arguments of a function.
        Never call on a variable of non functional type.*)

  val is_formal: varinfo -> fundec -> bool
  val is_local: varinfo -> fundec -> bool
  val is_formal_or_local: varinfo -> fundec -> bool
  val is_formal_of_prototype:
    varinfo (* to check *) -> varinfo (* of the prototype *) -> bool
    (** [is_formal_of_prototype v f] returns [true] iff [f] is a prototype and
        [v] is one of its formal parameters. *)

  val is_definition: cil_function -> bool
  val get_vi: cil_function -> varinfo
  val get_name: cil_function -> string
  val get_id: cil_function -> int
end

(* ************************************************************************** *)
(** {2 Predefined} *)
(* ************************************************************************** *)

val can_be_cea_function : string -> bool
val is_cea_function : string -> bool
val is_cea_dump_function : string -> bool
val is_cea_alloc_with_validity : string -> bool
val is_cea_dump_function : string -> bool
val is_cea_dump_file_function : string -> bool
val is_frama_c_builtin : string -> bool


(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
