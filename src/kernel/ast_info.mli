(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
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
(** {2 Expressions} *)
(* ************************************************************************** *)

val is_integral_const: constant -> bool
val possible_value_of_integral_const: constant -> Integer.t option
val possible_value_of_integral_expr: exp -> Integer.t option
val value_of_integral_const: constant -> Integer.t
val value_of_integral_expr: exp -> Integer.t
val constant_expr: loc:location -> Integer.t -> exp
val is_null_expr: exp -> bool
val is_non_null_expr: exp -> bool

(* ************************************************************************** *)
(** {2 Logical terms} *)
(* ************************************************************************** *)

val is_integral_logic_const: logic_constant -> bool
(** @return [true] if the constant has integral type [(integer, char,
    enum)]. [false] otherwise.
    @since Oxygen-20120901 *)

val possible_value_of_integral_logic_const: logic_constant -> Integer.t option
(** @return [Some n] if the constant has integral type [(integer, char,
    enum)]. [None] otherwise.
    @since Oxygen-20120901 *)

val value_of_integral_logic_const: logic_constant -> Integer.t
(** @return the value of the constant.
    Assume the argument is an integral constant.
    @since Oxygen-20120901 *)

val possible_value_of_integral_term: term -> Integer.t option
(** @return [Some n] if the term has integral type [(integer, char,
    enum)]. [None] Otherwise.
    @since Oxygen-20120901 *)

val term_lvals_of_term: term -> term_lval list
  (** @return the list of all the term lvals of a given term.
      Purely syntactic function. *)

val precondition : funspec -> predicate named
  (** Builds the precondition from [b_assumes] and [b_requires] clauses. 
      @since Carbon-20101201 *)

val behavior_assumes : funbehavior -> predicate named
  (** Builds the conjonction of the [b_assumes].
      @since Nitrogen-20111001 *)
                                        
val behavior_precondition : funbehavior -> predicate named
  (** Builds the precondition from [b_assumes] and [b_requires] clauses. 
      @since Carbon-20101201 *)

val behavior_postcondition : funbehavior -> termination_kind -> predicate named
  (** Builds the postcondition from [b_assumes] and [b_post_cond] clauses. 
      @modify Boron-20100401 added termination kind as filtering argument. *)

val disjoint_behaviors : funspec -> string list -> predicate named
  (** Builds the [disjoint_behaviors] property for the behavior names.
      @since Nitrogen-20111001 *)

val complete_behaviors : funspec -> string list -> predicate named
  (** Builds the [disjoint_behaviors] property for the behavior names.
      @since Nitrogen-20111001 *)

val merge_assigns_from_complete_bhvs: 
  ?warn:bool -> ?ungarded:bool -> funbehavior list -> string list list -> identified_term assigns
  (** @return the assigns of an unguarded behavior (when [ungarded]=true) 
      or a set of complete behaviors.
      - the funbehaviors can come from either a statement contract or a function
      contract. 
      - the list of sets of behavior names can come from the contract of the
      related function.
      Optional [warn] argument can be used to force emmiting or cancelation of 
      warnings.
      @since Oxygen-20120901 *)

val merge_assigns_from_spec: ?warn:bool -> funspec -> identified_term assigns
(** It is a shortcut for [merge_assigns_from_complete_bhvs
    spec.spec_complete_behaviors spec.spec_behavior].  Optional [warn] argument
    can be used to force emmiting or cancelation of warnings 
    @return the assigns of an unguarded behavior or a set of complete behaviors.
    @since Oxygen-20120901 *) 

val merge_assigns: ?warn:bool -> funbehavior list -> identified_term assigns
(** Returns the assigns of an unguarded behavior. 
    @modify Oxygen-20120901 Optional [warn] argument added which can be used to
    force emmiting or cancelation of warnings. *) 

val variable_term: location -> logic_var -> term
val constant_term: location -> Integer.t -> term
val is_null_term: term -> bool


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

val direct_array_size: typ -> Integer.t
val array_size: typ -> Integer.t
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
val is_cea_dump_file_function : string -> bool
val is_frama_c_builtin : string -> bool

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
