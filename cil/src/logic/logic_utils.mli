(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA   (Commissariat à l'énergie atomique et aux énergies            *)
(*           alternatives)                                                *)
(*    INRIA (Institut National de Recherche en Informatique et en         *)
(*           Automatique)                                                 *)
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
(*  See the GNU Lesser General Public License version v2.1                *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Cil_types

(** exception raised when a parsed logic expression is
    syntactically not well-formed. *)
exception Not_well_formed of Cil_types.location * string

(** basic utilities for logic terms and predicates. See also {! Logic_const}
    to build terms and predicates.
    @plugin development guide *)

(** add a logic function in the environment.
    See {!Logic_env.add_logic_function_gen}*)
val add_logic_function : logic_info -> unit

(** creates a new term
    @deprecated since Carbon-20101201 Use {!Logic_const.term} instead.
*)
val mk_dummy_term: term_node -> typ -> term

(** {2 Types} *)

(** instantiate type variables in a logic type. *)
val instantiate :
  (string * logic_type) list ->
  logic_type -> logic_type

(** expands logic type definitions. If the [unroll_typedef] flag is set to
    [true] (this is the default), C typedef will be expanded as well. *)
val unroll_type : ?unroll_typedef:bool -> logic_type -> logic_type

(** computes a type signature for a C type and removes attributes that
    are not meaningful for the logic. See {!Cil.typeSig} for more information.
 *)
val type_sig_logic : typ -> typsig

(** [isLogicType test typ] is [false] for pure logic types and the result
    of test for C types.
*)
val isLogicType : (typ -> bool) -> logic_type -> bool

(** {3 Predefined tests over types} *)
val isLogicArrayType : logic_type -> bool
val isLogicCharType : logic_type -> bool
val isLogicVoidType : logic_type -> bool
val isLogicPointerType : logic_type -> bool
val isLogicVoidPointerType : logic_type -> bool

(** {3 Type conversions} *)

(** @return the equivalent C type.
    @raise Failure if the type is purely logical *)
val logicCType : logic_type -> typ

(** transforms an array into pointer. *)
val array_to_ptr : logic_type -> logic_type

(** C type to logic type, with implicit conversion for arithmetic types. *)
val typ_to_logic_type : typ -> logic_type

(** {2 Predicates} *)

val named_of_identified_predicate: identified_predicate -> predicate named

(** transforms \old and \at(,Old) into \at(,L) for L a label pointing
 to the given statement, creating one if needed. *)
val translate_old_label: stmt -> predicate named -> predicate named

(** {2 Terms} *)

(** creates a cast. *)
val insert_logic_cast :
  typ -> term_node -> term_node

(** [true] if the term denotes a C array. *)
val is_C_array : term -> bool

(** creates a TStartOf from an TLval. *)
val mk_logic_StartOf : term -> term

(** [true] if the term is a pointer. *)
val isLogicPointer : term -> bool

(** creates either a TStartOf or the corresponding TLval. *)
val mk_logic_pointer_or_StartOf : term -> term

(** [array_with_range arr size] returns the logic term [array'+{0..(size-1)}],
    [array'] being [array] cast to a pointer to char *)
val array_with_range: exp -> term -> term


(** {3 Conversion from exp to term}*)
(** translates a C expression into an "equivalent" logical term.
    If cast is [true]: expressions with integral type are cast to corresponding
    C type. If cast is [false]: no cast performed to C type, except for
    constants since there are no logic integer constants for the time being =>
    they keep their C type.
    @plugin development guide *)
val expr_to_term : cast:bool -> exp -> term
val lval_to_term_lval : cast:bool -> lval -> term_lval
val host_to_term_host : cast:bool -> lhost -> term_lhost
val offset_to_term_offset :
  cast:bool -> offset -> term_offset

(** [remove_term_offset o] returns [o] without its last offset and
    this last offset. *)
val remove_term_offset :
  term_offset -> term_offset * term_offset

(** true if \result is included in the lval. *)
val lval_contains_result : term_lhost -> bool

(** true if \result is included in the offset. *)
val loffset_contains_result : term_offset -> bool

(** true if \result is included in the term *)
val contains_result : term -> bool

(** returns the body of the given predicate.
    @raise Not_found if the logic_info is not the definition of a predicate.
*)
val get_pred_body :
  logic_info -> predicate named

(** true if the term is \result or an offset of \result.
    @deprecated since Carbon-20101201 use Logic_const.is_result instead
*)
val is_result : term -> bool

val lhost_c_type : term_lhost -> typ

(** {2 Structural equality between annotations} *)

val is_same_logic_label :
  logic_label -> logic_label -> bool
val is_same_type : logic_type -> logic_type -> bool
val is_same_var : logic_var -> logic_var -> bool
val is_same_logic_signature :
  logic_info -> logic_info -> bool
val is_same_logic_profile :
  logic_info -> logic_info -> bool
val is_same_builtin_profile :
  builtin_logic_info -> builtin_logic_info -> bool
val is_same_logic_ctor_info :
  logic_ctor_info -> logic_ctor_info -> bool
val is_same_constant : constant -> constant -> bool
val is_same_term : term -> term -> bool
val is_same_logic_info : logic_info -> logic_info -> bool
val is_same_logic_body : logic_body -> logic_body -> bool
val is_same_indcase :
  string * logic_label list * string list *
  predicate named ->
  string * logic_label list * string list *
  predicate named -> bool
val is_same_tlval : term_lval -> term_lval -> bool
val is_same_lhost : term_lhost -> term_lhost -> bool
val is_same_offset : term_offset -> term_offset -> bool
val is_same_predicate : predicate -> predicate -> bool
val is_same_named_predicate :
  predicate named ->
  predicate named -> bool
val is_same_identified_predicate :
  identified_predicate -> identified_predicate -> bool
val is_same_identified_term :
  identified_term -> identified_term -> bool
val is_same_deps :
  identified_term deps ->
  identified_term deps -> bool
val is_same_assigns :
  identified_term assigns -> identified_term assigns -> bool
val is_same_variant : term variant -> term variant -> bool
val is_same_post_cond :
  termination_kind * identified_predicate ->
  termination_kind * identified_predicate -> bool
val is_same_behavior : funbehavior -> funbehavior -> bool
val is_same_spec : funspec -> funspec -> bool
val is_same_logic_type_def :
  logic_type_def -> logic_type_def -> bool
val is_same_logic_type_info :
  logic_type_info -> logic_type_info -> bool
val is_same_loop_pragma :
  term loop_pragma ->
  term loop_pragma -> bool
val is_same_slice_pragma :
  term slice_pragma ->
  term slice_pragma -> bool
val is_same_impact_pragma :
  term impact_pragma ->
  term impact_pragma -> bool
val is_same_pragma :
  term pragma -> term pragma -> bool
val is_same_code_annotation : code_annotation -> code_annotation -> bool
val is_same_global_annotation : global_annotation -> global_annotation -> bool
val is_same_axiomatic :
  global_annotation list -> global_annotation list -> bool

val is_same_lexpr: Logic_ptree.lexpr -> Logic_ptree.lexpr -> bool

(** {2 Merging contracts} *)

val get_behavior_names : ('a, 'b, 'c) spec -> string list

val merge_assigns :
  identified_term assigns ->
  identified_term assigns -> identified_term assigns

val merge_behaviors :
  silent:bool -> funbehavior list -> funbehavior list -> funbehavior list

val merge_funspec :
  ?silent_about_merging_behav:bool -> funspec -> funspec -> unit

(** {2 Discriminating code_annotations} *)
(** Functions below allows to test a special kind of code_annotation.
    Use them in conjunction with {!Annotations.get_filter} to retrieve
    a particular kind of annotations associated to a statement.
 *)

val is_assert : code_annotation -> bool
val is_contract : code_annotation -> bool
val is_stmt_invariant : code_annotation -> bool
val is_loop_invariant : code_annotation -> bool
val is_invariant : code_annotation -> bool
val is_variant : code_annotation -> bool
val is_assigns : code_annotation -> bool
val is_pragma : code_annotation -> bool
val is_loop_pragma : code_annotation -> bool
val is_slice_pragma : code_annotation -> bool
val is_impact_pragma : code_annotation -> bool
val is_loop_annot : code_annotation -> bool
val extract_loop_pragma :
  code_annotation list -> term loop_pragma list
val extract_contract :
  code_annotation list -> funspec list

(** {2 Parsing hackery} *)
(** Values that control the various modes of the parser and lexer for logic.
    Use with care.
*)

val kw_c_mode : bool ref
val enter_kw_c_mode : unit -> unit
val exit_kw_c_mode : unit -> unit
val is_kw_c_mode : unit -> bool
val rt_type_mode : bool ref
val enter_rt_type_mode : unit -> unit
val exit_rt_type_mode : unit -> unit
val is_rt_type_mode : unit -> bool


(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
