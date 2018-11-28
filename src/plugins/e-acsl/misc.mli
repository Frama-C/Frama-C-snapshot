(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2018                                               *)
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

(** Utilities for E-ACSL. *)

open Cil_types
open Cil_datatype

(* ************************************************************************** *)
(** {2 Builders} *)
(* ************************************************************************** *)

exception Unregistered_library_function of string
val get_lib_fun_vi: string -> varinfo
(** Return varinfo corresponding to a name of a given library function *)

val mk_call: loc:Location.t -> ?result:lval -> string -> exp list -> stmt
(** Call an E-ACSL library function or an E-ACSL built-in.
    @raise Unregistered_library_function if the given string does not represent
    such a function or if these functions were never registered (only possible
    when using E-ACSL through its API. *)

val mk_deref: loc:Location.t -> exp -> exp
(** Make a dereference of an expression *)

type annotation_kind =
  | Assertion
  | Precondition
  | Postcondition
  | Invariant
  | RTE

val mk_e_acsl_guard:
  ?reverse:bool -> annotation_kind -> kernel_function -> exp -> predicate
  -> stmt

val mk_block: Project.t -> stmt -> block -> stmt

(* ************************************************************************** *)
(** {2 Handling \result} *)
(* ************************************************************************** *)

val result_lhost: kernel_function -> lhost
(** @return the lhost corresponding to \result in the given function *)

val result_vi: kernel_function -> varinfo
(** @return the varinfo corresponding to \result in the given function *)

(* ************************************************************************** *)
(** {2 Handling the E-ACSL's C-libraries} *)
(* ************************************************************************** *)

val library_files: unit -> string list
val is_library_loc: location -> bool
val register_library_function: varinfo -> unit
val reset: unit -> unit

val mk_store_stmt: ?str_size:exp -> varinfo -> stmt
val mk_duplicate_store_stmt: ?str_size:exp -> varinfo -> stmt
val mk_delete_stmt: varinfo -> stmt
val mk_full_init_stmt: ?addr:bool -> varinfo -> stmt
val mk_initialize: loc:location -> lval -> stmt
val mk_mark_readonly: varinfo -> stmt

(* ************************************************************************** *)
(** {2 Other stuff} *)
(* ************************************************************************** *)

val term_addr_of: loc:location -> term_lval -> typ -> term

val reorder_ast: unit -> unit
(* Reorder current AST by bringing all global declarations belonging to the
 * E-ACSL runtime library and their dependencies (e.g., typedef size_t) to
 * the very top of the file. *)

val cty: logic_type -> typ
(* Assume that the logic type is indeed a C type. Just return it. *)

val ptr_index: ?loc:location -> ?index:exp -> exp
  -> Cil_types.exp * Cil_types.exp
(** Split pointer-arithmetic expression of the type `p + i` into its
pointer and integer parts. *)

val term_of_li: logic_info -> term
(* [term_of_li li] assumes that [li.l_body] matches [LBterm t]
   and returns [t]. *)

val is_set_of_ptr_or_array: logic_type -> bool
(* Checks whether the given logic type is a set of pointers. *)

val is_range_free: term -> bool
(* Returns [true] iff the given term does not contain any range. *)

val is_bitfield_pointers: logic_type -> bool
(* Returns [true] iff the given logic type is a bitfield pointer or a
   set of bitfield pointers. *)

val term_has_lv_from_vi: term -> bool
(* Return [true] iff the given term contains a variables that originates from
  a C varinfo, that is a non-purely logic variable. *)

type pred_or_term = PoT_pred of predicate | PoT_term of term

val mk_ptr_sizeof: typ -> location -> exp
(* [mk_ptr_sizeof ptr_typ loc] takes the pointer typ [ptr_typ] that points
   to a [typ] typ and returns [sizeof(typ)]. *)

val finite_min_and_max: Ival.t -> Integer.t * Integer.t
(* [finite_min_and_max i] takes the finite ival [i] and returns its bounds *)

(*
Local Variables:
compile-command: "make"
End:
*)
