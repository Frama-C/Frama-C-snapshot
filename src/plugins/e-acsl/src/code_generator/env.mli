(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's E-ACSL plug-in.                    *)
(*                                                                        *)
(*  Copyright (C) 2012-2019                                               *)
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

open Cil_types

(** Environments. 

    Environments handle all the new C constructs (variables, statements and
    annotations. *) 

type t

val dummy: t
val empty: Visitor.frama_c_visitor -> t

val has_no_new_stmt: t -> bool
(** Assume that a local context has been previously pushed.
    @return true iff the given env does not contain any new statement. *)

type localized_scope =
  | LGlobal
  | LFunction of kernel_function
  | LLocal_block of kernel_function

val new_var:
  loc:location -> ?scope:Varname.scope -> ?name:string ->
  t -> term option -> typ -> 
  (varinfo -> exp (* the var as exp *) -> stmt list) -> 
  varinfo * exp * t
(** [new_var env t ty mk_stmts] extends [env] with a fresh variable of type
    [ty] corresponding to [t]. [scope] is the scope of the new variable (default
    is [Block]).
    @return this variable as both a C variable and a C expression already
    initialized by applying it to [mk_stmts]. *)

val new_var_and_mpz_init:
  loc:location -> ?scope:Varname.scope -> ?name:string ->
  t -> term option -> 
  (varinfo -> exp (* the var as exp *) -> stmt list) -> 
  varinfo * exp * t
(** Same as [new_var], but dedicated to mpz_t variables initialized by 
    {!Mpz.init}. *)

module Logic_binding: sig
  val add: ?ty:typ -> t -> logic_var -> varinfo * exp * t
  (* Add a new C binding to the list of bindings for the logic variable. *)

  val add_binding: t -> logic_var -> varinfo -> t
  (* [add_binding env lv vi] defines [vi] as the latest C binding for
     [lv]. *)

  val get: t -> logic_var -> varinfo
  (* Return the latest C binding. *)

  val remove: t -> logic_var -> unit
  (* Remove the latest C binding. *)

end

val add_assert: t -> stmt -> predicate -> unit
(** [add_assert env s p] associates the assertion [p] to the statement [s] in
    the environment [env]. *)

val add_stmt: ?post:bool -> ?before:stmt -> t -> stmt -> t
(** [add_stmt env s] extends [env] with the new statement [s].
    [before] may define which stmt the new one is included before. This is to
    say that any labels attached to [before] are moved to [stmt]. [post]
    indicates that [stmt] should be added after the target statement. *)

val extend_stmt_in_place: t -> stmt -> label:logic_label -> block -> t
(** [extend_stmt_in_place env stmt ~label b] modifies [stmt] in place in
    order to add the given [block]. If [label] is [Here] or [Post],
    then this block is guaranteed to be at the first place of the resulting
    [stmt] whatever modification will be done by the visitor later. *)

val push: t -> t
(** Push a new local context in the environment *)

type where = Before | Middle | After
val pop_and_get:
  ?split:bool -> t -> stmt -> global_clear:bool -> where -> block * t
(** Pop the last local context and get back the corresponding new block
    containing the given [stmt] at the given place ([Before] is before the code
    corresponding to annotations, [After] is after this code and [Middle] is
    between the stmt corresponding to annotations and the ones for freeing the
    memory. When [where] is [After], set [split] to true in order to generate
    one block which contains exactly 2 stmt: one for [stmt] and one sub-block
    for the generated stmts. *)

val pop: t -> t
(** Pop the last local context (ignore the corresponding new block if any *)

val transfer: from:t -> t -> t
(** Pop the last local context of [from] and push it into the other env. *)

val get_generated_variables: t -> (varinfo * localized_scope) list
(** All the new variables local to the visited function. *)

val get_visitor: t -> Visitor.generic_frama_c_visitor
val get_behavior: t -> Visitor_behavior.t
val current_kf: t -> kernel_function option
(** Kernel function currently visited in the new project. *)

module Logic_scope: sig
  val get: t -> Lscope.t
  (** Return the logic scope associated to the environment. *)

  val extend: t -> Lscope.lscope_var -> t
  (** Add a new logic variable with its associated information in the
    logic scope of the environment. *)

  val reset: t -> t
  (** Return a new environment in which the logic scope is reset
    iff [set_reset _ true] has been called beforehand. Do nothing otherwise. *)

  val set_reset: t -> bool -> t
  (** Setter of the information indicating whether the logic scope should be
    reset at next call to [reset]. *)

  val get_reset: t -> bool
  (** Getter of the information indicating whether the logic scope should be
    reset at next call to [reset]. *)
end

val set_current_kf: t -> kernel_function -> unit
(* Set current kf of the environment *)

(* ************************************************************************** *)
(** {2 Current annotation kind} *)
(* ************************************************************************** *)

val annotation_kind: t -> Misc.annotation_kind
val set_annotation_kind: t -> Misc.annotation_kind -> t

(* ************************************************************************** *)
(** {2 Loop invariants} *)
(* ************************************************************************** *)

val push_loop: t -> t
val add_loop_invariant: t -> predicate -> t
val pop_loop: t -> predicate list * t

(* ************************************************************************** *)
(** {2 RTEs} *)
(* ************************************************************************** *)

val rte: t -> bool -> t
val generate_rte: t -> bool

(* ************************************************************************** *)
(** {2 Context for error handling} *)
(* ************************************************************************** *)

module Context: sig
  val save: t -> unit
  val restore: t -> t
end

val pretty: Format.formatter -> t -> unit

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
