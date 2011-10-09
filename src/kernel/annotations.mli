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

(** Annotations associated with a statement.
    The AST should be computed before calling functions of this module.
    @plugin development guide *)

open Cil_types

val get_code_annotation: rooted_code_annotation -> code_annotation
(** extract the undecorated [code_annotation] from an annotation. *)

(* TODO: why a list here? *)
val add: 
  kernel_function -> stmt -> State.t list -> rooted_code_annotation -> unit
    (** Associate one more annotation with the given stmt.
	The list is the states required for computing this binding.
	See {!State_builder.DASHTBL_OUTPUT.add} for details.
	@modify Nitrogen-20111001
	@modify Boron-20100401 *)

val add_assert: 
  kernel_function -> stmt -> State.t list -> predicate named -> unit
  (** Associate one more assertion annotation with the given stmt.
      The list is the states required for computing this binding.
      @modify Boron-20100401
      @modify Nitrogen-20111001 Argument [before] suppressed: it is always
         before
      @plugin development guide *)

val set_annot:
  ?reset:bool -> kernel_function -> stmt -> State.t list ->
  (rooted_code_annotation -> rooted_code_annotation) -> 
  unit
(** [replace ~reset kf stmt f] applies [f] on each annotation [a] associated
    to the given [stmt] of [kf] in order to replace it by [f a].
    @since Nitrogen-20111001 *)  

val reset_stmt: reset:bool -> kernel_function -> stmt -> unit
  (** Erase all the annotations associated to the given stmt.
      [reset] is [true] iff all the dependencies of all the bindings of this
      statement must be cleared.
      @modify Boron-20100401 *)

val get:
  ?who: State.t list -> stmt -> State.t ->
  (rooted_code_annotation * State.t) list
    (** Return all the bindings associated with the stmt and state.
        See {!State_builder.DASHTBL_OUTPUT.find_all_local} for details.
        @since Boron-20100401 *)

val get_annotations:
  ?who: State.t list -> stmt -> State.t ->
  rooted_code_annotation list
    (** Return all the annotations associated with the stmt and state.
        @since Boron-20100401 *)

val get_all:
  ?who: State.t list -> stmt ->
  (rooted_code_annotation * State.t) list
    (** Return all the bindings associated with the stmt.
        @modify Boron-20100401 *)

val get_all_annotations:
  ?who: State.t list -> stmt ->
  rooted_code_annotation list
    (** Return all the annotations associated with the stmt.
        since Boron-20100401 *)

val get_by_state:
  stmt -> (State.t * rooted_code_annotation list) list
    (** Return all the annotations associated with the stmt
        and sorted by states.
        @since Boron-20100401 *)

val get_filter:
  (code_annotation -> bool) -> stmt -> rooted_code_annotation list
    (** Returns all the annotation associated with the stmt that respects
        the given condition. Use it in conjunction with Logic_utils.is_*
        to retrieve a particular kind of annotations. *)

val iter_stmt:
  (State.t option -> rooted_code_annotation * State.t -> unit) ->
  stmt -> unit
  (** Iterator on each bindings of the given statement.
      @since Boron-20100401 *)

val single_iter_stmt:
  (rooted_code_annotation -> unit) -> stmt -> unit
  (** Iterator on each annotations of the given statement.
      Multiple bindings are only applied once.
      @since Boron-20100401 *)

val fold_stmt:
  (State.t option -> rooted_code_annotation * State.t -> 'a -> 'a)
  -> stmt -> 'a -> 'a
  (** Folder on each bindings of the given statement
      @since Boron-20100401 *)

val single_fold_stmt:
  (rooted_code_annotation -> 'a -> 'a) -> stmt -> 'a -> 'a
  (** Folder on each annotations of the given statement.
      Multiple bindings are only applied once.
      @since Boron-20100401 *)

val iter:
  (stmt -> State.t option ->
   rooted_code_annotation * State.t -> unit)
  -> unit
(** Iterator on each bindings.
    @since Boron-20100401 *)

val fold:
  (stmt -> State.t option ->
   rooted_code_annotation * State.t -> 'a -> 'a)
  -> 'a -> 'a
  (** Folder on each bindings.
      @since Boron-20100401 *)

val filter:
  reset:bool ->
  (stmt -> State.t option -> rooted_code_annotation -> bool) ->
  kernel_function -> stmt -> unit
  (** Filter the bindings associated to the given statement.
      See {!State_builder.DASHTBL_OUTPUT.filter} for details.
      @since Boron-20100401 *)

val self: State.t
  (** Internal states of the table associated annotations to statements.
      @since Boron-20100401 *)

(*
  Local Variables:
  compile-command: "make -C ../.."
  End:
*)
