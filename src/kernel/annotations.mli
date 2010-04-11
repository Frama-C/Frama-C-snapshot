(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2010                                               *)
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
    @plugin development guide *)

open Cil_types
open Db_types

val add: 
  stmt -> Project.Computation.t list -> rooted_code_annotation before_after -> 
  unit
    (** Associate one more annotation with the given stmt. 
	The list is the states required for computing this binding.
	See {!Computation.DASHTBL_OUTPUT.add} for details.
	@modify Boron-20100401 *)

val add_assert: 
  stmt -> Project.Computation.t list -> before:bool -> predicate named -> unit
  (** Associate one more assertion annotation with the given stmt.
      The list is the states required for computing this binding.
      @modify Boron-20100401 *)

val add_alarm: 
  stmt -> Project.Computation.t list -> before:bool -> Alarms.t -> 
  predicate named -> unit
  (** Associate one more alarm annotation with the given stmt.
      The list is the states required for computing this binding.
      @modify Boron-20100401 *)

val replace: 
  reset:bool -> 
  stmt -> Project.Computation.t list -> rooted_code_annotation before_after -> 
  unit
  (** Associate the given annotation with the given stmt.
      Previous annotations of this stmt disappear.
      The list is the states required for computing this binding.
      @modify Boron-20100401 *)

val reset_stmt: reset:bool -> stmt -> unit
  (** Erase all the annotations associated to the given stmt. 
      [reset] is [true] iff all the dependencies of all the bindings of this
      statement must be cleared.
      @modify Boron-20100401 *)

val get: 
  ?who: Project.Computation.t list -> stmt -> Project.Computation.t -> 
  (rooted_code_annotation before_after * Project.Computation.t) list
    (** Return all the bindings associated with the stmt and state.
	See {!Computation.DASHTBL_OUTPUT.find_all_local} for details.
	@since Boron-20100401 *)

val get_annotations: 
  ?who: Project.Computation.t list -> stmt -> Project.Computation.t -> 
  rooted_code_annotation before_after list
    (** Return all the annotations associated with the stmt and state.  
	@since Boron-20100401 *)

val get_all: 
  ?who: Project.Computation.t list -> stmt -> 
  (rooted_code_annotation before_after * Project.Computation.t) list
    (** Return all the bindings associated with the stmt. 
	@modify Boron-20100401 *)

val get_all_annotations: 
  ?who: Project.Computation.t list -> stmt ->
  rooted_code_annotation before_after list
    (** Return all the annotations associated with the stmt. 
	since Boron-20100401 *)

val get_by_state: 
  stmt -> 
  (Project.Computation.t * rooted_code_annotation before_after list) list
    (** Return all the annotations associated with the stmt 
	and sorted by states.
	@since Boron-20100401 *)

val get_filter:
  (code_annotation -> bool) -> stmt ->
  rooted_code_annotation before_after list
    (** Returns all the annotation associated with the stmt that respects
        the given condition. Use it in conjunction with Logic_utils.is_*
        to retrieve a particular kind of annotations. *)

val iter_stmt:
  (Project.Computation.t -> rooted_code_annotation before_after -> unit) 
  -> stmt
  -> unit
  (** Iterator on each bindings of the given statement.
      @since Boron-20100401 *)

val single_iter_stmt:
  (rooted_code_annotation before_after -> unit) -> stmt -> unit
  (** Iterator on each annotations of the given statement.
      Multiple bindings are only applied once.
      @since Boron-20100401 *)

val fold_stmt:
  (Project.Computation.t -> rooted_code_annotation before_after -> 'a -> 'a) 
  -> stmt
  -> 'a -> 'a
  (** Folder on each bindings of the given statement
      @since Boron-20100401 *)

val single_fold_stmt:
  (rooted_code_annotation before_after -> 'a -> 'a) -> stmt -> 'a -> 'a
  (** Folder on each annotations of the given statement.
      Multiple bindings are only applied once.
      @since Boron-20100401 *)

val iter:
  (stmt -> Project.Computation.t -> rooted_code_annotation before_after -> 
     unit) 
  -> unit
  (** Iterator on each bindings.
      @since Boron-20100401 *)

val fold:
  (stmt -> Project.Computation.t -> rooted_code_annotation before_after -> 
     'a -> 'a) 
  -> 'a -> 'a
  (** Folder on each bindings.
      @since Boron-20100401 *)

val filter:
  reset:bool ->
  (stmt -> Project.Computation.t -> rooted_code_annotation before_after ->
  bool)
  -> stmt -> unit
  (** Filter the bindings associated to the given statement.
      See {!Computation.DASHTBL_OUTPUT.filter} for details.
      @since Boron-20100401 *)

val iter_stmt:
  (Project.Computation.t -> rooted_code_annotation before_after -> unit)
  -> stmt -> unit
  (** Iterator on each bindings associated to the given statement
      @since Boron-20100401 *)
  
val self: Project.Computation.t
  (** Internal states of the table associated annotations to statements.
      @since Boron-20100401 *)

(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)
