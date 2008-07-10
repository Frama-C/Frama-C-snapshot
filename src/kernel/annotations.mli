(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2008                                               *)
(*    CEA (Commissariat à l'Énergie Atomique)                             *)
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

(* $Id: annotations.mli,v 1.23 2008/05/30 08:29:48 uid568 Exp $ *)

(** Annotations associated with a kinstr. 
    @plugin developer guide *)

open Cil_types
open Db_types

val add: stmt -> rooted_code_annotation before_after -> unit
  (** Associate one more annotation with a stmt. *)

val add_assert: stmt -> before:bool -> predicate named -> unit
  (** Associate one more assertion annotation with a stmt. *)

val add_alarm: stmt -> before:bool -> Alarms.t -> predicate named -> unit
  (** Associate one more alarm annotation with a stmt. *)

val replace: stmt -> rooted_code_annotation before_after -> unit
  (** Associate the given annotation with the given stmt.
      Previous annotations of this stmt disappear. *)

val reset_stmt: stmt -> unit
  (** Erase the annotations associated to the given stmt. *)

val get: stmt -> rooted_code_annotation before_after list
  (** Return all the annotations associated with the stmt. *)

val get_filter:
  (code_annotation -> bool) -> stmt ->
  rooted_code_annotation before_after list
    (** Returns all the annotation associated with the stmt that respects
        the given condition. Use it in conjunction with Logic_const.is_*
        to retrieve a particular kind of annotations.
    *)

val filter: (stmt ->rooted_code_annotation before_after -> bool) -> unit
  (** For each stmt, filter its associated annotations with the given
      predicate.
  *)

val iter:
  (stmt -> rooted_code_annotation before_after list ref -> unit) -> unit

val self: Project.Computation.t


(*
  Local Variables:
  compile-command: "LC_ALL=C make -C ../.."
  End:
*)
