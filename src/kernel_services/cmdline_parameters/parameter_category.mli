(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2015                                               *)
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

(** Category for parameter collections.
    A category groups together a set of possible values of a given type for some
    parameters. It may be created once and used several times. *)

type 'a t
(** [\tau t] is the type of a category for the type \tau. *)

type 'a accessor =
    < fold:'acc. ('a -> 'acc -> 'acc) -> 'acc -> 'acc (* fold on elements *);
      mem:('a -> bool) (* mem *) >
(** Type explaining how to manipulate the elements of the category. *)

val create:
  string ->
  'a Type.t ->
  register:bool ->
  State.t list ->
  'a accessor ->
  'a t
(** [create name ty ~register states access] creates a category of the given
    name for the given type. No category with such a name for the same type must
    be already registered. If [register], save the category for further re-use.
    [states] is a list of states which the category is based upon. [access] is
    how to manipulate this category. *)

val copy_and_rename: string -> register:bool -> 'a t -> 'a t
(** [copy_and_rename s ~register c] renames the category [c] into [s] and
    returns the new built category which is registered according to
    [register]. *)

val use: State.t -> 'a t -> unit
(** [use s c] indicates that the state [s] depends on the category [c]. *)

val get_name: 'a t -> string
(** Name of the category. *)

val get_fold: 'a t -> ('a -> 'acc -> 'acc) -> 'acc -> 'acc
(** Fold over the elements of the given category. *)

val get_mem: 'a t -> 'a -> bool
(** Is the given element present in the category? *)

(*
Local Variables:
compile-command: "make -C ../../.."
End:
*)
