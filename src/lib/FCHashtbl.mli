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

(** Extension of OCaml's [Hashtbl] module. *)

(* No need to expand OCaml's [Hashtbl.S] here: we do not provide an alternative
   implementation of [Hashtbl]. Hence, we will always be compatible with the
   stdlib. *)

module type S = sig

  include Hashtbl.S

  val iter_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> unit) -> 'a t -> unit
    (** Iter on the hashtbl, but respecting the order on keys induced
	by [cmp]. Use [Pervasives.compare] if [cmp] not given.

	If the table contains several bindings for the same key, they
	are passed to [f] in reverse order of introduction, that is,
	the most recent binding is passed first. *)

  val fold_sorted:
    ?cmp:(key -> key -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (** Fold on the hashtbl, but respecting the order on keys induced
	by [cmp]. Use [Pervasives.compare] if [cmp] not given.

	If the table contains several bindings for the same key, they
	are passed to [f] in reverse order of introduction, that is,
	the most recent binding is passed first. *)

  val iter_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted_by_entry:
    cmp:((key * 'a) -> (key * 'a) -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** Iter or fold on the hashtable, respecting the order on entries
      given by [cmp]. The table may contains several bindings for the
      same key. *)

  val iter_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> unit) -> 'a t -> unit
  val fold_sorted_by_value:
    cmp:('a -> 'a -> int) -> (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
(** Iter or fold on the hashtable, respecting the order on entries
    given by [cmp]. The relative order for entries whose values is
    equal according to cmp, is not specified. *)
end

module Make(H: Hashtbl.HashedType) : S with type key = H.t

val hash : 'a -> int
val hash_param : int -> int -> 'a -> int

