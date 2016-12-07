(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Jean-Christophe Filliatre                               *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  File modified by CEA (Commissariat à l'énergie atomique et aux        *)
(*                        énergies alternatives).                         *)
(*                                                                        *)
(**************************************************************************)

(** Leftist heaps.

    See for instance Chris Okasaki's "Purely Functional Data Structures" *)

exception Empty

module Make(X: Set.OrderedType) :
sig
  type t

  val empty: t

  val is_empty: t -> bool
    (** runs in O(1) *)

  val insert: X.t -> t -> t
    (** runs in O(log n) *)

  val min: t -> X.t
    (** runs in O(1) *)

  val extract_min: t -> X.t * t
    (** runs in O(log n) *)

  val merge: t -> t -> t
    (** runs in O(log max(n1, n2)) *)


  (* Added by CEA *)

  val fold: (X.t -> 'a -> 'a) -> t -> 'a -> 'a
    (** elements are presented in an arbitrary order *)

  val elements: t -> X.t list
    (** the returned list is not sorted *)

  val of_list: X.t list -> t
end
