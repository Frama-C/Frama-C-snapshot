(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2017                                               *)
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

(** Types, monads and utilitary functions for lattices in which the bottom is
    managed separately from other values. *)

module Type : sig

  type 'a or_bottom = [ `Value of 'a | `Bottom ]

  (** This monad propagates the `Bottom value if needed. *)
  val (>>-) : 'a or_bottom -> ('a -> 'b or_bottom) -> 'b or_bottom

  (** Use this monad if the following function returns a simple value. *)
  val (>>-:) : 'a or_bottom -> ('a -> 'b) -> 'b or_bottom

end

include module type of Type

val non_bottom: 'a or_bottom -> 'a

val equal:       ('a -> 'a -> bool) -> 'a or_bottom -> 'a or_bottom -> bool
val is_included: ('a -> 'a -> bool) -> 'a or_bottom -> 'a or_bottom -> bool
val join:        ('a -> 'a -> 'a) -> 'a or_bottom -> 'a or_bottom -> 'a or_bottom
val narrow:      ('a -> 'a -> 'a or_bottom) -> 'a or_bottom -> 'a or_bottom -> 'a or_bottom

val pretty :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a or_bottom -> unit


(** Datatype constructor. *)
module Make_Datatype
    (Domain: Datatype.S)
  : Datatype.S with type t = Domain.t or_bottom


(** Bounds a semi-lattice. *)
module Bound_Lattice
    (Lattice: Lattice_type.Join_Semi_Lattice)
  : Lattice_type.Bounded_Join_Semi_Lattice with type t = Lattice.t or_bottom



(** In a lattice where the elements are lists of non-bottom values,
    the empty list is the bottom case. *)

(** Conversion functions. *)
val bot_of_list: 'a list -> 'a list or_bottom
val list_of_bot: 'a list or_bottom -> 'a list

(** [elt >:: list] adds [elt] to the [list] if it is not bottom. *)
val add_to_list : 'a or_bottom -> 'a list -> 'a list


(** Lattices in which both top and bottom are managed separately *)
module Top: sig

  type 'a or_top_bottom = [ 'a or_bottom | `Top ]

  val join:
    ('a -> 'a -> 'a) ->
    'a or_top_bottom -> 'a or_top_bottom -> 'a or_top_bottom

  val narrow:
    ('a -> 'a -> 'a or_bottom) ->
    'a or_top_bottom -> 'a or_top_bottom -> 'a or_top_bottom

end
