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

(** Mutable stack in which it is possible to add data at the end (like a queue)
    and to handle non top elements.
    Current implementation is double linked list. *)

module type DATA = sig
  type t
  val equal: t -> t -> bool
end

module Make(D: DATA) : sig

  type t

  exception Empty

  val create: unit -> t
    (** Create a new empty stack. *)

  val singleton: D.t -> t
    (** Create a new qstack with a single element.
        @since Boron-20100401 *)

  val is_empty: t -> bool
    (** Test whether the stack is empty or not. *)

  val clear: t -> unit
    (** Remove all the elements of a stack. *)

  val add: D.t -> t -> unit
    (** Add at the beginning of the stack. Complexity: O(1). *)

  val add_at_end: D.t -> t -> unit
    (** Add at the end of the stack. Complexity: O(1). *)

  val top: t -> D.t
    (** Return the top element of the stack.  Raise [Empty] if the stack is
        empty. Complexity: amortized O(1). *)

  val mem: D.t -> t -> bool
    (** Return [true] if the data exists in the stack and [false] otherwise.
        Complexity: O(n). *)

  val filter: (D.t -> bool) -> t -> D.t list
    (** Return all data of the stack satisfying the specified predicate.
        The order of the data in the input stack is preserved.
        Not tail recursive. *)

  val find: (D.t -> bool) -> t -> D.t
    (** Return the first data of the stack satisfying the specified predicate.
        @raise Not_found if there is no such data in the stack *)

  val remove: D.t -> t -> unit
    (** Remove an element from the stack.
        Complexity: O(n). *)

  val move_at_top: D.t -> t -> unit
    (** Move the element [x] at the top of the stack [s].
        Complexity: O(n).
        @raise Invalid_argument if [not (mem x s)]. *)

  val move_at_end: D.t -> t -> unit
    (** Move the element [x] at the end of the stack [s].
        Complexity: O(n).
        @raise Invalid_argument if [not (mem x s)].
        @since Beryllium-20090901 *)

  val iter: (D.t -> unit) -> t -> unit
    (** Iter on all the elements from the top to the end of the stack.
        Not tail recursive. *)

  val map: (D.t -> D.t) -> t -> unit
    (** Replace in-place all the elements of the stack by mapping the old one.
        Not tail recursive.
        @since Beryllium-20090901 *)

  val fold: ('a -> D.t -> 'a) -> 'a -> t -> 'a
    (** Fold on all the elements from the top to the end of the stack.
        Not tail recursive. *)

  val nth: int -> t -> D.t
    (** @return the n-th element of the stack, if any.
        @raise Invalid_argument if there is not enough element in the stack.
        @since Beryllium-20090901 *)

  val length: t -> int
    (** @return the length of the stack
        @since Beryllium-20090901 *)

  val idx: D.t -> t -> int
    (** @return the index of the element in the stack
        @raise Not_found if the element is not in the stack
        This function is not tail recursive
        @since Beryllium-20090901 *)

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.."
End:
*)
