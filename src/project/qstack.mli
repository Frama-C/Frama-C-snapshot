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

(* $Id: qstack.mli,v 1.2 2008/04/10 15:48:06 uid562 Exp $ *)

(** Mutable stack in which it is possible to add data at the end
    and to manage non top elements. 
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

  val is_empty: t -> bool
    (** Test whether the stack is empty or not. *)

  val clear: t -> unit
    (** Remove all the elements of a stack. *)
    
  val add: D.t -> t -> unit
    (** Add at the begining of the stack. Complexity: O(1). *)

  val add_at_end: D.t -> t -> unit
    (** Add at the end of the stack. Complexity: O(1). *)

  val top: t -> D.t
    (** Return the top element of the stack.  Raise [Empty] if the stack is
	empty.  Complexity: amortized O(1). *)

  val mem: D.t -> t -> bool
    (** Return [true] if the data exists in the stack and [false] otherwise.
	Complexity: O(n). *)

  val filter: (D.t -> bool) -> t -> D.t list
    (** Return all data of the stack satisfying the specified predicate. 
	The order of the data in the input stack is preserved.
	Not tail recursive. *)
    
  val remove: D.t -> t -> unit
    (** Remove an element from the stack. 
	Complexity: O(n). *)

  val move_at_top: D.t -> t -> unit
    (** Move the element [x] at the top of the stack [s].
	Raise [Invalid_argument] if [not (mem x s)]. 
	Complexity: O(n). *)

  val iter: (D.t -> unit) -> t -> unit
    (** Iter on all the elements from the top to the end of the stack. 
	Not tail recursive. *)

  val fold: ('a -> D.t -> 'a) -> 'a -> t -> 'a
    (** Fold on all the elements from the top to the end of the stack. 
	Not tail recursive. *)

end

(*
Local Variables:
compile-command: "LC_ALL=C make -C ../.. -j"
End:
*)
