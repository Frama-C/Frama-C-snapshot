(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2016                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  Contact CEA LIST for licensing.                                       *)
(**************************************************************************)

(** Topological sort in graphs *)

module type Element =
sig
  type t
  val hash : t -> int
  val equal : t -> t -> bool
end

module Make(E : Element) :
sig

  type succ = (E.t -> unit) -> E.t -> unit
  (** Iterator on the successors of an element *)

  type root = (E.t -> unit) -> unit
  (** Iterator on the required roots *)

  val components : succ:succ -> root:root -> ?size:int ->
    unit -> E.t list array
  (** The array of components.
      	For two elements [a in Ci] and [b in Cj] with [i<=j], then [a->*b]
      	by transitive closure of [succ] relation. *)

end
