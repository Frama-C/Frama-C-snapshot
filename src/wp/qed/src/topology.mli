(**************************************************************************)
(*                                                                        *)
(*  This file is part of WP plug-in of Frama-C.                           *)
(*                                                                        *)
(*  Copyright (C) 2007-2014                                               *)
(*    CEA (Commissariat a l'energie atomique et aux energies              *)
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
