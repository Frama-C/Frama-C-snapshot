(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2019                                               *)
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

(** A partitioning index is a collection of states optimized to determine
    if a new state is included in one of the states it contains — in a more
    efficient way than to test the inclusion with all stored states.
    Such an index is used to keep track of all the states already propagated
    through a control point, and to rule out new incoming states included in
    previous ones.

    Partitioning index relies on an heuristics on the cvalue domain,
    and is very inefficient without it. *)

module type Domain = sig
  include Abstract_domain.Lattice
  include Datatype.S_with_collections with type t = state
  include Abstract.Interface with type t := state
                              and type 'a key := 'a Abstract_domain.key
end

module Make (Domain: Domain) : sig
  type t

  (** Creates an empty index. *)
  val empty: unit -> t

  (** Adds a state into an index. Returns true if the state did not belong to
      the index (and has indeed been added), and false if the index already
      contained the state. *)
  val add : Domain.t -> t -> bool

  val pretty : Format.formatter -> t -> unit
end


(*
Local Variables:
compile-command: "make -C ../../../.."
End:
*)
